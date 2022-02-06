#include "common.hpp"
#include "parser.hpp"
#include "types.hpp"

template <typename... Args>
[[noreturn]] inline void SYNTAX_ERROR_AFTER (Token const& tok, const char* format, Args... args) {
	_ERROR("syntax error", source_range( tok.source.end, tok.source.end+1 ), format, std::make_format_args(args...));
}

void dbg_print (AST* node, int depth) {
	if (!node) return;
	
	auto indent = [] (int depth) {
		for (int i=0; i<depth; ++i)
			printf("  ");
	};

	indent(depth);
	printf("%s", ASTKind_str[node->kind]);

	switch (node->kind) {
		case A_LITERAL: { auto* lit = (AST_literal*)node;
			std::string str(lit->src_tok->source.text());
			printf(" %s\n", str.c_str());
		} break;

		case A_VARDECL: { auto* var = (AST_vardecl*)node;
			std::string str(var->ident);
			printf(" %s\n", str.c_str()); // , Type_str[var->valtype]
		} break;

		case A_VAR: { auto* var = (AST_var*)node;
			std::string str(var->ident);
			printf(" %s\n", str.c_str());
		} break;

		case A_UNOP: { auto* op = (AST_unop*)node;
			printf("(%s)", OpType_str[op->op]);

			printf(" (\n");
			dbg_print(op->operand, depth+1);
			indent(depth); printf(")\n");
		} break;

		case A_BINOP: 
		case A_ASSIGNOP: { auto* op = (AST_binop*)node;
			printf("(%s) (\n", OpType_str[op->op]);

			dbg_print(op->lhs, depth+1);
			dbg_print(op->rhs, depth+1);

			indent(depth); printf(")\n");
		} break;
			
		case A_BLOCK: { auto* block = (AST_block*)node;
			printf(" (\n");
			for (auto* n : block->statements)
				dbg_print(n, depth+1);
			indent(depth); printf(")\n");
		} break;
			
		case A_IF:
		case A_SELECT: { auto* aif = (AST_if*)node;
			printf(" (\n");

			dbg_print(aif->cond     , depth+1);
			dbg_print(aif->if_body , depth+1);
			if (aif->else_body)
				dbg_print(aif->else_body, depth+1);

			indent(depth); printf(")\n");
		} break;

		case A_WHILE:
		case A_DO_WHILE:
		case A_FOR: { auto* loop = (AST_loop*)node;
			printf(" (\n");
			if (loop->start) {
				dbg_print(loop->start, depth+1);
			}

			dbg_print(loop->cond, depth+1);

			if (loop->end) {
				dbg_print(loop->end, depth+1);
			}
			
			dbg_print(loop->body, depth+1);

			indent(depth); printf(")\n");
		} break;

		case A_STRUCTDEF: { auto* struc = (AST_structdef*)node;
			
			printf(" (\n");
			for (auto* n : struc->members)
				dbg_print(n, depth+1);
			indent(depth); printf(")\n");
			
		} break;

		case A_FUNCDEF: { auto* f = (AST_funcdef*)node;
			std::string str(f->ident);
			printf(" %s\n", str.c_str());

			depth++;

			
			indent(depth); printf("args: (\n");
			for (auto* arg : f->args)
				dbg_print(arg, depth+1);
			indent(depth); printf(")\n");
			
			indent(depth); printf("rets: (\n");
			for (auto* ret : f->rets)
				dbg_print(ret, depth+1);
			indent(depth); printf(")\n");

			indent(depth); printf("(\n");
			dbg_print(f->body, depth+1);
			indent(depth); printf(")\n");
		} break;

		case A_CALLARG: { auto* arg = (AST_callarg*)node;
			if (!arg->ident.empty()) {
				std::string str(arg->ident);
				printf(" %s=", str.c_str());
			}
			
			printf(" (\n");
			dbg_print(arg->expr, depth+1);
			indent(depth); printf(")\n");
		} break;
		case A_CALL: { auto* call = (AST_call*)node;
			std::string str(call->ident);
			printf(" %s (\n", str.c_str());
			for (auto* arg : call->args)
				dbg_print(arg, depth+1);
			indent(depth); printf(")\n");
		} break;
		
		case A_RETURN: { auto* ret = (AST_return*)node;
			printf(" (\n");
			for (auto* arg : ret->args)
				dbg_print(arg, depth+1);
			indent(depth); printf(")\n");
		} break;
		case A_BREAK:
		case A_CONTINUE: {
			printf("\n");
		} break;

		INVALID_DEFAULT;
	}
}

template <typename T>
arrview<T> make_copy (arrview<T> tmp_arr) {
	T* arr = g_allocator.alloc_array<T>(tmp_arr.count);
	memcpy(arr, tmp_arr.data, tmp_arr.count * sizeof(T));
	return { arr, tmp_arr.count };
}

struct Parser {
	Token* tok;

	void eat_semicolon () {
		if (tok->type != T_SEMICOLON)
			SYNTAX_ERROR_AFTER(tok[-1], "';' expected");
		tok++;
	}

	template <typename T, typename FUNC>
	arrview<T> comma_seperated_list (TokenType endtok, const char* dbgname, FUNC element) {
		smallvec<T, 32> list;

		while (tok->type != endtok) {
			list.push( element() );

			if (tok->type == T_COMMA) {
				tok++;
			}
			else if (tok->type != endtok) {
				SYNTAX_ERROR_AFTER(tok[-1], "expected '%s' after %s!", TokenType_char[endtok], dbgname );
			}
		}

		return make_copy<T>(list);
	}

	//    (<expression>)                               -> Parenthesized expression 
	// or function call  -> function call with argument expressions
	// or <literal>
	AST* atom () {
		switch (tok->type) {

			case T_PAREN_OPEN: {
				// expression in parentheses
				tok++;

				AST* result = expression(0);

				if (tok->type != T_PAREN_CLOSE)
					SYNTAX_ERROR_AFTER(tok[-1], "expected ')' after parenthesized expression");
				tok++;

				return result;
			}

			case T_IDENTIFIER: {
				// func call
				if (tok[1].type == T_PAREN_OPEN) {
					return call();
				}
				// variable
				else {
					auto* var = ast_alloc<AST_var>(A_VAR, tok);
					var->ident = tok->source.text();

					tok++;
					return var;
				}
			}

			case T_LITERAL: {
				auto* lit = ast_alloc<AST_literal>(A_LITERAL, tok);
				lit->type  = Typeref::RValue( BASIC_TYPES[tok->lit_type] );
				lit->value = tok->lit_val;
				tok++;
				return lit;
			}
			
			default: {
				SYNTAX_ERROR_AFTER(tok[-1], "number or variable expected after operator");
				return nullptr;
			}
		}
	}

	// expression consisting of atoms combined with unary, binary or ternary operators parsed according to associativity and precedence rules
	AST* expression (unsigned min_prec) {

		AST* lhs;

		// unary prefix operators are right associative, so are parsed by recursing into expression(prec) with prec = unary op precendence
		if (is_unary_prefix_op(tok->type)) {
			unsigned prec = un_prec(tok->type);
			auto* unary_op = ast_alloc<AST_unop>(A_UNOP, tok);
			unary_op->op = tok2unop(tok->type);
			tok++;

			unary_op->operand = expression(prec);
			lhs = unary_op;
		}
		// once all unary operators are parsed right-recursion stops
		else {
			lhs = atom();
		}

		// unary postfix operators are left-associative, so are parsed by a loop
		while (is_unary_postfix_op(tok->type)) {
			unsigned prec = un_prec(tok->type);
			if (prec < min_prec) // handle precedence rules
				return lhs;

			auto* post_op = ast_alloc<AST_unop>(A_UNOP, tok);
			post_op->op = tok2unop(tok->type);
			tok++;

			post_op->operand = lhs;
			lhs = post_op;
		}

		// binary and ternary operators
		while (is_binary_or_ternary_op(tok->type)) {
			unsigned prec  = bt_prec( tok->type);
			unsigned assoc = bt_assoc(tok->type);

			if (prec < min_prec)
				break; // handle precedence rules

			Token* op_tok = tok++;

			AST* rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			// normal binary operator
			if (op_tok->type != T_QUESTIONMARK) {
				auto* op = ast_alloc<AST_binop>(A_BINOP, op_tok);
				op->op = tok2binop(op_tok->type);
				op->lhs = lhs;
				op->rhs = rhs;
				lhs = op;
			}
			// special case: ternary operator
			else {
				if (tok->type != T_COLON)
					SYNTAX_ERROR_AFTER(tok[-1], "':' expected after true case of select operator");
				tok++;

				auto* op = ast_alloc<AST_if>(A_SELECT, op_tok);
				op->cond     = lhs;
				op->if_body  = rhs;

				assert(assoc == RIGHT_ASSOC);
				op->else_body = expression(prec);

				lhs = op;
			}
		}

		return lhs;
	}

	//    <varname> :              -> variable declaration with inferred type (must be followed by  = <expression>  to infer the type from)
	// or <varname> : <typename>   -> variable declaration with explicit type
	AST_vardecl* var_decl (int allow_init=true) {
		if (!(tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON))
			SYNTAX_ERROR_AFTER(tok[-1], "expected variable declaration");

		auto* var = ast_alloc<AST_vardecl>(A_VARDECL, tok);
		var->ident = tok[0].source.text();

		tok += 2;

		// type specifier
		if (tok->type == T_IDENTIFIER) {
			// ident specifies type, but these identifiers can only be matched to AST_Type's in the later phase
			// leave later phase to find this on it's own
			
			//auto ident = tok->source.text();

			tok++;
		}
		else {
			// no type identifier, type will have to be inferred from initialization

			if (tok->type != T_ASSIGN)
				SYNTAX_ERROR_AFTER(tok[-1], "\neither specify type during variable declaration with \"<var> : <type>;\"\n"
				                            "or let type be inferred with \"<var> := <expr>;\"");
		}

		if (tok->type == T_ASSIGN) {
			if (!allow_init)
				ERROR(tok->source, "vardecl initialization not allowed in struct (yet)");

			tok++;
			var->init = expression(0);
		}
		return var;
	}

	//    <expression>
	// or <expression> = <expression>
	AST* assignment_or_expression () {
		AST* expr = expression(0);

		// lhs = rhs   or  lhs += rhs  etc.
		if (is_binary_assignemnt_op(tok->type)) {
			auto* op = ast_alloc<AST_binop>(A_ASSIGNOP, tok);
			op->op = tok2assignop(tok->type);
			tok++;

			op->lhs = expr;
			op->rhs = expression(0);
			return op;
		}
		return expr;
	}

	//    <expression>
	// or <expression> = <expression>
	// or <vardecl> = <expression>   ie.  <varname> : [typename] = <expression>
	AST* decl_or_assignment_or_expression () {
		// lhs is var decl
		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_COLON) {
			return var_decl();
		}
		// lhs is expression
		else {
			return assignment_or_expression();
		}
	}

	//    <expression>
	// or <argname> = <expression>
	AST_callarg* call_arg (TokenType endtok) {
		auto* arg = ast_alloc<AST_callarg>(A_CALLARG, tok);
		arg->ident = strview();

		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_ASSIGN) {
			arg->ident = tok[0].source.text();
			tok += 2;
		}

		arg->expr = expression(0);
		return arg;
	}

	// (<call_arg>, <call_arg> etc.)
	arrview<AST_callarg*> call_args () {
		assert(tok->type == T_PAREN_OPEN);
		tok++;

		auto arr = comma_seperated_list<AST_callarg*>(T_PAREN_CLOSE, "call argument list", [this] () {
			return call_arg(T_PAREN_CLOSE);
		});

		tok++; // T_PAREN_CLOSE
		return arr;
	}
	// <call_arg>, <call_arg> etc.
	arrview<AST_callarg*> return_args () {

		auto arr = comma_seperated_list<AST_callarg*>(T_SEMICOLON, "return argument list", [this] () {
			return call_arg(T_SEMICOLON);
		});

		return arr;
	}

	// <funcname><call_args>
	AST* call () {
		auto* call = ast_alloc<AST_call>(A_CALL, tok);
		call->ident = tok->source.text();
		tok++;

		call->args = call_args();

		return call;
	}

	// return <return_args>;
	AST* return_statement () {
		auto* ast = ast_alloc<AST_return>(A_RETURN, tok++);

		ast->args = return_args();

		eat_semicolon();
		return ast;
	}

	// parses  if <cond> {} elif <cond> {} elif <cond> else {} into a recursive if-else chain (else body points to new recursive if-else for elif)
	// where each elif and else is optional (else = null)
	AST* if_statement () {
		auto* aif = ast_alloc<AST_if>(A_IF, tok++); 

		aif->cond       = expression(0);

		aif->if_body   = block();
		aif->else_body = elif_statement();

		return aif;
	}
	AST* elif_statement () {
		if (tok->type == T_ELIF) {
			// code is same as if except that keyword happens to be elif instead of if (this is why C++ does else if)
			// solve via recurse
			return if_statement();
		}
		if (tok->type == T_ELSE) {
			tok++;
			return block();
		}
		return nullptr;
	}

	// while <cond> <block>
	AST* while_loop () {
		auto* loop = ast_alloc<AST_loop>(A_WHILE, tok++);

		loop->cond = expression(0);
		loop->body = block();

		return loop;
	}

	// do <block> while <cond>
	// NOTE: that <cond> has special scoping rules to allow it to access objects from inside the block
	AST* do_while_loop () {
		auto* loop = ast_alloc<AST_loop>(A_DO_WHILE, tok++);

		loop->body = block();

		if (tok->type != T_WHILE)
			SYNTAX_ERROR_AFTER(tok[-1], "while expected after do block!");
		tok++;

		loop->cond = expression(0);
		eat_semicolon();

		return loop;
	}

	// for [start]; <cond>; [end] <block>
	AST* for_loop () {
		auto* loop = ast_alloc<AST_loop>(A_FOR, tok++);

		loop->start = decl_or_assignment_or_expression();
		eat_semicolon();

		loop->cond  = expression(0);
		eat_semicolon();

		loop->end   = decl_or_assignment_or_expression();

		loop->body = block();

		return loop;
	}
	
	AST* struct_def () {
		auto* struc = ast_alloc<AST_structdef>(A_STRUCTDEF, tok);
		tok++;

		if (tok->type != T_IDENTIFIER)
			SYNTAX_ERROR_AFTER(tok[-1], "struct identifer expected after struct keyword!");
		struc->ident = tok->source.text();
		tok++;

		smallvec<AST_vardecl*, 16> members;

		if (tok->type != T_BLOCK_OPEN)
			SYNTAX_ERROR_AFTER(tok[-1], "'{' expected after struct identifier");
		tok++;

		while (tok->type != T_BLOCK_CLOSE) {
			auto* s = var_decl(false);
			if (s)
				members.push(s);
			eat_semicolon();
		}

		struc->members = make_copy<AST_vardecl*>(members);

		if (tok->type != T_BLOCK_CLOSE)
			SYNTAX_ERROR_AFTER(tok[-1], "syntax error, '}' expected");
		tok++;

		return struc;
	}
	
	AST_vardecl* funcdecl_arg () {
		auto* decl = (AST_vardecl*)var_decl();

		if (decl->init) {
			// TOOD: implement at const-foldable expression for default args at the very least
			// better yet allow things like  sqrt(5)  or even custom compile-time const functions to be called as well
			// or just const values in general (const globals or const locally captured vars)
			// the question is where the const folding happens -> wait until I get to actually implementing compile-time execution
			if (decl->init->kind != A_LITERAL)
				ERROR(decl->init->src_tok->source, "only literals allowed as default argument values (for now)");
			
			assert(decl->init->type.ty && decl->init->type.rval);
		}

		return decl;
	}

	arrview<AST_vardecl*> funcdecl_arglist () {
		assert(tok->type == T_PAREN_OPEN);
		tok++;

		auto arr = comma_seperated_list<AST_vardecl*>(T_PAREN_CLOSE, "function declaration argument list", [this] () {
			return funcdecl_arg();
		});

		tok++; // T_PAREN_CLOSE
		return arr;
	}

	//    func <funcname> (<arg_decl>, <arg_decl>, ...) <block>
	// or func <funcname> (<arg_decl>, <arg_decl>, ...) = (<ret_decl>, <ret_decl>) <block>
	AST* function_def () {
		auto* func = ast_alloc<AST_funcdef>(A_FUNCDEF, tok);
		tok++;

		if (tok->type != T_IDENTIFIER)
			SYNTAX_ERROR_AFTER(tok[-1], "function identifer expected after func keyword!");
		func->ident = tok->source.text();
		tok++;

		if (tok->type != T_PAREN_OPEN)
			SYNTAX_ERROR_AFTER(tok[-1], "'(' expected after function identifer!");

		func->args = funcdecl_arglist();

		// implicit (void) return list
		if (tok->type != T_ASSIGN) {
			// rets already empty
		}
		// explicit return list
		else {
			tok++;
			auto* retlist_tok = tok;

			func->rets = funcdecl_arglist();
		}

		func->body = block();

		return func;
	}

	//    <block>
	// or <if_statement>
	// or <while_loop>
	// or <do_while_loop>
	// or <for_loop>
	// or return <tuple>;   or break;   or continue;
	// or function_def
	// or <assignment_or_expression>;
	// or ;   -> empty statement, which does nothing
	AST* statement () {

		auto eat_semicolon = [this] () {
			if (tok->type != T_SEMICOLON)
				SYNTAX_ERROR_AFTER(tok[-1], "';' expected");
			tok++;
		};

		switch (tok[0].type) {
			case T_BLOCK_OPEN: 
				return block();

			case T_IF:
				return if_statement();

			case T_WHILE:
				return while_loop();
			case T_DO:
				return do_while_loop();
			case T_FOR:
				return for_loop();

			case T_RETURN: {
				return return_statement();
			}
			case T_BREAK: {
				auto* ast = ast_alloc<AST>(A_BREAK, tok++);
				eat_semicolon();
				return ast;
			}
			case T_CONTINUE: {
				auto* ast = ast_alloc<AST>(A_CONTINUE, tok++);
				eat_semicolon();
				return ast;
			}
			
			case T_STRUCT: {
				return struct_def();
			}

			case T_FUNC: {
				return function_def();
			}

			// allow empty statements
			case T_SEMICOLON: {
				tok++;
				return nullptr; // no-op
			}

			default: {
				AST* statement = decl_or_assignment_or_expression();
				eat_semicolon();
				return statement;
			}
		}
	}

	AST* _block (Token* blocktok, TokenType endtok) {
		auto* block = ast_alloc<AST_block>(A_BLOCK, blocktok);

		smallvec<AST*, 32> statements;

		while (tok->type != endtok) {
			auto* s = statement();
			if (s)
				statements.push(s);
		}

		block->statements = make_copy<AST*>(statements);

		return block;
	}

	// {
	//   <statement>
	//   <statement>
	//   ...
	// }
	AST* block () {
		if (tok->type != T_BLOCK_OPEN)
			SYNTAX_ERROR_AFTER(tok[-1], "'{' expected");

		auto* block = _block(tok++, T_BLOCK_CLOSE);

		if (tok->type != T_BLOCK_CLOSE)
			SYNTAX_ERROR_AFTER(tok[-1], "syntax error, '}' expected");

		tok++;
		return block;
	}

	// <statement>
	// <statement>
	// ...
	AST* file () {
		auto* block = _block(tok, T_EOF);

		if (tok->type != T_EOF)
			SYNTAX_ERROR_AFTER(tok[-1], "end of file expected");

		return block;
	}
};

AST* parse (Token* tokens) {
	ZoneScoped;

#if TRACY_ENABLE
	ast_nodes = 0;
#endif

	Parser parser { tokens };
	AST* ast = parser.file();

#if TRACY_ENABLE
	auto str = std::format("AST nodes: {}", ast_nodes);
	ZoneText(str.data(), str.size());
#endif

	if (options.print_ast) { // print AST
		print_seperator("AST:");
		dbg_print(ast);
	}

	return ast;
}
