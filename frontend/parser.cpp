#include "common.hpp"
#include "parser.hpp"
#include "types.hpp"

template <typename... Args>
[[noreturn]] _NOINLINE inline void SYNTAX_ERROR_AFTER (Lexer& tok, const char* format, ...) {
	va_list vl;
	va_start(vl, format);
	auto str = vprints(format, vl);
	va_end(vl);

	_ERROR("syntax error", SourceRange::after_tok(tok[-1].src), std::move(str));
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
			std::string str(lit->src.text());
			printf(" %s\n", str.c_str());
		} break;

		case A_VARDECL: { auto* vardecl = (AST_vardecl*)node;
			std::string str(vardecl->ident);
			printf(" %s (\n", str.c_str()); // , Type_str[var->valtype]
			
			indent(depth); printf(")\n");
		} break;

		case A_FUNCARG: { auto* arg = (AST_func_arg*)node;
			printf("(\n");

			dbg_print(arg->decl, depth+1);
			dbg_print(arg->init, depth+1);

			indent(depth); printf(")\n");
		} break;

		case A_VARREF: { auto* var = (AST_var*)node;
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

		case A_TUPLE:
		case A_VARDECL_LIST: { auto* list = (AST_list*)node;
			printf(" (\n");
			for (auto* e : list->elements)
				dbg_print(e, depth+1);
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

		case A_STRUCTDECL: { auto* struc = (AST_structdef*)node;
			
			printf(" (\n");
			for (auto* n : struc->members)
				dbg_print(n, depth+1);
			indent(depth); printf(")\n");
			
		} break;

		case A_FUNCDECL: { auto* f = (AST_funcdef*)node;
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
	Lexer tok;
	
	template <typename... Args>
	void expect (TokenType type, const char* format, Args... args) {
		if (tok[0].type != type) {
			SYNTAX_ERROR_AFTER(tok, format, args...);
		}
		tok.eat();
	}
	bool try_eat (TokenType type) {
		if (tok[0].type != type)
			return false;
		tok.eat();
		return true;
	}

	// <element>, <element> [,] <endtok>
	// ie. comma seperated list of elements, with optional trailing comma, followed by mandatory end token
	template <typename T, typename FUNC>
	arrview<T> comma_seperated_list (TokenType endtok, FUNC element) {
		smallvec<T, 32> list;

		while (tok[0].type != endtok) {
			list.push( element() );

			if (!try_eat(T_COMMA))
				break;
		}

		return make_copy<T>(list);
	}

	
	AST_literal* literal_ () {
		auto* lit = ast_alloc<AST_literal>(A_LITERAL, tok[0]);

		auto start = tok[0].src.start;
		auto end   = start + tok[0].src.length;

		auto lit_type = tok.parse_literal(tok[0].type, start, end, &lit->value);

		lit->type = Typeref::RValue( BASIC_TYPES[lit_type] );
		tok.eat();

		return lit;
	}
	
	//    <expression>
	// or <argname> = <expression>
	AST_callarg* call_arg () {
		auto src = tok[0].src;

		auto* arg = ast_alloc<AST_callarg>(A_CALLARG);
		arg->ident = strview();

		if (tok[0].type == T_IDENTIFIER && tok[1].type == T_ASSIGN) {
			arg->ident = tok[0].src.text();
			tok.eat();
			tok.eat();
		}

		arg->expr = expression();
		
		arg->src = SourceRange::range(src, tok[-1].src); // 'arg = expr' is the src range
		return arg;
	}

	AST_call* func_call () {
		assert(tok[0].type == T_IDENTIFIER && tok[1].type == T_PAREN_OPEN);
		
		auto src = tok[0].src;

		AST_call* call = ast_alloc<AST_call>(A_CALL);
		call->ident = tok[0].src.text();
		tok.eat(); // T_IDENTIFIER

		tok.eat(); // T_PAREN_OPEN

		call->args = comma_seperated_list<AST_callarg*>(T_PAREN_CLOSE, [this] () { return call_arg(); });
		
		expect(T_PAREN_CLOSE, "expected ')' after call argument list!");

		call->src = SourceRange::range(src, tok[-1].src);

		return call;
	}
	AST_var* single_var () {
		assert(tok[0].type == T_IDENTIFIER);

		auto* var = ast_alloc<AST_var>(A_VARREF, tok[0]);
		var->ident = tok[0].src.text();
		tok.eat();

		return var;
	}
	
	static bool is_expression (TokenType tok_type) {
		switch (tok_type) {
			case T_PAREN_OPEN:
				return true;

			case T_IDENTIFIER:
				return true;

			case T_LITERAL_BOOL:
			case T_LITERAL_INT:
			case T_LITERAL_FLT:
			case T_LITERAL_STR:
				return true;

			default:
				return false;
		}
	}
	AST* atom () {
		switch (tok[0].type) {

			case T_PAREN_OPEN: {
				tok.eat();
				return expression_or_expr_list();
			}

			case T_IDENTIFIER: {
				// func call
				if (tok[1].type == T_PAREN_OPEN) {
					return func_call();
				}
				// single variable
				else {
					return single_var();
				}
			}

			case T_LITERAL_BOOL:
			case T_LITERAL_INT:
			case T_LITERAL_FLT:
			case T_LITERAL_STR: {
				return literal_();
			}
			
			default: {
				SYNTAX_ERROR_AFTER(tok, "number or variable expected after operator");
				return nullptr;
			}
		}
	}

	// expression consisting of atoms combined with unary, binary or ternary operators parsed according to associativity and precedence rules
	AST* expression (unsigned min_prec=0) {

		AST* lhs;

		// unary prefix operators are right associative, so are parsed by recursing into expression(prec) with prec = unary op precendence
		if (is_unary_prefix_op(tok[0].type)) {
			unsigned prec = un_prec(tok[0].type);

			auto src = tok[0].src;

			auto* unary_op = ast_alloc<AST_unop>(A_UNOP);
			unary_op->op = tok2unop_prefix(tok[0].type);
			tok.eat();
			
			unary_op->operand = expression(prec);
			unary_op->src = SourceRange::range(src, tok[-1].src); // '-a' is the src range

			lhs = unary_op;
		}
		// once all unary operators are parsed right-recursion stops
		else {
			lhs = atom();
		}

		// unary postfix operators are left-associative, so are parsed by a loop
		while (is_unary_postfix_op(tok[0].type)) {
			unsigned prec = un_prec(tok[0].type);
			if (prec < min_prec) // handle precedence rules
				return lhs;

			auto src = tok[0].src;

			auto* post_op = ast_alloc<AST_unop>(A_UNOP);
			post_op->op = tok2unop_postfix(tok[0].type);
			tok.eat();

			post_op->operand = lhs;
			post_op->src = SourceRange::range_with_arrow(lhs->src, src, src); // 'a++' is the src range

			lhs = post_op;
		}

		// binary and ternary operators
		while (is_binary_or_ternary_op(tok[0].type)) {
			unsigned prec  = bt_prec( tok[0].type);
			unsigned assoc = bt_assoc(tok[0].type);

			if (prec < min_prec)
				break; // handle precedence rules

			Token op_tok = tok[0];
			tok.eat();

			AST* rhs = expression(assoc == LEFT_ASSOC ? prec+1 : prec);

			AST* bop;
			// special case: ternary operator
			if (op_tok.type == T_QUESTIONMARK) {
				expect(T_COLON, "':' expected after true case of select operator");

				auto* op = ast_alloc<AST_if>(A_SELECT);
				op->cond     = lhs;
				op->if_body  = rhs;

				assert(assoc == RIGHT_ASSOC);
				op->else_body = expression(prec);
				
				bop = op;
			}
			// normal binary operator
			else {
				auto* op = ast_alloc<AST_binop>(A_BINOP);
				op->op = tok2binop(op_tok.type);
				op->lhs = lhs;
				op->rhs = rhs;

				bop = op;
			}

			bop->src = SourceRange::range_with_arrow(lhs->src, op_tok.src, tok[-1].src); // 'a + b' or 'cond ? a : b' is the src range
			lhs = bop;
		}

		return lhs;
	}
	
	//    <expression>                     -> returns expression as AST*
	// or <expression>,                    -> returns AST_expr_list* as AST* (single expression as expression list)
	// or <expression>, <expression> [,]   -> returns AST_expr_list* as AST*
	// ie. comma seperated list of expressions, with optional trailing comma followed by endtok
	AST* expression_or_expr_list (arrview<Token> list_start = {}) {
		auto src = tok[0].src;
		
		AST* expr = expression();

		if (tok[0].type == T_ASSIGN || tok[0].type == T_SEMICOLON)
			return expr; // single expression

		smallvec<AST*, 32> list;
		list.push(expr);

		while (try_eat(T_COMMA) && !(tok[0].type == T_ASSIGN || tok[0].type == T_SEMICOLON)) {
			list.push( expression() );
		}

		auto* expr_list = ast_alloc<AST_list>(A_TUPLE);
		expr_list->elements = make_copy<AST*>(list);

		expr_list->src = SourceRange::range(src, tok[-1].src);
		return expr_list;
	}

	// lhs = rhs   or  lhs += rhs  etc.
	AST_binop* assignop (AST* lhs_expr) {
		assert(is_binary_assignemnt_op(tok[0].type));
		
		auto src = tok[0].src;

		auto* op = ast_alloc<AST_binop>(A_ASSIGNOP);
		op->op = tok2assignop(tok[0].type);
		tok.eat();

		op->lhs = lhs_expr;
		op->rhs = expression_or_expr_list();
				
		op->src = SourceRange::range_with_arrow(op->lhs->src, src, op->rhs->src); // 'a = b' is the src range
		return op;
	}

	//    <varname>                -> variable declaration with inferred type
	// or <varname> : <typename>   -> variable declaration with explicit type
	AST_vardecl* var_decl () {
		auto src = tok[0].src;

		auto* decl = ast_alloc<AST_vardecl>(A_VARDECL);
		decl->ident = tok[0].src.text();
		
		expect(T_IDENTIFIER, "expected variable identifier");
		
		// type specifier
		if (tok[0].type == T_COLON) {
			tok.eat();

			// ident specifies type, but these identifiers can only be matched to AST_Type's in the later phase
			// leave later phase to find this on it's own
			decl->typeexpr = tok[0].src;
			expect(T_IDENTIFIER, "expected type identifier");
		}

		decl->src = SourceRange::range_with_arrow(src, src, tok[-1].src); // 'a' or 'a : type' is the src range
		return decl;
	}

	AST* var_decl_list () {
		auto src = tok[0].src;

		if (tok[0].type == T_VAR) // T_VAR optional (func arg lists do not need var)
			tok.eat();
		
		auto* first = var_decl();

		if (tok[0].type == T_ASSIGN || tok[0].type == T_SEMICOLON)
			return first; // single var decl
		
		smallvec<AST*, 32> list;
		list.push(first);
		
		while (try_eat(T_COMMA) && !(tok[0].type == T_ASSIGN || tok[0].type == T_SEMICOLON)) {
			list.push(var_decl());
		}
		
		// var decl list
		auto* decll = ast_alloc<AST_list>(A_VARDECL_LIST);
		decll->elements = make_copy<AST*>(list);
		
		decll->src = SourceRange::range(src, tok[-1].src);
		return decll;
	}

	// return <return_args>;
	AST* return_statement () {
		auto src = tok[0].src;

		auto* ast = ast_alloc<AST_return>(A_RETURN);
		tok.eat();

		ast->args = comma_seperated_list<AST_callarg*>(T_SEMICOLON, [this] () { return call_arg(); });

		ast->src = SourceRange::range(src, tok[-1].src); // 'return arg = expr, expr2' is the src range
		return ast;
	}

	// just a normal expression for now, but could enable more rules
	AST* boolean_expression () {
		return expression();
	}

	// parses  if <cond> {} elif <cond> {} elif <cond> else {} into a recursive if-else chain (else body points to new recursive if-else for elif)
	// where each elif and else is optional (else = null)
	AST_if* if_statement () {
		auto* aif = ast_alloc<AST_if>(A_IF, tok[0]); 
		tok.eat();

		aif->cond      = boolean_expression();

		aif->if_body   = block();
		aif->else_body = elif_statement();

		return aif;
	}
	AST* elif_statement () {
		if (tok[0].type == T_ELIF) {
			// code is same as if except that keyword happens to be elif instead of if (this is why C++ does else if)
			// solve via recurse
			return if_statement();
		}
		if (tok[0].type == T_ELSE) {
			tok.eat();
			return block();
		}
		return nullptr;
	}

	// while <cond> <block>
	AST* while_loop () {
		auto* loop = ast_alloc<AST_loop>(A_WHILE, tok[0]);
		tok.eat();

		loop->cond = boolean_expression();
		loop->body = block();

		return loop;
	}

	// do <block> while <cond>
	// NOTE: that <cond> has special scoping rules to allow it to access objects from inside the block
	AST* do_while_loop () {
		auto* loop = ast_alloc<AST_loop>(A_DO_WHILE, tok[0]); // TODO: should the entire 'do {} while()' be the source range  
		tok.eat();

		loop->body = block();

		if (tok[0].type != T_WHILE)
			SYNTAX_ERROR_AFTER(tok, "while expected after do block!");
		tok.eat();

		loop->cond = boolean_expression();
		expect(T_SEMICOLON, "';' expected");

		return loop;
	}

	// for [start]; <cond>; [end] <block>
	AST* for_loop () {
		auto* loop = ast_alloc<AST_loop>(A_FOR, tok[0]);
		tok.eat();

		loop->start = basic_statement();
		expect(T_SEMICOLON, "';' expected");

		loop->cond  = boolean_expression();
		expect(T_SEMICOLON, "';' expected");

		loop->end   = basic_statement();

		loop->body = block();

		return loop;
	}
	
	AST* struct_def () {
		auto src = tok[0].src;

		auto* struc = ast_alloc<AST_structdef>(A_STRUCTDECL);
		tok.eat();
		
		expect(T_IDENTIFIER, "struct identifer expected after struct keyword!");

		struc->ident = tok[-1].src.text();
		struc->src = SourceRange::range(src, tok[-1].src); // 'struct name' is the src range

		smallvec<AST_vardecl*, 16> members;

		expect(T_BLOCK_OPEN, "'{' expected after struct identifier");

		while (tok[0].type != T_BLOCK_CLOSE) {
			auto* var = (AST_vardecl*)var_decl();
			assert(var->kind == A_VARDECL);

			if (tok[0].type == T_ASSIGN)
				ERROR(tok[0].src, "vardecl initialization not allowed in struct (yet)");

			if (var)
				members.push(var);
			expect(T_SEMICOLON, "';' expected");
		}

		struc->members = make_copy<AST_vardecl*>(members);

		expect(T_BLOCK_CLOSE, "syntax error, '}' expected");
		
		return struc;
	}
	
	AST_func_arg* funcdecl_arg () {
		auto* arg = ast_alloc<AST_func_arg>(A_FUNCARG);
		
		auto src = tok[0].src;

		arg->decl = var_decl();
		
		if (try_eat(T_ASSIGN)) {
			// TOOD: implement at const-foldable expression for default args at the very least
			// better yet allow things like  sqrt(5)  or even custom compile-time const functions to be called as well
			// or just const values in general (const globals or const locally captured vars)
			// the question is where the const folding happens -> wait until I get to actually implementing compile-time execution

			// only expression, not expression_or_expr_list() since we are already in a comma-seperated-list
			arg->init = expression();

			if (arg->init->kind != A_LITERAL)
				ERROR(arg->init->src, "only literals allowed as default argument values (for now)");
			
			assert(arg->init->type.ty && arg->init->type.rval);
		}

		arg->src = SourceRange::range(src, tok[-1].src);
		return arg;
	}

	//    func <funcname> (<arg_decl>, <arg_decl>, ...) <block>
	// or func <funcname> (<arg_decl>, <arg_decl>, ...) = (<ret_decl>, <ret_decl>) <block>
	AST* function_def () {
		auto src = tok[0].src;

		auto* func = ast_alloc<AST_funcdef>(A_FUNCDECL);
		tok.eat();
		
		if (tok[0].type != T_IDENTIFIER)
			SYNTAX_ERROR_AFTER(tok, "function identifer expected after func keyword!");
		func->ident = tok[0].src.text();
		tok.eat();

		expect(T_PAREN_OPEN, "'(' expected for function argument list!");
		func->args = comma_seperated_list<AST_func_arg*>(T_PAREN_CLOSE, [this] () { return funcdecl_arg(); });
		expect(T_PAREN_CLOSE, "expected ')' after function declaration argument list!");
		
		if (try_eat(T_ASSIGN)) {
			// explicit return list
			
			auto rets_src = tok[0].src;
			
			expect(T_PAREN_OPEN, "'(' expected for function returns list!");
			func->rets = comma_seperated_list<AST_func_arg*>(T_PAREN_CLOSE, [this] () { return funcdecl_arg(); });
			expect(T_PAREN_CLOSE, "expected ')' after function declaration return list!");

			if (func->rets.count > 1) {
				// Create struct for return values
				func->ret_struct = ast_alloc<AST_structdef>(A_STRUCTDECL);
				func->ret_struct->src = SourceRange::range(rets_src, tok[-1].src);
				func->ret_struct->ident = format("%.*s.Result", (int)func->ident.size(), func->ident.data());
				
				auto* arr = g_allocator.alloc_array<AST_vardecl*>(func->rets.count);
				for (size_t i=0; i<func->rets.count; ++i) {
					arr[i] = func->rets[i]->decl;
				}
				func->ret_struct->members = arrview<AST_vardecl*>(arr, func->rets.count);

				// Create type for return struct
				func->ret_struct_ty = ast_alloc<AST_type>(A_TYPE);
				func->ret_struct_ty->tclass = TY_STRUCT;
				func->ret_struct_ty->ident  = func->ret_struct->ident;
				func->ret_struct_ty->decl   = func->ret_struct;
				func->ret_struct_ty->src    = func->ret_struct->src;
			}
		} else {
			// implicit (void) return list
		}

		func->src = SourceRange::range(src, tok[-1].src); // 'func name (...) = (...)' is the src range

		func->body = block();

		return func;
	}

	// var_decl or assignop or expression-statement
	AST* basic_statement () {
		// allow empty statements
		if (tok[0].type == T_SEMICOLON)
			return nullptr; // no-op
		
		AST* expr;

		// variable declaration
		if (tok[0].type == T_VAR)
			expr = var_decl_list();
		else 
			expr = expression_or_expr_list();

		if (is_binary_assignemnt_op(tok[0].type))
			return assignop(expr);

		return expr;
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
		AST* statement;
		switch (tok[0].type) {
			
		//// non-statments (not followed by ';')
			// nested block
			case T_BLOCK_OPEN: 
				return block();

			// struct declaration
			case T_STRUCT:
				return struct_def();
				
			// function declaration
			case T_FUNC:
				return function_def();

			// control-flow structures
			case T_IF:
				return if_statement();

			case T_WHILE:
				return while_loop();
			case T_DO:
				return do_while_loop();
			case T_FOR:
				return for_loop();

		////statments followed by ';'
			case T_RETURN: {
				statement = return_statement();
			} break;
			case T_BREAK: {
				statement = ast_alloc<AST>(A_BREAK, tok[0]);
				tok.eat();
			} break;
			case T_CONTINUE: {
				statement = ast_alloc<AST>(A_CONTINUE, tok[0]);
				tok.eat();
			} break;
			
			default: {
				statement = basic_statement();
			} break;
		}
		
		expect(T_SEMICOLON, "';' expected");
		return statement;
	}

	AST_block* _block (TokenType endtok) {
		auto* block = ast_alloc<AST_block>(A_BLOCK);

		smallvec<AST*, 32> statements;

		while (tok[0].type != endtok) {
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
	AST_block* block () {
		auto src = tok[0].src;
		
		expect(T_BLOCK_OPEN, "'{{' expected");

		auto* block = _block(T_BLOCK_CLOSE);
		
		expect(T_BLOCK_CLOSE, "'}' expected");
		
		block->src = SourceRange::range(src, tok[-1].src); // '{ ... }' is the src range

		return block;
	}

	// <statement>
	// <statement>
	// ...
	AST_block* file () {
		auto src = tok[0].src;

		auto* block = _block(T_EOF);

		if (tok[0].type != T_EOF)
			SYNTAX_ERROR_AFTER(tok, "end of file expected");

		block->src = SourceRange::range(src, tok[-1].src); // whole file is the src range

		return block;
	}
};

void parse (AST_Module& modl, strview src) {
	ZoneScoped;
	
#if TRACY_ENABLE
	ast_nodes = 0;
#endif
	
	Parser parser { Lexer{src} };

	modl.ast = parser.file();

#if TRACY_ENABLE
	auto str = prints("AST nodes: %llu", ast_nodes);
	ZoneText(str.data(), str.size());
#endif

	if (options.print_ast) { // print AST
		print_seperator("AST:");
		dbg_print(modl.ast);
	}
}
