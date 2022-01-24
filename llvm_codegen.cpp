#include "llvm_pch.hpp"
#include "llvm_backend.hpp"

#include "common.hpp"
#include "builtins.hpp"

llvm::LLVMContext ctx;
llvm::IRBuilder<llvm::NoFolder> build{ctx};

struct LLVM_gen {
	std::vector<AST_funcdef*>& funcdefs;

	llvm::Module* modl;
	
	void declare_builtins () {
		// TODO: do this lazily once they are actually called?

		for (auto* builtin : builtin_funcs) {
			declare_function(builtin);
		}
	}
		
	#define PROFILE_DUPLICATE_FUNCS 1
	
#if PROFILE_DUPLICATE_FUNCS
	bool _is_builtin = true;
	int _dupl_func_i = 0;
#endif

	void generate (strview const& filename) {
		ZoneScoped;

		modl = new llvm::Module("<main>", ctx);
		modl->setSourceFileName(SR(filename));

		// declare builtin functions
		declare_builtins();

	#if PROFILE_DUPLICATE_FUNCS
		_is_builtin = false;
		for (_dupl_func_i=0; _dupl_func_i<10000; ++_dupl_func_i) {
	#endif

		// declare functions declared in source
		for (size_t funcid = 0; funcid < funcdefs.size(); ++funcid) {
			declare_function(funcdefs[funcid]);
		}

		// now that all callable functions are declared
		// generate IR for functions defined in source
		for (size_t funcid = 0; funcid < funcdefs.size(); ++funcid) {
			codegen_function(funcdefs[funcid]);
		}

	#if PROFILE_DUPLICATE_FUNCS
		}
	#endif

		if (options.print_ir) {
			print_seperator("LLVM IR");
			modl->print(llvm::errs(), nullptr);
		}
	}

	llvm::Function* declare_function (AST_funcdef* fdef) {
		// returns
		llvm::Type* ret_ty = llvm::Type::getVoidTy(ctx);
		assert(fdef->retc <= 1); // TODO: implememt multiple return values
		
		for (auto* ret = (AST_vardecl*)fdef->rets; ret != nullptr; ret = (AST_vardecl*)ret->next) {
			assert(ret->type != A_VARARGS);
			ret_ty = map_type(ret->valtype);
		}

		// arguments
		llvm::SmallVector<llvm::Type*, 16> args_ty;
		bool vararg = false;
		
		for (auto* arg = (AST_vardecl*)fdef->args; arg != nullptr; arg = (AST_vardecl*)arg->next) {
			if (arg->type == A_VARARGS) {
				vararg = true;
				assert(!arg->next);
			}
			else {
				args_ty.push_back(map_type(arg->valtype));
			}

			arg->is_arg = true;
			if (arg->init) {
				// TODO
				assert(false);
			}
		}

		auto* func_ty = llvm::FunctionType::get(ret_ty, args_ty, vararg);
		
	#if PROFILE_DUPLICATE_FUNCS
		llvm::Function* func;
		if (_is_builtin)
			func =  llvm::Function::Create(func_ty, llvm::Function::InternalLinkage, SR(fdef->ident), *modl);
		else
			func =  llvm::Function::Create(func_ty, llvm::Function::InternalLinkage, llvm::Twine(prints("_%d_", _dupl_func_i)) + SR(fdef->ident), *modl);
	#else
		auto* func =  llvm::Function::Create(func_ty, llvm::Function::InternalLinkage, SR(fdef->ident), *modl);
	#endif

		// set argument names
		unsigned i = 0;
		for (auto* arg = (AST_vardecl*)fdef->args; arg != nullptr && arg->type != A_VARARGS; arg = (AST_vardecl*)arg->next) {
			arg->llvm_value = func->getArg(i++);
			arg->llvm_value->setName(SR(arg->ident));
		}

		fdef->llvm_func = func;
		return func;
	}
	
	AST_funcdef*      cur_fdef;
	llvm::Function*   cur_func;
	llvm::BasicBlock* entry_block;

	llvm::BasicBlock* ib_block = nullptr; // ib = insert before   -> to preserve block order (keep it in source code order)
	
	// stack of loop blocks to allow for break and continue anywhere (except outside of loop, where loop_blocks will be empty)
	struct LoopBlocks {
		llvm::BasicBlock* break_block;    // loop cont  ie. block after loop
		llvm::BasicBlock* continue_block; // loop end   ie. block after loop body, but befor cond
	};
	llvm::SmallVector<LoopBlocks, 16> loop_blocks;

	// Improve names of llvm values and basic blocks by manually keeping count
	// TODO: This could actually impact compiler perf, so consider not even generating names at all in non-debug info builds
	// The way that allows for the least overhead while still allowing runtime switching of debug-info on/off
	// is to make the while codegen pass templated with a template<bool DebugInfo> and instantiate it twice
	size_t if_count = 0;
	size_t loop_count = 0;
	size_t cont_count = 0;
	size_t strlit_count = 0;

	struct _IDFormStrbuf { char str[32]; } format_id (size_t id) {
		_IDFormStrbuf buf; // fits any 64-bit int
		auto len = llvm::format("%" PRIu64, id).print(buf.str, ARRLEN(buf.str));
		if (len > ARRLEN(buf.str)) {
			// fail
			assert(false);
			buf.str[0] = '\0';
		}
		return buf;
	}

	llvm::Type* map_type (Type type) {
		switch (type) {
			case VOID: return llvm::Type::getVoidTy(ctx);
			case BOOL: return llvm::Type::getInt1Ty(ctx);
			case INT:  return llvm::Type::getInt64Ty(ctx);
			case FLT:  return llvm::Type::getDoubleTy(ctx);
			case STR:  return llvm::Type::getInt8PtrTy(ctx);
			INVALID_DEFAULT;
		}
	}

	void codegen_function (AST_funcdef* fdef) {
		ZoneScoped;

		cur_fdef = fdef;
		cur_func = fdef->llvm_func;

		//// entry block + start recursively codegening of the function body
		entry_block = llvm::BasicBlock::Create(ctx, "entry", cur_func);
		build.SetInsertPoint(entry_block);

		for (auto* ret = (AST_vardecl*)fdef->rets; ret != nullptr; ret = (AST_vardecl*)ret->next) {
			assert(ret->type != A_VARARGS);
			
			declare_local_var(ret);
		}

		codegen(fdef->body);
		
		//// implicit return instruction at end of function body
		return_ret_vars();
		
		//// finish function
		verifyFunction(*fdef->llvm_func);

		cur_func = nullptr;
		entry_block = nullptr;
	}
	
	llvm::Value* codegen_unary (AST_unop* op, llvm::Value* operand) {
		switch (op->op) {
			case OP_POSITIVE: {
				switch (op->valtype) {
					case INT:
					case FLT: return operand; // no-op
					default: throw CompilerExcept{"error: positive operator is not valid for type", op->src_tok->source};
				}
			}
			case OP_NEGATE: {
				switch (op->valtype) {
					case INT: return build.CreateNeg (operand, "_neg");
					case FLT: return build.CreateFNeg(operand, "_neg");
					default: throw CompilerExcept{"error: negate is not valid for type", op->src_tok->source};
				}
			}
			case OP_NOT: {
				switch (op->valtype) {
					case INT :
					case BOOL: return build.CreateNot(operand, "_not");
					default: throw CompilerExcept{"error: not is not valid for type", op->src_tok->source};
				}
			}
			case OP_INC: {
				switch (op->valtype) {
					case INT : return build.CreateAdd(operand, llvm::ConstantInt::get(operand->getType(), 1), "_inc");
					default: throw CompilerExcept{"error: increment is not valid for type", op->src_tok->source};
				}
			}
			case OP_DEC: {
				switch (op->valtype) {
					case INT : return build.CreateSub(operand, llvm::ConstantInt::get(operand->getType(), 1), "_dec");
					default: throw CompilerExcept{"error: decrement is not valid for type", op->src_tok->source};
				}
			}
			INVALID_DEFAULT;
		}
	}
	llvm::Value* codegen_binop (AST_binop* op, llvm::Value* lhs, llvm::Value* rhs) {
		assert(op->lhs->valtype == op->rhs->valtype);

		switch (op->valtype) {
		case INT: {
			switch (op->op) {
				case OP_ADD:        return build.CreateAdd (lhs, rhs, "_add");
				case OP_SUB:        return build.CreateSub (lhs, rhs, "_sub");
				case OP_MUL:        return build.CreateMul (lhs, rhs, "_mul");
				case OP_DIV:        return build.CreateSDiv(lhs, rhs, "_div");
				case OP_MOD:        return build.CreateURem(lhs, rhs, "_mod"); // TODO: is URem the correct thing for  sint % sint  ?
				case OP_LESS:       return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT, lhs, rhs, "_cmp");
				case OP_LESSEQ:     return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, lhs, rhs, "_cmp");
				case OP_GREATER:    return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT, lhs, rhs, "_cmp");
				case OP_GREATEREQ:  return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, lhs, rhs, "_cmp");
				case OP_EQUALS:     return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ , lhs, rhs, "_cmp");
				case OP_NOT_EQUALS: return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_NE , lhs, rhs, "_cmp");
				INVALID_DEFAULT;
			}
		} break;
		case BOOL: {
			switch (op->op) {
				case OP_ADD:        
				case OP_SUB:        
				case OP_MUL:        
				case OP_DIV:        
				case OP_MOD:        
					throw CompilerExcept{ "error: math ops not valid for this type", op->src_tok->source };
		
				case OP_LESS:       
				case OP_LESSEQ:     
				case OP_GREATER:    
				case OP_GREATEREQ:  
					throw CompilerExcept{ "error: can't compare bools like that", op->src_tok->source };
		
				case OP_EQUALS:     return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, lhs, rhs, "_cmp");
				case OP_NOT_EQUALS: return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_NE, lhs, rhs, "_cmp");
				INVALID_DEFAULT;
			}
		} break;
		case FLT: {
			switch (op->op) {
				case OP_MOD:
					throw CompilerExcept{ "error: remainder operator not valid for floats", op->src_tok->source };
				case OP_ADD:        return build.CreateFAdd(lhs, rhs, "_add");
				case OP_SUB:        return build.CreateFSub(lhs, rhs, "_sub");
				case OP_MUL:        return build.CreateFMul(lhs, rhs, "_mul");
				case OP_DIV:        return build.CreateFDiv(lhs, rhs, "_div");
				
				// always ordered comparisons (NaN behavior)
				case OP_LESS:       return build.CreateFCmp(llvm::CmpInst::Predicate::FCMP_OLT, lhs, rhs, "_cmp");
				case OP_LESSEQ:     return build.CreateFCmp(llvm::CmpInst::Predicate::FCMP_OLE, lhs, rhs, "_cmp");
				case OP_GREATER:    return build.CreateFCmp(llvm::CmpInst::Predicate::FCMP_OGT, lhs, rhs, "_cmp");
				case OP_GREATEREQ:  return build.CreateFCmp(llvm::CmpInst::Predicate::FCMP_OGE, lhs, rhs, "_cmp");
				case OP_EQUALS:     return build.CreateFCmp(llvm::CmpInst::Predicate::FCMP_OEQ , lhs, rhs, "_cmp");
				case OP_NOT_EQUALS: return build.CreateFCmp(llvm::CmpInst::Predicate::FCMP_ONE , lhs, rhs, "_cmp");
				INVALID_DEFAULT;
			}
		} break;
		default:
			throw CompilerExcept{ "error: math ops not valid for this type", op->src_tok->source };
		}
	}

	void declare_local_var (AST_vardecl* vardecl) {
		llvm::IRBuilder<llvm::NoFolder> tmp_build(entry_block, entry_block->begin());
		vardecl->llvm_value = tmp_build.CreateAlloca(map_type(vardecl->valtype), nullptr, SR(vardecl->ident));
	}

	// get alloca-ptr from local variable (not function arguments, since they are not allocas)
	llvm::Value* local_var_ptr (AST* op, AST_vardecl* vardecl) {
		assert(vardecl->type == A_VARDECL);

		if (vardecl->is_arg)
			throw CompilerExcept{"error: cannot operate on function arugment since arguments are immutable", op->src_tok->source};

		assert(vardecl->llvm_value);
		return vardecl->llvm_value; // should be a llvm::AllocaInst
	}

	void return_ret_vars () {
		// TODO: implement multiple returns
		assert(cur_fdef->retc <= 1);

		if (cur_fdef->retc == 1) {
			auto* vardecl = (AST_vardecl*)cur_fdef->rets;
			
			llvm::Value* ret = build.CreateLoad(map_type(vardecl->valtype), vardecl->llvm_value, vardecl->llvm_value->getName());

			build.CreateRet(ret);
		}
		else {
			build.CreateRetVoid();
		}
	}

	void unreachable () {
		// TODO: code beyond here (in the same block) is unreachable, what do?
		
		// This other code to fail
		//build.ClearInsertionPoint();
		
		// Could also manually keep track of when code is unreachable and skip generating instruction
		// but that is somewhat complicated, but might be faster than creating unreachable blocks

		// Create block that is unreachable
		auto* bb = llvm::BasicBlock::Create(ctx, "unreach", cur_func, ib_block);
		build.SetInsertPoint(bb);
	}

	llvm::Value* codegen (AST* ast) {
		switch (ast->type) {
		case A_LITERAL: {
			auto* lit = (AST_literal*)ast;
				
			llvm::Value* val;
			switch (lit->valtype) {
				case BOOL:
					val = llvm::ConstantInt::getBool(ctx, lit->value.b);
					break;
				case INT:
					val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx),
						llvm::APInt(64, (uint64_t)lit->value.i, true));
					break;
				case FLT:
					val = llvm::ConstantFP::get(llvm::Type::getDoubleTy(ctx),
						llvm::APFloat(lit->value.f));
					break;
				case STR:
					val = build.CreateGlobalStringPtr(lit->value.str, llvm::Twine("strlit.") + format_id(strlit_count++).str);
					break;
				INVALID_DEFAULT;
			}
			assert(val->getType() == map_type(lit->valtype));
			return val;
		}

		case A_VARDECL: {
			auto* vardecl = (AST_vardecl*)ast;

			declare_local_var(vardecl);

			if (vardecl->init) {
				// eval rhs
				auto* val = codegen(vardecl->init);
				// assign to new var
				build.CreateStore(val, vardecl->llvm_value);
			}
			return {};
		}

		case A_VAR: {
			auto* var = (AST_var*)ast;
			auto* vardecl = (AST_vardecl*)var->decl;
				
			if (vardecl->is_arg) {
				return vardecl->llvm_value;
			}
			else {
				return build.CreateLoad(map_type(var->valtype), vardecl->llvm_value, vardecl->llvm_value->getName());
			}
		}

		case A_ASSIGNOP: {
			auto* op = (AST_binop*)ast;

			if (op->lhs->type != A_VAR)
				throw CompilerExcept{"error: expected variable for assignop lhs", ast->src_tok->source};

			auto* var = (AST_var*)op->lhs;
			llvm::Value* var_ptr = local_var_ptr(op, var->decl);

			llvm::Value* result;

			if (op->op == OP_ASSIGN) {
				// simple assignment of rhs to lhs
				result = codegen(op->rhs);
			}
			else {
				// load lhs first
				llvm::Value* lhs = build.CreateLoad(map_type(op->valtype), var_ptr, var_ptr->getName());
				// eval rhs
				llvm::Value* rhs = codegen(op->rhs);
				// eval binary operator and assign to lhs
				result = codegen_binop(op, lhs, rhs);
			}

			build.CreateStore(result, var_ptr);
			return {};
		}

		case A_BINOP: {
			auto* op = (AST_binop*)ast;
				
			assert(op->valtype      == op->lhs->valtype);
			assert(op->lhs->valtype == op->rhs->valtype);
				
			// eval lhs
			llvm::Value* lhs = codegen(op->lhs);
			// eval rhs
			llvm::Value* rhs = codegen(op->rhs);
			// eval binary operator
			return codegen_binop(op, lhs, rhs);
		}

		case A_UNOP: {
			auto* op = (AST_unop*)ast;
			assert(op->valtype == op->operand->valtype);
				
			llvm::Value* old_val = codegen(op->operand);
			llvm::Value* result  = codegen_unary(op, old_val);
				
			switch (op->op) {
				case OP_POSITIVE:
				case OP_NEGATE: case OP_NOT: {
					return result;
				}

				case OP_INC: case OP_DEC: {
					if (op->operand->type != A_VAR)
						throw CompilerExcept{"error: expected variable for unary post operator", ast->src_tok->source};
					auto* var = (AST_var*)op->operand;

					llvm::Value* var_ptr = local_var_ptr(op, var->decl);

					// assign inc/decremented to var
					build.CreateStore(result, var_ptr);
					// return old value
					return old_val;
				}
			}
		}

		case A_BLOCK: {
			auto* block = (AST_block*)ast;

			for (auto* n=block->statements; n != nullptr; n = n->next) {
				codegen(n);
			}
			return {};
		}
			
		case A_IF:
		case A_SELECT: {
			auto* aif = (AST_if*)ast;

			auto id = format_id(if_count++);
			auto cid = format_id(cont_count++);

			auto* then_block =                  llvm::BasicBlock::Create(ctx, llvm::Twine("if.") + id.str + ".then", cur_func, ib_block);
			auto* else_block = aif->else_body ? llvm::BasicBlock::Create(ctx, llvm::Twine("if.") + id.str + ".else", cur_func, ib_block) : nullptr;
			auto* cont_block =                  llvm::BasicBlock::Create(ctx, llvm::Twine("cont.") + cid.str       , cur_func, ib_block);
				
			llvm::Value      *true_result,    *false_result;
			llvm::BasicBlock *phi_then_block, *phi_else_block;

			{ // condition at end of current block
				ib_block = then_block;

				llvm::Value* cond = codegen(aif->cond);
				build.CreateCondBr(cond, then_block, aif->else_body ? else_block : cont_block);
			}

			{ // generate then block
				ib_block = else_block ? else_block : cont_block;

				build.SetInsertPoint(then_block);

				true_result = codegen(aif->if_body);
				phi_then_block = build.GetInsertBlock(); // nested ifs or loops (in codegen) change the current block
				
				build.CreateBr(cont_block);
			}

			if (else_block) { // generate else block
				ib_block = cont_block;
				
				build.SetInsertPoint(else_block);
					
				false_result = codegen(aif->else_body);
				phi_else_block = build.GetInsertBlock();
					
				build.CreateBr(cont_block);
			}
				
			// following code will be in cont block
			ib_block = nullptr;

			build.SetInsertPoint(cont_block);
				
			if (aif->type == A_SELECT) {
				assert(else_block);
				assert(true_result && false_result);
				assert(true_result->getType() == false_result->getType());
				
				llvm::PHINode* result = build.CreatePHI(true_result->getType(), 2, "_sel");
				result->addIncoming(true_result, phi_then_block);
				result->addIncoming(false_result, phi_else_block);
				return result;
			}
			return {};
		}
			
		case A_WHILE:
		case A_DO_WHILE:
		case A_FOR: {
			auto* loop = (AST_loop*)ast;
				
			auto id = format_id(loop_count++);
			auto cid = format_id(cont_count++);

			auto* loop_cond_block = llvm::BasicBlock::Create(ctx, llvm::Twine("loop.") + id.str + ".cond", cur_func, ib_block); // loop->cond
			auto* loop_body_block = llvm::BasicBlock::Create(ctx, llvm::Twine("loop.") + id.str + ".body", cur_func, ib_block); // loop->body
			auto* loop_end_block  = llvm::BasicBlock::Create(ctx, llvm::Twine("loop.") + id.str + ".end" , cur_func, ib_block); // loop->end   need this block seperate from loop to allow for continue;
			auto* cont_block      = llvm::BasicBlock::Create(ctx, llvm::Twine("cont.") + cid.str         , cur_func, ib_block); // code after loop
				
			// do-while is:               for & while is:
			//   prev ---> loop --|         prev --|  loop --|
			//               ^    v                |    ^    v
			//               |   end               |    |   end
			//               |    |                ---v |    |
			//   cont <--  cond <--         cont <--  cond <--

			// break;    will jump to cont
			// continue; will jump to end

			loop_blocks.push_back(LoopBlocks{ cont_block, loop_end_block });

			{ // prev block
				ib_block = ast->type == A_DO_WHILE ? loop_body_block : loop_cond_block;

				if (loop->start)
					codegen(loop->start);
						
				// do-while:     prev block branches to loop body
				// for & while:  prev block branches to loop cond
				build.CreateBr(ast->type == A_DO_WHILE ? loop_body_block : loop_cond_block);
			}

			{ // loop body
				ib_block = loop_end_block;

				build.SetInsertPoint(loop_body_block);
						
				codegen(loop->body);
						
				build.CreateBr(loop_end_block);
			}

			{ // loop end
				ib_block = loop_cond_block;

				build.SetInsertPoint(loop_end_block);
						
				if (loop->end)
					codegen(loop->end);
						
				build.CreateBr(loop_cond_block);
			}
				
			// codegen cond last, since it does not matter for for & while loops
			//  but for do-while loop the cond is allowed to access the loop body scope, and thus we need to codegen the body before the cond
			//  or will crash since the local vars are not defined (alloca'd) in the IR yet
			{ // loop cond
				ib_block = ast->type == A_DO_WHILE ? cont_block : loop_body_block;

				build.SetInsertPoint(loop_cond_block);
						
				llvm::Value* cond = codegen(loop->cond);
				build.CreateCondBr(cond, loop_body_block, cont_block);
			}
				
			// following code will be in cont block
			ib_block = nullptr;

			build.SetInsertPoint(cont_block);

			loop_blocks.pop_back();

			return {};
		}

		case A_BREAK: {
			if (loop_blocks.empty())
				throw CompilerExcept{"error: break not inside of any loop", ast->src_tok->source};
				
			build.CreateBr( loop_blocks.back().break_block );
			
			unreachable();
			return {};
		}
		case A_CONTINUE: {
			if (loop_blocks.empty())
				throw CompilerExcept{"error: continue not inside of any loop", ast->src_tok->source};
				
			build.CreateBr( loop_blocks.back().continue_block );
			
			unreachable();
			return {};
		}
			
		case A_CALL: {
			auto* call = (AST_call*)ast;
			auto* fdef = (AST_funcdef*)call->fdef;

			// collect function args
			struct Argdecl {
				AST_vardecl* decl;
				bool         set;
				size_t       callarg;
			};
			std::vector<Argdecl> declargs;
			declargs.reserve(32);

			for (auto* arg = (AST_vardecl*)fdef->args; arg != nullptr; arg = (AST_vardecl*)arg->next) {
				declargs.push_back({ arg, false, (size_t)-1 });
			}

			// generate IR for callargs first (move into temps)
			std::vector<llvm::Value*> callargs;
			callargs.reserve(32);

			bool vararg = false;
			size_t vararg_i = 0;

			size_t calli = 0;
			for (auto* arg = (AST_callarg*)call->args; arg != nullptr; arg = (AST_callarg*)arg->next) {
				callargs.push_back( codegen(arg->expr) );

				if (arg->decl->type == A_VARARGS) {
					vararg = true;
					vararg_i = calli;
				}

				declargs[arg->decli].set = true;
				declargs[arg->decli].callarg = calli++;
			}

			return build.CreateCall(fdef->llvm_func, callargs, fdef->rets ? "_call" : "");
		}
			
		case A_RETURN: {
			auto* ret = (AST_return*)ast;

			// TODO: implement multiple returns
			assert(ret->argc <= 1);

			llvm::Value* retval = nullptr;
			for (auto* arg = (AST_callarg*)ret->args; arg != nullptr; arg = (AST_callarg*)arg->next) {
				llvm::Value* var_ptr = local_var_ptr(ret, arg->decl);

				llvm::Value* result = codegen(arg->expr);

				build.CreateStore(result, var_ptr);
			}
				
			return_ret_vars();

			unreachable();
			return {};
		}

		case A_FUNCDEF:
		default:
			return {};
		}
	}
};

llvm::Module* llvm_gen_module (strview const& filename, std::vector<AST_funcdef*>& funcdefs) {
	ZoneScoped;

	LLVM_gen llvm_gen = {
		funcdefs
	};
	llvm_gen.generate(filename);

	return llvm_gen.modl; // pass ownership to caller
}
void llvm_free_module (llvm::Module* modl) {
	delete modl;
}
