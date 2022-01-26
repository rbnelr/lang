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
		
	#define PROFILE_DUPLICATE_FUNCS 0
	
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

	void set_var_name (AST_vardecl* vardecl, llvm::Value* val) {
		auto id = format_id(vardecl->llvm_next_id++);

		val->setName(llvm::Twine(SR(vardecl->ident)) +"."+ id.str);
	}

	llvm::SmallVector<AST_vardecl*, 32> declared_vars;

	void declare_local_var (AST_vardecl* vardecl) {
		// declared vars are initially uninitialized, need to use an undef value here
		vardecl->llvm_value = llvm::UndefValue::get(map_type(vardecl->valtype));
		vardecl->llvm_next_id = 0;

		declared_vars.push_back(vardecl);
	}
	llvm::Value* load_local_var (AST* op, AST_vardecl* vardecl) {
		if (vardecl->is_arg) {
			return vardecl->llvm_value;
		}
		else {
			assert(vardecl->llvm_value != nullptr);
			//if (vardecl->llvm_value->) // how to check for UndefValue?
			//	throw CompilerExcept{"error: variable uninitialized", op->src_tok->source};
			
			return vardecl->llvm_value;
		}
	}
	void store_local_var (AST_vardecl* vardecl, llvm::Value* val) {
		set_var_name(vardecl, val);

		vardecl->llvm_value = val;
	}
	
	struct BasicBlock {
		llvm::BasicBlock* block;

		llvm::SmallVector<llvm::PHINode*, 32> phis;

		// once a block has been begun, we know what phis are needed in it, thus we should no longer lazily_add_phis()
		bool phis_fixed = false;
	};

	BasicBlock* cur_block = nullptr;

	// stack of loop blocks to allow for break and continue anywhere (except outside of loop, where loop_blocks will be empty)
	struct LoopBlocks {
		BasicBlock* break_block;    // loop cont  ie. block after loop
		BasicBlock* continue_block; // loop end   ie. block after loop body, but befor cond
	};
	llvm::SmallVector<LoopBlocks, 8> loop_blocks;
	

	// lazily add phis to current declared vars
	void lazily_add_phis (BasicBlock* block, size_t count) {
		size_t old_sz = block->phis.size();
		
		block->phis.resize(count);
		
		for (size_t i=old_sz; i<count; ++i) {
			auto* vardecl = declared_vars[i];
			// Create a phi node for every (declared at that point) variable at the start of every basic block
			block->phis[i] = build.CreatePHI(map_type(vardecl->valtype), 0);
		}
	}
	void add_phi_nodes_incoming (BasicBlock* dst_block) {
		
		build.SetInsertPoint(dst_block->block);

		if (!dst_block->phis_fixed) {
			if (dst_block->phis.size() < declared_vars.size())
				lazily_add_phis(dst_block, declared_vars.size());
		}

		for (size_t i=0; i<dst_block->phis.size(); ++i) {
			auto* vardecl = declared_vars[i];

			// Add incoming value for phi in dst_block for our cur_block using the latest value noted in declared_vars
			// which is:
			//  1. the latest value written in this block
			//  2. the phi created for this block if it declared before this block and was never overwritten
			//  3. a nullptr if it was declared in this block but never written (uninitialized)

			// TODO: How do I handle uinitialzed variables being put into phi nodes?
			// putting null in there likely is not valid, not crating the phi incoming is likely wrong too
			// Should I use that undef thing for uninitialized?
			assert(declared_vars[i]->llvm_value != nullptr);

			dst_block->phis[i]->addIncoming(declared_vars[i]->llvm_value, cur_block->block);
		}
	}

	// create basic block, so we can add branches to it
	BasicBlock* create_basic_block (llvm::Twine const& name) {
		
		BasicBlock* block = g_allocator.alloc<BasicBlock>();
		
		block->block = llvm::BasicBlock::Create(ctx, name);
		
		return block;
	}

	void begin_basic_block (BasicBlock* block) {
		cur_block = block;

		block->block->insertInto(cur_func);

		build.SetInsertPoint(block->block);
		
		// only vars declared before this block (in program order) might need to be phi nodes
		lazily_add_phis(block, declared_vars.size());
		
		for (size_t i=0; i < declared_vars.size(); ++i) {
			declared_vars[i]->llvm_value = block->phis[i];
			
			set_var_name(declared_vars[i], block->phis[i]);
		}

		block->phis_fixed = true;
	}

	void end_block_uncond (BasicBlock* dst_block) {
		build.CreateBr(dst_block->block);

		add_phi_nodes_incoming(dst_block);
	}
	void end_block_cond (llvm::Value* cond, BasicBlock* true_block, BasicBlock* false_block) {
		build.CreateCondBr(cond, true_block->block, false_block->block);

		add_phi_nodes_incoming(true_block);
		add_phi_nodes_incoming(false_block);
	}

	void unreachable () {
		// Could also manually keep track of when code is unreachable and skip generating instruction
		// but in debug mode we actually want to keep all code to allow things like "set next statement"

		// Create block that is unreachable
		auto* block = create_basic_block("unreach");
		begin_basic_block(block);

		//add_phi_nodes_incoming(block);
	}
	
////
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

////
	void veryfy_function (AST_funcdef* fdef) {
		llvm::SmallString<128> msg;
		llvm::raw_svector_ostream OS(msg);

		if (verifyFunction(*fdef->llvm_func, &OS)) {
			fprintf(stderr, ">>> LLVM error: %.*s\n", (int)msg.str().size(), msg.str().data());
			fflush(stderr);
			//throw CompilerExcept{ "LLVM: error!", fdef->src_tok->source };
		}
	}
	void codegen_function (AST_funcdef* fdef) {
		ZoneScoped;

		cur_fdef = fdef;
		cur_func = fdef->llvm_func;

		//// entry block + start recursively codegening of the function body
		BasicBlock* entry_block = create_basic_block("entry");
		begin_basic_block(entry_block); // should not create any phis

		for (auto* ret = (AST_vardecl*)fdef->rets; ret != nullptr; ret = (AST_vardecl*)ret->next) {
			assert(ret->type != A_VARARGS);
			
			declare_local_var(ret);
		}

		codegen(fdef->body);
		
		//// implicit return instruction at end of function body
		return_ret_vars();
		
		//// finish function
		veryfy_function(fdef);

		cur_func = nullptr;
		cur_block = nullptr;
	}

	void return_ret_vars () {
		// TODO: implement multiple returns
		assert(cur_fdef->retc <= 1);

		if (cur_fdef->retc == 1) {
			auto* vardecl = (AST_vardecl*)cur_fdef->rets;
			
			build.CreateRet(vardecl->llvm_value);
		}
		else {
			build.CreateRetVoid();
		}
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
				store_local_var(vardecl, val);
			}
			return {};
		}

		case A_VAR: {
			auto* var = (AST_var*)ast;
			auto* vardecl = (AST_vardecl*)var->decl;
			
			return load_local_var(ast, vardecl);
		}

		case A_ASSIGNOP: {
			auto* op = (AST_binop*)ast;

			if (op->lhs->type != A_VAR)
				throw CompilerExcept{"error: expected variable for assignop lhs", ast->src_tok->source};

			auto* var = (AST_var*)op->lhs;

			llvm::Value* result;

			if (op->op == OP_ASSIGN) {
				// simple assignment of rhs to lhs
				result = codegen(op->rhs);
			}
			else {
				// load lhs first
				llvm::Value* lhs = load_local_var(op, var->decl);
				// eval rhs
				llvm::Value* rhs = codegen(op->rhs);
				// eval binary operator and assign to lhs
				result = codegen_binop(op, lhs, rhs);
			}
			
			store_local_var(var->decl, result);
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

					// assign inc/decremented to var
					store_local_var(var->decl, result);
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

			auto id  = format_id(if_count++);
			auto cid = format_id(cont_count++);

			auto* then_block =                  create_basic_block(llvm::Twine("if.") + id.str + ".then");
			auto* else_block = aif->else_body ? create_basic_block(llvm::Twine("if.") + id.str + ".else") : nullptr;
			auto* cont_block =                  create_basic_block(llvm::Twine("cont.") + cid.str       );
				
			llvm::Value *true_result,    *false_result;
			BasicBlock  *phi_then_block, *phi_else_block;

			{ // condition at end of current block
				llvm::Value* cond = codegen(aif->cond);
				
				end_block_cond(cond, then_block, aif->else_body ? else_block : cont_block);
			}

			{ // generate then block
				begin_basic_block(then_block);

				true_result = codegen(aif->if_body);
				phi_then_block = cur_block; // nested ifs or loops (in codegen) change the current block
				
				end_block_uncond(cont_block);
			}

			if (else_block) { // generate else block
				begin_basic_block(else_block);
					
				false_result = codegen(aif->else_body);
				phi_else_block = cur_block;
					
				end_block_uncond(cont_block);
			}
				
			// following code will be in cont block
			begin_basic_block(cont_block);
				
			if (aif->type == A_SELECT) {
				assert(else_block);
				assert(true_result && false_result);
				assert(true_result->getType() == false_result->getType());
				
				llvm::PHINode* result = build.CreatePHI(true_result->getType(), 2, "_sel");
				result->addIncoming(true_result,  phi_then_block->block);
				result->addIncoming(false_result, phi_else_block->block);
				return result;
			}
			return {};
		}
			
		case A_WHILE:
		case A_DO_WHILE:
		case A_FOR: {
			auto* loop = (AST_loop*)ast;
				
			auto id  = format_id(loop_count++);
			auto cid = format_id(cont_count++);

			auto* loop_cond_block = create_basic_block(llvm::Twine("loop.") + id.str + ".cond"); // loop->cond
			auto* loop_body_block = create_basic_block(llvm::Twine("loop.") + id.str + ".body"); // loop->body
			auto* loop_end_block  = create_basic_block(llvm::Twine("loop.") + id.str + ".end" ); // loop->end   need this block seperate from loop to allow for continue;
			auto* cont_block      = create_basic_block(llvm::Twine("cont.") + cid.str         ); // code after loop

			// do-while is:               for & while are:
			//                                                     
			//   cur_block --> loop --|     cur_block --|  loop --|
			//                   ^    v                 |    ^    v
			//                   |   end                |    |   end
			//                   |    |                 ---v |    |
			//        cont <-- cond <--           cont <-- cond <--

			// break;    will jump to cont
			// continue; will jump to end

			loop_blocks.push_back(LoopBlocks{ cont_block, loop_end_block });

			{ // prev block
				if (loop->start)
					codegen(loop->start);
						
				// do-while:     prev block branches to loop body
				// for & while:  prev block branches to loop cond
				end_block_uncond(ast->type == A_DO_WHILE ? loop_body_block : loop_cond_block);
			}

			if (ast->type != A_DO_WHILE) { // loop cond
				begin_basic_block(loop_cond_block);
						
				llvm::Value* cond = codegen(loop->cond);

				end_block_cond(cond, loop_body_block, cont_block);
			}

			{ // loop body
				begin_basic_block(loop_body_block);
						
				codegen(loop->body);
				
				end_block_uncond(loop_end_block);
			}

			{ // loop end
				begin_basic_block(loop_end_block);
						
				if (loop->end)
					codegen(loop->end);
				
				end_block_uncond(loop_cond_block);
			}
				
			if (ast->type == A_DO_WHILE){ // loop cond
				begin_basic_block(loop_cond_block);
						
				llvm::Value* cond = codegen(loop->cond);

				end_block_cond(cond, loop_body_block, cont_block);
			}
				
			// following code will be in cont block
			begin_basic_block(cont_block);

			loop_blocks.pop_back();

			return {};
		}

		case A_BREAK: {
			if (loop_blocks.empty())
				throw CompilerExcept{"error: break not inside of any loop", ast->src_tok->source};
				
			end_block_uncond(loop_blocks.back().break_block);
			unreachable();
			return {};
		}
		case A_CONTINUE: {
			if (loop_blocks.empty())
				throw CompilerExcept{"error: continue not inside of any loop", ast->src_tok->source};
			
			end_block_uncond(loop_blocks.back().continue_block);
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
				llvm::Value* result = codegen(arg->expr);

				store_local_var(arg->decl, result);
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
