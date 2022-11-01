#include "llvm_pch.hpp"
#include "llvm_backend.hpp"

#include "common.hpp"
#include "frontend/builtins.hpp"

struct _IDFormStrbuf {
	char str[32];
};
inline _IDFormStrbuf format_id (size_t id) {
	_IDFormStrbuf buf; // fits any 64-bit int
	auto len = llvm::format(".%" PRIu64, id).print(buf.str, ARRLEN(buf.str));
	if (len > ARRLEN(buf.str)) {
		// fail
		assert(false);
		buf.str[0] = '\0';
	}
	return buf;
}

#define PROFILE_DUPLICATE_FUNCS 0

llvmModule::~llvmModule () {
	if (modl) {
		delete modl;
		delete ctx;
	}
}

struct LLVM_gen {
	arrview<AST_funcdef*>   funcdefs;
	arrview<AST_structdef*> structdefs;

	llvm::LLVMContext* ctx  = new llvm::LLVMContext();
	llvm::Module*      modl = new llvm::Module("<main>", *ctx);

	typedef llvm::IRBuilder<llvm::NoFolder> Builder;
	Builder build = Builder( *ctx );
	
	//std::unique_ptr<llvm::DIBuilder> di_build;

	//llvm::DICompileUnit* di_comp_unit;
	//llvm::DIFile*        di_file;

	void dbg_info (llvm::Instruction* I, AST* ast) {
		//auto start_lineno = lines.find_lineno(ast->src_tok->source.start);
		//auto line_str = lines.get_line_text(start_lineno);
		//
		//size_t charno = ast->src_tok->source.start - line_str.data();
		//
		//I->setDebugLoc( llvm::DILocation::get(ctx, (unsigned)(start_lineno+1), (unsigned)(charno), di_comp_unit));
	}

#if PROFILE_DUPLICATE_FUNCS
	bool _is_builtin = true;
	int _dupl_func_i = 0;
#endif
	
	void generate (strview filename) {
		ZoneScoped;

		/*
		di_build = std::make_unique<llvm::DIBuilder>(*modl);

		bool is_optimized = false;
		di_comp_unit = di_build->createCompileUnit(llvm::dwarf::DW_LANG_C,
			di_build->createFile(SR(filename), "."), "LA Compiler", is_optimized, "", 0);

		di_file = di_build->createFile(di_comp_unit->getFilename(), di_comp_unit->getDirectory());
		*/

		// declare builtin functions
		declare_builtins();

	#if PROFILE_DUPLICATE_FUNCS
		_is_builtin = false;
		for (_dupl_func_i=0; _dupl_func_i<1000; ++_dupl_func_i) {
	#endif
			
		// Clear structdef->llvm_struct just in case we codegen the same AST twice
		for (auto* structdef : structdefs) {
			structdef->llvm_ty = nullptr;
		}
		for (auto* structdef : structdefs) {
			declare_struct(structdef);
		}

		// declare functions declared in source
		for (auto* funcdef : funcdefs) {
			
			auto linkage = llvm::Function::InternalLinkage;
			
			if (funcdef->ident == "main")
				linkage = llvm::Function::ExternalLinkage; // need this to be able to actually call it later when JITed

			declare_function(funcdef, linkage);
		}

		// now that all callable functions are declared
		// generate IR for functions defined in source
		for (auto* funcdef : funcdefs) {
			codegen_function(funcdef);
		}

	#if PROFILE_DUPLICATE_FUNCS
		}
	#endif

		//di_build->finalize();

		if (options.print_ir) {
			print_seperator("LLVM IR");
			modl->print(llvm::errs(), nullptr);
		}
	}

	AST_funcdef*      cur_fdef;
	llvm::BasicBlock* alloca_block;

	// stack of loop blocks to allow for break and continue anywhere (except outside of loop, where loop_blocks will be empty)
	struct LoopBlocks {
		llvm::BasicBlock* break_block;    // loop cont  ie. block after loop
		llvm::BasicBlock* continue_block; // loop end   ie. block after loop body, but befor cond
	};
	llvm::SmallVector<LoopBlocks, 8> loop_blocks;
	
	// Improve names of llvm values and basic blocks by manually keeping count
	// TODO: This could actually impact compiler perf, so consider not even generating names at all in non-debug info builds
	// The way that allows for the least overhead while still allowing runtime switching of debug-info on/off
	// is to make the while codegen pass templated with a template<bool DebugInfo> and instantiate it twice
	size_t strlit_count = 0;

	// per function
	size_t sc_count;
	size_t if_count;
	size_t loop_count;
	size_t cont_count;

	llvm::Value* declare_local_var (AST_vardecl* vardecl) {
		auto tmp_build = llvm::IRBuilder<llvm::NoFolder>(alloca_block);

		vardecl->llvm_type = map_type(vardecl->type);
			
		auto* I = tmp_build.CreateAlloca(vardecl->llvm_type, nullptr, SR(vardecl->ident));
		vardecl->llvm_value = I;

		return I;
	}

	struct Value {
		llvm::Value*     llvm_val = nullptr;
		bool             rval;
		llvm::Type*      type;

		static Value RValue (llvm::Value* val) {
			assert(val);
			return { val, true };
		}
		static Value LValue (llvm::Value* ptr, llvm::Type* type) {
			assert(ptr && type);
			return { ptr, false, type };
		}
	};

	llvm::Value* load_value (Value val) {
		if (val.llvm_val == nullptr)
			return nullptr;
		// val is value
		else if (val.rval)
			return val.llvm_val;
		// val is ptr
		else
			return build.CreateLoad(val.type, val.llvm_val, val.llvm_val->getName());
	}
	llvm::Value* codegen_rval (AST* ast) {
		Value val = codegen(ast);
		llvm::Value* llvm_val = load_value(val);
		assert(llvm_val);
		return llvm_val;
	}

	void store_value (Value val, llvm::Value* rhs) {
		assert(val.llvm_val != nullptr); // cannot assign to void
		assert(!val.rval); // cannot assign to RValues
		
		build.CreateStore(rhs, val.llvm_val);
	}

	Value get_ret_struct_ptr (AST_funcdef* fdef, AST_vardecl* ret) {
		llvm::Value* indices[] = {
			llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), 0),
			llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), ret->llvm_GEP_idx),
		};
		auto* ptr = build.CreateInBoundsGEP(fdef->ret_struct->llvm_ty, fdef->llvm_ret_struct, indices, SR(ret->ident));

		return Value::LValue(ptr, ret->llvm_type);
	}

	Value var_value (AST_vardecl* decl) {
		// args that are passed by value are rvalues in llvm
		switch (decl->vartype) {
			case AST_vardecl::LOCAL: {
				return Value::LValue(decl->llvm_value, decl->llvm_type);
			}

			case AST_vardecl::ARG: {
				if (decl->type.ty->tclass == TY_STRUCT)
					return Value::LValue(decl->llvm_value, decl->llvm_type);
				else
					return Value::RValue(decl->llvm_value);
			}

			case AST_vardecl::RET: {
				if (cur_fdef->llvm_ret_struct)
					return get_ret_struct_ptr(cur_fdef, decl);
				else
					return Value::LValue(decl->llvm_value, decl->llvm_type);
			}

			INVALID_DEFAULT;
		}
	}

	void begin_basic_block (llvm::BasicBlock* block) {
		block->insertInto(cur_fdef->llvm_func);
		build.SetInsertPoint(block);
	}

	void unreachable () {
		// Could also manually keep track of when code is unreachable and skip generating instruction
		// but in debug mode we actually want to keep all code to allow things like "set next statement"

		// Create block that is unreachable
		auto* block = llvm::BasicBlock::Create(*ctx, "unreach");
		begin_basic_block(block);
	}
	
////
	llvm::Type* map_type (Typeref& type) {
		switch (type.ty->tclass) {
			//case TY_VOID: return llvm::Type::getVoidTy(ctx);
			case TY_BOOL: return llvm::Type::getInt1Ty(*ctx);
			case TY_INT:  return llvm::Type::getInt64Ty(*ctx);
			case TY_FLT:  return llvm::Type::getDoubleTy(*ctx);
			case TY_STR:  return llvm::Type::getInt8PtrTy(*ctx);

			case TY_STRUCT: {
				assert(type.ty->decl && type.ty->decl->kind == A_STRUCTDECL);
				auto* struc = (AST_structdef*)type.ty->decl;

				// handle out-of order struct dependencies
				if (struc->llvm_ty == nullptr) {
					declare_struct(struc);
				}
				assert(struc->llvm_ty);
				return struc->llvm_ty;
			}

			INVALID_DEFAULT;
		}
	}
	
	void declare_struct (AST_structdef* struc) {
		if (struc->llvm_ty)
			return; // already declared as a dependency of an earlier struct

		llvm::SmallVector<llvm::Type*, 32> elements;

		unsigned idx = 0;
		for (auto* member : struc->members) {
			member->llvm_type = map_type(member->type);

			elements.push_back(member->llvm_type);

			// struct members don't have values directly (neither SSA values nor alloca'd values on the stack)
			// instead ptrs are GEP'd via llvm_GEP_idx
			member->llvm_value = nullptr;
			member->llvm_GEP_idx = idx++;
		}

		struc->llvm_ty = llvm::StructType::create(*ctx, elements, SR(struc->ident));
	}

	llvm::Value* codegen_unary (OpType op, llvm::Value* operand, AST* operand_ast) {
		switch (operand_ast->type.ty->tclass) {
		case TY_INT: {
			switch (op) {
				case OP_POSITIVE:    return operand; // no-op
				case OP_NEGATE:      return build.CreateNeg(operand, "_neg");
				case OP_BIT_NOT:     return build.CreateNot(operand, "_not");
				//case OP_LOGICAL_NOT:
				case OP_INC:         return build.CreateAdd(operand, llvm::ConstantInt::get(operand->getType(), 1), "_inc");
				case OP_DEC:         return build.CreateSub(operand, llvm::ConstantInt::get(operand->getType(), 1), "_dec");
				INVALID_DEFAULT;
			}
		}
		case TY_BOOL: {
			switch (op) {
				case OP_BIT_NOT:
				case OP_LOGICAL_NOT: return build.CreateNot(operand, "_not");
				INVALID_DEFAULT;
			}
		}
		case TY_FLT: {
			switch (op) {
				case OP_POSITIVE:    return operand; // no-op
				case OP_NEGATE:      return build.CreateFNeg(operand, "_neg");
				INVALID_DEFAULT;
			}
		}
		INVALID_DEFAULT;
		}
	}
	llvm::Value* codegen_binop (OpType op, llvm::Value* lhs, llvm::Value* rhs, AST* lhs_ast, AST* rhs_ast) {
		assert(lhs_ast->type.ty == rhs_ast->type.ty);
		
		switch (lhs_ast->type.ty->tclass) {
		case TY_INT: {
			switch (op) {
				case OP_ADD:        return build.CreateAdd (lhs, rhs, "_add");
				case OP_SUB:        return build.CreateSub (lhs, rhs, "_sub");
				case OP_MUL:        return build.CreateMul (lhs, rhs, "_mul");
				case OP_DIV:        return build.CreateSDiv(lhs, rhs, "_div");
				case OP_MOD:        return build.CreateURem(lhs, rhs, "_mod"); // TODO: is URem the correct thing for  sint % sint  ?
				
				case OP_BIT_AND:    return build.CreateAnd (lhs, rhs, "_and");
				case OP_BIT_OR:     return build.CreateOr  (lhs, rhs, "_or");
				case OP_BIT_XOR:    return build.CreateXor (lhs, rhs, "_xor");

				case OP_LESS:       return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT, lhs, rhs, "_cmp");
				case OP_LESSEQ:     return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, lhs, rhs, "_cmp");
				case OP_GREATER:    return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT, lhs, rhs, "_cmp");
				case OP_GREATEREQ:  return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, lhs, rhs, "_cmp");
				case OP_EQUALS:     return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ , lhs, rhs, "_cmp");
				case OP_NOT_EQUALS: return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_NE , lhs, rhs, "_cmp");

				INVALID_DEFAULT;
			}
		} break;
		case TY_BOOL: {
			switch (op) {
				case OP_BIT_AND:    return build.CreateAnd (lhs, rhs, "_and");
				case OP_BIT_OR:     return build.CreateOr  (lhs, rhs, "_or");
				case OP_BIT_XOR:    return build.CreateXor (lhs, rhs, "_xor");
				
				// implemented below
				//case OP_LOGICAL_AND:
				//case OP_LOGICAL_OR:
				
				case OP_EQUALS:     return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, lhs, rhs, "_cmp");
				case OP_NOT_EQUALS: return build.CreateCmp(llvm::CmpInst::Predicate::ICMP_NE, lhs, rhs, "_cmp");

				INVALID_DEFAULT;
			}
		} break;
		case TY_FLT: {
			switch (op) {
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
		INVALID_DEFAULT;
		}
	}

////
	void verify_function (AST_funcdef* fdef) {
		llvm::SmallString<128> msg;
		llvm::raw_svector_ostream OS(msg);

		if (verifyFunction(*fdef->llvm_func, &OS)) {
			auto str = msg.str();
			ERROR(fdef->src, "LLVM error: %.*s\n", (int)str.size(), str.data());
		}
	}
	
	void declare_builtins () {
		// TODO: do this lazily once they are actually called?

		for (auto* builtin : BUILTIN_FUNCS) {
			declare_function(builtin, llvm::Function::InternalLinkage);
		}
	}
	
	void declare_function (AST_funcdef* fdef, llvm::GlobalValue::LinkageTypes linkage) {
		// returns
		llvm::Type* ret_ty;
		if (fdef->rets.count == 0)
			ret_ty = llvm::Type::getVoidTy(*ctx);
		else if (fdef->rets.count == 1)
			ret_ty = map_type(fdef->rets[0]->decl->type);
		else
			ret_ty = fdef->ret_struct->llvm_ty;

		// arguments
		llvm::SmallVector<llvm::Type*, 16> args_ty;
		bool vararg = false;
		
		for (auto* arg : fdef->args) {
			if (arg->decl->kind == A_VARARGS) {
				vararg = true;
				break;
			}
			else {
				auto* ty = map_type(arg->decl->type);

				// Always pass structs by reference for now
				if (arg->decl->type.ty->tclass == TY_STRUCT) {
					arg->decl->llvm_type = ty;
					ty = ty->getPointerTo();
				}
				
				args_ty.push_back(ty);
			}

			arg->decl->vartype = AST_vardecl::ARG;
		}

		auto* func_ty = llvm::FunctionType::get(ret_ty, args_ty, vararg);
		
		// TODO: unique func names for shadowed scoped funcs (funcs would have same name) ie. { func f () {} } func f () {}
		// But llvm actually handles this for us, though defining a main function might break  dyld.getSymbol("main")
	#if PROFILE_DUPLICATE_FUNCS
		llvm::Function* func;
		if (_is_builtin)
			func = llvm::Function::Create(func_ty, linkage, SR(fdef->ident), *modl);
		else
			func = llvm::Function::Create(func_ty, linkage, llvm::Twine(std::format("_{}_", _dupl_func_i)) + SR(fdef->ident), *modl);
	#else
		auto* func = llvm::Function::Create(func_ty, linkage, SR(fdef->ident), *modl);
	#endif
		func->addFnAttr(llvm::Attribute::NoUnwind); // don't need exception data when compiling
		assert(!func->hasFnAttribute(llvm::Attribute::UWTable));
		
		func->setCallingConv(llvm::CallingConv::Win64);

		// set argument names
		unsigned i = 0;
		for (auto* arg : fdef->args) {
			if (arg->decl->kind == A_VARARGS)
				break;
			arg->decl->llvm_value = func->getArg(i++);
			arg->decl->llvm_value->setName(SR(arg->decl->ident));
		}

		fdef->llvm_func = func;

		/*
		auto CreateFunctionType = [&] (size_t NumArgs) {
			llvm::SmallVector<llvm::Metadata*, 16> EltTys;
			llvm::DIType* DblTy = di_build->createBasicType("double", 64, llvm::dwarf::DW_ATE_float);
		
			// Add the result type.
			EltTys.push_back(DblTy);
		
			for (size_t i = 0, e = NumArgs; i != e; ++i)
				EltTys.push_back(DblTy);
		
			return di_build->createSubroutineType(di_build->getOrCreateTypeArray(EltTys));
		};
		
		unsigned LineNo = 0;
		unsigned ScopeLine = 0;
		auto* SP = di_build->createFunction(di_comp_unit, SR(fdef->ident), SR(fdef->ident), di_file, LineNo, 
			CreateFunctionType(args_ty.size()), ScopeLine);
		func->setSubprogram(SP);
		*/
	}
	
	void codegen_function (AST_funcdef* fdef) {
		ZoneScoped;

		cur_fdef = fdef;

		sc_count   = 0;
		if_count   = 0;
		loop_count = 0;
		cont_count = 0;

		//// 
		// dedicated block for local variable alloca (so they are all grouped together)
		alloca_block = llvm::BasicBlock::Create(*ctx, "entry");

		auto* entry_block = llvm::BasicBlock::Create(*ctx, "entry.cont");
		alloca_block->insertInto(cur_fdef->llvm_func);
		
		begin_basic_block(entry_block);
		
		if (fdef->rets.count > 0) {
			auto tmp_build = llvm::IRBuilder<llvm::NoFolder>(alloca_block);

			if (fdef->rets.count == 1) {
				auto ret = fdef->rets[0];
				
				ret->decl->llvm_type = map_type(ret->decl->type);
				ret->decl->llvm_value = tmp_build.CreateAlloca(ret->decl->llvm_type, nullptr, SR("ret"));

				ret->decl->vartype = AST_vardecl::RET;
			}
			else {
				fdef->llvm_ret_struct = tmp_build.CreateAlloca(fdef->ret_struct->llvm_ty, nullptr, SR("ret"));
			
				for (auto* ret : fdef->rets) {
					assert(ret->decl->kind != A_VARARGS);
					
					if (ret->init) {
						Value ptr = get_ret_struct_ptr(fdef, ret->decl);
						llvm::Value* val = codegen_rval(ret->init);

						store_value(ptr, val);
					}
				
					ret->decl->vartype = AST_vardecl::RET;
				}
			}
		}

		codegen(fdef->body);
		
		//// implicit return instruction at end of function body
		return_ret_vars(fdef);
		
		// finally add branch from alloca to entry
		build.SetInsertPoint(alloca_block);
		build.CreateBr(entry_block);

		//// finish function
		verify_function(fdef);

		alloca_block = nullptr;
		cur_fdef = nullptr;
	}

	void return_ret_vars (AST* ast) {
		if (cur_fdef->rets.count == 0) {
			build.CreateRetVoid();
		}
		else {
			llvm::Value* val;
			
			if (cur_fdef->llvm_ret_struct) {
				val = build.CreateLoad(cur_fdef->ret_struct->llvm_ty, cur_fdef->llvm_ret_struct, "retval");
			}
			else {
				auto* ret = cur_fdef->rets[0];
				val = build.CreateLoad(ret->decl->llvm_type, ret->decl->llvm_value, "retval");
			}

			build.CreateRet(val);
		}
	}

	Value codegen (AST* ast) {
		switch (ast->kind) {
		case A_LITERAL: {
			auto* lit = (AST_literal*)ast;
				
			llvm::Value* val;
			switch (lit->type.ty->tclass) {
				case TY_BOOL:
					val = llvm::ConstantInt::getBool(*ctx, lit->value.b);
					break;
				case TY_INT:
					val = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx),
						llvm::APInt(64, (uint64_t)lit->value.i, true));
					break;
				case TY_FLT:
					val = llvm::ConstantFP::get(llvm::Type::getDoubleTy(*ctx),
						llvm::APFloat(lit->value.f));
					break;
				case TY_STR:
					val = build.CreateGlobalStringPtr(lit->value.str, llvm::Twine("strlit") + format_id(strlit_count++).str);
					break;
				INVALID_DEFAULT;
			}
			assert(val->getType() == map_type(lit->type));

			return Value::RValue(val);
		}

		case A_VARDECL: {
			auto* vardecl = (AST_vardecl*)ast;

			auto* val = declare_local_var(vardecl);
			return Value::LValue(val, vardecl->llvm_type);
		}

		case A_VAR: {
			auto* var = (AST_var*)ast;
			auto* vardecl = (AST_vardecl*)var->decl;
			assert(vardecl);
			
			return var_value(vardecl);
		}

		// Is explicitly handles during assignops
		case A_TUPLE: {
			assert(false);
			_UNREACHABLE;
		}

		case A_ASSIGNOP: {
			auto* op = (AST_binop*)ast;

			if (op->lhs->kind == A_TUPLE || op->rhs->kind == A_TUPLE) {

				// a, b += c, d  not allowed
				assert(op->op == OP_ASSIGN);
				
				smallvec<Value,        32> l;
				smallvec<llvm::Value*, 32> r;
				
				// eval lhs first
				if (op->lhs->kind == A_TUPLE) {
					auto exprs = ((AST_list*)op->lhs)->elements;
					for (size_t i=0; i<exprs.count; ++i)
						l.push( codegen(exprs[i]) );
				}
				else {
					assert(op->lhs->type.ty->tclass == TY_STRUCT);
					
					auto members = ((AST_structdef*)op->lhs->type.ty->decl)->members;
					
					Value struct_val = codegen(op->lhs);
					assert(!struct_val.rval);

					for (size_t i=0; i<members.count; ++i) {
						assert(members[i]->llvm_GEP_idx == i);

						llvm::Value* indices[] = {
							llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), 0),
							llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), (unsigned)i),
						};
						llvm::Value* ptr = build.CreateInBoundsGEP(struct_val.type, struct_val.llvm_val, indices, SR(members[i]->ident));

						l.push( Value::LValue(ptr, members[i]->llvm_type) );
					}
				}

				// eval rhs
				if (op->rhs->kind == A_TUPLE) {
					auto exprs = ((AST_list*)op->rhs)->elements;
					for (size_t i=0; i<exprs.count; ++i)
						r.push( codegen_rval(exprs[i]) );
				}
				else {
					assert(op->rhs->type.ty->tclass == TY_STRUCT);
					
					llvm::Value* struct_val = codegen_rval(op->rhs);

					auto members = ((AST_structdef*)op->rhs->type.ty->decl)->members;
					for (size_t i=0; i<members.count; ++i) {
						assert(members[i]->llvm_GEP_idx == i);
						unsigned indices[] = { (unsigned)i };
						r.push( build.CreateExtractValue(struct_val, indices, SR(members[i]->ident)) );
					}
				}

				assert(l.count == r.count);

				for (size_t i=0; i<l.count; ++i) {
					store_value(l[i], r[i]);
				}
			}
			else {
				// eval lhs first (it's a lvalue, so a ptr)
				Value lhs_val = codegen(op->lhs);
				// eval rhs
				llvm::Value* rhs_val = codegen_rval(op->rhs);

				if (op->op != OP_ASSIGN) {
					// eval binary operator and assign to lhs
					rhs_val = codegen_binop(op->op, load_value(lhs_val), rhs_val, op->lhs, op->rhs);
				}
				
				store_value(lhs_val, rhs_val);
			}
			return {};
		}

		case A_UNOP: {
			auto* op = (AST_unop*)ast;
			assert(op->type.ty == op->operand->type.ty);
			
			Value operand = codegen(op->operand);

			llvm::Value* old_val = load_value(operand);
			llvm::Value* result  = codegen_unary(op->op, old_val, op->operand);
				
			if (op->op == OP_INC || op->op == OP_DEC) {
				// assign inc/decremented to var
				store_value(operand, result);
				// return old value
				result = old_val;
			}

			return Value::RValue(result);
		}

		case A_BINOP: {
			auto* op = (AST_binop*)ast;
			
			if (op->op == OP_LOGICAL_AND || op->op == OP_LOGICAL_OR) {
				auto id  = format_id(sc_count++);
				auto cid = format_id(cont_count++);

				const char* name = op->op == OP_LOGICAL_AND ? "and" : "or";

				auto* else_block = llvm::BasicBlock::Create(*ctx, llvm::Twine(name) + id.str);
				auto* cont_block = llvm::BasicBlock::Create(*ctx, llvm::Twine("cont") + cid.str);
				
				// eval lhs
				llvm::Value* cond = codegen_rval(op->lhs);

				auto* phi_short_circuit_block = build.GetInsertBlock(); // nested ifs or loops (in codegen) change the current block
				
				llvm::Value* short_circuit_result;
				if (op->op == OP_LOGICAL_AND) {
					// lhs == false  -->  short circuit to false
					short_circuit_result = llvm::ConstantInt::getBool(llvm::Type::getInt1Ty(*ctx), false);
					build.CreateCondBr(cond, else_block, cont_block);
				}
				else {
					// lhs == true  -->  short circuit to true
					short_circuit_result = llvm::ConstantInt::getBool(llvm::Type::getInt1Ty(*ctx), true);
					build.CreateCondBr(cond, cont_block, else_block);
				}


				// eval rhs if did not short-circuit
				begin_basic_block(else_block);
					
				llvm::Value* rhs_result = codegen_rval(op->rhs);
				auto* phi_rhs_block = build.GetInsertBlock();
					
				build.CreateBr(cont_block);
				

				// following code will be in cont block
				begin_basic_block(cont_block);
				
				llvm::PHINode* result = build.CreatePHI(llvm::Type::getInt1Ty(*ctx), 2,  llvm::Twine("_") + name);
				result->addIncoming(short_circuit_result, phi_short_circuit_block);
				result->addIncoming(rhs_result, phi_rhs_block);
				return Value::RValue(result);
			}

			if (op->op == OP_MEMBER) {
				assert(op->rhs->kind == A_VAR);

				//auto* struc = (AST_structdef*)op->lhs->type.ty->decl;
				auto* memb = (AST_var*)op->rhs;
				assert(memb->decl != nullptr);

				Value lhs = codegen(op->lhs);
				if (lhs.rval) {
					assert(false);
					//unsigned indices[] = {
					//	memb->decl->llvm_GEP_idx,
					//};
					//llvm::Value* val = build.CreateExtractValue(lhs.llvm_val, indices, SR(memb->decl->ident));
					//
					//return Value::RValue(val);
				}
				else {
					assert(lhs.type && lhs.type->isStructTy());
					
					llvm::Value* indices[] = {
						llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), 0),
						llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), memb->decl->llvm_GEP_idx),
					};
					llvm::Value* ptr = build.CreateInBoundsGEP(lhs.type, lhs.llvm_val, indices, SR(memb->decl->ident));

					return Value::LValue(ptr, memb->decl->llvm_type);
				}
			}

			// eval lhs
			llvm::Value* lhs_val = codegen_rval(op->lhs);
			// eval rhs
			llvm::Value* rhs_val = codegen_rval(op->rhs);
			// eval binary operator
			return Value::RValue(codegen_binop(op->op, lhs_val, rhs_val, op->lhs, op->rhs));
		}

		case A_BLOCK: {
			auto* block = (AST_block*)ast;

			for (auto* n : block->statements) {
				codegen(n);
			}
			return {};
		}
			
		case A_IF:
		case A_SELECT: {
			auto* aif = (AST_if*)ast;

			auto id  = format_id(if_count++);
			auto cid = format_id(cont_count++);

			auto* then_block =                  llvm::BasicBlock::Create(*ctx, llvm::Twine("if") + id.str + ".then");
			auto* else_block = aif->else_body ? llvm::BasicBlock::Create(*ctx, llvm::Twine("if") + id.str + ".else") : nullptr;
			auto* cont_block =                  llvm::BasicBlock::Create(*ctx, llvm::Twine("cont") + cid.str       );
				
			Value              true_result,     false_result;
			llvm::BasicBlock  *phi_then_block, *phi_else_block;

			{ // condition at end of current block
				llvm::Value* cond = load_value(codegen(aif->cond));
				
				build.CreateCondBr(cond, then_block, aif->else_body ? else_block : cont_block);
			}

			{ // generate then block
				begin_basic_block(then_block);

				true_result = codegen(aif->if_body);
				phi_then_block = build.GetInsertBlock(); // nested ifs or loops (in codegen) change the current block
				
				build.CreateBr(cont_block);
			}

			if (else_block) { // generate else block
				begin_basic_block(else_block);
					
				false_result = codegen(aif->else_body);
				phi_else_block = build.GetInsertBlock();
					
				build.CreateBr(cont_block);
			}
				
			// following code will be in cont block
			begin_basic_block(cont_block);
			
			// normal if-else
			if (aif->kind != A_SELECT) {
				return {};
			}
			// ternary operator a?b:c
			else {
				assert(else_block);

				llvm::Value *T, *F;

				assert(aif->type.ty);
				if (aif->type.rval) {
					// at least one value are RValues, load them and return RValue
					T = load_value(true_result);
					F = load_value(false_result);

					assert(T && F);
					assert(T->getType() == F->getType());

					llvm::PHINode* result = build.CreatePHI(T->getType(), 2, "_sel");
					result->addIncoming(T, phi_then_block);
					result->addIncoming(F, phi_else_block);
					return Value::RValue(result);
				}
				else {
					// both values are LValues, select between the ptrs
					// to allow for  (cond ? a : b) = 5;

					assert(!true_result.rval && !false_result.rval);
					assert( true_result.type ==  false_result.type);
					
					T = true_result .llvm_val; // select between ptr
					F = false_result.llvm_val;

					llvm::PHINode* result = build.CreatePHI(T->getType(), 2, "_sel");
					result->addIncoming(T, phi_then_block);
					result->addIncoming(F, phi_else_block);
					return Value::LValue(result, true_result.type);
				}
			}
		}
			
		case A_WHILE:
		case A_DO_WHILE:
		case A_FOR: {
			auto* loop = (AST_loop*)ast;
				
			auto id  = format_id(loop_count++);
			auto cid = format_id(cont_count++);

			auto* loop_cond_block = llvm::BasicBlock::Create(*ctx, llvm::Twine("loop") + id.str + ".cond"); // loop->cond
			auto* loop_body_block = llvm::BasicBlock::Create(*ctx, llvm::Twine("loop") + id.str + ".body"); // loop->body
			auto* loop_end_block  = llvm::BasicBlock::Create(*ctx, llvm::Twine("loop") + id.str + ".end" ); // loop->end   need this block seperate from loop to allow for continue;
			auto* cont_block      = llvm::BasicBlock::Create(*ctx, llvm::Twine("cont") + cid.str         ); // code after loop

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
				build.CreateBr(ast->kind == A_DO_WHILE ? loop_body_block : loop_cond_block);
			}

			if (ast->kind != A_DO_WHILE) { // loop cond for (for & while)
				begin_basic_block(loop_cond_block);
						
				llvm::Value* cond = codegen_rval(loop->cond);

				build.CreateCondBr(cond, loop_body_block, cont_block);
			}

			{ // loop body
				begin_basic_block(loop_body_block);
						
				codegen(loop->body);
				
				build.CreateBr(loop_end_block);
			}

			{ // loop end
				begin_basic_block(loop_end_block);
						
				if (loop->end)
					codegen(loop->end);
				
				build.CreateBr(loop_cond_block);
			}
				
			if (ast->kind == A_DO_WHILE){ // loop cond (do-while)
				begin_basic_block(loop_cond_block);
						
				llvm::Value* cond = codegen_rval(loop->cond);

				build.CreateCondBr(cond, loop_body_block, cont_block);
			}
				
			// following code will be in cont block
			begin_basic_block(cont_block);

			loop_blocks.pop_back();

			return {};
		}

		case A_BREAK: {
			if (loop_blocks.empty())
				ERROR(ast->src, "break not inside of any loop");
				
			build.CreateBr(loop_blocks.back().break_block);
			unreachable();
			return {};
		}
		case A_CONTINUE: {
			if (loop_blocks.empty())
				ERROR(ast->src, "continue not inside of any loop");
			
			build.CreateBr(loop_blocks.back().continue_block);
			unreachable();
			return {};
		}
			
		case A_CALL: {
			auto* call = (AST_call*)ast;
			auto* fdef = (AST_funcdef*)call->fdef;

			llvm::SmallVector<llvm::Value*, 16> arg_values;
			arg_values.resize(call->resolved_args.count, nullptr);

			for (size_t i=0; i<call->resolved_args.count; ++i) {
				auto* arg = call->resolved_args[i];

				Value val = codegen(call->resolved_args[i]);
				
				// Always pass structs by reference for now
				if (arg->type.ty->tclass == TY_STRUCT) {
					// There might be RValue structs if I implement struct init syntax
					// Since I want to always pass structs by const ref, I might have to put it on the stack here
					assert(!val.rval);
					assert(val.type && val.type->isStructTy() && val.llvm_val->getType()->isPointerTy());

					arg_values[i] = val.llvm_val;
				}
				else {
					arg_values[i] = load_value(val);
				}
				assert(arg_values[i]);
			}

			llvm::Value* retval = build.CreateCall(fdef->llvm_func, arg_values, fdef->ret_struct ? "_call" : "");
			return Value::RValue(retval);
		}
			
		case A_RETURN: {
			auto* ret = (AST_return*)ast;

			for (size_t i=0; i<ret->args.count; ++i) {
				llvm::Value* expr = codegen_rval(ret->args[i]->expr);

				auto val = var_value(ret->args[i]->decl);

				store_value(val, expr);
			}
			
			return_ret_vars(ret);
			unreachable();
			return {};
		}

		case A_FUNCDECL:
		default:
			return {};
		}
	}
};

llvmModule llvm_gen_module (AST_Module& modl) {
	ZoneScoped;

	LLVM_gen llvm_gen = {
		modl.funcs, modl.structs
	};
	llvm_gen.generate(modl.filename);

	return llvmModule( llvm_gen.ctx, llvm_gen.modl ); // pass ownership to caller
}
