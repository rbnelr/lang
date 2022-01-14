#include "llvm_pch.hpp"
#include "llvm_gen.hpp"
#include "builtins.hpp"

//#include "windows.h"

llvm::ExitOnError ExitOnErr;

struct LLVM_Backend {
	llvm::LLVMContext ctx; // TODO: can this go away after IR was built but before it was compiled?
	llvm::IRBuilder<> build;

	LLVM_Backend (): ctx{}, build{ctx} {}
};
std::unique_ptr<LLVM_Backend> llvm_backend;

void llvm_init () {
	if (llvm_backend) {
		ZoneScopedN("LLVM deinit");
		llvm_backend.reset(); // recreate to properly asses cost during profiling
	}

	ZoneScoped;
	llvm_backend = std::make_unique<LLVM_Backend>();
}

//// LLVM IR gen

struct LLVM_gen {
	llvm::LLVMContext& ctx;
	llvm::IRBuilder<>& build;
	
	std::vector<AST_funcdef*>& funcdefs;
	
	llvm::Module* modl;

	struct LoopLabels {
		size_t cont;
		size_t end;
	};
	std::vector<LoopLabels> loop_lbls;
	
	_FORCEINLINE llvm::StringRef SR (std::string_view sv) {
		return { sv.data(), sv.size() };
	}
	
	
	void declare_builtins () {
		// TODO: do this lazily once they are actually called?

		for (auto* builtin : builtin_funcs) {
			declare_function(builtin);
		}
	}
	void generate (strview const& filename) {
		ZoneScoped;

		modl = new llvm::Module("<main>", llvm_backend->ctx);
		modl->setSourceFileName(SR(filename));

		// declare builtin functions
		declare_builtins();
		
		// declare functions declared in source
		for (size_t funcid = 0; funcid < funcdefs.size(); ++funcid) {
			declare_function(funcdefs[funcid]);
		}

		// now that all callable functions are declared
		// generate IR for functions defined in source
		for (size_t funcid = 0; funcid < funcdefs.size(); ++funcid) {
			codegen_function(funcdefs[funcid]);
		}

		if (options.print_ir) {
			print_seperator("LLVM IR");
			modl->print(llvm::errs(), nullptr);
		}
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

			if (arg->init) {
				// TODO
				assert(false);
			}
		}

		auto* func_ty = llvm::FunctionType::get(ret_ty, args_ty, vararg);
		auto* func =  llvm::Function::Create(func_ty, llvm::Function::InternalLinkage, SR(fdef->ident), *modl);

		// set argument names
		unsigned i = 0;
		for (auto* arg = (AST_vardecl*)fdef->args; arg != nullptr && arg->type != A_VARARGS; arg = (AST_vardecl*)arg->next) {
			arg->llvm_value = func->getArg(i++);
			arg->llvm_value->setName(SR(arg->ident));
		}

		fdef->llvm_func = func;
		return func;
	}
	void codegen_function (AST_funcdef* fdef) {
		ZoneScoped;
		//// entry block + start recursively codegening of the function body
		auto* bb_entry = llvm::BasicBlock::Create(ctx, "entry", fdef->llvm_func);
		build.SetInsertPoint(bb_entry);

		codegen(fdef->body);
		
		//// implicit return instruction at end of function body
		build.CreateRetVoid();
		
		//// finish function
		verifyFunction(*fdef->llvm_func);
	}
	
	/*
	IROpType unary2ir (AST* ast, OpType op, Type type) {
		switch (op) {
			case OP_POSITIVE: {
				switch (type) {
					case INT: return OP_NONE; // no-op
					case FLT: return OP_NONE; // no-op
					default: throw CompilerExcept{"error: positive operator is not valid for type", ast->src_tok->source};
				}
			}
			case OP_NEGATE: {
				switch (type) {
					case INT: return OP_i_NEG;
					case FLT: return OP_f_NEG;
					default: throw CompilerExcept{"error: negate is not valid for type", ast->src_tok->source};
				}
			}
			case OP_NOT: {
				switch (type) {
					case INT : return OP_i_NOT;
					case BOOL: return OP_b_NOT;
					default: throw CompilerExcept{"error: not is not valid for type", ast->src_tok->source};
				}
			}
			case OP_INC: {
				switch (type) {
					case INT : return OP_i_INC;
					default: throw CompilerExcept{"error: increment is not valid for type", ast->src_tok->source};
				}
			}
			case OP_DEC: {
				switch (type) {
					case INT : return OP_i_DEC;
					default: throw CompilerExcept{"error: decrement is not valid for type", ast->src_tok->source};
				}
			}
			INVALID_DEFAULT;
		}
	}*/

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
						val = build.CreateGlobalStringPtr(lit->value.str, "strlit");
						break;
					INVALID_DEFAULT;
				}
				assert(val->getType() == map_type(lit->valtype));
				return val;
			}

			case A_VARDECL: {
				auto* vardecl = (AST_vardecl*)ast;

				if (vardecl->init) {
					vardecl->llvm_value = codegen(vardecl->init);
					return vardecl->llvm_value;
				}
				return {};
			}

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* vardecl = (AST_vardecl*)var->decl;
				return vardecl->llvm_value;
			}

			case A_BINOP: {
				auto* op = (AST_binop*)ast;

				assert(op->lhs->valtype == op->rhs->valtype);
				
				llvm::Value* lhs = codegen(op->lhs);
				llvm::Value* rhs = codegen(op->rhs);

				switch (op->valtype) {
				case INT: {
					switch (op->op) {
						case OP_ADD:        return build.CreateAdd(lhs, rhs, "addtmp");
						case OP_SUB:        return build.CreateSub(lhs, rhs, "subtmp");
						case OP_MUL:        return build.CreateMul(lhs, rhs, "multmp");
						case OP_DIV:        return build.CreateSDiv(lhs, rhs, "divtmp");
						//case OP_REMAINDER:  return OP_i_REMAIND;
						//case OP_LESS:       return OP_i_LT;
						//case OP_LESSEQ:     return OP_i_LE;
						//case OP_GREATER:    return OP_i_GT;
						//case OP_GREATEREQ:  return OP_i_GE;
						//case OP_EQUALS:     return OP_i_EQ;
						//case OP_NOT_EQUALS: return OP_i_NEQ;
						INVALID_DEFAULT;
					}
				} break;
				//case BOOL: {
				//	switch (op) {
				//		case OP_ADD:        return OP_i_ADD;
				//		case OP_SUB:        return OP_i_SUB;
				//		case OP_MUL:        return OP_i_MUL;
				//		case OP_DIV:        return OP_i_DIV;
				//		case OP_REMAINDER:  return OP_i_REMAIND;
				//			throw CompilerExcept{ "error: math ops not valid for this type", ast->src_tok->source };
				//
				//		case OP_LESS:       return OP_i_LT;
				//		case OP_LESSEQ:     return OP_i_LE;
				//		case OP_GREATER:    return OP_i_GT;
				//		case OP_GREATEREQ:  return OP_i_GE;
				//			throw CompilerExcept{ "error: can't compare bools like that", ast->src_tok->source };
				//
				//		case OP_EQUALS:     return OP_b_EQ; 
				//		case OP_NOT_EQUALS: return OP_b_NEQ;
				//		INVALID_DEFAULT;
				//	}
				//} break;
				//case FLT: {
				//	switch (op) {
				//		case OP_REMAINDER:
				//			throw CompilerExcept{ "error: remainder operator not valid for floats", ast->src_tok->source };
				//		case OP_ADD:        return OP_f_ADD;
				//		case OP_SUB:        return OP_f_SUB;
				//		case OP_MUL:        return OP_f_MUL;
				//		case OP_DIV:        return OP_f_DIV;
				//		case OP_LESS:       return OP_f_LT;
				//		case OP_LESSEQ:     return OP_f_LE;
				//		case OP_GREATER:    return OP_f_GT;
				//		case OP_GREATEREQ:  return OP_f_GE;
				//		case OP_EQUALS:     return OP_f_EQ;
				//		case OP_NOT_EQUALS: return OP_f_NEQ;
				//		INVALID_DEFAULT;
				//	}
				//} break;
				default:
					throw CompilerExcept{ "error: math ops not valid for this type", ast->src_tok->source };
				}
			}

			case A_BLOCK: {
				auto* block = (AST_block*)ast;

				for (auto* n=block->statements; n != nullptr; n = n->next) {
					codegen(n);
				}

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

				return build.CreateCall(fdef->llvm_func, callargs, fdef->rets ? "calltmp" : "");
			}
			
			case A_RETURN: {
				auto* ret = (AST_return*)ast;

				// TODO: implement multiple returns
				assert(ret->argc <= 1);

				llvm::Value* retval = nullptr;
				for (auto* arg = (AST_callarg*)ret->args; arg != nullptr; arg = (AST_callarg*)arg->next) {
					llvm::Value* val = codegen(arg->expr);
					retval = val;
				}
				
				if (retval)
					build.CreateRet(retval);
				else
					build.CreateRetVoid();

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
		llvm_backend->ctx,
		llvm_backend->build,
		funcdefs
	};
	llvm_gen.generate(filename);

	return llvm_gen.modl; // pass ownership to caller
}
void llvm_free_module (llvm::Module* modl) {
	delete modl;
}

//// LLVM jit & exec

struct JIT {
	class Resolver : public llvm::JITSymbolResolver {
	public:
		Resolver () {}

		virtual ~Resolver() = default;
		
		/// Returns the fully resolved address and flags for each of the given
		///        symbols.
		///
		/// This method will return an error if any of the given symbols can not be
		/// resolved, or if the resolution process itself triggers an error.
		virtual void lookup(const LookupSet &Symbols,
							OnResolvedFunction OnResolved) {

			std::map<llvm::StringRef, llvm::JITEvaluatedSymbol> results;

			for (auto& Sym : Symbols) {
				if (Sym == "printf") {
					results.emplace(Sym, llvm::JITEvaluatedSymbol{
						(llvm::JITTargetAddress)&my_printf,
						llvm::JITSymbolFlags::Absolute | // TODO: do I need this?
						llvm::JITSymbolFlags::Callable
					});
				}
				else {
					assert(false);
				}
			}

			OnResolved(results);
		}

		/// Returns the subset of the given symbols that should be materialized by
		/// the caller. Only weak/common symbols should be looked up, as strong
		/// definitions are implicitly always part of the caller's responsibility.
		virtual llvm::Expected<LookupSet>
		getResponsibilitySet(const LookupSet &Symbols) {
			LookupSet Result;
			assert(Symbols.size() == 0);
			return Result;
		}

		/// Specify if this resolver can return valid symbols with zero value.
		//virtual bool allowsZeroSymbols() { return false; }

	};

	Resolver                             resolver;
	llvm::SectionMemoryManager           MM;
	
	llvm::RuntimeDyld                    dyld;

	std::unique_ptr<llvm::TargetMachine> TM;

	JIT (): resolver{}, MM{}, dyld{MM, resolver} {
		ZoneScoped;

		// TODO: I can't pass -debug to my own app and expect LLVM to set this flag can I?
		// so just set it manually to print stuff?
	#ifndef NDEBUG
		//llvm::DebugFlag = true;
	#endif

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();

		auto triple_str = llvm::sys::getProcessTriple();
		auto triple = llvm::Triple(triple_str);

		llvm::orc::JITTargetMachineBuilder JTMB(triple);

		TM = llvm::cantFail( JTMB.createTargetMachine() );
	}
	
	void setup_PM (llvm::legacy::PassManager& PM, llvm::raw_svector_ostream& ObjStream) {
		
		// mem2reg pass for alloca'd local vars
		//PM.add(llvm::createPromoteMemoryToRegisterPass());

		//
		llvm::MCContext* Ctx;
		if (TM->addPassesToEmitMC(PM, Ctx, ObjStream)) {
			ExitOnErr( llvm::make_error<llvm::StringError>(
				"Target does not support MC emission",
				llvm::inconvertibleErrorCode())
			);
		}
	}

	void compile_and_load (llvm::Module* modl) {
		
		llvm::SmallVector<char, 0> ObjBufferSV;
		llvm::raw_svector_ostream ObjStream(ObjBufferSV);

		{
			llvm::legacy::PassManager PM;
			setup_PM(PM, ObjStream);

			PM.run(*modl);
		}

		llvm::SmallVectorMemoryBuffer ObjBuffer {
			std::move(ObjBufferSV),
			modl->getModuleIdentifier() + "-jitted-objectbuffer"
		};
			
		auto Obj = ExitOnErr(
			llvm::object::ObjectFile::createObjectFile(ObjBuffer.getMemBufferRef())
		);
			
		auto loadedObj = dyld.loadObject(*Obj);
	}
	
	
	void jit_and_execute (llvm::Module* modl) {
		ZoneScoped;

		auto DL = TM->createDataLayout();

		modl->setDataLayout(DL);
		
		compile_and_load(modl);

		dyld.finalizeWithMemoryManagerLocking(); // calls resolveRelocations
		
		print_seperator("Execute JITed LLVM code:");
		typedef void (*main_fp)();
		auto fptr = (main_fp)dyld.getSymbol("main").getAddress();
		fptr();
	}
};

void llvm_jit_and_exec (llvm::Module* modl) {
	ZoneScoped;

	JIT jit {};

	jit.jit_and_execute(modl);
}
