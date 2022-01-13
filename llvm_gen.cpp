
#include <inttypes.h>

#pragma warning(push, 0)
#pragma warning (disable : 4244)

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Verifier.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"

#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"

#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h" // warning C4244: 'initializing': conversion from '_Ty' to '_Ty2', possible loss of data

#pragma warning(pop)

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

	std::vector<llvm::Function*> func_irs;

	struct LoopLabels {
		size_t cont;
		size_t end;
	};
	std::vector<LoopLabels> loop_lbls;
	size_t                  return_lbl;

	void generate () {
		ZoneScoped;

		func_irs.resize(funcdefs.size());

		modl = new llvm::Module("llvm_test", llvm_backend->ctx);

		add_printf();

		for (size_t funcid = 0; funcid < funcdefs.size(); ++funcid) {
			auto& func = funcdefs[funcid];
			func->codegen_funcid = funcid;

			func_irs[funcid] = funcdef(func);
		}

		if (options.print_ir)
			modl->print(llvm::errs(), nullptr);
	}
	
	llvm::Function* _printf;
	llvm::Function* add_printf () {
		llvm::Type* ret_ty = llvm::Type::getVoidTy(ctx);
		std::vector<llvm::Type*> args_ty = {
			llvm::Type::getInt8PtrTy(ctx) // AddressSpace ?
		};
		auto* func_ty = llvm::FunctionType::get(ret_ty, args_ty, true);
		
		_printf = llvm::Function::Create(func_ty, llvm::Function::ExternalLinkage, "printf", *modl);
		return _printf;
	}

	llvm::Function* funcdef (AST_funcdef* func_def) {
		ZoneScoped;

		// Func args and returns here
		
		llvm::Type* ret_ty = llvm::Type::getVoidTy(ctx);
		std::vector<llvm::Type*> args_ty = {};

		auto* func_ty = llvm::FunctionType::get(ret_ty, args_ty, false);
		
		auto* func = llvm::Function::Create(func_ty, llvm::Function::InternalLinkage, "main", *modl);
		
		auto* bb_entry = llvm::BasicBlock::Create(ctx, "entry", func);
		build.SetInsertPoint(bb_entry);

		gen(func_def->body);
		
		build.CreateRetVoid();
		
		verifyFunction(*func);

		return func;
	}
	
	struct Var {

	};

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

	llvm::Value* gen (AST* ast) {
		switch (ast->type) {
			case A_LITERAL: {
				auto* lit = (AST_literal*)ast;
				
				using namespace llvm;
				using llvm::Type;

				switch (lit->valtype) {
					case BOOL: return lit->value.b ? build.getTrue() : build.getFalse();
					case INT:  return ConstantInt::get(Type::getInt64Ty(ctx), lit->value.i, true);
					case FLT:  return ConstantFP ::get(Type::getDoubleTy(ctx), APFloat(lit->value.f));
					case STR:  return build.CreateGlobalStringPtr(lit->value.str, "strlit");
					INVALID_DEFAULT;
				}
			}

			case A_VARDECL: {
				auto* var = (AST_vardecl*)ast;

				if (var->init) {
					var->llvm_value = gen(var->init);
					return (llvm::Value*)var->llvm_value;
				}
				return {};
			}

			case A_VAR: {
				auto* var = (AST_var*)ast;
				auto* vardecl = (AST_vardecl*)var->decl;
				return (llvm::Value*)vardecl->llvm_value;
			}

			case A_BINOP: {
				auto* op = (AST_binop*)ast;

				assert(op->lhs->valtype == op->rhs->valtype);
				
				llvm::Value* lhs = gen(op->lhs);
				llvm::Value* rhs = gen(op->rhs);

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
					gen(n);
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

				bool vararg     = false;
				size_t vararg_i = 0;

				size_t calli = 0;
				for (auto* arg = (AST_callarg*)call->args; arg != nullptr; arg = (AST_callarg*)arg->next) {
					callargs.push_back( gen(arg->expr) );

					if (arg->decl->type == A_VARARGS) {
						vararg = true;
						vararg_i = calli;
					}

					declargs[arg->decli].set = true;
					declargs[arg->decli].callarg = calli++;
				}

				if (fdef == (AST_funcdef*)&BF_PRINTF) {
					return build.CreateCall(_printf, callargs); // TODO: non-void returns can get a "calltmp" name
				}
				else {
					assert(false);
					return {};
				}
			}

			case A_FUNCDEF:
			default:
				return {};
		}
	}
};

llvm::Module* llvm_gen_module (std::vector<AST_funcdef*>& funcdefs) {
	ZoneScoped;

	LLVM_gen llvm_gen = {
		llvm_backend->ctx,
		llvm_backend->build,
		funcdefs
	};
	llvm_gen.generate();

	return llvm_gen.modl; // pass ownership to caller
}
void llvm_free_module (llvm::Module* modl) {
	delete modl;
}

//// LLVM jit & exec

#define TEST 1

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

struct JIT {

#if TEST
	std::unique_ptr<Resolver> resolver;
	std::unique_ptr<llvm::RuntimeDyld::MemoryManager> MM;
	std::unique_ptr<llvm::RuntimeDyld> RD;
#else
	ThreadSafeContext TSC;
	ThreadSafeModule  TSM;
	
	std::unique_ptr<ExecutionSession>  ES;
	
	std::unique_ptr<DataLayout>               DL;
	std::unique_ptr<MangleAndInterner>        Mangle;
	
	std::unique_ptr<RTDyldObjectLinkingLayer> ObjectLayer;
	std::unique_ptr<IRCompileLayer>           CompileLayer;
	
	JITDylib*                                 MainJD;
#endif

	void init () {
		ZoneScoped;

		// TODO: I can't pass -debug to my own app and expect LLVM to set this flag can I?
		// so just set it manually to print stuff?
	#ifndef NDEBUG
		//llvm::DebugFlag = true;
	#endif

		llvm::InitializeNativeTarget();
		llvm::InitializeNativeTargetAsmPrinter();
		llvm::InitializeNativeTargetAsmParser();
	#if TEST
		resolver = std::make_unique<Resolver>();

		MM = std::make_unique<llvm::SectionMemoryManager>();
		RD = std::make_unique<llvm::RuntimeDyld>(*MM, *resolver);
	#else
		TSC = ThreadSafeContext(std::make_unique<LLVMContext>());
		TSM = ThreadSafeModule(std::make_unique<Module>("llvm_test", *TSC.getContext()), TSC);
		
		auto EPC = ExitOnErr( SelfExecutorProcessControl::Create() );
		ES = std::make_unique<ExecutionSession>(std::move(EPC));
		
		JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());
		
		DL = std::make_unique<DataLayout>(ExitOnErr( JTMB.getDefaultDataLayoutForTarget() ));
		
		Mangle = std::make_unique<MangleAndInterner>(*ES, *DL);
		
		ObjectLayer = std::make_unique<RTDyldObjectLinkingLayer>(*this->ES, []() { return std::make_unique<SectionMemoryManager>(); });
		
		CompileLayer = std::make_unique<IRCompileLayer>(*this->ES, *ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB)));
		
		MainJD = &ES->createBareJITDylib("<main>");
		
		MainJD->addGenerator(cantFail(
			DynamicLibrarySearchGenerator::GetForCurrentProcess(DL->getGlobalPrefix())
		));
		
		TSM.getModuleUnlocked()->setDataLayout(*DL);
		
		if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
			ObjectLayer->setOverrideObjectFlagsWithResponsibilityFlags(true);
			ObjectLayer->setAutoClaimResponsibilityForObjectSymbols(true);
		}
	#endif
	}

	void destroy () {
	#if TEST
	#else
		if (auto Err = ES->endSession())
			ES->reportError(std::move(Err));
	#endif
	}
	
	void jit_and_execute (llvm::Module* modl) {
		ZoneScoped;

	#if TEST
		auto EPC = ExitOnErr( llvm::orc::SelfExecutorProcessControl::Create() );
		auto ES = std::make_unique<llvm::orc::ExecutionSession>(std::move(EPC)); // TOOD: can get rid of ES

		llvm::orc::JITTargetMachineBuilder JTMB(ES->getExecutorProcessControl().getTargetTriple());

		auto DL = std::make_unique<llvm::DataLayout>(ExitOnErr( JTMB.getDefaultDataLayoutForTarget() ));

		modl->setDataLayout(*DL);

		auto TM = llvm::cantFail( JTMB.createTargetMachine() );
		auto SC = llvm::orc::SimpleCompiler(*TM);

		auto O = ExitOnErr( SC(*modl) );

		auto Obj = ExitOnErr( llvm::object::ObjectFile::createObjectFile(*O) );

		auto loadedO = RD->loadObject(*Obj);

		RD->finalizeWithMemoryManagerLocking(); // calls resolveRelocations
		
		typedef void (*main_fp)();
		auto fptr = (main_fp)RD->getSymbol("main").getAddress();

	#else
		auto RT = MainJD->getDefaultResourceTracker();
		CompileLayer->add(RT, std::move(TSM));
		
		auto func2 = ES->lookup({MainJD}, (*Mangle)("test"));
		//auto faddr = func->getAddress();
		
		auto func = ES->lookup({MainJD}, (*Mangle)("test"));
		auto faddr = func->getAddress();

		auto fptr = (func_t)faddr;
	#endif
		
		fptr();
	}
};

//void llvm_test () {
//	JIT jit;
//	jit.create();
//
//#if TEST
//	jit.compile_test(jit.ctx.get(), jit.modl.get());
//#else
//	jit.compile_test(jit.TSC.getContext(), jit.TSM.getModuleUnlocked());
//#endif
//
//	jit.run_test();
//	jit.destroy();
//
//	exit(0);
//}

void llvm_jit_and_exec (llvm::Module* modl) {
	ZoneScoped;

	JIT jit;
	jit.init();
	jit.jit_and_execute(modl);
}
