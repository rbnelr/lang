#pragma once
#include "common.hpp"
#include "ir_gen.hpp"

namespace IR {

struct IROpt {
	IR& ir;

	void copy_prop () {
		// propagate MOV <temp> = <anything>
		// since temps are only assigned once and only used after assignment copy propagation should be safe across blocks
		
		std::vector<Var> vars;
		vars.resize(ir.temp_count, Var{ VT_UNDEFINED, 0 });

		for (auto& i : ir.code) {
			if (i.lhs.type == VT_TEMPID && vars[i.lhs.id].type != VT_UNDEFINED) i.lhs = vars[i.lhs.id];
			if (i.rhs.type == VT_TEMPID && vars[i.rhs.id].type != VT_UNDEFINED) i.rhs = vars[i.rhs.id];

			if (i.type == MOVE && i.dst.type == VT_TEMPID) {
				vars[i.dst.id] = i.lhs;
			}
		}
	}

	void dead_code () {
		// eliminate temporaries if they are (no longer) referenced
		// should be safe globally across blocks

		std::vector<bool> vars;
		vars.resize(ir.temp_count, false);

		for (auto& i : ir.code) {
			if (i.lhs.type == VT_TEMPID) vars[i.lhs.id] = true;
			if (i.rhs.type == VT_TEMPID) vars[i.rhs.id] = true;
		}

		for (auto& i : ir.code) {
			if (i.dst.type == VT_TEMPID && vars[i.dst.id] == false) {
				i = { DEAD };
			}
		}
	}
};

void func_opt (IR& ir, strview const& funcname) {
	IROpt opt = {ir};

	if (options.print_ir) {
		printf(">>> Before copy propagate:\n");
		ir.dbg_print();
	}

	opt.copy_prop();

	if (options.print_ir) {
		printf(">>> After copy propagate:\n");
		ir.dbg_print();
	}

	opt.dead_code();

	if (options.print_ir) {
		printf(">>> After dead code elimination:\n");
	}
}

void ir_opt (IRGen& ir) {
	ZoneScoped;
	
	for (auto& func : ir.funcdefs) {
		auto& fir = ir.func_irs[func->codegen_funcid];

		if (options.print_ir)
			printf("\n%.*s():\n", (int)func->ident.size(), func->ident.data());

	#if 0
		// Totally buggy without SSA
		if (options.optimized)
			func_opt(fir, func->ident);
	#else
		if (options.print_ir)
			fir.dbg_print();
	#endif
	}
}

}
