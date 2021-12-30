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

#ifndef TRACY_ENABLE
	printf(":: %.*s:\n", (int)funcname.size(), funcname.data());

	//printf(">>> Before copy propagate:\n");
	//ir.dbg_print();
#endif

	opt.copy_prop();

#ifndef TRACY_ENABLE
	//printf(">>> After copy propagate:\n");
	//ir.dbg_print();
#endif

	opt.dead_code();

#ifndef TRACY_ENABLE
	printf(">>> After dead code elimination:\n");
	ir.dbg_print();
#endif
}

void ir_opt (IRGen& ir) {
	ZoneScoped;
	
	for (auto& func : ir.funcdefs) {
		auto& fir = ir.func_irs[func->codegen_funcid];
		func_opt(fir, func->decl.ident);
	}
}

}
