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
		vars.resize(ir.temp_count, Var{ UNDEFINED, 0 });

		for (auto& i : ir.code) {
			if (i.lhs.type == TEMP && vars[i.lhs.id].type != UNDEFINED) i.lhs = vars[i.lhs.id];
			if (i.rhs.type == TEMP && vars[i.rhs.id].type != UNDEFINED) i.rhs = vars[i.rhs.id];

			if (i.type == MOVE && i.dst.type == TEMP) {
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
			if (i.lhs.type == TEMP) vars[i.lhs.id] = true;
			if (i.rhs.type == TEMP) vars[i.rhs.id] = true;
		}

		for (auto& i : ir.code) {
			if (i.dst.type == TEMP && vars[i.dst.id] == false) {
				i = { DEAD };
			}
		}
	}
};

void ir_opt (IR& ir) {
	IROpt opt = {ir};

	printf(">>> Before copy propagate:\n");
	ir.dbg_print();
	
	opt.copy_prop();
	
	printf(">>> After copy propagate:\n");
	ir.dbg_print();
	
	opt.dead_code();
	
	printf(">>> After dead code elimination:\n");
	ir.dbg_print();
}

}
