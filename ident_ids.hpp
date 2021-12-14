#pragma once
#include "common.hpp"

typedef size_t ident_id_t;

struct IdentiferIDs {
	ident_id_t next_id = 1;

	std::unordered_map<strview, ident_id_t> ident_map;

	ident_id_t get_id (strview const& name) {
		auto ret = ident_map.try_emplace(name, next_id);
		if (ret.second) {
			// name did not exist yet, next_id was inserted, increment next_id
			next_id++;
		}
		return ret.first->second; // return existing or newly inserted id
	}
};
