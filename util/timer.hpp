#pragma once
#include "stdint.h"

uint64_t get_timestamp ();
extern uint64_t timestamp_freq;

struct Timer {
	uint64_t begin;

	static Timer start () {
		return { get_timestamp() };
	}
	float end () {
		uint64_t now = get_timestamp();
		return (float)(now - begin) / (float)timestamp_freq;
	}
};

#define TIME_START(name) auto __##name = Timer::start()
#define TIME_END(name) auto __##name##_time = __##name.end()

