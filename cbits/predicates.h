#ifndef H_WARDEN_PREDICATES
#define H_WARDEN_PREDICATES

#include "warden.h"

static inline bool is_digit(char c) {
	if (c >= '0' && c <= '9') {
		return TRUE;
	}
	return FALSE;
}

#endif
