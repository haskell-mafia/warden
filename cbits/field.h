#ifndef H_WARDEN_FIELD
#define H_WARDEN_FIELD

#include <stdlib.h>
#include <stdint.h>

#include "warden.h"

typedef enum _numeric_field {
	non_numeric_field = 0,
	integral_field = 1,
	real_field = 2
} numeric_field;

bool warden_field_bool(const char *, size_t);

numeric_field warden_field_numeric(const char *, size_t);

bool warden_field_datetime(const char *, size_t);

#endif
