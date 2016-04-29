#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "row.h"

void warden_ascii_to_lower(size_t n, int8_t *in, int8_t *out) {
	int i;
	memcpy(out, in, n);
	for (i = 0; i < n; i++) {
		out[i] |= 0x20;
	}
}
