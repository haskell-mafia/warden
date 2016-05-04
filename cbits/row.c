#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "row.h"

void warden_ascii_to_lower(size_t n, int8_t *out, int8_t *in) {
	int i = 0;
	memcpy(out, in, n);
	if (n >= 8) {
		for ( ; i < n; i += 8) {
			int64_t *p = (int64_t *) &out[i];
			*p |= 0x2020202020202020;
		}
		i -= 8;
	}
	for ( ; i < n; i++) {
		out[i] |= 0x20;
	}
}
