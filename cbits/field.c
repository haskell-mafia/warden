#include <string.h>

#include "warden.h"

bool warden_field_bool(char *buf, size_t n) {
	/* little-endian "false" */
	static const int64_t false_bits = 0x00000065736c6166;
	static const int64_t false_mask = 0x000000ffffffffff;
	/* little-endian "true" */
	static const int64_t true_bits  = 0x0000000065757274;
	static const int64_t true_mask  = 0x00000000ffffffff;
	/* "f" */
	static const int64_t f_bits     = 0x0000000000000066;
	/* "t" */
	static const int64_t t_bits     = 0x0000000000000074;
	static const int64_t tf_mask    = 0x00000000000000ff;

	/* Will be either the first 8 bytes of buf, or all of buf with
	   zero-padding. */
	int64_t first_word = 0;
	size_t word_bytes = 8;

	if (n < word_bytes) {
		word_bytes = n;
	}

	/* A direct read would be faster here, but would require handling
	   ByteStrings differently so we can have at least a word's
	   padding at the end. */
	memcpy(&first_word, buf, word_bytes);

	/* Lowercase. */
	first_word |= 0x2020202020202020;

	if (((first_word & true_mask) == true_bits && n == 4) ||
	    ((first_word & false_mask) == false_bits && n == 5) ||
	    ((first_word & tf_mask) == t_bits && n == 1) ||
	    ((first_word & tf_mask) == f_bits && n == 1)) {
		return TRUE;
	}
	return FALSE;
}
