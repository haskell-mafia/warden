#include <string.h>

#include "field.h"
#include "predicates.h"

/* Returns TRUE if the buffer we're passed contains a bool, otherwise
 * FALSE. */
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

/* Returns integral_field for a (possibly signed) decimal integer.

   Returns real_field for a (possibly signed) decimal number, possibly
   in scientific notation.

   Otherwise returns non_numeric_field. */
numeric_field warden_field_numeric(char *buf, size_t n) {
	size_t i = 0;
	int preradix_digits = 0; /* digits before the radix point */
	int exponent_digits = 0; /* digits in the exponent (scientific notation) */

	if (n < 1) {
		return non_numeric_field;
	}
	if (buf[i] == '+' || buf[i] == '-') {
		i++;
	}
	while (is_digit(buf[i]) && i < n) {
		i++;
		preradix_digits++;
	}

	/* "+" or "-" on its own */
	if (preradix_digits == 0) {
		return non_numeric_field;
	}

	/* got the whole int, we're done */
	if (i == n) {
		return integral_field;
	}

	/* optional postradix part */
	if (buf[i] == '.') {
		i++;
		while (i < n && is_digit(buf[i])) {
			i++;
		}
		/* one of "n." or "n.m", counts as real either way */
		if (i == n) {
			return real_field;
		}
	}

	/* scientific notation, or just cruft on the end? */
	if (buf[i] != 'e') {
		/* cruft */
		return non_numeric_field;
	}

	/* scientific notation */

	i++;

	if (i < n && (buf[i] == '+' || buf[i] == '-')) {
		i++;
	}

	while (i < n && is_digit(buf[i])) {
		i++;
		exponent_digits++;
	}

	/* need at least one exponent digit, "1.2e-" doesn't make sense on its
	   own */
	if (i == n && exponent_digits) {
		return real_field;
	}

	/* just cruft on the end after all */
	return non_numeric_field;
}


static inline bool is_separator(char c) {
	return (c == '-' || c == '/' || c == '.');
}

/* Match a year in the 20xx century, in big-endian date format with or
   without separators. */
static inline bool match_ymd(const char *buf, size_t n) {
	/* The shortest thing we're willing to call a "date" at this
	   point is YYYYMMDD. */
	if (n < 8) {
		return FALSE;
	}

	/* 0xc0 = 0x80 | 0x40 - if these bits are set, the byte is too
	   high to be a digit or a separator. */
	static const int64_t ymd_mask = 0xc0c0c0c0c0c0ffff;

	/* No 0x80 or 0x40 set anywhere, and the first two bytes must
	   be "20". */
	static const int64_t ymd_bits = 0x0000000000003032;
	int64_t *p = (int64_t *) buf;

	/* First, we drop everything which doesn't start with '20' and
	   have eight bytes compatible with a YYYYxMMxDD format. */
	if (!(((*p & ymd_mask) == ymd_bits) && is_digit(buf[2]) && is_digit(buf[3]))) {
		return FALSE;
	}

	/* YYYY-MM-DD */
	if (is_separator(buf[4])) {
		return (n >= 10 &&
			is_digit(buf[5]) &&
			is_digit(buf[6]) &&
			is_separator(buf[7]) &&
			is_digit(buf[8]) &&
			is_digit(buf[9]));
	}

	/* YYYYMMDD */
	return (is_digit(buf[4]) &&
		is_digit(buf[5]) &&
		is_digit(buf[6]) &&
		is_digit(buf[7]));

}

/* Returns TRUE if the data in the buffer looks like a date, otherwise
   FALSE.

   Currently checks:

    - Fields beginning with big-endian dates.

   FIXME: more supported date formats
*/
bool warden_field_datetime(char *buf, size_t n) {
	return match_ymd(buf, n);
}
