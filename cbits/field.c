#include "warden.h"

/* matches /(t|true)/i */
static inline bool bool_true(char *buf, size_t n) {
	if (n == 1) {
		return TRUE; /* "T" or "t" */
	}
	if (n != 4) {
		return FALSE;
	}
	if ((buf[1] | 0x20) == 'r' &&
	    (buf[2] | 0x20) == 'u' &&
	    (buf[3] | 0x20) == 'e') {
		return TRUE;
	}
	return FALSE;
}

/* matches /(f|false)/i */
static inline bool bool_false(char *buf, size_t n) {
	if (n == 1) {
		return TRUE; /* "F" or "f" */
	}
	if (n != 5) {
		return FALSE;
	}
	if ((buf[1] | 0x20) == 'a' &&
	    (buf[2] | 0x20) == 'l' &&
	    (buf[3] | 0x20) == 's' &&
	    (buf[4] | 0x20) == 'e') {
		return TRUE;
	}
	return FALSE;
}

bool warden_field_bool(char *buf, size_t n) {
	char c;
	if (n < 1) {
		return FALSE;
	}
	c = buf[0] | 0x20;
	if (c == 't') {
		return bool_true(buf, n);
	} else if (c == 'f') {
		return bool_false(buf, n);
	}
	return FALSE;
}
