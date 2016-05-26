#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "pii.h"
#include "predicates.h"

/* Stuff we expect to find separating digit groups in phone numbers,
 * credit card numbers, et cetera. */
static inline bool is_number_filler(char c) {
	if (c == ' ' || c == '-' || c == '.') {
		return TRUE;
	}
	return FALSE;
}

/*
  Email address checks.
*/

bool warden_check_email(char *buf, size_t n) {
	size_t i = 1;

	/* local part */

	if (n < 1 || buf[0] == '@') {
		return FALSE;
	}

	while (i < n) {
		char c = buf[i];
		i++;
		if (c == '(' || c == ')') {
			return FALSE;
		}
		if (c == '@') {
			break;
		}
	}

	/* host part */

	while (i < n) {
		char c = buf[i];
		i++;
		if (c == '@' || c == ' ') {
			return FALSE;
		}
		if (c == '.') {
			break;
		}
	}

	/* Domain/tld part. */

	/* TLD must be at least two characters. */
	if (i + 2 > n) {
		return FALSE;
	}
	/* FIXME: make this bit more restrictive */
	for ( ; i < n; i++) {
		if (buf[i] == '@') {
			return FALSE;
		}
	}
	return TRUE;
}

/*
   Phone number checks.
*/

/* Matches numbers of the form +xxxxxxxxxxx
   Expects the initial '+' to be removed/already checked. */
static inline bool check_international_phone(char *buf, size_t n) {
	size_t i;
	int phone_chars = 0;
	/* We need exactly 11 digits, but don't care if there are
	   filler characters as well. */
	for (i = 0; i < n; i++) {
		if (is_digit(buf[i])) {
			phone_chars++;
		} else if (!is_number_filler(buf[i])) {
			return FALSE;
		}
	}
	if (phone_chars == 11) {
		return TRUE;
	}
	return FALSE;
}

static inline bool check_australian_phone(char *buf, size_t n) {
	size_t i;
	int phone_chars = 0;
	/* length already checked, >= 10 */
	if (buf[0] != '0') {
		return FALSE;
	}
	/* ensure valid area code */
	if (buf[1] != '2' /* NSW/ACT */
	    && buf[1] != '3' /* VIC/TAS */
	    && buf[1] != '4' /* mobiles */
	    && buf[1] != '7' /* QLD */
	    && buf[1] != '8' /* SA/WA/NT */
		) {
		return FALSE;
	}
	/* Count the rest of the digits - we want exactly 8 more,
	   excluding filler. */
	for (i = 2; i < n; i++) {
		if (is_digit(buf[i])) {
			phone_chars++;
		} else if (!is_number_filler(buf[i])) {
			return FALSE;
		}
	}
	if (phone_chars == 8) {
		return TRUE;
	}
	return FALSE;
}

bool warden_check_phone_number(char *buf, size_t n) {
	/* Field too short, no point checking it. */
	if (n < 10) {
		return FALSE;
	}
	/* If it might be an international number, strip the + and
	   pass the rest for validation. */
	if (buf[0] == '+') {
		return check_international_phone(buf + 1, n - 1);
	}
	return check_australian_phone(buf, n);
}
/*
   Address checks.
*/

#define N_STREET_TYPES 7

/* Only need to check unique prefixes here, e.g., "st" matches
 * "street" as well. */
static char *street_types[N_STREET_TYPES] = {
	"st",
	"rd",
	"road",
	"lane",
	"ln",
	"cres",
	"ave"
};

static inline bool check_street_type(char *buf, size_t n) {
	int i;
	/* no street types shorter than this */
	if (2 > n) {
		return FALSE;
	}
	for (i = 0; i < N_STREET_TYPES; i++) {
		size_t s = strlen(street_types[i]);
		if (s > n) {
			continue;
		}
		if (memcmp(buf, street_types[i], s) == 0) {
			return TRUE;
		}
	}
	return FALSE;
}

/* Check if a character is lowercase alpha. */
static inline bool is_alpha(char c) {
	if (c >= 'a' && c <= 'z') {
		return TRUE;
	}
	return FALSE;
}

bool warden_check_address(char *buf, size_t n) {
	size_t i;
	if (n < 1) {
		return FALSE;
	}
	/* First character should be part of a street number. */
	if (!is_digit(buf[0])) {
		return FALSE;
	}

	/* street number part */

	for (i = 1; i < n; i++) {
		if (!is_digit(buf[i]) && buf[i] != '/') {
			if (buf[i] != ' ') {
				return FALSE;
			}
			i++;
			break;
		}
	}

	/* street name part */

	if (i >= n) {
		return FALSE;
	}
	if (!is_alpha(buf[i])) {
		return FALSE;
	}
	i++;
	for ( ; i < n; i++) {
		if (!is_alpha(buf[i])) {
			if (buf[i] != ' ') {
				return FALSE;
			}
			i++;
			break;
		}
	}
	/* Check that a valid street type is present as a suffix; we
	   don't care what else is at the end of the string. */
	return check_street_type(buf + i, n - i);
}

/* Use the Luhn algorithm to check for potential credit card numbers,
 * after performing some sanity checks on the length.
 *
 * https://en.wikipedia.org/wiki/Luhn_algorithm */
bool warden_check_creditcard(char *buf, size_t n) {
	char c;
	size_t i;
	int luhn_sum = 0;
	int num_digits = 0;
	bool doubling_digit = FALSE;

	/* CC numbers are of varying length, but no major vendor is
	 * less than 12 bytes (Maestro). The longest is VISA (19),
	 * and we leave some room for filler characters. */
	if (n < 12 || n > 25) {
		return FALSE;
	}

	/* Walk backwards through the field accumulating a Luhn sum
	 * and exiting early if we see impossible things. */
	for (i = n; i > 0; i--) {
		c = buf[i-1];
		/* If it's a digit, add its numeric value to the Luhn
		 * sum. */
		if (is_digit(c)) {
			int x = c - '0';

			num_digits++;

			/* We double every second digit from the right-hand-side
			 * (starting with the penultimate digit). */
			if (doubling_digit) {
				x *= 2;
				if (x > 9) {
					x -= 9;
				}
			}
			luhn_sum += x;
			doubling_digit = ~doubling_digit;
		}
		/* Letters and random punctuation don't belong here,
		 * but we expect hyphens or spaces. */
		else if (!is_number_filler(c)) {
			return FALSE;
		}
	}

	/* Check digit must be correct and we must have seen at least
	 * twelve digits (to avoid cases like "0-----------" matching). */
	return (luhn_sum % 10 == 0) && (num_digits >= 12);
}
