#ifndef H_WARDEN_PII
#define H_WARDEN_PII

#include <stdlib.h>

#include "warden.h"

bool warden_check_email(const char *, size_t);

bool warden_check_phone_number(const char *, size_t);

bool warden_check_address(const char *, size_t);

bool warden_check_creditcard(const char *, size_t);

#endif
