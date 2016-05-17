#ifndef H_WARDEN_PII
#define H_WARDEN_PII

#include <stdlib.h>

#include "warden.h"

bool warden_check_email(char *, size_t);

bool warden_check_phone_number(char *, size_t);

bool warden_check_address(char *, size_t);

#endif
