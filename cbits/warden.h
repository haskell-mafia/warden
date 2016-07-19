#ifndef H_WARDEN
#define H_WARDEN

#include <stdlib.h>
#include <stdint.h>

#include <endian.h>

#if __BYTE_ORDER != __LITTLE_ENDIAN
#error "warden only works on little-endian architectures."
#endif

typedef uint8_t bool;

#define TRUE 1
#define FALSE 0

#endif
