#ifndef OVER_C_CORE_H
#define OVER_C_CORE_H

#include <stdint.h>
#include <stdbool.h>

#define Int8 int8_t
#define Int16 int16_t
#define Int32 int32_t
#define Int64 int64_t
#define Int Int64
#define UInt8 uint8_t
#define UInt16 uint16_t
#define UInt32 uint32_t
#define UInt64 uint64_t
#define UInt UInt64
#define Float32 float
#define Float64 double
#define Float Float64
#define Bool _Bool
#define Char char

typedef struct String {
    Int length;
    Char *ptr;
} String;

#endif