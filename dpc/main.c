#include <stdio.h>
#include <stdint.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");

typedef uint64_t SNAKEVAL;
const uint64_t BOOL_TAG   = 0x0000000000000001;
const SNAKEVAL BOOL_TRUE  = 0x8000000000000001; // These must be the same values
const SNAKEVAL BOOL_FALSE = 0x0000000000000001; // as chosen in compile.ml
SNAKEVAL print(SNAKEVAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#018lx", val); // print unknown val in hex
  }
  return val;
}

int main(int argc, char** argv) {
  int64_t result = our_code_starts_here();
  print(result);
  return 0;
}
