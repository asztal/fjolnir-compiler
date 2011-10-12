#include <stdint.h>

typedef uintptr_t Value;

#define makeWord(x) (((x)<<1)|1)

Value makeStef(void* fun, int rv, int v);
void* loadStef(Value stef, int m, int n);
Value makeReal(double);
Value makeString(const char*);
const static Value nil = 0;
