#include "mjollnir.h"

#include <stdio.h>

static Value test(Value* x, Value* y) {
    puts("loadStef test");
}
    
Value makeStef(void* fun, int m, int n) {
    return nil;
}

void* loadStef(Value stef, int m, int n) {
    return test;
}

