#include <stdint.h>

typedef uintptr_t Value;

#define makeWord(x) ((Value)((((uintptr_t)(x))<<4)|T_WORD))
#define isWord(x) (((x)&7)==T_WORD)
#define getWord(x) ((uint16_t)((x)>>4))
#define getWordS(x) ((int16_t)((x)>>4))

#define makeThing(x,t) ((Value)(((uintptr_t)(x))|(t)))
#define getThing(c,x) ((c*)((x)&(~((uintptr_t)7))))
#define whatThing(x) (((uintptr_t)(x))&7)

typedef struct stef {
    void *fn;
    uint16_t m;
    uint16_t n;
} stef;

typedef struct string {
    uintptr_t length;
    unsigned char data[0];
} string;

typedef struct pair {
    Value first;
    Value second;
} pair;

typedef struct array {
    uintptr_t size;
    Value *data;
} array;

#define T_NIL 0
#define T_WORD 1    // 16-bit word
#define T_REAL 2
#define T_STRING 3
#define T_PAIR 4
#define T_STEF 5
#define T_ARRAY 6
#define T_OTHER 7

Value makeStef(void* fun, int rv, int v);
void* loadStef(Value stef, int m, int n);
Value makeReal(double);
Value makeString(const char*);
const static Value nil = 0;
