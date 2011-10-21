#include "mjollnir.h"

#include <gc.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Runtime checks

static void checkMalloc(void *x) {
    if(!x) {
        fputs("Out of memory.\n", stderr);
        exit(1);
    }
}

static void checkType(const char *fn, Value x, uintptr_t type) {
    static const char* typeNames[] = {
        "nil", "word", "real", "string", "pair", "stef", "other (unknown)", "garbage"
    };
    
    if(whatThing(x) != type) {
        fprintf(stderr, "%s: expected argument of type %s (%d), got %s (%d)\n",
            fn,
            typeNames[type&7],
            (int)type,
            typeNames[whatThing(x)],
            whatThing(x));
        fprintf(stderr, "The value of x was 0x%08x.\n", x);
        exit(1);
    }
}

static uint16_t checkWord(const char *fn, Value x) {
    checkType(fn, x, T_WORD);
    return getWord(x);
}

static int16_t checkWordS(const char *fn, Value x) {
    checkType(fn, x, T_WORD);
    return getWordS(x);
}

static double getDouble(const char* fn, Value x) {
    checkType(fn, x, T_REAL);
    return *getThing(double, x);
}

static pair* getPair(const char *fn, Value x) {
    checkType(fn, x, T_PAIR);
    return getThing(pair, x);
}

// Primitives

Value makeStef(void* fun, int m, int n) {
    stef *s = (stef*)GC_MALLOC(sizeof(stef));
    checkMalloc(s);
    s->fn = fun;
    s->m = m;
    s->n = n;
    return makeThing(s, T_STEF);
}

void* loadStef(Value thing, int m, int n) {
    if(whatThing(thing) != T_STEF) {
        fputs("loadStef: not a stef!\n", stderr);
        exit(1);
    }
    stef *s = getThing(stef, thing);
    if (s->m != m || s->n != n) {
        fputs("loadStef: incorrect arity\n", stderr);
        exit(1);
    }
    return s->fn;
}

Value makeReal(double d) {
    double *r = GC_MALLOC(sizeof(d));
    checkMalloc(r);
    *r = d;
    return makeThing(r, T_REAL);
}

Value makeString(const char *str) {
    uintptr_t len = strlen(str);
    string *s = GC_MALLOC(sizeof(string) + len + 1);
    checkMalloc(s);
    s->length = len;
    strcpy(s->data, str);
    return makeThing(s, T_STRING);
}

Value makePair(Value x, Value y) {
    pair *p = GC_MALLOC(sizeof(pair));
    checkMalloc(p);
    p->first = x;
    p->second = y;
    return makeThing(p, T_PAIR);
}

// Library functions

Value newLine () {
    putchar('\n');
    return nil;
}

Value skrifa(Value x) {
    switch(whatThing(x)) {
        case T_NIL: puts("[]"); break;
        case T_WORD: printf("%u", (uint)getWord(x)); break;
        case T_REAL: printf("%f", getDouble("skrifa", x)); break;
        default:
            break;
    }
    return nil;
}

Value skrifastreng(Value x) {
    checkType("skrifastreng", x, T_STRING);
    string *str = getThing(string,x);
    printf("%s", str->data);
    return nil;
}

Value plusU(Value a, Value b) {
    uint16_t x = checkWord("+", a);
    uint16_t y = checkWord("+", b);
    return makeWord(x + y);
}

Value plusS(Value a, Value b) {
    int16_t x = checkWordS("++", a);
    int16_t y = checkWordS("++", b);
    return makeWord(x + y);
}

Value plusF(Value a, Value b) {
    double x = getDouble("+++", a);
    double y = getDouble("+++", b);
    return makeReal(x + y);
}

Value minusU(Value a, Value b) {
    uint16_t x = checkWord("-", a);
    uint16_t y = checkWord("-", b);
    return makeWord(x - y);
}

Value minusS(Value a, Value b) {
    int16_t x = checkWordS("--", a);
    int16_t y = checkWordS("--", b);
    return makeWord(x - y);
}

Value minusF(Value a, Value b) {
    double x = getDouble("---", a);
    double y = getDouble("---", b);
    return makeReal(x - y);
}

Value mulU(Value a, Value b) {
    uint16_t x = checkWord("*", a);
    uint16_t y = checkWord("*", b);
    return makeWord(x * y);
}

Value mulS(Value a, Value b) {
    int16_t x = checkWordS("**", a);
    int16_t y = checkWordS("**", b);
    return makeWord(x * y);
}

Value mulF(Value a, Value b) {
    double x = getDouble("***", a);
    double y = getDouble("***", b);
    return makeReal(x * y);
}

Value divU(Value a, Value b) {
    uint16_t x = checkWord("/", a);
    uint16_t y = checkWord("/", b);
    return makeWord(x / y);
}

Value divS(Value a, Value b) {
    int16_t x = checkWordS("//", a);
    int16_t y = checkWordS("//", b);
    return makeWord(x / y);
}

Value divF(Value a, Value b) {
    double x = getDouble("///", a);
    double y = getDouble("///", b);
    return makeReal(x / y);
}

Value ltU(Value a, Value b) {
    uint16_t x = checkWord("<", a);
    uint16_t y = checkWord("<", b);
    if (x < y)
        return makeWord(1);
    return nil;
}

Value ltS(Value a, Value b) {
    int16_t x = checkWordS("<<", a);
    int16_t y = checkWordS("<<", b);
    if (x < y)
        return makeWord(1);
    return nil;
}

Value ltF(Value a, Value b) {
    double x = getDouble("<<<", a);
    double y = getDouble("<<<", b);
    return x < y ? makeWord(1) : nil;
}

Value lteU(Value a, Value b) {
    uint16_t x = checkWord("<=", a);
    uint16_t y = checkWord("<=", b);
    if (x <= y)
        return makeWord(1);
    return nil;
}

Value lteS(Value a, Value b) {
    int16_t x = checkWordS("<=<=", a);
    int16_t y = checkWordS("<=<=", b);
    if (x <= y)
        return makeWord(1);
    return nil;
}

Value lteF(Value a, Value b) {
    double x = getDouble("<=<=<=", a);
    double y = getDouble("<=<=<=", b);
    return x <= y ? makeWord(1) : nil;
}

Value gtU(Value a, Value b) {
    uint16_t x = checkWord(">", a);
    uint16_t y = checkWord(">", b);
    if (x > y)
        return makeWord(1);
    return nil;
}

Value gtS(Value a, Value b) {
    int16_t x = checkWordS(">>", a);
    int16_t y = checkWordS(">>", b);
    if (x > y)
        return makeWord(1);
    return nil;
}

Value gtF(Value a, Value b) {
    double x = getDouble(">>>", a);
    double y = getDouble(">>>", b);
    return x > y ? makeWord(1) : nil;
}

Value gteU(Value a, Value b) {
    uint16_t x = checkWord(">=", a);
    uint16_t y = checkWord(">=", b);
    if (x >= y)
        return makeWord(1);
    return nil;
}

Value gteS(Value a, Value b) {
    int16_t x = checkWordS(">=>=", a);
    int16_t y = checkWordS(">=>=", b);
    if (x >= y)
        return makeWord(1);
    return nil;
}

Value gteF(Value a, Value b) {
    double x = getDouble(">=>=>=", a);
    double y = getDouble(">=>=>=", b);
    return x >= y ? makeWord(1) : nil;
}

Value eqU(Value a, Value b) {
    uint16_t x = checkWord("=", a);
    uint16_t y = checkWord("=", b);
    if (x == y)
        return makeWord(1);
    return nil;
}

Value eqS(Value a, Value b) {
    int16_t x = checkWordS("==", a);
    int16_t y = checkWordS("==", b);
    if (x == y)
        return makeWord(1);
    return nil;
}

Value eqF(Value a, Value b) {
    double x = getDouble("===", a);
    double y = getDouble("===", b);
    return x == y ? makeWord(1) : nil;
}

Value neqU(Value a, Value b) {
    uint16_t x = checkWord("<>", a);
    uint16_t y = checkWord("<>", b);
    if (x != y)
        return makeWord(1);
    return nil;
}

Value neqS(Value a, Value b) {
    int16_t x = checkWordS("<><>", a);
    int16_t y = checkWordS("<><>", b);
    if (x != y)
        return makeWord(1);
    return nil;
}

Value neqF(Value a, Value b) {
    double x = getDouble("<><><>", a);
    double y = getDouble("<><><>", b);
    return x != y ? makeWord(1) : nil;
}


Value remU(Value a, Value b) {
    uint16_t x = checkWord("%", a);
    uint16_t y = checkWord("%", b);
    return makeWord(x % y);
}

Value remS(Value a, Value b) {
    int16_t x = checkWordS("%%", a);
    int16_t y = checkWordS("%%", b);
    return makeWord(x % y);
}

Value not(Value x) {
    if (x == nil)
        return makeWord(1);
    return nil;
}

Value cons(Value x, Value y) {
    return makePair(x, y);
}

Value haus(Value p) {
    return getPair("haus", p)->first;
}

Value hali(Value p) {
    return getPair("hali", p)->second;
}
