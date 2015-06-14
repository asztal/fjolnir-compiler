# Introduction #

The Fjölnir language is an Icelandic modular imperative language with first-class functions (but no closures), lisp-style lists (but no symbols), and dynamic typing. It was used in _Háskóli Íslands_ (the University of Iceland) during the 1980s and is now rapidly becoming the foremost programming language amongst hipster developers.

# Data types #

There are few data types available, and new ones cannot be defined (except as lists or arrays).
  * **16-bit integers** (_fjöldatölur_ and _heiltölur_), which are treated as either signed or unsigned depending on the function which is used.
  * **floating-point numbers** (_fleytitala_), which in this implementation are double-precision IEEE floating point numbers.
  * **arrays** (_hlunkur_), which seem to be one-dimensional only.
  * **pairs** (_pár_), which are mutable and are used to build lists.
  * **strings** (_strengur_), which are also mutable and I haven't quite figured out all the quirks yet.
  * **functions** (_stef_).
  * **nil**.

# Example #

A simple Fjölnir program, dissected:

```
;; This is a program "fac.EXE" whose entry point is "main" in the following module.
"fac" < main
{
    ;; Modules contain exports of 3 types. Here, we export a function called 'main'.
    ;; This function cannot refer to other exports of this module (see "modules").
    main -> stef (;)
    stofn
        ;; Write a string to stdout.
        ;; Note the semicolon. Functions in fjölnir can take reference parameters
        ;; too, which are modifiable by the function. These go before the semicolon.
        skrifastreng(;"Halló, heimur!\n"),
    stofnlok

    ;; As an example, here is how one would export a variable.
    ;; (An exported variable can be used using the innflutt keyword.)
    x -> breyta

    ;; And this re-exports a name from another module.
    r -> s
}
;; Specifies that the module above will look inside "GRUNNUR" to resolve unresolved
;; function names and imported variables.
*
"GRUNNUR" ;; The base module which most programs should use.
          ;; "GRUNNUR" is composed of submodules with which we can refine our imports.
;
```

# Modules #

Modules can export three types of object:
  * **Functions**, which cannot refer to other functions or variables within the module. Module combinators (see below) must be used to achieve this.
  * **Variables**, which can be imported using the innflutt keyword
  * Or the module can **re-export** an existing object under a different name.

Modules are combined with each other to produce other modules, using 4 infix left-binding binary operators (`+`, `*`, `:`, and `&`) and a prefix unary operator (`!`).

## Module Operators ##

  1. The `+` (_plus_) operator merges two modules, provided that there is no intersection between the modules. If both modules export the same name, an error occurs. This operator is associative and commutative.
  1. The `*` (_compose_) operator uses exports from the right-hand module to satisfy unresolved imports in the left-hand module. The resulting module has the same exports as the left-hand module, but the imports have changed. This operator is neither commutative nor associative.
  1. The `:` (_combine_) operator also uses exports from the right-hand module to satisfy unresolved imports in the left-hand module; however, where possible, exports from the right-hand module are added to the result, where no clashes would occur. This operator is neither commutative nor associative.
  1. The `!` (_recursion_) operator links a module with itself. This can be used to write a mutually recursive pair of functions. It is possible to create a chain of re-exports which forms a cycle; this is an error. This operator is idempotent.
  1. The `&` (_mutual recursion_) operator links two modules with each other, similar to `*` but bidirectional. `x & y` is identical to `!(x + y)`, which means that x and y cannot share any exports, and that each module can refer to exports within itself. It is commutative and associative.

## Import Conflicts ##

It is an error for a module to import the same name with differing import types. For example, the following definition is invalid because it imports **g** as both a variable and a function:

```
m = {
   f -> stef (;) stofn g(;), stofnlok
   f2 -> stef (;) innflutt g stofn g stofnlok
   ;; An aside: if a function imports a variable using 'innflutt'
   ;; but does not use it, the program behaves as though
   ;; it were never imported. This is never specified
   ;; anywhere, it's just how the original compiler works.
};
```

The result of compiling this code is as follows:

```
Compilation error:
  Conflicting imports found in a module:
    g: [stef 0 0] and breyta
    when defining module m (at test.fjo:1:1)
```

## Declarations ##

At the top level of a program there are two types of statement: entry point declarations and module declarations.
```
;; Module declaration.
"FAC" = {
    fac -> stef(;n)
        stofn
            ef n < 2 þá
                1,
            annars
               n * fac(;n-1),
            eflok
        stofnlok
} & "GRUNNUR";

;; Entry-point declaration.
"fac" < main {
    main -> stef(;)
        stofn
            ;; An alternative function application syntax. If the function takes only one
            ;; argument, it can be written like this.
            \skrifa \fac 7,
            \skrifastreng "\n"
        stofnlok
} * "FAC" ;
```

## Semantics ##

A Fjolnir program is composed of declarations which combine modules to form programs. There are several type of modules: module files, module variables, and code modules, which can be combined using the module operators.

Consider this example:

```
m = { b -> a } * { a -> breyta };
"M0" = m;
"M1" = m * "GRUNNUR";
```

There are three declarations. They are performed in the order in which they appear, and it is an error to reference a module which has not yet been defined.

  * **m** is a module variable. The code for this module is not written to a file at any point.
  * **"M0"** is a module file, and is written when its declaration is encountered.
  * **`{ b -> a }`** is a code module with one export and one import.

Modules are allowed to have unresolved imports when they are written to disk. Unresolved imports are only disallowed when defining an entry point, as this is the point at which native code generation is performed.

It is important to note a few semantics of modules. Imagine the following scenario:

```
m = { a -> breyta };
"M0" = m;
"M1" = m;
```

Both `M0` and `M1` are written to disk, and both export a variable called `a`, but they are not the same `a`. If you import `M0` and `M1` in the same program, `M0`'s `a` and `M1`'s `a` will behave as separate variables.

Similarly, if `M0` and `M1` and define functions which write to and read from `a`, they will use separate versions of `a` to do so.

Within a module, however, variable equivalence is preserved:

```
"M0" = !{
   x -> breyta,
   y -> x,
   z -> x
};
"M1" = { x -> r, y -> r, z -> r } * { r -> breyta };
```

`x`, `y` and `z` are all the same variable, and will act as such when imported in a program. (`M0` and `M1` are equivalent.)

# Syntax #

TODO: Syntax overview. For now, see the [Forritunarmálið Fjölnir Notendahandbók](Fjolnir#References.md). I hope you can read Icelandic.]

# Standard library #

TODO: `"GRUNNUR"` overview. For now, see the [Forritunarmálið Fjölnir Notendahandbók](Fjolnir#References.md). I hope you can read Icelandic.

# References #
  1. [Forritunarmálið Fjölnir Notendahandbók](http://www.hi.is/~snorri/087133-03/fjolnir.pdf); ; Snorri Agnarsson, Páll Björnsson, Jón Harðarson; 25 Feb 1996.