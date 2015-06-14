A Fjölnir-to-C compiler, written in Haskell.

To build this you will need GHC 7 with the `utf8-string`, `syb`, `filemanip`, and `binary` packages (and possibly more). To build just execute `ghc Mjöllnir.hs`, to use the compiler `./Mjöllnir /path/to/file.fjo [/path/to/other.fjo ..]`. There are no command-line options.

The compiler compiles to ugly and unoptimised C code for which the runtime code is not yet complete.

The compiler supports one language feature not present in the original language -- it supports a `cstef` keyword for linking to functions written in C. Note that the C functions have to be written with Fjölnir in mind. This feature is used to implement the standard library.

```
"KJARNI" = { 
    + -> cstef plusU mjollnir (0;2)
};

;; ...

"GRUNNUR" = "FELAGRUN" : ("KJARNI" + "STRENGUR" + ...);
```
