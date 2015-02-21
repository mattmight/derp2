# derp2: expressive parsing


`derp2` is as an alternate implementation of the `derp` parsing tool used in my
compilers class.

`derp2` compiles complex regular operations, reductions and special compound forms
in context-free grammars down to the simplified Backus-Naur Form suitable for use
in parsing tools like Racket's `parser-tools/yacc` or `parser-tools/cfg-parser`.


## Building

To build, run `make`.


## Installing

To install in `/usr/local/bin`, run `sudo make install`.

To install elsewhere run:

```
$ make install INSTALLDIR=/path/to/dir
```


## Usage

To convert a derp-compatible S-Expression grammar into a yacc-compatible
S-Expression grammar, run `derp2`:

```
$ derp2 foo.grammar.sx  # produces foo.yacc.sx
```

At the moment, you need to concatenate the `.yacc.sx` file into the proper
place within a `cfg-parser` specification.

For an example of how to do this, look at `examples/calc-parser`.


## TODO

 + Allow a `--grammar` flag which takes a `cfg-parser`-like specification, but
   with derp-style rules, and outputs a `cfg-parser` specification.  This would
   allow using `(include filename)` instead of concatenating items together.

 + Create a `derp-parser` macro that uses these transforms internally.  Then
   there would be no need to use the `include` mechanism either.


