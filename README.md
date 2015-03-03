# derp2: expressive parsing


`derp2` is as an alternate implementation of the `derp` parsing tool used in my
compilers class.

`derp2` compiles complex regular operations, reductions and special compound forms
in context-free grammars down to the simplified Backus-Naur Form suitable for use
in parsing tools like Racket's `parser-tools/yacc` or `parser-tools/cfg-parser`.

## Documentation

The pattern forms available in the tool are documented in [a blog post on
desugaring regular operations in context-free grammars](http://matt.might.net/articles/regular-context-free-grammars/).


## Building

To build and install as a racket collection, run `raco pkg install`.

## Usage

```
(require derp2)
```

Use like [cfg-parser](http://docs.racket-lang.org/parser-tools/Context-Free_Parsers.html), but place derp-style rules in the `(grammar)` section.
