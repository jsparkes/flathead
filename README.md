## SharpHead

This is a port of @ericlippert's flathead Z-machine (zork) interpreter
from ocaml to F#.  The original can be found at
https://github.com/ericlippert/flathead/tree/working and documented in
his blog starting with http://ericlippert.com/2016/02/01/west-of-house/.
See them all at http://ericlippert.com/category/zmachine

Since F# was originally based on OCaml, I figured it wouldn't be much
work to translate.  Most of the differences are based on the newer
syntax of F#.  The F# formatter does most of the transformation for
me.  I prefer to always run my code through the formatter for
consistency.

Some of the key differences are:

1. F# has a byte variable type.  Need to be correct when mixing bytes,
   ints and chars.  This caused a lot of work in Story.ml and
   Types.ml.  For example `fetch_bit`, is the last argument a word or
   byte?  I chose to keep it a word, and cast bytes to int when
   calling.  (Function overloading could help here, or defining a
   different function to take a byte as an argument.)
1. different string representation.
   1. F# uses .NET strings.
   1. F# differentiates more between strings and byte arrays.
   1. OCaml uses ^ for concatenation, whereas F# uses +.  (F# accepts
      ^ but warns about it.)

1. F# is indentation sensitive.  Record declarations and matches are
   fussier.

1. casting operators are different
2. different operators for logical bit operations.
1. extra keywords `base`, `static`, `global`



Jeff Sparkes, jsparkes@gmail.com
