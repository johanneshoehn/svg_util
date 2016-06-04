svg-util, a small SVG helper library
====================================

This crate provides some helper data structures and functions for working with SVG files.
Currently it only parses and prints SVG path strings.

License
-------
The code is licensed under [Creative Commons Zero v1.0 Universal](http://creativecommons.org/publicdomain/zero/1.0/legalcode), which essentially means that it's in public domain.

Version history
---------------

Planned features
----------------

 - Measure the maximum precision (digits after the decimal in a number) when parsing paths.
 - `Display` and `Debug` implementations for `&[PathSeg]` or types you can iterate over generally if possible.
 - Complete the conversions between `PathSeg` and `Primitive`.
 - Helper functions to parse paths directly into `Primitive`s.
 - Support to parse `<polygon>`s and `<polyline>`s into `Primitive`s.
 - Support to convert `<rect>`s, `<circle>`s, `<ellipse>`s, and `<line>`s into `Primitive`s.
 - Support for parsing and applying `transform`s.
 - Extensive testing.
 - Use `[feature]`s to make compilation time and binary size better for users. Optimized output, precision could be made turned into features that can be turned off.
 - Benchmarking and optimisation.
 - Maybe a C API.