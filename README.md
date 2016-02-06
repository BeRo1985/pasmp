# PasMP
PasMP - a parallel-processing/multi-processing library for Object Pascal 

License: zlib

### Features

- Fully-strict fork-join model (because it's less errorprone to work with it than with terminally-strict fork-join for my taste)
- Work-first lock-free Chase-Lev work-stealing
- Lock-free job memory allocator 
- Parallel-for pattern
- Parallel intro sort (direct and indirect)
- Parellel merge sort (direct and indirect)
- Compatible with FreePascal >= 2.6.x and Delphi >= 7


