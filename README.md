# PasMP
PasMP - a parallel-processing/multi-processing library for Object Pascal 

License: zlib

### Features

- Low-level-based design with optional high-level-based constructs
- Fully-strict fork-join model (because it's less error-prone to work with it than with terminally-strict fork-join for my taste)
- Work-first lock-free Chase-Lev work-stealing
- Lock-free job memory allocator (al least lock-free on x86-32 and x86-64 targets)
- Parallel-for pattern
- Parallel intro sort (direct and indirect)
- Parellel merge sort (direct and indirect)
- Optional singleton usage option (besides the option of usage of multiple PasMP instances)
- Compatible with FreePascal >= 2.6.x and Delphi >= 7
 


 



