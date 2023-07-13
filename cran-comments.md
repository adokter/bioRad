## bioRad 0.7.1 

Addresses all CRAN instructions from the recent 0.7.1 submission:

1. Updated the DESCRIPTION file with references in the suggested format.
2. Replaced 'T' and 'F' with 'TRUE' and 'FALSE'.
3. Added \value tags to all .Rd files that were missing it and provided thorough descriptions of function outputs.
4. Removed all commented code lines in examples. Replaced \dontrun{} with \donttest{} for examples that take more than 5 seconds to execute and kept \dontrun{} where an external model is required 
5. Adjusted functions to avoid writing in the user's home filespace. All examples, vignettes, and tests now write to tempdir() by default.

 The deprecation of spatial packages `rgdal` and `maptools`, and the evolution of `sp` led to the archiving of bioRad on 07.07.2023.  All issues arising from spatial packages have been fixed.
