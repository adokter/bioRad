# bioRad 0.3.0

Release consistent with and in preparation of the bioRad methods paper (https://doi.org/10.1111/ecog.04028). All issues included in this release can be found [here](https://github.com/adokter/bioRad/milestone/1).

* Functions (#84), arguments (#112) and objects (#80) have been renamed to be consistent (#51). Deprecated functions will remain functional for now, but we will trigger a warning: **we advise to use the new functions names**. See the lists for [current functions](../reference/) and [deprecated functions](../reference/bioRad-deprecated.html).

* `integrate_profile()` replaces the functionality of `cmt()` (#75) and `mt()` (#76).

* `plot()` can now we used for scans (#71), e.g. `plot(example_scan)`.

* Functions are [organized in sections](../reference/) on the website (#110).

* Changelog section (this page) has been added to website (#144).

* Package R code is reorganized as one function = one file for easier maintenance (#50).

* First tests are included for some functions.

* Contributors (#90) and citation (#141) have been updated.

* bioRad now has a hex logo (#137). ✨
