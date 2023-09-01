## bioRad 0.7.2 

Updates to external links and tests. Errors addressed from CRAN package check.

1. Replaced radar reference with https://aloftdata.eu/radars/ JSON link. 

2. Fixes an issue where flight altitude quantiles were incorrectly shifted down by up to one height bin in integrate_profile(). 

3. Tests brought to date with the latest version of the `testthat` package, which requires handling each indvidual warning and deprecates expect_equivalent()
