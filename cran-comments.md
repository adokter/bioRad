## bioRad 0.8.0 

* Double-checked that all use of suggested packages is made conditional in examples and package code
* rmarkdown is kept as suggested package, because it is used only in compilation of vignette. Moving it to depends makes devtools::check complain that it isn't used.
* Verified conditional use by uninstalling suggested packages and running devtools::check(env_vars = c("_R_CHECK_DEPENDS_ONLY_"="true"))
* Addresses CRAN policy violation of copying files to user home directory in examples
