# Identify minimum required package version of dependencies.

Identify the minimum required package version of dependencies listed in
Suggests or Imports of the DESCRIPTION file.

## Usage

``` r
min_package_version(pkg)
```

## Arguments

- pkg:

  A character string with a package name.

## Value

A character string with the numeric version. When no version is
specified or the package is not listed in Suggests or Depends a value
`NULL` is returned.
