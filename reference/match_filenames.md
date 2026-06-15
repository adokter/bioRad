# Match a set of regular expressions to a list of files

Match a set of regular expressions to a list of files and return those
filenames that comply to any of the provided regular expressions. This
function basically wraps a grep to make it work on vectors by combining
the vector of regex options as possible options.

## Usage

``` r
match_filenames(file_list, regex_list)
```

## Arguments

- file_list:

  character vector. Haystack of filenames/filepaths.

- regex_list:

  character vector. Needle of regular expressions to which filenames
  should comply.

## Value

character vector. Subset of filenames from the file_list that comply to
the provided regular expressions in regex_list.
