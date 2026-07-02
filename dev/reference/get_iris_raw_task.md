# Check the `task` type of an IRIS RAW file

Checks what type of `task`(s), i.e. polar volume types, are contained in
an IRIS RAW file.

## Usage

``` r
get_iris_raw_task(
  file,
  header_size = 50,
  task = c("WIND", "SURVEILLANCE", "VOL_A", "VOL_B")
)
```

## Arguments

- file:

  Character. Path to a polar volume file in IRIS RAW format.

- header_size:

  Integer. Number of header bytes to search.

- task:

  Character (vector). Task names to search for in the file header.

## Value

Specified `task` names found in the header or `NA` if none of the task
names were found.

## See also

Other pvol metadata functions:
[`get_odim_object_type()`](http://adriaandokter.com/bioRad/dev/reference/get_odim_object_type.md)
