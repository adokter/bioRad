# Check if character date is in specific format

Check if character date is in specific format

## Usage

``` r
check_date_format(date, format)
```

## Arguments

- date:

  character. Character representation of a date, e.g. `"2018-12-13"`.

- format:

  character. strptime format the date should have, e.g. `"\%Y-\%m-\%d"`

## Value

NULL. Will stop and show error message if date does not have correct
date format.
