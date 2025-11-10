# Value Matching

Value Matching

## Usage

``` r
x %nin% table
```

## Arguments

- x:

  value to search

- table:

  table of values

## Value

the opposite of x %in% table

## Examples

``` r
1:10 %in% c(1, 3, 5, 9)
#>  [1]  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
1:10 %nin% c(1, 3, 5, 9)
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
```
