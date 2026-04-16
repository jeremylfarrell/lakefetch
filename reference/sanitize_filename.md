# Sanitize a String for Use in Filenames

Remove or replace invalid filename characters.

## Usage

``` r
sanitize_filename(name)
```

## Arguments

- name:

  Character string to sanitize

## Value

A sanitized string safe for use as a filename

## Examples

``` r
sanitize_filename("Lake O'Brien (2024)")
#> [1] "Lake_OBrien_2024"
```
