# Read in sequence data

Read in sequence data

## Usage

``` r
readSequences(filepath)
```

## Arguments

- filepath:

  the filepath to the specific .txt file you are trying to read in

## Value

a dataframe with two columns, column one is for the sequence name,
column two is for the sequence data itself, each row is for each
sequence entered

## Examples

``` r
if (FALSE) { # \dontrun{
sequencesDF <- readSequences("path/to/sequences.txt")
} # }
```
