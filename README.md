
<!-- README.md is generated from README.Rmd. Please edit that file -->
hablar
======

`hablar` give users an easy and effective way to work with data types. Additionally, it provides non-astonishing results when summarizing data.

The ambition is to lower the barrier to R but also provides simple tools that experienced R users could benefit from.

Installation
------------

You can install `hablar` from github with:

``` r
# install.packages("devtools")
devtools::install_github("davidsjoberg/hablar")
```

convert
-------

The most useful function of `hablar` is maybe convert. convert helps the user to quickly change data type of columns in a data frame. convert always converts factors to character before further conversion.

``` r
## convert column:
# gear, vs to integers (int)
# cyl to factor (fct)
# am, gear and carb to character (chr)
mtcars %>% 
  convert(int(gear, vs),
          fct(cyl),
          chr(am, gear, carb))
```

    #> # A tibble: 32 x 11
    #>     mpg cyl    disp    hp  drat    wt  qsec    vs am    gear  carb 
    #>   <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <int> <chr> <chr> <chr>
    #> 1  21   6       160   110  3.9   2.62  16.5     0 1     4     4    
    #> 2  21   6       160   110  3.9   2.88  17.0     0 1     4     4    
    #> 3  22.8 4       108    93  3.85  2.32  18.6     1 1     4     1    
    #> 4  21.4 6       258   110  3.08  3.22  19.4     1 0     3     1    
    #> # ... with 28 more rows

For more information type `vignette("convert")` in the console.

retype
------

A function for quick and dirty data type conversion. All columns are evaluated and converted to the simplest possible without loosing any information.

``` r
## convert all columns to character
df <- mtcars %>% convert(chr(everything()))
df
```

    #> # A tibble: 32 x 11
    #>   mpg   cyl   disp  hp    drat  wt    qsec  vs    am    gear  carb 
    #>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
    #> 1 21    6     160   110   3.9   2.62  16.46 0     1     4     4    
    #> 2 21    6     160   110   3.9   2.875 17.02 0     1     4     4    
    #> 3 22.8  4     108   93    3.85  2.32  18.61 1     1     4     1    
    #> 4 21.4  6     258   110   3.08  3.215 19.44 1     0     3     1    
    #> # ... with 28 more rows

``` r
## let retype guess the best data type
df %>% retype()
```

    #> # A tibble: 32 x 11
    #>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
    #>   <dbl> <int> <dbl> <int> <dbl> <dbl> <dbl> <int> <int> <int> <int>
    #> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
    #> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
    #> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
    #> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
    #> # ... with 28 more rows

For more information type `vignette("retype")` in the console.

s
-

Often summary function like min, max and mean return suprising results. Combining `s` with your summary function ensures you that you will get a result, if there is one in your data.

``` r
## Base R
x <- c(NaN, 1, 2, NA)
min(x)
```

    #> [1] NA

``` r
## With s
min(s(x))
```

    #> [1] 1

For more information type `vignette("s")` in the console.

### Note

Hablar means 'speak R' in spanish.
