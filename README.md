
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hablar

The mission of `hablar` is for you to get non-astonishing results\! That
means that functions return what you expected. R has some intuitive
quirks that beginners and experienced programmers fail to identify. Some
of the first weird features of R that `hablar` solves:

  - Missing values `NA` and irrational values `Inf`, `NaN` is dominant.
    For example, in R `sum(c(1, 2, NA))` is `NA` and not 3. In `hablar`
    the addition of an underscore `sum_(c(1, 2, NA))` returns 3, as is
    often expected.

  - Factors (categorical variables) that are converted to numeric
    returns the number of the category rather than the value. In
    `hablar` the `convert()` function always changes the type of the
    values.

  - Finding duplicates, and rows with `NA` can be cumbersome. The
    functions `find_duplicates()` and `find_na()` make it easy to find
    where the data frame needs to be fixed. When the issues are found
    the utility replacement functions, e.g. `if_else_()`, `if_na()`,
    `zero_if()` easily fixes many of the most common problems you face.

`hablar` follows the syntax API of `tidyverse` and works seamlessly with
`dplyr` and `tidyselect`.

## Installation

You can install `hablar` from CRAN:

``` r
install.packages("hablar")
```

Or preferably:

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, hablar)
```

## convert

The most useful function of `hablar` is maybe convert. convert helps the
user to quickly and dynamically change data type of columns in a data
frame. convert always converts factors to character before further
conversion. Works with `tidyselect`.

``` r
mtcars %>% 
  convert(int(cyl, am),
          fct(disp:drat),
          chr(contains("w")))
```

    #> # A tibble: 32 x 11
    #>     mpg   cyl disp  hp    drat  wt     qsec    vs    am  gear  carb
    #>   <dbl> <int> <fct> <fct> <fct> <chr> <dbl> <dbl> <int> <dbl> <dbl>
    #> 1  21       6 160   110   3.9   2.62   16.5     0     1     4     4
    #> 2  21       6 160   110   3.9   2.875  17.0     0     1     4     4
    #> 3  22.8     4 108   93    3.85  2.32   18.6     1     1     4     1
    #> 4  21.4     6 258   110   3.08  3.215  19.4     1     0     3     1
    #> # … with 28 more rows

For more information type `vignette("convert")` in the console.

## Non-Astonishing summary functions

Often summary function like min, max and mean return surprising results.
Combining `_` with your summary function ensures you that you will get a
result, if there is one in your data. It ignores irrational numbers like
`Inf` and `NaN` as well as `NA`. If all elements are `NA, Inf, NaN` it
returns NA.

``` r
starwars %>% 
  summarise(min_height_baseR = min(height),
            min_height_hablar = min_(height))
```

    #> # A tibble: 1 x 2
    #>   min_height_baseR min_height_hablar
    #>              <dbl>             <int>
    #> 1               NA                66

The function `min_` omitted that the variable `height` contained `NA`.
For more information type `vignette("s")` in the console.

## Find the problem

When cleaning data you spend a lot of time understanding your data.
Sometimes you get more row than you expected when doing a `left_join()`.
Or you did not know that certain column contained missing values `NA` or
irrational values like `Inf` or `NaN`.

In `hablar` the `find_*` functions speeds up your search for the
problem. To find duplicated rows you simply `df %>% find_duplicates()`.
You can also find duplicates in in specific columns, which can be useful
before joins.

``` r
# Create df with duplicates
df <- mtcars %>% 
  bind_rows(mtcars %>% slice(1, 5, 9))

# Return rows with duplicates in cyl and am
df %>% 
  find_duplicates(cyl, am)
```

    #> # A tibble: 35 x 11
    #>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
    #>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    #> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
    #> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
    #> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
    #> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
    #> # … with 31 more rows

There are also find functions for other cases. For example `find_na()`
returns rows with missing values.

``` r
starwars %>% 
  find_na(height)
```

    #> # A tibble: 6 x 13
    #>   name  height  mass hair_color skin_color eye_color birth_year gender
    #>   <chr>  <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> 
    #> 1 Arve…     NA    NA brown      fair       brown             NA male  
    #> 2 Finn      NA    NA black      dark       dark              NA male  
    #> 3 Rey       NA    NA brown      light      hazel             NA female
    #> 4 Poe …     NA    NA brown      light      brown             NA male  
    #> # … with 2 more rows, and 5 more variables: homeworld <chr>,
    #> #   species <chr>, films <list>, vehicles <list>, starships <list>

If you rather want a Boolean value instead then e.g.
`check_duplicates()` returns `TRUE` if the data frame contains
duplicates, otherwise it returns `FALSE`.

##### **…apply the solution**

Let’s say that we have found a problem is caused by missing values in
the column `height` and you want to replace all missing values with the
integer 100. `hablar` comes with an additional ways of doing if-or-else.

``` r
starwars %>% 
  find_na(height) %>% 
  mutate(height = if_na(height, 100L))
```

    #> # A tibble: 6 x 13
    #>   name  height  mass hair_color skin_color eye_color birth_year gender
    #>   <chr>  <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> 
    #> 1 Arve…    100    NA brown      fair       brown             NA male  
    #> 2 Finn     100    NA black      dark       dark              NA male  
    #> 3 Rey      100    NA brown      light      hazel             NA female
    #> 4 Poe …    100    NA brown      light      brown             NA male  
    #> # … with 2 more rows, and 5 more variables: homeworld <chr>,
    #> #   species <chr>, films <list>, vehicles <list>, starships <list>

In the chunk above we successfully replaced all missing heights with the
integer 100. `hablar` also contain the self explained:

  - `if_zero()` and `zero_if()`
  - `if_inf()` and `inf_if()`
  - `if_nan()` and `nan_if()`

which works in the same way as the examples above.

## retype

A function for quick and dirty data type conversion. All columns are
evaluated and converted to the simplest possible without loosing any
information.

``` r
mtcars %>% retype()
```

    #> # A tibble: 32 x 11
    #>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
    #>   <dbl> <int> <dbl> <int> <dbl> <dbl> <dbl> <int> <int> <int> <int>
    #> 1  21       6   160   110  3.9   2.62  16.5     0     1     4     4
    #> 2  21       6   160   110  3.9   2.88  17.0     0     1     4     4
    #> 3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1
    #> 4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1
    #> # … with 28 more rows

All variables with only integer were converted to type integer. For more
information type `vignette("retype")` in the console.

### Note

Hablar means ‘speak R’ in Spanish.
