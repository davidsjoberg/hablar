## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(hablar)
library(dplyr)
library(knitr)

options(tibble.print_min = 4L, tibble.print_max = 4L)

## ---- echo=FALSE--------------------------------------------------------------
df <- tibble(name = c("Fredrik", "Maria", "Astrid"),
           graduation_date = as.Date(c("2016-06-15",
                                       NA,
                                       "2014-06-15")),
           age = c(21L, 16L, 23L))

df

## -----------------------------------------------------------------------------
df %>% 
  mutate(min_baseR = min(graduation_date),
         min_hablar = min_(graduation_date))

## -----------------------------------------------------------------------------
mtcars %>% 
  convert(int(cyl, am),
          num(disp:drat))

## -----------------------------------------------------------------------------
mtcars %>% 
  convert(
    chr(last_col()),       # Last colum to character
    int(1:2),              # First two columns to integer
    fct(hp, wt),           # hp and wt to factors
    dte(vs),               # vs to date (if you really want)
    num(contains("car"))   # car as in carb to numeric
  )           

## -----------------------------------------------------------------------------
# Create df with duplicates
df <- mtcars %>% 
  bind_rows(mtcars %>% slice(1, 5, 9))

# Return rows with duplicates in cyl and am
df %>% 
  find_duplicates(cyl, am)

## -----------------------------------------------------------------------------
starwars %>% 
  find_na(height)

## -----------------------------------------------------------------------------
starwars %>% 
  find_na(height) %>% 
  mutate(height = if_na(height, 100L))

## -----------------------------------------------------------------------------
starwars %>% 
  mutate(skin_color = if_else_(hair_color == "brown", NA, hair_color))

