## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = F,
  comment = "#>"
)
options(tibble.print_min = 4L, tibble.print_max = 4L)

library(gapminder)
library(hablar)
library(dplyr)

## ------------------------------------------------------------------------
library(gapminder)
gapminder


## ------------------------------------------------------------------------
gapminder %>% 
  convert(chr(country))

## ------------------------------------------------------------------------
gapminder %>% 
  convert(chr(country, 
              continent),
          int(lifeExp),
          dbl(pop),
          num(gdpPercap))

## ------------------------------------------------------------------------
gapminder %>%
  mutate(country = as.character(country),
         continent = as.character(continent),
         lifeExp = as.integer(lifeExp),
         pop = as.double(pop),
         gdpPercap = as.numeric(gdpPercap))

## ------------------------------------------------------------------------
gapminder %>% 
  mutate_at(vars(country, continent), funs(as.character)) %>% 
  mutate_at(vars(lifeExp), funs(as.integer)) %>% 
  mutate_at(vars(pop), funs(as.double)) %>% 
  mutate_at(vars(gdpPercap), funs(as.numeric))

## ------------------------------------------------------------------------
gapminder %>% 
  convert(chr(country, 
               continent),
           int(lifeExp),
           dbl(pop),
           num(gdpPercap))

## ------------------------------------------------------------------------
tibble(dates = c(12818, 13891),
        sunny = c("yes", "no")) %>% 
  convert(dte(dates, .args = list(origin = "1900-01-01")))

## ------------------------------------------------------------------------
gapminder %>% 
  convert(fct(contains("e")))

