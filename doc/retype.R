## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = F,
  comment = "#>"
)
library(DiagrammeR)
library(hablar)
library(dplyr)

## ------------------------------------------------------------------------
x <- as.numeric(3)
retype(x)

## ------------------------------------------------------------------------
class(retype(x))

## ------------------------------------------------------------------------
x <- as.character("2017-03-02")
retype(x)

## ------------------------------------------------------------------------
class(retype(x))

## ------------------------------------------------------------------------
x <- as.character(c("3,56", "0,78"))
retype(x)

## ------------------------------------------------------------------------
class(retype(x))

## ------------------------------------------------------------------------
x <- as.factor(c(3, 4))
retype(x)

## ------------------------------------------------------------------------
class(retype(x))

## ---- echo=F-------------------------------------------------------------
grViz("digraph d {

      node [shape = circle, style = filled]
      factor;character;numeric;integer;'date time';date;logical;list;complex

      logical -> integer;
      factor -> character;
      character -> numeric
      numeric -> integer;
      character -> 'date time';
      'date time' -> date;
      }")

## ------------------------------------------------------------------------
df <- starwars %>% 
  select(1:4) %>% 
  convert(fct(name),
           chr(height:mass),
           fct(hair_color)) %>% 
  print()
  

## ------------------------------------------------------------------------
df %>% 
  retype()

