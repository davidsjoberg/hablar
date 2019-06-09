#' @import dplyr
#' @importFrom stats median sd var na.omit
#' @importFrom utils install.packages installed.packages
#'
#' @export
#' 
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
requireNamespace("rstudioapi", quietly = TRUE)

# could_this_be_that -----------------------------------------------------------

#' @title Tests is a vector could be of another data type
#' @name could_this_be_that
#' @aliases  could_chr_be_num
#' @aliases  could_chr_be_int
#' @aliases  could_num_be_int
#' @aliases  could_chr_be_dtm
#' @aliases  could_dtm_be_dte
#'
#' @description
#' Tests if vector could be a another data type without errors.
#'
#' @param .x vector of the data type that should be tested.
#'
#' @details The name logic of \code{could_chr_be_num} should be interpreted as:
#' Could this character vector be a numeric vector?
#' The same logic goes for all functions named could_this_be_that.
#'
#' @return TRUE or FALSE
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' x <- c("1", "3", "7")
#' could_chr_be_num(x)
#' could_chr_be_int(x)
#'
#' x <- c("abc", "3", "Hello world")
#' could_chr_be_num(x)
#'
#' x <- c(NA, "3.45", "5,98")
#' could_chr_be_num(x)
#' could_chr_be_int(x)
#'
#' x <- as.numeric(c(3.45, 1.5))
#' could_num_be_int(x)
#'
#' x <- as.numeric(c(7, 2))
#' could_num_be_int(x)
#'
#' @rdname could_this_be_that
#' @export
could_chr_be_num <- function(.x) {
  if(!is.character(.x)) {
    stop("Only works with character vectors")}
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  .test <- tryCatch(as.numeric(.x),
                    error=function(e) e,
                    warning=function(w) w)
  if(any(attributes(.test)$class == "warning")) {
    return(FALSE)
  } else {TRUE}
}

#' @rdname could_this_be_that
#' @export
could_chr_be_int <- function(.x) {
  if(could_chr_be_num(.x) != TRUE) {
    return(FALSE)
  }
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  .x <- as.numeric(.x)
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  ifelse(all(.x[!is.na(.x)] == as.integer(.x[!is.na(.x)])), TRUE, FALSE)
}

#' @rdname could_this_be_that
#' @export
could_num_be_int <- function(.x) {
  if(!is.numeric(.x)) {
    stop("Only works with numeric vectors")}
  if(all(is.na(.x)) | length(.x) == 0 | any(is.nan(.x) | any(is.infinite(.x)))) {
    return(FALSE)}
  ifelse(all(.x[!is.na(.x)] == as.integer(.x[!is.na(.x)])), TRUE, FALSE)
}

#' @rdname could_this_be_that
#' @export
could_chr_be_dtm <- function(.x) {
  if(!is.character(.x)) {
    stop("Only works with character vectors")}
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  res <- try(as.POSIXct(.x),silent = TRUE)
  ifelse(all(class(res) != "try-error") == TRUE, TRUE, FALSE)
}

#' @rdname could_this_be_that
#' @export
could_dtm_be_dte <- function(.x) {
  if(!any(class(.x) %in% c("POSIXct", "POSIXt"))) {
    stop("Only works with date-time vectors (POSIXct)")}
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  .timestamps <- strftime(.x, format="%H:%M:%S")
  ifelse(length(unique(.timestamps[!is.na(.timestamps)])) == 1, TRUE, FALSE)
}


# S3 rationalize ---------------------------------------------------------------


#' @title Only allow rational values in numeric vectors
#'
#' \code{rationalize} transforms all numeric elements to be rational values or NA,
#' thus removes all \code{NaN,Inf} and replaces them with \code{NA}.
#'
#' @param .x vector or data.frame
#' @param ... columns to be evaluated. Only applicable if .x is a data frame.
#'
#' @return For vectors: same data type/class as .x.
#' @return For data.frame: a tbl data frame.
#' 
#' @details #' If a non-numeric vector is passed, it is unchanged. If a data.frame is
#' passed, it evaluates all columns separately.
#'
#' @seealso \code{\link{s}}, \code{\link{rationalize}}, \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' x <- c(3, -Inf, 6.56, 9.3, NaN, 5, -Inf)
#' rationalize(x)
#'
#' df <- data.frame(num_col = c(Inf, 3, NaN), 
#'                  chr_col = c("a", "b", "c"), 
#'                  stringsAsFactors = FALSE)
#' df
#' rationalize(df)
#'
#' @rdname rationalize
#' @export

rationalize <- function(.x, ...) {
  UseMethod("rationalize")
}


#' @return \code{NULL}
#'
#' @rdname rationalize
#' @method rationalize default
#' @export

rationalize.default <- function(.x, ...) {
  .x
}


#' @return \code{NULL}
#'
#' @rdname rationalize
#' @method rationalize numeric
#' @export

rationalize.numeric <- function(.x, ...) {
  .x[is.infinite(.x)] <- NA
  .x[is.nan(.x)] <- NA
  return(.x)
}


#' @return \code{NULL}
#'
#' @rdname rationalize
#' @method rationalize data.frame
#' @export

rationalize.data.frame <- function(.x, ...) {
  if(length(quos(...)) == 0){
    .vars <- numeric(0)
  } else {
    .vars <- quos(...)
  }

  if(length(.vars) != 0){
    .x <- .x %>%
      mutate_at(vars(!!!.vars),
                ~rationalize(.))
  } else {
    .x <- .x %>%
      mutate_at(vars(everything()),
                ~rationalize(.))
  }
  return(as_tibble(.x))
}


# S3 retype --------------------------------------------------------------------

#' Return simple data types
#'
#' \code{retype} transforms all elements into simple classes. The simple classes
#' are date, numeric and character. By transforming all elements to these
#' classes no information is lost, while simplifying the object. See details below for
#' more information or type \code{vignette("retype")} in the console.
#'
#' @param .x vector or data.frame
#' @param ... column names to be evaluated. Only if .x is a data frame.
#'
#' @return For vectors: simple class of .x.
#' @return For data.frame: a tbl data frame with simple classes.
#'
#' @seealso \code{\link{s}}, \code{\link{rationalize}} #' \code{vignette("retype")}, \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' # Dates
#' dte <- as.Date(c("2018-01-01", "2016-03-21", "1970-01-05"))
#' retype(dte)
#' retype(dte)
#'
#' # Factors
#' fct <- as.factor(c("good", "bad", "average"))
#' retype(dte)
#'
#' # Character that only contains numeric elements
#' num_chr <- c("3","4.0", "3,5")
#' retype(num_chr)
#'
#' # Logical
#' lgl <- c(TRUE, FALSE, TRUE)
#' retype(lgl)
#'
#' # Data frame with all the above vectors
#' df <- data.frame(dte = dte, 
#'                  fct = fct, 
#'                  num_chr = num_chr, 
#'                  lgl = lgl, 
#'                  stringsAsFactos = FALSE)
#' df
#' retype(df)
#'
#' @details Each vector past to \code{retype} is reclassified into the highest position in
#' a simplification hierarchy without loosing any information. This means that:
#' Factors are converted to characters.
#' However, character vectors (or vectors changed to character initially)
#' are checked to see if they could be a numeric vector without error.
#' If so, it is transformed into a numeric vector which is higher in the hierarchy.
#' Vectors of class logical, integer are changed to numerical.
#' Dates and date time (POSIXct) goes through the same procedure.
#' Lists and complex vectors are left unchanged because the are neither simple nor complicated.
#'
#' @rdname retype
#' @export

# S3 retype
retype<- function(.x, ...) {
  UseMethod("retype")
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype default
#' @export
retype.default <- function(.x, ...) {
  .x <- as.character(.x)

  # Numericals
  if(could_chr_be_num(.x) == TRUE) {
    .x <- as.numeric(gsub(",", ".", .x))
    if(could_num_be_int(.x) == TRUE) {
      return(as.integer(.x))
    } else {
      return(.x)
    }
    }

  # Dates
  if(could_chr_be_dtm(.x) == TRUE) {
    .x <- as.POSIXct(.x)
    if(could_dtm_be_dte(.x) == TRUE) {
      return(as.Date(strftime(.x)))
    } else {
      return(.x)
    }
  }

  return(.x)
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype logical
#' @export
retype.logical <- function(.x, ...) {
  as.integer(.x)
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype integer
#' @export
retype.integer <- function(.x, ...) {
  .x
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype Date
#' @export
retype.Date <- function(.x, ...) {
  .x
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype POSIXct
#' @export
retype.POSIXct <- function(.x, ...) {
  if(could_dtm_be_dte(.x) == TRUE) {
    .x <- as.Date(strftime(.x))
  }
  return(.x)
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype numeric
#' @export
retype.numeric <- function(.x, ...) {
  if(could_num_be_int(.x) == TRUE) {
    .x <- as.integer(.x)
  }
  return(.x)
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype list
#' @export
retype.list <- function(.x, ...) {
  .x
}

#' @return \code{NULL}
#'
#' @rdname retype
#' @method retype data.frame
#' @export
retype.data.frame <- function(.x, ...) {
  if(length(quos(...)) == 0){
    .vars <- numeric(0)
  } else {
    .vars <- quos(...)
  }

  if(length(.vars) != 0){
    .x <- .x %>%
      mutate_at(vars(!!!.vars),
                ~retype(.))
  } else {
    .x <- .x %>%
      mutate_all(retype)
  }
  return(as_tibble(.x))
}



# as_reliable_[data type] ------------------------------------------------------

#' @title Reliable conversion to another data type
#' @name as_reliable
#' @aliases as_reliable_num
#' @aliases as_reliable_int
#' @aliases as_reliable_lgl
#'
#' @description
#' Support functions for the \code{convert} function. These functions coerces vectors to a new data type, e.g. \code{as.numeric}
#' except that it converts factors to character first.
#' See \code{\link{convert}} for more information.
#'
#' @usage as_reliable_num(.x, ...)
#'
#' as_reliable_int(.x, ...)
#'
#' as_reliable_lgl(.x, ...)
#'
#' as_reliable_dte(.x, origin = "1970-01-01", ...)
#'
#' as_reliable_dtm(.x, origin = "1970-01-01", tz = "Europe/London", ...)
#'
#' @param .x vector
#' @param origin argument to set origin for date/date time.
#' @param tz argument to set time zone for date/date time. Default is Europe/London.
#' @param ... additional arguments
#'
#' @return vector
#' 
#' @seealso \code{vignette("convert")}, \code{vignette("hablar")}
#'
#' @examples
#' x <- as.factor(c("1", "3.5"))
#' as_reliable_num(x)
#'
#' x <- as.factor(c("9", "7"))
#' as_reliable_int(x)
#'
#' x <- as.factor(c("1", "0"))
#' as_reliable_lgl(x)
#'
#' @rdname as_reliable
#' @export

as_reliable_num <- function(.x, ...) {
  if(is.factor(.x)) {
    return(as.numeric(as.character(.x), ...))}
  if(TRUE) {
    return(as.numeric(.x, ...))}
}

#' @rdname as_reliable
#' @export
as_reliable_int <- function(.x, ...) {
  if(is.factor(.x)) {
    return(as.integer(as.character(.x), ...))}
  if(TRUE) {
    return(as.integer(.x, ...))}
}


#' @rdname as_reliable
#' @export
as_reliable_lgl <- function(.x, ...) {
  if(is.logical(.x)) {
    return(.x)}
  if(any(class(.x) %in% c("POSIXct", "Date"))) {
    stop("Date and Date-time vectors can't be converted to logical.")}
  if(is.factor(.x)) {
    .x <- as.character(.x)}
  if(is.character(.x)){
    if(could_chr_be_int(.x)) {
      return(as.logical(as.integer(.x)))}
  }
  if(TRUE) {
    return(as.logical(.x, ...))}
}



#' @rdname as_reliable
#' @export
as_reliable_dte <- function(.x, origin = "1970-01-01", ...) {
  if(any(class(.x) == "Date")) {
    return(.x)}
  if(is.logical(.x)) {
    stop("Logical vectors can't be converted to date.")}
  if(is.factor(.x)) {
    .x <- as.character(.x)}
  if(any(class(.x) == "POSIXct")) {
    .x <- strftime(.x)}
  if(TRUE) {
    return(as.Date(.x, origin = origin, ...))}
}


#' @rdname as_reliable
#' @export
as_reliable_dtm <- function(.x, origin = "1970-01-01", tz = "Europe/London", ...) {
  if(any(class(.x) == "POSIXct")) {
    return(.x)}
  if(is.logical(.x)) {
    stop("Logical vectors can't be converted to date time.")}
  if(is.factor(.x)) {
    .x <- as.character(.x)}
  if(TRUE) {
    return(as.POSIXct(.x, origin = origin, tz = tz, ...))}
}



# convert ----------------------------------------------------------------------

#' @title Convert data type of columns
#' @name convert
#' @aliases convert
#' @aliases num
#' @aliases dbl
#' @aliases int
#' @aliases dte
#' @aliases dtm
#' @aliases fct
#' @aliases lgl
#'
#' \code{convert} converts columns to new classes through scoping functions. Always converts factors to
#' character before conversion Type \code{vignette("convert")} in the console for more information.
#'
#' @param .x A data.frame
#' @param ... Scoping functions, see details
#' @param .args extra argument to be passed to support function.
#'
#' @return a tbl data frame
#'
#' @details
#' The convert function requires a scoped function inside. Column names should not be quoted:.
#' \describe{
#'   \item{num}{Changes to numeric columns. \cr\strong{Usage: }
#'   \code{.df %>% convert(num(.x, .y))} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{chr}{Changes to character columns. \cr\strong{Usage: }
#'   \code{.df %>% convert(chr(.x, .y))} where
#'   \code{.x} and \code{.y} are column names of \code{.df}}
#'
#'   \item{int}{Changes to integer columns. \cr\strong{Usage: }
#'   \code{.df %>% convert(int(.x, .y))} where
#'   \code{.x} and \code{.y} are column names of \code{.df}}
#'
#'   \item{lgl}{Changes to logical columns. \cr\strong{Usage: }
#'   \code{.df %>% convert(lgl(.x, .y))} where
#'   \code{.x} and \code{.y} are column names of \code{.df}}
#'
#'   \item{dte}{Changes to date columns. Default origin set to "1970-01-01" \cr\strong{Usage: }
#'   \code{.df %>% convert(dte(.x, .y))} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{dtm}{Changes to date-time columns (POSIXct). Default origin set to "1970-01-01". \cr\strong{Usage: }
#'   \code{.df %>% convert(dtm(.x, .y))} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{fct}{Changes to factor columns. \cr\strong{Usage: }
#'   \code{.df %>% convert(fct(.x, .y))} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{dbl}{Changes to numeric columns. \cr\strong{Usage: }
#'   \code{.df %>% convert(dbl(.x, .y))} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#' }
#'
#' It is also possible to use multiple scoping functions inside \code{convert}. For example,
#' \code{.df %>% convert(num(.x), chr(.y))} works as well.
#' 
#' @seealso \code{vignette("convert")}, \code{vignette("hablar")}
#'
#' @examples
#' \dontrun{
#' 
#' # Change one column to numeric and another to character
#' mtcars %>% 
#'   convert(num(gear),
#'           chr(mpg))
#'
#'
#' # Changing multiple data types on multiple columns
#' mtcars %>% 
#'   convert(int(hp,
#'               wt),
#'           fct(qsec,
#'               cyl,
#'               drat))
#' 
#' # Also works with tidyselect convenience functions
#' mtcars %>%
#'   convert(int(vs:carb),
#'           fct(last_col()))
#' 
#' }
#'
#' @rdname convert
#' @export
num <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as_reliable_num(., !!!.args))}

#' @rdname convert
#' @export
chr <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as.character(., !!!.args))}

#' @rdname convert
#' @export
lgl <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as_reliable_lgl(., !!!.args))}

#' @rdname convert
#' @export
int <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as_reliable_int(., !!!.args))}

#' @rdname convert
#' @export
dbl <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as_reliable_num(., !!!.args))}

#' @rdname convert
#' @export
fct <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as.factor(., !!!.args))}

#' @rdname convert
#' @export
dtm <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as_reliable_dtm(., !!!.args))}

#' @rdname convert
#' @export
dte <- function(..., .args = list()){
  list(vars = quos(...),
       fun = ~as_reliable_dte(., !!!.args))}

#' @rdname convert
#' @export
convert <- function(.x, ...){
  if(!is.data.frame(.x)) {
    stop("convert only works with data frames.")
  }
  args <- list(...)
  
  for(i in seq_along(args)) {
    .vars <- args[[i]]$vars
    .fun  <- args[[i]]$fun
    .x <- .x %>% mutate_at(vars(!!!.vars), .fun)
  }
  return(as_tibble(.x))
}


# s ----------------------------------------------------------------------------

#' Make vector shorter and simpler
#'
#' \code{s} means simple and short. It removes all non-values, i.e. \code{NA,Inf,NaN}  from a vector.
#' However, if the length is 0 it returns NA.
#' It is useful in combination with summary functions, e.g. mean, sum or min, when
#' an answer is desired, if there is one in the data. In any other case NA is returned. 
#' Type \code{vignette("s")} in the console for more information.
#'
#' @param ... one or more vectors. Does not work for factors.
#' @param ignore_na if TRUE then NA omitted from results, as long as any non-NA element is left.
#'
#' @return a shortened and simplified vector
#' 
#' @seealso \code{\link{retype}}, \code{\link{rationalize}}, \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' ## s on a weird numeric vector
#' vector <- c(7, NaN, 6, -Inf, 5, 4, NA)
#' s(vector)
#'
#' ## Sum vector with non-rational values
#' vector <- c(7, NaN, -Inf, 4)
#' # Base R
#' sum(vector)
#' # With s
#' sum(s(vector))
#'
#' ## Max of vector with only NA
#' # Base R
#' max(vector, na.rm = TRUE)
#' # With s
#' max(s(vector))
#'
#' ## First of vector when NA is first element
#' vector <- c(NA, "X", "Y")
#' # dplyr R
#' first(vector)
#' # With s
#' first(s(vector))
#'
#' ## Use of s when NA should not be removes
#' vector <- c(7, Inf, NA, 4)
#' # Base R
#' sum(vector)
#' # With s
#' sum(s(vector, ignore_na = FALSE))
#'
#' ## s when summarizing a weird data.frame
#' df_test <- data.frame(a = c(NaN, 1, -Inf, 3), 
#'                       b = c(NA, "Q", "P", "P"), 
#'                       c = c(NA, NA, NA, NA), 
#'                       stringsAsFactors = FALSE) 
#' df_test
#' 
#' # Base R aggregation with dplyr's summarize
#' summarise(df_test, mean_a = mean(a), 
#'                    min_c = min(c, na.rm = TRUE))
#' # With s
#' summarise(df_test, mean_a = mean(s(a)), 
#'                    min_c = min(s(c)))
#' }
#' @rdname s
#' @export

s <- function(..., ignore_na = TRUE) {
  if(is.factor(...)){
    stop("s does not work with factors. Consider converting it into another data type with hablar::convert or hablar::retype.")
  }
  .v <- rationalize(c(...))
  if(all(is.na(.v)) | length(.v) == 0) {
    return(NA)
  }
  if(ignore_na) {return(c(.v[!is.na(.v)]))}
  return(.v)
}



# simplifying summary functions ---------------------------------------------------

#' @title Combine aggregate functions and s
#' @name wrapper - s and summary funs
#' @aliases sum_
#' @aliases mean_
#' @aliases max_
#' @aliases min_
#' @aliases sd_
#' @aliases var_
#' @aliases first_
#' @aliases last_
#'
#' @description
#' \code{[summary function_*]} functions are simple wrappers of aggregate function
#' and the \code{s} function. \code{s} removes all non-values,
#' i.e. \code{NA,Inf,NaN}  from a vector.
#' However, if the length is 0 it returns NA. The result is then passed to the
#' corresponding aggregation function. For example,
#' \code{min_(x)} is identical to \code{min(s(x))}. Please read \code{vignette("s")} for more information.
#'
#' @usage sum_(..., ignore_na = TRUE)
#'
#' mean_(..., ignore_na = TRUE)
#'
#' max_(..., ignore_na = TRUE)
#'
#' min_(..., ignore_na = TRUE)
#' 
#' median_(..., ignore_na = TRUE)
#' 
#' sd_(..., ignore_na = TRUE)
#' 
#' var_(..., ignore_na = TRUE)
#'
#' first_(..., ignore_na = TRUE)
#' 
#' last_(..., ignore_na = TRUE)
#'
#' @param ... one or more vectors
#' @param ignore_na if false missing values are not omitted.
#'
#' @return a single aggregated value
#' 
#' @seealso \code{vignette("convert")}, \code{vignette("hablar")}
#'
#' @examples
#' ## sum_ on non-rational numeric vector
#' vector <- c(7, NaN, -Inf, 4)
#' sum_(vector)
#'
#' ## Min of vector with length 0
#' vector <- c()
#' # With a wrapped s
#' min_(vector)
#'
#' ## Max of vector with only NA
#' # With a wrapped s
#' max_(vector)
#'
#' ## Use of s when NA should not be removed
#' vector <- c(7, Inf, NA, 4)
#' # With a wrapped s
#' sum_(vector, ignore_na = FALSE)
#'
#' @rdname aggregators
#' @export

max_ <- function(..., ignore_na = TRUE) {
  max(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
min_ <- function(..., ignore_na = TRUE) {
  min(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
sum_ <- function(..., ignore_na = TRUE) {
  sum(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
mean_ <- function(..., ignore_na = TRUE) {
  mean(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
median_ <- function(..., ignore_na = TRUE) { 
  stats::median(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
sd_ <- function(..., ignore_na = TRUE) {
  stats::sd(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
var_ <- function(..., ignore_na = TRUE) {
  stats::var(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
first_ <- function(..., ignore_na = TRUE) {
  dplyr::first(s(..., ignore_na = ignore_na))}

#' @rdname aggregators
#' @export
last_ <- function(..., ignore_na = TRUE) {
  dplyr::last(s(..., ignore_na = ignore_na))}


# simplifying math functions ---------------------------------------------------
#' @title Ignore NA in math
#' @name math ignore NA in math funs
#' @aliases %minus_%
#' @aliases %plus_%
#'
#' @description
#' Simplifying math functions are simple wrappers of math function (- +).
#' If any of the left-hand side or right-hand side is NA, Inf or NaN it 
#' returns any rational value, if there is any. 
#' 
#' However, if the both values are irriational it returns NA. 
#' The result is then passed to the
#' corresponding math function.
#'
#' @param .x numeric or integer element
#' @param .y numeric or integer element
#'
#' @return a single value
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' # The simplest case
#' 3 %minus_% 2
#' 
#' # But with NA it returns 3 as if the NA were zero
#' 3 %minus_% NA
#' 
#' # It doesnt matter if the irrational number is on left- or right-hand.
#' NA %plus_% 5
#'
#' @rdname math
#' @export
`%minus_%` <- function(.x, .y) {
  if(is.na(s(.x)) & is.na(s(.y))) {return(NA)}
  .x <- ifelse(is.na(s(.x)), 0, .x)
  .y <- ifelse(is.na(s(.y)), 0, .y)
  .x - .y
}

#' @rdname math
#' @export
`%plus_%` <- function(.x, .y) {
  if(is.na(s(.x)) & is.na(s(.y))) {return(NA)}
  .x <- ifelse(is.na(s(.x)), 0, .x)
  .y <- ifelse(is.na(s(.y)), 0, .y)
  .x + .y
}




# Row summary function ---------------------------------------------------------
#' @title row functions inside dplyr mutate
#' @name fun_by_row tidyverse_syntax for funs on rows
#' @aliases row_sum
#' @aliases row_sum_
#' @aliases row_mean
#' @aliases row_mean_
#'
#' @description
#' Simple and tidyversish syntac for sum and mean of a row, or select vars.
#' The suffix *_ version of the function removes irrational numbers and NA by default. 
#' 
#' Must be used inside the dplyr function \code{mutate} in order to work.
#' 
#'
#' @usage row_sum(..., ignore_na = FALSE, rationalize = FALSE)
#' 
#' row_sum_(..., ignore_na = TRUE, rationalize = TRUE)
#' 
#' row_mean(..., ignore_na = FALSE, rationalize = FALSE)
#' 
#' row_sum_(..., ignore_na = TRUE, rationalize = TRUE)
#'
#' @param ... vars or tidyselect function
#' @param ignore_na a logical indicating whether missing values should be removed
#' @param rationalize a logical indicating whether irriational values (NaN, Inf) should be removed
#'
#' @return a single numeric vector of the same length as the data frame it
#' is applied to.
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' 
#' \dontrun{
#' mtcars %>% 
#'   mutate(sum_of_row = row_sum_(cyl:wt))
#'   
#' mtcars %>% 
#'   mutate(mean_of_row = row_mean(disp, drat, gear))
#' } 
#'
#' @rdname fun_by_row
#' @export
row_sum_ <- function(..., ignore_na = TRUE, rationalize = TRUE){
  .vars <- apply_columns_quosure(...)
  .df <- select(get_caller_df(), !!!.vars)
  if("grouped_df" %in% class(.df)) {
    stop("Grouped data frames not yet supported")
  }
  if(rationalize){
    rowSums(rationalize(.df), na.rm = ignore_na)
  } else {
    rowSums(.df, na.rm = ignore_na)
  }
}

#' @rdname fun_by_row
#' @export
row_sum <- function(..., ignore_na = FALSE, rationalize = FALSE){
  .vars <- apply_columns_quosure(...)
  .df <- select(get_caller_df(), !!!.vars)
  if("grouped_df" %in% class(.df)) {
    stop("Grouped data frames not yet supported")
  }
  if(rationalize){
    rowSums(rationalize(.df), na.rm = ignore_na)
  } else {
    rowSums(.df, na.rm = ignore_na)
  }
}

#' @rdname fun_by_row
#' @export
row_mean_ <- function(..., ignore_na = TRUE, rationalize = TRUE){
  .vars <- apply_columns_quosure(...)
  .df <- select(get_caller_df(), !!!.vars)
  if("grouped_df" %in% class(.df)) {
    stop("Grouped data frames not yet supported")
  }
  if(rationalize){
    rowMeans(rationalize(.df), na.rm = ignore_na)
  } else {
    rowMeans(.df, na.rm = ignore_na)
  }
}

#' @rdname fun_by_row
#' @export
row_mean <- function(..., ignore_na = FALSE, rationalize = FALSE){
  .vars <- apply_columns_quosure(...)
  .df <- select(get_caller_df(), !!!.vars)
  if("grouped_df" %in% class(.df)) {
    stop("Grouped data frames not yet supported")
  }
  if(rationalize){
    rowMeans(rationalize(.df), na.rm = ignore_na)
  } else {
    rowMeans(.df, na.rm = ignore_na)
  }
}


# Count unique elements --------------------------------------------------------
#' @title n_unique
#' @name n_unique count unique elements
#' @aliases n_unique
#' @aliases n_unique_
#'
#' @description
#' Simple wrapper for length(unique(.x)). If you use n_unique_(.x) then NA
#' is ignored when counting.
#' 
#' @usage n_unique(.x, ignore_na = FALSE)
#' 
#' n_unique_(.x, ignore_na = TRUE)
#'
#' @param .x a vector
#' @param ignore_na a logical indicating whether missing values should be removed
#'
#' @return a single numeric vector of the same length as the data frame it
#' is applied to.
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' 
#' # Simple
#' n_unique(c(1, 2, 2, 3))
#' 
#' # Same result as above eventhough vector includes NA
#' n_unique_(c(1, 2, 2, 3, NA))
#'
#' @rdname n_unique
#' @export
n_unique <- function(.x, ignore_na = FALSE) {
  if(ignore_na) {
    length(na.omit(unique(.x)))
  } else {
    length(unique(.x))
  }
}

#' @rdname n_unique
#' @export
n_unique_ <- function(.x, ignore_na = TRUE) {
  if(ignore_na) {
    length(na.omit(unique(.x)))
  } else {
    length(unique(.x))
  }
}



# if_else_ ---------------------------------------------------------------------
#' @title if_this_else_that_
#' @name if_else_
#'
#' @description
#' A vectorised if or else function. It checks that the true or false (or the optional missing)
#' arguments have the same type. However it accepts a generic NA. Built upon
#' dplyr's [if_else()] function. The only difference is that the user do not have to specify
#' the type of NA. if_else_ is faster than base [ifelse()] and a tad slower than 
#' dplyr's [if_else()]. Attributes are taken from either true or false because one
#' generic NA.
#' 
#' @usage if_else_(condition, true, false, missing = NULL)

#' @param condition logical vector
#' @param true value to replace if condition is true. Must be same length as condition or 1.
#' @param false value to replace if condition is false. Must be same length as condition or 1.
#' @param missing optional. a replacement if condition returns NA. Must be same length as condition or 1.
#'
#' @return a vector
#' 
#' @details If the returning vector have attributes (e.g. for factors) it returns the attributes
#' for the first non-generic NA in the order true, false and then missing.
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' 
#' v <- c(TRUE, FALSE, TRUE, FALSE)
#' if_else_(v, "true", "false")
#' 
#' v <- c(TRUE, FALSE, NA, FALSE)
#' if_else_(v, 1, NA, 999)
#' 
#' @rdname if_else_
#' @export
if_else_ <- function(condition, true, false, missing = NULL){
  if(!check_single_generic_na(true)){
    templ <- true[1]
  } else if(!check_single_generic_na(false)){
    templ <- false[1]
  } else if(!is.null(missing)){
    if(!check_single_generic_na(missing)){
      templ <- missing[1]
    }
  } else{
    templ <- as.logical(T)
  }
  
  if(check_single_generic_na(true)){
    templ[2] <- NA
    true <- templ[2]
  }
  if(check_single_generic_na(false)){
    templ[2] <- NA
    false <- templ[2]
  }
  if(!is.null(missing)){
    if(check_single_generic_na(missing)){
      templ[2] <- NA
      missing <- templ[2]
    }
  }
  
  if (is.factor(templ)) {
    if (is.null(missing)) {
      if (length(unique(list(levels(true),
                             levels(false)))) != 1) {
        warning("Factor levels differs")
      }
      } else {
        if (length(unique(list(
          levels(true),
          levels(false),
          levels(missing)
        ))) != 1) {
          warning("Factor levels differs")
        }
      }
  }
  if_else(condition, true, false, missing)
}


# replacemnt and specials ------------------------------------------------------------------------
#' @title replacemnt and specials
#' @name replacers
#' @aliases if_na
#' @aliases if_inf
#' @aliases if_nan
#' @aliases if_zero
#' @aliases na_if
#' @aliases inf_if
#' @aliases nan_if
#' @aliases zero_if
#'
#' @description
#' If-this-type-then replace with x. And the other way around; replace with x
#' if this.
#' 
#' @usage if_na(.x, replacement, missing = NULL)
#' @usage if_nan(.x, replacement, missing = NULL)
#' @usage if_inf(.x, replacement, missing = NULL)
#' @usage if_zero(.x, replacement, missing = NULL)
#' @usage na_if(.x, condition, replace_na = FALSE)
#' @usage nan_if(.x, condition, replace_na = FALSE)
#' @usage inf_if(.x, condition, replace_na = FALSE)
#' @usage zero_if(.x, condition, replace_na = FALSE)
#'
#' @param .x a vector
#' @param condition a predicament
#' @param missing a value that replace missing values in condition.
#' @param replacement a replacement if condition is TRUE
#' @param replace_na if TRUE, missing values in condition will be replaced as well
#'
#' @return a vector
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' 
#' v <- c(1, NA, 2)
#' if_na(v, 100)
#' 
#' v <- c(999, NA, 2)
#' zero_if(v, v == 999)
#' 
#' @rdname replacers
#' @export
if_na <- function(.x, replacement, missing = NULL){
  if_else_(is.na(.x), replacement, .x, missing)
}

#' @rdname replacers
#' @export
if_inf <- function(.x, replacement, missing = NULL){
  if_else_(is.infinite(.x), replacement, .x, missing)
}

#' @rdname replacers
#' @export
if_nan <- function(.x, replacement, missing = NULL){
  if_else_(is.nan(.x), replacement, .x, missing)
}

#' @rdname replacers
#' @export
if_zero <- function(.x, replacement, missing = NULL){
  if_else_(.x == 0, replacement, .x, missing)
}

#' @rdname replacers
#' @export
na_if <- function(.x, condition, replace_na = FALSE){
  if(replace_na){
    missing <- NA
  } else {
    missing <- NA
  }
  if_else_(condition, NA, .x, missing)
}

#' @rdname replacers
#' @export
inf_if <- function(.x, condition, replace_na = FALSE){
  if(!class(.x) %in% c("numeric")){
    stop("inf_if only works on numeric vector")
  }
  if(replace_na){
    missing <- Inf
  } else {
    missing <- NA
  }
  if_else_(condition, Inf, .x)
}

#' @rdname replacers
#' @export
nan_if <- function(.x, condition, replace_na = FALSE){
  if(!class(.x) %in% c("numeric")){
    stop("nan_if only works on numeric vector")
  }
  if(replace_na){
    missing <- NaN
  } else {
    missing <- NA
  }
  if_else_(condition, NaN, .x)
}

#' @rdname replacers
#' @export
zero_if <- function(.x, condition, replace_na = FALSE){
  if(!class(.x) %in% c("integer", "numeric")){
    stop("zero_if only works on numeric and integer vector")
  }
  if(replace_na){
    if(is.integer(.x)){
      missing <- 0L
    } else {
      missing <- 0
    }
  } else {
    missing <- NA
  }
  if_else_(condition, 0, .x, missing)
}

# create_dummy -----------------------------------------------------------------
#' @title Create a simple dummy
#' @name create_dummy
#' @aliases dummy
#' @aliases dummy_
#'
#' @description
#' Creates a vector of the integers 1 and 0. If condition is true it returns 1. If false 0.
#' If condition returns NA it returns NA, if not explicitly not stated than NA
#' should be replaced.
#' 
#' @usage dummy(condition, missing = NA)
#' dummy_(condition, missing = 0L)
#'
#' @param condition a predicament
#' @param missing a replacement if condition is NA
#'
#' @return a vector of the integers 1, 0 and NA (if not dummy_ is used).
#' 
#' @seealso \code{vignette("hablar")}
#'
#' @examples
#' v <- c(10, 5, 3, NA, 9)
#' dummy(v > 5)
#' dummy_(v > 5)
#' 
#' @rdname create_dummy
#' @export
dummy <- function(condition, missing = NA){
  if_else_(condition, 1L, 0L, as.integer(missing))
}

#' @rdname create_dummy
#' @export
dummy_ <- function(condition, missing = 0L){
  if_else_(condition, 1L, 0L, as.integer(missing))
}


# find in df ------------------------------------------------------------
#' @title Special filters
#' @name find_in_df
#'
#' @description
#' Filters a data frame for special cases. For example, find_duplicates() returns
#' all rows that are duplicates. If variables are passed to the function
#' then duplicates for those variables are returned.
#'
#' @param .data a data frame
#' @param ... variables that should be considered. If empty, all variables are used.
#'
#' @return a filtered data frame
#' 
#' @details irrational values are Inf and NaN
#' 
#' @seealso \code{vignette("s")}, \code{vignette("hablar")}
#' 
#' @seealso \code{\link{check_df}} to return TRUE or FALSE instead of rows.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = c("A", NA, "B", "C", "C"),
#'                  b = c(NA, 1, 1, 3, 3), 
#'                  c = c(7, 8, 2, 3, 3), 
#'                  stringsAsFactors = FALSE)
#' 
#' # Returns duplicated rows
#' df %>% find_duplicates()
#' 
#' # Returns duplicates in specific variables
#' df %>% find_duplicates(b:c)
#' 
#' # Returns rows where NA in variable b
#' df %>% find_na(b)
#' }
#' 
#' @rdname find_in_df
#' @export
find_duplicates <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("find_duplicates() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    group_by_at(vars(!!!vars)) %>%
    filter(n() > 1) %>%
    ungroup()
}

#' @rdname find_in_df
#' @export
find_na <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("find_na() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.na(.))) 
}

#' @rdname find_in_df
#' @export
find_irrational <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("find_irrational() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.nan(.) | is.infinite(.))) 
}

#' @rdname find_in_df
#' @export
find_nan <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("find_nan() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.nan(.)))
}

#' @rdname find_in_df
#' @export
find_inf <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("find_inf() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.infinite(.))) }


# check df ------------------------------------------------------------
#' @title Special checks
#' @name check_df
#'
#' @description
#' Returns TRUE if data frame have the specified special cases. For example, find_duplicates() returns
#' TRUE if any rows are duplicates. If variables are passed to the function
#' then TRUE or FALSE is returned for those variables.
#'
#' @param .data a data frame
#' @param ... variables that should be considered. If empty, all variables are used.
#'
#' @return TRUE or FALSE
#' 
#' @details irrational values are Inf and NaN
#' 
#' @seealso \code{\link{find_in_df}} to return rows instead of TRUE or FALSE. \code{vignette("s")}, \code{vignette("hablar")}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = c("A", NA, "B", "C", "C"),
#'                  b = c(7, 8, 2, 3, 3),
#'                  c = c(NA, 1, NaN, 3, 2),
#'                  stringsAsFactors = FALSE)
#' 
#' # Returns FALSE because there is no duplicates
#' df %>% check_duplicates()
#' 
#' # Returns TRUE because there is duplicates in column a through b
#' df %>% check_duplicates(a:b)
#' 
#' # Returns FALSE because there is no NA column b
#' df %>% check_na(b)
#' 
#' # Returns TRUE because there is no NaN column c
#' df %>% check_nan(c)
#' }
#' 
#' @rdname check_df
#' @export
check_duplicates <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("check_duplicates() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    group_by_at(vars(!!!vars)) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    has_rows()
}

#' @rdname check_df
#' @export
check_na <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("check_na() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.na(.))) %>%
    has_rows()
}

#' @rdname check_df
#' @export
check_irrational <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("check_irrational() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.nan(.) | is.infinite(.))) %>%
    has_rows()
}

#' @rdname check_df
#' @export
check_nan <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("check_nan() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.nan(.))) %>%
    has_rows()
}

#' @rdname check_df
#' @export
check_inf <- function(.data, ...){
  if(!is.data.frame(.data)) {
    stop("check_inf() only works with data frames.")
  }
  vars <- apply_columns_quosure(...)
  
  .data %>%
    filter_at(vars(!!!vars), any_vars(is.infinite(.))) %>%
    has_rows()
}



# Set wd to script path --------------------------------------------------------
#' @title Set wd to script path
#' @name set_wd_to_script_path
#'
#' @description
#' Sets working directory to the path where the R-script is located. Only works
#' inside [Rstudio] and in a script (i.e. not in the console). Additionally, the R-script needs to
#' be saved in a path to work.
#' 
#' @usage set_wd_to_script_path()
#'
#' @return NULL. In the background the working directory has changed if not any errors occurred.
#' 
#' @rdname set_wd_to_script_path
#' @export
set_wd_to_script_path <- function(){
  if(Sys.getenv("RSTUDIO") == "1") {
    path <- dirname(rstudioapi::getActiveDocumentContext()$path)
    if(path != "") {
      setwd(path)
    } else {
      stop("You have to save the script before you can set wd to script path.")
    }} else {
      stop("set_wd_to_script_path() only works in a script executed in RStudio, i.e. does not work in console.")
    }
}



# Unexported functions / Support Functions -------------------------------------

# Gets the .Data from mutate call if called from inside a function in a mutate call.
# That is df %>% mutate(a = f()) and the function f() contains get_caller_df()
get_caller_df <- function (.vars) {
  frames <- sys.frames()
  frames[[length(frames) - 4]]$.data
}

# A function that sets a class on a generic NA. If not a generic NA, the function returns input.
check_single_generic_na <- function(.x){
  if(length(.x) != 1){
    return(FALSE)
  }
  if(!is.na(.x)){
    return(FALSE)
  }
  if(!is.logical(.x)){
    return(FALSE)
  }
  return(TRUE)
}

# If not variables are choosed all should be chosen
apply_columns_quosure <- function(...){
  .vars <- quos(...)
  if(length(.vars) == 0) .vars <- quos(everything())
  return(.vars)
}

# Checks if df has any rows
has_rows <- function(.data, ...) {
  nrow(.data) > 0
}
