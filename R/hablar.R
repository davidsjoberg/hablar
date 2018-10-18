#' @import dplyr
#'
#' @export

###############################################
# could_this_be_that
###############################################

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
  .x <- gsub(",", ".", .x)
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
  .x <- gsub(",", ".", .x)
  .x <- as.numeric(.x)
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  ifelse(all(.x[!is.na(.x)] == as.integer(.x[!is.na(.x)])), T, F)
}

#' @rdname could_this_be_that
#' @export
could_num_be_int <- function(.x) {
  if(!is.numeric(.x)) {
    stop("Only works with numeric vectors")}
  if(all(is.na(.x)) | length(.x) == 0 | any(is.nan(.x) | any(is.infinite(.x)))) {
    return(FALSE)}
  ifelse(all(.x[!is.na(.x)] == as.integer(.x[!is.na(.x)])), T, F)
}

#' @rdname could_this_be_that
#' @export
could_chr_be_dtm <- function(.x) {
  if(!is.character(.x)) {
    stop("Only works with character vectors")}
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  res <- try(as.POSIXct(.x),silent = TRUE)
  ifelse(all(class(res) != "try-error") == T, T, F)
}

#' @rdname could_this_be_that
#' @export
could_dtm_be_dte <- function(.x) {
  if(!any(class(.x) %in% c("POSIXct", "POSIXt"))) {
    stop("Only works with date-time vectors (POSIXct)")}
  if(all(is.na(.x)) | length(.x) == 0) {
    return(FALSE)}
  .timestamps <- strftime(.x, format="%H:%M:%S")
  ifelse(length(unique(.timestamps[!is.na(.timestamps)])) == 1, T, F)
}


###############################################
# S3 rationalize
###############################################

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
#' @seealso \code{\link{s}}, \code{\link{rationalize}}
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
                funs(rationalize))
  } else {
    .x <- .x %>%
      mutate_at(vars(everything()),
                funs(rationalize))
  }
  return(as_tibble(.x))
}


###############################################
# S3 retype
###############################################

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
#' @seealso \code{\link{s}}, \code{\link{rationalize}}
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
                funs(retype))
  } else {
    .x <- .x %>%
      mutate_all(retype)
  }
  return(as_tibble(.x))
}



###############################################
# s
###############################################

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
#' @seealso \code{\link{retype}}, \code{\link{rationalize}}
#'
#' @examples
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
#' dplyr::first(vector)
#' # With s
#' dplyr::first(s(vector))
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
#' # Base R aggregation with dplyr's summarize
#' dplyr::summarise(df_test, mean_a = mean(a), 
#'                           min_c = min(c, na.rm = TRUE))
#' # With s
#' dplyr::summarise(df_test, mean_a = mean(s(a)), 
#'                           min_c = min(s(c)))
#'
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



##################################
### simplifying math functions ###
##################################

#' @title Combine aggregate function and s
#' @name wrapper - s and summary funs
#' @aliases sum_
#' @aliases mean_
#' @aliases max_
#' @aliases min_
#' @aliases first_
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
#' first_(..., ignore_na = TRUE)
#'
#' @param ... one or more vectors
#' @param ignore_na if false missing values are not omitted.
#'
#' @return a single aggregated value
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
#' ## Use of s when NA should not be removes
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
first_ <- function(..., ignore_na = TRUE) {
  first(s(..., ignore_na = ignore_na))}



###############################################
# as_reliable_[data type]
###############################################

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
    stop("Logical vectors can't be converted to logical.")}
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
    stop("Logical vectors can't be converted to logical.")}
  if(is.factor(.x)) {
    .x <- as.character(.x)}
  if(TRUE) {
    return(as.POSIXct(.x, origin = origin, tz = tz, ...))}
}



###############################################
# convert
###############################################

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
#' @usage convert(.x, ...)
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
#'   \code{convert(.df, num(.x, .y)} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{chr}{Changes to character columns. \cr\strong{Usage: }
#'   \code{convert(.df, chr(.x, .y)} where
#'   \code{.x} and \code{.y} are column names of \code{.df}}
#'
#'   \item{int}{Changes to integer columns. \cr\strong{Usage: }
#'   \code{convert(.df, int(.x, .y)} where
#'   \code{.x} and \code{.y} are column names of \code{.df}}
#'
#'   \item{lgl}{Changes to logical columns. \cr\strong{Usage: }
#'   \code{convert(.df, lgl(.x, .y)} where
#'   \code{.x} and \code{.y} are column names of \code{.df}}
#'
#'   \item{dte}{Changes to date columns. Default origin set to "1970-01-01" \cr\strong{Usage: }
#'   \code{convert(.df, dte(.x, .y)} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{dtm}{Changes to date-time columns (POSIXct). Default origin set to "1970-01-01". \cr\strong{Usage: }
#'   \code{convert(.df, dtm(.x, .y)} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{fct}{Changes to factor columns. \cr\strong{Usage: }
#'   \code{convert(.df, fct(.x, .y)} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#'
#'   \item{dbl}{Changes to numeric columns. \cr\strong{Usage: }
#'   \code{convert(.df, dbl(.x, .y)} where \code{.x} and
#'   \code{.y} are column names of \code{.df}}
#' }
#'
#' It is also possible to use multiple scoping functions inside \code{convert}. For example,
#' \code{convert(.df, num(.x), chr(.y))} works as well.
#'
#' @examples
#' # Changing a columns to numeric
#' # and another one to character
#' convert(mtcars, num(gear),
#'                 chr(mpg))
#'
#'
#' # Changing multiple data types on multiple columns
#' convert(mtcars, int(hp,
#'                     wt),
#'                 fct(qsec,
#'                     cyl,
#'                     drat))
#'
#' @rdname convert
#' @export

num <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as_reliable_num), .args))}

#' @rdname convert
#' @export
chr <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as.character), .args))}

#' @rdname convert
#' @export
lgl <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as_reliable_lgl)))}

#' @rdname convert
#' @export
int <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as_reliable_int), .args))}

#' @rdname convert
#' @export
dbl <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as_reliable_num), .args))}

#' @rdname convert
#' @export
fct <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as.factor), .args))}

#' @rdname convert
#' @export
dtm <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as_reliable_dtm), .args))}

#' @rdname convert
#' @export
dte <- function(..., .args = list()){
  list(vars = quos(...),
       fun = funs_(substitute(as_reliable_dte), args = .args))}

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

