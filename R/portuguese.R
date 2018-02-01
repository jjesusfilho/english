#' @name as.portuguese
#' @title Generic functions and methods for S3 class english
#'
#' @description The functions \code{as.portuguese} and \code{portuguese} are fully
#'    equivalent generic constructor functions for the S3 class
#'    \code{portuguese}, which allows numeric objects to be represented in
#'    the form of their customary Portuguese expression.
#'
#'
#' @param x Any numeric object.  If the components are not integers, they are
#'    rounded.  In the case of \code{portuguese.default} this may be an
#'    object of any class, but will always result in an error.
#' @param BR A logical flag, should Brazil-style numbers be given (\code{TRUE})
#'     or Portugal style (\code{FALSE})? If missing, a default value is
#'     chosen as \code{TRUE} unless the locale in use is guessed to be an
#'     Portuguese locale of Portugal.
#' @param ... For \code{rep}, and \code{format} arguments passed on to other
#'     methods.  Ignored in all other cases.
#' @param i  Any allowable form of index vector.
#'
#' @details The function \code{portuguese} is a generic constructor function for
#'     objects of class \code{portuguese}.  The methods either mark the object
#'     as having the appropriate class, or, in the case of
#'     \code{portuguese.default}, result in an error message.  The function
#'     \code{as.portuguese} is provided as a corresponding function to
#'     \code{as.roman} in the \code{utils} package.  The method
#'     \code{portuguese.portuguese} simply allows the constructor function to act
#'     as a coercion and to have no effect on objects already of the class.
#' @return  An object of class \code{portuguese}, if possible, or an error message if
#'     not.
#'
#' @export
as.portuguese <- portuguese<-function (x, ...) {
  UseMethod("portuguese")
}


#' @describeIn as.portuguese Generic functions and methods for S3 class english
portuguese.default <- function (x, ...)
  stop("no method defined for objects of class ",
       paste(dQuote(class(x)), collapse = ", "))


#' @describeIn as.portuguese Convert an portuguese object back to class numeric
#' @export
portuguese.numeric <- portuguese.portuguese <- function (x, BR, ...) {
  if (missing(BR)) {
    BR <- !grepl("^(pt_br|portuguese_brazil)",
                 tolower(Sys.getlocale("LC_CTYPE")))
  } else {
    BR <- as.logical(BR)[1]
    if (is.na(BR))
      stop("Bad specification of the BR flag. Must be logical scalar")
  }
  structure(x, class = "portuguese", useBR = BR)
}

#' Arithmetic operations with objects of class portuguse
#'
#' Allows arithmetic with objects of class \code{portuguese}.
#'
#' @param e1 Numeric vectors, of which at least one must be of class \code{portuguese}.
#' @param e2 Numeric vectors, of which at least one must be of class \code{portuguese}.
#'
#' @return A numeric vector of class \code{portuguese}.
#' @export
#'
Ops.portuguese <- function (e1, e2) {
  e1 <- unclass(e1)
  if (!missing(e2))
    e2 <- unclass(e2)
  structure(NextMethod(.Generic),
            class = "portuguese",
            useBR = attr(e1, "useBR"))
}

#' @describeIn as.portuguese Convert an Portuguese object back to class numeric
#'
#' This a convenience function that simply removes the S3 class attribute
#'    from and object of class \code{portuguese}.
#'
#'
#' @return A numeric vector of class \code{portuguese}.
#' @export
#'
as.numeric.portuguese <- function(x, ...) {
  x <- unclass(x)
  attr(x, "useBR") <- NULL
  x
}



#' @describeIn as.portuguese A print method for objects of class portuguese
#' @description  Provides a \code{print} method for objects of class \code{portuguese}.
#' @return The original object, invisibly.
#' @export
#'
#' @examples
print.portuguese <- function (x, ...) {
  print(noquote(as.character.portuguese(x)))
  invisible(x)
}
#'
#' @describeIn as.portuguese A sorting method for objects of class  portuguese
#'
#' A method for the generic function \code{sort} which allows numeric
#'     objects of class \code{portuguese} to be sorted.
#'
#' @param decreasing logical: should the object be sorted in decreasing order?
#'
#' @return An object of class \code{portuguese} with its components in numerically
#'     sorted order.
#' @export
#'
sort.portuguese <- function (x, decreasing = FALSE, ...) {
  structure(NextMethod("sort"), class = "portuguese", usePT = attr(x, "useBR"))
}

as.character.portuguese <- local({
  ones <-
    structure(
      c(
        "",
        "um",
        "dois",
        "tr\u00eas",
        "quatro",
        "cinco",
        "seis",
        "sete",
        "oito",
        "nove"
      ),
      .Names = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    )
  
  suffixes <-
    c(
      "mil",
      "milh\u00e3o",
      "bilh\u00e3o",
      "trilh\u00e3o",
      "quatrilh\u00e3o",
      "quintilh\u00e3o",
      "sextilh\u00e3o",
      "setilh\u00e3o"
    )
  
  hundreds <-
    structure(
      c(
        "cento",
        "duzentos",
        "trezentos",
        "quatrocentos",
        "quinhentos",
        "seiscentos",
        "setencentos",
        "oitocentos",
        "novecentos"
      ),
      .Names = c("1", "2", "3", "4", "5", "6",
                 "7", "8", "9")
    )
  
  teens <- structure(
    c(
      "dez",
      "onze",
      "doze",
      "treze",
      "quatorze",
      "quinze",
      "dezesseis",
      "dezessete",
      "dezoito",
      "dezenove"
    ),
    .Names = c("0", "1", "2", "3", "4", "5", "6",
               "7", "8", "9")
  )
  
  tens <-
    structure(
      c(
        "vinte",
        "trinta",
        "quarenta",
        "cinquenta",
        "sessenta",
        "setenta",
        "oitenta",
        "noventa"
      ),
      .Names = c("2", "3", "4", "5", "6", "7", "8", "9")
    )
  
  makeNumber <- function (n)
    as.numeric(paste(n, collapse = ""))
  trim <- function (text)
    sub("^ *", "", sub(" *$", "", gsub("  +", " ", text)))
  
  function (x, ...) {
    BR <- attr(x, "useBR")
    and <- function (dvec) {
      if (BR && (d <- makeNumber(dvec)) > 0 && d < 100)
        "e"
      else
        ""
    }
    helper <- function (x) {
      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1)
        as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19)
          as.vector(teens[digits[1]])
      else
        trim(paste(tens[digits[2]],
                   helper(as.numeric(digits[1]))))
      else if (nDigits == 3)
        trim(paste(hundreds[digits[3]],
                   and(digits[2:1]),
                   helper(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) {
          warning(paste(x, "is too large!"))
          return(as.character(as.vector(x)))
        }
        trim(paste(
          helper(makeNumber(digits[nDigits:(3 * nSuffix + 1)])),
          suffixes[nSuffix],
          and(digits[(3 * nSuffix):1]),
          helper(makeNumber(digits[(3 * nSuffix):1]))
        ))
      }
    }
    
    opts <- options(scipen = 100)
    on.exit(options(opts))
    r <- character(length(x))
    bad <- is.na(x) | is.nan(x) | is.infinite(x)
    if (any(!bad & x %% 1 != 0)) {
      warning("non-integer values rounded for display")
      x <- round(x)
    }
    if (any(n <- !bad & x < 0))
      r[n] <- paste("minus", sapply(-x[n], helper))
    if (any(z <- !bad & x == 0))
      r[z] <- "zero"
    if (any(p <- !bad & x > 0))
      r[p] <- sapply(x[p], helper)
    r[is.na(x)] <- ""
    r[is.nan(x)] <- "not a number"
    if (any(k <- x < 0 & is.infinite(x)))
      r[k] <- "minus infinity"
    if (any(k <- x > 0 & is.infinite(x)))
      r[k] <- "infinity"
    names(r) <- names(x)
    r
  }
})

#' @describeIn as.portuguese  class portuguese
rep.portuguese <- function (x, ...)
  structure(rep(unclass(x), ...), class = class(x))

#' @describeIn as.portuguese Old method
`[.portuguese` <- function(x, i) {
  cl <- oldClass(x)
  y <- NextMethod("[")
  oldClass(y) <- cl
  y
}

#' @describeIn as.portuguese Format for portuguese
format.portuguese <- function(x, ...) {
  format(as.character.portuguese(x), ...)
}
