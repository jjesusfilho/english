#' @name ordinal 
#' @title  Generic functions and methods generating portuguese versions of
#'     ordinal whole numbers as character strings
#' 
#' @description The function \code{ordinal} is generic and works in much
#'   the same way as the function \code{as.character.portuguese} but
#'   produces an ordinal, rather than a cardinal number version
#' 
#' 
#' @param x  An object to produce portuguese ordinal output.
#' @param ... Additional arguments passed on to \code{portuguese} for the initial
#'     conversion.
#' @details The object is first converted to a character string an \code{portuguese}
#'     object, if necessary, then to character and finally adjusted so that
#'    it provides the ordinal version of the portuguese representation.
#' 
#' @return A character string vector of ordinal numbers in portuguese.
#' @examples
#'   ## for Brazil style portuguese:
#'   ordinal(c(1, 9, 10, 11, 12, 19, 20, 21, 99, 100, 101, 109, 111,
#'             119, 1000, 1100, 1199, 9999, 10000, 10001), UK = TRUE)
#'   ## for Portugal style portuguese:
#'   ordinal(c(1, 9, 10, 11, 12, 19, 20, 21, 99, 100, 101, 109, 111,
#'             119, 1000, 1100, 1199, 9999, 10000, 10001), UK = FALSE)
#'   ## For mothers of small children:
#'   cat(paste("Esta é a", ordinal(1:5), "vez que eu lhe falo!"), sep = "\n")
#' 
#' @export
ordinal <- function(x, ...) {
  UseMethod("ordinal")
}

#' @export
#' @describeIn ordinal convert ordinal to text
ordinal.portuguese <- local({
  Cards <-
    c("um",
      "dois",
      "tr\u00eas",
      "quatro",
      "cinco",
      "seis",
      "sete",
      "oito",
      "nove",
      "dez")
  Ords <-
    c(
      "primeiro",
      "segundo",
      "terceiro",
      "quarto",
      "quinto",
      "sexto",
      "s\u00e9timo",
      "oitavo",
      "nono",
      "d\u00e9cimo"
    )
  
  function(x, ...) {
    initial <- sub("^(.*) (.*)$", "\\1 ", x)
    final   <- sub("^(.*) (.*)$", "\\2",  x)
    
    initial[initial == final] <- ""
    
    
    final <- ifelse(final %in% Cards, Ords[match(final, Cards)],
                    final)
    
    noquote(final)
  }
})

#' @export
#' @describeIn ordinal convert ordinal to text
ordinal.numeric <- function(x, ...)
  ordinal.portuguese(portuguese(x, ...))

#' @export
#' @describeIn ordinal convert ordinal to text
ordinal.character <- ordinal.portuguese
