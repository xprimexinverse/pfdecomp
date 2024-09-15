#' Class definition for pfdecomp Cobb-Douglas object
#'
#' @slot output ts.
#' @slot capital ts.
#' @slot labour ts.
#' @slot beta numeric.
#' @slot solow ts.
#' @slot contribs mts.
#' @slot avg_gr matrix.
#'
#' @return
#' @export
#'
#' @examples
setClass("Cobb-Douglas",
         slots = list(output   = "ts",
                      capital  = "ts",
                      labour   = "ts",
                      beta     = "numeric",
                      solow    = "ts",
                      contribs = "mts",
                      avg_gr   = "matrix"))

#' Class definition for pfdecomp CES object
#'
#' @slot output ts.
#' @slot capital ts.
#' @slot labour ts.
#' @slot beta numeric.
#' @slot sigma numeric.
#' @slot solow ts.
#' @slot contribs mts.
#' @slot avg_gr matrix.
#'
#' @return
#' @export
#'
#' @examples
setClass("CES",
         slots = list(output   = "ts",
                      capital  = "ts",
                      labour   = "ts",
                      beta     = "numeric",
                      sigma    = "numeric",
                      solow    = "ts",
                      contribs = "mts",
                      avg_gr   = "matrix"))
