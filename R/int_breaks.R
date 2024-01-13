#' Title
#'
#' @param x 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
int_breaks <- function(x, n = 5) {
    if (length(unique(x)) > 1) {
        pretty(x, n)[round(pretty(x, n), 1)%%1 == 0]
    }
    else {
        round(unique(x)) + c(-1, 0, 1)
    }
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
int_limits <- function(x) {
    if (length(unique(x)) > 1) {
        range(x)
    }
    else {
        range(int_breaks(x))
    }
}
