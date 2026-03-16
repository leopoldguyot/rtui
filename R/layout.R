#' Vertical box layout
#'
#' Stacks child components vertically.
#'
#' @param ... Child components built with layout or component functions.
#'
#' @return A `rtuiComponent` list node of type `"vbox"`.
#'
#' @export
tuiColumn <- function(...) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtuiComponent"))
      stop("All children must be rtuiComponent objects.")
  })
  structure(
    list(type = "vbox", children = children),
    class = "rtuiComponent"
  )
}

#' Horizontal box layout
#'
#' Places child components side by side horizontally.
#'
#' @param ... Child components built with layout or component functions.
#'
#' @return A `rtuiComponent` list node of type `"hbox"`.
#'
#' @export
tuiRow <- function(...) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtuiComponent"))
      stop("All children must be rtuiComponent objects.")
  })
  structure(
    list(type = "hbox", children = children),
    class = "rtuiComponent"
  )
}
