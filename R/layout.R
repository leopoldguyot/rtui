#' Vertical box layout
#'
#' Stacks child components vertically.
#'
#' @param ... Child components built with layout or component functions.
#'
#' @return A `rtui_component` list node of type `"vbox"`.
#'
#' @export
tui_vbox <- function(...) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtui_component"))
      stop("All children must be rtui components.")
  })
  structure(
    list(type = "vbox", children = children),
    class = "rtui_component"
  )
}

#' Horizontal box layout
#'
#' Places child components side by side horizontally.
#'
#' @param ... Child components built with layout or component functions.
#'
#' @return A `rtui_component` list node of type `"hbox"`.
#'
#' @export
tui_hbox <- function(...) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtui_component"))
      stop("All children must be rtui components.")
  })
  structure(
    list(type = "hbox", children = children),
    class = "rtui_component"
  )
}
