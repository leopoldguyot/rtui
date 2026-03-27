#' Vertical box layout
#'
#' Stacks child components vertically.
#'
#' @param ... Child components built with layout or component functions.
#'
#' @return A `rtuiComponent` list node of type `"column"`.
#'
#' @export
tuiColumn <- function(...) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtuiComponent"))
      stop("All children must be rtuiComponent objects.")
  })
  structure(
    list(type = "column", children = children),
    class = "rtuiComponent"
  )
}

#' Horizontal box layout
#'
#' Places child components side by side horizontally.
#'
#' @param ... Child components built with layout or component functions.
#'
#' @return A `rtuiComponent` list node of type `"row"`.
#'
#' @export
tuiRow <- function(...) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtuiComponent"))
      stop("All children must be rtuiComponent objects.")
  })
  structure(
    list(type = "row", children = children),
    class = "rtuiComponent"
  )
}

#' Box layout wrapper
#'
#' Draws a border around a child component, with optional title and style.
#'
#' @param child A single `rtuiComponent` to wrap.
#' @param title Optional single character string title displayed in the border.
#'   Use `NULL` (default) for no title.
#' @param color Optional border color. Supports named colors (`"default"`,
#'   `"black"`, `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`,
#'   `"cyan"`, `"graylight"`, `"graydark"`, `"redlight"`, `"greenlight"`,
#'   `"yellowlight"`, `"bluelight"`, `"magentalight"`, `"cyanlight"`,
#'   `"white"`) or a hex string like `"#RRGGBB"`.
#' @param style Border style. One of `"rounded"` (default), `"light"`,
#'   `"dashed"`, `"heavy"`, `"double"`, or `"empty"`.
#' @param titleStyle Title rendering style. One of `"header"` (default), which
#'   places the title inside the box as a header separated by a bar, or
#'   `"border"`, which places the title in the top-left border line.
#'
#' @return A `rtuiComponent` list node of type `"box"`.
#'
#' @export
tuiBox <- function(
    child,
    title = NULL,
    color = NULL,
    style = "rounded",
    titleStyle = "header"
) {
  if (!inherits(child, "rtuiComponent")) {
    stop("`child` must be a rtuiComponent object.")
  }
  if (!is.null(title) && (!is.character(title) || length(title) != 1L || is.na(title))) {
    stop("`title` must be NULL or a single character string.")
  }
  if (!is.character(style) || length(style) != 1L || is.na(style)) {
    stop("`style` must be a single character string.")
  }
  style <- tolower(trimws(style))
  allowed_styles <- c("rounded", "light", "dashed", "heavy", "double", "empty")
  if (!style %in% allowed_styles) {
    stop("`style` must be one of ", paste(shQuote(allowed_styles), collapse = ", "), ".")
  }

  if (!is.character(titleStyle) || length(titleStyle) != 1L || is.na(titleStyle)) {
    stop("`titleStyle` must be a single character string.")
  }
  titleStyle <- tolower(trimws(titleStyle))
  allowed_title_styles <- c("header", "border")
  if (!titleStyle %in% allowed_title_styles) {
    stop(
      "`titleStyle` must be one of ",
      paste(shQuote(allowed_title_styles), collapse = ", "),
      "."
    )
  }

  color <- .rtuiNormalizeColor(color)

  component <- list(
    type = "box",
    child = child,
    style = style,
    titleStyle = titleStyle
  )
  if (!is.null(title)) {
    component$title <- title
  }
  if (!is.null(color)) {
    component$color <- color
  }

  structure(component, class = "rtuiComponent")
}
