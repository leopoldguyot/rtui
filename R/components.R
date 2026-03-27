#' Text output component
#'
#' Displays the value of `output$<outputId>` as text.
#'
#' @param outputId A single character string naming the output id.
#'
#' @return A `rtuiComponent` list node of type `"outputText"`.
#'
#' @export
tuiOutputText <- function(outputId) {
  if (!is.character(outputId) || length(outputId) != 1L || is.na(outputId))
    stop("`outputId` must be a single character string.")

  structure(
    list(type = "outputText", outputId = outputId),
    class = "rtuiComponent"
  )
}

#' Numeric output component
#'
#' Displays the value of `output$<outputId>` as numeric text.
#'
#' @param outputId A single character string naming the output id.
#'
#' @return A `rtuiComponent` list node of type `"outputNumeric"`.
#'
#' @export
tuiOutputNumeric <- function(outputId) {
  if (!is.character(outputId) || length(outputId) != 1L || is.na(outputId))
    stop("`outputId` must be a single character string.")

  structure(
    list(type = "outputNumeric", outputId = outputId),
    class = "rtuiComponent"
  )
}

#' Internal helper `.rtuiNormalizeButtonColor`.
#'
#' Validates and normalizes a button color argument.
#'
#' @param color Optional color value for `tuiInputButton()`.
#'
#' @return `NULL` or a normalized lowercase color string.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeButtonColor <- function(color) {
  if (is.null(color)) {
    return(NULL)
  }

  if (!is.character(color) || length(color) != 1L || is.na(color)) {
    stop("`color` must be NULL or a single character string.")
  }

  normalized <- tolower(trimws(color))
  if (nchar(normalized) == 0L) {
    stop("`color` must not be an empty string.")
  }

  if (grepl("^#[0-9a-f]{6}$", normalized)) {
    return(normalized)
  }

  aliases <- c(
    gray = "graylight",
    grey = "graylight",
    greylight = "graylight",
    greydark = "graydark"
  )
  if (normalized %in% names(aliases)) {
    normalized <- aliases[[normalized]]
  }

  allowed <- c(
    "default", "black", "red", "green", "yellow", "blue", "magenta", "cyan",
    "graylight", "graydark", "redlight", "greenlight", "yellowlight",
    "bluelight", "magentalight", "cyanlight", "white"
  )

  if (!normalized %in% allowed) {
    stop(
      "`color` must be one of ",
      paste(shQuote(allowed), collapse = ", "),
      ", or a hex color like '#RRGGBB'."
    )
  }

  normalized
}

#' Button input component
#'
#' A focusable button. When activated (Enter key), it increments `input$id`
#' and triggers a server update.
#'
#' @param label A character string shown as the button label.
#' @param id A character string used as the input key (`input$<id>`).
#' @param color Optional button color. Supports named colors (`"default"`,
#'   `"black"`, `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`,
#'   `"cyan"`, `"graylight"`, `"graydark"`, `"redlight"`, `"greenlight"`,
#'   `"yellowlight"`, `"bluelight"`, `"magentalight"`, `"cyanlight"`,
#'   `"white"`) or a hex string like `"#RRGGBB"`.
#'
#' @return A `rtuiComponent` list node of type `"button"`.
#'
#' @export
tuiInputButton <- function(label, id, color = NULL) {
  if (!is.character(label) || length(label) != 1L || is.na(label))
    stop("`label` must be a single character string.")
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")
  color <- .rtuiNormalizeButtonColor(color)

  component <- list(type = "button", label = label, id = id)
  if (!is.null(color)) {
    component$color <- color
  }

  structure(
    component,
    class = "rtuiComponent"
  )
}

#' Text input component
#'
#' A focusable text input. The typed value is stored automatically in
#' `input$<id>` and triggers a server update on change. The input starts with
#' `value`. By default (`multiline = FALSE`), Enter triggers submit without
#' inserting a newline. Set `multiline = TRUE` to allow newlines.
#'
#' @param id A character string used as the input key (`input$<id>`).
#' @param placeholder A character string shown when the input is empty.
#' @param value A character string used as the initial/default input value.
#' @param multiline A single logical value. If `FALSE` (default), Enter does
#'   not modify the text content. If `TRUE`, Enter inserts a newline.
#'
#' @return A `rtuiComponent` list node of type `"input"`.
#'
#' @export
tuiInputText <- function(id, placeholder = "", value = "", multiline = FALSE) {
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")
  if (!is.character(placeholder) || length(placeholder) != 1L || is.na(placeholder))
    stop("`placeholder` must be a single character string.")
  if (!is.character(value) || length(value) != 1L || is.na(value))
    stop("`value` must be a single character string.")
  if (!is.logical(multiline) || length(multiline) != 1L || is.na(multiline))
    stop("`multiline` must be TRUE or FALSE.")

  component <- list(type = "input", id = id, placeholder = placeholder, value = value)
  if (isTRUE(multiline)) {
    component$multiline <- TRUE
  }

  structure(
    component,
    class = "rtuiComponent"
  )
}
