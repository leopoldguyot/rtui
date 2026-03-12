#' Text output component
#'
#' Displays the value of a state key as text. The display updates automatically
#' whenever the state changes.
#'
#' @param key A character string naming the state key whose value to display.
#'
#' @return A `rtui_component` list node of type `"text"`.
#'
#' @export
tui_render_text <- function(key) {
  if (!is.character(key) || length(key) != 1L)
    stop("`key` must be a single character string.")
  structure(
    list(type = "text", key = key),
    class = "rtui_component"
  )
}

#' Button component
#'
#' A focusable button that, when activated (Enter key), calls the handler
#' identified by `id`.
#'
#' @param label A character string shown as the button label.
#' @param id A character string matching a key in the `handlers` list passed
#'   to [tui_app()].
#'
#' @return A `rtui_component` list node of type `"button"`.
#'
#' @export
tui_input_button <- function(label, id) {
  if (!is.character(label) || length(label) != 1L)
    stop("`label` must be a single character string.")
  if (!is.character(id) || length(id) != 1L)
    stop("`id` must be a single character string.")
  structure(
    list(type = "button", label = label, id = id),
    class = "rtui_component"
  )
}

#' Text input component
#'
#' A focusable single-line text input. The typed value is stored in the state
#' under the given `id` key and is accessible inside handlers.
#'
#' @param id A character string used as the state key for the input's value.
#' @param placeholder A character string shown when the input is empty.
#'
#' @return A `rtui_component` list node of type `"input"`.
#'
#' @export
tui_input_text <- function(id, placeholder = "") {
  if (!is.character(id) || length(id) != 1L)
    stop("`id` must be a single character string.")
  if (!is.character(placeholder) || length(placeholder) != 1L)
    stop("`placeholder` must be a single character string.")
  structure(
    list(type = "input", id = id, placeholder = placeholder),
    class = "rtui_component"
  )
}
