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

#' Button input component
#'
#' A focusable button. When activated (Enter key), it increments `input$id`
#' and triggers a server update.
#'
#' @param label A character string shown as the button label.
#' @param id A character string used as the input key (`input$<id>`).
#'
#' @return A `rtuiComponent` list node of type `"button"`.
#'
#' @export
tuiInputButton <- function(label, id) {
  if (!is.character(label) || length(label) != 1L || is.na(label))
    stop("`label` must be a single character string.")
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")

  structure(
    list(type = "button", label = label, id = id),
    class = "rtuiComponent"
  )
}

#' Text input component
#'
#' A focusable single-line text input. The typed value is stored automatically
#' in `input$<id>` and triggers a server update on change. The input starts
#' with `value`.
#'
#' @param id A character string used as the input key (`input$<id>`).
#' @param placeholder A character string shown when the input is empty.
#' @param value A character string used as the initial/default input value.
#'
#' @return A `rtuiComponent` list node of type `"input"`.
#'
#' @export
tuiInputText <- function(id, placeholder = "", value = "") {
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")
  if (!is.character(placeholder) || length(placeholder) != 1L || is.na(placeholder))
    stop("`placeholder` must be a single character string.")
  if (!is.character(value) || length(value) != 1L || is.na(value))
    stop("`value` must be a single character string.")

  structure(
    list(type = "input", id = id, placeholder = placeholder, value = value),
    class = "rtuiComponent"
  )
}
