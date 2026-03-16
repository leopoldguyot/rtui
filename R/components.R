#' Text output component
#'
#' Displays the value of an `output` key as text. The display updates
#' automatically whenever the server updates that output key.
#'
#' @param key An `output$<key>` or `output[["key"]]` reference.
#'
#' @return A `rtuiComponent` list node of type `"text"`.
#'
#' @export
tuiRenderText <- function(key) {
  keyExpr <- substitute(key)
  outputKey <- .rtuiExtractMemberId(keyExpr, "output")
  if (is.null(outputKey))
    stop(
      "`key` must be an `output$<key>` or `output[[\"key\"]]` reference."
    )

  key <- outputKey
  structure(
    list(type = "text", key = key),
    class = "rtuiComponent"
  )
}

#' Button component
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
  if (!is.character(label) || length(label) != 1L)
    stop("`label` must be a single character string.")
  if (!is.character(id) || length(id) != 1L)
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
  if (!is.character(id) || length(id) != 1L)
    stop("`id` must be a single character string.")
  if (!is.character(placeholder) || length(placeholder) != 1L)
    stop("`placeholder` must be a single character string.")
  if (!is.character(value) || length(value) != 1L)
    stop("`value` must be a single character string.")
  structure(
    list(type = "input", id = id, placeholder = placeholder, value = value),
    class = "rtuiComponent"
  )
}
