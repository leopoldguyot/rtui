#' Create a TUI application
#'
#' Defines a terminal UI application with a reactive state, a UI component
#' tree, and named event handlers.
#'
#' @param state A named list representing the initial application state.
#' @param ui A UI component tree built with [tui_vbox()], [tui_hbox()],
#'   [tui_render_text()], [tui_input_button()], or [tui_input_text()].
#' @param handlers A named list of functions. Each function receives the
#'   current state list and must return the updated state list.
#'
#' @return An object of class `rtui_app`.
#'
#' @examples
#' app <- tui_app(
#'   state = list(counter = 0L),
#'   ui = tui_vbox(
#'     tui_render_text("counter"),
#'     tui_input_button("Increment", id = "inc")
#'   ),
#'   handlers = list(
#'     inc = function(state) {
#'       state$counter <- state$counter + 1L
#'       state
#'     }
#'   )
#' )
#'
#' @export
tui_app <- function(state = list(), ui, handlers = list()) {
  if (!is.list(state))
    stop("`state` must be a named list.")
  if (!inherits(ui, "rtui_component"))
    stop("`ui` must be a rtui component (built with tui_vbox, tui_hbox, etc.).")
  if (!is.list(handlers))
    stop("`handlers` must be a named list of functions.")

  structure(
    list(state = state, ui = ui, handlers = handlers),
    class = "rtui_app"
  )
}

#' Run a TUI application
#'
#' Starts the terminal event loop for an `rtui_app`. This call is **blocking**:
#' it takes over the terminal until the user presses `Escape` or `Ctrl+Q`.
#'
#' @param app An `rtui_app` object created by [tui_app()].
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tui_run <- function(app) {
  if (!inherits(app, "rtui_app"))
    stop("`app` must be an `rtui_app` created by `tui_app()`.")

  run_tui_app(app$ui, app$state, app$handlers)
  invisible(NULL)
}

#' @export
print.rtui_app <- function(x, ...) {
  cat("<rtui_app>\n")
  cat("  state keys:", paste(names(x$state), collapse = ", "), "\n")
  cat("  handlers:  ", paste(names(x$handlers), collapse = ", "), "\n")
  invisible(x)
}
