#' Run a TUI application
#'
#' Starts the terminal event loop for an `rtuiApp`. This call is **blocking**:
#' it takes over the terminal until the user presses `Escape` or `Ctrl+Q`.
#'
#' @param app An `rtuiApp` object created by [tuiApp()].
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiRun <- function(app) {
  if (!inherits(app, "rtuiApp")) {
    stop("`app` must be an `rtuiApp` created by `tuiApp()`.")
  }

  if (is.null(app$ui) || is.null(app$server)) {
    stop("`app` is invalid. Recreate it with `tuiApp(ui, server)`.")
  }

  fresh_app <- tuiApp(app$ui, app$server)
  runTuiApp(fresh_app$ui, fresh_app$state, fresh_app$handlers)
  invisible(NULL)
}

#' @export
print.rtuiApp <- function(x, ...) {
  cat("<rtuiApp>\n")
  cat("  input ids: ", paste(names(x$state$input), collapse = ", "), "\n")
  cat("  output ids:", paste(names(x$state$output), collapse = ", "), "\n")
  invisible(x)
}
