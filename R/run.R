#' Run a TUI application
#'
#' Starts the terminal event loop for an `rtuiApp`. This call is **blocking**:
#' it takes over the terminal until the user presses `Escape` or `Ctrl+Q`.
#'
#' @param app An `rtuiApp` object created by [tuiApp()].
#' @param overflow Overflow handling strategy when content exceeds terminal size.
#'   Use `"clip"` (default) to crop content to the visible terminal viewport, or
#'   `"scroll"` to wrap the app in a global scrollable viewport.
#'   Use `"block"` to temporarily block interaction and show only a warning
#'   message while the terminal is smaller than the app's minimum required size.
#'
#'   This setting is global to the whole app. Per-container overflow can be
#'   configured independently with `overflowX` / `overflowY` on [tuiRow()],
#'   [tuiColumn()], and [tuiBox()].
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiRun <- function(app, overflow = "clip") {
  if (!inherits(app, "rtuiApp")) {
    stop("`app` must be an `rtuiApp` created by `tuiApp()`.")
  }

  if (!is.character(overflow) || length(overflow) != 1L || is.na(overflow)) {
    stop("`overflow` must be a single character string.")
  }
  overflow <- tolower(trimws(overflow))
  allowed_overflow <- c("clip", "scroll", "block")
  if (!overflow %in% allowed_overflow) {
    stop(
      "`overflow` must be one of ",
      paste(shQuote(allowed_overflow), collapse = ", "),
      "."
    )
  }

  if (is.null(app$ui) || is.null(app$server)) {
    stop("`app` is invalid. Recreate it with `tuiApp(ui, server)`.")
  }

  fresh_app <- tuiApp(app$ui, app$server)
  runTuiApp(fresh_app$ui, fresh_app$state, fresh_app$handlers, overflow)
  invisible(NULL)
}

#' @export
print.rtuiApp <- function(x, ...) {
  cat("<rtuiApp>\n")
  cat("  input ids: ", paste(names(x$state$input), collapse = ", "), "\n")
  cat("  output ids:", paste(names(x$state$output), collapse = ", "), "\n")
  invisible(x)
}
