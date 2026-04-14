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
#' @param screen Screen backend strategy. Use `"fullscreen"` for full-screen
#'   alternate-buffer rendering, `"terminal"` for inline terminal output mode, or
#'   `"auto"` (default) to choose automatically for supported terminal sessions.
#'
#' `tuiRun()` requires an interactive TTY for both input and output. The RStudio
#' Console is not a real terminal TTY and is therefore not supported for running
#' rtui apps directly.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiRun <- function(app, overflow = "clip", screen = "auto") {
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

  if (!is.character(screen) || length(screen) != 1L || is.na(screen)) {
    stop("`screen` must be a single character string.")
  }
  screen <- tolower(trimws(screen))
  allowed_screen <- c("auto", "fullscreen", "terminal")
  if (!screen %in% allowed_screen) {
    stop(
      "`screen` must be one of ",
      paste(shQuote(allowed_screen), collapse = ", "),
      "."
    )
  }

  if (is.null(app$ui) || is.null(app$server)) {
    stop("`app` is invalid. Recreate it with `tuiApp(ui, server)`.")
  }

  input_is_tty <- isTRUE(isatty(stdin()))
  output_is_tty <- isTRUE(isatty(stdout()))
  if (!input_is_tty || !output_is_tty) {
    stop(
      "`tuiRun()` requires an interactive TTY for both stdin and stdout. ",
      "This session is not attached to a real terminal (for example, the RStudio Console). ",
      "Run the app from a terminal (R, Rscript, or RStudio Terminal)."
    )
  }

  is_rstudio_console <- identical(.Platform$GUI, "RStudio")
  resolved_screen <- if (screen == "auto") {
    if (is_rstudio_console) "terminal" else "fullscreen"
  } else {
    screen
  }

  fresh_app <- tuiApp(app$ui, app$server)
  runTuiApp(
    fresh_app$ui,
    fresh_app$state,
    fresh_app$handlers,
    overflow,
    resolved_screen
  )
  invisible(NULL)
}

#' @export
print.rtuiApp <- function(x, ...) {
  cat("<rtuiApp>\n")
  cat("  input ids: ", paste(names(x$state$input), collapse = ", "), "\n")
  cat("  output ids:", paste(names(x$state$output), collapse = ", "), "\n")
  invisible(x)
}
