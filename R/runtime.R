.rtuiRuntimeContext <- new.env(parent = emptyenv())
.rtuiRuntimeContext$currentRuntime <- NULL

#' Internal helper `.rtuiCurrentRuntime`.
#'
#' Returns the runtime currently bound in the global runtime context.
#'
#' @return Runtime environment for the current server run.
#'
#' @keywords internal
#' @noRd
.rtuiCurrentRuntime <- function() {
  runtime <- .rtuiRuntimeContext$currentRuntime
  if (is.null(runtime)) {
    stop(
      "Reactive/render helpers must be used inside `tuiApp(..., server = ...)`."
    )
  }
  runtime
}

#' Internal helper `.rtuiInIsolate`.
#'
#' Reports whether the runtime is currently inside `tuiIsolate()`.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return `TRUE` when isolate depth is greater than zero.
#'
#' @keywords internal
#' @noRd
.rtuiInIsolate <- function(runtime) {
  isTRUE(runtime$currentIsolateDepth > 0L)
}

#' Internal helper `.rtuiCaptureSuspended`.
#'
#' Reports whether dependency capture is currently suspended.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return `TRUE` when capture depth is greater than zero.
#'
#' @keywords internal
#' @noRd
.rtuiCaptureSuspended <- function(runtime) {
  isTRUE(runtime$currentCaptureDepth > 0L)
}

#' Internal helper `.rtuiWithCaptureSuspended`.
#'
#' Evaluates an expression with dependency capture temporarily suspended.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param expr Expression to evaluate or inspect.
#'
#' @return Value of `expr`.
#'
#' @keywords internal
#' @noRd
.rtuiWithCaptureSuspended <- function(runtime, expr) {
  runtime$currentCaptureDepth <- runtime$currentCaptureDepth + 1L
  on.exit({
    runtime$currentCaptureDepth <- max(0L, runtime$currentCaptureDepth - 1L)
  }, add = TRUE)
  expr
}

#' Internal helper `.rtuiWithCaptureEnabled`.
#'
#' Evaluates an expression with dependency capture force-enabled.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param expr Expression to evaluate or inspect.
#'
#' @return Value of `expr`.
#'
#' @keywords internal
#' @noRd
.rtuiWithCaptureEnabled <- function(runtime, expr) {
  previousDepth <- runtime$currentCaptureDepth
  runtime$currentCaptureDepth <- 0L
  on.exit({
    runtime$currentCaptureDepth <- previousDepth
  }, add = TRUE)
  expr
}
