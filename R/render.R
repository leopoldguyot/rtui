#' Internal helper `.rtuiResolveOutputValue`.
#'
#' Resolves renderer objects to final output values for the UI.
#'
#' @param value Value to evaluate, resolve, or store.
#'
#' @return Resolved output value suitable for terminal rendering.
#'
#' @keywords internal
#' @noRd
.rtuiResolveOutputValue <- function(value) {
  if (!inherits(value, "rtuiRenderer")) {
    return(value)
  }

  runtime <- .rtuiCurrentRuntime()
  evalEnv <- new.env(parent = value$env)
  evalEnv$input <- runtime$currentInput
  evalEnv$output <- runtime$currentOutput

  rendered <- tryCatch(
    {
      evaluated <- eval(value$expr, envir = evalEnv)
      .rtuiEvalMaybeReactive(evaluated)
    },
    rtui_req_error = function(err) {
      .rtuiReqDefaultValue()
    }
  )

  if (identical(value$kind, "text")) {
    if (inherits(rendered, "rtuiReqDefault")) {
      return("")
    }
    if (is.null(rendered) || length(rendered) == 0L) {
      return("")
    }
    return(as.character(rendered[[1L]]))
  }

  if (identical(value$kind, "numeric")) {
    if (inherits(rendered, "rtuiReqDefault")) {
      return("NA")
    }
    numericValue <- suppressWarnings(as.numeric(rendered))
    if (length(numericValue) == 0L) {
      return("NA")
    }

    scalar <- numericValue[[1L]]
    if (!is.finite(scalar)) {
      return(as.character(scalar))
    }

    if (!is.null(value$digits)) {
      scalar <- round(scalar, digits = value$digits)
      return(format(
        scalar,
        nsmall = value$digits,
        trim = TRUE,
        scientific = FALSE
      ))
    }

    if (is.finite(scalar) && abs(scalar - round(scalar)) <= sqrt(.Machine$double.eps)) {
      return(as.integer(round(scalar)))
    }
    return(as.character(scalar))
  }

  value
}

#' Create a text renderer for `output$...`
#'
#' @param expr An expression returning text-compatible output.
#'
#' @details
#' Renderer expressions should ideally be side-effect free. If a `tuiRender*`
#' expression mutates reactive state (for example via `tuiReactiveVal()`), the
#' output can invalidate itself during its own evaluation. In that case, `rtui`
#' emits a warning (once per output id) and still completes the render pass.
#'
#' @return A text renderer object for assignment in `output$...`.
#'
#' @export
tuiRenderText <- function(expr) {
  structure(
    list(kind = "text", expr = substitute(expr), env = parent.frame()),
    class = "rtuiRenderer"
  )
}

#' Create a numeric renderer for `output$...`
#'
#' @param expr An expression returning numeric-compatible output.
#' @param digits Optional number of digits after the decimal point.
#'
#' @details
#' Renderer expressions should ideally be side-effect free. If a `tuiRender*`
#' expression mutates reactive state (for example via `tuiReactiveVal()`), the
#' output can invalidate itself during its own evaluation. In that case, `rtui`
#' emits a warning (once per output id) and still completes the render pass.
#'
#' @return A numeric renderer object for assignment in `output$...`.
#'
#' @export
tuiRenderNumeric <- function(expr, digits = NULL) {
  if (!is.null(digits)) {
    if (
      !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0
    ) {
      stop("`digits` must be NULL or a single non-negative number.")
    }
    digits <- as.integer(digits)
  }

  structure(
    list(
      kind = "numeric",
      expr = substitute(expr),
      env = parent.frame(),
      digits = digits
    ),
    class = "rtuiRenderer"
  )
}
