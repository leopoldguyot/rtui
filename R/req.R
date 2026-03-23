#' Internal helper `.rtuiReqTruth`.
#'
#' Implements the truthiness rules used by `tuiReq()`.
#'
#' @param x Value to inspect.
#'
#' @return `TRUE` when `x` is considered available/truthy by `tuiReq()`.
#'
#' @keywords internal
#' @noRd
.rtuiReqTruth <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }

  if (inherits(x, "try-error")) {
    return(FALSE)
  }

  if (is.logical(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(x))
  }

  if (is.character(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(nzchar(x)))
  }

  if (is.numeric(x) || is.integer(x) || is.complex(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(x != 0))
  }

  if (is.raw(x)) {
    return(length(x) > 0L)
  }

  if (inherits(x, "data.frame")) {
    return(nrow(x) > 0L)
  }

  if (is.list(x)) {
    return(length(x) > 0L)
  }

  if (is.factor(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(nzchar(as.character(x))))
  }

  if (length(x) == 0L) {
    return(FALSE)
  }

  TRUE
}

#' Internal helper `.rtuiReqAbort`.
#'
#' Raises an internal req condition to abort the current reactive/render computation.
#'
#' @param message Optional message attached to the req-abort condition.
#'
#' @return This function always raises an error and does not return.
#'
#' @keywords internal
#' @noRd
.rtuiReqAbort <- function(message = NULL) {
  condition <- structure(
    list(message = message),
    class = c("rtui_req_error", "error", "condition")
  )
  stop(condition)
}

#' Internal helper `.rtuiReqDefaultValue`.
#'
#' Creates the sentinel object used when a req failure should yield a default.
#'
#' @return An object of class `"rtuiReqDefault"`.
#'
#' @keywords internal
#' @noRd
.rtuiReqDefaultValue <- function() {
  structure(list(), class = "rtuiReqDefault")
}

#' Internal helper `.rtuiReqPropagation`.
#'
#' Wraps an error so req failures can be propagated after cleanup.
#'
#' @param error Error object to carry through req propagation.
#'
#' @return An object of class `"rtuiReqPropagation"` containing `error`.
#'
#' @keywords internal
#' @noRd
.rtuiReqPropagation <- function(error) {
  structure(list(error = error), class = "rtuiReqPropagation")
}

#' Internal helper `.rtuiHandleReqCondition`.
#'
#' Handles req errors by reusing previous values when available and otherwise propagating.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param reactiveId Internal identifier of a reactive node.
#' @param err Captured `rtui_req_error` condition.
#' @param cacheKey Optional cache key used in `runtime$currentReactiveCache`.
#'
#' @return Existing cached value when available, otherwise an `rtuiReqPropagation` wrapper.
#'
#' @keywords internal
#' @noRd
.rtuiHandleReqCondition <- function(runtime, reactiveId, err, cacheKey = NULL) {
  current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
  if (isTRUE(current$hasValue)) {
    current$dirty <- FALSE
    assign(reactiveId, current, envir = runtime$reactiveState)
    .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
    if (!is.null(cacheKey)) {
      assign(cacheKey, current$value, envir = runtime$currentReactiveCache)
    }
    return(current$value)
  }

  .rtuiReactiveStoreMarkDirty(runtime, reactiveId, dirty = FALSE)
  .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
  .rtuiReqPropagation(err)
}

#' Require values to be available/truthy in reactive code
#'
#' Stops the current reactive/render evaluation when any supplied value is not
#' considered available (falsy), similarly to `shiny::req()`.
#'
#' @param ... Values to validate.
#' @param cancelOutput Ignored for now. Present for API compatibility.
#'
#' @return Invisibly returns the first supplied value when all values are truthy.
#'
#' @export
tuiReq <- function(..., cancelOutput = FALSE) {
  if (!is.logical(cancelOutput) || length(cancelOutput) != 1L || is.na(cancelOutput)) {
    stop("`cancelOutput` must be TRUE or FALSE.")
  }

  values <- list(...)
  if (length(values) == 0L) {
    .rtuiReqAbort()
  }

  for (value in values) {
    if (!isTRUE(.rtuiReqTruth(value))) {
      .rtuiReqAbort()
    }
  }

  invisible(values[[1L]])
}
