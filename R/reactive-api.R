#' Internal helper `.rtuiIsReactiveObject`.
#'
#' Checks whether a value is an internal reactive object function.
#'
#' @param x Value to inspect.
#'
#' @return `TRUE` if `x` is a reactive object function, otherwise `FALSE`.
#'
#' @keywords internal
#' @noRd
.rtuiIsReactiveObject <- function(x) {
  is.function(x) && inherits(x, "rtuiReactiveObject")
}

#' Internal helper `.rtuiEvalMaybeReactive`.
#'
#' Evaluates reactive objects and leaves non-reactive values unchanged.
#'
#' @param value Value to evaluate, resolve, or store.
#'
#' @return Evaluated reactive value, or `value` unchanged when non-reactive.
#'
#' @keywords internal
#' @noRd
.rtuiEvalMaybeReactive <- function(value) {
  if (.rtuiIsReactiveObject(value)) {
    return(value())
  }
  value
}

#' Create a reactive expression
#'
#' Creates a reactive object that re-evaluates `expr` when called during server
#' execution.
#'
#' @param expr An expression producing a reactive value.
#'
#' @return A reactive object (function) to be called with `()`.
#'
#' @export
tuiReactive <- function(expr) {
  exprSub <- substitute(expr)
  exprEnv <- parent.frame()
  reactiveId <- .rtuiNextReactiveId("reactive")

  reactiveObject <- function() {
    runtime <- .rtuiCurrentRuntime()
    .rtuiGraphEnsureReactiveNode(runtime, reactiveId, "reactive", hasValue = FALSE)
    .rtuiGraphRegisterRead(runtime, reactiveId)

    cacheKey <- .rtuiReactiveCacheKey("reactive", reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      return(get(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE))
    }

    current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
    if (isTRUE(current$hasValue) && !isTRUE(current$dirty)) {
      value <- current$value
      assign(cacheKey, value, envir = runtime$currentReactiveCache)
      return(value)
    }

    success <- FALSE
    .rtuiGraphBeginEvaluation(runtime, reactiveId)
    on.exit({
      .rtuiGraphEndEvaluation(runtime, reactiveId, success)
    }, add = TRUE)

    value <- tryCatch(
      eval(exprSub, envir = exprEnv),
      rtui_req_error = function(err) {
        .rtuiHandleReqCondition(runtime, reactiveId, err, cacheKey = cacheKey)
      }
    )
    if (inherits(value, "rtuiReqPropagation")) {
      success <- TRUE
      stop(value$error)
    }
    value <- .rtuiEvalMaybeReactive(value)
    if (inherits(value, "rtuiReqPropagation")) {
      success <- TRUE
      stop(value$error)
    }
    value <- .rtuiUpdateReactiveState(runtime, reactiveId, value)

    success <- TRUE
    assign(cacheKey, value, envir = runtime$currentReactiveCache)
    value
  }

  class(reactiveObject) <- c("rtuiReactiveObject", "rtuiReactive", class(reactiveObject))
  attr(reactiveObject, "rtuiId") <- reactiveId
  reactiveObject
}

#' Create a mutable reactive value
#'
#' Creates a reactive value container. Call without arguments to read, and with
#' one argument to update.
#'
#' @param value Initial value.
#'
#' @return A reactive value function.
#'
#' @export
tuiReactiveVal <- function(value = NULL) {
  runtime <- .rtuiCurrentRuntime()
  reactiveId <- .rtuiNextReactiveId("reactiveVal")

  .rtuiGraphEnsureReactiveNode(
    runtime,
    reactiveId,
    "reactiveVal",
    defaultValue = value,
    hasValue = TRUE
  )

  reactiveValue <- function(value) {
    runtime <- .rtuiCurrentRuntime()
    .rtuiGraphEnsureReactiveNode(runtime, reactiveId, "reactiveVal", hasValue = FALSE)
    current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
    cacheKey <- .rtuiReactiveCacheKey("reactiveVal", reactiveId)

    if (missing(value)) {
      .rtuiGraphRegisterRead(runtime, reactiveId)
      if (!exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
        assign(cacheKey, current$value, envir = runtime$currentReactiveCache)
      }
      return(current$value)
    }

    changed <- !isTRUE(current$hasValue) || !identical(current$value, value)
    .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE, dirty = FALSE)
    .rtuiSetReactiveChanged(runtime, reactiveId, changed, force = TRUE)
    assign(cacheKey, value, envir = runtime$currentReactiveCache)
    if (isTRUE(changed)) {
      .rtuiGraphInvalidateDependents(runtime, reactiveId)
    }
    invisible(value)
  }

  class(reactiveValue) <- c("rtuiReactiveObject", "rtuiReactiveVal", class(reactiveValue))
  attr(reactiveValue, "rtuiId") <- reactiveId
  reactiveValue
}

#' Observe reactive values
#'
#' Evaluates `expr` every server run.
#'
#' @param expr An expression to evaluate.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiObserve <- function(expr) {
  tryCatch(
    eval.parent(substitute(expr)),
    rtui_req_error = function(err) {
      invisible(NULL)
    }
  )
  invisible(NULL)
}

#' Evaluate an expression in isolation
#'
#' Evaluates `expr` without publishing reactive read-change information for the
#' current server cycle. This is useful when you want the current value of a
#' reactive object without making it act as an event trigger.
#'
#' @param expr An expression to evaluate.
#'
#' @return The evaluated value of `expr`.
#'
#' @export
tuiIsolate <- function(expr) {
  runtime <- .rtuiRuntimeContext$currentRuntime
  if (is.null(runtime)) {
    return(eval.parent(substitute(expr)))
  }

  runtime$currentIsolateDepth <- runtime$currentIsolateDepth + 1L
  on.exit({
    runtime$currentIsolateDepth <- max(0L, runtime$currentIsolateDepth - 1L)
  }, add = TRUE)

  eval.parent(substitute(expr))
}
