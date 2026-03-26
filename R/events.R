#' Internal helper `.rtuiExtractMemberId`.
#'
#' Extracts an id from expressions like `root$foo` or `root[["foo"]]`.
#'
#' @param expr Expression to evaluate or inspect.
#' @param rootName Root symbol name expected in member access (for example `"input"`).
#'
#' @return A single id string when `expr` matches the expected member access; otherwise `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiExtractMemberId <- function(expr, rootName) {
  isDollarAccess <- is.call(expr) &&
    identical(expr[[1L]], as.name("$")) &&
    length(expr) == 3L &&
    is.symbol(expr[[2L]]) &&
    identical(as.character(expr[[2L]]), rootName) &&
    is.symbol(expr[[3L]])
  if (isDollarAccess) {
    return(as.character(expr[[3L]]))
  }

  isBracketAccess <- is.call(expr) &&
    identical(expr[[1L]], as.name("[[")) &&
    length(expr) == 3L &&
    is.symbol(expr[[2L]]) &&
    identical(as.character(expr[[2L]]), rootName) &&
    is.character(expr[[3L]]) &&
    length(expr[[3L]]) == 1L &&
    !is.na(expr[[3L]])
  if (isBracketAccess) {
    return(expr[[3L]])
  }

  NULL
}

#' Internal helper `.rtuiResolveReactiveEventCall`.
#'
#' Resolves zero-argument reactive calls used as event specifications.
#'
#' @param eventExpr Quoted event expression supplied by the caller.
#' @param eventEnv Environment used to resolve `eventExpr`.
#'
#' @return Reactive object referenced by `eventExpr`, or `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiResolveReactiveEventCall <- function(eventExpr, eventEnv) {
  if (!is.call(eventExpr) || length(eventExpr) < 1L) {
    return(NULL)
  }

  calleeExpr <- eventExpr[[1L]]
  if (!is.symbol(calleeExpr)) {
    return(NULL)
  }

  calleeName <- as.character(calleeExpr)
  if (!exists(calleeName, envir = eventEnv, inherits = TRUE)) {
    return(NULL)
  }

  eventValue <- get(calleeName, envir = eventEnv, inherits = TRUE)
  if (!.rtuiIsReactiveObject(eventValue)) {
    return(NULL)
  }

  if (length(eventExpr) > 1L) {
    stop(
      "Reactive `event` calls must not include arguments. ",
      "Use a zero-argument call like `myReactive()`."
    )
  }

  eventValue
}

#' Internal helper `.rtuiResolveEventSpec`.
#'
#' Normalizes an event expression into an input or reactive spec.
#'
#' @param eventExpr Quoted event expression supplied by the caller.
#' @param eventEnv Environment used to resolve `eventExpr`.
#'
#' @return A normalized event-spec list with `type` and associated fields.
#'
#' @keywords internal
#' @noRd
.rtuiResolveEventSpec <- function(eventExpr, eventEnv) {
  inputId <- .rtuiExtractMemberId(eventExpr, "input")
  if (!is.null(inputId)) {
    return(list(type = "input", inputId = inputId))
  }

  eventValue <- .rtuiResolveReactiveEventCall(eventExpr, eventEnv)
  if (!is.null(eventValue)) {
    reactiveId <- attr(eventValue, "rtuiId", exact = TRUE)
    if (is.null(reactiveId) || !is.character(reactiveId) || length(reactiveId) != 1L) {
      stop("Reactive events must provide a valid internal reactive id.")
    }
    return(list(type = "reactive", object = eventValue, reactiveId = reactiveId))
  }

  eventValue <- eval(eventExpr, envir = eventEnv)
  if (.rtuiIsReactiveObject(eventValue)) {
    reactiveId <- attr(eventValue, "rtuiId", exact = TRUE)
    if (is.null(reactiveId) || !is.character(reactiveId) || length(reactiveId) != 1L) {
      stop("Reactive events must provide a valid internal reactive id.")
    }
    return(list(type = "reactive", object = eventValue, reactiveId = reactiveId))
  }

  stop(
    "`event` must be an `input$<id>` / `input[[\"id\"]]` reference, ",
    "or a reactive call/object created with `tuiReactive()`, ",
    "`tuiReactiveVal()`, or `tuiReactiveEvent()`."
  )
}

#' Internal helper `.rtuiShouldTriggerEvent`.
#'
#' Determines whether an event-controlled expression should run this cycle.
#'
#' @param eventSpec Normalized event specification list.
#' @param runAtInit Whether the event should also trigger during app initialization.
#'
#' @return `TRUE` when the event should trigger evaluation in this cycle.
#'
#' @keywords internal
#' @noRd
.rtuiShouldTriggerEvent <- function(eventSpec, runAtInit) {
  runtime <- .rtuiCurrentRuntime()
  isInitRun <- is.null(runtime$currentEventId)

  if (identical(eventSpec$type, "input")) {
    matchesInput <- !is.null(runtime$currentEventId) &&
      identical(runtime$currentEventId, eventSpec$inputId)
    return(matchesInput || (isInitRun && isTRUE(runAtInit)))
  }

  if (identical(eventSpec$type, "reactive")) {
    eventValue <- tryCatch(
      .rtuiWithCaptureEnabled(runtime, eventSpec$object()),
      rtui_req_error = function(err) {
        .rtuiReqDefaultValue()
      }
    )
    if (inherits(eventValue, "rtuiReqDefault")) {
      return(FALSE)
    }
    reactiveChanged <- .rtuiGetReactiveChanged(runtime, eventSpec$reactiveId)
    if (isInitRun) {
      return(isTRUE(runAtInit))
    }
    return(reactiveChanged)
  }

  FALSE
}

#' Internal helper `.rtuiReactiveEventDependencies`.
#'
#' Computes dependency ids tracked for reactive-event nodes.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param eventSpec Normalized event specification list.
#' @param runAtInit Whether the event should also trigger during app initialization.
#'
#' @return Unique character vector of dependency node ids.
#'
#' @keywords internal
#' @noRd
.rtuiReactiveEventDependencies <- function(runtime, eventSpec, runAtInit) {
  dependencies <- character()

  if (identical(eventSpec$type, "input")) {
    return(.rtuiInputNodeId(eventSpec$inputId))
  }

  if (!identical(eventSpec$type, "reactive")) {
    return(dependencies)
  }

  dependencies <- c(dependencies, eventSpec$reactiveId)

  if (!is.null(runtime$currentEventId) || isTRUE(runAtInit)) {
    eventDeps <- .rtuiGraphMapGet(runtime$graphDependencies, eventSpec$reactiveId)
    if (length(eventDeps) > 0L) {
      dependencies <- c(dependencies, eventDeps)
    }
  }

  unique(dependencies)
}

#' Create an event-driven reactive expression
#'
#' Creates a reactive object that updates only when `event` is triggered.
#'
#' @param event An `input$<id>` / `input[["id"]]` reference, or a reactive
#'   call/object from [tuiReactive()], [tuiReactiveVal()], or
#'   [tuiReactiveEvent()].
#' @param expr An expression evaluated when `event` triggers.
#' @param runAtInit A single logical value. If `TRUE`, evaluate once during app
#'   initialization.
#'
#' @return A reactive object (function) to be called with `()`.
#'
#' @export
tuiReactiveEvent <- function(event, expr, runAtInit = FALSE) {
  if (!is.logical(runAtInit) || length(runAtInit) != 1L || is.na(runAtInit)) {
    stop("`runAtInit` must be TRUE or FALSE.")
  }

  eventSpec <- .rtuiResolveEventSpec(substitute(event), parent.frame())
  exprSub <- substitute(expr)
  exprEnv <- parent.frame()
  reactiveId <- .rtuiNextReactiveId("reactiveEvent")

  reactiveObject <- function() {
    runtime <- .rtuiCurrentRuntime()
    .rtuiGraphEnsureReactiveNode(runtime, reactiveId, "reactiveEvent", hasValue = FALSE)
    .rtuiGraphRegisterRead(runtime, reactiveId)
    dependencyIds <- .rtuiReactiveEventDependencies(runtime, eventSpec, runAtInit)
    .rtuiGraphUpdateDependencies(runtime, reactiveId, dependencyIds)

    cacheKey <- .rtuiReactiveCacheKey("reactiveEvent", reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      return(get(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE))
    }

    current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
    if (!isTRUE(current$dirty) && isTRUE(current$hasValue)) {
      assign(cacheKey, current$value, envir = runtime$currentReactiveCache)
      return(current$value)
    }
    if (!isTRUE(current$dirty) && !isTRUE(current$hasValue)) {
      .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
      assign(cacheKey, NULL, envir = runtime$currentReactiveCache)
      return(NULL)
    }

    shouldTrigger <- .rtuiShouldTriggerEvent(eventSpec, runAtInit = runAtInit)
    deferredDirtyTrigger <- !isTRUE(shouldTrigger) &&
      isTRUE(current$dirty) &&
      isTRUE(current$hasValue)

    if (isTRUE(shouldTrigger) || isTRUE(deferredDirtyTrigger)) {
      success <- FALSE
      .rtuiGraphBeginEvaluation(runtime, reactiveId)
      on.exit({
        .rtuiGraphEndEvaluation(runtime, reactiveId, success)
      }, add = TRUE)

      .rtuiGraphMapSet(runtime$currentEvalDeps, reactiveId, dependencyIds)
      value <- tryCatch(
        .rtuiWithCaptureSuspended(runtime, eval(exprSub, envir = exprEnv)),
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
      return(value)
    }

    previous <- .rtuiReactiveStoreGet(runtime, reactiveId)
    .rtuiReactiveStoreMarkDirty(runtime, reactiveId, dirty = FALSE)
    .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
    value <- if (is.null(previous) || !isTRUE(previous$hasValue)) NULL else previous$value
    assign(cacheKey, value, envir = runtime$currentReactiveCache)
    value
  }

  class(reactiveObject) <- c("rtuiReactiveObject", "rtuiReactiveEvent", class(reactiveObject))
  attr(reactiveObject, "rtuiId") <- reactiveId
  reactiveObject
}

#' Observe selected input/reactive events
#'
#' Registers an observer that evaluates `expr` only when `event` is triggered.
#'
#' @param event An `input$<id>` / `input[["id"]]` reference, or a reactive
#'   call/object from [tuiReactive()], [tuiReactiveVal()], or
#'   [tuiReactiveEvent()].
#' @param expr An expression evaluated when `event` triggers.
#' @param runAtInit A single logical value. If `TRUE`, evaluate once during app
#'   initialization.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiObserveEvent <- function(event, expr, runAtInit = FALSE) {
  if (!is.logical(runAtInit) || length(runAtInit) != 1L || is.na(runAtInit)) {
    stop("`runAtInit` must be TRUE or FALSE.")
  }

  eventSpec <- .rtuiResolveEventSpec(substitute(event), parent.frame())
  .rtuiRegisterObserver(
    exprSub = substitute(expr),
    exprEnv = parent.frame(),
    type = "observeEvent",
    eventSpec = eventSpec,
    runAtInit = runAtInit
  )

  invisible(NULL)
}
