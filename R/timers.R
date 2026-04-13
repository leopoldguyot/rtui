#' Internal helper `.rtuiNormalizeTimerIntervalMs`.
#'
#' Validates a timer interval argument in milliseconds.
#'
#' @param intervalMs Candidate interval in milliseconds.
#'
#' @return Positive integer milliseconds.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeTimerIntervalMs <- function(intervalMs) {
  if (!is.numeric(intervalMs) || length(intervalMs) != 1L || is.na(intervalMs) || !is.finite(intervalMs)) {
    stop("`intervalMs` must be a single positive numeric value.")
  }
  if (intervalMs <= 0) {
    stop("`intervalMs` must be a single positive numeric value.")
  }
  as.integer(ceiling(intervalMs))
}

#' Internal helper `.rtuiRegisterTimer`.
#'
#' Registers a timer callback in the current runtime scheduler.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param intervalMs Timer interval in milliseconds.
#' @param onTick Function called with an integer `step` count when timer fires.
#'
#' @return Internal timer id.
#'
#' @keywords internal
#' @noRd
.rtuiRegisterTimer <- function(runtime, intervalMs, onTick) {
  if (!is.function(onTick)) {
    stop("`onTick` must be a function.")
  }

  intervalMs <- .rtuiNormalizeTimerIntervalMs(intervalMs)
  tickMs <- as.integer(runtime$timerTickMs)
  if (!is.finite(tickMs) || tickMs <= 0L) {
    tickMs <- 50L
  }
  intervalTicks <- max(1L, as.integer(ceiling(intervalMs / tickMs)))
  currentTick <- suppressWarnings(as.integer(runtime$currentInputState[[.rtuiTimerTickId]]))
  if (is.na(currentTick)) {
    currentTick <- 0L
  }

  runtime$timerIndex <- runtime$timerIndex + 1L
  timerId <- paste0("timer_", runtime$timerIndex)
  runtime$timers[[timerId]] <- list(
    intervalTicks = intervalTicks,
    lastTick = currentTick,
    onTick = onTick
  )

  timerId
}

#' Internal helper `.rtuiProcessTimers`.
#'
#' Processes due timers for the current scheduler tick.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return Invisibly returns `TRUE` when at least one timer fired.
#'
#' @keywords internal
#' @noRd
.rtuiProcessTimers <- function(runtime) {
  if (is.null(runtime$timers) || !is.list(runtime$timers) || length(runtime$timers) == 0L) {
    return(invisible(FALSE))
  }

  currentTick <- suppressWarnings(as.integer(runtime$currentInputState[[.rtuiTimerTickId]]))
  if (is.na(currentTick)) {
    currentTick <- 0L
  }

  didFire <- FALSE
  timerIds <- names(runtime$timers)
  for (timerId in timerIds) {
    timer <- runtime$timers[[timerId]]
    if (is.null(timer)) {
      next
    }
    intervalTicks <- suppressWarnings(as.integer(timer$intervalTicks))
    lastTick <- suppressWarnings(as.integer(timer$lastTick))
    if (is.na(intervalTicks) || intervalTicks < 1L) {
      intervalTicks <- 1L
    }
    if (is.na(lastTick)) {
      lastTick <- currentTick
    }

    elapsed <- currentTick - lastTick
    if (elapsed < intervalTicks) {
      timer$intervalTicks <- intervalTicks
      timer$lastTick <- lastTick
      runtime$timers[[timerId]] <- timer
      next
    }

    step <- as.integer(elapsed %/% intervalTicks)
    if (step <= 0L) {
      timer$intervalTicks <- intervalTicks
      timer$lastTick <- lastTick
      runtime$timers[[timerId]] <- timer
      next
    }

    timer$intervalTicks <- intervalTicks
    timer$lastTick <- lastTick + step * intervalTicks
    runtime$timers[[timerId]] <- timer

    .rtuiWithCaptureSuspended(runtime, {
      timer$onTick(step)
    })
    didFire <- TRUE
  }

  invisible(didFire)
}

#' Create a timer-driven reactive value
#'
#' Creates a reactive object that increments at a fixed interval.
#'
#' @param intervalMs Timer interval in milliseconds (must be > 0).
#'
#' @return A reactive object (function) to be called with `()`.
#'
#' @export
tuiReactiveTimer <- function(intervalMs = 1000L) {
  intervalMs <- .rtuiNormalizeTimerIntervalMs(intervalMs)
  runtime <- .rtuiCurrentRuntime()
  ticks <- tuiReactiveVal(0L)

  .rtuiRegisterTimer(runtime, intervalMs, function(step) {
    current <- suppressWarnings(as.integer(ticks()))
    if (is.na(current)) {
      current <- 0L
    }
    ticks(current + as.integer(step))
    invisible(NULL)
  })

  ticks
}
