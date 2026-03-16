.rtuiRuntimeContext <- new.env(parent = emptyenv())
.rtuiRuntimeContext$eventId <- NULL

.rtuiExtractMemberId <- function(expr, rootName) {
  isDollarAccess <- is.call(expr) &&
    identical(expr[[1L]], as.name("$")) &&
    length(expr) == 3L &&
    is.symbol(expr[[2L]]) &&
    identical(as.character(expr[[2L]]), rootName) &&
    is.symbol(expr[[3L]])
  if (isDollarAccess)
    return(as.character(expr[[3L]]))

  isBracketAccess <- is.call(expr) &&
    identical(expr[[1L]], as.name("[[")) &&
    length(expr) == 3L &&
    is.symbol(expr[[2L]]) &&
    identical(as.character(expr[[2L]]), rootName) &&
    is.character(expr[[3L]]) &&
    length(expr[[3L]]) == 1L &&
    !is.na(expr[[3L]])
  if (isBracketAccess)
    return(expr[[3L]])

  NULL
}

#' Create a TUI application
#'
#' Defines a terminal UI application from a UI tree and a server function.
#' The runtime automatically manages `input` and `output` state, similarly to
#' Shiny's conceptual model.
#'
#' @param ui A UI component tree built with [tuiColumn()], [tuiRow()],
#'   [tuiRenderText()], [tuiInputButton()], or [tuiInputText()].
#' @param server A function called as `server(input, output)`. Both `input`
#'   and `output` are environments:
#'   - `input$<id>` is updated automatically from buttons and text inputs.
#'   - assign to `output$<key>` to update values displayed by
#'     [tuiRenderText()].
#'   - use [tuiObserveEvent()] to run updates only for specific input events,
#'     optionally with `runAtInit = TRUE`.
#'
#' @return An object of class `rtuiApp`.
#'
#' @examples
#' app <- tuiApp(
#'   ui = tuiColumn(
#'     tuiRenderText(output$counter),
#'     tuiInputButton("Increment", id = "inc")
#'   ),
#'   server = function(input, output) {
#'     output$counter <- input$inc
#'   }
#' )
#'
#' @export
tuiApp <- function(ui, server) {
  if (!inherits(ui, "rtuiComponent"))
    stop("`ui` must be a rtuiComponent (built with tuiColumn, tuiRow, etc.).")
  if (!is.function(server))
    stop("`server` must be a function called as `server(input, output)`.")

  meta <- .rtuiCollectUiMeta(ui)
  overlapIds <- intersect(meta$buttonIds, meta$textInputIds)
  if (length(overlapIds) > 0L) {
    stop(
      "Component ids must be unique across buttons and text inputs. Duplicates: ",
      paste(overlapIds, collapse = ", ")
    )
  }

  inputState <- .rtuiInitialInput(meta)
  outputState <- .rtuiInitialOutput(meta)

  state <- list(
    input = inputState,
    output = .rtuiRunServer(server, inputState, outputState)
  )

  handlerIds <- unique(c(meta$buttonIds, meta$textInputIds))
  handlers <- stats::setNames(vector("list", length(handlerIds)), handlerIds)
  for (id in handlerIds) {
    handlers[[id]] <- local({
      currentEventId <- id
      function(state) {
        state$output <- .rtuiRunServer(
          server = server,
          inputState = state$input,
          outputState = state$output,
          eventId = currentEventId
        )
        state
      }
    })
  }

  structure(
    list(state = state, ui = ui, handlers = handlers),
    class = "rtuiApp"
  )
}

.rtuiCollectUiMeta <- function(node) {
  outputKeys <- character()
  buttonIds <- character()
  textInputIds <- character()
  textInputDefaults <- list()

  walk <- function(x) {
    type <- x$type

    if (identical(type, "text"))
      outputKeys <<- c(outputKeys, x$key)
    if (identical(type, "button"))
      buttonIds <<- c(buttonIds, x$id)
    if (identical(type, "input")) {
      textInputIds <<- c(textInputIds, x$id)
      defaultValue <- if (is.null(x$value)) "" else x$value
      textInputDefaults[[x$id]] <<- defaultValue
    }

    if (!is.null(x$children)) {
      for (child in x$children)
        walk(child)
    }
  }

  walk(node)

  textInputIds <- unique(textInputIds)

  list(
    outputKeys = unique(outputKeys),
    buttonIds = unique(buttonIds),
    textInputIds = textInputIds,
    textInputDefaults = textInputDefaults[textInputIds]
  )
}

.rtuiInitialInput <- function(meta) {
  ids <- c(meta$buttonIds, meta$textInputIds)
  input <- stats::setNames(vector("list", length(ids)), ids)

  for (id in meta$buttonIds)
    input[[id]] <- 0L
  textDefaults <- if (is.null(meta$textInputDefaults)) list() else meta$textInputDefaults
  for (id in meta$textInputIds)
    input[[id]] <- if (!is.null(textDefaults[[id]])) textDefaults[[id]] else ""

  input
}

.rtuiInitialOutput <- function(meta) {
  output <- stats::setNames(
    vector("list", length(meta$outputKeys)),
    meta$outputKeys
  )
  for (key in meta$outputKeys)
    output[[key]] <- ""
  output
}

.rtuiRunServer <- function(server, inputState, outputState, eventId = NULL) {
  oldEventId <- .rtuiRuntimeContext$eventId
  .rtuiRuntimeContext$eventId <- eventId
  on.exit({
    .rtuiRuntimeContext$eventId <- oldEventId
  }, add = TRUE)

  input <- list2env(inputState, parent = emptyenv())
  output <- list2env(outputState, parent = emptyenv())

  server(input, output)

  as.list(output, all.names = TRUE)
}

#' Run code only for selected input events
#'
#' Evaluates `expr` only when the current server execution was triggered by one
#' of the given input ids. This is useful to isolate updates so they run only
#' after explicit actions (for example an "Apply" button).
#'
#' @param event An `input$<id>` or `input[["id"]]` reference.
#' @param expr An expression evaluated only when `event` matches the triggering
#'   input id.
#' @param runAtInit A single logical value. If `TRUE`, run `expr` once during
#'   app initialization before any input event.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' app <- tuiApp(
#'   ui = tuiColumn(
#'     tuiRenderText(output$name),
#'     tuiInputButton("Apply name", id = "applyName"),
#'     tuiInputText("nameInput", value = "John")
#'   ),
#'   server = function(input, output) {
#'     tuiObserveEvent(input$applyName, runAtInit = TRUE, {
#'       output$name <- input$nameInput
#'     })
#'   }
#' )
#'
#' @export
tuiObserveEvent <- function(event, expr, runAtInit = FALSE) {
  eventExpr <- substitute(event)
  eventId <- .rtuiExtractMemberId(eventExpr, "input")
  if (is.null(eventId))
    stop("`event` must be an `input$<id>` or `input[[\"id\"]]` reference.")

  if (!is.logical(runAtInit) || length(runAtInit) != 1L || is.na(runAtInit))
    stop("`runAtInit` must be TRUE or FALSE.")

  currentEventId <- .rtuiRuntimeContext$eventId
  isMatchingEvent <- !is.null(currentEventId) && identical(currentEventId, eventId)
  isInitRun <- is.null(currentEventId) && isTRUE(runAtInit)

  if (isMatchingEvent || isInitRun)
    eval.parent(substitute(expr))

  invisible(NULL)
}

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
  if (!inherits(app, "rtuiApp"))
    stop("`app` must be an `rtuiApp` created by `tuiApp()`.")

  runTuiApp(app$ui, app$state, app$handlers)
  invisible(NULL)
}

#' @export
print.rtuiApp <- function(x, ...) {
  cat("<rtuiApp>\n")
  cat("  input ids: ", paste(names(x$state$input), collapse = ", "), "\n")
  cat("  output ids:", paste(names(x$state$output), collapse = ", "), "\n")
  cat("  handlers:  ", paste(names(x$handlers), collapse = ", "), "\n")
  invisible(x)
}
