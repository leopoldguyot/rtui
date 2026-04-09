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

  if (identical(value$kind, "table")) {
    if (inherits(rendered, "rtuiReqDefault")) {
      return(.rtuiSerializeTableOutput(NULL))
    }
    return(.rtuiSerializeTableOutput(rendered, rowNames = isTRUE(value$rowNames)))
  }

  value
}

#' Internal helper `.rtuiSerializeTableOutput`.
#'
#' Converts a data frame-like value into the serialized structure consumed by
#' the C++ table renderer.
#'
#' @param value Value returned by a `tuiRenderTable()` expression.
#' @param rowNames Whether row names should be prepended as a first column.
#'
#' @return A list with `columns` and `rows` fields.
#'
#' @keywords internal
#' @noRd
.rtuiSerializeTableOutput <- function(value, rowNames = FALSE) {
  empty <- list(columns = character(), rows = list())

  if (is.null(value) || length(value) == 0L) {
    return(empty)
  }
  if (!is.data.frame(value)) {
    stop("`tuiRenderTable()` expression must return a data.frame.")
  }

  tableData <- value
  if (isTRUE(rowNames)) {
    rn <- rownames(tableData)
    if (is.null(rn)) {
      rn <- as.character(seq_len(nrow(tableData)))
    }
    tableData <- data.frame(
      `(row)` = as.character(rn),
      tableData,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  columns <- names(tableData)
  if (is.null(columns)) {
    columns <- rep("", ncol(tableData))
  }
  columns <- as.character(columns)

  rowCount <- nrow(tableData)
  if (rowCount == 0L) {
    return(list(columns = columns, rows = list()))
  }

  if (ncol(tableData) == 0L) {
    return(list(columns = columns, rows = rep(list(character()), rowCount)))
  }

  columnValues <- lapply(tableData, function(column) {
    values <- as.character(column)
    values[is.na(values)] <- "NA"
    values
  })

  rows <- vector("list", rowCount)
  for (rowIndex in seq_len(rowCount)) {
    rows[[rowIndex]] <- unname(
      vapply(
        columnValues,
        function(column) column[[rowIndex]],
        character(1)
      )
    )
  }

  list(columns = columns, rows = rows)
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

#' Create a data-frame table renderer for `output$...`
#'
#' @param expr An expression returning a data frame (or tibble).
#' @param rowNames A single logical value. If `TRUE`, row names are added as
#'   the first column.
#'
#' @details
#' Renderer expressions should ideally be side-effect free. If a `tuiRender*`
#' expression mutates reactive state (for example via `tuiReactiveVal()`), the
#' output can invalidate itself during its own evaluation. In that case, `rtui`
#' emits a warning (once per output id) and still completes the render pass.
#'
#' @return A table renderer object for assignment in `output$...`.
#'
#' @export
tuiRenderTable <- function(expr, rowNames = FALSE) {
  if (!is.logical(rowNames) || length(rowNames) != 1L || is.na(rowNames)) {
    stop("`rowNames` must be TRUE or FALSE.")
  }

  structure(
    list(
      kind = "table",
      expr = substitute(expr),
      env = parent.frame(),
      rowNames = isTRUE(rowNames)
    ),
    class = "rtuiRenderer"
  )
}
