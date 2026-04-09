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
    showRowNames <- if (!is.null(value$showRowNames)) {
      isTRUE(value$showRowNames)
    } else {
      isTRUE(value$rowNames)
    }
    showHeader <- if (is.null(value$showHeader)) TRUE else isTRUE(value$showHeader)
    outerBorder <- if (is.null(value$outerBorder)) TRUE else isTRUE(value$outerBorder)
    rowBorder <- if (is.null(value$rowBorder)) FALSE else isTRUE(value$rowBorder)
    colBorder <- if (is.null(value$colBorder)) TRUE else isTRUE(value$colBorder)
    headerRowBorder <- if (is.null(value$headerRowBorder)) {
      if (is.null(value$headerBorder)) TRUE else isTRUE(value$headerBorder)
    } else {
      isTRUE(value$headerRowBorder)
    }
    headerColBorder <- if (is.null(value$headerColBorder)) {
      if (is.null(value$headerBorder)) TRUE else isTRUE(value$headerBorder)
    } else {
      isTRUE(value$headerColBorder)
    }
    borderStyle <- if (is.null(value$borderStyle)) "light" else value$borderStyle
    headerBold <- if (is.null(value$headerBold)) TRUE else isTRUE(value$headerBold)
    zebraRows <- if (is.null(value$zebraRows)) FALSE else isTRUE(value$zebraRows)
    cellPaddingX <- if (is.null(value$cellPaddingX)) 0L else value$cellPaddingX
    cellPaddingY <- if (is.null(value$cellPaddingY)) 0L else value$cellPaddingY
    cellOverflow <- if (is.null(value$cellOverflow)) "clip" else value$cellOverflow
    naText <- if (is.null(value$naText)) "NA" else value$naText

    if (inherits(rendered, "rtuiReqDefault")) {
      return(.rtuiSerializeTableOutput(
        NULL,
        showRowNames = showRowNames,
        showHeader = showHeader,
        outerBorder = outerBorder,
        rowBorder = rowBorder,
        colBorder = colBorder,
        headerRowBorder = headerRowBorder,
        headerColBorder = headerColBorder,
        borderStyle = borderStyle,
        borderColor = value$borderColor,
        headerBorderColor = value$headerBorderColor,
        headerBold = headerBold,
        headerColor = value$headerColor,
        headerBgColor = value$headerBgColor,
        zebraRows = zebraRows,
        zebraColorOdd = value$zebraColorOdd,
        zebraColorEven = value$zebraColorEven,
        cellPaddingX = cellPaddingX,
        cellPaddingY = cellPaddingY,
        cellOverflow = cellOverflow,
        minColWidth = value$minColWidth,
        maxColWidth = value$maxColWidth,
        columnAlign = value$columnAlign,
        naText = naText
      ))
    }
    return(.rtuiSerializeTableOutput(
      rendered,
      showRowNames = showRowNames,
      showHeader = showHeader,
      outerBorder = outerBorder,
      rowBorder = rowBorder,
      colBorder = colBorder,
      headerRowBorder = headerRowBorder,
      headerColBorder = headerColBorder,
      borderStyle = borderStyle,
      borderColor = value$borderColor,
      headerBorderColor = value$headerBorderColor,
      headerBold = headerBold,
      headerColor = value$headerColor,
      headerBgColor = value$headerBgColor,
      zebraRows = zebraRows,
      zebraColorOdd = value$zebraColorOdd,
      zebraColorEven = value$zebraColorEven,
      cellPaddingX = cellPaddingX,
      cellPaddingY = cellPaddingY,
      cellOverflow = cellOverflow,
      minColWidth = value$minColWidth,
      maxColWidth = value$maxColWidth,
      columnAlign = value$columnAlign,
      naText = naText
    ))
  }

  value
}

#' Internal helper `.rtuiSerializeTableOutput`.
#'
#' Converts a data frame-like value into the serialized structure consumed by
#' the C++ table renderer.
#'
#' @param value Value returned by a `tuiRenderTable()` expression.
#' @param showRowNames Whether row names should be prepended as a first column.
#' @param showHeader Whether the table header row should be rendered.
#' @param outerBorder Whether to draw the outer table border.
#' @param rowBorder,colBorder Border toggles for row and column separators.
#' @param headerRowBorder,headerColBorder Header-only separator toggles that are
#'   applied only when the corresponding global separator is disabled
#'   (`rowBorder = FALSE` and `colBorder = FALSE` respectively).
#' @param borderStyle Border style name.
#' @param borderColor,headerBorderColor Optional border colors.
#' @param headerBold Whether to render header cells in bold.
#' @param headerColor,headerBgColor Optional header text/background colors.
#' @param zebraRows Whether alternate row coloring is enabled.
#' @param zebraColorOdd,zebraColorEven Optional zebra background colors.
#' @param cellPaddingX,cellPaddingY Cell horizontal/vertical padding.
#' @param cellOverflow Cell overflow policy (`"clip"`, `"wrap"`, `"ellipsis"`).
#' @param minColWidth,maxColWidth Optional minimum/maximum column width.
#' @param columnAlign Optional per-column alignment specification.
#' @param naText Replacement text for `NA` cell values.
#'
#' @return A list with `columns`, `rows`, and `options` fields.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeTableLogical <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", arg, "` must be TRUE or FALSE.")
  }
  isTRUE(value)
}

#' @keywords internal
#' @noRd
.rtuiNormalizeTableColumnAlign <- function(columnAlign, columns) {
  columnCount <- length(columns)
  if (columnCount == 0L) {
    return(character())
  }
  if (is.null(columnAlign)) {
    return(rep("left", columnCount))
  }

  if (is.list(columnAlign)) {
    if (is.null(names(columnAlign))) {
      columnAlign <- unlist(columnAlign, use.names = FALSE)
    } else {
      columnAlign <- unlist(columnAlign, use.names = TRUE)
    }
  }
  if (!is.character(columnAlign)) {
    stop("`columnAlign` must be NULL, a character vector, or a named list.")
  }
  if (length(columnAlign) == 0L || any(is.na(columnAlign))) {
    stop("`columnAlign` entries must be non-empty alignment strings.")
  }

  normalized <- tolower(trimws(columnAlign))
  allowed <- c("left", "center", "right")
  if (!all(normalized %in% allowed)) {
    stop("`columnAlign` entries must be one of ", paste(shQuote(allowed), collapse = ", "), ".")
  }

  if (length(normalized) == 1L && is.null(names(normalized))) {
    return(rep(normalized, columnCount))
  }

  providedNames <- names(normalized)
  if (!is.null(providedNames) && any(nzchar(providedNames))) {
    resolved <- rep("left", columnCount)
    for (i in seq_along(normalized)) {
      columnName <- providedNames[[i]]
      if (!nzchar(columnName)) {
        stop("Named `columnAlign` entries must include a column name for every value.")
      }
      index <- match(columnName, columns)
      if (is.na(index)) {
        stop("`columnAlign` includes unknown column name `", columnName, "`.")
      }
      resolved[[index]] <- normalized[[i]]
    }
    return(resolved)
  }

  if (length(normalized) != columnCount) {
    stop("`columnAlign` must have length 1, match the number of columns, or be a named vector/list.")
  }
  unname(normalized)
}

#' @keywords internal
#' @noRd
.rtuiSerializeTableOutput <- function(
    value,
    showRowNames = FALSE,
    showHeader = TRUE,
    outerBorder = TRUE,
    rowBorder = FALSE,
    colBorder = TRUE,
    headerRowBorder = TRUE,
    headerColBorder = TRUE,
    borderStyle = "light",
    borderColor = NULL,
    headerBorderColor = NULL,
    headerBold = TRUE,
    headerColor = NULL,
    headerBgColor = NULL,
    zebraRows = FALSE,
    zebraColorOdd = NULL,
    zebraColorEven = NULL,
    cellPaddingX = 0L,
    cellPaddingY = 0L,
    cellOverflow = "clip",
    minColWidth = NULL,
    maxColWidth = NULL,
    columnAlign = NULL,
    naText = "NA"
) {
  showRowNames <- .rtuiNormalizeTableLogical(showRowNames, "showRowNames")
  showHeader <- .rtuiNormalizeTableLogical(showHeader, "showHeader")
  outerBorder <- .rtuiNormalizeTableLogical(outerBorder, "outerBorder")
  rowBorder <- .rtuiNormalizeTableLogical(rowBorder, "rowBorder")
  colBorder <- .rtuiNormalizeTableLogical(colBorder, "colBorder")
  headerRowBorder <- .rtuiNormalizeTableLogical(headerRowBorder, "headerRowBorder")
  headerColBorder <- .rtuiNormalizeTableLogical(headerColBorder, "headerColBorder")
  headerBold <- .rtuiNormalizeTableLogical(headerBold, "headerBold")
  zebraRows <- .rtuiNormalizeTableLogical(zebraRows, "zebraRows")

  borderStyle <- .rtuiNormalizeOverflowChoice(
    borderStyle,
    "borderStyle",
    c("light", "dashed", "heavy", "double", "rounded", "empty")
  )
  borderColor <- .rtuiNormalizeColor(borderColor)
  headerBorderColor <- .rtuiNormalizeColor(headerBorderColor)
  headerColor <- .rtuiNormalizeColor(headerColor)
  headerBgColor <- .rtuiNormalizeColor(headerBgColor)
  zebraColorOdd <- .rtuiNormalizeColor(zebraColorOdd)
  zebraColorEven <- .rtuiNormalizeColor(zebraColorEven)

  cellPaddingX <- .rtuiNormalizeSizeInteger(cellPaddingX, "cellPaddingX")
  cellPaddingY <- .rtuiNormalizeSizeInteger(cellPaddingY, "cellPaddingY")
  minColWidth <- .rtuiNormalizeSizeInteger(minColWidth, "minColWidth")
  maxColWidth <- .rtuiNormalizeSizeInteger(maxColWidth, "maxColWidth")
  if (is.null(cellPaddingX)) cellPaddingX <- 0L
  if (is.null(cellPaddingY)) cellPaddingY <- 0L
  if (!is.null(minColWidth) && !is.null(maxColWidth) && minColWidth > maxColWidth) {
    stop("`minColWidth` must be less than or equal to `maxColWidth`.")
  }

  cellOverflow <- .rtuiNormalizeOverflowChoice(
    cellOverflow,
    "cellOverflow",
    c("clip", "wrap", "ellipsis")
  )
  if (!is.character(naText) || length(naText) != 1L || is.na(naText)) {
    stop("`naText` must be a single character string.")
  }

  columns <- character()
  rows <- list()
  if (!is.null(value) && length(value) != 0L) {
    if (!is.data.frame(value)) {
      stop("`tuiRenderTable()` expression must return a data.frame.")
    }

    tableData <- value
    if (isTRUE(showRowNames)) {
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
    if (rowCount > 0L) {
      if (ncol(tableData) == 0L) {
        rows <- rep(list(character()), rowCount)
      } else {
        columnValues <- lapply(tableData, function(column) {
          values <- as.character(column)
          values[is.na(values)] <- naText
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
      }
    }
  }

  options <- list(
    showHeader = showHeader,
    outerBorder = outerBorder,
    rowBorder = rowBorder,
    colBorder = colBorder,
    headerRowBorder = headerRowBorder,
    headerColBorder = headerColBorder,
    borderStyle = borderStyle,
    borderColor = borderColor,
    headerBorderColor = headerBorderColor,
    headerBold = headerBold,
    headerColor = headerColor,
    headerBgColor = headerBgColor,
    zebraRows = zebraRows,
    zebraColorOdd = zebraColorOdd,
    zebraColorEven = zebraColorEven,
    cellPaddingX = as.integer(cellPaddingX),
    cellPaddingY = as.integer(cellPaddingY),
    cellOverflow = cellOverflow,
    minColWidth = minColWidth,
    maxColWidth = maxColWidth,
    columnAlign = .rtuiNormalizeTableColumnAlign(columnAlign, columns)
  )
  options <- options[!vapply(options, is.null, logical(1))]

  list(columns = columns, rows = rows, options = options)
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
#' @param showRowNames Whether to prepend row names as the first column.
#' @param showHeader Whether the header row should be shown.
#' @param outerBorder Whether to draw the table outer border.
#' @param rowBorder,colBorder Whether to draw row and column separators.
#' @param headerRowBorder,headerColBorder Header-only separators. They are
#'   applied only when the corresponding global separator is disabled:
#'   `headerRowBorder` only when `rowBorder = FALSE`, and `headerColBorder`
#'   only when `colBorder = FALSE`.
#' @param borderStyle Border style (`"light"`, `"dashed"`, `"heavy"`,
#'   `"double"`, `"rounded"`, `"empty"`).
#' @param borderColor,headerBorderColor Optional border colors.
#' @param headerBold Whether header cells are bold.
#' @param headerColor,headerBgColor Optional header text/background colors.
#' @param zebraRows Whether alternating row backgrounds are enabled.
#' @param zebraColorOdd,zebraColorEven Optional alternating row background
#'   colors.
#' @param cellPaddingX,cellPaddingY Cell horizontal/vertical padding.
#' @param cellOverflow Cell overflow policy (`"clip"`, `"wrap"`, `"ellipsis"`).
#' @param minColWidth,maxColWidth Optional global min/max column width.
#' @param columnAlign Optional per-column alignment (`"left"`, `"center"`,
#'   `"right"`), as a scalar, vector, or named vector/list.
#' @param naText Replacement text for `NA` values in table cells.
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
tuiRenderTable <- function(
    expr,
    showRowNames = FALSE,
    showHeader = TRUE,
    outerBorder = TRUE,
    rowBorder = FALSE,
    colBorder = TRUE,
    headerRowBorder = TRUE,
    headerColBorder = TRUE,
    borderStyle = "light",
    borderColor = NULL,
    headerBorderColor = NULL,
    headerBold = TRUE,
    headerColor = NULL,
    headerBgColor = NULL,
    zebraRows = FALSE,
    zebraColorOdd = NULL,
    zebraColorEven = NULL,
    cellPaddingX = 0L,
    cellPaddingY = 0L,
    cellOverflow = "clip",
    minColWidth = NULL,
    maxColWidth = NULL,
    columnAlign = NULL,
    naText = "NA"
) {
  showRowNames <- .rtuiNormalizeTableLogical(showRowNames, "showRowNames")
  showHeader <- .rtuiNormalizeTableLogical(showHeader, "showHeader")
  outerBorder <- .rtuiNormalizeTableLogical(outerBorder, "outerBorder")
  rowBorder <- .rtuiNormalizeTableLogical(rowBorder, "rowBorder")
  colBorder <- .rtuiNormalizeTableLogical(colBorder, "colBorder")
  headerRowBorder <- .rtuiNormalizeTableLogical(headerRowBorder, "headerRowBorder")
  headerColBorder <- .rtuiNormalizeTableLogical(headerColBorder, "headerColBorder")
  headerBold <- .rtuiNormalizeTableLogical(headerBold, "headerBold")
  zebraRows <- .rtuiNormalizeTableLogical(zebraRows, "zebraRows")

  borderStyle <- .rtuiNormalizeOverflowChoice(
    borderStyle,
    "borderStyle",
    c("light", "dashed", "heavy", "double", "rounded", "empty")
  )
  borderColor <- .rtuiNormalizeColor(borderColor)
  headerBorderColor <- .rtuiNormalizeColor(headerBorderColor)
  headerColor <- .rtuiNormalizeColor(headerColor)
  headerBgColor <- .rtuiNormalizeColor(headerBgColor)
  zebraColorOdd <- .rtuiNormalizeColor(zebraColorOdd)
  zebraColorEven <- .rtuiNormalizeColor(zebraColorEven)

  cellPaddingX <- .rtuiNormalizeSizeInteger(cellPaddingX, "cellPaddingX")
  cellPaddingY <- .rtuiNormalizeSizeInteger(cellPaddingY, "cellPaddingY")
  if (is.null(cellPaddingX)) cellPaddingX <- 0L
  if (is.null(cellPaddingY)) cellPaddingY <- 0L

  cellOverflow <- .rtuiNormalizeOverflowChoice(
    cellOverflow,
    "cellOverflow",
    c("clip", "wrap", "ellipsis")
  )
  minColWidth <- .rtuiNormalizeSizeInteger(minColWidth, "minColWidth")
  maxColWidth <- .rtuiNormalizeSizeInteger(maxColWidth, "maxColWidth")
  if (!is.null(minColWidth) && !is.null(maxColWidth) && minColWidth > maxColWidth) {
    stop("`minColWidth` must be less than or equal to `maxColWidth`.")
  }

  if (!is.null(columnAlign) &&
      !is.character(columnAlign) &&
      !is.list(columnAlign)) {
    stop("`columnAlign` must be NULL, a character vector, or a named list.")
  }
  if (!is.character(naText) || length(naText) != 1L || is.na(naText)) {
    stop("`naText` must be a single character string.")
  }

  structure(
    list(
      kind = "table",
      expr = substitute(expr),
      env = parent.frame(),
      showRowNames = showRowNames,
      showHeader = showHeader,
      outerBorder = outerBorder,
      rowBorder = rowBorder,
      colBorder = colBorder,
      headerRowBorder = headerRowBorder,
      headerColBorder = headerColBorder,
      borderStyle = borderStyle,
      borderColor = borderColor,
      headerBorderColor = headerBorderColor,
      headerBold = headerBold,
      headerColor = headerColor,
      headerBgColor = headerBgColor,
      zebraRows = zebraRows,
      zebraColorOdd = zebraColorOdd,
      zebraColorEven = zebraColorEven,
      cellPaddingX = as.integer(cellPaddingX),
      cellPaddingY = as.integer(cellPaddingY),
      cellOverflow = cellOverflow,
      minColWidth = minColWidth,
      maxColWidth = maxColWidth,
      columnAlign = columnAlign,
      naText = naText
    ),
    class = "rtuiRenderer"
  )
}
