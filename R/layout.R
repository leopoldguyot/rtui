#' Internal helper `.rtuiValidateMainAxisPercentSum`.
#'
#' Ensures strict percentage children do not over-allocate the parent axis.
#'
#' @param children Child component list.
#' @param field Size field to inspect (`"widthPercent"` or `"heightPercent"`).
#' @param context Human-readable function context for error messages.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiValidateMainAxisPercentSum <- function(children, field, context) {
  percents <- vapply(
    children,
    function(component) {
      value <- component[[field, exact = TRUE]]
      if (is.null(value)) {
        return(NA_real_)
      }
      as.numeric(value)
    },
    numeric(1L)
  )

  total <- sum(percents, na.rm = TRUE)
  if (total > 1 + 1e-9) {
    stop(
      "Sum of `",
      field,
      "` values in ",
      context,
      " children must be <= 1."
    )
  }

  invisible(NULL)
}

#' Internal helper `.rtuiApplyContainerOverflowSpec`.
#'
#' Validates and attaches container overflow policy fields to a component node.
#'
#' @param component Component node list.
#' @param overflowX Horizontal overflow policy (`"visible"`, `"clip"`, `"scroll"`).
#' @param overflowY Vertical overflow policy (`"visible"`, `"clip"`, `"scroll"`).
#'
#' @return Updated component node list.
#'
#' @keywords internal
#' @noRd
.rtuiApplyContainerOverflowSpec <- function(
    component,
    overflowX = "visible",
    overflowY = "visible"
) {
  overflowX <- .rtuiNormalizeOverflowChoice(
    overflowX,
    "overflowX",
    c("visible", "clip", "scroll")
  )
  overflowY <- .rtuiNormalizeOverflowChoice(
    overflowY,
    "overflowY",
    c("visible", "clip", "scroll")
  )

  if (!identical(overflowX, "visible")) component$overflowX <- overflowX
  if (!identical(overflowY, "visible")) component$overflowY <- overflowY

  component
}

#' Vertical layout container
#'
#' Stacks child components vertically.
#'
#' @param ... Child components built with layout or component functions.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#' @param overflowX,overflowY Container overflow policy for each axis.
#'   Use `"visible"` (default) to keep normal flow, `"clip"` to crop child
#'   drawing to the container bounds on that axis, or `"scroll"` to enable a
#'   focus-driven scrollable viewport on that axis.
#'
#' @return A `rtuiComponent` list node of type `"column"`.
#'
#' @export
tuiColumn <- function(
    ...,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL,
    overflowX = "visible",
    overflowY = "visible"
) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtuiComponent"))
      stop("All children must be rtuiComponent objects.")
  })
  .rtuiValidateMainAxisPercentSum(children, "heightPercent", "tuiColumn()")

  component <- .rtuiApplySizeSpec(
    list(type = "column", children = children),
    width = width,
    height = height,
    minHeight = minHeight,
    maxHeight = maxHeight,
    widthPercent = widthPercent,
    heightPercent = heightPercent
  )
  component <- .rtuiApplyContainerOverflowSpec(
    component,
    overflowX = overflowX,
    overflowY = overflowY
  )

  structure(
    component,
    class = "rtuiComponent"
  )
}

#' Horizontal layout container
#'
#' Places child components side by side horizontally.
#'
#' @param ... Child components built with layout or component functions.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#' @param overflowX,overflowY Container overflow policy for each axis.
#'   Use `"visible"` (default) to keep normal flow, `"clip"` to crop child
#'   drawing to the container bounds on that axis, or `"scroll"` to enable a
#'   focus-driven scrollable viewport on that axis.
#'
#' @return A `rtuiComponent` list node of type `"row"`.
#'
#' @export
tuiRow <- function(
    ...,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL,
    overflowX = "visible",
    overflowY = "visible"
) {
  children <- list(...)
  lapply(children, function(c) {
    if (!inherits(c, "rtuiComponent"))
      stop("All children must be rtuiComponent objects.")
  })
  .rtuiValidateMainAxisPercentSum(children, "widthPercent", "tuiRow()")

  component <- .rtuiApplySizeSpec(
    list(type = "row", children = children),
    width = width,
    height = height,
    minHeight = minHeight,
    maxHeight = maxHeight,
    widthPercent = widthPercent,
    heightPercent = heightPercent
  )
  component <- .rtuiApplyContainerOverflowSpec(
    component,
    overflowX = overflowX,
    overflowY = overflowY
  )

  structure(
    component,
    class = "rtuiComponent"
  )
}

#' Conditionally render a child component by terminal size
#'
#' Wraps a single child component and shows it only when terminal dimensions
#' satisfy the specified constraints. This is useful for responsive breakpoints
#' (for example, hiding secondary panels on narrow terminals) while keeping a
#' declarative static UI tree.
#'
#' Conditions are evaluated against the **current terminal size** and rechecked
#' during rendering, so visibility updates automatically when the terminal is
#' resized.
#'
#' @param child A single `rtuiComponent` to conditionally render.
#' @param minTerminalWidth,maxTerminalWidth Optional minimum/maximum terminal
#'   width (in terminal cells) required to show `child`.
#' @param minTerminalHeight,maxTerminalHeight Optional minimum/maximum terminal
#'   height (in terminal cells) required to show `child`.
#'
#' @return A `rtuiComponent` list node of type `"showIf"`.
#'
#' @export
tuiShowIf <- function(
    child,
    minTerminalWidth = NULL,
    maxTerminalWidth = NULL,
    minTerminalHeight = NULL,
    maxTerminalHeight = NULL
) {
  if (!inherits(child, "rtuiComponent")) {
    stop("`child` must be a rtuiComponent object.")
  }

  minTerminalWidth <- .rtuiNormalizeSizeInteger(minTerminalWidth, "minTerminalWidth")
  maxTerminalWidth <- .rtuiNormalizeSizeInteger(maxTerminalWidth, "maxTerminalWidth")
  minTerminalHeight <- .rtuiNormalizeSizeInteger(minTerminalHeight, "minTerminalHeight")
  maxTerminalHeight <- .rtuiNormalizeSizeInteger(maxTerminalHeight, "maxTerminalHeight")

  if (!is.null(minTerminalWidth) && !is.null(maxTerminalWidth) &&
      minTerminalWidth > maxTerminalWidth) {
    stop("`minTerminalWidth` must be less than or equal to `maxTerminalWidth`.")
  }
  if (!is.null(minTerminalHeight) && !is.null(maxTerminalHeight) &&
      minTerminalHeight > maxTerminalHeight) {
    stop("`minTerminalHeight` must be less than or equal to `maxTerminalHeight`.")
  }

  component <- list(type = "showIf", child = child)
  if (!is.null(minTerminalWidth)) component$minTerminalWidth <- minTerminalWidth
  if (!is.null(maxTerminalWidth)) component$maxTerminalWidth <- maxTerminalWidth
  if (!is.null(minTerminalHeight)) component$minTerminalHeight <- minTerminalHeight
  if (!is.null(maxTerminalHeight)) component$maxTerminalHeight <- maxTerminalHeight

  structure(component, class = "rtuiComponent")
}

#' Box layout wrapper
#'
#' Draws a border around a child component, with optional title and style.
#'
#' @param child A single `rtuiComponent` to wrap.
#' @param title Optional single character string title displayed in the border.
#'   Use `NULL` (default) for no title.
#' @param color Optional border color. Supports named colors (`"default"`,
#'   `"black"`, `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`,
#'   `"cyan"`, `"graylight"`, `"graydark"`, `"redlight"`, `"greenlight"`,
#'   `"yellowlight"`, `"bluelight"`, `"magentalight"`, `"cyanlight"`,
#'   `"white"`) or a hex string like `"#RRGGBB"`.
#' @param style Border style. One of `"rounded"` (default), `"light"`,
#'   `"dashed"`, `"heavy"`, `"double"`, or `"empty"`.
#' @param titleStyle Title rendering style. One of `"header"` (default), which
#'   places the title inside the box as a header separated by a bar, or
#'   `"border"`, which places the title in the top-left border line.
#' @param titleAlign Title alignment for `"header"` mode. One of `"left"`
#'   (default), `"center"`, or `"right"`.
#' @param margin Integer number of spaces outside the box (default `0`).
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#' @param overflowX,overflowY Container overflow policy for each axis.
#'   Use `"visible"` (default) to keep normal flow, `"clip"` to crop child
#'   drawing to the container box on that axis, or `"scroll"` to enable a
#'   focus-driven scrollable viewport on that axis.
#'
#' @return A `rtuiComponent` list node of type `"box"`.
#'
#' @export
tuiBox <- function(
    child,
    title = NULL,
    color = NULL,
    style = "rounded",
    titleStyle = "header",
    titleAlign = "left",
    margin = 0L,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL,
    overflowX = "visible",
    overflowY = "visible"
) {
  if (!inherits(child, "rtuiComponent")) {
    stop("`child` must be a rtuiComponent object.")
  }
  if (!is.null(title) && (!is.character(title) || length(title) != 1L || is.na(title))) {
    stop("`title` must be NULL or a single character string.")
  }
  if (!is.character(style) || length(style) != 1L || is.na(style)) {
    stop("`style` must be a single character string.")
  }
  style <- tolower(trimws(style))
  allowed_styles <- c("rounded", "light", "dashed", "heavy", "double", "empty")
  if (!style %in% allowed_styles) {
    stop("`style` must be one of ", paste(shQuote(allowed_styles), collapse = ", "), ".")
  }

  if (!is.character(titleStyle) || length(titleStyle) != 1L || is.na(titleStyle)) {
    stop("`titleStyle` must be a single character string.")
  }
  titleStyle <- tolower(trimws(titleStyle))
  allowed_title_styles <- c("header", "border")
  if (!titleStyle %in% allowed_title_styles) {
    stop(
      "`titleStyle` must be one of ",
      paste(shQuote(allowed_title_styles), collapse = ", "),
      "."
    )
  }

  if (!is.character(titleAlign) || length(titleAlign) != 1L || is.na(titleAlign)) {
    stop("`titleAlign` must be a single character string.")
  }
  titleAlign <- tolower(trimws(titleAlign))
  allowed_title_align <- c("left", "center", "right")
  if (!titleAlign %in% allowed_title_align) {
    stop(
      "`titleAlign` must be one of ",
      paste(shQuote(allowed_title_align), collapse = ", "),
      "."
    )
  }

  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin)) {
    stop("`margin` must be a single non-negative integer.")
  }
  if (margin < 0 || margin != as.integer(margin)) {
    stop("`margin` must be a single non-negative integer.")
  }
  margin <- as.integer(margin)

  color <- .rtuiNormalizeColor(color)

  component <- list(
    type = "box",
    child = child,
    style = style,
    titleStyle = titleStyle,
    titleAlign = titleAlign,
    margin = margin
  )
  if (!is.null(title)) {
    component$title <- title
  }
  if (!is.null(color)) {
    component$color <- color
  }
  component <- .rtuiApplySizeSpec(
    component,
    width = width,
    height = height,
    minHeight = minHeight,
    maxHeight = maxHeight,
    widthPercent = widthPercent,
    heightPercent = heightPercent
  )
  component <- .rtuiApplyContainerOverflowSpec(
    component,
    overflowX = overflowX,
    overflowY = overflowY
  )

  structure(component, class = "rtuiComponent")
}
