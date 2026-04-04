#' Text output component
#'
#' Displays the value of `output$<outputId>` as text.
#'
#' @param outputId A single character string naming the output id.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#'
#' @return A `rtuiComponent` list node of type `"outputText"`.
#'
#' @export
tuiOutputText <- function(
    outputId,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL
) {
  if (!is.character(outputId) || length(outputId) != 1L || is.na(outputId))
    stop("`outputId` must be a single character string.")

  component <- .rtuiApplySizeSpec(
    list(type = "outputText", outputId = outputId),
    width = width,
    height = height,
    minHeight = minHeight,
    maxHeight = maxHeight,
    widthPercent = widthPercent,
    heightPercent = heightPercent
  )

  structure(component, class = "rtuiComponent")
}

#' Numeric output component
#'
#' Displays the value of `output$<outputId>` as numeric text.
#'
#' @param outputId A single character string naming the output id.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#'
#' @return A `rtuiComponent` list node of type `"outputNumeric"`.
#'
#' @export
tuiOutputNumeric <- function(
    outputId,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL
) {
  if (!is.character(outputId) || length(outputId) != 1L || is.na(outputId))
    stop("`outputId` must be a single character string.")

  component <- .rtuiApplySizeSpec(
    list(type = "outputNumeric", outputId = outputId),
    width = width,
    height = height,
    minHeight = minHeight,
    maxHeight = maxHeight,
    widthPercent = widthPercent,
    heightPercent = heightPercent
  )

  structure(component, class = "rtuiComponent")
}

#' Internal helper `.rtuiNormalizeColor`.
#'
#' Validates and normalizes a UI color argument.
#'
#' @param color Optional color value for UI components.
#'
#' @return `NULL` or a normalized lowercase color string.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeColor <- function(color) {
  if (is.null(color)) {
    return(NULL)
  }

  if (!is.character(color) || length(color) != 1L || is.na(color)) {
    stop("`color` must be NULL or a single character string.")
  }

  normalized <- tolower(trimws(color))
  if (nchar(normalized) == 0L) {
    stop("`color` must not be an empty string.")
  }

  if (grepl("^#[0-9a-f]{6}$", normalized)) {
    return(normalized)
  }

  aliases <- c(
    gray = "graylight",
    grey = "graylight",
    greylight = "graylight",
    greydark = "graydark"
  )
  if (normalized %in% names(aliases)) {
    normalized <- aliases[[normalized]]
  }

  allowed <- c(
    "default", "black", "red", "green", "yellow", "blue", "magenta", "cyan",
    "graylight", "graydark", "redlight", "greenlight", "yellowlight",
    "bluelight", "magentalight", "cyanlight", "white"
  )

  if (!normalized %in% allowed) {
    stop(
      "`color` must be one of ",
      paste(shQuote(allowed), collapse = ", "),
      ", or a hex color like '#RRGGBB'."
    )
  }

  normalized
}

#' Internal helper `.rtuiNormalizeSizeInteger`.
#'
#' Validates optional non-negative integer size arguments.
#'
#' @param value Optional size value.
#' @param arg Argument name (for error messages).
#'
#' @return `NULL` or an integer value.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeSizeInteger <- function(value, arg) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    stop("`", arg, "` must be NULL or a single non-negative integer.")
  }
  if (value != as.integer(value) || value < 0) {
    stop("`", arg, "` must be NULL or a single non-negative integer.")
  }

  as.integer(value)
}

#' Internal helper `.rtuiNormalizeSizePercent`.
#'
#' Validates optional percentage size arguments.
#'
#' @param value Optional percentage value.
#' @param arg Argument name (for error messages).
#'
#' @return `NULL` or a numeric value between `0` and `1`.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeSizePercent <- function(value, arg) {
  if (is.null(value)) {
    return(NULL)
  }

  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    stop("`", arg, "` must be NULL or a single numeric value between 0 and 1.")
  }
  if (value < 0 || value > 1) {
    stop("`", arg, "` must be NULL or a single numeric value between 0 and 1.")
  }

  as.numeric(value)
}

#' Internal helper `.rtuiApplySizeSpec`.
#'
#' Validates and attaches size-related fields to a component node.
#'
#' @param component Component node list.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'
#' @return Updated component node list.
#'
#' @keywords internal
#' @noRd
.rtuiApplySizeSpec <- function(
    component,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL
) {
  width <- .rtuiNormalizeSizeInteger(width, "width")
  height <- .rtuiNormalizeSizeInteger(height, "height")
  minHeight <- .rtuiNormalizeSizeInteger(minHeight, "minHeight")
  maxHeight <- .rtuiNormalizeSizeInteger(maxHeight, "maxHeight")
  widthPercent <- .rtuiNormalizeSizePercent(widthPercent, "widthPercent")
  heightPercent <- .rtuiNormalizeSizePercent(heightPercent, "heightPercent")

  if (!is.null(width) && !is.null(widthPercent)) {
    stop("`width` and `widthPercent` cannot both be set.")
  }
  if (!is.null(height) && !is.null(heightPercent)) {
    stop("`height` and `heightPercent` cannot both be set.")
  }
  if (!is.null(minHeight) && !is.null(maxHeight) && minHeight > maxHeight) {
    stop("`minHeight` must be less than or equal to `maxHeight`.")
  }
  if (!is.null(height) && !is.null(minHeight) && height < minHeight) {
    stop("`height` must be greater than or equal to `minHeight`.")
  }
  if (!is.null(height) && !is.null(maxHeight) && height > maxHeight) {
    stop("`height` must be less than or equal to `maxHeight`.")
  }

  if (!is.null(width)) component$width <- width
  if (!is.null(height)) component$height <- height
  if (!is.null(minHeight)) component$minHeight <- minHeight
  if (!is.null(maxHeight)) component$maxHeight <- maxHeight
  if (!is.null(widthPercent)) component$widthPercent <- widthPercent
  if (!is.null(heightPercent)) component$heightPercent <- heightPercent

  component
}

#' Button input component
#'
#' A focusable button. When activated (Enter key), it increments `input$id`
#' and triggers a server update.
#'
#' @param label A character string shown as the button label.
#' @param id A character string used as the input key (`input$<id>`).
#' @param color Optional button color. Supports named colors (`"default"`,
#'   `"black"`, `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`,
#'   `"cyan"`, `"graylight"`, `"graydark"`, `"redlight"`, `"greenlight"`,
#'   `"yellowlight"`, `"bluelight"`, `"magentalight"`, `"cyanlight"`,
#'   `"white"`) or a hex string like `"#RRGGBB"`.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#'
#' @return A `rtuiComponent` list node of type `"button"`.
#'
#' @export
tuiInputButton <- function(
    label,
    id,
    color = NULL,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL
) {
  if (!is.character(label) || length(label) != 1L || is.na(label))
    stop("`label` must be a single character string.")
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")
  color <- .rtuiNormalizeColor(color)

  component <- list(type = "button", label = label, id = id)
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

  structure(
    component,
    class = "rtuiComponent"
  )
}

#' Text input component
#'
#' A focusable text input. The typed value is stored automatically in
#' `input$<id>` and triggers a server update on change. The input starts with
#' `value`. By default (`multiline = FALSE`), Enter triggers submit without
#' inserting a newline. Set `multiline = TRUE` to allow newlines.
#'
#' @param id A character string used as the input key (`input$<id>`).
#' @param placeholder A character string shown when the input is empty.
#' @param value A character string used as the initial/default input value.
#' @param multiline A single logical value. If `FALSE` (default), Enter does
#'   not modify the text content. If `TRUE`, Enter inserts a newline.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#'
#' @return A `rtuiComponent` list node of type `"input"`.
#'
#' @export
tuiInputText <- function(
    id,
    placeholder = "",
    value = "",
    multiline = FALSE,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL
) {
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")
  if (!is.character(placeholder) || length(placeholder) != 1L || is.na(placeholder))
    stop("`placeholder` must be a single character string.")
  if (!is.character(value) || length(value) != 1L || is.na(value))
    stop("`value` must be a single character string.")
  if (!is.logical(multiline) || length(multiline) != 1L || is.na(multiline))
    stop("`multiline` must be TRUE or FALSE.")

  component <- list(type = "input", id = id, placeholder = placeholder, value = value)
  if (isTRUE(multiline)) {
    component$multiline <- TRUE
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

  structure(
    component,
    class = "rtuiComponent"
  )
}

#' Checkbox input component
#'
#' A focusable checkbox. Toggling it updates `input$<id>` and triggers
#' a server update.
#'
#' @param label A character string shown as the checkbox label.
#' @param id A character string used as the input key (`input$<id>`).
#' @param value A single logical value used as the initial/default checkbox value.
#' @param width,height Optional fixed width/height in terminal cells.
#' @param minHeight,maxHeight Optional min/max height in terminal cells.
#' @param widthPercent,heightPercent Optional relative size between `0` and `1`.
#'   `widthPercent` is interpreted by [tuiRow()] and `heightPercent` by
#'   [tuiColumn()] for strict main-axis percentages.
#'
#' @return A `rtuiComponent` list node of type `"checkbox"`.
#'
#' @export
tuiInputCheckbox <- function(
    label,
    id,
    value = FALSE,
    width = NULL,
    height = NULL,
    minHeight = NULL,
    maxHeight = NULL,
    widthPercent = NULL,
    heightPercent = NULL
) {
  if (!is.character(label) || length(label) != 1L || is.na(label))
    stop("`label` must be a single character string.")
  if (!is.character(id) || length(id) != 1L || is.na(id))
    stop("`id` must be a single character string.")
  if (!is.logical(value) || length(value) != 1L || is.na(value))
    stop("`value` must be TRUE or FALSE.")

  component <- .rtuiApplySizeSpec(
    list(type = "checkbox", label = label, id = id, value = isTRUE(value)),
    width = width,
    height = height,
    minHeight = minHeight,
    maxHeight = maxHeight,
    widthPercent = widthPercent,
    heightPercent = heightPercent
  )

  structure(component, class = "rtuiComponent")
}
