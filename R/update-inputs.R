#' Internal helper `.rtuiNormalizeUpdateInputId`.
#'
#' Validates an input id used by `tuiUpdate*Input()` helpers.
#'
#' @param id Candidate input id.
#'
#' @return Normalized input id string.
#'
#' @keywords internal
#' @noRd
.rtuiNormalizeUpdateInputId <- function(id) {
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    stop("`id` must be a single character string.")
  }
  id
}

#' Internal helper `.rtuiRequireMutableInput`.
#'
#' Ensures an input id exists and (optionally) matches an expected input type.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param id Input id.
#' @param expectedType Optional expected input type.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiRequireMutableInput <- function(runtime, id, expectedType = NULL) {
  if (!id %in% names(runtime$currentInputState)) {
    stop("Unknown input id `", id, "`.")
  }

  if (!is.null(expectedType)) {
    actualType <- runtime$currentInputTypes[[id]]
    if (!identical(actualType, expectedType)) {
      stop("`id` must reference a ", expectedType, " input id.")
    }
  }

  invisible(NULL)
}

#' Internal helper `.rtuiMarkInputUpdated`.
#'
#' Marks an input as server-updated and invalidates its reactive dependents.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param id Input id.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiMarkInputUpdated <- function(runtime, id) {
  runtime$currentUpdatedInputIds <- unique(c(runtime$currentUpdatedInputIds, id))
  .rtuiGraphInvalidateDependents(runtime, .rtuiInputNodeId(id))
  invisible(NULL)
}

#' Update a text input value from server logic
#'
#' Sets the current value of an existing [tuiInputText()] input.
#'
#' @param id Input id passed to [tuiInputText()].
#' @param value New text value.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiUpdateTextInput <- function(id, value) {
  id <- .rtuiNormalizeUpdateInputId(id)
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("`value` must be a single character string.")
  }

  runtime <- .rtuiCurrentRuntime()
  .rtuiRequireMutableInput(runtime, id, expectedType = "text")

  if (!identical(runtime$currentInputState[[id]], value)) {
    runtime$currentInputState[[id]] <- value
    .rtuiMarkInputUpdated(runtime, id)
  }

  invisible(NULL)
}

#' Update a checkbox input value from server logic
#'
#' Sets the current value of an existing [tuiInputCheckbox()] input.
#'
#' @param id Input id passed to [tuiInputCheckbox()].
#' @param value New logical value.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiUpdateCheckboxInput <- function(id, value) {
  id <- .rtuiNormalizeUpdateInputId(id)
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`value` must be TRUE or FALSE.")
  }
  value <- isTRUE(value)

  runtime <- .rtuiCurrentRuntime()
  .rtuiRequireMutableInput(runtime, id, expectedType = "checkbox")

  if (!identical(runtime$currentInputState[[id]], value)) {
    runtime$currentInputState[[id]] <- value
    .rtuiMarkInputUpdated(runtime, id)
  }

  invisible(NULL)
}

#' Update a dropdown input value (and optional choices) from server logic
#'
#' Updates the selection and/or choices of an existing [tuiInputDropdown()]
#' input.
#'
#' @param id Input id passed to [tuiInputDropdown()].
#' @param selected Optional selected value. Must match one of effective choices.
#'   If `NULL`, current value is kept when valid; otherwise the first choice is
#'   selected.
#' @param choices Optional replacement choice vector.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiUpdateDropdownInput <- function(id, selected = NULL, choices = NULL) {
  id <- .rtuiNormalizeUpdateInputId(id)
  normalized_choices <- if (is.null(choices)) NULL else .rtuiNormalizeChoices(choices, "choices")
  if (!is.null(selected) &&
      (!is.character(selected) || length(selected) != 1L || is.na(selected))) {
    stop("`selected` must be NULL or a single character string.")
  }

  runtime <- .rtuiCurrentRuntime()
  .rtuiRequireMutableInput(runtime, id, expectedType = "dropdown")

  old_choices <- runtime$currentDropdownChoices[[id]]
  if (is.null(old_choices) || length(old_choices) == 0L) {
    old_choices <- as.character(runtime$currentInputState[[id]])
  }
  old_choices <- as.character(old_choices)

  new_choices <- if (is.null(normalized_choices)) old_choices else normalized_choices

  if (is.null(selected)) {
    current <- runtime$currentInputState[[id]]
    if (is.character(current) && length(current) == 1L && !is.na(current) && current %in% new_choices) {
      selected <- current
    } else {
      selected <- new_choices[[1L]]
    }
  } else {
    if (!selected %in% new_choices) {
      stop("`selected` must match one of `choices`.")
    }
  }

  choices_changed <- !identical(old_choices, new_choices)
  value_changed <- !identical(runtime$currentInputState[[id]], selected)

  if (choices_changed) {
    runtime$currentDropdownChoices[[id]] <- new_choices
  }
  if (value_changed) {
    runtime$currentInputState[[id]] <- selected
  }

  if (choices_changed || value_changed) {
    .rtuiMarkInputUpdated(runtime, id)
  }

  invisible(NULL)
}
