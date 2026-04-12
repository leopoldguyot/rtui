library(rtui)

modal_behaviors <- c(
  "reactive-escape",
  "reactive-no-escape",
  "static-open",
  "static-closed"
)

app <- tuiApp(
  ui = tuiModal(
    child = tuiColumn(
      tuiOutputText("title"),
      tuiOutputText("behavior", overflow = "wrap"),
      tuiOutputText("status"),
      tuiRow(
        tuiInputButton("Next behavior", id = "nextBehavior", widthPercent = 0.5),
        tuiInputButton("Open modal", id = "openModal", widthPercent = 0.5)
      ),
      tuiOutputText("hint", overflow = "wrap"),
      tuiShowIf(
        child = tuiInputCheckbox("internal modal visibility", id = "showModal", value = FALSE),
        minTerminalWidth = 99999L
      )
    ),
    modal = tuiBox(
      tuiColumn(
        tuiOutputText("modalTitle"),
        tuiOutputText("modalBehavior", overflow = "wrap"),
        tuiOutputText("modalBody", overflow = "wrap"),
        tuiRow(
          tuiInputButton("Next behavior", id = "nextBehaviorModal", widthPercent = 0.5),
          tuiInputButton("Close modal", id = "closeModal", widthPercent = 0.5)
        ),
        tuiOutputText("modalHint", overflow = "wrap")
      ),
      title = "Modal playground",
      style = "double",
      color = "cyan",
      width = 64,
      minHeight = 11
    ),
    showInputId = "showModal",
    closeOnEscape = TRUE
  ),
  server = function(input, output) {
    behavior <- tuiReactiveVal(modal_behaviors[[1]])
    modal_visible <- tuiReactiveVal(FALSE)

    # Demo helper: update visibility input from button handlers.
    set_modal_visible <- function(value) {
      visible <- isTRUE(value)
      modal_visible(visible)
      runtime <- rtui:::.rtuiCurrentRuntime()
      runtime$currentInputState[["showModal"]] <- visible
      invisible(visible)
    }

    advance_behavior <- function() {
      current <- behavior()
      current_index <- match(current, modal_behaviors)
      if (is.na(current_index)) {
        current_index <- 1L
      }
      next_index <- (current_index %% length(modal_behaviors)) + 1L
      next_value <- modal_behaviors[[next_index]]
      behavior(next_value)

      if (identical(next_value, "static-open")) {
        set_modal_visible(TRUE)
      } else if (identical(next_value, "static-closed")) {
        set_modal_visible(FALSE)
      }

      invisible(next_value)
    }

    tuiObserveEvent(input$nextBehavior, {
      advance_behavior()
    })
    tuiObserveEvent(input$nextBehaviorModal, {
      advance_behavior()
    })

    tuiObserveEvent(input$openModal, {
      if (!identical(behavior(), "static-closed")) {
        set_modal_visible(TRUE)
      }
    })

    tuiObserveEvent(input$closeModal, {
      if (!identical(behavior(), "static-open")) {
        set_modal_visible(FALSE)
      }
    })

    tuiObserveEvent(input$showModal, {
      requested <- isTRUE(input$showModal)
      mode <- behavior()

      if (identical(mode, "reactive-no-escape") && !requested) {
        set_modal_visible(TRUE)
        return(invisible(NULL))
      }
      if (identical(mode, "static-open")) {
        set_modal_visible(TRUE)
        return(invisible(NULL))
      }
      if (identical(mode, "static-closed")) {
        set_modal_visible(FALSE)
        return(invisible(NULL))
      }

      modal_visible(requested)
      invisible(NULL)
    })

    behavior_label <- tuiReactive({
      switch(
        behavior(),
        "reactive-escape" = "reactive-escape: visibility is reactive and Escape closes the modal.",
        "reactive-no-escape" = "reactive-no-escape: visibility is reactive and Escape is ignored.",
        "static-open" = "static-open: modal visibility is forced to open.",
        "static-closed" = "static-closed: modal visibility is forced to closed."
      )
    })

    output$title <- tuiRenderText("tuiModal interactive behavior demo")
    output$behavior <- tuiRenderText(paste("Current behavior:", behavior_label()))
    output$status <- tuiRenderText(if (isTRUE(modal_visible())) "Modal: visible" else "Modal: hidden")
    output$hint <- tuiRenderText(
      paste(
        "Use Next behavior to cycle behaviors.",
        "Use Open modal when the popup is hidden.",
        "Use Close modal from inside the popup.",
        "Press Escape while open to test the current behavior."
      )
    )

    output$modalTitle <- tuiRenderText("Modal content is a standard rtui subtree.")
    output$modalBehavior <- tuiRenderText(paste("Current behavior:", behavior_label()))
    output$modalBody <- tuiRenderText(
      "This demo includes a button to cycle behavior and a button to close the modal."
    )
    output$modalHint <- tuiRenderText(
      "closeOnEscape is enabled in the modal config; behavior mode can force reopen/close semantics."
    )
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
