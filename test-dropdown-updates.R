library(rtui)

owners_primary <- c("ui", "api", "core")
owners_secondary <- c("ops", "infra", "qa")
owners_long <- paste0("team-", sprintf("%02d", seq_len(20)))

dropdown_menu_height <- 8L

app <- tuiApp(
    ui = tuiColumn(
        tuiOutputText("title"),
        tuiOutputText("help", overflow = "wrap"),
        tuiRow(
            tuiInputButton("Set name: Ada", id = "setAda", color = "green", widthPercent = 0.34),
            tuiInputButton("Toggle enabled", id = "toggleEnabled", color = "yellow", widthPercent = 0.33),
            tuiInputButton("Next owner", id = "nextOwner", color = "cyan", widthPercent = 0.33)
        ),
        tuiRow(
            tuiInputButton("Switch owner choices", id = "switchChoices", widthPercent = 0.34),
            tuiInputButton("Use long choices", id = "setLongChoices", color = "blue", widthPercent = 0.33),
            tuiInputButton("Reset values", id = "resetAll", color = "magenta", widthPercent = 0.33)
        ),
        tuiInputText(id = "nameInput", placeholder = "Type a name", value = "John"),
        tuiInputCheckbox("Enabled", id = "enabledInput", value = FALSE),
        tuiInputDropdown(
            id = "ownerInput",
            choices = owners_primary,
            selected = "ui",
            maxMenuHeight = dropdown_menu_height,
            maxHeight = dropdown_menu_height
        ),
        tuiOutputText("state", overflow = "wrap"),
        tuiOutputText("lastEvent", overflow = "wrap")
    ),
    server = function(input, output) {
        owner_mode <- tuiReactiveVal("primary")
        last_event <- tuiReactiveVal("none")

        owners_for_mode <- function(mode) {
            if (identical(mode, "primary")) {
                owners_primary
            } else if (identical(mode, "secondary")) {
                owners_secondary
            } else {
                owners_long
            }
        }

        tuiObserveEvent(input$nameInput, {
            last_event(paste("nameInput changed ->", input$nameInput))
        })

        tuiObserveEvent(input$enabledInput, {
            value <- if (isTRUE(input$enabledInput)) "TRUE" else "FALSE"
            last_event(paste("enabledInput changed ->", value))
        })

        tuiObserveEvent(input$ownerInput, {
            last_event(paste("ownerInput changed ->", input$ownerInput))
        })

        tuiObserveEvent(input$setAda, {
            tuiUpdateTextInput("nameInput", "Ada")
        })

        tuiObserveEvent(input$toggleEnabled, {
            tuiUpdateCheckboxInput("enabledInput", !isTRUE(input$enabledInput))
        })

        tuiObserveEvent(input$nextOwner, {
            choices <- owners_for_mode(owner_mode())
            current <- input$ownerInput
            idx <- match(current, choices)
            if (is.na(idx)) idx <- 1L
            next_idx <- (idx %% length(choices)) + 1L
            tuiUpdateDropdownInput("ownerInput", selected = choices[[next_idx]])
        })

        tuiObserveEvent(input$switchChoices, {
            if (identical(owner_mode(), "primary")) {
                owner_mode("secondary")
                tuiUpdateDropdownInput("ownerInput", choices = owners_secondary, selected = owners_secondary[[1L]])
            } else {
                owner_mode("primary")
                tuiUpdateDropdownInput("ownerInput", choices = owners_primary, selected = owners_primary[[1L]])
            }
        })

        tuiObserveEvent(input$setLongChoices, {
            owner_mode("long")
            tuiUpdateDropdownInput("ownerInput", choices = owners_long, selected = owners_long[[1L]])
        })

        tuiObserveEvent(input$resetAll, {
            owner_mode("primary")
            tuiUpdateTextInput("nameInput", "John")
            tuiUpdateCheckboxInput("enabledInput", FALSE)
            tuiUpdateDropdownInput("ownerInput", choices = owners_primary, selected = "ui")
            last_event("reset via update* input functions")
        })

        output$title <- tuiRenderText("Dropdown + update* input API demo")
        output$help <- tuiRenderText(
            paste(
                "Type directly in the text field / checkbox / dropdown, then compare with button-driven server updates.",
                "Buttons use tuiUpdateTextInput(), tuiUpdateCheckboxInput(), and tuiUpdateDropdownInput().",
                "When closed, the dropdown stays compact (no large empty border area).",
                "Use long choices to load many entries: opening the dropdown shows a scrollbar."
            )
        )

        output$state <- tuiRenderText({
            paste(
                "Current values:",
                paste0("nameInput=", input$nameInput),
                paste0("enabledInput=", if (isTRUE(input$enabledInput)) "TRUE" else "FALSE"),
                paste0("ownerInput=", input$ownerInput),
                paste0("choiceSet=", owner_mode())
            )
        })

        output$lastEvent <- tuiRenderText(paste("Last input event:", last_event()))
    }
)

tuiRun(app, overflow = "clip", screen = "terminal") # press Escape or Ctrl+Q to quit
