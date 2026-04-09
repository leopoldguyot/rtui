library(rtui)

border_styles <- c("light", "dashed", "heavy", "double", "rounded", "empty")
overflow_modes <- c("clip", "wrap", "ellipsis")
align_modes <- c("left", "center", "right")
width_presets <- list(
  list(label = "none", min = NULL, max = NULL),
  list(label = "narrow (6..16)", min = 6L, max = 16L),
  list(label = "balanced (10..24)", min = 10L, max = 24L)
)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputText("help", overflow = "wrap"),
    tuiRow(
      tuiInputButton("Add row", id = "addRows", widthPercent = 0.5),
      tuiInputButton("Remove row", id = "removeRows", widthPercent = 0.5)
    ),
    tuiRow(
      tuiInputButton("Toggle row names", id = "toggleRowNames", widthPercent = 0.34),
      tuiInputButton("Toggle header", id = "toggleHeader", widthPercent = 0.33),
      tuiInputButton("Toggle zebra", id = "toggleZebra", widthPercent = 0.33)
    ),
    tuiRow(
      tuiInputButton("Toggle row border", id = "toggleRowBorder", widthPercent = 0.34),
      tuiInputButton("Toggle col border", id = "toggleColBorder", widthPercent = 0.33),
      tuiInputButton("Toggle header border", id = "toggleHeaderBorder", widthPercent = 0.33)
    ),
    tuiRow(
      tuiInputButton("Next border style", id = "nextBorderStyle", widthPercent = 0.34),
      tuiInputButton("Next cell overflow", id = "nextCellOverflow", widthPercent = 0.33),
      tuiInputButton("Next notes align", id = "nextNotesAlign", widthPercent = 0.33)
    ),
    tuiRow(
      tuiInputButton("Next width preset", id = "nextWidthPreset", widthPercent = 0.5),
      tuiInputButton("Next padding", id = "nextPadding", widthPercent = 0.5)
    ),
    tuiBox(
      title = "Interactive table preview",
      color = "green",
      minHeight = 16,
      child = tuiOutputTable(
        "tablePreview",
        overflowX = "scroll",
        overflowY = "scroll"
      )
    )
  ),
  server = function(input, output) {
    click_count <- function(value) {
      if (is.null(value) || length(value) != 1L || is.na(value)) {
        return(0L)
      }
      value <- suppressWarnings(as.integer(value))
      if (is.na(value)) {
        return(0L)
      }
      value
    }

    row_count <- tuiReactive({
      max(4L, 8L + click_count(input$addRows) - click_count(input$removeRows))
    })

    show_row_names <- tuiReactive({
      (click_count(input$toggleRowNames) %% 2L) == 1L
    })
    show_header <- tuiReactive({
      (click_count(input$toggleHeader) %% 2L) == 0L
    })
    zebra_rows <- tuiReactive({
      (click_count(input$toggleZebra) %% 2L) == 1L
    })

    row_border <- tuiReactive({
      (click_count(input$toggleRowBorder) %% 2L) == 1L
    })
    col_border <- tuiReactive({
      (click_count(input$toggleColBorder) %% 2L) == 0L
    })
    header_border <- tuiReactive({
      (click_count(input$toggleHeaderBorder) %% 2L) == 0L
    })

    border_style <- tuiReactive({
      border_styles[(click_count(input$nextBorderStyle) %% length(border_styles)) + 1L]
    })
    cell_overflow <- tuiReactive({
      overflow_modes[(click_count(input$nextCellOverflow) %% length(overflow_modes)) + 1L]
    })
    notes_align <- tuiReactive({
      align_modes[(click_count(input$nextNotesAlign) %% length(align_modes)) + 1L]
    })

    width_preset <- tuiReactive({
      width_presets[[(click_count(input$nextWidthPreset) %% length(width_presets)) + 1L]]
    })

    padding_settings <- tuiReactive({
      index <- click_count(input$nextPadding) %% 4L
      list(
        x = as.integer(index),
        y = if (index >= 2L) 1L else 0L,
        label = if (index < 2L) {
          paste0("x=", index, ", y=0")
        } else {
          paste0("x=", index, ", y=1")
        }
      )
    })

    table_data <- tuiReactive({
      n <- row_count()
      notes <- paste(
        "This is a deliberately long note used to demonstrate clipping, wrapping, and ellipsis.",
        "Row", seq_len(n), "contains additional context for overflow testing."
      )
      notes[seq.int(3L, n, by = 5L)] <- NA_character_
      data.frame(
        feature = paste0("feature-", sprintf("%02d", seq_len(n))),
        status = c("pending", "active", "paused")[((seq_len(n) - 1L) %% 3L) + 1L],
        owner = c("ui", "api", "core", "ops")[((seq_len(n) - 1L) %% 4L) + 1L],
        notes = notes,
        score = seq_len(n) * 7L,
        stringsAsFactors = FALSE
      )
    })

    tuiObserve({
      show_row_names_value <- show_row_names()
      show_header_value <- show_header()
      zebra_rows_value <- zebra_rows()
      row_border_value <- row_border()
      col_border_value <- col_border()
      header_border_value <- header_border()
      border_style_value <- border_style()
      cell_overflow_value <- cell_overflow()
      notes_align_value <- notes_align()
      width_preset_value <- width_preset()
      padding_settings_value <- padding_settings()
      row_count_value <- row_count()

      output$help <- tuiRenderText(
        paste(
          "Interactive tuiRenderTable() demo with button-based switches.",
          paste0("Rows=", row_count_value, "."),
          paste0(
            "showRowNames=", show_row_names_value,
            ", showHeader=", show_header_value,
            ", zebraRows=", zebra_rows_value, "."
          ),
          paste0(
            "rowBorder=", row_border_value,
            ", colBorder=", col_border_value,
            ", headerBorder=", header_border_value,
            ", borderStyle=", border_style_value, "."
          ),
          paste0(
            "cellOverflow=", cell_overflow_value,
            ", notesAlign=", notes_align_value,
            ", widthPreset=", width_preset_value$label,
            ", padding=", padding_settings_value$label, "."
          ),
          "Tab to the table, then use Ctrl+Arrow/Page/Home/End, mouse wheel, or drag the scrollbar."
        )
      )

      output$tablePreview <- tuiRenderTable(
        table_data(),
        showRowNames = show_row_names_value,
        showHeader = show_header_value,
        rowBorder = row_border_value,
        colBorder = col_border_value,
        headerBorder = header_border_value,
        borderStyle = border_style_value,
        headerBold = TRUE,
        headerColor = "yellowlight",
        headerBgColor = "blue",
        zebraRows = zebra_rows_value,
        zebraColorOdd = if (zebra_rows_value) "graydark" else NULL,
        zebraColorEven = if (zebra_rows_value) "black" else NULL,
        cellPaddingX = padding_settings_value$x,
        cellPaddingY = padding_settings_value$y,
        cellOverflow = cell_overflow_value,
        minColWidth = width_preset_value$min,
        maxColWidth = width_preset_value$max,
        columnAlign = c(
          feature = "left",
          status = "center",
          owner = "center",
          notes = notes_align_value,
          score = "right"
        ),
        naText = "<NA>"
      )
    })
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
