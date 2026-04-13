test_that("tuiOutputText stores wrap and overflow policies", {
  default_output <- tuiOutputText("message")
  expect_null(default_output$wrap)
  expect_null(default_output$overflow)

  wrapped_output <- tuiOutputText("message", wrap = TRUE)
  expect_identical(wrapped_output$wrap, TRUE)
  expect_identical(wrapped_output$overflow, "wrap")

  clipped_output <- tuiOutputText("message", overflow = "clip")
  expect_null(clipped_output$overflow)

  ellipsis_output <- tuiOutputText("message", overflow = "ellipsis")
  expect_identical(ellipsis_output$overflow, "ellipsis")
})

test_that("tuiOutputText validates wrap and overflow arguments", {
  expect_error(
    tuiOutputText("message", wrap = "yes"),
    "`wrap` must be TRUE or FALSE."
  )
  expect_error(
    tuiOutputText("message", wrap = NA),
    "`wrap` must be TRUE or FALSE."
  )
  expect_error(
    tuiOutputText("message", overflow = 1),
    "`overflow` must be a single character string."
  )
  expect_error(
    tuiOutputText("message", overflow = "invalid"),
    "`overflow` must be one of"
  )
  expect_error(
    tuiOutputText("message", wrap = TRUE, overflow = "clip"),
    "`wrap = TRUE` requires `overflow` to be NULL or \"wrap\"."
  )
})

test_that("tuiOutputTable stores overflow and validates inputs", {
  default_table <- tuiOutputTable("tableOut")
  expect_identical(default_table$type, "outputTable")
  expect_identical(default_table$overflowX, "scroll")
  expect_identical(default_table$overflowY, "scroll")
  expect_null(default_table$headerClickId)

  clipped_table <- tuiOutputTable(
    "tableOut2",
    overflowX = "clip",
    overflowY = "visible",
    headerClickId = "table_header_click"
  )
  expect_identical(clipped_table$overflowX, "clip")
  expect_identical(clipped_table$overflowY, "visible")
  expect_identical(clipped_table$headerClickId, "table_header_click")

  expect_error(
    tuiOutputTable(1),
    "`outputId` must be a single character string."
  )
  expect_error(
    tuiOutputTable("tableOut", overflowX = 1),
    "`overflowX` must be a single character string."
  )
  expect_error(
    tuiOutputTable("tableOut", overflowY = "invalid"),
    "`overflowY` must be one of"
  )
  expect_error(
    tuiOutputTable("tableOut", headerClickId = 1),
    "`headerClickId` must be NULL or a single character string."
  )
  expect_error(
    tuiOutputTable("tableOut", headerClickId = NA_character_),
    "`headerClickId` must be NULL or a single character string."
  )
})

test_that("tuiRenderTable stores and validates table customization options", {
  renderer <- tuiRenderTable(
    data.frame(x = 1),
    showRowNames = TRUE,
    showHeader = TRUE,
    outerBorder = TRUE,
    rowBorder = TRUE,
    colBorder = FALSE,
    headerRowBorder = TRUE,
    headerColBorder = FALSE,
    borderStyle = "double",
    borderColor = "green",
    headerBorderColor = "yellow",
    headerBold = FALSE,
    headerColor = "cyan",
    headerBgColor = "black",
    zebraRows = TRUE,
    zebraColorOdd = "graydark",
    zebraColorEven = "default",
    cellPaddingX = 1,
    cellPaddingY = 0,
    cellOverflow = "ellipsis",
    minColWidth = 4,
    maxColWidth = 12,
    columnAlign = c(x = "right"),
    naText = "-"
  )
  expect_identical(renderer$showRowNames, TRUE)
  expect_identical(renderer$rowBorder, TRUE)
  expect_identical(renderer$colBorder, FALSE)
  expect_identical(renderer$headerRowBorder, TRUE)
  expect_identical(renderer$headerColBorder, FALSE)
  expect_identical(renderer$borderStyle, "double")
  expect_identical(renderer$cellOverflow, "ellipsis")
  expect_identical(renderer$naText, "-")

  expect_error(
    tuiRenderTable(data.frame(x = 1), showRowNames = "yes"),
    "`showRowNames` must be TRUE or FALSE."
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), rowBorder = NA),
    "`rowBorder` must be TRUE or FALSE."
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), headerRowBorder = "yes"),
    "`headerRowBorder` must be TRUE or FALSE."
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), headerColBorder = NA),
    "`headerColBorder` must be TRUE or FALSE."
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), borderStyle = "unknown"),
    "`borderStyle` must be one of"
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), cellOverflow = "unknown"),
    "`cellOverflow` must be one of"
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), minColWidth = 8, maxColWidth = 4),
    "`minColWidth` must be less than or equal to `maxColWidth`."
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), columnAlign = 1),
    "`columnAlign` must be NULL, a character vector, or a named list."
  )
  expect_error(
    tuiRenderTable(data.frame(x = 1), naText = NA_character_),
    "`naText` must be a single character string."
  )
})

test_that("tuiInputButton stores optional color", {
  button <- tuiInputButton("Apply", id = "apply", color = "blue")
  expect_identical(button$color, "blue")

  hex_button <- tuiInputButton("Apply", id = "apply_hex", color = "#A1b2C3")
  expect_identical(hex_button$color, "#a1b2c3")
})

test_that("tuiInputButton normalizes accepted color aliases", {
  button <- tuiInputButton("Apply", id = "apply", color = "grey")
  expect_identical(button$color, "graylight")

  gray_button <- tuiInputButton("Apply", id = "apply2", color = "gray")
  expect_identical(gray_button$color, "graylight")
})

test_that("tuiInputButton validates color values", {
  expect_error(
    tuiInputButton("Apply", id = "bad", color = 1),
    "`color` must be NULL or a single character string."
  )
  expect_error(
    tuiInputButton("Apply", id = "bad", color = ""),
    "`color` must not be an empty string."
  )
  expect_error(
    tuiInputButton("Apply", id = "bad", color = "not-a-color"),
    "`color` must be one of"
  )
  expect_error(
    tuiInputButton("Apply", id = "bad", color = "#12345"),
    "`color` must be one of"
  )
})

test_that("tuiInputText stores multiline flag", {
  input <- tuiInputText(id = "name")
  expect_null(input$multiline)

  multiline_input <- tuiInputText(id = "name", multiline = TRUE)
  expect_identical(multiline_input$multiline, TRUE)
})

test_that("tuiInputText validates multiline argument", {
  expect_error(
    tuiInputText(id = "name", multiline = "no"),
    "`multiline` must be TRUE or FALSE."
  )
  expect_error(
    tuiInputText(id = "name", multiline = NA),
    "`multiline` must be TRUE or FALSE."
  )
})

test_that("tuiInputCheckbox stores value and validates inputs", {
  checked <- tuiInputCheckbox("Enable feature", id = "enabled", value = TRUE)
  expect_identical(checked$type, "checkbox")
  expect_identical(checked$label, "Enable feature")
  expect_identical(checked$id, "enabled")
  expect_identical(checked$value, TRUE)

  unchecked <- tuiInputCheckbox("Enable feature", id = "enabled2")
  expect_identical(unchecked$value, FALSE)

  expect_error(
    tuiInputCheckbox(1, id = "enabled"),
    "`label` must be a single character string."
  )
  expect_error(
    tuiInputCheckbox("Enable feature", id = NA_character_),
    "`id` must be a single character string."
  )
  expect_error(
    tuiInputCheckbox("Enable feature", id = "enabled", value = "yes"),
    "`value` must be TRUE or FALSE."
  )
})

test_that("tuiInputDropdown stores values and validates inputs", {
  dropdown <- tuiInputDropdown(
    id = "owner",
    choices = c("ui", "api", "core"),
    selected = "api"
  )
  expect_identical(dropdown$type, "dropdown")
  expect_identical(dropdown$id, "owner")
  expect_identical(dropdown$choices, c("ui", "api", "core"))
  expect_identical(dropdown$value, "api")
  expect_null(dropdown$maxMenuHeight)

  default_selected <- tuiInputDropdown(
    id = "status",
    choices = c("pending", "active")
  )
  expect_identical(default_selected$value, "pending")

  custom_menu_height <- tuiInputDropdown(
    id = "queue",
    choices = c("new", "running", "done"),
    maxMenuHeight = 8
  )
  expect_identical(custom_menu_height$maxMenuHeight, 8L)

  alias_dropdown <- tuiDropDownInput(
    id = "alias",
    choices = c("a", "b"),
    selected = "b"
  )
  expect_identical(alias_dropdown$type, "dropdown")
  expect_identical(alias_dropdown$value, "b")

  expect_error(
    tuiInputDropdown(id = NA_character_, choices = c("a")),
    "`id` must be a single character string."
  )
  expect_error(
    tuiInputDropdown(id = "x", choices = c()),
    "`choices` must be a non-empty character vector without NA values."
  )
  expect_error(
    tuiInputDropdown(id = "x", choices = c("a", NA_character_)),
    "`choices` must be a non-empty character vector without NA values."
  )
  expect_error(
    tuiInputDropdown(id = "x", choices = c("a", "b"), selected = 1),
    "`selected` must be NULL or a single character string."
  )
  expect_error(
    tuiInputDropdown(id = "x", choices = c("a", "b"), selected = "z"),
    "`selected` must match one of `choices`."
  )
  expect_error(
    tuiInputDropdown(id = "x", choices = c("a"), maxMenuHeight = 0),
    "`maxMenuHeight` must be NULL or a single positive integer."
  )
})

test_that("tuiBox stores configuration and validates inputs", {
  wrapped <- tuiBox(
    child = tuiOutputText("out"),
    title = "Panel",
    color = "blue",
    style = "double",
    titleStyle = "border",
    titleAlign = "center",
    margin = 1L,
    overflowX = "clip",
    overflowY = "scroll"
  )

  expect_identical(wrapped$type, "box")
  expect_identical(wrapped$title, "Panel")
  expect_identical(wrapped$color, "blue")
  expect_identical(wrapped$style, "double")
  expect_identical(wrapped$titleStyle, "border")
  expect_identical(wrapped$titleAlign, "center")
  expect_identical(wrapped$margin, 1L)
  expect_identical(wrapped$overflowX, "clip")
  expect_identical(wrapped$overflowY, "scroll")
  expect_s3_class(wrapped$child, "rtuiComponent")

  default_box <- tuiBox(tuiOutputText("out"))
  expect_identical(default_box$style, "rounded")
  expect_identical(default_box$titleStyle, "header")
  expect_identical(default_box$titleAlign, "left")
  expect_identical(default_box$margin, 0L)
  expect_null(default_box[["title"]])
  expect_null(default_box$color)
  expect_null(default_box$overflowX)
  expect_null(default_box$overflowY)

  expect_error(
    tuiBox(child = "not-a-component"),
    "`child` must be a rtuiComponent object."
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), title = NA_character_),
    "`title` must be NULL or a single character string."
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), style = "unknown"),
    "`style` must be one of"
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), titleStyle = "middle"),
    "`titleStyle` must be one of"
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), titleAlign = "middle"),
    "`titleAlign` must be one of"
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), margin = -1L),
    "`margin` must be a single non-negative integer."
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), margin = 1.5),
    "`margin` must be a single non-negative integer."
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), color = "not-a-color"),
    "`color` must be one of"
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), overflowX = "invalid"),
    "`overflowX` must be one of"
  )
  expect_error(
    tuiBox(child = tuiOutputText("out"), overflowY = 1),
    "`overflowY` must be a single character string."
  )
})

test_that("size arguments are stored on components and layouts", {
  sized_output <- tuiOutputText(
    "out",
    width = 20,
    minHeight = 1,
    maxHeight = 3
  )
  expect_identical(sized_output$width, 20L)
  expect_identical(sized_output$minHeight, 1L)
  expect_identical(sized_output$maxHeight, 3L)

  sized_table <- tuiOutputTable(
    "tableOut",
    width = 32,
    minHeight = 2,
    maxHeight = 6,
    widthPercent = NULL,
    heightPercent = 0.5,
    overflowX = "clip",
    overflowY = "scroll"
  )
  expect_identical(sized_table$width, 32L)
  expect_identical(sized_table$minHeight, 2L)
  expect_identical(sized_table$maxHeight, 6L)
  expect_identical(sized_table$heightPercent, 0.5)
  expect_identical(sized_table$overflowX, "clip")
  expect_identical(sized_table$overflowY, "scroll")

  sized_button <- tuiInputButton(
    "Apply",
    id = "apply",
    widthPercent = 0.2
  )
  expect_identical(sized_button$widthPercent, 0.2)

  sized_dropdown <- tuiInputDropdown(
    id = "owner",
    choices = c("ui", "api"),
    widthPercent = 0.3
  )
  expect_identical(sized_dropdown$widthPercent, 0.3)

  sized_box <- tuiBox(
    child = tuiOutputText("panel"),
    width = 40,
    minHeight = 2,
    maxHeight = 8,
    heightPercent = 0.5,
    overflowX = "clip",
    overflowY = "scroll"
  )
  expect_identical(sized_box$width, 40L)
  expect_identical(sized_box$minHeight, 2L)
  expect_identical(sized_box$maxHeight, 8L)
  expect_identical(sized_box$heightPercent, 0.5)
  expect_identical(sized_box$overflowX, "clip")
  expect_identical(sized_box$overflowY, "scroll")

  sized_row <- tuiRow(
    tuiOutputText("a"),
    tuiOutputText("b"),
    overflowX = "scroll",
    overflowY = "clip"
  )
  expect_identical(sized_row$overflowX, "scroll")
  expect_identical(sized_row$overflowY, "clip")

  sized_column <- tuiColumn(
    tuiOutputText("a"),
    overflowX = "clip",
    overflowY = "scroll"
  )
  expect_identical(sized_column$overflowX, "clip")
  expect_identical(sized_column$overflowY, "scroll")
})

test_that("size argument validation and strict percent sums are enforced", {
  expect_error(
    tuiOutputText("out", width = -1),
    "`width` must be NULL or a single non-negative integer."
  )
  expect_error(
    tuiOutputText("out", widthPercent = 1.2),
    "`widthPercent` must be NULL or a single numeric value between 0 and 1."
  )
  expect_error(
    tuiOutputText("out", width = 10, widthPercent = 0.5),
    "`width` and `widthPercent` cannot both be set."
  )
  expect_error(
    tuiOutputText("out", height = 2, minHeight = 4),
    "`height` must be greater than or equal to `minHeight`."
  )
  expect_error(
    tuiRow(
      tuiOutputText("a"),
      overflowX = "invalid"
    ),
    "`overflowX` must be one of"
  )
  expect_error(
    tuiColumn(
      tuiOutputText("a"),
      overflowY = NA_character_
    ),
    "`overflowY` must be a single character string."
  )

  expect_error(
    tuiRow(
      tuiOutputText("a", widthPercent = 0.7),
      tuiOutputText("b", widthPercent = 0.4)
    ),
    "Sum of `widthPercent` values in tuiRow\\(\\) children must be <= 1."
  )
  expect_error(
    tuiColumn(
      tuiOutputText("a", heightPercent = 0.7),
      tuiOutputText("b", heightPercent = 0.4)
    ),
    "Sum of `heightPercent` values in tuiColumn\\(\\) children must be <= 1."
  )
})

test_that("tuiShowIf stores constraints and validates inputs", {
  visible <- tuiShowIf(
    child = tuiOutputText("out"),
    minTerminalWidth = 80,
    maxTerminalWidth = 140,
    minTerminalHeight = 24,
    maxTerminalHeight = 60
  )

  expect_identical(visible$type, "showIf")
  expect_s3_class(visible$child, "rtuiComponent")
  expect_identical(visible$minTerminalWidth, 80L)
  expect_identical(visible$maxTerminalWidth, 140L)
  expect_identical(visible$minTerminalHeight, 24L)
  expect_identical(visible$maxTerminalHeight, 60L)

  expect_error(
    tuiShowIf(child = "not-a-component"),
    "`child` must be a rtuiComponent object."
  )
  expect_error(
    tuiShowIf(child = tuiOutputText("out"), minTerminalWidth = -1),
    "`minTerminalWidth` must be NULL or a single non-negative integer."
  )
  expect_error(
    tuiShowIf(
      child = tuiOutputText("out"),
      minTerminalWidth = 120,
      maxTerminalWidth = 100
    ),
    "`minTerminalWidth` must be less than or equal to `maxTerminalWidth`."
  )
  expect_error(
    tuiShowIf(
      child = tuiOutputText("out"),
      minTerminalHeight = 50,
      maxTerminalHeight = 40
    ),
    "`minTerminalHeight` must be less than or equal to `maxTerminalHeight`."
  )
})

test_that("tuiModal stores configuration and validates inputs", {
  wrapped <- tuiModal(
    child = tuiOutputText("mainOut"),
    modal = tuiBox(tuiOutputText("modalOut"), title = "Popup"),
    show = TRUE,
    showInputId = "showModal",
    closeOnEscape = FALSE
  )

  expect_identical(wrapped$type, "modal")
  expect_s3_class(wrapped$child, "rtuiComponent")
  expect_s3_class(wrapped$modal, "rtuiComponent")
  expect_identical(wrapped$show, TRUE)
  expect_identical(wrapped$showInputId, "showModal")
  expect_identical(wrapped$closeOnEscape, FALSE)

  default_modal <- tuiModal(
    child = tuiOutputText("mainOut"),
    modal = tuiOutputText("modalOut")
  )
  expect_identical(default_modal$show, FALSE)
  expect_identical(default_modal$closeOnEscape, TRUE)
  expect_null(default_modal$showInputId)

  expect_error(
    tuiModal(child = "not-a-component", modal = tuiOutputText("modalOut")),
    "`child` must be a rtuiComponent object."
  )
  expect_error(
    tuiModal(child = tuiOutputText("mainOut"), modal = "not-a-component"),
    "`modal` must be a rtuiComponent object."
  )
  expect_error(
    tuiModal(child = tuiOutputText("mainOut"), modal = tuiOutputText("modalOut"), show = "yes"),
    "`show` must be TRUE or FALSE."
  )
  expect_error(
    tuiModal(
      child = tuiOutputText("mainOut"),
      modal = tuiOutputText("modalOut"),
      showInputId = NA_character_
    ),
    "`showInputId` must be NULL or a single character string."
  )
  expect_error(
    tuiModal(
      child = tuiOutputText("mainOut"),
      modal = tuiOutputText("modalOut"),
      closeOnEscape = 1
    ),
    "`closeOnEscape` must be TRUE or FALSE."
  )
})

test_that("tuiModal collects ids from both branches", {
  app <- tuiApp(
    ui = tuiModal(
      child = tuiOutputText("mainOut"),
      modal = tuiColumn(
        tuiOutputText("modalOut"),
        tuiInputButton("Close", id = "closeModal")
      ),
      showInputId = "modalVisible"
    ),
    server = function(input, output) {
      output$mainOut <- tuiRenderText("Main")
      output$modalOut <- tuiRenderText("Modal")
    }
  )

  expect_true(all(c("mainOut", "modalOut") %in% names(app$state$output)))
  expect_true("closeModal" %in% names(app$state$input))
})

test_that("tuiRun validates overflow argument", {
  app <- tuiApp(
    ui = tuiOutputText("out"),
    server = function(input, output) {
      output$out <- tuiRenderText("ok")
    }
  )

  expect_error(
    tuiRun(app, overflow = 1),
    "`overflow` must be a single character string."
  )

  expect_error(
    tuiRun(app, overflow = "invalid"),
    "`overflow` must be one of"
  )
  expect_error(
    tuiRun(app, overflow = "invalid"),
    "block"
  )
})
