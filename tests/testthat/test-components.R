test_that("tuiOutputText stores wrap flag", {
  default_output <- tuiOutputText("message")
  expect_null(default_output$wrap)

  wrapped_output <- tuiOutputText("message", wrap = TRUE)
  expect_identical(wrapped_output$wrap, TRUE)
})

test_that("tuiOutputText validates wrap argument", {
  expect_error(
    tuiOutputText("message", wrap = "yes"),
    "`wrap` must be TRUE or FALSE."
  )
  expect_error(
    tuiOutputText("message", wrap = NA),
    "`wrap` must be TRUE or FALSE."
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

test_that("tuiBox stores configuration and validates inputs", {
  wrapped <- tuiBox(
    child = tuiOutputText("out"),
    title = "Panel",
    color = "blue",
    style = "double",
    titleStyle = "border",
    titleAlign = "center",
    margin = 1L
  )

  expect_identical(wrapped$type, "box")
  expect_identical(wrapped$title, "Panel")
  expect_identical(wrapped$color, "blue")
  expect_identical(wrapped$style, "double")
  expect_identical(wrapped$titleStyle, "border")
  expect_identical(wrapped$titleAlign, "center")
  expect_identical(wrapped$margin, 1L)
  expect_s3_class(wrapped$child, "rtuiComponent")

  default_box <- tuiBox(tuiOutputText("out"))
  expect_identical(default_box$style, "rounded")
  expect_identical(default_box$titleStyle, "header")
  expect_identical(default_box$titleAlign, "left")
  expect_identical(default_box$margin, 0L)
  expect_null(default_box[["title"]])
  expect_null(default_box$color)

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

  sized_button <- tuiInputButton(
    "Apply",
    id = "apply",
    widthPercent = 0.2
  )
  expect_identical(sized_button$widthPercent, 0.2)

  sized_box <- tuiBox(
    child = tuiOutputText("panel"),
    width = 40,
    minHeight = 2,
    maxHeight = 8,
    heightPercent = 0.5
  )
  expect_identical(sized_box$width, 40L)
  expect_identical(sized_box$minHeight, 2L)
  expect_identical(sized_box$maxHeight, 8L)
  expect_identical(sized_box$heightPercent, 0.5)
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
})
