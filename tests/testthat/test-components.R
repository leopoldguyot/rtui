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

test_that("tuiBox stores configuration and validates inputs", {
  wrapped <- tuiBox(
    child = tuiOutputText("out"),
    title = "Panel",
    color = "blue",
    style = "double",
    titleStyle = "border",
    titleAlign = "center",
    margin = 1L,
    backgroundColor = "#123456"
  )

  expect_identical(wrapped$type, "box")
  expect_identical(wrapped$title, "Panel")
  expect_identical(wrapped$color, "blue")
  expect_identical(wrapped$style, "double")
  expect_identical(wrapped$titleStyle, "border")
  expect_identical(wrapped$titleAlign, "center")
  expect_identical(wrapped$margin, 1L)
  expect_identical(wrapped$backgroundColor, "#123456")
  expect_s3_class(wrapped$child, "rtuiComponent")

  default_box <- tuiBox(tuiOutputText("out"))
  expect_identical(default_box$style, "rounded")
  expect_identical(default_box$titleStyle, "header")
  expect_identical(default_box$titleAlign, "left")
  expect_identical(default_box$margin, 0L)
  expect_null(default_box[["title"]])
  expect_null(default_box$color)
  expect_null(default_box$backgroundColor)

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
    tuiBox(child = tuiOutputText("out"), backgroundColor = "not-a-color"),
    "`color` must be one of"
  )
})
