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
