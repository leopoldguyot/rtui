test_that("tuiInputText updates input state and renderer output", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputText(id = "name", value = "John"),
      tuiOutputText("out")
    ),
    server = function(input, output) {
      output$out <- tuiRenderText(input$name)
    }
  )

  expect_identical(app$state$output$out, "John")
  app <- set_text_input(app, "name", "Ada")
  expect_identical(app$state$output$out, "Ada")
  app <- set_text_input(app, "name", "Grace")
  expect_identical(app$state$output$out, "Grace")
})

test_that("tuiObserveEvent runs only for the selected input event", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("inc", id = "inc"),
      tuiInputButton("dec", id = "dec"),
      tuiOutputNumeric("count")
    ),
    server = function(input, output) {
      observed <- tuiReactiveVal(0L)
      tuiObserveEvent(input$inc, {
        observed(observed() + 1L)
      })
      output$count <- tuiRenderNumeric(observed())
    }
  )

  expect_identical(as.character(app$state$output$count), "0")
  app <- click_input(app, "dec")
  expect_identical(as.character(app$state$output$count), "0")
  app <- click_input(app, "inc")
  expect_identical(as.character(app$state$output$count), "1")
  app <- click_input(app, "inc")
  expect_identical(as.character(app$state$output$count), "2")
})

test_that("tuiReactiveEvent with input trigger keeps previous value until next event", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputText(id = "name", value = "John"),
      tuiInputButton("apply", id = "apply"),
      tuiOutputText("out")
    ),
    server = function(input, output) {
      applied <- tuiReactiveEvent(input$apply, runAtInit = TRUE, {
        input$name
      })
      output$out <- tuiRenderText(applied())
    }
  )

  expect_identical(app$state$output$out, "John")
  app <- set_text_input(app, "name", "Ada")
  expect_identical(app$state$output$out, "John")
  app <- click_input(app, "apply")
  expect_identical(app$state$output$out, "Ada")
  app <- set_text_input(app, "name", "Grace")
  expect_identical(app$state$output$out, "Ada")
  app <- click_input(app, "apply")
  expect_identical(app$state$output$out, "Grace")
})

test_that("tuiIsolate in observeEvent avoids reactive event dependency", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("inc", id = "inc"),
      tuiInputButton("dec", id = "dec"),
      tuiOutputNumeric("count")
    ),
    server = function(input, output) {
      observed <- tuiReactiveVal(0L)
      dec_value <- tuiReactive(input$dec)
      tuiObserveEvent(input$inc, {
        tuiIsolate(dec_value())
        observed(observed() + 1L)
      })
      output$count <- tuiRenderNumeric(observed())
    }
  )

  expect_identical(as.character(app$state$output$count), "0")
  app <- click_input(app, "dec")
  expect_identical(as.character(app$state$output$count), "0")
  app <- click_input(app, "inc")
  expect_identical(as.character(app$state$output$count), "1")
  app <- click_input(app, "dec")
  expect_identical(as.character(app$state$output$count), "1")
  app <- click_input(app, "inc")
  expect_identical(as.character(app$state$output$count), "2")
})

test_that("tuiIsolate in output skips dependency on isolated reactive event", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("inc", id = "inc"),
      tuiInputButton("apply", id = "apply"),
      tuiInputText(id = "name", value = "John"),
      tuiOutputText("message")
    ),
    server = function(input, output) {
      counter <- tuiReactive(input$inc)
      applied <- tuiReactiveEvent(input$apply, runAtInit = TRUE, {
        input$name
      })

      output$message <- tuiRenderText({
        counter()
        paste0("counter changed while name is ", tuiIsolate(applied()))
      })
    }
  )

  expect_identical(app$state$output$message, "counter changed while name is John")
  app <- set_text_input(app, "name", "Ada")
  expect_identical(app$state$output$message, "counter changed while name is John")
  app <- click_input(app, "apply")
  expect_identical(app$state$output$message, "counter changed while name is John")
  app <- click_input(app, "inc")
  expect_identical(app$state$output$message, "counter changed while name is Ada")
})

test_that("tuiRender warns once when output self-invalidates via side effects", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("inc", id = "inc"),
      tuiOutputText("message")
    ),
    server = function(input, output) {
      n <- tuiReactiveVal(0L)
      output$message <- tuiRenderText({
        input$inc
        n(n() + 1L)
        paste0("n=", n())
      })
    }
  )

  expect_warning(
    {
      app <- click_input(app, "inc")
    },
    "tuiRenderText\\(\\)"
  )
  expect_no_warning({
    app <- click_input(app, "inc")
  })
})

test_that("recreating app for run resets initial input defaults", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputText(id = "name", value = "John"),
      tuiOutputText("out")
    ),
    server = function(input, output) {
      output$out <- tuiRenderText(input$name)
    }
  )

  app <- set_text_input(app, "name", "Ada")
  expect_identical(app$state$input$name, "Ada")

  rerun <- tuiApp(app$ui, app$server)
  expect_identical(rerun$state$input$name, "John")
  expect_identical(rerun$state$output$out, "John")
})
