test_that("tuiReq returns first value when truthy and errors when falsy", {
  expect_identical(tuiReq(TRUE, 1L, "ok"), TRUE)
  expect_error(tuiReq(NULL), class = "rtui_req_error")
  expect_error(tuiReq(character(0)), class = "rtui_req_error")
  expect_error(tuiReq(FALSE), class = "rtui_req_error")
})

test_that("tuiReq in renderer keeps output blank until value is present", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputText(id = "name", value = ""),
      tuiOutputText("out")
    ),
    server = function(input, output) {
      output$out <- tuiRenderText({
        tuiReq(input$name)
        paste("Hello", input$name)
      })
    }
  )

  expect_identical(app$state$output$out, "")
  app <- set_text_input(app, "name", "Ada")
  expect_identical(app$state$output$out, "Hello Ada")
})

test_that("tuiReq in reactive preserves previous stable value", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputText(id = "name", value = "Lin"),
      tuiOutputText("out")
    ),
    server = function(input, output) {
      safe_name <- tuiReactive({
        tuiReq(input$name)
        input$name
      })
      output$out <- tuiRenderText(safe_name())
    }
  )

  expect_identical(app$state$output$out, "Lin")
  app <- set_text_input(app, "name", "")
  expect_identical(app$state$output$out, "Lin")
  app <- set_text_input(app, "name", "Grace")
  expect_identical(app$state$output$out, "Grace")
})

test_that("tuiReq short-circuits observe blocks silently", {
  hit_count <- 0L
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("go", id = "go"),
      tuiOutputNumeric("out")
    ),
    server = function(input, output) {
      tuiObserve({
        tuiReq(input$go > 0L)
        hit_count <<- hit_count + 1L
      })
      output$out <- tuiRenderNumeric(hit_count)
    }
  )

  expect_identical(as.character(app$state$output$out), "0")
  app <- click_input(app, "go")
  expect_identical(as.character(app$state$output$out), "1")
})
