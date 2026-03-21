test_that("reactive chain propagates input invalidation", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("inc", id = "inc"),
      tuiOutputNumeric("out")
    ),
    server = function(input, output) {
      base <- tuiReactive(input$inc)
      doubled <- tuiReactive(base() * 2L)
      output$out <- tuiRenderNumeric(doubled())
    }
  )

  expect_identical(as.character(app$state$output$out), "0")
  app <- click_input(app, "inc")
  expect_identical(as.character(app$state$output$out), "2")
  app <- click_input(app, "inc")
  expect_identical(as.character(app$state$output$out), "4")
})

test_that("reactive event follows reactive source changes", {
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("inc", id = "inc"),
      tuiOutputText("out")
    ),
    server = function(input, output) {
      base <- tuiReactive(input$inc %% 2L)
      evented <- tuiReactiveEvent(base(), runAtInit = FALSE, {
        paste0("v", base())
      })
      output$out <- tuiRenderText(if (is.null(evented())) "none" else evented())
    }
  )

  expect_identical(app$state$output$out, "none")
  app <- click_input(app, "inc")
  expect_identical(app$state$output$out, "v1")
  app <- click_input(app, "inc")
  expect_identical(app$state$output$out, "v0")
})

test_that("tuiIsolate prevents dependency registration", {
  eval_count <- 0L
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("A", id = "a"),
      tuiInputButton("B", id = "b"),
      tuiOutputNumeric("out")
    ),
    server = function(input, output) {
      value <- tuiReactive({
        eval_count <<- eval_count + 1L
        input$a + tuiIsolate(input$b) * 0L
      })
      output$out <- tuiRenderNumeric(value())
    }
  )

  expect_identical(eval_count, 1L)
  app <- click_input(app, "b")
  expect_identical(eval_count, 1L)
  app <- click_input(app, "a")
  expect_identical(eval_count, 2L)
})

test_that("dynamic dependencies switch with control reactive", {
  eval_count <- 0L
  app <- tuiApp(
    ui = tuiColumn(
      tuiInputButton("toggle", id = "toggle"),
      tuiInputButton("A", id = "a"),
      tuiInputButton("B", id = "b"),
      tuiOutputNumeric("out")
    ),
    server = function(input, output) {
      use_a <- tuiReactive(input$toggle %% 2L == 0L)
      chosen <- tuiReactive({
        eval_count <<- eval_count + 1L
        if (use_a()) input$a else input$b
      })
      output$out <- tuiRenderNumeric(chosen())
    }
  )

  expect_identical(eval_count, 1L)
  app <- click_input(app, "b")
  expect_identical(eval_count, 1L)
  app <- click_input(app, "a")
  expect_identical(eval_count, 2L)
  app <- click_input(app, "toggle")
  expect_identical(eval_count, 3L)
  app <- click_input(app, "a")
  expect_identical(eval_count, 3L)
  app <- click_input(app, "b")
  expect_identical(eval_count, 4L)
})
