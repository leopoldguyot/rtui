click_input <- function(app, id) {
  app$state$input[[id]] <- as.integer(app$state$input[[id]]) + 1L
  app$state <- app$handlers[[id]](app$state)
  app
}

set_text_input <- function(app, id, value) {
  app$state$input[[id]] <- as.character(value)
  app$state <- app$handlers[[id]](app$state)
  app
}
