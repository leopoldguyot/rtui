#pragma once

#include <Rcpp.h>
#include <ftxui/component/component.hpp>
#include <string>
#include <memory>
#include <functional>

// Shared mutable state passed by reference into all component callbacks.
// When a handler fires, it replaces *state and FTXUI re-renders automatically
// because the renderer lambda captures state by reference.
struct AppState {
  Rcpp::List values;
};

// Recursively convert an R UI list node into an FTXUI Component.
// - node: an R list with at least a "type" character element
// - state: shared mutable state
// - handlers: named R list of R functions
ftxui::Component build_component(
    const Rcpp::List& node,
    std::shared_ptr<AppState> state,
    const Rcpp::List& handlers
);
