#include <Rcpp.h>
#include "ui_tree.h"

#include <ftxui/component/component.hpp>
#include <ftxui/component/screen_interactive.hpp>
#include <ftxui/dom/elements.hpp>

#include <memory>
#include <string>

using namespace ftxui;

// [[Rcpp::export]]
void runTuiApp(
    Rcpp::List uiList,
    Rcpp::List stateList,
    Rcpp::List handlers
) {
  // Shared state — all component lambdas capture this by shared_ptr.
  auto state = std::make_shared<AppState>();
  state->values = stateList;

  // Build the FTXUI component tree from the R UI list tree.
  Component root = build_component(uiList, state, handlers);

  // Wrap root in a CatchEvent component that handles Ctrl+Q / Escape to quit.
  auto screen = ScreenInteractive::Fullscreen();

  Component app = CatchEvent(root, [&screen](Event event) -> bool {
    if (event == Event::Escape ||
        event == Event::CtrlQ) {
      screen.ExitLoopClosure()();
      return true;
    }
    return false;
  });

  // Blocking: takes over the terminal until the user quits.
  screen.Loop(app);
}
