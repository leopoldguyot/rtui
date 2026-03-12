#include "ui_tree.h"

#include <ftxui/component/component.hpp>
#include <ftxui/component/component_base.hpp>
#include <ftxui/dom/elements.hpp>

#include <string>
#include <vector>

using namespace ftxui;

ftxui::Component build_component(
    const Rcpp::List& node,
    std::shared_ptr<AppState> state,
    const Rcpp::List& handlers
) {
  std::string type = Rcpp::as<std::string>(node["type"]);

  // ── Layout containers ────────────────────────────────────────────────────

  if (type == "vbox" || type == "hbox") {
    Rcpp::List children = node["children"];
    Components comps;
    for (int i = 0; i < children.size(); ++i) {
      comps.push_back(
        build_component(Rcpp::as<Rcpp::List>(children[i]), state, handlers)
      );
    }
    if (type == "vbox")
      return Container::Vertical(comps);
    else
      return Container::Horizontal(comps);
  }

  // ── Text output (reads a key from state) ─────────────────────────────────

  if (type == "text") {
    std::string key = Rcpp::as<std::string>(node["key"]);
    return Renderer([state, key] {
      // Convert whatever is stored under `key` to a string for display.
      SEXP val = state->values[key];
      std::string display;
      if (TYPEOF(val) == INTSXP)
        display = std::to_string(Rcpp::as<int>(val));
      else if (TYPEOF(val) == REALSXP)
        display = std::to_string(Rcpp::as<double>(val));
      else
        display = Rcpp::as<std::string>(val);
      return text(display);
    });
  }

  // ── Button ───────────────────────────────────────────────────────────────

  if (type == "button") {
    std::string label   = Rcpp::as<std::string>(node["label"]);
    std::string handler = Rcpp::as<std::string>(node["id"]);

    return Button(label, [state, handlers, handler] {
      if (handlers.containsElementNamed(handler.c_str())) {
        Rcpp::Function fn = handlers[handler];
        Rcpp::List new_state = fn(state->values);
        state->values = new_state;
      }
    });
  }

  // ── Text input ───────────────────────────────────────────────────────────

  if (type == "input") {
    std::string id          = Rcpp::as<std::string>(node["id"]);
    std::string placeholder = Rcpp::as<std::string>(node["placeholder"]);

    // Store the input string inside state so it's accessible to handlers.
    // We keep a shared_ptr<string> synced with state->values[id].
    auto content = std::make_shared<std::string>(
      state->values.containsElementNamed(id.c_str())
        ? Rcpp::as<std::string>(state->values[id])
        : ""
    );

    InputOption opts;
    opts.placeholder = placeholder;
    opts.on_change = [state, id, content] {
      state->values[id] = *content;
    };

    return Input(content.get(), opts);
  }

  // Fallback: empty renderer for unknown types
  return Renderer([] { return text(""); });
}
