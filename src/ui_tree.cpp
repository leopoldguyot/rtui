#include "ui_tree.h"

#include <ftxui/component/component.hpp>
#include <ftxui/component/component_base.hpp>
#include <ftxui/dom/elements.hpp>

#include <memory>
#include <string>
#include <vector>

using namespace ftxui;

namespace {

Rcpp::List get_sublist_or_empty(const Rcpp::List& values, const char* name) {
  if (values.containsElementNamed(name)) {
    SEXP candidate = values[name];
    if (TYPEOF(candidate) == VECSXP) {
      return Rcpp::as<Rcpp::List>(candidate);
    }
  }
  return Rcpp::List::create();
}

std::string value_to_string(SEXP val) {
  if (Rf_isNull(val) || Rf_length(val) == 0) {
    return "";
  }
  if (TYPEOF(val) == INTSXP) {
    int x = Rcpp::as<int>(val);
    return x == NA_INTEGER ? "NA" : std::to_string(x);
  }
  if (TYPEOF(val) == REALSXP) {
    return std::to_string(Rcpp::as<double>(val));
  }
  if (TYPEOF(val) == STRSXP) {
    return Rcpp::as<std::string>(val);
  }
  if (TYPEOF(val) == LGLSXP) {
    int x = Rcpp::as<int>(val);
    if (x == NA_LOGICAL) {
      return "NA";
    }
    return x ? "TRUE" : "FALSE";
  }

  Rcpp::Function as_character("as.character");
  Rcpp::CharacterVector chars = as_character(val);
  if (chars.size() == 0) {
    return "";
  }
  return Rcpp::as<std::string>(chars[0]);
}

SEXP get_output_value(
    const std::shared_ptr<AppState>& state,
    const std::string& key
) {
  Rcpp::List output_values = get_sublist_or_empty(state->values, "output");
  if (output_values.containsElementNamed(key.c_str())) {
    return output_values[key];
  }
  return R_NilValue;
}

std::string get_input_string(
    const std::shared_ptr<AppState>& state,
    const std::string& id
) {
  Rcpp::List input_values = get_sublist_or_empty(state->values, "input");
  if (!input_values.containsElementNamed(id.c_str())) {
    return "";
  }
  return value_to_string(input_values[id]);
}

void run_handler_if_present(
    const Rcpp::List& handlers,
    const std::string& id,
    const std::shared_ptr<AppState>& state
) {
  if (!handlers.containsElementNamed(id.c_str())) {
    return;
  }
  Rcpp::Function fn = handlers[id];
  Rcpp::List new_state = fn(state->values);
  state->values = new_state;
}

void set_input_value(
    const std::shared_ptr<AppState>& state,
    const std::string& id,
    SEXP value
) {
  Rcpp::List input_values = get_sublist_or_empty(state->values, "input");
  input_values[id] = value;
  state->values["input"] = input_values;
}

void increment_button_input(
    const std::shared_ptr<AppState>& state,
    const std::string& id
) {
  Rcpp::List input_values = get_sublist_or_empty(state->values, "input");
  int current = 0;

  if (input_values.containsElementNamed(id.c_str())) {
    SEXP val = input_values[id];
    if (TYPEOF(val) == INTSXP || TYPEOF(val) == REALSXP) {
      current = Rcpp::as<int>(val);
    }
  }

  input_values[id] = current + 1;
  state->values["input"] = input_values;
}

}  // namespace

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

  // ── Text output (reads a key from state$output) ──────────────────────────

  if (type == "text") {
    std::string key = Rcpp::as<std::string>(node["key"]);
    return Renderer([state, key] {
      SEXP val = get_output_value(state, key);
      return text(value_to_string(val));
    });
  }

  // ── Button ───────────────────────────────────────────────────────────────

  if (type == "button") {
    std::string label   = Rcpp::as<std::string>(node["label"]);
    std::string id = Rcpp::as<std::string>(node["id"]);

    return Button(label, [state, handlers, id] {
      increment_button_input(state, id);
      run_handler_if_present(handlers, id, state);
    });
  }

  // ── Text input ───────────────────────────────────────────────────────────

  if (type == "input") {
    std::string id          = Rcpp::as<std::string>(node["id"]);
    std::string placeholder = Rcpp::as<std::string>(node["placeholder"]);

    auto content = std::make_shared<std::string>(get_input_string(state, id));

    InputOption opts;
    opts.placeholder = placeholder;
    opts.on_change = [state, handlers, id, content] {
      set_input_value(state, id, Rcpp::wrap(*content));
      run_handler_if_present(handlers, id, state);
    };

    return Input(content.get(), opts);
  }

  // Fallback: empty renderer for unknown types
  return Renderer([] { return text(""); });
}
