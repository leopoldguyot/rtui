#include "ui_tree.h"

#include <ftxui/component/component.hpp>
#include <ftxui/component/component_options.hpp>
#include <ftxui/component/component_base.hpp>
#include <ftxui/dom/elements.hpp>
#include <ftxui/screen/color.hpp>

#include <cctype>
#include <cstdint>
#include <memory>
#include <optional>
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
    const std::string& output_id
) {
  Rcpp::List output_values = get_sublist_or_empty(state->values, "output");
  if (output_values.containsElementNamed(output_id.c_str())) {
    return output_values[output_id];
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

std::string ascii_lower(std::string value) {
  for (char& ch : value) {
    ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
  }
  return value;
}

bool is_single_string(SEXP value) {
  return TYPEOF(value) == STRSXP &&
      Rf_length(value) == 1 &&
      STRING_ELT(value, 0) != NA_STRING;
}

bool is_hex_color(const std::string& value) {
  if (value.size() != 7 || value[0] != '#') {
    return false;
  }

  for (size_t i = 1; i < value.size(); ++i) {
    if (std::isxdigit(static_cast<unsigned char>(value[i])) == 0) {
      return false;
    }
  }
  return true;
}

uint8_t hex_pair_to_byte(char high, char low) {
  auto nibble = [](char ch) -> int {
    unsigned char value = static_cast<unsigned char>(ch);
    if (value >= '0' && value <= '9') {
      return static_cast<int>(value - '0');
    }
    value = static_cast<unsigned char>(std::tolower(value));
    if (value >= 'a' && value <= 'f') {
      return static_cast<int>(10 + value - 'a');
    }
    return -1;
  };

  int high_value = nibble(high);
  int low_value = nibble(low);
  return static_cast<uint8_t>(high_value * 16 + low_value);
}

Color named_button_color(const std::string& normalized) {
  if (normalized == "default") return Color::Default;
  if (normalized == "black") return Color::Black;
  if (normalized == "red") return Color::Red;
  if (normalized == "green") return Color::Green;
  if (normalized == "yellow") return Color::Yellow;
  if (normalized == "blue") return Color::Blue;
  if (normalized == "magenta") return Color::Magenta;
  if (normalized == "cyan") return Color::Cyan;
  if (normalized == "graylight") return Color::GrayLight;
  if (normalized == "graydark") return Color::GrayDark;
  if (normalized == "redlight") return Color::RedLight;
  if (normalized == "greenlight") return Color::GreenLight;
  if (normalized == "yellowlight") return Color::YellowLight;
  if (normalized == "bluelight") return Color::BlueLight;
  if (normalized == "magentalight") return Color::MagentaLight;
  if (normalized == "cyanlight") return Color::CyanLight;
  if (normalized == "white") return Color::White;
  Rcpp::stop("Unsupported button color `%s`.", normalized.c_str());
  return Color::Default;
}

std::optional<Color> parse_optional_color(const Rcpp::List& node) {
  if (!node.containsElementNamed("color")) {
    return std::nullopt;
  }

  SEXP candidate = node["color"];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`color` must be a single character string.");
  }

  std::string normalized = ascii_lower(Rcpp::as<std::string>(candidate));
  if (normalized == "gray" || normalized == "grey" || normalized == "greylight") {
    normalized = "graylight";
  }
  if (normalized == "greydark") {
    normalized = "graydark";
  }

  if (is_hex_color(normalized)) {
    uint8_t red = hex_pair_to_byte(normalized[1], normalized[2]);
    uint8_t green = hex_pair_to_byte(normalized[3], normalized[4]);
    uint8_t blue = hex_pair_to_byte(normalized[5], normalized[6]);
    return Color::RGB(red, green, blue);
  }

  return named_button_color(normalized);
}

std::optional<std::string> parse_optional_title(const Rcpp::List& node) {
  if (!node.containsElementNamed("title")) {
    return std::nullopt;
  }

  SEXP candidate = node["title"];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`title` must be a single character string.");
  }

  return Rcpp::as<std::string>(candidate);
}

BorderStyle parse_box_style(const Rcpp::List& node) {
  if (!node.containsElementNamed("style")) {
    return ROUNDED;
  }

  SEXP candidate = node["style"];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`style` must be a single character string.");
  }

  std::string style = ascii_lower(Rcpp::as<std::string>(candidate));
  if (style == "rounded") return ROUNDED;
  if (style == "light") return LIGHT;
  if (style == "dashed") return DASHED;
  if (style == "heavy") return HEAVY;
  if (style == "double") return DOUBLE;
  if (style == "empty") return EMPTY;

  Rcpp::stop("Unsupported box style `%s`.", style.c_str());
  return ROUNDED;
}

std::string parse_box_title_style(const Rcpp::List& node) {
  if (!node.containsElementNamed("titleStyle")) {
    return "header";
  }

  SEXP candidate = node["titleStyle"];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`titleStyle` must be a single character string.");
  }

  std::string title_style = ascii_lower(Rcpp::as<std::string>(candidate));
  if (title_style != "header" && title_style != "border") {
    Rcpp::stop("Unsupported box title style `%s`.", title_style.c_str());
  }
  return title_style;
}

std::string box_left_corner(BorderStyle style) {
  switch (style) {
    case LIGHT:
      return "┌";
    case DASHED:
      return "┏";
    case HEAVY:
      return "┏";
    case DOUBLE:
      return "╔";
    case ROUNDED:
      return "╭";
    case EMPTY:
      return " ";
  }
  return "┌";
}

bool parse_input_multiline(const Rcpp::List& node) {
  if (!node.containsElementNamed("multiline")) {
    return false;
  }

  SEXP candidate = node["multiline"];
  if (TYPEOF(candidate) != LGLSXP ||
      Rf_length(candidate) != 1 ||
      LOGICAL(candidate)[0] == NA_LOGICAL) {
    Rcpp::stop("`multiline` must be TRUE or FALSE.");
  }
  return Rcpp::as<bool>(candidate);
}

}  // namespace

ftxui::Component build_component(
    const Rcpp::List& node,
    std::shared_ptr<AppState> state,
    const Rcpp::List& handlers
) {
  std::string type = Rcpp::as<std::string>(node["type"]);

  // ── Layout containers ────────────────────────────────────────────────────

  if (type == "column" || type == "row") {
    Rcpp::List children = node["children"];
    Components comps;
    for (int i = 0; i < children.size(); ++i) {
      comps.push_back(
        build_component(Rcpp::as<Rcpp::List>(children[i]), state, handlers)
      );
    }
    if (type == "column")
      return Container::Vertical(comps);
    else
      return Container::Horizontal(comps);
  }

  // ── Output text / numeric (reads from state$output) ──────────────────────

  if (type == "outputText" || type == "outputNumeric") {
    std::string output_id = Rcpp::as<std::string>(node["outputId"]);
    return Renderer([state, output_id] {
      SEXP val = get_output_value(state, output_id);
      return text(value_to_string(val));
    });
  }

  // ── Button ───────────────────────────────────────────────────────────────

  if (type == "button") {
    std::string label   = Rcpp::as<std::string>(node["label"]);
    std::string id = Rcpp::as<std::string>(node["id"]);
    std::optional<Color> button_color = parse_optional_color(node);

    ButtonOption option = ButtonOption::Simple();
    if (button_color.has_value()) {
      const Color resolved_color = *button_color;
      option.transform = [resolved_color](const EntryState& state) {
        auto element = text(state.label) | borderLight;
        element |= ftxui::color(resolved_color);
        if (state.focused) {
          element |= inverted;
        }
        return element;
      };
    }

    return Button(label, [state, handlers, id] {
      increment_button_input(state, id);
      run_handler_if_present(handlers, id, state);
    }, option);
  }

  // ── Box wrapper ───────────────────────────────────────────────────────────

  if (type == "box") {
    if (!node.containsElementNamed("child")) {
      Rcpp::stop("`box` component requires a `child` field.");
    }

    Component child = build_component(Rcpp::as<Rcpp::List>(node["child"]), state, handlers);
    BorderStyle style = parse_box_style(node);
    std::optional<Color> box_color = parse_optional_color(node);
    std::optional<std::string> title = parse_optional_title(node);
    std::string title_style = parse_box_title_style(node);

    return Renderer(child, [child, style, box_color, title, title_style] {
      Element content = child->Render();
      Element bordered = box_color.has_value()
          ? content | borderStyled(style, *box_color)
          : content | borderStyled(style);

      if (title.has_value()) {
        Element title_element = text(*title) | bold;

        if (title_style == "border") {
          Element overlay;
          if (box_color.has_value()) {
            overlay = hbox({
              text(box_left_corner(style)) | ftxui::color(*box_color),
              text(std::string(" ") + *title + " ") | bold
            });
          } else {
            overlay = text(box_left_corner(style) + std::string(" ") + *title + " ") | bold;
          }
          return dbox({std::move(bordered), std::move(overlay)});
        }

        Element separator_line = separatorStyled(style);

        Element inner = vbox({
          std::move(title_element),
          std::move(separator_line),
          content
        });

        if (box_color.has_value()) {
          return inner | borderStyled(style, *box_color);
        }
        return inner | borderStyled(style);
      }

      return bordered;
    });
  }

  // ── Text input ───────────────────────────────────────────────────────────

  if (type == "input") {
    std::string id          = Rcpp::as<std::string>(node["id"]);
    std::string placeholder = Rcpp::as<std::string>(node["placeholder"]);
    bool multiline = parse_input_multiline(node);

    auto content = std::make_shared<std::string>(get_input_string(state, id));

    InputOption opts;
    opts.placeholder = placeholder;
    opts.multiline = multiline;
    opts.on_change = [state, handlers, id, content] {
      set_input_value(state, id, Rcpp::wrap(*content));
      run_handler_if_present(handlers, id, state);
    };

    return Input(content.get(), opts);
  }

  // Fallback: empty renderer for unknown types
  return Renderer([] { return text(""); });
}
