#include "ui_tree.h"

#include <ftxui/component/component.hpp>
#include <ftxui/component/component_options.hpp>
#include <ftxui/component/component_base.hpp>
#include <ftxui/component/event.hpp>
#include <ftxui/dom/elements.hpp>
#include <ftxui/dom/node.hpp>
#include <ftxui/dom/node_decorator.hpp>
#include <ftxui/dom/table.hpp>
#include <ftxui/screen/color.hpp>
#include <ftxui/screen/terminal.hpp>
#include <ftxui/screen/string.hpp>
#include <ftxui/util/autoreset.hpp>

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cmath>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using namespace ftxui;

namespace {

constexpr const char* kTerminalWidthId = "terminalWidth";
constexpr const char* kTerminalHeightId = "terminalHeight";

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

struct SerializedTableOutput {
  std::vector<std::string> columns;
  std::vector<std::vector<std::string>> rows;
};

std::vector<std::string> parse_string_cells(SEXP value) {
  std::vector<std::string> cells;
  if (Rf_isNull(value) || Rf_length(value) == 0) {
    return cells;
  }

  if (TYPEOF(value) == STRSXP) {
    Rcpp::CharacterVector chars(value);
    cells.reserve(chars.size());
    for (int i = 0; i < chars.size(); ++i) {
      if (chars[i] == NA_STRING) {
        cells.push_back("NA");
      } else {
        cells.push_back(Rcpp::as<std::string>(chars[i]));
      }
    }
    return cells;
  }

  if (TYPEOF(value) == VECSXP) {
    Rcpp::List list(value);
    cells.reserve(list.size());
    for (int i = 0; i < list.size(); ++i) {
      cells.push_back(value_to_string(list[i]));
    }
    return cells;
  }

  Rcpp::Function as_character("as.character");
  Rcpp::CharacterVector chars = as_character(value);
  cells.reserve(chars.size());
  for (int i = 0; i < chars.size(); ++i) {
    if (chars[i] == NA_STRING) {
      cells.push_back("NA");
    } else {
      cells.push_back(Rcpp::as<std::string>(chars[i]));
    }
  }
  return cells;
}

SerializedTableOutput parse_serialized_table_output(SEXP value) {
  SerializedTableOutput table;
  if (Rf_isNull(value) || TYPEOF(value) != VECSXP) {
    return table;
  }

  Rcpp::List serialized(value);
  if (!serialized.containsElementNamed("columns") ||
      !serialized.containsElementNamed("rows")) {
    return table;
  }

  table.columns = parse_string_cells(serialized["columns"]);
  if (table.columns.empty()) {
    return table;
  }

  SEXP rows_value = serialized["rows"];
  if (TYPEOF(rows_value) != VECSXP) {
    return table;
  }

  Rcpp::List rows(rows_value);
  table.rows.reserve(rows.size());
  for (int i = 0; i < rows.size(); ++i) {
    std::vector<std::string> row = parse_string_cells(rows[i]);
    if (row.size() < table.columns.size()) {
      row.resize(table.columns.size(), "");
    } else if (row.size() > table.columns.size()) {
      row.resize(table.columns.size());
    }
    table.rows.push_back(std::move(row));
  }

  return table;
}

Element render_serialized_table_output(SEXP value) {
  SerializedTableOutput table_data = parse_serialized_table_output(value);
  if (table_data.columns.empty()) {
    return text("");
  }

  std::vector<std::vector<std::string>> rows;
  rows.reserve(table_data.rows.size() + 1);
  rows.push_back(table_data.columns);
  for (const auto& row : table_data.rows) {
    rows.push_back(row);
  }

  Table table(std::move(rows));
  table.SelectAll().Border(LIGHT);
  table.SelectAll().SeparatorVertical(LIGHT);
  table.SelectRow(0).Decorate(bold);
  table.SelectRow(0).SeparatorHorizontal(LIGHT);

  return table.Render() | xflex;
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

int get_input_integer(
    const std::shared_ptr<AppState>& state,
    const std::string& id,
    int fallback
) {
  Rcpp::List input_values = get_sublist_or_empty(state->values, "input");
  if (!input_values.containsElementNamed(id.c_str())) {
    return fallback;
  }

  SEXP val = input_values[id];
  if (TYPEOF(val) == INTSXP && Rf_length(val) == 1 && INTEGER(val)[0] != NA_INTEGER) {
    return INTEGER(val)[0];
  }
  if (TYPEOF(val) == REALSXP && Rf_length(val) == 1 && !ISNAN(REAL(val)[0])) {
    return static_cast<int>(REAL(val)[0]);
  }
  if (TYPEOF(val) == LGLSXP && Rf_length(val) == 1 && LOGICAL(val)[0] != NA_LOGICAL) {
    return LOGICAL(val)[0] == TRUE ? 1 : 0;
  }
  return fallback;
}

bool get_input_bool(
    const std::shared_ptr<AppState>& state,
    const std::string& id
) {
  auto lower = [](std::string value) {
    for (char& ch : value) {
      ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
    }
    return value;
  };

  Rcpp::List input_values = get_sublist_or_empty(state->values, "input");
  if (!input_values.containsElementNamed(id.c_str())) {
    return false;
  }

  SEXP val = input_values[id];
  if (TYPEOF(val) == LGLSXP && Rf_length(val) == 1 && LOGICAL(val)[0] != NA_LOGICAL) {
    return LOGICAL(val)[0] == TRUE;
  }
  if (TYPEOF(val) == INTSXP && Rf_length(val) == 1 && INTEGER(val)[0] != NA_INTEGER) {
    return INTEGER(val)[0] != 0;
  }
  if (TYPEOF(val) == REALSXP && Rf_length(val) == 1 && !ISNAN(REAL(val)[0])) {
    return REAL(val)[0] != 0.0;
  }

  if (TYPEOF(val) == STRSXP && Rf_length(val) == 1 && STRING_ELT(val, 0) != NA_STRING) {
    std::string normalized = lower(Rcpp::as<std::string>(val));
    if (normalized == "true" || normalized == "1") {
      return true;
    }
    if (normalized == "false" || normalized == "0") {
      return false;
    }
  }

  return false;
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

Color named_ui_color(const std::string& normalized) {
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
  Rcpp::stop("Unsupported color `%s`.", normalized.c_str());
  return Color::Default;
}

std::optional<Color> parse_optional_color(
    const Rcpp::List& node,
    const char* field_name = "color",
    const char* arg_name = "color"
) {
  if (!node.containsElementNamed(field_name)) {
    return std::nullopt;
  }

  SEXP candidate = node[field_name];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`%s` must be a single character string.", arg_name);
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

  return named_ui_color(normalized);
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

std::string parse_box_title_align(const Rcpp::List& node) {
  if (!node.containsElementNamed("titleAlign")) {
    return "left";
  }

  SEXP candidate = node["titleAlign"];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`titleAlign` must be a single character string.");
  }

  std::string title_align = ascii_lower(Rcpp::as<std::string>(candidate));
  if (title_align != "left" && title_align != "center" && title_align != "right") {
    Rcpp::stop("Unsupported box title align `%s`.", title_align.c_str());
  }
  return title_align;
}

int parse_box_margin(const Rcpp::List& node) {
  if (!node.containsElementNamed("margin")) {
    return 0;
  }

  SEXP candidate = node["margin"];
  if (TYPEOF(candidate) == INTSXP) {
    if (Rf_length(candidate) != 1 || INTEGER(candidate)[0] == NA_INTEGER) {
      Rcpp::stop("`margin` must be a single non-negative integer.");
    }
  } else if (TYPEOF(candidate) == REALSXP) {
    if (Rf_length(candidate) != 1 || ISNAN(REAL(candidate)[0])) {
      Rcpp::stop("`margin` must be a single non-negative integer.");
    }
  } else {
    Rcpp::stop("`margin` must be a single non-negative integer.");
  }

  int margin = Rcpp::as<int>(candidate);
  if (margin < 0) {
    Rcpp::stop("`margin` must be a single non-negative integer.");
  }
  return margin;
}

std::optional<int> parse_optional_non_negative_integer(
    const Rcpp::List& node,
    const char* field_name,
    const char* arg_name
) {
  if (!node.containsElementNamed(field_name)) {
    return std::nullopt;
  }

  SEXP candidate = node[field_name];
  double raw = 0.0;
  if (TYPEOF(candidate) == INTSXP) {
    if (Rf_length(candidate) != 1 || INTEGER(candidate)[0] == NA_INTEGER) {
      Rcpp::stop("`%s` must be a single non-negative integer.", arg_name);
    }
    raw = static_cast<double>(INTEGER(candidate)[0]);
  } else if (TYPEOF(candidate) == REALSXP) {
    if (Rf_length(candidate) != 1 || ISNAN(REAL(candidate)[0])) {
      Rcpp::stop("`%s` must be a single non-negative integer.", arg_name);
    }
    raw = REAL(candidate)[0];
  } else {
    Rcpp::stop("`%s` must be a single non-negative integer.", arg_name);
  }

  if (raw < 0.0 || std::floor(raw) != raw) {
    Rcpp::stop("`%s` must be a single non-negative integer.", arg_name);
  }
  return static_cast<int>(raw);
}

std::optional<double> parse_optional_percent(
    const Rcpp::List& node,
    const char* field_name,
    const char* arg_name
) {
  if (!node.containsElementNamed(field_name)) {
    return std::nullopt;
  }

  SEXP candidate = node[field_name];
  if ((TYPEOF(candidate) != INTSXP && TYPEOF(candidate) != REALSXP) ||
      Rf_length(candidate) != 1) {
    Rcpp::stop(
      "`%s` must be a single numeric value between 0 and 1.",
      arg_name
    );
  }

  double value = Rcpp::as<double>(candidate);
  if (ISNAN(value) || value < 0.0 || value > 1.0) {
    Rcpp::stop(
      "`%s` must be a single numeric value between 0 and 1.",
      arg_name
    );
  }

  return value;
}

enum class OverflowMode {
  Visible,
  Clip,
  Scroll
};

OverflowMode parse_optional_overflow_mode(
    const Rcpp::List& node,
    const char* field_name,
    const char* arg_name
) {
  if (!node.containsElementNamed(field_name)) {
    return OverflowMode::Visible;
  }

  SEXP candidate = node[field_name];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`%s` must be a single character string.", arg_name);
  }

  const std::string mode = ascii_lower(Rcpp::as<std::string>(candidate));
  if (mode == "visible") return OverflowMode::Visible;
  if (mode == "clip") return OverflowMode::Clip;
  if (mode == "scroll") return OverflowMode::Scroll;

  Rcpp::stop(
    "`%s` must be one of 'visible', 'clip', or 'scroll'.",
    arg_name
  );
  return OverflowMode::Visible;
}

struct NodeOverflowSpec {
  OverflowMode overflow_x = OverflowMode::Visible;
  OverflowMode overflow_y = OverflowMode::Visible;

  bool has_overflow() const {
    return overflow_x != OverflowMode::Visible ||
      overflow_y != OverflowMode::Visible;
  }
};

NodeOverflowSpec parse_node_overflow_spec(const Rcpp::List& node) {
  return NodeOverflowSpec{
    parse_optional_overflow_mode(node, "overflowX", "overflowX"),
    parse_optional_overflow_mode(node, "overflowY", "overflowY")
  };
}

struct ShowIfSpec {
  std::optional<int> min_terminal_width;
  std::optional<int> max_terminal_width;
  std::optional<int> min_terminal_height;
  std::optional<int> max_terminal_height;

  bool has_constraints() const {
    return min_terminal_width.has_value() ||
      max_terminal_width.has_value() ||
      min_terminal_height.has_value() ||
      max_terminal_height.has_value();
  }
};

ShowIfSpec parse_show_if_spec(const Rcpp::List& node) {
  ShowIfSpec spec{
    parse_optional_non_negative_integer(
      node,
      "minTerminalWidth",
      "minTerminalWidth"
    ),
    parse_optional_non_negative_integer(
      node,
      "maxTerminalWidth",
      "maxTerminalWidth"
    ),
    parse_optional_non_negative_integer(
      node,
      "minTerminalHeight",
      "minTerminalHeight"
    ),
    parse_optional_non_negative_integer(
      node,
      "maxTerminalHeight",
      "maxTerminalHeight"
    )
  };

  if (spec.min_terminal_width.has_value() &&
      spec.max_terminal_width.has_value() &&
      *spec.min_terminal_width > *spec.max_terminal_width) {
    Rcpp::stop("`minTerminalWidth` must be less than or equal to `maxTerminalWidth`.");
  }
  if (spec.min_terminal_height.has_value() &&
      spec.max_terminal_height.has_value() &&
      *spec.min_terminal_height > *spec.max_terminal_height) {
    Rcpp::stop("`minTerminalHeight` must be less than or equal to `maxTerminalHeight`.");
  }

  return spec;
}

struct NodeSizeSpec {
  std::optional<int> width;
  std::optional<int> height;
  std::optional<int> min_height;
  std::optional<int> max_height;

  bool has_constraints() const {
    return width.has_value() ||
      height.has_value() ||
      min_height.has_value() ||
      max_height.has_value();
  }
};

NodeSizeSpec parse_node_size_spec(const Rcpp::List& node) {
  return NodeSizeSpec{
    parse_optional_non_negative_integer(node, "width", "width"),
    parse_optional_non_negative_integer(node, "height", "height"),
    parse_optional_non_negative_integer(node, "minHeight", "minHeight"),
    parse_optional_non_negative_integer(node, "maxHeight", "maxHeight")
  };
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

std::string box_right_corner(BorderStyle style) {
  switch (style) {
    case LIGHT:
      return "┐";
    case DASHED:
      return "┓";
    case HEAVY:
      return "┓";
    case DOUBLE:
      return "╗";
    case ROUNDED:
      return "╮";
    case EMPTY:
      return " ";
  }
  return "┐";
}

Element align_title_element(Element title_element, const std::string& title_align) {
  if (title_align == "center") {
    return hcenter(std::move(title_element));
  }
  if (title_align == "right") {
    return align_right(std::move(title_element));
  }
  return std::move(title_element);
}

Element apply_margin(Element element, int margin) {
  if (margin <= 0) {
    return element;
  }

  std::string side_padding(static_cast<size_t>(margin), ' ');
  Elements rows;
  rows.reserve(static_cast<size_t>(margin * 2 + 1));
  for (int i = 0; i < margin; ++i) {
    rows.push_back(text(""));
  }
  rows.push_back(hbox({
    text(side_padding),
    std::move(element) | xflex | yflex,
    text(side_padding)
  }) | xflex | yflex);
  for (int i = 0; i < margin; ++i) {
    rows.push_back(text(""));
  }
  return vbox(std::move(rows)) | xflex | yflex;
}

class BoldTextElement : public Node {
 public:
  explicit BoldTextElement(std::string text) : text_(std::move(text)) {}

  void ComputeRequirement() override {
    requirement_.min_x = string_width(text_);
    requirement_.min_y = 1;
  }

  void Render(Screen& screen) override {
    int x = box_.x_min;
    const int y = box_.y_min;
    if (y > box_.y_max) {
      return;
    }

    for (const auto& glyph : Utf8ToGlyphs(text_)) {
      if (x > box_.x_max) {
        break;
      }
      if (glyph == "\n") {
        continue;
      }
      Pixel& pixel = screen.PixelAt(x, y);
      pixel.character = glyph;
      pixel.bold = true;
      ++x;
    }
  }

 private:
  std::string text_;
};

class BoxTopLineOverlayElement : public Node {
 public:
  BoxTopLineOverlayElement(BorderStyle style, std::string title, std::string title_align)
      : style_(style), title_(std::move(title)), title_align_(std::move(title_align)) {}

  void ComputeRequirement() override {
    requirement_.min_x = 0;
    requirement_.min_y = 0;
  }

  void Render(Screen& screen) override {
    const int y = box_.y_min;
    if (y > box_.y_max) {
      return;
    }

    const int x_min = box_.x_min;
    const int x_max = box_.x_max;
    auto write_bold = [&screen, y, x_min, x_max](int x, const std::string& glyph) {
      if (glyph == "\n") {
        return;
      }
      if (x < x_min || x > x_max) {
        return;
      }
      Pixel& pixel = screen.PixelAt(x, y);
      pixel.character = glyph;
      pixel.bold = true;
    };

    auto write_text = [&write_bold](int start_x, const std::string& text) {
      int x = start_x;
      for (const auto& glyph : Utf8ToGlyphs(text)) {
        write_bold(x, glyph);
        ++x;
      }
    };

    const std::string left_corner = box_left_corner(style_);
    const std::string right_corner = box_right_corner(style_);
    const std::string title_with_padding = std::string(" ") + title_ + " ";
    const int full_width = box_.x_max - box_.x_min + 1;
    const int title_width = string_width(title_with_padding);

    if (title_align_ == "left") {
      write_text(box_.x_min, left_corner);
      write_text(box_.x_min + 1, title_with_padding);
      return;
    }

    if (title_align_ == "right") {
      const int start = std::max(box_.x_min, box_.x_max - title_width);
      write_text(start, title_with_padding);
      write_text(box_.x_max, right_corner);
      return;
    }

    int start = box_.x_min + (full_width - title_width) / 2;
    start = std::max(box_.x_min + 1, start);
    if (start + title_width > box_.x_max) {
      start = std::max(box_.x_min + 1, box_.x_max - title_width);
    }
    write_text(start, title_with_padding);
  }

 private:
  BorderStyle style_;
  std::string title_;
  std::string title_align_;
};

Element box_top_line_overlay(
    BorderStyle style,
    const std::string& title,
    const std::string& title_align
) {
  return std::make_shared<BoxTopLineOverlayElement>(style, title, title_align);
}

class SizeConstraintElement : public NodeDecorator {
 public:
  SizeConstraintElement(Element child, NodeSizeSpec spec)
      : NodeDecorator(std::move(child)), spec_(std::move(spec)) {}

  void ComputeRequirement() override {
    NodeDecorator::ComputeRequirement();
    requirement_ = children_[0]->requirement();

    if (spec_.width.has_value()) {
      requirement_.min_x = *spec_.width;
      requirement_.flex_grow_x = 0;
      requirement_.flex_shrink_x = 0;
    }

    if (spec_.min_height.has_value()) {
      requirement_.min_y = std::max(requirement_.min_y, *spec_.min_height);
    }
    if (spec_.max_height.has_value()) {
      requirement_.min_y = std::min(requirement_.min_y, *spec_.max_height);
    }
    if (spec_.height.has_value()) {
      requirement_.min_y = *spec_.height;
      requirement_.flex_grow_y = 0;
      requirement_.flex_shrink_y = 0;
    }
  }

  void SetBox(Box box) override {
    box_ = box;
    Box child_box = box;

    auto resolve_axis_size = [](
        int available,
        const std::optional<int>& fixed,
        const std::optional<int>& min_value,
        const std::optional<int>& max_value
    ) {
      if (available <= 0) {
        return 0;
      }

      int size = available;
      if (fixed.has_value()) {
        size = *fixed;
      }
      if (min_value.has_value()) {
        size = std::max(size, *min_value);
      }
      if (max_value.has_value()) {
        size = std::min(size, *max_value);
      }
      return std::max(0, size);
    };

    const int available_width = std::max(0, box.x_max - box.x_min + 1);
    const int available_height = std::max(0, box.y_max - box.y_min + 1);

    const int resolved_width = resolve_axis_size(
      available_width,
      spec_.width,
      std::nullopt,
      std::nullopt
    );
    const int resolved_height = resolve_axis_size(
      available_height,
      spec_.height,
      spec_.min_height,
      spec_.max_height
    );

    child_box.x_max = child_box.x_min + resolved_width - 1;
    child_box.y_max = child_box.y_min + resolved_height - 1;
    children_[0]->SetBox(child_box);
  }

 private:
  NodeSizeSpec spec_;
};

Element apply_size_constraints(Element element, const NodeSizeSpec& spec) {
  if (!spec.has_constraints()) {
    return element;
  }
  return std::make_shared<SizeConstraintElement>(std::move(element), spec);
}

class AxisClipElement : public NodeDecorator {
 public:
  AxisClipElement(Element child, bool clip_x, bool clip_y)
      : NodeDecorator(std::move(child)), clip_x_(clip_x), clip_y_(clip_y) {}

  void ComputeRequirement() override {
    NodeDecorator::ComputeRequirement();
    requirement_ = children_[0]->requirement();
  }

  void Render(Screen& screen) override {
    Box stencil = screen.stencil;
    if (clip_x_) {
      stencil.x_min = std::max(stencil.x_min, box_.x_min);
      stencil.x_max = std::min(stencil.x_max, box_.x_max);
    }
    if (clip_y_) {
      stencil.y_min = std::max(stencil.y_min, box_.y_min);
      stencil.y_max = std::min(stencil.y_max, box_.y_max);
    }
    const AutoReset<Box> reset(
      &screen.stencil,
      Box::Intersection(stencil, screen.stencil)
    );
    children_[0]->Render(screen);
  }

 private:
  bool clip_x_;
  bool clip_y_;
};

Element clip_axes(Element element, bool clip_x, bool clip_y) {
  if (!clip_x && !clip_y) {
    return element;
  }
  return std::make_shared<AxisClipElement>(
    std::move(element),
    clip_x,
    clip_y
  );
}

struct OverflowScrollState {
  std::shared_ptr<int> scroll_x = std::make_shared<int>(0);
  std::shared_ptr<int> scroll_y = std::make_shared<int>(0);
  std::shared_ptr<int> max_scroll_x = std::make_shared<int>(0);
  std::shared_ptr<int> max_scroll_y = std::make_shared<int>(0);
};

class ManualOverflowFrameElement : public NodeDecorator {
 public:
  ManualOverflowFrameElement(
      Element child,
      bool scroll_x,
      bool scroll_y,
      bool clip_x,
      bool clip_y,
      OverflowScrollState state
  )
      : NodeDecorator(std::move(child)),
        scroll_x_enabled_(scroll_x),
        scroll_y_enabled_(scroll_y),
        clip_x_(clip_x),
        clip_y_(clip_y),
        state_(std::move(state)) {}

  void ComputeRequirement() override {
    NodeDecorator::ComputeRequirement();
    requirement_ = children_[0]->requirement();
  }

  void SetBox(Box box) override {
    box_ = box;
    Box child_box = box;

    const int viewport_width = std::max(0, box.x_max - box.x_min + 1);
    const int viewport_height = std::max(0, box.y_max - box.y_min + 1);
    const int vertical_slider_width = scroll_y_enabled_ ? 1 : 0;
    const int horizontal_slider_height = scroll_x_enabled_ ? 1 : 0;

    child_viewport_width_ = std::max(0, viewport_width - vertical_slider_width);
    child_viewport_height_ = std::max(0, viewport_height - horizontal_slider_height);

    intrinsic_width_ = std::max(requirement_.min_x, child_viewport_width_);
    intrinsic_height_ = std::max(requirement_.min_y, child_viewport_height_);

    const int max_x = scroll_x_enabled_
        ? std::max(0, intrinsic_width_ - child_viewport_width_)
        : 0;
    const int max_y = scroll_y_enabled_
        ? std::max(0, intrinsic_height_ - child_viewport_height_)
        : 0;
    *state_.max_scroll_x = max_x;
    *state_.max_scroll_y = max_y;

    *state_.scroll_x = std::clamp(*state_.scroll_x, 0, max_x);
    *state_.scroll_y = std::clamp(*state_.scroll_y, 0, max_y);

    child_box.x_min = box.x_min - (scroll_x_enabled_ ? *state_.scroll_x : 0);
    child_box.x_max = child_box.x_min + intrinsic_width_ - 1;
    child_box.y_min = box.y_min - (scroll_y_enabled_ ? *state_.scroll_y : 0);
    child_box.y_max = child_box.y_min + intrinsic_height_ - 1;
    children_[0]->SetBox(child_box);
  }

  void Render(Screen& screen) override {
    Box stencil = screen.stencil;
    if (clip_x_) {
      stencil.x_min = std::max(stencil.x_min, box_.x_min);
      stencil.x_max = std::min(stencil.x_max, box_.x_max);
    }
    if (clip_y_) {
      stencil.y_min = std::max(stencil.y_min, box_.y_min);
      stencil.y_max = std::min(stencil.y_max, box_.y_max);
    }
    const AutoReset<Box> reset(
      &screen.stencil,
      Box::Intersection(stencil, screen.stencil)
    );
    children_[0]->Render(screen);
    render_vertical_slider(screen);
    render_horizontal_slider(screen);
  }

 private:
  void render_vertical_slider(Screen& screen) const {
    if (!scroll_y_enabled_) {
      return;
    }

    const int track_x = box_.x_max;
    const int track_top = box_.y_min;
    const int track_bottom = box_.y_max - (scroll_x_enabled_ ? 1 : 0);
    const int track_size = track_bottom - track_top + 1;
    if (track_size <= 0 || child_viewport_height_ <= 0) {
      return;
    }

    const bool has_overflow = *state_.max_scroll_y > 0;
    int thumb_size = track_size;
    int thumb_start = 0;
    if (has_overflow) {
      thumb_size = std::max(
        1,
        (child_viewport_height_ * track_size) / std::max(1, intrinsic_height_)
      );
      const int max_thumb_start = std::max(0, track_size - thumb_size);
      thumb_start = (*state_.scroll_y * max_thumb_start) / std::max(1, *state_.max_scroll_y);
    }

    for (int y = track_top; y <= track_bottom; ++y) {
      const int offset = y - track_top;
      const bool in_thumb = has_overflow &&
        offset >= thumb_start &&
        offset < thumb_start + thumb_size;
      Pixel& pixel = screen.PixelAt(track_x, y);
      pixel.character = in_thumb ? "█" : "│";
    }
  }

  void render_horizontal_slider(Screen& screen) const {
    if (!scroll_x_enabled_) {
      return;
    }

    const int track_y = box_.y_max;
    const int track_left = box_.x_min;
    const int track_right = box_.x_max - (scroll_y_enabled_ ? 1 : 0);
    const int track_size = track_right - track_left + 1;
    if (track_size <= 0 || child_viewport_width_ <= 0) {
      return;
    }

    const bool has_overflow = *state_.max_scroll_x > 0;
    int thumb_size = track_size;
    int thumb_start = 0;
    if (has_overflow) {
      thumb_size = std::max(
        1,
        (child_viewport_width_ * track_size) / std::max(1, intrinsic_width_)
      );
      const int max_thumb_start = std::max(0, track_size - thumb_size);
      thumb_start = (*state_.scroll_x * max_thumb_start) / std::max(1, *state_.max_scroll_x);
    }

    for (int x = track_left; x <= track_right; ++x) {
      const int offset = x - track_left;
      const bool in_thumb = has_overflow &&
        offset >= thumb_start &&
        offset < thumb_start + thumb_size;
      Pixel& pixel = screen.PixelAt(x, track_y);
      pixel.character = in_thumb ? "█" : "─";
    }
  }

  bool scroll_x_enabled_;
  bool scroll_y_enabled_;
  bool clip_x_;
  bool clip_y_;
  OverflowScrollState state_;
  int child_viewport_width_ = 0;
  int child_viewport_height_ = 0;
  int intrinsic_width_ = 0;
  int intrinsic_height_ = 0;
};

Element manual_overflow_frame(
    Element element,
    bool scroll_x,
    bool scroll_y,
    bool clip_x,
    bool clip_y,
    const OverflowScrollState& state
) {
  return std::make_shared<ManualOverflowFrameElement>(
    std::move(element),
    scroll_x,
    scroll_y,
    clip_x,
    clip_y,
    state
  );
}

Element apply_overflow_constraints(
    Element element,
    const NodeOverflowSpec& spec,
    const OverflowScrollState* state = nullptr
) {
  const bool scroll_x = spec.overflow_x == OverflowMode::Scroll;
  const bool scroll_y = spec.overflow_y == OverflowMode::Scroll;
  const bool clip_x = spec.overflow_x != OverflowMode::Visible;
  const bool clip_y = spec.overflow_y != OverflowMode::Visible;

  if (scroll_x || scroll_y) {
    if (state == nullptr) {
      return element;
    }
    return manual_overflow_frame(
      std::move(element),
      scroll_x,
      scroll_y,
      clip_x,
      clip_y,
      *state
    );
  }

  return clip_axes(
    std::move(element),
    spec.overflow_x == OverflowMode::Clip,
    spec.overflow_y == OverflowMode::Clip
  );
}

class ShowIfElement : public NodeDecorator {
 public:
  ShowIfElement(
      Element child,
      ShowIfSpec spec,
      std::shared_ptr<AppState> state
  )
      : NodeDecorator(std::move(child)),
        spec_(std::move(spec)),
        state_(std::move(state)) {}

  bool should_show() const {
    const Dimensions terminal = Terminal::Size();
    int width = std::max(0, terminal.dimx);
    int height = std::max(0, terminal.dimy);

    if (state_) {
      width = std::max(
        0,
        get_input_integer(state_, kTerminalWidthId, width)
      );
      height = std::max(
        0,
        get_input_integer(state_, kTerminalHeightId, height)
      );
    }

    if (spec_.min_terminal_width.has_value() && width < *spec_.min_terminal_width) {
      return false;
    }
    if (spec_.max_terminal_width.has_value() && width > *spec_.max_terminal_width) {
      return false;
    }
    if (spec_.min_terminal_height.has_value() && height < *spec_.min_terminal_height) {
      return false;
    }
    if (spec_.max_terminal_height.has_value() && height > *spec_.max_terminal_height) {
      return false;
    }
    return true;
  }

  void ComputeRequirement() override {
    if (!should_show()) {
      requirement_ = Requirement{};
      return;
    }

    NodeDecorator::ComputeRequirement();
    requirement_ = children_[0]->requirement();
  }

  void SetBox(Box box) override {
    box_ = box;
    if (!should_show()) {
      Box hidden = box;
      hidden.x_max = hidden.x_min - 1;
      hidden.y_max = hidden.y_min - 1;
      children_[0]->SetBox(hidden);
      return;
    }
    children_[0]->SetBox(box);
  }

  void Render(Screen& screen) override {
    if (!should_show()) {
      return;
    }
    children_[0]->Render(screen);
  }

 private:
  ShowIfSpec spec_;
  std::shared_ptr<AppState> state_;
};

Element show_if(
    Element element,
    const ShowIfSpec& spec,
    std::shared_ptr<AppState> state
) {
  if (!spec.has_constraints()) {
    return element;
  }
  return std::make_shared<ShowIfElement>(
    std::move(element),
    spec,
    std::move(state)
  );
}

class WrappedTextElement : public NodeDecorator {
 public:
  explicit WrappedTextElement(Element child)
      : NodeDecorator(std::move(child)) {}

  void ComputeRequirement() override {
    NodeDecorator::ComputeRequirement();
    requirement_ = children_[0]->requirement();

    // Let wrapping text shrink to the actual available width instead of
    // enforcing its full unwrapped line width upstream.
    requirement_.min_x = std::min(requirement_.min_x, 1);
    requirement_.flex_shrink_x = std::max(requirement_.flex_shrink_x, 1);
  }
};

Element wrapped_text(Element element) {
  return std::make_shared<WrappedTextElement>(std::move(element));
}

class EllipsisTextElement : public Node {
 public:
  explicit EllipsisTextElement(std::string text)
      : text_(std::move(text)) {}

  void ComputeRequirement() override {
    requirement_.min_x = 1;
    requirement_.min_y = 1;
    requirement_.flex_shrink_x = 1;
  }

  void Render(Screen& screen) override {
    const int y = box_.y_min;
    const int width = box_.x_max - box_.x_min + 1;
    if (y > box_.y_max || width <= 0) {
      return;
    }

    std::string line = text_;
    const std::string::size_type newline = line.find('\n');
    if (newline != std::string::npos) {
      line = line.substr(0, newline);
    }

    const std::vector<std::string> glyphs = Utf8ToGlyphs(line);
    const bool truncated = static_cast<int>(glyphs.size()) > width;
    const int visible_glyph_count = truncated
        ? std::max(0, width - 1)
        : std::min(width, static_cast<int>(glyphs.size()));

    int x = box_.x_min;
    for (int i = 0; i < visible_glyph_count && x <= box_.x_max; ++i, ++x) {
      if (glyphs[static_cast<size_t>(i)] == "\n") {
        continue;
      }
      screen.PixelAt(x, y).character = glyphs[static_cast<size_t>(i)];
    }

    if (truncated && x <= box_.x_max) {
      screen.PixelAt(x, y).character = "…";
    }
  }

 private:
  std::string text_;
};

Element ellipsis_text(const std::string& text) {
  return std::make_shared<EllipsisTextElement>(text);
}

std::vector<int> allocate_axis_sizes(
    int total_size,
    const std::vector<int>& min_sizes,
    const std::vector<int>& flex_grow,
    const std::vector<std::optional<double>>& percents
) {
  const size_t count = min_sizes.size();
  std::vector<int> sizes(count, 0);
  if (count == 0 || total_size <= 0) {
    return sizes;
  }

  std::vector<size_t> specified_indices;
  std::vector<size_t> unspecified_indices;
  specified_indices.reserve(count);
  unspecified_indices.reserve(count);

  int assigned = 0;
  for (size_t i = 0; i < count; ++i) {
    if (i < percents.size() && percents[i].has_value()) {
      const int size = std::max(
        0,
        static_cast<int>(std::round(percents[i].value() * total_size))
      );
      sizes[i] = size;
      assigned += size;
      specified_indices.push_back(i);
    } else {
      unspecified_indices.push_back(i);
    }
  }

  if (assigned > total_size) {
    int overflow = assigned - total_size;
    for (auto it = specified_indices.rbegin(); it != specified_indices.rend(); ++it) {
      size_t index = *it;
      const int reduce = std::min(overflow, sizes[index]);
      sizes[index] -= reduce;
      overflow -= reduce;
      if (overflow == 0) {
        break;
      }
    }
  }

  int remaining = total_size;
  for (int size : sizes) {
    remaining -= size;
  }
  remaining = std::max(0, remaining);

  if (!unspecified_indices.empty()) {
    int min_sum = 0;
    for (size_t index : unspecified_indices) {
      min_sum += std::max(0, min_sizes[index]);
    }

    if (min_sum <= remaining) {
      for (size_t index : unspecified_indices) {
        sizes[index] = std::max(0, min_sizes[index]);
      }

      int extra = remaining - min_sum;
      if (extra > 0) {
        std::vector<int> weights;
        weights.reserve(unspecified_indices.size());
        int weight_sum = 0;
        for (size_t index : unspecified_indices) {
          const int weight = std::max(1, flex_grow[index]);
          weights.push_back(weight);
          weight_sum += weight;
        }

        int extra_left = extra;
        int weight_left = weight_sum;
        for (size_t i = 0; i < unspecified_indices.size(); ++i) {
          const int add = weight_left > 0
              ? (extra_left * weights[i]) / weight_left
              : 0;
          sizes[unspecified_indices[i]] += add;
          extra_left -= add;
          weight_left -= weights[i];
        }
        for (size_t i = 0; extra_left > 0 && !unspecified_indices.empty(); ++i, --extra_left) {
          sizes[unspecified_indices[i % unspecified_indices.size()]] += 1;
        }
      }
    } else if (min_sum > 0) {
      int remaining_left = remaining;
      int min_left = min_sum;
      for (size_t index : unspecified_indices) {
        const int min_size = std::max(0, min_sizes[index]);
        const int allocated = min_left > 0
            ? (remaining_left * min_size) / min_left
            : 0;
        sizes[index] = allocated;
        remaining_left -= allocated;
        min_left -= min_size;
      }
      for (size_t i = 0; remaining_left > 0 && !unspecified_indices.empty(); ++i, --remaining_left) {
        sizes[unspecified_indices[i % unspecified_indices.size()]] += 1;
      }
    } else {
      const int base = remaining / static_cast<int>(unspecified_indices.size());
      int rest = remaining % static_cast<int>(unspecified_indices.size());
      for (size_t index : unspecified_indices) {
        sizes[index] = base + (rest > 0 ? 1 : 0);
        if (rest > 0) {
          --rest;
        }
      }
    }
  } else if (!sizes.empty() && remaining > 0) {
    sizes.back() += remaining;
  }

  int total_allocated = 0;
  for (int size : sizes) {
    total_allocated += size;
  }
  if (!sizes.empty() && total_allocated < total_size) {
    sizes.back() += total_size - total_allocated;
  }
  if (!sizes.empty() && total_allocated > total_size) {
    int overflow = total_allocated - total_size;
    for (auto it = sizes.rbegin(); it != sizes.rend() && overflow > 0; ++it) {
      const int reduce = std::min(overflow, *it);
      *it -= reduce;
      overflow -= reduce;
    }
  }

  return sizes;
}

class StrictPercentHBoxElement : public Node {
 public:
  StrictPercentHBoxElement(Elements children, std::vector<std::optional<double>> percents)
      : Node(std::move(children)), percents_(std::move(percents)) {}

  void ComputeRequirement() override {
    requirement_ = Requirement{};

    for (auto& child : children_) {
      child->ComputeRequirement();

      if (requirement_.focused.Prefer(child->requirement().focused)) {
        requirement_.focused = child->requirement().focused;
        requirement_.focused.box.Shift(requirement_.min_x, 0);
      }

      requirement_.min_x += child->requirement().min_x;
      requirement_.min_y = std::max(requirement_.min_y, child->requirement().min_y);
    }
  }

  void SetBox(Box box) override {
    Node::SetBox(box);
    const int total_width = std::max(0, box.x_max - box.x_min + 1);

    std::vector<int> min_sizes;
    std::vector<int> flex_grow;
    min_sizes.reserve(children_.size());
    flex_grow.reserve(children_.size());
    for (const auto& child : children_) {
      min_sizes.push_back(std::max(0, child->requirement().min_x));
      flex_grow.push_back(std::max(0, child->requirement().flex_grow_x));
    }

    const std::vector<int> sizes = allocate_axis_sizes(
      total_width,
      min_sizes,
      flex_grow,
      percents_
    );

    int x = box.x_min;
    for (size_t i = 0; i < children_.size(); ++i) {
      Box child_box = box;
      child_box.x_min = x;
      child_box.x_max = x + sizes[i] - 1;
      children_[i]->SetBox(child_box);
      x += sizes[i];
    }
  }

 private:
  std::vector<std::optional<double>> percents_;
};

class StrictPercentVBoxElement : public Node {
 public:
  StrictPercentVBoxElement(Elements children, std::vector<std::optional<double>> percents)
      : Node(std::move(children)), percents_(std::move(percents)) {}

  void ComputeRequirement() override {
    requirement_ = Requirement{};

    for (auto& child : children_) {
      child->ComputeRequirement();

      if (requirement_.focused.Prefer(child->requirement().focused)) {
        requirement_.focused = child->requirement().focused;
        requirement_.focused.box.Shift(0, requirement_.min_y);
      }

      requirement_.min_y += child->requirement().min_y;
      requirement_.min_x = std::max(requirement_.min_x, child->requirement().min_x);
    }
  }

  void SetBox(Box box) override {
    Node::SetBox(box);
    const int total_height = std::max(0, box.y_max - box.y_min + 1);

    std::vector<int> min_sizes;
    std::vector<int> flex_grow;
    min_sizes.reserve(children_.size());
    flex_grow.reserve(children_.size());
    for (const auto& child : children_) {
      min_sizes.push_back(std::max(0, child->requirement().min_y));
      flex_grow.push_back(std::max(0, child->requirement().flex_grow_y));
    }

    const std::vector<int> sizes = allocate_axis_sizes(
      total_height,
      min_sizes,
      flex_grow,
      percents_
    );

    int y = box.y_min;
    for (size_t i = 0; i < children_.size(); ++i) {
      Box child_box = box;
      child_box.y_min = y;
      child_box.y_max = y + sizes[i] - 1;
      children_[i]->SetBox(child_box);
      y += sizes[i];
    }
  }

 private:
  std::vector<std::optional<double>> percents_;
};

Element strict_percent_hbox(
    Elements children,
    std::vector<std::optional<double>> percents
) {
  return std::make_shared<StrictPercentHBoxElement>(
    std::move(children),
    std::move(percents)
  );
}

Element strict_percent_vbox(
    Elements children,
    std::vector<std::optional<double>> percents
) {
  return std::make_shared<StrictPercentVBoxElement>(
    std::move(children),
    std::move(percents)
  );
}

Component apply_node_size_spec(Component component, const Rcpp::List& node) {
  const NodeSizeSpec spec = parse_node_size_spec(node);
  if (!spec.has_constraints()) {
    return component;
  }

  return Renderer(component, [component, spec] {
    Element content = component->Render();
    return apply_size_constraints(std::move(content), spec);
  });
}

Component apply_node_overflow_spec(Component component, const Rcpp::List& node) {
  const NodeOverflowSpec spec = parse_node_overflow_spec(node);
  if (!spec.has_overflow()) {
    return component;
  }

  const bool scroll_x = spec.overflow_x == OverflowMode::Scroll;
  const bool scroll_y = spec.overflow_y == OverflowMode::Scroll;
  OverflowScrollState state;
  auto reflected_box = std::make_shared<Box>();

  Component rendered = Renderer(component, [component, spec, state, reflected_box] {
    Element content = component->Render();
    content = apply_overflow_constraints(std::move(content), spec, &state);
    content |= reflect(*reflected_box);
    return content;
  });

  if (!scroll_x && !scroll_y) {
    return rendered;
  }

  return CatchEvent(rendered, [component, reflected_box, state, scroll_x, scroll_y](Event event) {
    auto clamp = [](std::shared_ptr<int> value, std::shared_ptr<int> max_value) {
      *value = std::clamp(*value, 0, *max_value);
    };
    auto shift_x = [&](int delta) {
      if (!scroll_x || *state.max_scroll_x <= 0) {
        return false;
      }
      const int previous = *state.scroll_x;
      *state.scroll_x += delta;
      clamp(state.scroll_x, state.max_scroll_x);
      return *state.scroll_x != previous;
    };
    auto shift_y = [&](int delta) {
      if (!scroll_y || *state.max_scroll_y <= 0) {
        return false;
      }
      const int previous = *state.scroll_y;
      *state.scroll_y += delta;
      clamp(state.scroll_y, state.max_scroll_y);
      return *state.scroll_y != previous;
    };

    if (component->Focused()) {
      if (event == Event::ArrowUpCtrl) {
        return shift_y(-1);
      }
      if (event == Event::ArrowDownCtrl) {
        return shift_y(1);
      }
      if (event == Event::ArrowLeftCtrl) {
        return shift_x(-1);
      }
      if (event == Event::ArrowRightCtrl) {
        return shift_x(1);
      }
      if (event == Event::PageUp) {
        const int step = std::max(1, reflected_box->y_max - reflected_box->y_min);
        return shift_y(-step);
      }
      if (event == Event::PageDown) {
        const int step = std::max(1, reflected_box->y_max - reflected_box->y_min);
        return shift_y(step);
      }
      if (event == Event::Home) {
        bool changed = false;
        if (scroll_x && *state.scroll_x != 0) {
          *state.scroll_x = 0;
          changed = true;
        }
        if (scroll_y && *state.scroll_y != 0) {
          *state.scroll_y = 0;
          changed = true;
        }
        return changed;
      }
      if (event == Event::End) {
        bool changed = false;
        if (scroll_x && *state.scroll_x != *state.max_scroll_x) {
          *state.scroll_x = *state.max_scroll_x;
          changed = true;
        }
        if (scroll_y && *state.scroll_y != *state.max_scroll_y) {
          *state.scroll_y = *state.max_scroll_y;
          changed = true;
        }
        return changed;
      }
    }

    if (!event.is_mouse()) {
      return false;
    }

    const Mouse& mouse = event.mouse();
    const bool has_box =
      reflected_box->x_max >= reflected_box->x_min &&
      reflected_box->y_max >= reflected_box->y_min;
    if (!has_box) {
      return false;
    }

    const bool inside_box =
      mouse.x >= reflected_box->x_min &&
      mouse.x <= reflected_box->x_max &&
      mouse.y >= reflected_box->y_min &&
      mouse.y <= reflected_box->y_max;
    if (!inside_box) {
      return false;
    }

    if (mouse.button == Mouse::WheelUp) {
      return shift_y(-3);
    }
    if (mouse.button == Mouse::WheelDown) {
      return shift_y(3);
    }
    if (mouse.button == Mouse::WheelLeft) {
      return shift_x(-3);
    }
    if (mouse.button == Mouse::WheelRight) {
      return shift_x(3);
    }

    if ((mouse.motion == Mouse::Pressed || mouse.motion == Mouse::Moved) &&
        mouse.button == Mouse::Left) {
      bool changed = false;
      if (scroll_y && *state.max_scroll_y > 0) {
        const int slider_x = reflected_box->x_max;
        const int slider_top = reflected_box->y_min;
        const int slider_bottom = reflected_box->y_max - (scroll_x ? 1 : 0);
        if (mouse.x == slider_x && mouse.y >= slider_top && mouse.y <= slider_bottom) {
          const int slider_size = slider_bottom - slider_top + 1;
          if (slider_size > 1) {
            const int offset = mouse.y - slider_top;
            const int target = (offset * *state.max_scroll_y) / (slider_size - 1);
            if (*state.scroll_y != target) {
              *state.scroll_y = target;
              changed = true;
            }
          }
        }
      }

      if (scroll_x && *state.max_scroll_x > 0) {
        const int slider_y = reflected_box->y_max;
        const int slider_left = reflected_box->x_min;
        const int slider_right = reflected_box->x_max - (scroll_y ? 1 : 0);
        if (mouse.y == slider_y && mouse.x >= slider_left && mouse.x <= slider_right) {
          const int slider_size = slider_right - slider_left + 1;
          if (slider_size > 1) {
            const int offset = mouse.x - slider_left;
            const int target = (offset * *state.max_scroll_x) / (slider_size - 1);
            if (*state.scroll_x != target) {
              *state.scroll_x = target;
              changed = true;
            }
          }
        }
      }
      return changed;
    }

    return false;
  });
}

bool parse_optional_flag(
    const Rcpp::List& node,
    const char* field_name,
    const char* arg_name
) {
  if (!node.containsElementNamed(field_name)) {
    return false;
  }

  SEXP candidate = node[field_name];
  if (TYPEOF(candidate) != LGLSXP ||
      Rf_length(candidate) != 1 ||
      LOGICAL(candidate)[0] == NA_LOGICAL) {
    Rcpp::stop("`%s` must be TRUE or FALSE.", arg_name);
  }
  return Rcpp::as<bool>(candidate);
}

bool parse_input_multiline(const Rcpp::List& node) {
  return parse_optional_flag(node, "multiline", "multiline");
}

bool parse_output_wrap(const Rcpp::List& node) {
  return parse_optional_flag(node, "wrap", "wrap");
}

enum class TextOverflowPolicy {
  Clip,
  Wrap,
  Ellipsis
};

TextOverflowPolicy parse_text_overflow_policy(const Rcpp::List& node) {
  if (!node.containsElementNamed("overflow")) {
    return parse_output_wrap(node)
      ? TextOverflowPolicy::Wrap
      : TextOverflowPolicy::Clip;
  }

  SEXP candidate = node["overflow"];
  if (!is_single_string(candidate)) {
    Rcpp::stop("`overflow` must be a single character string.");
  }

  const std::string policy = ascii_lower(Rcpp::as<std::string>(candidate));
  if (policy == "clip") return TextOverflowPolicy::Clip;
  if (policy == "wrap") return TextOverflowPolicy::Wrap;
  if (policy == "ellipsis") return TextOverflowPolicy::Ellipsis;

  Rcpp::stop("`overflow` must be one of 'clip', 'wrap', or 'ellipsis'.");
  return TextOverflowPolicy::Clip;
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
    std::vector<std::optional<double>> main_axis_percents;
    const bool is_column = type == "column";
    main_axis_percents.reserve(static_cast<size_t>(children.size()));
    for (int i = 0; i < children.size(); ++i) {
      const Rcpp::List child_node = Rcpp::as<Rcpp::List>(children[i]);
      main_axis_percents.push_back(
        is_column
          ? parse_optional_percent(child_node, "heightPercent", "heightPercent")
          : parse_optional_percent(child_node, "widthPercent", "widthPercent")
      );
      comps.push_back(
        build_component(child_node, state, handlers)
      );
    }

    Component container = is_column
      ? Container::Vertical(comps)
      : Container::Horizontal(comps);

    Component rendered = Renderer(
      container,
      [comps, main_axis_percents, is_column] {
        Elements elements;
        elements.reserve(comps.size());
        for (const auto& comp : comps) {
          elements.push_back(comp->Render());
        }
        if (is_column) {
          return strict_percent_vbox(std::move(elements), main_axis_percents);
        }
        return strict_percent_hbox(std::move(elements), main_axis_percents);
      }
    );
    Component with_size = apply_node_size_spec(std::move(rendered), node);
    return apply_node_overflow_spec(std::move(with_size), node);
  }

  if (type == "showIf") {
    if (!node.containsElementNamed("child")) {
      Rcpp::stop("`showIf` component requires a `child` field.");
    }

    Component child = build_component(Rcpp::as<Rcpp::List>(node["child"]), state, handlers);
    ShowIfSpec show_if_spec = parse_show_if_spec(node);
    Component rendered = Renderer(child, [child, show_if_spec, state] {
      Element content = child->Render();
      return show_if(std::move(content), show_if_spec, state);
    });
    Component with_size = apply_node_size_spec(std::move(rendered), node);
    return apply_node_overflow_spec(std::move(with_size), node);
  }

  // ── Output table (reads from state$output) ───────────────────────────────

  if (type == "outputTable") {
    std::string output_id = Rcpp::as<std::string>(node["outputId"]);
    Component component = Renderer([state, output_id](bool) {
      SEXP val = get_output_value(state, output_id);
      return render_serialized_table_output(val);
    });
    Component with_size = apply_node_size_spec(std::move(component), node);
    return apply_node_overflow_spec(std::move(with_size), node);
  }

  // ── Output text / numeric (reads from state$output) ──────────────────────

  if (type == "outputText" || type == "outputNumeric") {
    std::string output_id = Rcpp::as<std::string>(node["outputId"]);
    const TextOverflowPolicy overflow_policy =
      type == "outputText"
        ? parse_text_overflow_policy(node)
        : TextOverflowPolicy::Clip;
    Component component = Renderer([state, output_id, overflow_policy] {
      SEXP val = get_output_value(state, output_id);
      const std::string output_value = value_to_string(val);
      if (overflow_policy == TextOverflowPolicy::Wrap) {
        return wrapped_text(paragraph(output_value)) | xflex;
      }
      if (overflow_policy == TextOverflowPolicy::Ellipsis) {
        return ellipsis_text(output_value) | xflex;
      }
      return text(output_value);
    });
    Component with_size = apply_node_size_spec(std::move(component), node);
    return apply_node_overflow_spec(std::move(with_size), node);
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

    Component component = Button(label, [state, handlers, id] {
      increment_button_input(state, id);
      run_handler_if_present(handlers, id, state);
    }, option);
    Component with_size = apply_node_size_spec(std::move(component), node);
    return apply_node_overflow_spec(std::move(with_size), node);
  }

  // ── Checkbox ──────────────────────────────────────────────────────────────

  if (type == "checkbox") {
    std::string label = Rcpp::as<std::string>(node["label"]);
    std::string id = Rcpp::as<std::string>(node["id"]);
    auto checked = std::make_shared<bool>(get_input_bool(state, id));
    CheckboxOption option = CheckboxOption::Simple();
    option.on_change = [state, handlers, id, checked] {
      set_input_value(state, id, Rcpp::wrap(*checked));
      run_handler_if_present(handlers, id, state);
    };

    Component component = Checkbox(label, checked.get(), option);
    Component with_size = apply_node_size_spec(std::move(component), node);
    return apply_node_overflow_spec(std::move(with_size), node);
  }

  // ── Box wrapper ───────────────────────────────────────────────────────────

  if (type == "box") {
    if (!node.containsElementNamed("child")) {
      Rcpp::stop("`box` component requires a `child` field.");
    }

    Component child = build_component(Rcpp::as<Rcpp::List>(node["child"]), state, handlers);
    // Box overflow should affect the inner content viewport, not the outer border.
    Component overflow_child = apply_node_overflow_spec(std::move(child), node);
    BorderStyle style = parse_box_style(node);
    std::optional<Color> box_color = parse_optional_color(node);
    std::optional<std::string> title = parse_optional_title(node);
    std::string title_style = parse_box_title_style(node);
    std::string title_align = parse_box_title_align(node);
    int margin = parse_box_margin(node);

    Component component = Renderer(
        overflow_child,
        [overflow_child, style, box_color, title, title_style, title_align, margin] {
      Element content = overflow_child->Render() | xflex | yflex;

      if (title.has_value()) {
        if (title_style == "border") {
          Element bordered = box_color.has_value()
              ? content | borderStyled(style, *box_color)
              : content | borderStyled(style);
          bordered |= xflex;
          bordered |= yflex;

          Element overlay = box_top_line_overlay(style, *title, title_align);

          Element with_title = dbox({std::move(bordered), std::move(overlay)});
          with_title |= xflex;
          with_title |= yflex;
          return apply_margin(std::move(with_title), margin);
        }

        Element title_element = text(*title) | bold;
        title_element = align_title_element(std::move(title_element), title_align);
        Element separator_line = separatorStyled(style);

        Element inner = vbox({
          std::move(title_element),
          std::move(separator_line),
          std::move(content)
        });
        inner |= xflex;
        inner |= yflex;

        Element boxed = box_color.has_value()
            ? inner | borderStyled(style, *box_color)
            : inner | borderStyled(style);
        boxed |= xflex;
        boxed |= yflex;
        return apply_margin(std::move(boxed), margin);
      }

      Element bordered = box_color.has_value()
          ? content | borderStyled(style, *box_color)
          : content | borderStyled(style);
      bordered |= xflex;
      bordered |= yflex;
      return apply_margin(std::move(bordered), margin);
    });
    Component with_size = apply_node_size_spec(std::move(component), node);
    return with_size;
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

    Component component = Input(content.get(), opts);
    Component with_size = apply_node_size_spec(std::move(component), node);
    return apply_node_overflow_spec(std::move(with_size), node);
  }

  // Fallback: empty renderer for unknown types
  return Renderer([] { return text(""); });
}
