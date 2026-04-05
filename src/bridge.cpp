#include <Rcpp.h>
#include "ui_tree.h"

#include <algorithm>
#include <ftxui/component/component.hpp>
#include <ftxui/component/screen_interactive.hpp>
#include <ftxui/dom/elements.hpp>
#include <ftxui/dom/node_decorator.hpp>
#include <ftxui/util/autoreset.hpp>

#include <memory>
#include <string>

using namespace ftxui;

namespace {

class OffsetFrame : public NodeDecorator {
 public:
  OffsetFrame(
      Element child,
      std::shared_ptr<int> scroll_x,
      std::shared_ptr<int> scroll_y,
      std::shared_ptr<int> max_scroll_x,
      std::shared_ptr<int> max_scroll_y
  )
      : NodeDecorator(std::move(child)),
        scroll_x_(std::move(scroll_x)),
        scroll_y_(std::move(scroll_y)),
        max_scroll_x_(std::move(max_scroll_x)),
        max_scroll_y_(std::move(max_scroll_y)) {}

  void ComputeRequirement() override {
    NodeDecorator::ComputeRequirement();
    requirement_ = children_[0]->requirement();
  }

  void SetBox(Box box) override {
    box_ = box;
    Box child_box = box;

    const int viewport_w = std::max(1, box.x_max - box.x_min + 1);
    const int viewport_h = std::max(1, box.y_max - box.y_min + 1);
    const int intrinsic_w = std::max(requirement_.min_x, viewport_w);
    const int content_h = std::max(requirement_.min_y, viewport_h);

    const int max_x = std::max(0, intrinsic_w - viewport_w);
    const int max_y = std::max(0, content_h - viewport_h);
    *max_scroll_x_ = max_x;
    *max_scroll_y_ = max_y;

    *scroll_x_ = std::clamp(*scroll_x_, 0, max_x);
    *scroll_y_ = std::clamp(*scroll_y_, 0, max_y);

    // Keep width reactive to terminal size by default (scroll_x == 0), so
    // wrapped text can reflow. When panning horizontally, expand to intrinsic
    // width to reveal off-screen content.
    const int content_w = *scroll_x_ > 0 ? intrinsic_w : viewport_w;

    child_box.x_min = box.x_min - *scroll_x_;
    child_box.x_max = child_box.x_min + content_w - 1;
    child_box.y_min = box.y_min - *scroll_y_;
    child_box.y_max = child_box.y_min + content_h - 1;
    children_[0]->SetBox(child_box);
  }

  void Render(Screen& screen) override {
    const AutoReset<Box> stencil(
      &screen.stencil,
      Box::Intersection(box_, screen.stencil)
    );
    children_[0]->Render(screen);
  }

 private:
  std::shared_ptr<int> scroll_x_;
  std::shared_ptr<int> scroll_y_;
  std::shared_ptr<int> max_scroll_x_;
  std::shared_ptr<int> max_scroll_y_;
};

Element offset_frame(
    Element child,
    std::shared_ptr<int> scroll_x,
    std::shared_ptr<int> scroll_y,
    std::shared_ptr<int> max_scroll_x,
    std::shared_ptr<int> max_scroll_y
) {
  return std::make_shared<OffsetFrame>(
    std::move(child),
    std::move(scroll_x),
    std::move(scroll_y),
    std::move(max_scroll_x),
    std::move(max_scroll_y)
  );
}

void clamp_scroll(std::shared_ptr<int> value, std::shared_ptr<int> max_value) {
  *value = std::clamp(*value, 0, *max_value);
}

}  // namespace

// [[Rcpp::export]]
void runTuiApp(
    Rcpp::List uiList,
    Rcpp::List stateList,
    Rcpp::List handlers,
    std::string overflow
) {
  // Shared state — all component lambdas capture this by shared_ptr.
  auto state = std::make_shared<AppState>();
  state->values = stateList;

  // Build the FTXUI component tree from the R UI list tree.
  Component root = build_component(uiList, state, handlers);
  auto scroll_x = std::make_shared<int>(0);
  auto scroll_y = std::make_shared<int>(0);
  auto max_scroll_x = std::make_shared<int>(0);
  auto max_scroll_y = std::make_shared<int>(0);

  if (overflow == "scroll") {
    root = Renderer(root, [root, scroll_x, scroll_y, max_scroll_x, max_scroll_y] {
      Element content = root->Render();
      return offset_frame(
        std::move(content),
        scroll_x,
        scroll_y,
        max_scroll_x,
        max_scroll_y
      );
    });
  } else if (overflow == "clip") {
    root = Renderer(root, [root] {
      Element content = root->Render();
      content->ComputeRequirement();
      const Requirement requirement = content->requirement();
      const Dimensions terminal = Terminal::Size();

      const bool clipped_x = requirement.min_x > terminal.dimx;
      const bool clipped_y = requirement.min_y > terminal.dimy;
      if (!clipped_x && !clipped_y) {
        return content;
      }

      const std::string warning_text =
          " Terminal too small - enlarge window (need " +
          std::to_string(requirement.min_x) + "x" +
          std::to_string(requirement.min_y) + ") ";

      Element warning = text(warning_text) |
          color(Color::Black) |
          bgcolor(Color::Yellow) |
          bold;

      Element overlay = vbox({
        hbox({filler(), std::move(warning), filler()}),
        filler()
      });

      return dbox({std::move(content), std::move(overlay)});
    });
  } else if (overflow != "clip") {
    Rcpp::stop("`overflow` must be one of 'clip' or 'scroll'.");
  }

  // Wrap root in a CatchEvent component that handles Ctrl+Q / Escape to quit.
  auto screen = ScreenInteractive::Fullscreen();

  Component app = CatchEvent(
    root,
    [&screen, overflow, scroll_x, scroll_y, max_scroll_x, max_scroll_y](Event event) -> bool {
    if (overflow == "scroll") {
      if (event == Event::ArrowUpCtrl) {
        *scroll_y -= 1;
        clamp_scroll(scroll_y, max_scroll_y);
        return true;
      }
      if (event == Event::ArrowDownCtrl) {
        *scroll_y += 1;
        clamp_scroll(scroll_y, max_scroll_y);
        return true;
      }
      if (event == Event::ArrowLeftCtrl) {
        *scroll_x -= 1;
        clamp_scroll(scroll_x, max_scroll_x);
        return true;
      }
      if (event == Event::ArrowRightCtrl) {
        *scroll_x += 1;
        clamp_scroll(scroll_x, max_scroll_x);
        return true;
      }
      if (event == Event::PageUp) {
        *scroll_y -= 10;
        clamp_scroll(scroll_y, max_scroll_y);
        return true;
      }
      if (event == Event::PageDown) {
        *scroll_y += 10;
        clamp_scroll(scroll_y, max_scroll_y);
        return true;
      }
      if (event == Event::Home) {
        *scroll_x = 0;
        *scroll_y = 0;
        return true;
      }
      if (event == Event::End) {
        *scroll_x = *max_scroll_x;
        *scroll_y = *max_scroll_y;
        return true;
      }
      if (event.is_mouse()) {
        if (event.mouse().button == Mouse::WheelUp) {
          *scroll_y -= 3;
          clamp_scroll(scroll_y, max_scroll_y);
          return true;
        }
        if (event.mouse().button == Mouse::WheelDown) {
          *scroll_y += 3;
          clamp_scroll(scroll_y, max_scroll_y);
          return true;
        }
        if (event.mouse().button == Mouse::WheelLeft) {
          *scroll_x -= 3;
          clamp_scroll(scroll_x, max_scroll_x);
          return true;
        }
        if (event.mouse().button == Mouse::WheelRight) {
          *scroll_x += 3;
          clamp_scroll(scroll_x, max_scroll_x);
          return true;
        }
      }
    }

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
