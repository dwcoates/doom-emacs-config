// @vitest-environment jsdom
// The "load previous page" control's loading state and its trigger on sight.
// See src/feed/walk-control.ts.
import { describe, expect, it } from "vitest";
import {
  FEED_LOAD_MORE_LABEL,
  FEED_LOADING_LABEL,
  WALK_LOADING_ATTRIBUTE,
  WALK_SPINNER_CLASS,
  createWalkTrigger,
  drawWalkControl,
  setWalkLoading,
} from "../../src/feed/walk-control.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";
import { captureLogRecords } from "../log-capture.js";
import feedViewSource from "../../src/feed/feed-view.ts?raw";
import { codeOf } from "../source-text.js";

/** A control in the document, counting the clicks its listeners heard. */
function mounted() {
  const control = drawWalkControl();
  document.body.replaceChildren(control);
  let clicks = 0;
  control.addEventListener("click", () => (clicks += 1));
  return { control, clicks: () => clicks };
}

describe("drawWalkControl", () => {
  it("draws the control idle, its spinner alone with no text", () => {
    // Arrange / Act
    const control = drawWalkControl();

    // Assert
    expect({
      text: control.textContent,
      spinners: control.querySelectorAll(`.${WALK_SPINNER_CLASS}`).length,
      children: control.children.length,
      disabled: control.disabled,
      loading: control.hasAttribute(WALK_LOADING_ATTRIBUTE),
      label: control.getAttribute("aria-label"),
      marked: control.hasAttribute("data-load-more"),
      className: control.className,
    }).toEqual({
      text: "",
      spinners: 1,
      children: 1,
      disabled: false,
      loading: false,
      label: FEED_LOAD_MORE_LABEL,
      marked: true,
      className: "feed-load-more",
    });
  });
});

describe("setWalkLoading", () => {
  it("marks the control loading, keeping its one spinner, while loading", () => {
    // Arrange
    const control = drawWalkControl();

    // Act
    setWalkLoading(control, true);

    // Assert
    expect({
      text: control.textContent,
      spinners: control.querySelectorAll(`.${WALK_SPINNER_CLASS}`).length,
      children: control.children.length,
      loading: control.hasAttribute(WALK_LOADING_ATTRIBUTE),
      label: control.getAttribute("aria-label"),
    }).toEqual({ text: "", spinners: 1, children: 1, loading: true, label: FEED_LOADING_LABEL });
  });

  it("disables the control while loading", () => {
    // Arrange
    const control = drawWalkControl();

    // Act
    setWalkLoading(control, true);

    // Assert
    expect(control.getAttribute("aria-disabled")).toBe("true");
  });

  it("rests the spinner and enables the control once idle again", () => {
    // Arrange
    const control = drawWalkControl();
    setWalkLoading(control, true);

    // Act
    setWalkLoading(control, false);

    // Assert
    expect({
      text: control.textContent,
      spinners: control.querySelectorAll(`.${WALK_SPINNER_CLASS}`).length,
      disabled: control.disabled,
      loading: control.hasAttribute(WALK_LOADING_ATTRIBUTE),
      label: control.getAttribute("aria-label"),
    }).toEqual({ text: "", spinners: 1, disabled: false, loading: false, label: FEED_LOAD_MORE_LABEL });
  });

  it("shows one spinner however often loading is set", () => {
    // Arrange
    const control = drawWalkControl();

    // Act
    setWalkLoading(control, true);
    setWalkLoading(control, true);

    // Assert
    expect(control.querySelectorAll(`.${WALK_SPINNER_CLASS}`)).toHaveLength(1);
  });
});

describe("createWalkTrigger", () => {
  it("clicks the control when it comes into view", () => {
    // Arrange
    const { control, clicks } = mounted();
    const trigger = createWalkTrigger(control, "root");

    // Act
    fireIntersection(control, true);

    // Assert
    expect(clicks()).toBe(1);
    trigger.dispose();
  });

  it("does nothing when the control leaves the view", () => {
    // Arrange
    const { control, clicks } = mounted();
    const trigger = createWalkTrigger(control, "root");

    // Act
    fireIntersection(control, false);

    // Assert
    expect(clicks()).toBe(0);
    trigger.dispose();
  });

  it("does nothing for a control no longer in the document", () => {
    // Arrange
    const { control, clicks } = mounted();
    const trigger = createWalkTrigger(control, "root");
    control.remove();

    // Act
    fireIntersection(control, true);

    // Assert
    expect(clicks()).toBe(0);
    trigger.dispose();
  });

  it("runs nothing on a sighting while a walk is in flight", () => {
    // Arrange
    const { control, clicks } = mounted();
    const trigger = createWalkTrigger(control, "root");
    setWalkLoading(control, true);

    // Act
    fireIntersection(control, true);

    // Assert
    expect(clicks()).toBe(0);
    trigger.dispose();
  });

  it("watches against the viewport, which every scroll box clips", () => {
    // Arrange
    const { control } = mounted();

    // Act
    const trigger = createWalkTrigger(control, "root");

    // Assert
    const watching = intersectionObservers().filter((r) => r.targets.has(control));
    expect(watching.map((r) => r.root)).toEqual([null]);
    trigger.dispose();
  });

  it("asks the observer afresh on rearm", () => {
    // Arrange
    const { control } = mounted();
    const observed: Element[] = [];
    const unobserved: Element[] = [];
    class Recording {
      observe(el: Element): void {
        observed.push(el);
      }
      unobserve(el: Element): void {
        unobserved.push(el);
      }
      disconnect(): void {}
    }
    const trigger = createWalkTrigger(control, "root", Recording as unknown as typeof IntersectionObserver);

    // Act
    trigger.rearm();

    // Assert
    expect({ observed: observed.length, unobserved: unobserved.length }).toEqual({ observed: 2, unobserved: 1 });
  });

  it("stops watching the control once disposed", () => {
    // Arrange
    const { control } = mounted();
    const trigger = createWalkTrigger(control, "root");

    // Act
    trigger.dispose();

    // Assert
    expect(() => fireIntersection(control, true)).toThrow(/no IntersectionObserver/);
  });

  it("records each sighting with the feed it belongs to", async () => {
    // Arrange
    const { control } = mounted();
    const capture = captureLogRecords("debug");
    const trigger = createWalkTrigger(control, "bubble-7");

    // Act
    fireIntersection(control, true);

    // Assert
    capture.logger.flush();
    await Promise.resolve();
    const record = capture.sent.find((r) => r.operation === "feed.walk-trigger.sighted");
    expect(record?.context).toMatchObject({ feed: "bubble-7", outcome: "clicked" });
    trigger.dispose();
  });

  it("refuses a page with no IntersectionObserver, loudly", async () => {
    // Arrange
    const { control } = mounted();
    const capture = captureLogRecords("debug");

    // Act
    const build = (): unknown => createWalkTrigger(control, "root", null);

    // Assert
    expect(build).toThrow(/no IntersectionObserver/);
    capture.logger.flush();
    await Promise.resolve();
    const record = capture.sent.find((r) => r.operation === "feed.walk-trigger.unsupported");
    expect({ level: record?.level.case, feed: record?.context?.feed }).toEqual({ level: "error", feed: "root" });
  });
});

describe("the feed's walk control goes through walk-control.ts", () => {
  it("builds its control with drawWalkControl", () => {
    expect(codeOf(feedViewSource)).toMatch(/drawWalkControl\(\)/);
  });

  it("never sets the control's disabled state by hand", () => {
    expect(codeOf(feedViewSource)).not.toMatch(/loadMore\.disabled\s*=/);
  });

  it("never hand-rolls the control's class", () => {
    expect(codeOf(feedViewSource)).not.toMatch(/"feed-load-more"/);
  });
});
