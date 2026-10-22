// @vitest-environment jsdom
/**
 * collapsed-width — a collapsed bubble is as wide as the text it shows (owner
 * request, 2026-10-16). The arithmetic is pinned here over hand-laid lines, the
 * DOM geometry and the column follow over stubbed rects and observers; what the
 * real engine lays out is test/webkit/collapsed-width.webkit.test.ts's.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { createBubbleBody } from "../../src/bubble/body.js";
import {
  COLLAPSED_FIT_CLASS,
  COLLAPSED_WIDTH_FITTED,
  COLLAPSED_WIDTH_PROPERTY,
  COLLAPSED_WIDTH_UNMEASURABLE,
  applyCollapsedWidth,
  clearCollapsedWidth,
  collapsedWidth,
  collapsedWidthOf,
  domCollapsedGeometry,
  followContainingWidth,
  type CollapsedGeometry,
  type FloatRect,
  type LineRect,
} from "../../src/feed/collapsed-width.js";
import { stopTicking } from "../../src/feed/ticking.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { fireResize } from "../resize-observer.js";

afterEach(() => {
  vi.restoreAllMocks();
  document.body.replaceChildren();
});

/** A line fragment on the line at TOP (20px tall), ending at RIGHT. */
function line(right: number, top = 0): LineRect {
  return { right, top, bottom: top + 20 };
}

/** A float whose margin edge is at LEFT, spanning the first line. */
function float(left: number, top = 0): FloatRect {
  return { left, top, bottom: top + 20 };
}

describe("collapsedWidth: the width the shown lines need", () => {
  it.each([
    ["one short line", [line(100)], [], 0, 160],
    ["the widest of several lines", [line(100), line(150, 20)], [], 0, 210],
    ["a line reaching the content edge", [line(300)], [], 0, null],
    ["a line past the content edge", [line(320)], [], 0, null],
    ["a float beside the line", [line(100)], [float(260)], 0, 200],
    ["a float outside the content box", [line(100)], [float(310)], 0, 160],
    ["a float beside another line only", [line(100)], [float(260, 40)], 0, 160],
    ["the ellipsis after the last line", [line(100), line(150, 20)], [], 10, 220],
    ["the ellipsis making the last line the widest", [line(150), line(145, 20)], [], 10, 215],
    ["a fractional slack, rounded up", [line(100.4)], [], 0, 161],
  ] as const)("fits %s", (_label, lines, floats, ellipsis, want) => {
    // Arrange -- a 360px box whose body's content edge is at x = 300.

    // Act
    const got = collapsedWidth(360, 300, lines, floats, ellipsis);

    // Assert
    expect(got).toBe(want);
  });

  it("gives up nothing when the slack is under a pixel", () => {
    // Arrange / Act
    const got = collapsedWidth(360, 300, [line(299.5)], [], 0);

    // Assert
    expect(got).toBeNull();
  });
});

/** A geometry answering fixed values, recording what was read. */
function geometry(over: Partial<CollapsedGeometry> = {}): CollapsedGeometry {
  return {
    boxWidth: () => 360,
    contentRight: () => 300,
    visibleLines: () => [line(100)],
    floats: () => [],
    ellipsisWidth: () => 10,
    ...over,
  };
}

/** A box holding a body, as a bubble's box does. */
function boxWithBody(): { box: HTMLElement; body: HTMLElement } {
  const box = document.createElement("div");
  box.className = "bubble-box bubble-scroll";
  const body = document.createElement("div");
  body.className = "bubble-body";
  box.append(body);
  return { box, body };
}

describe("applyCollapsedWidth: the box takes the fit", () => {
  it("states the fitted width and wears the fit class", () => {
    // Arrange
    const { box, body } = boxWithBody();

    // Act
    applyCollapsedWidth(box, body, false, "", geometry());

    // Assert
    expect([box.style.getPropertyValue(COLLAPSED_WIDTH_PROPERTY), box.classList.contains(COLLAPSED_FIT_CLASS)]).toEqual(
      ["160px", true],
    );
  });

  it("leaves a box whose lines need its whole width natural", () => {
    // Arrange
    const { box, body } = boxWithBody();

    // Act
    applyCollapsedWidth(box, body, false, "", geometry({ visibleLines: () => [line(300)] }));

    // Assert
    expect([box.style.getPropertyValue(COLLAPSED_WIDTH_PROPERTY), box.classList.contains(COLLAPSED_FIT_CLASS)]).toEqual(
      ["", false],
    );
  });

  it("gives a box that shows no line no width, reading nothing else", () => {
    // Arrange -- a zero-line cap: the face is the header strip alone.
    const { box, body } = boxWithBody();
    const refuse = (): never => {
      throw new Error("read");
    };

    // Act
    applyCollapsedWidth(
      box,
      body,
      true,
      "",
      geometry({ visibleLines: () => [], contentRight: refuse, floats: refuse, ellipsisWidth: refuse }),
    );

    // Assert
    expect(box.style.getPropertyValue(COLLAPSED_WIDTH_PROPERTY)).toBe("0px");
  });

  it("counts the ellipsis only on an ellipsis bubble", () => {
    // Arrange
    const { box, body } = boxWithBody();
    const ellipsisWidth = vi.fn(() => 10);

    // Act
    applyCollapsedWidth(box, body, false, "", geometry({ ellipsisWidth }));

    // Assert
    expect(ellipsisWidth).not.toHaveBeenCalled();
  });

  it("counts the ellipsis after an ellipsis bubble's line", () => {
    // Arrange
    const { box, body } = boxWithBody();

    // Act
    applyCollapsedWidth(box, body, true, "", geometry());

    // Assert
    expect(box.style.getPropertyValue(COLLAPSED_WIDTH_PROPERTY)).toBe("170px");
  });

  it("records a changed width at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { box, body } = boxWithBody();

    // Act
    applyCollapsedWidth(box, body, false, "300px", geometry());

    // Assert
    const record = await forwardedRecord(capture, COLLAPSED_WIDTH_FITTED);
    expect([record.level.case, record.context?.fitted, record.context?.previous, record.context?.natural_px]).toEqual([
      "debug",
      "160px",
      "300px",
      360,
    ]);
  });

  it("records nothing when the width did not change", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { box, body } = boxWithBody();

    // Act
    applyCollapsedWidth(box, body, false, "160px", geometry());

    // Assert
    await expect(forwardedRecord(capture, COLLAPSED_WIDTH_FITTED)).rejects.toThrow(/no forwarded record/);
  });
});

describe("clearCollapsedWidth / collapsedWidthOf: the fit comes off", () => {
  it("drops the width and the class", () => {
    // Arrange
    const { box, body } = boxWithBody();
    applyCollapsedWidth(box, body, false, "", geometry());

    // Act
    clearCollapsedWidth(box);

    // Assert
    expect([collapsedWidthOf(box), box.classList.contains(COLLAPSED_FIT_CLASS)]).toEqual(["", false]);
  });

  it("reads the width a fitted box wears", () => {
    // Arrange
    const { box, body } = boxWithBody();

    // Act
    applyCollapsedWidth(box, body, false, "", geometry());

    // Assert
    expect(collapsedWidthOf(box)).toBe("160px");
  });
});

/** Stub EL's clip edge: its top and its client height. */
function clip(el: HTMLElement, top: number, height: number): void {
  vi.spyOn(el, "getBoundingClientRect").mockReturnValue({ top, left: 0, right: 0, width: 0 } as DOMRect);
  Object.defineProperty(el, "clientTop", { configurable: true, value: 0 });
  Object.defineProperty(el, "clientHeight", { configurable: true, value: height });
}

/** A rect list of RECTS. */
function rects(...list: Array<{ right: number; top: number; width?: number }>): DOMRectList {
  return list.map((r) => ({ right: r.right, top: r.top, bottom: r.top + 20, width: r.width ?? 10 })) as unknown as DOMRectList;
}

describe("domCollapsedGeometry.visibleLines: the fragments the box shows", () => {
  /** A body holding a paragraph's TEXT and an inline code span, its text nodes answering RECTS. */
  function laidBody(textRects: DOMRectList, codeRects: DOMRectList): { box: HTMLElement; body: HTMLElement } {
    const { box, body } = boxWithBody();
    const p = document.createElement("p");
    p.append("hello ");
    const code = document.createElement("code");
    code.append("x");
    p.append(code);
    body.append(p);
    document.body.append(box);
    const answers = new Map<Node, DOMRectList>([
      [p.firstChild!, textRects],
      [code.firstChild!, rects()],
    ]);
    let selected: Node | null = null;
    vi.spyOn(document, "createRange").mockReturnValue({
      selectNodeContents: (node: Node) => {
        selected = node;
      },
      getClientRects: () => (selected === null ? rects() : (answers.get(selected) ?? rects())),
    } as unknown as Range);
    vi.spyOn(code, "getClientRects").mockReturnValue(codeRects);
    return { box, body };
  }

  it("keeps the fragments above the box's clip edge", () => {
    // Arrange -- the box shows 20px; the body runs past it.
    const { box, body } = laidBody(rects({ right: 50, top: 0 }, { right: 90, top: 20 }), rects());
    clip(box, 0, 20);
    clip(body, 0, 400);

    // Act
    const lines = domCollapsedGeometry.visibleLines(box, body);

    // Assert
    expect(lines.map((l) => l.right)).toEqual([50]);
  });

  it("keeps only the fragments above an ellipsis body's own clip edge", () => {
    // Arrange -- the clamped body clips sooner than the box.
    const { box, body } = laidBody(rects({ right: 50, top: 0 }, { right: 90, top: 20 }), rects());
    clip(box, 0, 400);
    clip(body, 0, 20);

    // Act
    const lines = domCollapsedGeometry.visibleLines(box, body);

    // Assert
    expect(lines.map((l) => l.right)).toEqual([50]);
  });

  it("skips a fragment with no width", () => {
    // Arrange -- WebKit answers zero-width rects for collapsed whitespace.
    const { box, body } = laidBody(rects({ right: 50, top: 0 }, { right: 70, top: 0, width: 0 }), rects());
    clip(box, 0, 20);
    clip(body, 0, 20);

    // Act
    const lines = domCollapsedGeometry.visibleLines(box, body);

    // Assert
    expect(lines.map((l) => l.right)).toEqual([50]);
  });

  it("counts an inline element's own box, padding included", () => {
    // Arrange -- the code span's box ends past its text.
    const { box, body } = laidBody(rects({ right: 50, top: 0 }), rects({ right: 64, top: 0 }));
    clip(box, 0, 20);
    clip(body, 0, 20);

    // Act
    const lines = domCollapsedGeometry.visibleLines(box, body);

    // Assert
    expect(lines.map((l) => l.right)).toEqual([50, 64]);
  });

  it("counts a block image's own box, which holds no text", () => {
    // Arrange -- a collapsed prompt's thumbnail, wider than the words above it.
    const { box, body } = laidBody(rects({ right: 50, top: 0 }), rects());
    const image = document.createElement("img");
    image.style.display = "block";
    body.append(image);
    vi.spyOn(image, "getClientRects").mockReturnValue(rects({ right: 120, top: 20 }));
    clip(box, 0, 60);
    clip(body, 0, 60);

    // Act
    const lines = domCollapsedGeometry.visibleLines(box, body);

    // Assert
    expect(lines.map((l) => l.right)).toEqual([50, 120]);
  });
});

describe("domCollapsedGeometry.floats: what sits beside the body", () => {
  it("answers a floated sibling's left margin edge", () => {
    // Arrange
    const { box, body } = boxWithBody();
    const corner = document.createElement("div");
    corner.style.cssFloat = "right";
    corner.style.marginLeft = "-20px";
    box.prepend(corner);
    document.body.append(box);
    vi.spyOn(corner, "getBoundingClientRect").mockReturnValue({ left: 280, top: 0, bottom: 20 } as DOMRect);

    // Act
    const floats = domCollapsedGeometry.floats(box, body);

    // Assert
    expect(floats).toEqual([{ left: 300, top: 0, bottom: 20 }]);
  });

  it("skips a sibling that does not float", () => {
    // Arrange
    const { box, body } = boxWithBody();
    box.prepend(document.createElement("div"));
    document.body.append(box);

    // Act
    const floats = domCollapsedGeometry.floats(box, body);

    // Assert
    expect(floats).toEqual([]);
  });

  it("refuses a float whose margin is not a px length, and records it at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { box, body } = boxWithBody();
    box.prepend(document.createElement("div"));
    vi.spyOn(window, "getComputedStyle").mockReturnValue({
      float: "right",
      display: "block",
      marginLeft: "auto",
    } as CSSStyleDeclaration);

    // Act
    const measure = (): FloatRect[] => domCollapsedGeometry.floats(box, body);

    // Assert
    expect(measure).toThrow(/collapsed width unmeasurable/);
    const record = await forwardedRecord(capture, COLLAPSED_WIDTH_UNMEASURABLE);
    expect([record.level.case, record.context?.margin]).toEqual(["error", "auto"]);
  });
});

describe("followContainingWidth: the column's width is followed", () => {
  /** A body in a box in a bubble, followed; attach it under a column. */
  function followed(): {
    body: ReturnType<typeof createBubbleBody>;
    box: HTMLElement;
    bubble: HTMLElement;
    refresh: ReturnType<typeof vi.fn>;
  } {
    const body = createBubbleBody();
    const box = document.createElement("div");
    const bubble = document.createElement("div");
    bubble.className = "bubble";
    box.append(body);
    bubble.append(box);
    const refresh = vi.fn();
    followContainingWidth(body, refresh);
    return { body, box, bubble, refresh };
  }

  /** A column of WIDTH px in the document. */
  function column(width: number): HTMLElement {
    const el = document.createElement("div");
    document.body.append(el);
    vi.spyOn(el, "getBoundingClientRect").mockReturnValue({ width } as DOMRect);
    return el;
  }

  /** Report COL resized, and let the observer's frame come. */
  function resize(col: HTMLElement): void {
    vi.useFakeTimers({ toFake: ["requestAnimationFrame"] });
    fireResize(col);
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
  }

  it("refreshes the body's box when the bubble's parent resizes", () => {
    // Arrange
    const { box, bubble, refresh } = followed();
    const col = column(800);
    col.append(bubble);

    // Act
    resize(col);

    // Assert
    expect(refresh).toHaveBeenCalledWith(box);
  });

  it("refreshes only when the width moved", () => {
    // Arrange -- a height change resizes the column too.
    const { bubble, refresh } = followed();
    const col = column(800);
    col.append(bubble);
    resize(col);

    // Act
    resize(col);

    // Assert
    expect(refresh).toHaveBeenCalledTimes(1);
  });

  it("follows the new parent when the bubble moves, and drops the old", () => {
    // Arrange
    const { box, bubble, refresh } = followed();
    const first = column(800);
    const second = column(600);
    first.append(bubble);

    // Act
    second.append(bubble);
    resize(second);

    // Assert
    expect(refresh).toHaveBeenCalledWith(box);
    expect(() => fireResize(first)).toThrow(/no ResizeObserver/);
  });

  it("stops following when the body is discarded", () => {
    // Arrange
    const { body, bubble } = followed();
    const col = column(800);
    col.append(bubble);

    // Act
    stopTicking(body);

    // Assert
    expect(() => fireResize(col)).toThrow(/no ResizeObserver/);
  });

  it("refreshes nothing once the body has left the document", () => {
    // Arrange
    const { bubble, refresh } = followed();
    const col = column(800);
    col.append(bubble);
    bubble.remove();

    // Act
    resize(col);

    // Assert
    expect(refresh).not.toHaveBeenCalled();
  });
});
