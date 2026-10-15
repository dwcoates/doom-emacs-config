// @vitest-environment jsdom
/**
 * ellipsis-trim — a one-line clamp's `…` follows a word character (owner
 * request, 2026-10-15). The arithmetic is pinned here over hand-laid
 * characters, and the DOM geometry over stubbed rects; where the real engine
 * puts the `…` is test/webkit/ellipsis-trim.webkit.test.ts's.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  ELLIPSIS_TRIM_PROPERTY,
  ELLIPSIS_TRIM_SLACK,
  ELLIPSIS_TRIM_UNMEASURABLE,
  RECT_SNAP,
  applyEllipsisTrim,
  clearEllipsisTrim,
  domClampGeometry,
  ellipsisTrim,
  isTrimmable,
  type ClampGeometry,
  type LineChar,
} from "../../src/feed/ellipsis-trim.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** Each character 10px wide from x = 0, so its right edge is 10 per character. */
function laid(text: string): LineChar[] {
  return [...text].map((ch, i) => ({ ch, right: (i + 1) * 10 }));
}

/** The `…`'s width in these fixtures. */
const ELLIPSIS_PX = 10;

afterEach(() => {
  vi.restoreAllMocks();
});

describe("isTrimmable: what may never stand right before the `…`", () => {
  it.each([
    ["a space", " ", true],
    ["a tab", "\t", true],
    ["a no-break space", " ", true],
    ["a period", ".", true],
    ["a comma", ",", true],
    ["a hyphen", "-", true],
    ["a letter", "a", false],
    ["a digit", "7", false],
    ["a slash", "/", false],
    ["a closing paren", ")", false],
  ] as const)("%s is trimmable: %s", (_label, ch, trimmable) => {
    // Arrange / Act / Assert
    expect(isTrimmable(ch)).toBe(trimmable);
  });
});

describe("ellipsisTrim: how far a line narrows", () => {
  it.each([
    ["a space", "hello world and", 130, 120 - 110],
    ["a period", "hello world. and", 130, 120 - 110],
    ["a comma", "hello world, and", 130, 120 - 110],
    ["a hyphen", "hello world-and", 130, 120 - 110],
    ["a run of all four", "hello world., - and", 160, 150 - 110],
  ] as const)("moves a cut after %s back to the word before it", (_label, text, right, room) => {
    // Arrange -- RIGHT fits the run but not the word after it beside the `…`.
    const chars = laid(text);

    // Act
    const trim = ellipsisTrim(chars, right, ELLIPSIS_PX);

    // Assert -- the line now ends just past "world…" and its slack.
    expect(trim).toBe(room - ELLIPSIS_TRIM_SLACK);
  });

  it("leaves a cut after a word character alone", () => {
    // Arrange -- "hello wo" fits beside the `…`, the next "r" does not.
    const chars = laid("hello world");

    // Act
    const trim = ellipsisTrim(chars, 95, ELLIPSIS_PX);

    // Assert
    expect(trim).toBe(0);
  });

  it("leaves a line of nothing but trimmable characters alone", () => {
    // Arrange
    const chars = laid(". . . . . .");

    // Act
    const trim = ellipsisTrim(chars, 60, ELLIPSIS_PX);

    // Assert
    expect(trim).toBe(0);
  });

  it("leaves a line alone when not even its first character fits beside the `…`", () => {
    // Arrange
    const chars = laid(" hello");

    // Act
    const trim = ellipsisTrim(chars, 15, ELLIPSIS_PX);

    // Assert
    expect(trim).toBe(0);
  });

  it("reads a character measured up to a snap past the room as fitting", () => {
    // Arrange -- "hello " ends at 60, and 60 + 10 overruns 69.5 by less than a snap.
    const chars = laid("hello world");
    const right = 70 - RECT_SNAP + 0.5;

    // Act
    const trim = ellipsisTrim(chars, right, ELLIPSIS_PX);

    // Assert -- the space counted as kept, so the line narrows back to "hello".
    expect(trim).toBe(right - (50 + ELLIPSIS_PX + ELLIPSIS_TRIM_SLACK));
  });

  it("never answers a negative trim", () => {
    // Arrange -- a narrow space fits within its snap, and the kept "a"'s slack overruns the edge.
    const chars = [
      { ch: "a", right: 10 },
      { ch: " ", right: 10.4 },
    ];

    // Act
    const trim = ellipsisTrim(chars, 19.6, ELLIPSIS_PX);

    // Assert
    expect(trim).toBe(0);
  });
});

/** A geometry over CHARS, a content edge at RIGHT and the fixtures' `…`. */
function geometry(chars: LineChar[], right: number): ClampGeometry {
  return { firstLine: () => chars, contentRight: () => right, ellipsisWidth: () => ELLIPSIS_PX };
}

describe("applyEllipsisTrim and clearEllipsisTrim: the clamp's property", () => {
  it("sets the trim on a clamp whose cut follows a space", () => {
    // Arrange
    const clamp = document.createElement("div");

    // Act
    applyEllipsisTrim(clamp, geometry(laid("hello world and"), 130));

    // Assert
    expect(clamp.style.getPropertyValue(ELLIPSIS_TRIM_PROPERTY)).toBe(`${10 - ELLIPSIS_TRIM_SLACK}px`);
  });

  it("sets nothing on a clamp whose cut follows a word character", () => {
    // Arrange
    const clamp = document.createElement("div");

    // Act
    applyEllipsisTrim(clamp, geometry(laid("hello world"), 95));

    // Assert
    expect(clamp.style.getPropertyValue(ELLIPSIS_TRIM_PROPERTY)).toBe("");
  });

  it("measures nothing more on a clamp with no laid-out line", () => {
    // Arrange
    const clamp = document.createElement("div");
    const contentRight = vi.fn(() => 100);
    const ellipsisWidth = vi.fn(() => ELLIPSIS_PX);

    // Act
    applyEllipsisTrim(clamp, { firstLine: () => [], contentRight, ellipsisWidth });

    // Assert
    expect([contentRight.mock.calls.length, ellipsisWidth.mock.calls.length]).toEqual([0, 0]);
  });

  it("clears a trim", () => {
    // Arrange
    const clamp = document.createElement("div");
    clamp.style.setProperty(ELLIPSIS_TRIM_PROPERTY, "4px");

    // Act
    clearEllipsisTrim(clamp);

    // Assert
    expect(clamp.style.getPropertyValue(ELLIPSIS_TRIM_PROPERTY)).toBe("");
  });
});

/** A rect at X..RIGHT on line LINE (16px tall), as a range answers it. */
function rect(left: number, right: number, line: number): DOMRect {
  return { left, right, width: right - left, top: line * 16, bottom: line * 16 + 16 } as DOMRect;
}

describe("domClampGeometry.firstLine: the characters of the first line", () => {
  it("walks every text node in order and stops at the second line", () => {
    // Arrange -- "ab" then "c" on line 0, "d" on line 1, across two text nodes.
    const clamp = document.createElement("div");
    clamp.append("ab", Object.assign(document.createElement("b"), { textContent: "cd" }));
    const at: Record<string, DOMRect[]> = {
      a: [rect(0, 8, 0)],
      b: [rect(8, 16, 0)],
      c: [rect(16, 24, 0)],
      d: [rect(0, 8, 1)],
    };
    vi.spyOn(Range.prototype, "getClientRects").mockImplementation(function (this: Range) {
      return at[this.toString()] as unknown as DOMRectList;
    });

    // Act
    const line = domClampGeometry.firstLine(clamp);

    // Assert
    expect(line).toEqual([
      { ch: "a", right: 8 },
      { ch: "b", right: 16 },
      { ch: "c", right: 24 },
    ]);
  });

  it("places a character by its inked rect, never by WebKit's zero-width one at the previous line's end", () => {
    // Arrange -- the "b" opening line 1 also answers an empty rect at line 0's end.
    const clamp = document.createElement("div");
    clamp.append("ab");
    const at: Record<string, DOMRect[]> = {
      a: [rect(0, 8, 0)],
      b: [rect(8, 8, 0), rect(0, 8, 1)],
    };
    vi.spyOn(Range.prototype, "getClientRects").mockImplementation(function (this: Range) {
      return at[this.toString()] as unknown as DOMRectList;
    });

    // Act
    const line = domClampGeometry.firstLine(clamp);

    // Assert
    expect(line).toEqual([{ ch: "a", right: 8 }]);
  });

  it("skips a character with no box, such as collapsed whitespace", () => {
    // Arrange
    const clamp = document.createElement("div");
    clamp.append("a  b");
    const at: Record<string, DOMRect[]> = {
      a: [rect(0, 8, 0)],
      " ": [],
      b: [rect(12, 20, 0)],
    };
    vi.spyOn(Range.prototype, "getClientRects").mockImplementation(function (this: Range) {
      return at[this.toString()] as unknown as DOMRectList;
    });

    // Act
    const line = domClampGeometry.firstLine(clamp);

    // Assert
    expect(line.map((c) => c.ch)).toEqual(["a", "b"]);
  });

  it("keeps a surrogate pair whole as one character", () => {
    // Arrange
    const clamp = document.createElement("div");
    clamp.append("😀");
    vi.spyOn(Range.prototype, "getClientRects").mockReturnValue([rect(0, 14, 0)] as unknown as DOMRectList);

    // Act
    const line = domClampGeometry.firstLine(clamp);

    // Assert
    expect(line).toEqual([{ ch: "😀", right: 14 }]);
  });
});

/** Stub CLAMP's computed right border and padding. */
function computed(style: Partial<CSSStyleDeclaration>): void {
  vi.spyOn(window, "getComputedStyle").mockReturnValue(style as CSSStyleDeclaration);
}

describe("domClampGeometry.contentRight: the right edge of the content box", () => {
  it("is the box's right edge less its right border and padding", () => {
    // Arrange
    const clamp = document.createElement("div");
    vi.spyOn(clamp, "getBoundingClientRect").mockReturnValue({ right: 200 } as DOMRect);
    computed({ borderRightWidth: "2px", paddingRight: "10px" });

    // Act
    const right = domClampGeometry.contentRight(clamp);

    // Assert
    expect(right).toBe(188);
  });

  it("refuses a box whose padding is not a px length, and records it at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const clamp = document.createElement("div");
    clamp.className = "title-fold";
    computed({ borderRightWidth: "0px", paddingRight: "auto" });

    // Act
    const measure = (): number => domClampGeometry.contentRight(clamp);

    // Assert
    expect(measure).toThrow(/ellipsis trim unmeasurable/);
    const record = await forwardedRecord(capture, ELLIPSIS_TRIM_UNMEASURABLE);
    expect([record.level.case, record.context?.clamp, record.context?.padding]).toEqual([
      "error",
      "title-fold",
      "auto",
    ]);
  });
});

describe("domClampGeometry.ellipsisWidth: the `…` in the clamp's font", () => {
  it("measures the `…` in the clamp's computed font", () => {
    // Arrange
    // A document the geometry has not measured in yet, so no context is cached.
    const doc = document.implementation.createHTMLDocument("fresh");
    vi.spyOn(doc, "defaultView", "get").mockReturnValue(window);
    const clamp = doc.createElement("div");
    computed({ fontStyle: "italic", fontWeight: "400", fontSize: "13px", fontFamily: "Menlo" });
    const ctx = { font: "", measureText: vi.fn(() => ({ width: 9.5 })) };
    vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockReturnValue(ctx as unknown as CanvasRenderingContext2D);

    // Act
    const width = domClampGeometry.ellipsisWidth(clamp);

    // Assert
    expect([width, ctx.font, ctx.measureText.mock.calls[0]]).toEqual([9.5, "italic 400 13px Menlo", ["…"]]);
  });

  it("refuses a page with no 2D canvas, and records it at error", async () => {
    // Arrange -- a document the geometry has not measured in yet, so no context is cached.
    const capture = captureLogRecords();
    const doc = document.implementation.createHTMLDocument("canvasless");
    const clamp = doc.createElement("div");
    clamp.className = "bubble-body";
    vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockReturnValue(null);

    // Act
    const measure = (): number => domClampGeometry.ellipsisWidth(clamp);

    // Assert
    expect(measure).toThrow(/ellipsis trim unmeasurable/);
    const record = await forwardedRecord(capture, ELLIPSIS_TRIM_UNMEASURABLE);
    expect([record.level.case, record.context?.clamp]).toEqual(["error", "bubble-body"]);
  });

  it("refuses a clamp with no computed style, and records it at error", async () => {
    // Arrange -- a windowless document has a canvas but computes no style.
    const capture = captureLogRecords();
    const doc = document.implementation.createHTMLDocument("windowless");
    const clamp = doc.createElement("div");
    clamp.className = "title-fold";
    const ctx = { font: "", measureText: () => ({ width: 9.5 }) };
    vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockReturnValue(ctx as unknown as CanvasRenderingContext2D);

    // Act
    const measure = (): number => domClampGeometry.ellipsisWidth(clamp);

    // Assert
    expect(measure).toThrow(/ellipsis trim unmeasurable/);
    const record = await forwardedRecord(capture, ELLIPSIS_TRIM_UNMEASURABLE);
    expect([record.level.case, record.context?.cause]).toEqual(["error", "the clamp has no computed style"]);
  });
});
