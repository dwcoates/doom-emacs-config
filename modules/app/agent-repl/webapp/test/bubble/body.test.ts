// @vitest-environment jsdom
/**
 * body — the one bubble body: the custom element that knows when it joins the
 * document, the cap measurement a metaprompt tree wraps to, and its refusals.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  BUBBLE_BODY_TAG,
  TREE_WIDTH_UNMEASURABLE,
  createBubbleBody,
  measureTreeCols,
  paintWhole,
  proseNeedsWidth,
} from "../../src/bubble/body.js";
import { drawBubble } from "../../src/bubble/draw.js";
import { useTreeLayout } from "../tree-layout.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

afterEach(() => {
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

/** Place a drawn bubble in the document under its own containing block. */
function mount(el: HTMLElement): HTMLElement {
  const column = document.createElement("div");
  column.append(el);
  document.body.append(column);
  return column;
}

/**
 * A body inside a scroll box inside a bubble (of BUBBLECLASS), attached under
 * its own column unless DETACHED.
 */
function stageBody(opts: { detached?: boolean; bubbleClass?: string } = {}): HTMLElement {
  const bubble = document.createElement("div");
  bubble.className = opts.bubbleClass ?? "bubble assistant md";
  const scroll = document.createElement("div");
  scroll.className = "bubble-scroll";
  const body = document.createElement("div");
  body.className = "bubble-body";
  scroll.append(body);
  bubble.append(scroll);
  if (opts.detached !== true) mount(bubble);
  return body;
}

/** The tree the metaprompt renderer recognizes, with its header. */
const TREE = ["Response (✏️ changes made)", "", "1 🔧 Fixed it", "├── 1.1 Detail", "└── 1.2 More"].join(
  "\n",
);

/**
 * `measureTreeCols` reports the columns a tree wraps to: the body's content
 * width at the bubble's CAP, in columns of the tree font — and nothing else.
 * `installTreeLayout` stages the reads the way a real engine answers them, so a
 * DETACHED element reads as no box and no style, exactly as in the webview.
 */
describe("the columns a tree wraps to are measured against the bubble cap", () => {
  const staged = useTreeLayout();


  it("measures the cap less the chrome, in columns of the tree font", () => {
    // Arrange — 77% of 1000px = 770px, less 2 x 10px body padding = 750px.
    const body = stageBody();
    // Act
    const cols = measureTreeCols(body);
    // Assert — floor(750 / 8).
    expect(cols).toBe(93);
  });

  it("resolves the percentage cap against the containing block, so a wider column yields more columns", () => {
    // Arrange
    const body = stageBody();
    const narrow = measureTreeCols(body);
    // Act
    staged.layout.containingPx = 1400;
    const wide = measureTreeCols(body);
    // Assert
    expect(wide).toBeGreaterThan(narrow);
  });

  it("honors a px max-width cap directly", () => {
    // Arrange — an engine that resolves the cap to px hands it back as px.
    staged.layout.maxWidth = "560px";
    const body = stageBody();
    // Act + Assert — 560 - 20 = 540px, floor(540 / 8).
    expect(measureTreeCols(body)).toBe(67);
  });

  it("resolves a single-percentage calc() cap as that percentage", () => {
    // Arrange
    staged.layout.maxWidth = "calc(77%)";
    const body = stageBody();
    // Act + Assert
    expect(measureTreeCols(body)).toBe(93);
  });

  it("never reads the bubble's own fit-content width", () => {
    // Arrange — the bubble renders far narrower than its cap.
    const body = stageBody();
    const bubble = body.closest<HTMLElement>(".bubble");
    if (bubble === null) throw new Error("no bubble");
    const atCap = measureTreeCols(body);
    // Act — shrink the bubble's box and the body's own width.
    bubble.style.width = "120px";
    bubble.getBoundingClientRect = () => ({ width: 120 }) as DOMRect;
    Object.defineProperty(body, "clientWidth", { value: 100, configurable: true });
    // Assert — the budget is the cap's, unmoved.
    expect(measureTreeCols(body)).toBe(atCap);
  });

  it("records the measured budget at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const body = stageBody();
    // Act
    measureTreeCols(body);
    // Assert
    const record = await forwardedRecord(capture, "feed.cards.response.tree-cols");
    expect(record.context).toMatchObject({ cols: 93, char_px: 8 });
  });
});

describe("an unmeasurable tree width is an invariant violation", () => {
  const staged = useTreeLayout();


  /** Measure BODY, and answer the reason the violation was recorded with. */
  async function violation(body: HTMLElement): Promise<string> {
    const capture = captureLogRecords();
    expect(() => measureTreeCols(body)).toThrow(/metaprompt tree width unmeasurable/);
    const record = await forwardedRecord(capture, TREE_WIDTH_UNMEASURABLE);
    expect(record.level.case).toBe("error");
    const reason = record.context?.reason;
    return typeof reason === "string" ? reason : "";
  }

  it("refuses a body that is not in the document", async () => {
    // Arrange
    const body = stageBody({ detached: true });
    // Act + Assert
    expect(await violation(body)).toBe("the bubble body is not in the document");
  });

  it("refuses a body with no bubble around it", async () => {
    // Arrange
    const body = stageBody({ bubbleClass: "not-a-bubble" });
    // Act + Assert
    expect(await violation(body)).toBe("the body has no bubble around it");
  });

  it("refuses a bubble with no containing block", async () => {
    // Arrange — the document element is the one connected element with no parent.
    const root = document.documentElement;
    root.classList.add("bubble");
    const body = document.createElement("div");
    body.className = "bubble-body";
    document.body.append(body);
    try {
      // Act + Assert
      expect(await violation(body)).toBe("the bubble has no containing block");
    } finally {
      root.classList.remove("bubble");
    }
  });

  it("refuses a tree font whose column measures no width", async () => {
    // Arrange — a laid-out page that gave the probe no box.
    staged.layout.charPx = 0;
    const body = stageBody();
    // Act + Assert
    expect(await violation(body)).toBe("the tree font's column measured no width");
  });

  it("refuses a containing block with no width", async () => {
    // Arrange
    staged.layout.containingPx = 0;
    const body = stageBody();
    // Act + Assert
    expect(await violation(body)).toBe("the bubble's containing block has no width");
  });

  it("refuses a max-width that does not resolve", async () => {
    // Arrange
    staged.layout.maxWidth = "none";
    const body = stageBody();
    // Act + Assert
    expect(await violation(body)).toBe("the bubble's max-width does not resolve");
  });

  it("refuses a computed length that is not in px", async () => {
    // Arrange — jsdom's own unit-less answer, which no laid-out engine gives.
    const body = stageBody();
    vi.spyOn(window, "getComputedStyle").mockImplementation(
      () => ({ maxWidth: "77%", paddingLeft: "0", paddingRight: "0" }) as unknown as CSSStyleDeclaration,
    );
    // Act + Assert
    expect(await violation(body)).toBe("a computed length is not in px");
    vi.restoreAllMocks();
  });

  it("refuses a cap whose content width holds no column", async () => {
    // Arrange — a body padding wider than the whole cap.
    staged.layout.bodyPaddingPx = 400;
    const body = stageBody();
    // Act + Assert
    expect(await violation(body)).toBe("the bubble's content width at its cap holds no column");
  });

  it("refuses to follow the column on a page with no ResizeObserver", async () => {
    // Arrange
    vi.stubGlobal("ResizeObserver", undefined);
    const capture = captureLogRecords();
    const { bubble: el, body } = drawBubble({
      role: "response",
      variant: "response",
      content: [],
      capLines: "feed",
    });
    paintWhole(body, TREE);
    // Act — the attach is where the tree first paints and the observer is armed;
    // jsdom reports a custom element reaction's throw rather than rethrowing it.
    const reported: unknown[] = [];
    const onError = (event: ErrorEvent): void => {
      event.preventDefault();
      reported.push(event.error);
    };
    window.addEventListener("error", onError);
    try {
      mount(el);
    } finally {
      window.removeEventListener("error", onError);
    }
    // Assert
    expect(reported).toHaveLength(1);
    const record = await forwardedRecord(capture, TREE_WIDTH_UNMEASURABLE);
    expect(record.context).toMatchObject({
      reason: "the page has no ResizeObserver to follow the column's width",
    });
  });
});

describe("proseNeedsWidth", () => {
  it.each([
    { name: "a bare tree", markdown: TREE, want: true },
    { name: "a fenced tree", markdown: ["```", TREE, "```"].join("\n"), want: true },
    { name: "plain prose", markdown: "**done**, nothing tree-shaped", want: false },
  ])("says whether $name needs a measured width", ({ markdown, want }) => {
    // Act + Assert
    expect(proseNeedsWidth(markdown)).toBe(want);
  });
});

describe("the bubble body", () => {
  it("is the custom element that knows when it joins the document", () => {
    // Arrange / Act
    const body = createBubbleBody();
    // Assert
    expect([body.localName, body.className]).toEqual([BUBBLE_BODY_TAG, "bubble-body"]);
  });

  it("runs its hooks every time it joins the document", () => {
    // Arrange
    const body = createBubbleBody();
    let runs = 0;
    body.onConnect(() => runs++);
    // Act
    document.body.append(body);
    document.body.prepend(body);
    // Assert — an attach and a move.
    expect(runs).toBe(2);
  });
});
