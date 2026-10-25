// @vitest-environment jsdom
/**
 * body — the one bubble body: the custom element that knows when it joins the
 * document, the cap measurement a metaprompt tree wraps to, and its refusals.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  BODY_INVARIANT,
  BUBBLE_BODY_TAG,
  MARKDOWN_SLOT_ATTRIBUTE,
  addSlotMark,
  isMarkdownSlot,
  TREE_WIDTH_UNMEASURABLE,
  createBubbleBody,
  isBubbleBody,
  markdownSlot,
  measureTreeCols,
  observeContainingBlock,
  paintBody,
  paintGeneration,
  proseCharacters,
  proseHtml,
  proseSource,
  unparsedProse,
  proseNeedsWidth,
  revealSlot,
  slotText,
  whenSlotPainted,
  type BubbleBody,
} from "../../src/bubble/body.js";
import { drawBubble } from "../../src/bubble/draw.js";
import { RESPONSE_RUN_CLASS } from "../../src/feed/response-run.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { useTreeLayout } from "../tree-layout.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { fireResize } from "../resize-observer.js";
import { plainProse, promptProse, servedProse, treeProse } from "../prose.js";
import { create } from "@bufbuild/protobuf";
import { ParsedProseSchema } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import { MalformedView } from "../../src/rpc/malformed.js";

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
function stageBody(opts: { detached?: boolean; bubbleClass?: string; boxClass?: string } = {}): HTMLElement {
  const bubble = document.createElement("div");
  bubble.className = opts.bubbleClass ?? "bubble assistant md";
  const scroll = document.createElement("div");
  scroll.className = opts.boxClass ?? "bubble-scroll bubble-box";
  const body = document.createElement("div");
  body.className = "bubble-body";
  scroll.append(body);
  bubble.append(scroll);
  if (opts.detached !== true) mount(bubble);
  return body;
}

/**
 * A body inside a bubble COALESCED INTO A RESPONSE RUN: column > run > member
 * item > bubble > scroll box > body, the shape feed/response-run.ts builds.
 */
function stageRunMemberBody(): HTMLElement {
  const run = document.createElement("div");
  run.className = RESPONSE_RUN_CLASS;
  const item = document.createElement("div");
  item.className = "feed-item";
  run.append(item);
  const body = stageBody({ detached: true });
  const bubble = body.closest<HTMLElement>(".bubble");
  if (bubble === null) throw new Error("no bubble");
  item.append(bubble);
  mount(run);
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
    // Arrange — 77% of 1000px = 770px, less 2 x 10px body padding and the 8px
    // scrollbar gutter = 742px.
    const body = stageBody();
    // Act
    const cols = measureTreeCols(body);
    // Assert — floor(742 / 8).
    expect(cols).toBe(92);
  });

  it.each([
    ["an overlay bar's 0px", 0, 93],
    ["a classic bar's 14px", 14, 92],
  ] as const)("takes %s measured gutter off the budget", (_label, gutterPx, want) => {
    // Arrange — 770px cap less 20px body padding less the gutter.
    staged.layout.scrollbarPx = gutterPx;
    const body = stageBody();
    // Act
    const cols = measureTreeCols(body);
    // Assert — floor(750 / 8) and floor(736 / 8).
    expect(cols).toBe(want);
  });

  it("takes no gutter off an uncapped bubble's budget, whose box reserves none", () => {
    // Arrange — the uncapped box wears `.bubble-box` alone (BUBBLE_UNCAPPED).
    const body = stageBody({ boxClass: "bubble-box" });
    // Act
    const cols = measureTreeCols(body);
    // Assert — floor(750 / 8): the 8px classic gutter is not taken.
    expect(cols).toBe(93);
  });

  it("wraps a collapsed bubble to the same budget as the expanded one", () => {
    // Arrange
    const body = stageBody();
    const scroll = body.parentElement;
    if (scroll === null) throw new Error("no scroll box");
    const collapsed = measureTreeCols(body);
    // Act
    scroll.classList.add(EXPANDED_CLASS);
    const expanded = measureTreeCols(body);
    // Assert
    expect(expanded).toBe(collapsed);
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
    // Act + Assert — 560 - 20 - 8 = 532px, floor(532 / 8).
    expect(measureTreeCols(body)).toBe(66);
  });

  it("resolves a single-percentage calc() cap as that percentage", () => {
    // Arrange
    staged.layout.maxWidth = "calc(77%)";
    const body = stageBody();
    // Act + Assert
    expect(measureTreeCols(body)).toBe(92);
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

  it("measures a bubble coalesced into a run against the run's cap", () => {
    // Arrange — the member's own max-width is `none`; the run carries 77%.
    const body = stageRunMemberBody();
    // Act
    const cols = measureTreeCols(body);
    // Assert — the same floor(742 / 8) as a bubble capped by itself.
    expect(cols).toBe(92);
  });

  it("resolves a run member's cap against the run's containing block", () => {
    // Arrange
    const body = stageRunMemberBody();
    const narrow = measureTreeCols(body);
    // Act
    staged.layout.containingPx = 1400;
    const wide = measureTreeCols(body);
    // Assert
    expect(wide).toBeGreaterThan(narrow);
  });

  it("records the measured budget at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const body = stageBody();
    // Act
    measureTreeCols(body);
    // Assert
    const record = await forwardedRecord(capture, "bubble.body.tree-cols");
    expect(record.context).toMatchObject({ cols: 92, char_px: 8, gutter_px: 8 });
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

  it("refuses a scroll box whose gutter measures negative", async () => {
    // Arrange — a clientWidth wider than the offsetWidth.
    staged.layout.scrollbarPx = -4;
    const body = stageBody();
    // Act + Assert
    expect(await violation(body)).toBe("the scroll box's scrollbar gutter measured negative or not finite");
  });

  it("refuses a body with no scroll box", async () => {
    // Arrange — the body hangs straight off the bubble.
    const body = stageBody();
    const scroll = body.parentElement;
    if (scroll === null) throw new Error("no scroll box");
    scroll.replaceWith(body);
    // Act + Assert
    expect(await violation(body)).toBe("the bubble body has no scroll box");
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
    const { bubble: el } = drawBubble({
      role: "response",
      variant: "response",
      content: [markdownSlot("prose", treeProse(TREE))],
      capLines: "feed",
    });
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

describe("proseHtml", () => {
  it("draws a plain block as markdown", () => {
    // Act + Assert
    expect(proseHtml(plainProse("**done**"), () => 80)).toContain("<strong>done</strong>");
  });

  it("draws a tree block as the tree, wrapped to the budget", () => {
    // Act
    const html = proseHtml(servedProse("1 🔧 Fixed it\n└── 1.1 x"), () => 80);
    // Assert
    expect(html).toMatch(/^<div class="mp-tree">.*<\/div>$/s);
  });

  it("draws the blocks in the order they were served", () => {
    // Arrange — prose, then a tree, then prose.
    const prose = create(ParsedProseSchema, {
      blocks: [
        { block: { case: "plain", value: { markdown: "before" } } },
        ...servedProse("1 🔧 Fixed it\n└── 1.1 x").blocks,
        { block: { case: "plain", value: { markdown: "after" } } },
      ],
    });
    const host = document.createElement("div");
    // Act
    host.innerHTML = proseHtml(prose, () => 80);
    // Assert
    expect([...host.children].map((c) => c.className || c.tagName)).toEqual(["P", "mp-tree", "P"]);
  });

  it("draws a prompt's entries as the tree their labels make", () => {
    // Arrange
    const host = document.createElement("div");
    // Act
    host.innerHTML = proseHtml(promptProse("3.1 - yes"), () => 80);
    // Assert
    const tree = host.querySelector(".mp-entry-tree");
    expect([tree?.classList.contains("mp-tree"), tree?.querySelector('.mp-entry[data-entry-label="3.1"]') !== null]).toEqual([true, true]);
  });

  it("measures no budget for prose with no tree", () => {
    // Arrange
    const cols = vi.fn(() => 80);
    // Act
    proseHtml(plainProse("hello"), cols);
    // Assert
    expect(cols).not.toHaveBeenCalled();
  });

  it("refuses a block with no arm, recording it through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const prose = create(ParsedProseSchema, { blocks: [{}] });
    // Act
    expect(() => proseHtml(prose, () => 80)).toThrow(/bubble body invariant/);
    // Assert
    const record = await forwardedRecord(capture, BODY_INVARIANT);
    expect(record.level.case).toBe("error");
  });
});

describe("proseCharacters", () => {
  it("counts the characters of the served source", () => {
    // Act + Assert
    expect(proseCharacters(plainProse("hello\n"))).toBe(6);
  });

  it("refuses prose served without its source", () => {
    // Act + Assert
    expect(() => proseCharacters(create(ParsedProseSchema, {}))).toThrow(MalformedView);
  });
});

describe("proseSource", () => {
  it("answers the served source", () => {
    // Act + Assert
    expect(proseSource(plainProse("hello\n"))).toBe("hello\n");
  });

  it("refuses prose served without its source", () => {
    // Act + Assert
    expect(() => proseSource(create(ParsedProseSchema, {}))).toThrow(MalformedView);
  });
});

describe("unparsedProse", () => {
  it.each([
    { name: "one line", markdown: "hello", want: ["hello"] },
    { name: "a trailing newline, dropped from the block", markdown: "hello\n", want: ["hello"] },
    { name: "only whitespace, as no block", markdown: "  \n", want: [] },
    { name: "a tree, as one plain block rather than a tree", markdown: "1 🔧 A\n└── 1.1 b", want: ["1 🔧 A\n└── 1.1 b"] },
  ])("holds $name", ({ markdown, want }) => {
    // Act
    const prose = unparsedProse(markdown);
    // Assert
    expect([prose.blocks.map((b) => (b.block.case === "plain" ? b.block.value.markdown : b.block.case)), proseSource(prose)]).toEqual([
      want,
      markdown,
    ]);
  });
});

describe("proseNeedsWidth", () => {
  it.each([
    { name: "a bare tree", markdown: TREE, want: true },
    { name: "a fenced tree", markdown: ["```", TREE, "```"].join("\n"), want: true },
    { name: "plain prose", markdown: "**done**, nothing tree-shaped", want: false },
  ])("says whether $name needs a measured width", ({ markdown, want }) => {
    // Act + Assert
    expect(proseNeedsWidth(servedProse(markdown))).toBe(want);
  });

  it("says a prompt's run of entries needs a measured width", () => {
    // Arrange
    const prose = promptProse("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Act + Assert
    expect([prose.blocks.map((b) => b.block.case), proseNeedsWidth(prose)]).toEqual([["entries"], true]);
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

/** A response bubble holding one markdown slot of MARKDOWN, and that slot. */
function slotted(markdown: string): { bubble: HTMLElement; slot: HTMLElement } {
  const slot = markdownSlot("prose", servedProse(markdown));
  const { bubble } = drawBubble({ role: "response", variant: "response", content: [slot], capLines: "feed" });
  return { bubble, slot };
}

describe("markdown slots", () => {
  it("wears the slot attribute the pipeline finds it by", () => {
    // Arrange / Act
    const slot = markdownSlot("prose", plainProse("**done**"));
    // Assert
    expect(slot.hasAttribute(MARKDOWN_SLOT_ATTRIBUTE)).toBe(true);
  });

  it("draws nothing of its own until a body paints it", () => {
    // Arrange / Act
    const slot = markdownSlot("prose", plainProse("**done**"));
    // Assert
    expect(slot.childNodes).toHaveLength(0);
  });

  it("is painted as markdown by paintBody", () => {
    // Arrange
    const body = createBubbleBody();
    const slot = markdownSlot("prose", plainProse("**done**"));
    // Act
    paintBody(body, [slot]);
    // Assert
    expect(slot.querySelector("strong")?.textContent).toBe("done");
  });

  it("paints every slot of the content, nested ones included", () => {
    // Arrange
    const body = createBubbleBody();
    const wrapper = document.createElement("div");
    const nested = markdownSlot("nested", plainProse("*deep*"));
    wrapper.append(nested);
    // Act
    paintBody(body, [markdownSlot("top", plainProse("top")), wrapper]);
    // Assert
    expect(nested.querySelector("em")?.textContent).toBe("deep");
  });

  it("applies its mark to the painted markdown", () => {
    // Arrange
    const body = createBubbleBody();
    const slot = markdownSlot("prose", plainProse("a b"), (rendered) => {
      rendered.querySelector("p")?.classList.add("marked");
    });
    // Act
    paintBody(body, [slot]);
    // Assert
    expect(slot.querySelector("p.marked")).not.toBeNull();
  });

  it("keeps its mark across a repaint of the same slot", () => {
    // Arrange
    const body = createBubbleBody();
    const mark = (rendered: HTMLElement): void => {
      rendered.querySelector("p")?.classList.add("marked");
    };
    const slot = markdownSlot("prose", plainProse("one"), mark);
    paintBody(body, [slot]);
    // Act
    paintBody(body, [markdownSlot("prose", plainProse("two"), mark)]);
    // Assert
    expect(slot.querySelector("p.marked")?.textContent).toBe("two");
  });

  it("drops its mark when the repainting slot carries none", () => {
    // Arrange
    const body = createBubbleBody();
    const slot = markdownSlot("prose", plainProse("one"), (rendered) => {
      rendered.querySelector("p")?.classList.add("marked");
    });
    paintBody(body, [slot]);
    // Act
    paintBody(body, [markdownSlot("prose", plainProse("two"))]);
    // Assert
    expect(slot.querySelector("p.marked")).toBeNull();
  });

  it("places the content's other nodes untouched, in order", () => {
    // Arrange
    const body = createBubbleBody();
    const badge = document.createElement("span");
    badge.textContent = "plan";
    // Act
    paintBody(body, [badge, markdownSlot("prose", plainProse("text"))]);
    // Assert
    expect(body.firstChild).toBe(badge);
  });
});

describe("a slot holding a tree paints once the bubble is laid out", () => {
  useTreeLayout();

  it("paints plain prose at once, detached", () => {
    // Arrange / Act
    const { slot } = slotted("**done**");
    // Assert
    expect(slot.querySelector("strong")).not.toBeNull();
  });

  it("paints nothing of a tree while the bubble is detached", () => {
    // Arrange / Act
    const { slot } = slotted(TREE);
    // Assert
    expect(slot.childNodes).toHaveLength(0);
  });

  it("paints the tree the moment the bubble is attached", () => {
    // Arrange
    const { bubble, slot } = slotted(TREE);
    // Act
    mount(bubble);
    // Assert
    expect(slot.querySelector(".mp-tree")).not.toBeNull();
  });

  it("waits while the bubble is attached but hidden", () => {
    // Arrange
    const { bubble, slot } = slotted(TREE);
    bubble.hidden = true;
    // Act
    mount(bubble);
    // Assert
    expect(slot.querySelector(".mp-tree")).toBeNull();
  });

  it("paints a hidden bubble's tree when the bubble is shown and its column resizes", () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["requestAnimationFrame"] });
    const { bubble, slot } = slotted(TREE);
    bubble.hidden = true;
    const column = mount(bubble);
    // Act
    bubble.hidden = false;
    fireResize(column);
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
    // Assert
    expect(slot.querySelector(".mp-tree")).not.toBeNull();
  });

  it("records the deferral at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    slotted(TREE);
    // Assert
    const record = await forwardedRecord(capture, "bubble.body.paint-deferred");
    expect(record.context).toMatchObject({ connected: false });
  });

  it("does not measure a column that went hidden under a painted tree", () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["requestAnimationFrame"] });
    const { bubble } = slotted(TREE);
    const column = mount(bubble);
    // Act — the column is hidden: it has no box, and no width to wrap to.
    column.hidden = true;
    fireResize(column);
    // Assert
    expect(() => vi.runOnlyPendingTimers()).not.toThrow();
    vi.useRealTimers();
  });
});

describe("revealSlot", () => {
  /** A body painted with one slot of MARKDOWN; answers both. */
  function painted(markdown: string): { body: BubbleBody; slot: HTMLElement } {
    const body = createBubbleBody();
    const [slot] = paintBody(body, [markdownSlot("prose", servedProse(markdown))]);
    return { body, slot: slot as HTMLElement };
  }

  it("shows only the first characters of the rendered text", () => {
    // Arrange
    const { body, slot } = painted("hello world");
    // Act
    revealSlot(body, slot, 5);
    // Assert
    expect(slot.textContent?.trim()).toBe("hello");
  });

  it("shows the whole text again when the reveal is cleared", () => {
    // Arrange
    const { body, slot } = painted("hello world");
    revealSlot(body, slot, 5);
    // Act
    revealSlot(body, slot, undefined);
    // Assert
    expect(slot.textContent?.trim()).toBe("hello world");
  });

  it("never draws a half-typed construct as its raw syntax", () => {
    // Arrange
    const { body, slot } = painted("**bold** text");
    // Act
    revealSlot(body, slot, 2);
    // Assert
    expect([slot.querySelector("strong")?.textContent, slot.textContent?.includes("*")]).toEqual(["bo", false]);
  });

  it("keeps the cut when the slot repaints with a new source", () => {
    // Arrange
    const { body, slot } = painted("hello world");
    revealSlot(body, slot, 3);
    // Act
    paintBody(body, [markdownSlot("prose", plainProse("hello there"))]);
    // Assert
    expect(slot.textContent?.trim()).toBe("hel");
  });

  it("keeps an unchanged leading paragraph's node across a repaint while cut", () => {
    // Arrange
    const { body, slot } = painted("first\n\nsec");
    revealSlot(body, slot, 7);
    const first = slot.firstElementChild;
    // Act
    paintBody(body, [markdownSlot("prose", plainProse("first\n\nsecond"))]);
    // Assert
    expect(slot.firstElementChild).toBe(first);
  });

  it("refuses an element that is not a slot of this body, and records it", async () => {
    // Arrange
    const capture = captureLogRecords();
    const body = createBubbleBody();
    const stranger = markdownSlot("prose", plainProse("elsewhere"));
    // Act + Assert
    expect(() => revealSlot(body, stranger, 1)).toThrow(/bubble body invariant/);
    const record = await forwardedRecord(capture, BODY_INVARIANT);
    expect(record.context).toMatchObject({
      reason: "a reveal named an element that is not a markdown slot of this body",
    });
  });

  it("leaves the generation alone", () => {
    // Arrange
    const { body, slot } = painted("one");
    const before = paintGeneration(body);
    // Act
    revealSlot(body, slot, 1);
    // Assert
    expect(paintGeneration(body)).toBe(before);
  });
});

describe("slotText", () => {
  it("answers the whole rendered text while the slot is cut", () => {
    // Arrange
    const body = createBubbleBody();
    const [slot] = paintBody(body, [markdownSlot("prose", plainProse("**hello** world"))]);
    revealSlot(body, slot as HTMLElement, 2);
    // Act
    const text = slotText(slot as HTMLElement);
    // Assert
    expect(text?.trim()).toBe("hello world");
  });
});

describe("a revealing slot holding a tree", () => {
  useTreeLayout();

  it("has no text before it paints", () => {
    // Arrange / Act
    const { slot } = slotted(TREE);
    // Assert
    expect(slotText(slot)).toBeUndefined();
  });

  it("runs a paint waiter once the tree paints", () => {
    // Arrange
    const { bubble, slot } = slotted(TREE);
    const ran = vi.fn();
    whenSlotPainted(slot, ran);
    // Act
    mount(bubble);
    // Assert
    expect(ran).toHaveBeenCalledTimes(1);
  });

  it("applies a reveal set before the paint to the paint itself", () => {
    // Arrange
    const { bubble, slot } = slotted(TREE);
    const body = slot.parentElement as BubbleBody;
    revealSlot(body, slot, 4);
    // Act
    mount(bubble);
    // Assert
    expect(slot.textContent?.replace(/\s/g, "")).toBe("Resp");
  });
});

describe("paintBody", () => {
  it("refuses a body the pipeline did not make, and records it", async () => {
    // Arrange — a bare element of the body's tag has no painter.
    createBubbleBody();
    const capture = captureLogRecords();
    const bare = document.createElement(BUBBLE_BODY_TAG) as BubbleBody;
    // Act + Assert
    expect(() => paintBody(bare, [])).toThrow(/bubble body invariant/);
    const record = await forwardedRecord(capture, BODY_INVARIANT);
    expect(record.context).toMatchObject({ reason: "a bubble body was painted that has no painter" });
  });
});

describe("paintBody: a repaint is in place", () => {
  it("keeps a slot of the same class and repaints it with the new source", () => {
    // Arrange
    const body = createBubbleBody();
    const [slot] = paintBody(body, [markdownSlot("prose", plainProse("one"))]);
    // Act
    const [again] = paintBody(body, [markdownSlot("prose", plainProse("**two**"))]);
    // Assert
    expect([again, slot.textContent?.trim()]).toEqual([slot, "two"]);
  });

  it("keeps an unchanged paragraph's node across the repaint", () => {
    // Arrange
    const body = createBubbleBody();
    const [slot] = paintBody(body, [markdownSlot("prose", plainProse("first\n\nsec"))]);
    const first = (slot as HTMLElement).firstElementChild;
    // Act
    paintBody(body, [markdownSlot("prose", plainProse("first\n\nsecond"))]);
    // Assert
    expect((slot as HTMLElement).firstElementChild).toBe(first);
  });

  it("takes a slot of another class as the new content's own", () => {
    // Arrange
    const body = createBubbleBody();
    const [slot] = paintBody(body, [markdownSlot("prose", plainProse("one"))]);
    // Act
    const [again] = paintBody(body, [markdownSlot("plan-prose", plainProse("one"))]);
    // Assert
    expect(again).not.toBe(slot);
  });

  it("never keeps a stale non-slot node, whose listeners belong to the old push", () => {
    // Arrange
    const body = createBubbleBody();
    const badge = document.createElement("span");
    paintBody(body, [badge]);
    const fresh = document.createElement("span");
    // Act
    const [drawn] = paintBody(body, [fresh]);
    // Assert
    expect(drawn).toBe(fresh);
  });

  it("takes the body's next paint generation", () => {
    // Arrange
    const body = createBubbleBody();
    const before = paintGeneration(body);
    // Act
    paintBody(body, []);
    // Assert
    expect(paintGeneration(body)).toBe(before + 1);
  });

});

describe("isBubbleBody", () => {
  it("knows a body this pipeline made", () => {
    expect(isBubbleBody(createBubbleBody())).toBe(true);
  });

  it("refuses any other element", () => {
    expect(isBubbleBody(document.createElement("div"))).toBe(false);
  });
});

describe("observeContainingBlock: the one observer of a bubble's column", () => {
  it("hands the bubble's parent to the callback once the frame comes", () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["requestAnimationFrame"] });
    const body = stageBody();
    const column = body.closest(".bubble")!.parentElement!;
    const rewrap = vi.fn();
    observeContainingBlock(body, rewrap);

    // Act
    fireResize(column);
    vi.runOnlyPendingTimers();
    vi.useRealTimers();

    // Assert
    expect(rewrap.mock.calls).toEqual([[column]]);
  });

  it("coalesces a burst of resizes into one call", () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["requestAnimationFrame"] });
    const body = stageBody();
    const column = body.closest(".bubble")!.parentElement!;
    const rewrap = vi.fn();
    observeContainingBlock(body, rewrap);

    // Act
    fireResize(column);
    fireResize(column);
    vi.runOnlyPendingTimers();
    vi.useRealTimers();

    // Assert
    expect(rewrap).toHaveBeenCalledTimes(1);
  });

  it("refuses a body in no bubble, and records it at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const body = document.createElement("div");
    document.body.append(body);

    // Act
    const observe = (): ResizeObserver => observeContainingBlock(body, vi.fn());

    // Assert
    expect(observe).toThrow();
    const record = await forwardedRecord(capture, TREE_WIDTH_UNMEASURABLE);
    expect(record.level.case).toBe("error");
  });
});

describe("isMarkdownSlot", () => {
  it.each([
    ["a markdown slot", () => markdownSlot("prose", plainProse("hi")), true],
    ["an element that only wears the slot attribute", () => {
      const el = document.createElement("div");
      el.setAttribute(MARKDOWN_SLOT_ATTRIBUTE, "");
      return el;
    }, false],
    ["a text node", () => document.createTextNode("hi"), false],
    ["nothing", () => undefined, false],
  ])("answers %s", (_name, make, want) => {
    // Act / Assert
    expect(isMarkdownSlot(make())).toBe(want);
  });
});

describe("addSlotMark", () => {
  /** Paint SLOT alone into a fresh body, answering the live slot. */
  function paintAlone(slot: HTMLElement): HTMLElement {
    const body = createBubbleBody();
    document.body.append(body);
    return paintBody(body, [slot])[0] as HTMLElement;
  }

  it("marks every paint of a slot that had no mark", () => {
    // Arrange
    const slot = markdownSlot("prose", plainProse("hi"));
    addSlotMark(slot, (rendered) => rendered.querySelector("p")?.setAttribute("data-added", ""));
    // Act
    const live = paintAlone(slot);
    // Assert
    expect(live.querySelector("p")?.hasAttribute("data-added")).toBe(true);
  });

  it("runs after the slot kind's own mark, keeping both", () => {
    // Arrange
    const order: string[] = [];
    const slot = markdownSlot("prose", plainProse("hi"), () => order.push("own"));
    addSlotMark(slot, () => order.push("added"));
    // Act
    paintAlone(slot);
    // Assert
    expect(order).toEqual(["own", "added"]);
  });

  it("is carried to the live slot by a repaint in place", () => {
    // Arrange
    const body = createBubbleBody();
    document.body.append(body);
    const [live] = paintBody(body, [markdownSlot("prose", plainProse("before"))]);
    const next = markdownSlot("prose", plainProse("after"));
    addSlotMark(next, (rendered) => rendered.querySelector("p")?.setAttribute("data-added", ""));
    // Act
    const [repainted] = paintBody(body, [next]);
    // Assert
    expect([repainted === live, (repainted as HTMLElement).querySelector("p")?.hasAttribute("data-added")]).toEqual([true, true]);
  });

  it("refuses an element that is not a markdown slot, recording it through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const el = document.createElement("div");
    el.className = "not-a-slot";
    // Act
    expect(() => addSlotMark(el, () => undefined)).toThrow(/bubble body invariant/);
    // Assert
    const record = await forwardedRecord(capture, BODY_INVARIANT);
    expect([record.level.case, record.context?.slot]).toEqual(["error", "not-a-slot"]);
  });
});
