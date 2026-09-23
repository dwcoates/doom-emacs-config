// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  CAPPED_CLASSES,
  CAPPED_SELECTOR,
  EXPANDED_CLASS,
  PANEL_CLASS,
  Section,
  applyExpanded,
  cappedSectionAt,
  expandAction,
  expandedKeys,
  cappedSectionsOf,
  carryExpanded,
  retainRows,
  snapshotExpanded,
  isCappedSection,
  installClickExpand,
  isExpanded,
  ownsSection,
  toggleExpanded,
} from "../src/expand.js";

/** A section carrying CLASSES, with a live classList the toggle can drive. */
function section(...classes: string[]): Section & { classes: Set<string> } {
  const set = new Set(classes);
  return {
    classes: set,
    classList: {
      contains: (name: string) => set.has(name),
      add: (name: string) => set.add(name),
      remove: (name: string) => set.delete(name),
    },
  };
}

/** Fake ancestor-chain node: a classed element as cappedSectionAt reads it. */
interface FakeNode {
  name: string;
  parentElement: FakeNode | null;
  classList: { contains(name: string): boolean };
}

function node(name: string, parent: FakeNode | null, ...classes: string[]): FakeNode {
  return {
    name,
    parentElement: parent,
    classList: { contains: (c: string) => classes.includes(c) },
  };
}

describe("isCappedSection", () => {
  it("accepts a card-level tool/skill fold", () => {
    // Owner ruling, 2026-09-15: a tool-call/skill card is ONE fold, opened as a
    // unit, so the whole `.tool-fold` card is the capped section.
    expect(isCappedSection(section("tool-card", "tool-fold").classList)).toBe(true);
  });

  it("accepts the detached shell's live tail, which keeps its own per-section fold", () => {
    // Arrange + Act + Assert
    expect(isCappedSection(section("tool-output", "bash-output", "shell-tail").classList)).toBe(true);
  });

  it("accepts a hook card's box, which keeps its own per-section fold", () => {
    // Arrange + Act + Assert
    expect(isCappedSection(section("tool-output", "bash-output", "hook-output").classList)).toBe(true);
  });

  it("accepts a response bubble's scroll box, now that bubbles are click-to-expand", () => {
    // Owner ruling, 2026-09-15: a collapsed bubble no longer scrolls; a click
    // expands it. So its scroll box is a capped, expandable section.
    expect(isCappedSection(section("bubble-scroll").classList)).toBe(true);
  });

  it("rejects a tool card's INNER output box, which no longer expands on its own", () => {
    // The card-level fold owns the expansion now; a bare output box inside a
    // `.tool-fold` card is not itself a click-to-expand section.
    expect(isCappedSection(section("tool-output", "bash-output").classList)).toBe(false);
  });

  it("rejects a tool card's INNER input line", () => {
    // Arrange + Act + Assert — the input line is the header's body, capped by
    // the card's collapsed state, never a section of its own.
    expect(isCappedSection(section("bash-input").classList)).toBe(false);
  });

  it("rejects an uncapped element such as an assistant bubble", () => {
    // Arrange + Act + Assert
    expect(isCappedSection(section("bubble", "assistant", "md").classList)).toBe(false);
  });
});

describe("CAPPED_SELECTOR", () => {
  it("selects every capped class", () => {
    // Arrange + Act + Assert — the selector sectionsIn queries the DOM with.
    expect(CAPPED_SELECTOR).toBe(CAPPED_CLASSES.map((c) => `.${c}`).join(", "));
  });
});

describe("cappedSectionAt", () => {
  it("resolves a click inside a tool card's output to the CARD, opened as a unit", () => {
    // Arrange — a card-level fold: a click deep in the (revealed) output box
    // resolves to the whole `.tool-fold` card, never to the inner box.
    const feed = node("feed", null);
    const card = node("card", feed, "tool-card", "tool-fold");
    const out = node("out", card, "tool-output", "bash-output");
    const text = node("text", out, "stderr");
    // Act + Assert
    expect(cappedSectionAt(text, feed)?.name).toBe("card");
  });

  it("returns the clicked section itself when it is the capped one", () => {
    // Arrange — the detached shell tail keeps its own per-section fold.
    const feed = node("feed", null);
    const tail = node("tail", feed, "tool-output", "bash-output", "shell-tail");
    // Act + Assert
    expect(cappedSectionAt(tail, feed)?.name).toBe("tail");
  });

  it("returns null for a click on an uncapped part of the feed", () => {
    // Arrange
    const feed = node("feed", null);
    const bubble = node("bubble", feed, "bubble", "assistant");
    // Act + Assert
    expect(cappedSectionAt(bubble, feed)).toBeNull();
  });

  it("returns null for a click on no element at all", () => {
    // Arrange
    const feed = node("feed", null);
    // Act + Assert
    expect(cappedSectionAt(null, feed)).toBeNull();
  });

  it("resolves a click inside a response bubble to its scroll box, so bubbles expand", () => {
    // Arrange — a response bubble as drawFeedResponse lays it out: the body sits
    // inside the .bubble-scroll box, and a click lands in the body.
    const feed = node("feed", null);
    const bubble = node("bubble", feed, "bubble", "assistant", "md");
    const scroll = node("scroll", bubble, "bubble-scroll");
    const body = node("body", scroll, "bubble-body");
    // Act + Assert — the innermost capped section is the scroll box, not the body.
    expect(cappedSectionAt(body, feed)?.name).toBe("scroll");
  });

  it("resolves a click on a tool card's collapsed header line to the CARD", () => {
    // Arrange — a collapsed tool card: the two-row input line is all the reader
    // can aim at, and clicking it opens the whole `.tool-fold` card.
    const feed = node("feed", null);
    const card = node("card", feed, "tool-card", "tool-fold");
    const input = node("input", card, "bash-input", "cmd");
    // Act + Assert
    expect(cappedSectionAt(input, feed)?.name).toBe("card");
  });
});

describe("expandAction", () => {
  const base = { section: "sec", interactive: false, selectedText: "" };

  it("toggles the section under a plain click", () => {
    // Arrange + Act + Assert
    expect(expandAction(base)).toBe("sec");
  });

  it("leaves a click over no capped section alone", () => {
    // Arrange + Act + Assert
    expect(expandAction({ ...base, section: null })).toBeNull();
  });

  it("leaves the click that ends a text highlight to the selection", () => {
    // Arrange + Act + Assert
    expect(expandAction({ ...base, selectedText: "grep -rn foo" })).toBeNull();
  });

  it("toggles despite a whitespace-only highlight, which is no highlight", () => {
    // Arrange + Act + Assert
    expect(expandAction({ ...base, selectedText: "  \n " })).toBe("sec");
  });

  it("leaves a click on a link or disclosure control to that control", () => {
    // Arrange + Act + Assert
    expect(expandAction({ ...base, interactive: true })).toBeNull();
  });
});

describe("toggleExpanded", () => {
  it("expands a capped section on the first click", () => {
    // Arrange
    const sec = section("tool-output", "bash-output");
    // Act
    const expanded = toggleExpanded(sec);
    // Assert
    expect(expanded).toBe(true);
    expect(sec.classes.has(EXPANDED_CLASS)).toBe(true);
  });

  it("re-caps an expanded section on the second click", () => {
    // Arrange
    const sec = section("tool-output", EXPANDED_CLASS);
    // Act
    const expanded = toggleExpanded(sec);
    // Assert
    expect(expanded).toBe(false);
    expect(sec.classes.has(EXPANDED_CLASS)).toBe(false);
  });

  it("leaves the section's own classes intact across a toggle", () => {
    // Arrange
    const sec = section("tool-output", "bash-output");
    // Act
    toggleExpanded(sec);
    toggleExpanded(sec);
    // Assert
    expect([...sec.classes]).toEqual(["tool-output", "bash-output"]);
  });
});

describe("isExpanded", () => {
  it("reports a capped section as not expanded", () => {
    // Arrange + Act + Assert
    expect(isExpanded(section("tool-output"))).toBe(false);
  });

  it("reports an expanded section as expanded", () => {
    // Arrange + Act + Assert
    expect(isExpanded(section("tool-output", EXPANDED_CLASS))).toBe(true);
  });
});

describe("expandedKeys", () => {
  it("keys the expanded card by class and occurrence", () => {
    // Arrange — a tool card whose fold the reader opened.
    const sections = [section("tool-fold", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["tool-fold:0"]);
  });

  it("counts occurrences among sections sharing a class", () => {
    // Arrange — two tool cards in one item, only the second open.
    const sections = [section("tool-fold"), section("tool-fold", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["tool-fold:1"]);
  });

  it("keys a section by its CAPPED class, ignoring the non-capped classes beside it", () => {
    // Arrange — the shell tail carries tool-output and bash-output too, but only
    // shell-tail is a capped class, so it names the key.
    const sections = [section("tool-output", "bash-output", "shell-tail", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["shell-tail:0"]);
  });

  it("keeps a card's key stable when a different-class section lands above it", () => {
    // Arrange — an open tool card.
    const body = section("tool-fold", EXPANDED_CLASS);
    const before = expandedKeys([body]);
    // Act — a shell tail (a different capped class) arrives ahead of it.
    const after = expandedKeys([section("shell-tail"), body]);
    // Assert
    expect(after).toEqual(before);
  });

  it("records nothing for an item with no expanded section", () => {
    // Arrange
    const sections = [section("bash-input"), section("bash-output")];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual([]);
  });

  it("records nothing for an item with no capped section at all", () => {
    // Arrange + Act + Assert
    expect(expandedKeys([])).toEqual([]);
  });
});

/** A real DOM element carrying CLASSES — the two helpers below walk the DOM. */
function el(...classes: string[]): HTMLElement {
  const node = document.createElement("div");
  node.className = classes.join(" ");
  return node;
}

describe("cappedSectionsOf", () => {
  it("counts the root itself when the root is the capped section", () => {
    // Arrange — a card-level fold IS the element the renderer hands back.
    const root = el("tool-card", "tool-fold");
    // Act + Assert
    expect(cappedSectionsOf(root)).toEqual([root]);
  });

  it("omits a root that is not a capped section", () => {
    // Arrange
    const root = el("tool-card");
    // Act + Assert
    expect(cappedSectionsOf(root)).toEqual([]);
  });

  it("puts the capped root ahead of its capped descendants", () => {
    // Arrange
    const root = el("tool-fold");
    const nested = el("shell-tail");
    root.append(nested);
    // Act + Assert
    expect(cappedSectionsOf(root)).toEqual([root, nested]);
  });

  it("walks the descendants in document order", () => {
    // Arrange
    const root = el("tool-card");
    const first = el("hook-output");
    const second = el("shell-tail");
    root.append(first, second);
    // Act + Assert
    expect(cappedSectionsOf(root)).toEqual([first, second]);
  });
});

describe("carryExpanded", () => {
  it("carries a card-level fold the reader opened onto the redrawn card", () => {
    // Arrange — the replaced body IS the open fold.
    const previous = el("tool-card", "tool-fold");
    previous.classList.add(EXPANDED_CLASS);
    const next = el("tool-card", "tool-fold");
    // Act
    carryExpanded(previous, next);
    // Assert
    expect(next.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("carries a nested section the reader opened", () => {
    // Arrange
    const previous = el("tool-card");
    const openBox = el("hook-output");
    openBox.classList.add(EXPANDED_CLASS);
    previous.append(openBox);
    const next = el("tool-card");
    const freshBox = el("hook-output");
    next.append(freshBox);
    // Act
    carryExpanded(previous, next);
    // Assert
    expect(freshBox.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("leaves a section the reader never opened collapsed", () => {
    // Arrange
    const previous = el("tool-card", "tool-fold");
    const next = el("tool-card", "tool-fold");
    // Act
    carryExpanded(previous, next);
    // Assert
    expect(next.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("carries only the occurrence that was open", () => {
    // Arrange — two folds in one body, the second open.
    const previous = el("tool-card");
    const shut = el("tool-fold");
    const open = el("tool-fold");
    open.classList.add(EXPANDED_CLASS);
    previous.append(shut, open);
    const next = el("tool-card");
    const freshFirst = el("tool-fold");
    const freshSecond = el("tool-fold");
    next.append(freshFirst, freshSecond);
    // Act
    carryExpanded(previous, next);
    // Assert
    expect([
      freshFirst.classList.contains(EXPANDED_CLASS),
      freshSecond.classList.contains(EXPANDED_CLASS),
    ]).toEqual([false, true]);
  });
});

describe("applyExpanded", () => {
  it("re-expands the sections a re-render replaced", () => {
    // Arrange — the rebuilt item's fresh, collapsed cards.
    const sections = [section("tool-fold"), section("tool-fold")];
    // Act
    applyExpanded(sections, ["tool-fold:1"]);
    // Assert
    expect(sections.map((s) => s.classes.has(EXPANDED_CLASS))).toEqual([false, true]);
  });

  it("leaves an unlisted section capped", () => {
    // Arrange
    const sections = [section("bash-input")];
    // Act
    applyExpanded(sections, []);
    // Assert
    expect(sections[0].classes.has(EXPANDED_CLASS)).toBe(false);
  });

  it("drops a key whose section the re-render no longer renders", () => {
    // Arrange — the rebuilt item carries one card where a card and a shell tail
    // were open.
    const sections = [section("tool-fold")];
    // Act + Assert — the surviving card reopens and the missing key is ignored.
    expect(() => applyExpanded(sections, ["tool-fold:0", "shell-tail:0"])).not.toThrow();
    expect(sections[0].classes.has(EXPANDED_CLASS)).toBe(true);
  });

  it("keeps an open card open when a different-class section lands above it", () => {
    // Arrange — an open tool card whose re-render grew a shell tail above it,
    // the layout shift that breaks a positional index.
    const before = [section("tool-fold", EXPANDED_CLASS)];
    const after = [section("shell-tail"), section("tool-fold")];
    // Act
    applyExpanded(after, expandedKeys(before));
    // Assert
    expect(after.map((s) => s.classes.has(EXPANDED_CLASS))).toEqual([false, true]);
  });
});

describe("ownsSection", () => {
  it("owns a card's own output box", () => {
    // Arrange — the card's own result, no activity panel between it and the card.
    const card = node("card", null, "feed-item");
    const out = node("out", card, "tool-output");
    // Act + Assert
    expect(ownsSection(out, card)).toBe(true);
  });

  it("disowns an output box nested inside an open activity panel", () => {
    // Arrange — a child card's output, rendered inside the agent's panel.
    const card = node("card", null, "feed-item");
    const panel = node("panel", card, PANEL_CLASS);
    const childCard = node("child", panel, "tool-card");
    const out = node("out", childCard, "tool-output");
    // Act + Assert
    expect(ownsSection(out, card)).toBe(false);
  });
});

describe("primaryClass fallback", () => {
  it("keys a section carrying no capped class under the empty class name", () => {
    // Arrange — an expanded element that is not a capped section at all, the
    // only input for which `primaryClass` has no CAPPED_CLASSES entry to find.
    const sections = [section(EXPANDED_CLASS)];
    // Act
    const keys = expandedKeys(sections);
    // Assert — the empty class name, still numbered by occurrence.
    expect(keys).toEqual([":0"]);
  });
});

describe("installClickExpand", () => {
  let feed: HTMLElement | null = null;

  afterEach(() => {
    vi.restoreAllMocks();
    feed?.remove();
    feed = null;
  });

  /** A feed holding one capped section, mounted for real click dispatch. */
  function mountFeed(inner = ""): { feed: HTMLElement; box: HTMLElement } {
    const el = document.createElement("div");
    // `.tool-fold` is the card-level capped section the handler toggles: a whole
    // tool-call/skill card, opened as one unit (CAPPED_CLASSES).
    el.innerHTML = `<div class="tool-fold">${inner}</div>`;
    document.body.appendChild(el);
    feed = el;
    return { feed: el, box: el.querySelector(".tool-fold") as HTMLElement };
  }

  it("expands the capped section a click lands on", () => {
    // Arrange
    const { feed: el, box } = mountFeed("body text");
    installClickExpand(el, () => "");
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("expands a standalone title fold, a card title with no card fold, on its own click", () => {
    // Arrange — a hook card's headline: its own fold (title-fold.ts).
    const el = document.createElement("div");
    el.innerHTML = `<div class="tool-card tool-hook"><span class="tool-name title-fold title-fold-standalone">h</span></div>`;
    document.body.appendChild(el);
    feed = el;
    const title = el.querySelector(".title-fold") as HTMLElement;
    installClickExpand(el, () => "");
    // Act
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(title.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("opens the whole card, not the title, on a click on a card-owned title", () => {
    // Arrange — a tool call's input line, which defers to its `.tool-fold` card.
    const { feed: el, box } = mountFeed(`<pre class="bash-input title-fold">$ ls</pre>`);
    installClickExpand(el, () => "");
    // Act
    (el.querySelector(".title-fold") as HTMLElement).dispatchEvent(
      new MouseEvent("click", { bubbles: true }),
    );
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("restores the capped preview on the second click", () => {
    // Arrange — an already-expanded section, the state a first click leaves.
    const { feed: el, box } = mountFeed("body text");
    installClickExpand(el, () => "");
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("leaves a click on a control inside the section to that control", () => {
    // Arrange — a button, one of CLICK_THROUGH_SELECTOR's own.
    const { feed: el, box } = mountFeed(`<button id="b">run</button>`);
    installClickExpand(el, () => "");
    // Act
    (el.querySelector("#b") as HTMLElement).dispatchEvent(
      new MouseEvent("click", { bubbles: true }),
    );
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("leaves a click that ends a text selection alone", () => {
    // Arrange — the selection probe reports live selected text.
    const { feed: el, box } = mountFeed("body text");
    installClickExpand(el, () => "body");
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("ignores a click on no capped section at all", () => {
    // Arrange — the click lands on the feed itself, above every section.
    const { feed: el, box } = mountFeed("body text");
    installClickExpand(el, () => "");
    // Act
    el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("ignores a click whose target is not an HTML element", () => {
    // Arrange — an SVG child: an Element, but never an HTMLElement.
    const { feed: el, box } = mountFeed("");
    const svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    box.appendChild(svg);
    installClickExpand(el, () => "");
    // Act
    svg.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("reads the live window selection when no probe is supplied", () => {
    // Arrange — the default probe, with the page reporting selected text.
    const { feed: el, box } = mountFeed("body text");
    vi.spyOn(window, "getSelection").mockReturnValue({
      toString: () => "body",
    } as unknown as globalThis.Selection);
    installClickExpand(el);
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert — the selection gesture wins over the toggle.
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("treats an absent window selection as no selected text", () => {
    // Arrange — the default probe, with getSelection answering null.
    const { feed: el, box } = mountFeed("body text");
    vi.spyOn(window, "getSelection").mockReturnValue(null);
    installClickExpand(el);
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  /** OBSERVABLE scrollTop on BOX (jsdom's own is a no-op that stays 0). */
  function observeScrollTop(box: HTMLElement, initial: number): { readonly top: number } {
    let top = initial;
    Object.defineProperty(box, "scrollTop", {
      configurable: true,
      get: () => top,
      set: (v: number) => {
        top = v;
      },
    });
    return {
      get top() {
        return top;
      },
    };
  }

  it("FIX3: resets the section's scrollTop to 0 when it collapses", () => {
    // Arrange: an expanded box the reader has scrolled partway down.
    const { feed: el, box } = mountFeed("body text");
    const scroll = observeScrollTop(box, 0);
    installClickExpand(el, () => "");
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // expand
    (box as unknown as { scrollTop: number }).scrollTop = 120;
    // Act: collapse it.
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert: the next collapsed view starts at the top, not mid-scroll.
    expect(scroll.top).toBe(0);
  });

  it("FIX3: leaves scrollTop untouched when the section expands", () => {
    // Arrange: expanding must not disturb the scroll position.
    const { feed: el, box } = mountFeed("body text");
    const scroll = observeScrollTop(box, 37);
    installClickExpand(el, () => "");
    // Act: expand.
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(scroll.top).toBe(37);
  });

  it("hands afterToggle the expanded state on expand", () => {
    // Arrange
    const { feed: el, box } = mountFeed("body text");
    const calls: Array<[HTMLElement, boolean]> = [];
    installClickExpand(el, () => "", (s, e) => calls.push([s, e]));
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(calls).toEqual([[box, true]]);
  });

  it("hands afterToggle the collapsed state on collapse", () => {
    // Arrange
    const { feed: el, box } = mountFeed("body text");
    const calls: Array<[HTMLElement, boolean]> = [];
    installClickExpand(el, () => "", (s, e) => calls.push([s, e]));
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // expand
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // collapse
    // Assert
    expect(calls.at(-1)).toEqual([box, false]);
  });
});

// A PAGE REPLACE has no previous element to read a fold back off: every row is
// torn down and rebuilt, and the row's own id is the only thing that survives.

describe("snapshotExpanded", () => {
  it("keys an open fold by its row id", () => {
    // Arrange
    const body = el("tool-card", "tool-fold");
    body.classList.add(EXPANDED_CLASS);
    // Act
    const snapshot = snapshotExpanded([["row-1", body]]);
    // Assert
    expect(snapshot.get("row-1")).toEqual(["tool-fold:0"]);
  });

  it("holds nothing for a row whose folds are all closed", () => {
    // Arrange
    const body = el("tool-card", "tool-fold");
    // Act
    const snapshot = snapshotExpanded([["row-1", body]]);
    // Assert
    expect(snapshot.has("row-1")).toBe(false);
  });

  it("keys the expanded bubble's scroll box the same way as any other section", () => {
    // Arrange — the 50vh response/prompt bubble is a CAPPED_CLASSES section.
    const body = el("bubble", "bubble-scroll");
    body.classList.add(EXPANDED_CLASS);
    // Act
    const snapshot = snapshotExpanded([["row-1", body]]);
    // Assert
    expect(snapshot.get("row-1")).toEqual(["bubble-scroll:0"]);
  });

  it("re-opens the same section when its keys are applied to a rebuilt body", () => {
    // Arrange
    const before = el("tool-card", "tool-fold");
    before.classList.add(EXPANDED_CLASS);
    const snapshot = snapshotExpanded([["row-1", before]]);
    // Act — the replace rebuilds the row from the same push.
    const after = el("tool-card", "tool-fold");
    applyExpanded(cappedSectionsOf(after), snapshot.get("row-1") ?? []);
    // Assert
    expect(after.classList.contains(EXPANDED_CLASS)).toBe(true);
  });
});

describe("retainRows", () => {
  it("drops the keys of a row the replacing page did not serve", () => {
    // Arrange
    const snapshot = new Map([["gone", ["tool-fold:0"]]]);
    // Act
    retainRows(snapshot, new Set(["kept"]));
    // Assert
    expect(snapshot.has("gone")).toBe(false);
  });

  it("keeps the keys of a row the replacing page served again", () => {
    // Arrange
    const snapshot = new Map([["kept", ["tool-fold:0"]]]);
    // Act
    retainRows(snapshot, new Set(["kept"]));
    // Assert
    expect(snapshot.get("kept")).toEqual(["tool-fold:0"]);
  });
});
