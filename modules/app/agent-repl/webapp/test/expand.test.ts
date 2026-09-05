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
  it("accepts a Bash output section", () => {
    // Arrange + Act + Assert
    expect(isCappedSection(section("tool-output", "bash-output").classList)).toBe(true);
  });

  it("accepts a Read preview section", () => {
    // Arrange + Act + Assert
    expect(isCappedSection(section("tool-output", "tool-read-output").classList)).toBe(true);
  });

  it("accepts an Edit diff section", () => {
    // Arrange + Act + Assert
    expect(isCappedSection(section("diff", "diff-output").classList)).toBe(true);
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
  it("finds the capped section above the click target", () => {
    // Arrange
    const feed = node("feed", null);
    const card = node("card", feed, "tool-card");
    const out = node("out", card, "tool-output", "bash-output");
    const text = node("text", out, "stderr");
    // Act + Assert
    expect(cappedSectionAt(text, feed)?.name).toBe("out");
  });

  it("returns the clicked section itself when it is the capped one", () => {
    // Arrange
    const feed = node("feed", null);
    const out = node("out", feed, "tool-output");
    // Act + Assert
    expect(cappedSectionAt(out, feed)?.name).toBe("out");
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

  it("resolves a click on a subagent description to the box holding its folded JSON", () => {
    // Arrange — the Agent card as render.ts lays it out: the description line is
    // all the user can aim at, and the .tool-input box around it is what expands.
    const feed = node("feed", null);
    const card = node("card", feed, "tool-card", "tool-agent");
    const box = node("box", card, "tool-input", "agent-input");
    const desc = node("desc", box, "file-path", "agent-input-desc");
    // Act + Assert
    expect(cappedSectionAt(desc, feed)?.name).toBe("box");
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
  it("keys the expanded section by class and occurrence", () => {
    // Arrange — a Bash card whose output is open and whose command is not.
    const sections = [section("bash-input"), section("bash-output", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["bash-output:0"]);
  });

  it("counts occurrences among sections sharing a class", () => {
    // Arrange — two outputs in one item, only the second open.
    const sections = [section("tool-output"), section("tool-output", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["tool-output:1"]);
  });

  it("keys a multi-class section by its SPECIFIC class, not the generic wrapper", () => {
    // Arrange — the Bash output carries both tool-output and bash-output. The
    // specific class names the key, so a plain .tool-output appearing beside
    // it cannot renumber it out from under an open section.
    const sections = [section("tool-output", "bash-output", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["bash-output:0"]);
  });

  it("keeps a specific section's key stable when a generic one lands above it", () => {
    // Arrange — a Skill card gaining an error result above its open body,
    // which is the collision the ordering exists to prevent.
    const body = section("tool-output", "skill-content", EXPANDED_CLASS);
    const before = expandedKeys([body]);
    // Act — the result box arrives ahead of it.
    const after = expandedKeys([section("tool-output"), body]);
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

describe("applyExpanded", () => {
  it("re-expands the sections a re-render replaced", () => {
    // Arrange — the rebuilt item's fresh, capped sections.
    const sections = [section("bash-input"), section("bash-output")];
    // Act
    applyExpanded(sections, ["bash-output:0"]);
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
    // Arrange — the rebuilt item carries one section where two were open.
    const sections = [section("bash-input")];
    // Act + Assert — the surviving section reopens and the missing one is ignored.
    expect(() => applyExpanded(sections, ["bash-input:0", "bash-output:0"])).not.toThrow();
    expect(sections[0].classes.has(EXPANDED_CLASS)).toBe(true);
  });

  it("keeps an open section open when a different-class section lands above it", () => {
    // Arrange — an open output whose re-render grew a command line above it,
    // the layout shift that breaks a positional index.
    const before = [section("bash-output", EXPANDED_CLASS)];
    const after = [section("bash-input"), section("bash-output")];
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
    el.innerHTML = `<div class="tool-output">${inner}</div>`;
    document.body.appendChild(el);
    feed = el;
    return { feed: el, box: el.querySelector(".tool-output") as HTMLElement };
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
});
