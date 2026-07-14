import { describe, expect, it } from "vitest";
import {
  CAPPED_CLASSES,
  CAPPED_SELECTOR,
  EXPANDED_CLASS,
  Section,
  applyExpanded,
  cappedSectionAt,
  expandAction,
  expandedKeys,
  isCappedSection,
  isExpanded,
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

  it("keys a multi-class section by its first capped class", () => {
    // Arrange — the Bash output carries both tool-output and bash-output;
    // tool-output comes first in CAPPED_CLASSES, so it names the key.
    const sections = [section("tool-output", "bash-output", EXPANDED_CLASS)];
    // Act + Assert
    expect(expandedKeys(sections)).toEqual(["tool-output:0"]);
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
