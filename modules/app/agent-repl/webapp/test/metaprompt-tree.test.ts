import { describe, expect, it } from "vitest";
import {
  isMetapromptTree,
  railOffsets,
  renderTreeHtml,
  splitTreeLine,
} from "../src/metaprompt-tree.js";

const TREE = [
  "Response (✏️ changes made)",
  "",
  "1 🔧 Fixed the thing in module.ts",
  "├── 1.1 First supporting detail",
  "│   └── 1.1.1 Nested detail",
  "└── 1.2 Second supporting detail",
  "",
  "2 ✅ Tests pass",
].join("\n");

const identity = (s: string): string => s;

describe("isMetapromptTree", () => {
  it("detects a full tree with header and connectors", () => {
    // Act + Assert
    expect(isMetapromptTree(TREE)).toBe(true);
  });

  it("detects a connectorless depth-1 tree via emoji roots", () => {
    // Arrange — shallow trees have no ├──/└── lines at all.
    const text = "1 🔧 Fixed the thing\n\n2 ✅ Tests pass";
    // Act + Assert
    expect(isMetapromptTree(text)).toBe(true);
  });

  it("rejects ordinary prose", () => {
    // Act + Assert
    expect(isMetapromptTree("Just a normal answer.\nWith two lines.")).toBe(false);
  });

  it("rejects a single line", () => {
    // Act + Assert
    expect(isMetapromptTree("1 🔧 Fixed the thing")).toBe(false);
  });

  it("rejects numbered lines with neither connectors nor emoji roots", () => {
    // Arrange — plain enumerations must keep their markdown rendering.
    const text = "1 first point\n2 second point\n3 third point";
    // Act + Assert
    expect(isMetapromptTree(text)).toBe(false);
  });

  it("rejects text where tree lines are a minority", () => {
    // Arrange — one connector line buried in prose.
    const text = ["p1", "p2", "p3", "p4", "├── 1.1 stray"].join("\n");
    // Act + Assert
    expect(isMetapromptTree(text)).toBe(false);
  });
});

describe("splitTreeLine", () => {
  it("splits a connector branch at its content start", () => {
    // Act + Assert
    expect(splitTreeLine("├── 1.2 Second detail")).toEqual({
      prefix: "├── 1.2 ",
      content: "Second detail",
    });
  });

  it("keeps leading vertical bars in the prefix", () => {
    // Act + Assert
    expect(splitTreeLine("│   └── 1.1.1 Nested")).toEqual({
      prefix: "│   └── 1.1.1 ",
      content: "Nested",
    });
  });

  it("includes the root emoji in the prefix", () => {
    // Act + Assert
    expect(splitTreeLine("1 🔧 Fixed the thing")).toEqual({
      prefix: "1 🔧 ",
      content: "Fixed the thing",
    });
  });

  it("treats a plain header line as all content", () => {
    // Act + Assert
    expect(splitTreeLine("Response (✏️ changes made)")).toEqual({
      prefix: "",
      content: "Response (✏️ changes made)",
    });
  });
});

describe("railOffsets", () => {
  it("gives a ├ connector a rail at its own column", () => {
    // Act + Assert
    expect(railOffsets("├── 1.1 ")).toEqual([0]);
  });

  it("gives a └ connector no rail — it ends the line", () => {
    // Act + Assert
    expect(railOffsets("└── 1.2 ")).toEqual([]);
  });

  it("keeps a leading vertical bar's rail through a └ branch", () => {
    // Act + Assert
    expect(railOffsets("│   └── 1.1.1 ")).toEqual([0]);
  });

  it("stacks leading-bar and ├ connector rails", () => {
    // Act + Assert
    expect(railOffsets("│   ├── 1.1.1 ")).toEqual([0, 4]);
  });

  it("gives an emoji root no rails", () => {
    // Act + Assert
    expect(railOffsets("1 🔧 ")).toEqual([]);
  });
});

describe("renderTreeHtml", () => {
  it("renders each line as a prefix/content flex pair", () => {
    // Act
    const html = renderTreeHtml("├── 1.1 Detail", identity);
    // Assert
    expect(html).toContain(`<span class="mp-prefix">├── 1.1 `);
    expect(html).toContain(`<span class="mp-content">Detail</span>`);
  });

  it("paints a continuation rail inside a ├ branch's prefix", () => {
    // Act
    const html = renderTreeHtml("├── 1.1 Detail", identity);
    // Assert — centered in the connector's ch cell.
    expect(html).toContain(`<i class="mp-rail" style="left:0.5ch"></i></span>`);
  });

  it("paints no continuation rail for a └ branch", () => {
    // Act + Assert
    expect(renderTreeHtml("└── 1.2 Detail", identity)).not.toContain("mp-rail");
  });

  it("paints one rail per continuing column on nested branches", () => {
    // Act
    const html = renderTreeHtml("│   ├── 1.1.1 Deep detail", identity);
    // Assert
    expect(html).toContain(`<i class="mp-rail" style="left:0.5ch"></i>`);
    expect(html).toContain(`<i class="mp-rail" style="left:4.5ch"></i>`);
  });

  it("renders blank lines as spacer rows", () => {
    // Act
    const html = renderTreeHtml("1 🔧 A\n\n2 ✅ B", identity);
    // Assert
    expect(html).toContain(`class="mp-line mp-blank"`);
  });

  it("escapes markup in tree content", () => {
    // Act
    const html = renderTreeHtml("├── 1.1 <img src=x>", identity);
    // Assert
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img src=x&gt;");
  });

  it("routes content through the injected inline pass", () => {
    // Arrange
    const shout = (s: string): string => s.toUpperCase();
    // Act + Assert
    expect(renderTreeHtml("├── 1.1 detail", shout)).toContain("DETAIL");
  });
});
