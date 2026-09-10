import { describe, expect, it } from "vitest";
import {
  findTreeRegion,
  isMetapromptTree,
  looksLikeIntendedTree,
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

describe("findTreeRegion", () => {
  const HEADER = "Response (✏️ changes made)";
  const BODY = ["1 🔧 Fixed it", "├── 1.1 Detail", "└── 1.2 More"].join("\n");

  it("returns the whole tree with empty prefix and suffix for a clean tree", () => {
    // Act
    const region = findTreeRegion(`${HEADER}\n\n${BODY}`);
    // Assert
    expect(region).toEqual({ before: "", tree: `${HEADER}\n\n${BODY}`, after: "" });
  });

  it("splits leading prose out of the tree region", () => {
    // Act
    const region = findTreeRegion(`Some preamble.\n\n${HEADER}\n\n${BODY}`);
    // Assert
    expect(region?.before).toBe("Some preamble.\n");
    expect(region?.tree).toBe(`${HEADER}\n\n${BODY}`);
  });

  it("splits trailing prose out of the tree region", () => {
    // Act
    const region = findTreeRegion(`${HEADER}\n\n${BODY}\n\nA closing note.`);
    // Assert
    expect(region?.tree).toBe(`${HEADER}\n\n${BODY}`);
    expect(region?.after).toBe("\nA closing note.");
  });

  it("pulls a directly-preceding header into the region", () => {
    // Arrange — header on the line immediately above the first root.
    const region = findTreeRegion(`${HEADER}\n${BODY}`);
    // Assert
    expect(region?.tree.startsWith(HEADER)).toBe(true);
  });

  it("keeps the region even when no header line precedes the tree", () => {
    // Act
    const region = findTreeRegion(BODY);
    // Assert
    expect(region).toEqual({ before: "", tree: BODY, after: "" });
  });

  it("excludes a trailing fenced block from the tree region", () => {
    // Arrange — a stray fence must not suppress the bare tree.
    const region = findTreeRegion(`${HEADER}\n\n${BODY}\n\n\`\`\`\ncode\n\`\`\``);
    // Assert
    expect(region?.tree).toBe(`${HEADER}\n\n${BODY}`);
    expect(region?.after).toContain("```");
  });

  it("does not detect a tree that lives inside a fence", () => {
    // Act + Assert — the fence handler owns fenced trees.
    expect(findTreeRegion(`\`\`\`\n${BODY}\n\`\`\``)).toBeNull();
  });

  it("returns null for ordinary prose", () => {
    // Act + Assert
    expect(findTreeRegion("Just a normal answer.\nWith two lines.")).toBeNull();
  });

  it("returns null for a single stray connector line in prose", () => {
    // Arrange — one connector is not two, so it is not a tree.
    expect(findTreeRegion(["p1", "p2", "├── 1.1 stray", "p3"].join("\n"))).toBeNull();
  });
});

describe("looksLikeIntendedTree", () => {
  it("is true when the first non-blank line is the Response header", () => {
    // Act + Assert
    expect(looksLikeIntendedTree("Response (👀 no changes made)\n\nprose")).toBe(true);
  });

  it("is false when no header opens the text", () => {
    // Act + Assert
    expect(looksLikeIntendedTree("Just prose here.\nMore prose.")).toBe(false);
  });
});

describe("dotted labels with more than one level", () => {
  it("keeps a `2.1` label whole rather than stopping at its first dot", () => {
    // Arrange
    const text = [
      "Response (✏️ changes made)",
      "",
      "1.1 🔧 Fixed the thing in module.ts",
      "├── 1.1.1 First supporting detail",
      "└── 1.1.2 Second supporting detail",
      "",
      "1.2 ✅ Tests pass",
    ].join("\n");
    // Act + Assert
    expect(isMetapromptTree(text)).toBe(true);
  });
});

describe("looksLikeIntendedTree: nothing to judge", () => {
  it("is false for text that is entirely blank, having no first line to read", () => {
    // Act + Assert
    expect(looksLikeIntendedTree("\n   \n\n")).toBe(false);
  });
});

/**
 * The daemon wraps a settled response's bare tree to 105 columns before it is
 * served (`daemon/internal/resolve/feed/tree.go`), so every consumer sees
 * branches split across a head line and CONTINUATION lines: the ancestors'
 * rails then the wrapped branch's label width in padding. A continuation is
 * neither a connector line nor an emoji root, and before this was pinned the
 * region ended at the first one — shearing the tree exactly where the daemon
 * wrapped it and dropping the rest onto the markdown path.
 */
describe("daemon-wrapped continuation lines", () => {
  const ROOT = "1 🌳 A bare Unicode tree, the shape the metaprompt answers in.";
  // The exact lines `treefmt.FormatBlock(…, 105)` produces for the fake SDK's
  // `!md` showcase tree, taken from that formatter rather than invented.
  const WRAPPED = [
    ROOT,
    "├── 1.1 This branch is deliberately longer than the daemon's 105-column limit, so it is wrapped before it",
    "│   │   is served, and every continuation line must still carry the rails of the branches around it.",
    "│   └── 1.1.1 A child beneath the wrapped branch, so the rail through the wrap is load-bearing.",
    "└── 1.2 The last branch, whose continuation carries no rail because nothing follows it, once it too runs",
    "        past the daemon's limit and wraps onto a second line.",
  ].join("\n");

  it("keeps a wrapped ├── branch's continuation inside the region", () => {
    // Arrange — 1.1 wrapped, with 1.1.1 beneath it, so the continuation is
    // rails-only (`│   │   `).
    const text = [ROOT, WRAPPED.split("\n")[1], WRAPPED.split("\n")[2], WRAPPED.split("\n")[3]].join(
      "\n",
    );
    // Act
    const region = findTreeRegion(text);
    // Assert — all four lines, not the two the shear left.
    expect(region?.tree.split("\n")).toHaveLength(4);
  });

  it("keeps a wrapped └── last branch's spaces-only continuation inside the region", () => {
    // Arrange — the last branch's continuation carries no rail at all.
    // Act
    const region = findTreeRegion(WRAPPED);
    // Assert — including the trailing continuation, which ends the region.
    expect(region).toEqual({ before: "", tree: WRAPPED, after: "" });
  });

  it.each([
    { name: "a childless root, padded with spaces", cont: "  answers in." },
    { name: "a root with children, whose first pad column is a rail", cont: "│ answers in." },
  ])("keeps a wrapped root's continuation inside the region: $name", ({ cont }) => {
    // Arrange
    const text = ["1 🌳 A bare Unicode tree, the shape the metaprompt", cont, "├── 1.1 Detail"].join(
      "\n",
    );
    // Act
    const region = findTreeRegion(text);
    // Assert
    expect(region?.tree.split("\n")).toHaveLength(3);
  });

  it("never counts a continuation toward the two-core-lines minimum", () => {
    // Arrange — one root that wrapped, and nothing else: still a lone branch.
    const text = ["1 🌳 A bare Unicode tree, the shape the metaprompt", "  answers in."].join("\n");
    // Act + Assert
    expect(findTreeRegion(text)).toBeNull();
  });

  it("still ends the region at a plain prose line in column 0", () => {
    // Arrange — prose starts at column 0, so it is no one's remainder.
    const text = `${WRAPPED}\nThat is the whole demo.`;
    // Act
    const region = findTreeRegion(text);
    // Assert
    expect(region?.tree).toBe(WRAPPED);
    expect(region?.after).toBe("That is the whole demo.");
  });

  it("reads a wrapped tree as a tree despite the continuations' share of its lines", () => {
    // Arrange — two of the six lines are continuations; counting them as
    // prose puts the tree-shaped ratio under TREE_LINE_RATIO.
    // Act + Assert
    expect(isMetapromptTree(WRAPPED)).toBe(true);
  });

  it("paints a rail at every │ column of a continuation's prefix", () => {
    // Act — the rails-only continuation under a wrapped ├── branch.
    const html = renderTreeHtml("│   │   is served, and every continuation line", identity);
    // Assert — one hairline per rail column, centered in its ch cell.
    expect(html).toContain(`<i class="mp-rail" style="left:0.5ch"></i>`);
    expect(html).toContain(`<i class="mp-rail" style="left:4.5ch"></i>`);
  });
});
