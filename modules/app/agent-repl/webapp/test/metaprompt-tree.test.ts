import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import * as metapromptTree from "../src/metaprompt-tree.js";
import {
  WIDE_CHAR_CLASS,
  boxWideChars,
  formatTree,
  layoutTree,
  lineText,
  renderTreeHtml,
  TreeOverflowError,
  visibleWidth,
} from "../src/metaprompt-tree.js";
import { ProseTreeSchema, type ProseTree } from "../../proto/gen/ts/frontend/v1/prose_pb";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { inline } from "../src/markdown.js";
import { treeProse } from "./prose.js";

/** The root every wrap case hangs its branches under: nine columns wide. */
const R = "1 🎯 Root";

const identity = (s: string): string => s;

/** TEXT as the daemon parses it, which must be exactly one tree. */
function tree(text: string): ProseTree {
  const blocks = treeProse(text).blocks;
  const only = blocks[0]?.block;
  if (blocks.length !== 1 || only?.case !== "tree") throw new Error(`not one tree: ${JSON.stringify(text)}`);
  return only.value;
}

/** TEXT's tree, wrapped to WIDTH and drawn as text. */
function wrap(text: string, width: number): string {
  return formatTree(tree(text), width)
    .lines.map(lineText)
    .join("\n");
}

/** A hand-built graph, for the layouts a well-formed parse never produces. */
type NodeSpec = { parent?: number; label: string; text: string; marker?: string };
function graph(nodes: NodeSpec[], header?: string): ProseTree {
  return create(ProseTreeSchema, {
    header: header === undefined ? undefined : { text: header },
    nodes: nodes.map((n, index) => ({
      id: { index },
      place:
        n.parent === undefined
          ? { case: "root" as const, value: { marker: n.marker === undefined ? undefined : { text: n.marker } } }
          : { case: "branch" as const, value: { parent: { index: n.parent } } },
      label: { text: n.label },
      text: { markdown: n.text },
    })),
  });
}

describe("visibleWidth (rendered width)", () => {
  it.each([
    { name: "plain ascii", raw: "abc", want: 3 },
    { name: "tags contribute nothing", raw: `<a href="https://x/y">abc</a>`, want: 3 },
    { name: "mark and bold contribute nothing", raw: "<mark><b>abc</b></mark>", want: 3 },
    { name: "entity counts as one character", raw: "&lt;plugin&gt;", want: 8 },
    { name: "ampersand entity counts as one", raw: "a&amp;b", want: 3 },
    { name: "box drawing is single width", raw: "├── ", want: 4 },
    { name: "wide emoji counts as two", raw: "🎯", want: 2 },
    { name: "variation-selector emoji counts as two", raw: "✏️", want: 2 },
    { name: "zero-width joiner is free", raw: "a\u200db", want: 2 },
  ])("$name", ({ raw, want }) => {
    // Act + Assert
    expect(visibleWidth(raw)).toBe(want);
  });
});

// --- Layout: the graph as drawn rows ---

describe("layoutTree", () => {
  it("draws a canonical tree, header and all, exactly as it was written", () => {
    // Arrange
    const text = [
      "Response (✏️ changes made)",
      "",
      "1 🔧 Fixed the thing in module.ts",
      "├── 1.1 First supporting detail",
      "│   └── 1.1.1 Nested detail",
      "└── 1.2 Second supporting detail",
      "",
      "2 ✅ Tests pass",
      "└── 2.1 All of them",
    ].join("\n");
    // Act + Assert
    expect(wrap(text, 100)).toBe(text);
  });

  it("draws the rails from the graph, never from what the model wrote", () => {
    // Arrange — a stray leading space before a connector.
    const text = `${R}\n └── 1.1 alpha beta`;
    // Act + Assert
    expect(wrap(text, 100)).toBe(`${R}\n└── 1.1 alpha beta`);
  });

  it("draws a last child's descendants behind four spaces rather than a rail", () => {
    // Arrange
    const text = `${R}\n└── 1.1 a\n    ├── 1.1.1 b\n    └── 1.1.2 c`;
    // Act + Assert
    expect(wrap(text, 100)).toBe(text);
  });

  it("draws a root with no marker as its label and text", () => {
    // Arrange
    const g = graph([{ label: "1", text: "orphan" }, { parent: 0, label: "1.1", text: "leaf" }]);
    // Act + Assert
    expect(formatTree(g, 100).lines.map(lineText)).toEqual(["1 orphan", "└── 1.1 leaf"]);
  });

  it("draws a root whose only content is its marker as the marker alone", () => {
    // Arrange
    const g = graph([{ label: "1", text: "", marker: "🔧" }]);
    // Act + Assert
    expect(formatTree(g, 100).lines.map(lineText)).toEqual(["1 🔧"]);
  });

  it("draws a node's code beneath its head, in order", () => {
    // Arrange
    const g = create(ProseTreeSchema, {
      nodes: [
        {
          id: { index: 0 },
          place: { case: "root", value: { marker: { text: "🔧" } } },
          label: { text: "1" },
          text: { markdown: "A" },
          code: [{ language: "go", body: "x := 1" }],
        },
      ],
    });
    // Act
    const rows = layoutTree(g);
    // Assert
    expect(rows.map((r) => r.kind)).toEqual(["branch", "code"]);
  });

  it.each([
    {
      name: "a node whose id is not its index",
      tree: () => create(ProseTreeSchema, { nodes: [{ id: { index: 3 }, place: { case: "root", value: {} }, label: { text: "1" }, text: { markdown: "a" } }] }),
    },
    {
      name: "a node with no label",
      tree: () => create(ProseTreeSchema, { nodes: [{ id: { index: 0 }, place: { case: "root", value: {} }, text: { markdown: "a" } }] }),
    },
    {
      name: "a branch naming a later parent",
      tree: () => graph([{ label: "1", text: "a" }, { parent: 1, label: "1.1", text: "b" }]),
    },
    {
      name: "a node with no place",
      tree: () => create(ProseTreeSchema, { nodes: [{ id: { index: 0 }, label: { text: "1" }, text: { markdown: "a" } }] }),
    },
  ])("refuses $name, recording it through the canonical logger", async ({ tree: malformed }) => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => layoutTree(malformed())).toThrow(/metaprompt tree/);
    // Assert
    const record = await forwardedRecord(capture, "metaprompt-tree.malformed");
    expect(record.level.case).toBe("error");
  });
});

// --- The wrap engine, the former daemon treefmt algorithm ---
//
// Each case keeps the input shape, field width and expectation of the Go test
// it was carried over from (the daemon's treefmt suite, since deleted), restated
// as a whole tree the daemon parses: the branch under test hangs from R, and the
// width grows by the columns its label lost (`1.3. ` became `1.1 `).

describe("wrap engine — the daemon algorithm, ported", () => {
  it("leaves branches that fit untouched", () => {
    // Arrange
    const text = `${R}\n├── 1.1 Short enough.\n└── 1.2 Also short.`;
    // Act + Assert
    expect(wrap(text, 100)).toBe(text);
  });

  it("wraps a too-wide branch with a hanging indent under its text column", () => {
    // Arrange — width 14 fits "├── 1.1 foo" (11 columns) but not " bar".
    const text = `${R}\n├── 1.1 foo bar\n└── 1.2 x`;
    // Act + Assert
    expect(wrap(text, 14)).toBe(`${R}\n├── 1.1 foo\n│       bar\n└── 1.2 x`);
  });

  it("blanks a wrapped last child's connector column", () => {
    // Act + Assert
    expect(wrap(`${R}\n└── 1.1 foo bar`, 14)).toBe(`${R}\n└── 1.1 foo\n        bar`);
  });

  it("aligns a wrapped root under its own text, holding its children's rail", () => {
    // Act + Assert
    expect(wrap("1 🎯 foo bar\n└── 1.1 x", 9)).toBe("1 🎯 foo\n│ bar\n└── 1.1 x");
  });

  it("extends every ancestor rule on a nested continuation", () => {
    // Arrange
    const text = `${R}\n├── 1.1 a\n│   ├── 1.1.1 foo bar\n│   └── 1.1.2 y\n└── 1.2 z`;
    // Act + Assert
    expect(wrap(text, 20)).toBe(`${R}\n├── 1.1 a\n│   ├── 1.1.1 foo\n│   │         bar\n│   └── 1.1.2 y\n└── 1.2 z`);
  });

  it("holds open the column of a wrapped branch's own children", () => {
    // Arrange — a wrap above a subtree keeps the child connector column busy.
    const text = `${R}\n├── 1.1 aaaa bbbb cccc\n│   └── 1.1.1 Leaf.\n└── 1.2 z`;
    // Act + Assert
    expect(wrap(text, 21)).toBe(`${R}\n├── 1.1 aaaa bbbb\n│   │   cccc\n│   └── 1.1.1 Leaf.\n└── 1.2 z`);
  });

  it("blanks the column when the next branch is a sibling, not a child", () => {
    // Arrange
    const text = `${R}\n├── 1.1 aaaa bbbb cccc\n└── 1.2 Leaf.`;
    // Act + Assert
    expect(wrap(text, 21)).toBe(`${R}\n├── 1.1 aaaa bbbb\n│       cccc\n└── 1.2 Leaf.`);
  });

  it("wraps repeatedly when one continuation is not enough", () => {
    // Act + Assert
    expect(wrap(`${R}\n├── 1.1 aaa bbb ccc ddd\n└── 1.2 x`, 12)).toBe(
      `${R}\n├── 1.1 aaa\n│       bbb\n│       ccc\n│       ddd\n└── 1.2 x`,
    );
  });

  it("preserves a blank line between roots", () => {
    // Arrange
    const text = "1 🔧 First.\n└── 1.1 a\n\n2 ✅ Second.\n└── 2.1 b";
    // Act + Assert
    expect(wrap(text, 100)).toBe(text);
  });

  it("never splits an inline element that fits on its own", () => {
    // Arrange — the field is 15 - 8 = 7 columns, so the anchor lands whole.
    const text = `${R}\n└── 1.1 see <a href="https://x/y">symbol</a> now`;
    // Act + Assert
    expect(wrap(text, 15)).toBe(`${R}\n└── 1.1 see\n        <a href="https://x/y">symbol</a>\n        now`);
  });

  it("splits an overlong element with its tags closed and reopened", () => {
    // Act + Assert
    expect(wrap(`${R}\n└── 1.1 <mark><b>alpha beta gamma</b></mark>`, 18)).toBe(
      `${R}\n└── 1.1 <mark><b>alpha beta</b></mark>\n        <mark><b>gamma</b></mark>`,
    );
  });

  it("measures entities as one character each", () => {
    // Arrange — "&lt;plugin&gt;" renders as 8 columns, so it fits 16.
    const text = `${R}\n└── 1.1 &lt;plugin&gt;`;
    // Act + Assert
    expect(wrap(text, 16)).toBe(text);
  });

  it("reports an unsplittable word wider than the limit rather than truncating it", () => {
    // Act
    const result = formatTree(tree(`${R}\n└── 1.1 aaaaaaaaaaaaaaaaaaaa`), 12);
    // Assert — the word is kept whole and named in overflows.
    expect([result.overflows, lineText(result.lines[1])]).toEqual([
      ["aaaaaaaaaaaaaaaaaaaa"],
      "└── 1.1 aaaaaaaaaaaaaaaaaaaa",
    ]);
  });

  it("refuses a branch whose prefix alone exceeds the width", () => {
    // Arrange — "    └── 1.1.1 " is 14 columns, wider than 10.
    const deep = tree(`${R}\n└── 1.1 a\n    └── 1.1.1 text`);
    // Act + Assert
    expect(() => formatTree(deep, 10)).toThrow(TreeOverflowError);
  });
});

// --- Inline-code balancing across a wrap break ---
//
// The branch body is RAW MARKDOWN rendered later through the `inline()` pass, so
// a backtick code span split across two wrapped lines must be closed at the end
// of one line and reopened at the start of the next, leaving every line a
// self-contained, balanced inline-code span.

/** The count of unescaped backticks in S (a `\`` is a literal, not counted). */
function unescapedBackticks(s: string): number {
  let count = 0;
  for (let i = 0; i < s.length; i++) {
    if (s[i] === "\\") {
      i++;
      continue;
    }
    if (s[i] === "`") count++;
  }
  return count;
}

describe("wrap engine — inline-code balancing", () => {
  it("wraps a long inline-code branch into lines that are each balanced", () => {
    // Arrange — the owner's example: one long inline-code span on a root branch.
    const body = `${R}\n└── 1.1 \`i am longer than one line after 'one'\``;
    // Act
    const lines = formatTree(tree(body), 29).lines.map(lineText);
    // Assert — it wrapped, and every wrapped line has even (balanced) backticks.
    expect(lines.length).toBeGreaterThan(1);
    expect(lines.every((l) => unescapedBackticks(l) % 2 === 0)).toBe(true);
  });

  it("closes and reopens the span at a break that falls mid-code-span", () => {
    // Arrange — width 18 leaves a 10-column field; the span breaks twice.
    // Act + Assert — each line is its own valid `code` span, none unbalanced.
    expect(wrap(`${R}\n└── 1.1 \`alpha beta gamma\``, 18)).toBe(
      `${R}\n└── 1.1 \`alpha\`\n        \`beta\`\n        \`gamma\``,
    );
  });

  it("leaves a code span that fits on one line untouched", () => {
    // Arrange — the whole branch fits, so no backtick is added or moved.
    const body = `${R}\n├── 1.1 see \`short code\` here\n└── 1.2 x`;
    // Act + Assert
    expect(wrap(body, 100)).toBe(body);
  });

  it("balances the code span in a branch mixing plain text and code", () => {
    // Arrange — plain words surround a code span that must break across lines.
    const body = `${R}\n└── 1.1 run \`alpha beta gamma delta\` now`;
    // Act
    const lines = formatTree(tree(body), 20).lines.map(lineText);
    // Assert — it wrapped and no line carries an odd/unbalanced backtick.
    expect(lines.length).toBeGreaterThan(1);
    expect(lines.every((l) => unescapedBackticks(l) % 2 === 0)).toBe(true);
    // The plain trailing word rejoins prose, carrying no open span across it.
    expect(lines.some((l) => l.includes("now") && unescapedBackticks(l) % 2 === 0)).toBe(true);
  });

  it("never itself overflows the width with the added balancing backticks", () => {
    // Arrange — a wide code span at a tight width; balancing must not spill.
    const body = `${R}\n└── 1.1 \`aaaa bbbb cccc dddd eeee ffff\``;
    // Act
    const lines = formatTree(tree(body), 17).lines.map(lineText);
    // Assert — every emitted line, backticks included, stays within the limit.
    expect(lines.every((l) => visibleWidth(l) <= 17)).toBe(true);
  });

  it("treats a backslash-escaped backtick as a literal, not a delimiter", () => {
    // Arrange — the escaped backtick opens no span, so no balancing is added.
    const body = `${R}\n└── 1.1 a \\\` b c d e f g h i j k`;
    // Act — it must wrap without throwing and keep the literal escaped backtick.
    const lines = formatTree(tree(body), 17).lines.map(lineText);
    // Assert — the escaped backtick survives and only it is present (odd count,
    // because it is a literal the wrapper never balanced).
    const joined = lines.join("\n");
    expect(joined).toContain("\\`");
    expect(unescapedBackticks(joined)).toBe(0);
  });

  it("renders each wrapped code line as a <code> element, not a stray backtick", () => {
    // Arrange — a minimal inline pass that turns `x` spans into <code>x</code>.
    const inlineCode = (s: string): string => s.replace(/`([^`]+)`/g, "<code>$1</code>");
    // Act
    const html = renderTreeHtml(tree(`${R}\n└── 1.1 \`alpha beta gamma\``), inlineCode, 18);
    // Assert — every content span of the branch became a <code>, and no bare
    // backtick leaks.
    const contents = [...html.matchAll(/<span class="mp-content">(.*?)<\/span>/g)].map((m) => m[1]).slice(1);
    expect(contents.length).toBeGreaterThan(1);
    expect(contents.every((c) => c.includes("<code>") && !c.includes("`"))).toBe(true);
  });
});

describe("renderTreeHtml", () => {
  it("renders a fitting branch as a prefix/content pair with real connectors", () => {
    // Act
    const html = renderTreeHtml(tree(`${R}\n├── 1.1 Detail\n└── 1.2 x`), identity, 100);
    // Assert
    expect(html).toContain(`<span class="mp-prefix">├── 1.1 </span>`);
    expect(html).toContain(`<span class="mp-content">Detail</span>`);
  });

  it("emits real rail characters on a wrapped branch's continuation", () => {
    // Arrange — 1.1 has a child, so the wrap holds a rail; force a wrap at 15.
    const html = renderTreeHtml(tree(`${R}\n├── 1.1 aaaa bbbb cccc\n│   └── 1.1.1 x\n└── 1.2 y`), identity, 15);
    // Assert — the continuation prefix carries the ancestor rail plus 1.1's own
    // held-open child rail as REAL characters in the prefix span, no repaint.
    expect(html).toContain(`<span class="mp-prefix">│   │`);
    expect(html).not.toContain("mp-rail");
  });

  it("renders blank lines as spacer rows", () => {
    // Act
    const html = renderTreeHtml(tree("1 🔧 A\n└── 1.1 a\n\n2 ✅ B\n└── 2.1 b"), identity, 100);
    // Assert
    expect(html).toContain(`class="mp-line mp-blank"`);
  });

  it("escapes markup in tree content", () => {
    // Act
    const html = renderTreeHtml(tree(`${R}\n└── 1.1 <img src=x>`), identity, 100);
    // Assert
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img src=x&gt;");
  });

  it("routes content through the injected inline pass", () => {
    // Arrange
    const shout = (s: string): string => s.toUpperCase();
    // Act + Assert
    expect(renderTreeHtml(tree(`${R}\n└── 1.1 detail`), shout, 100)).toContain("DETAIL");
  });

  it("falls back to unwrapped lines and surfaces the issue when a prefix cannot fit", () => {
    // Arrange — a prefix wider than the width, and a spy for the issue callback.
    const issues: string[] = [];
    // Act
    const html = renderTreeHtml(tree(`${R}\n└── 1.1 a\n    └── 1.1.1 text`), identity, 10, (message) =>
      issues.push(message),
    );
    // Assert — the tree is still drawn, and the refusal was surfaced, not swallowed.
    expect(html).toContain("text");
    expect(issues).toHaveLength(1);
    expect(issues[0]).toContain("could not be wrapped");
  });

  it("renders an interior fenced block as an opaque code block, not tree lines", () => {
    // Arrange — a code fence nested beneath 1.2, with a branch after it.
    const text = [
      "1 🔧 Made a file",
      "├── 1.1 File contents",
      "    ```python",
      '    if __name__ == "__main__":',
      "        main()",
      "    ```",
      "└── 1.2 Done",
    ].join("\n");
    // Act
    const html = renderTreeHtml(tree(text), identity, 100);
    // Assert — the code sits in a <pre><code>, not a tree line, and is NOT
    // markdown-parsed, so `__name__` stays literal and is never bolded.
    expect(html).toContain('<pre class="md-code">');
    expect(html).toContain("__name__");
    expect(html).not.toContain("<strong>");
    // The fence delimiters themselves never leak as tree lines.
    expect(html).not.toContain("```");
    // Branches around the code still render as tree lines.
    expect(html).toContain(`<span class="mp-prefix">└── 1.2 </span>`);
  });

  it("renders a fence drawn behind its branch's rail as a code block with the rail stripped", () => {
    // Arrange — the delimiters and body all carry the `│   ` rail of 1.1.
    const text = [
      "1 🔧 Refused",
      "├── 1.1 The daemon logged the refusal",
      "│   ```json",
      '│   "cause": "unknown_repository"',
      "│   ```",
      "└── 1.2 Done",
    ].join("\n");
    // Act
    const html = renderTreeHtml(tree(text), identity, 100);
    // Assert — one code block holding the body without its rail, no delimiter
    // leaking as a tree line, and the branch after it still a tree line.
    expect(html).toMatch(/<pre class="md-code">(?:(?!<\/pre>).)*<code class="hljs lang-json">/);
    expect(html).not.toContain("```");
    expect(html).not.toMatch(/<code[^>]*>│/);
    expect(html).toContain(`<span class="mp-prefix">└── 1.2 </span>`);
  });

  it("keeps a code body's own rail beyond its fence's indent", () => {
    // Arrange — an unrailed fence whose code is itself a drawn tree.
    const text = [R, "├── 1.1 Shape", "    ```", "    │   ├── x", "    ```", "└── 1.2 Done"].join("\n");
    // Act
    const html = renderTreeHtml(tree(text), identity, 100);
    // Assert
    expect(html).toContain("<code class=\"hljs\">│   ├── x</code>");
  });

  it("escapes an interior fenced block's markup rather than emitting it", () => {
    // Arrange — a language-less fence whose body carries a raw tag.
    const text = [R, "├── 1.1 Snippet", "    ```", "    <img src=x>", "    ```", "└── 1.2 Done"].join("\n");
    // Act
    const html = renderTreeHtml(tree(text), identity, 100);
    // Assert — the tag is escaped inside the code block, not rendered.
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img src=x&gt;");
  });
});

describe("renderTreeHtml's column budget", () => {
  it("exports no default width to fall back to", () => {
    // Act + Assert
    expect(Object.keys(metapromptTree)).not.toContain("DEFAULT_TREE_COLS");
  });

  it.each([
    { name: "zero", width: 0 },
    { name: "a negative width", width: -3 },
    { name: "a fractional width", width: 40.5 },
    { name: "NaN", width: Number.NaN },
  ])("refuses $name rather than guessing a width", ({ width }) => {
    // Act + Assert
    expect(() => renderTreeHtml(tree(`${R}\n└── 1.1 Detail`), identity, width)).toThrow(RangeError);
  });

  it("records the refused width through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => renderTreeHtml(tree(`${R}\n└── 1.1 Detail`), identity, 0)).toThrow(RangeError);
    // Assert
    const record = await forwardedRecord(capture, "metaprompt-tree.invalid-width");
    expect([record.level.case, record.context]).toEqual(["error", expect.objectContaining({ width: 0 })]);
  });
});

describe("boxWideChars", () => {
  const box = (cluster: string): string => `<span class="${WIDE_CHAR_CLASS}">${cluster}</span>`;

  it.each([
    { name: "an astral emoji", html: "a 🔧 b", want: `a ${box("🔧")} b` },
    { name: "an emoji with its variation selector", html: "✏️x", want: `${box("✏️")}x` },
    { name: "a BMP emoji", html: "✅", want: box("✅") },
    { name: "a CJK ideograph", html: "中", want: box("中") },
    { name: "a zero-width-joined pair, joiner outside", html: "👨\u200d👩", want: `${box("👨")}\u200d${box("👩")}` },
    { name: "narrow text and box-drawing rails", html: "│   ├── 1.1 abc", want: "│   ├── 1.1 abc" },
    { name: "an emoji inside a tag's text, the tag untouched", html: "<code>🔧</code>", want: `<code>${box("🔧")}</code>` },
    { name: "an emoji-looking attribute, never split", html: '<a title="🔧">x</a>', want: '<a title="🔧">x</a>' },
  ])("boxes $name", ({ html, want }) => {
    // Act + Assert
    expect(boxWideChars(html)).toBe(want);
  });

  it("boxes exactly the characters the width model counts as two columns", () => {
    // Arrange
    const text = "1 🔧 ✏️ ✅ 中 a";
    // Act — the box count, times two, plus the narrow characters left outside.
    const boxed = boxWideChars(text);
    const boxes = boxed.split(`class="${WIDE_CHAR_CLASS}"`).length - 1;
    const outside = boxed.replace(new RegExp(`<span class="${WIDE_CHAR_CLASS}">[^<]*</span>`, "g"), "");
    // Assert
    expect(boxes * 2 + visibleWidth(outside)).toBe(visibleWidth(text));
  });

  it("draws a root line's emoji inside a two-column box", () => {
    // Act
    const html = renderTreeHtml(tree("1 🔧 Fixed it\n└── 1.1 x"), identity, 100);
    // Assert
    expect(html).toContain(`<span class="mp-content">${box("🔧")} Fixed it</span>`);
  });
});

describe("inline code keeps its runs of spaces", () => {
  const wrapped = (body: string, width: number): string[] => formatTree(tree(body), width).lines.map(lineText);

  it("keeps a run of spaces inside an inline-code span on an unwrapped line", () => {
    // Arrange
    const body = [R, "└── 1.1 Inline `a    b` here."].join("\n");

    // Act
    const lines = wrapped(body, 200);

    // Assert
    expect(lines[1]).toBe("└── 1.1 Inline `a    b` here.");
  });

  it("keeps the span whole, spaces and all, when its line wraps elsewhere", () => {
    // Arrange
    const body = [R, "└── 1.1 a long line of plain prose that has to wrap `x  y` and then more prose after it"].join("\n");

    // Act
    const lines = wrapped(body, 40);

    // Assert
    expect(lines.some((line) => line.includes("`x  y`"))).toBe(true);
  });

  it("still collapses runs of spaces outside any span", () => {
    // Arrange
    const body = [R, "└── 1.1 plain    prose"].join("\n");

    // Act
    const lines = wrapped(body, 200);

    // Assert
    expect(lines[1]).toBe("└── 1.1 plain prose");
  });

  it("reads an unmatched backtick as prose, so the rest of the line still wraps", () => {
    // Arrange
    const body = [R, "└── 1.1 a stray ` backtick followed by enough plain words to need a wrap here"].join("\n");

    // Act
    const lines = wrapped(body, 40);

    // Assert
    expect(lines.length).toBeGreaterThan(2);
    expect(lines.every((line) => line.length <= 40)).toBe(true);
  });
});

describe("renderTreeHtml wraps by the width a line draws", () => {
  const lineContents = (html: string): string[] =>
    [...html.matchAll(/<span class="mp-content">(.*?)<\/span><\/div>/g)].map((m) => m[1]);
  const drawn = (content: string): string => content.replace(/<[^>]*>/g, "");

  it.each([
    {
      name: "a ~/ code span file link",
      src: "└── 1.1 see `~/a/very/long/directory/path/to/b.ts:12` now",
      text: "see b.ts:12 now",
    },
    {
      name: "an explicit link whose text is a path",
      src: "└── 1.1 see [~/a/very/long/directory/b.ts](~/a/very/long/directory/b.ts) now",
      text: "see b.ts now",
    },
  ])("keeps a branch on one line when $name draws only its file name", ({ src, text }) => {
    // Arrange — "└── 1.1 " is 8 columns; the drawn text fits 8 + 16, the source does not.
    // Act
    const contents = lineContents(renderTreeHtml(tree(`${R}\n${src}`), inline, 24)).slice(1);
    // Assert
    expect(contents.map(drawn)).toEqual([text]);
  });

  it("wraps before a link rather than cutting its markdown apart", () => {
    // Act — the drawn "aaaa the long config" is 20 columns, wider than the 16-column field.
    const contents = lineContents(renderTreeHtml(tree(`${R}\n└── 1.1 aaaa [the long config](~/x.el)`), inline, 24)).slice(1);
    // Assert — the link is one whole anchor on the continuation, never a literal "[" or "](".
    expect(contents.map(drawn)).toEqual(["aaaa", "the long config"]);
    expect(contents[1]).toMatch(/^<a href="~\/x.el"[^>]*>the long config<\/a>$/);
  });

  it("draws a bold phrase that crosses no wrap with no literal asterisks", () => {
    // Act
    const contents = lineContents(renderTreeHtml(tree(`${R}\n└── 1.1 aaaa **bold words**`), inline, 18)).slice(1);
    // Assert
    expect(contents.join("")).not.toContain("*");
    expect(contents.join("")).toContain("<strong>bold words</strong>");
  });

  it("does not count a code span's backticks toward the width", () => {
    // Act — "abc defgh" draws 9 columns, exactly the field; with backticks it is 11.
    const contents = lineContents(renderTreeHtml(tree(`${R}\n└── 1.1 abc \`defgh\``), inline, 17)).slice(1);
    // Assert
    expect(contents).toEqual(["abc <code>defgh</code>"]);
  });

  it("runs a header line outside any branch through the inline pass", () => {
    // Act
    const html = renderTreeHtml(tree("Response (`x`)\n\n1 🔧 A\n└── 1.1 b"), inline, 40);
    // Assert
    expect(html).toContain("Response (<code>x</code>)");
  });
});

describe("formatTree without an inline pass", () => {
  it("still wraps the raw markdown, backticks counted, as the daemon formatter does", () => {
    // Act
    const { lines } = formatTree(tree(`${R}\n└── 1.1 abc \`defgh\``), 17);
    // Assert
    expect(lines.map(lineText)).toEqual([R, "└── 1.1 abc", "        `defgh`"]);
  });
});
