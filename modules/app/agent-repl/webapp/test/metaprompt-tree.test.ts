import { describe, expect, it } from "vitest";
import * as metapromptTree from "../src/metaprompt-tree.js";
import {
  WIDE_CHAR_CLASS,
  boxWideChars,
  findTreeRegion,
  formatTree,
  isMetapromptTree,
  lineText,
  looksLikeIntendedTree,
  renderTreeHtml,
  TreeOverflowError,
  visibleWidth,
} from "../src/metaprompt-tree.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

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

/**
 * wrapBody is the oracle harness, mirroring the daemon treefmt suite's own
 * `wrap` helper: format a bare tree body and return the wrapped text lines
 * rejoined, so a case can be stated exactly as the Go test states it.
 */
function wrapBody(body: string, width: number): string {
  return formatTree(body.split("\n"), width)
    .lines.map(lineText)
    .join("\n");
}

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

// --- The wrap engine, checked against the daemon treefmt suite's own cases ---
//
// Each case below has the same input, width and expectation as the Go test it
// mirrors (daemon/internal/treefmt/treefmt_test.go), so the TypeScript port is
// checked against the daemon algorithm's own evidence rather than a rewrite of
// it. The daemon used a fixed 105-column width; the port takes the width as an
// argument, but the algorithm producing the wrap is identical.

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
    { name: "zero-width joiner is free", raw: "a‍b", want: 2 },
  ])("$name", ({ raw, want }) => {
    // Act + Assert
    expect(visibleWidth(raw)).toBe(want);
  });
});

describe("wrap engine — the daemon algorithm, ported", () => {
  it("leaves a branch that fits untouched", () => {
    // Arrange — both branches are inside the width.
    const body = "├── 1.1. Short enough.\n└── 1.2. Also short.";
    // Act + Assert
    expect(wrapBody(body, 100)).toBe(body);
  });

  it("wraps a too-wide branch with a hanging indent under its text column", () => {
    // Arrange — width 15 fits "├── 1.3. foo" (12 columns) but not " bar".
    // Act + Assert
    expect(wrapBody("├── 1.3. foo bar", 15)).toBe("├── 1.3. foo\n│        bar");
  });

  it("blanks a wrapped last child's connector column", () => {
    // Act + Assert
    expect(wrapBody("└── 1.3. foo bar", 15)).toBe("└── 1.3. foo\n         bar");
  });

  it("aligns a wrapped root under its own text", () => {
    // Act + Assert
    expect(wrapBody("1. foo bar", 8)).toBe("1. foo\n   bar");
  });

  it("extends every ancestor rule on a nested continuation", () => {
    // Act + Assert
    expect(wrapBody("│   ├── 1.2.1. foo bar", 21)).toBe("│   ├── 1.2.1. foo\n│   │          bar");
  });

  it("holds open the column of a wrapped branch's own children", () => {
    // Arrange — a wrap above a subtree keeps the child connector column busy.
    const body = "├── 1.1. aaaa bbbb cccc\n│   └── 1.1.1. Leaf.";
    // Act + Assert
    expect(wrapBody(body, 22)).toBe("├── 1.1. aaaa bbbb\n│   │    cccc\n│   └── 1.1.1. Leaf.");
  });

  it("blanks the column when the next branch is a sibling, not a child", () => {
    // Arrange.
    const body = "├── 1.1. aaaa bbbb cccc\n└── 1.2. Leaf.";
    // Act + Assert
    expect(wrapBody(body, 22)).toBe("├── 1.1. aaaa bbbb\n│        cccc\n└── 1.2. Leaf.");
  });

  it("wraps repeatedly when one continuation is not enough", () => {
    // Act + Assert
    expect(wrapBody("├── 1.1. aaa bbb ccc ddd", 13)).toBe(
      "├── 1.1. aaa\n│        bbb\n│        ccc\n│        ddd",
    );
  });

  it("wraps an emoji-prefixed root under its emoji", () => {
    // Act + Assert
    expect(wrapBody("1. 🎯 goal here", 12)).toBe("1. 🎯 goal\n   here");
  });

  it("preserves a blank line between roots", () => {
    // Arrange.
    const body = "1. First.\n\n2. Second.";
    // Act + Assert
    expect(wrapBody(body, 100)).toBe(body);
  });

  it("never splits an inline element that fits on its own", () => {
    // Arrange — field is 16 - 9 = 7 columns, so the anchor lands whole.
    const body = `├── 1.1. see <a href="https://x/y">symbol</a> now`;
    // Act + Assert
    expect(wrapBody(body, 16)).toBe(
      '├── 1.1. see\n│        <a href="https://x/y">symbol</a>\n│        now',
    );
  });

  it("splits an overlong element with its tags closed and reopened", () => {
    // Act + Assert
    expect(wrapBody("1. <mark><b>alpha beta gamma</b></mark>", 13)).toBe(
      "1. <mark><b>alpha beta</b></mark>\n   <mark><b>gamma</b></mark>",
    );
  });

  it("measures entities as one character each", () => {
    // Arrange — "&lt;plugin&gt;" renders as 8 columns, so it fits 12.
    // Act + Assert
    expect(wrapBody("1. &lt;plugin&gt;", 12)).toBe("1. &lt;plugin&gt;");
  });

  it("is idempotent: re-wrapping already-wrapped output changes nothing", () => {
    // Arrange.
    const once = wrapBody("├── 1.1. aaa bbb ccc\n└── 1.2. x", 13);
    // Act + Assert
    expect(wrapBody(once, 13)).toBe(once);
  });

  it("unwraps when re-wrapped at a wider limit", () => {
    // Arrange.
    const narrow = wrapBody("├── 1.1. aaa bbb ccc\n└── 1.2. x", 13);
    // Act + Assert
    expect(wrapBody(narrow, 100)).toBe("├── 1.1. aaa bbb ccc\n└── 1.2. x");
  });

  it("reports an unsplittable word wider than the limit rather than truncating it", () => {
    // Act
    const result = formatTree(["1. aaaaaaaaaaaaaaaaaaaa"], 10);
    // Assert — the word is kept whole and named in overflows.
    expect(result.overflows).toEqual(["aaaaaaaaaaaaaaaaaaaa"]);
    expect(lineText(result.lines[0])).toContain("aaaaaaaaaaaaaaaaaaaa");
  });

  it("refuses a branch whose prefix alone exceeds the width", () => {
    // Arrange — a prefix that leaves no room for any text.
    // Act + Assert
    expect(() => formatTree(["│   ├── 1.2.1. text"], 10)).toThrow(TreeOverflowError);
  });

  it("wraps a connector line carrying a stray leading space (the live overflow bug)", () => {
    // Arrange — the exact repro line: one leading space before the connector.
    const line =
      " └── 1.1 Nothing has changed since the last message, with hello/ (Go) and hello-rs/ (Rust)";
    // Act
    const result = formatTree([line], 80);
    // Assert — it wraps instead of passing through raw, and every line fits.
    expect(result.lines.length).toBeGreaterThan(1);
    expect(result.lines.every((l) => !l.raw)).toBe(true);
    expect(result.lines.every((l) => visibleWidth(lineText(l)) <= 80)).toBe(true);
  });

  it("preserves the stray leading indent on the branch and its continuations", () => {
    // Arrange — the wrap continuation must hang under the same one-space indent.
    const line = " └── 1.1 alpha beta gamma delta epsilon zeta eta theta iota kappa";
    // Act
    const lines = formatTree([line], 20).lines;
    // Assert — head keeps the leading space, and every wrap line starts with it.
    expect(lineText(lines[0]).startsWith(" └── 1.1 ")).toBe(true);
    expect(lines.slice(1).every((l) => l.prefix.startsWith(" "))).toBe(true);
  });

  it("wraps a deeper indented connector, indentation preserved", () => {
    // Arrange — a stray space before an already-nested connector.
    const line = " │   └── 2.1 alpha beta gamma delta epsilon zeta eta theta iota";
    // Act
    const lines = formatTree([line], 22).lines;
    // Assert — the whole prefix (leading space + rails) is kept and it wraps.
    expect(lines.length).toBeGreaterThan(1);
    expect(lineText(lines[0]).startsWith(" │   └── 2.1 ")).toBe(true);
    expect(lines.every((l) => visibleWidth(lineText(l)) <= 22)).toBe(true);
  });

  it("keeps a legitimate 4-space-aligned level as a segment, not stray indent", () => {
    // Arrange — four leading spaces is one real `└──`-continuation level.
    const line = "    ├── 3.2 alpha beta gamma delta epsilon zeta eta theta iota kappa";
    // Act
    const lines = formatTree([line], 24).lines;
    // Assert — it wraps and the aligned level is preserved verbatim.
    expect(lines.length).toBeGreaterThan(1);
    expect(lineText(lines[0]).startsWith("    ├── 3.2 ")).toBe(true);
    expect(lines.every((l) => visibleWidth(lineText(l)) <= 24)).toBe(true);
  });

  it("wraps a no-leading-space connector exactly as before (regression guard)", () => {
    // Arrange — the same body without the leading space.
    const bare = "└── 1.1 Nothing has changed since the last message, with hello/ (Go) and hello-rs/ (Rust)";
    // Act
    const result = formatTree([bare], 80);
    // Assert — still wraps, still non-raw, still fits.
    expect(result.lines.length).toBe(2);
    expect(result.lines.every((l) => !l.raw)).toBe(true);
    expect(result.lines.every((l) => visibleWidth(lineText(l)) <= 80)).toBe(true);
  });

  it("still passes a genuine indented prose line through raw", () => {
    // Arrange — leading spaces then prose with no connector and no dotted label.
    const prose = "  just some indented prose that is not a tree branch at all";
    // Act
    const lines = formatTree([prose], 20).lines;
    // Assert — untouched: one raw line, verbatim.
    expect(lines).toHaveLength(1);
    expect(lines[0].raw).toBe(true);
    expect(lines[0].body).toBe(prose);
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
    const body = "1. `i am longer than one line after 'one'`";
    // Act
    const lines = formatTree([body], 24).lines.map(lineText);
    // Assert — it wrapped, and every wrapped line has even (balanced) backticks.
    expect(lines.length).toBeGreaterThan(1);
    expect(lines.every((l) => unescapedBackticks(l) % 2 === 0)).toBe(true);
  });

  it("closes and reopens the span at a break that falls mid-code-span", () => {
    // Arrange — width 13 leaves a 10-column field; the span breaks twice.
    // Act + Assert — each line is its own valid `code` span, none unbalanced.
    expect(wrapBody("1. `alpha beta gamma`", 13)).toBe("1. `alpha`\n   `beta`\n   `gamma`");
  });

  it("leaves a code span that fits on one line untouched", () => {
    // Arrange — the whole branch fits, so no backtick is added or moved.
    const body = "├── 1.1 see `short code` here";
    // Act + Assert
    expect(wrapBody(body, 100)).toBe(body);
  });

  it("balances the code span in a branch mixing plain text and code", () => {
    // Arrange — plain words surround a code span that must break across lines.
    const body = "└── 1.2 run `alpha beta gamma delta` now";
    // Act
    const lines = formatTree([body], 20).lines.map(lineText);
    // Assert — it wrapped and no line carries an odd/unbalanced backtick.
    expect(lines.length).toBeGreaterThan(1);
    expect(lines.every((l) => unescapedBackticks(l) % 2 === 0)).toBe(true);
    // The plain trailing word rejoins prose, carrying no open span across it.
    expect(lines.some((l) => l.includes("now") && unescapedBackticks(l) % 2 === 0)).toBe(true);
  });

  it("never itself overflows the width with the added balancing backticks", () => {
    // Arrange — a wide code span at a tight width; balancing must not spill.
    const body = "1. `aaaa bbbb cccc dddd eeee ffff`";
    // Act
    const lines = formatTree([body], 12).lines.map(lineText);
    // Assert — every emitted line, backticks included, stays within the limit.
    expect(lines.every((l) => visibleWidth(l) <= 12)).toBe(true);
  });

  it("treats a backslash-escaped backtick as a literal, not a delimiter", () => {
    // Arrange — the escaped backtick opens no span, so no balancing is added.
    const body = "1. a \\` b c d e f g h i j k";
    // Act — it must wrap without throwing and keep the literal escaped backtick.
    const lines = formatTree([body], 12).lines.map(lineText);
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
    const html = renderTreeHtml("1. `alpha beta gamma`", inlineCode, 13);
    // Assert — every content span became a <code>, and no bare backtick leaks.
    const contents = [...html.matchAll(/<span class="mp-content">(.*?)<\/span>/g)].map((m) => m[1]);
    expect(contents.length).toBeGreaterThan(1);
    expect(contents.every((c) => c.includes("<code>") && !c.includes("`"))).toBe(true);
  });
});

describe("renderTreeHtml", () => {
  it("renders a fitting branch as a prefix/content pair with real connectors", () => {
    // Act
    const html = renderTreeHtml("├── 1.1 Detail", identity, 100);
    // Assert
    expect(html).toContain(`<span class="mp-prefix">├── 1.1 </span>`);
    expect(html).toContain(`<span class="mp-content">Detail</span>`);
  });

  it("emits real rail characters on a wrapped branch's continuation", () => {
    // Arrange — 1.1 has a child, so the wrap holds a rail; force a wrap at 15.
    const html = renderTreeHtml("├── 1.1 aaaa bbbb cccc\n│   └── 1.1.1 x", identity, 15);
    // Assert — the continuation prefix carries the ancestor rail plus 1.1's own
    // held-open child rail as REAL characters in the prefix span, no repaint.
    expect(html).toContain(`<span class="mp-prefix">│   │`);
    expect(html).not.toContain("mp-rail");
  });

  it("renders blank lines as spacer rows", () => {
    // Act
    const html = renderTreeHtml("1 🔧 A\n\n2 ✅ B", identity, 100);
    // Assert
    expect(html).toContain(`class="mp-line mp-blank"`);
  });

  it("escapes markup in tree content", () => {
    // Act
    const html = renderTreeHtml("├── 1.1 <img src=x>", identity, 100);
    // Assert
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img src=x&gt;");
  });

  it("routes content through the injected inline pass", () => {
    // Arrange
    const shout = (s: string): string => s.toUpperCase();
    // Act + Assert
    expect(renderTreeHtml("├── 1.1 detail", shout, 100)).toContain("DETAIL");
  });

  it("falls back to unwrapped lines and surfaces the issue when a prefix cannot fit", () => {
    // Arrange — a prefix wider than the width, and a spy for the issue callback.
    const issues: string[] = [];
    // Act
    const html = renderTreeHtml("│   ├── 1.2.1 text", identity, 10, (message) => issues.push(message));
    // Assert — the tree is still drawn, and the refusal was surfaced, not swallowed.
    expect(html).toContain("text");
    expect(issues).toHaveLength(1);
    expect(issues[0]).toContain("could not be wrapped");
  });

  it("renders an interior fenced block as an opaque code block, not tree lines", () => {
    // Arrange — a code fence nested beneath 1.2, with a branch after it.
    const tree = [
      "1 🔧 Made a file",
      "├── 1.2 File contents",
      "    ```python",
      '    if __name__ == "__main__":',
      "        main()",
      "    ```",
      "└── 1.3 Done",
    ].join("\n");
    // Act
    const html = renderTreeHtml(tree, identity, 100);
    // Assert — the code sits in a <pre><code>, not a tree line, and is NOT
    // markdown-parsed, so `__name__` stays literal and is never bolded.
    expect(html).toContain('<pre class="md-code">');
    expect(html).toContain("__name__");
    expect(html).not.toContain("<strong>");
    // The fence delimiters themselves never leak as tree lines.
    expect(html).not.toContain("```");
    // Branches around the code still render as tree lines.
    expect(html).toContain(`<span class="mp-prefix">└── 1.3 </span>`);
  });

  it("renders a fence drawn behind its branch's rail as a code block with the rail stripped", () => {
    // Arrange — the delimiters and body all carry the `│   ` rail of 2.3.
    const tree = [
      "├── 2.3 The daemon logged the refusal",
      "│   ```json",
      '│   "cause": "unknown_repository"',
      "│   ```",
      "└── 2.4 Done",
    ].join("\n");
    // Act
    const html = renderTreeHtml(tree, identity, 100);
    // Assert — one code block holding the body without its rail, no delimiter
    // leaking as a tree line, and the branch after it still a tree line.
    expect(html).toContain('<pre class="md-code"><code class="hljs lang-json">');
    expect(html).not.toContain("```");
    expect(html).not.toMatch(/<code[^>]*>│/);
    expect(html).toContain(`<span class="mp-prefix">└── 2.4 </span>`);
  });

  it("keeps a code body's own rail beyond its fence's indent", () => {
    // Arrange — an unrailed fence whose code is itself a drawn tree.
    const tree = ["├── 1.1 Shape", "    ```", "    │   ├── x", "    ```", "└── 1.2 Done"].join("\n");
    // Act
    const html = renderTreeHtml(tree, identity, 100);
    // Assert
    expect(html).toContain("<code class=\"hljs\">│   ├── x</code>");
  });

  it("escapes an interior fenced block's markup rather than emitting it", () => {
    // Arrange — a language-less fence whose body carries a raw tag.
    const tree = ["├── 1.1 Snippet", "    ```", "    <img src=x>", "    ```", "└── 1.2 Done"].join("\n");
    // Act
    const html = renderTreeHtml(tree, identity, 100);
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
    expect(() => renderTreeHtml("├── 1.1 Detail", identity, width)).toThrow(RangeError);
  });

  it("records the refused width through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => renderTreeHtml("├── 1.1 Detail", identity, 0)).toThrow(RangeError);
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
    const html = renderTreeHtml("1 🔧 Fixed it", identity, 100);
    // Assert
    expect(html).toContain(`<span class="mp-content">${box("🔧")} Fixed it</span>`);
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

  it("spans an interior fenced block and keeps every later branch in the region", () => {
    // Arrange — a code fence attached beneath 1.2, with 1.3/1.4 after it: the
    // whole thing is one tree region, the fence is interior, nothing spills.
    const text = [
      HEADER,
      "1 🔧 Subagent created `hello_world.py` at the repo root",
      "├── 1.2 File contents",
      "    ```python",
      "    def main() -> None:",
      '        print("Hello, world!")',
      "",
      '    if __name__ == "__main__":',
      "        main()",
      "    ```",
      "├── 1.3 Follows your Python conventions",
      "└── 1.4 Verified by running `./hello_world.py`",
    ].join("\n");
    // Act
    const region = findTreeRegion(text);
    // Assert — nothing spilled into `after`, and the branches after the fence
    // plus the fence itself all stayed inside the tree region.
    expect(region?.after).toBe("");
    expect(region?.before).toBe("");
    expect(region?.tree).toContain("```python");
    expect(region?.tree).toContain("1.3 Follows");
    expect(region?.tree).toContain("1.4 Verified");
  });

  it("spans an interior fenced block drawn behind its branch's rail", () => {
    // Arrange — the 2026-09-30 response: the fence under 2.3 carried the `│`
    // rail, and every branch after it spilled onto the markdown path.
    const text = [
      HEADER,
      "2 🔍 Root cause",
      "├── 2.3 The daemon logged the refusal",
      "│   ```json",
      '│   "cause": "unknown_repository"',
      "│   ```",
      "└── 2.4 The session still said it dispatched",
      "",
      "3 ⚠️ Knock-on",
      "└── 3.1 The same line misleads later dispatchers",
    ].join("\n");
    // Act
    const region = findTreeRegion(text);
    // Assert
    expect(region?.after).toBe("");
    expect(region?.tree).toContain("2.4 The session");
    expect(region?.tree).toContain("3.1 The same line");
  });

  it("keeps a trailing fenced block with a language tag in `after`", () => {
    // Arrange — a fence with no branch after it is trailing, not interior.
    const region = findTreeRegion(`${HEADER}\n\n${BODY}\n\n\`\`\`python\nx = 1\n\`\`\``);
    // Assert
    expect(region?.tree).toBe(`${HEADER}\n\n${BODY}`);
    expect(region?.after).toContain("```python");
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

  it("is false for text that is entirely blank, having no first line to read", () => {
    // Act + Assert
    expect(looksLikeIntendedTree("\n   \n\n")).toBe(false);
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
