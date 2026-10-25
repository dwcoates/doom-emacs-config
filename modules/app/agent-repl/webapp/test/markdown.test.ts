// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { codeBlockHtml } from "../src/code-block.js";
import { inline, LEAD_CLASS, renderMarkdown, setInlineMarkdown } from "../src/markdown.js";

describe("renderMarkdown blocks", () => {
  it("renders headings at their level", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("## Title")).toBe("<h2>Title</h2>\n");
  });

  it("renders paragraphs with soft line breaks", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("one\ntwo")).toBe("<p>one<br>\ntwo</p>\n");
  });

  it("separates paragraphs on blank lines", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("a\n\nb")).toBe("<p>a</p>\n<p>b</p>\n");
  });

  it("renders unordered lists", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- a\n- b")).toBe("<ul>\n<li>a</li>\n<li>b</li>\n</ul>\n");
  });

  it("renders ordered lists", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("1. a\n2. b")).toBe("<ol>\n<li>a</li>\n<li>b</li>\n</ol>\n");
  });

  it("splits adjacent lists of different kinds", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- a\n1. b")).toBe(
      "<ul>\n<li>a</li>\n</ul>\n<ol>\n<li>b</li>\n</ol>\n",
    );
  });

  it("nests a sub-list inside its parent item", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- a\n  - b")).toBe(
      "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n</ul>\n",
    );
  });

  it("renders blockquotes", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("> wisdom")).toBe("<blockquote>\n<p>wisdom</p>\n</blockquote>\n");
  });

  it("renders horizontal rules", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("---")).toBe("<hr>\n");
  });
});

describe("renderMarkdown GFM", () => {
  it("renders a table with header and body cells", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("| A | B |\n|---|---|\n| 1 | 2 |")).toBe(
      "<table>\n<thead>\n<tr>\n<th>A</th>\n<th>B</th>\n</tr>\n</thead>\n" +
        "<tbody>\n<tr>\n<td>1</td>\n<td>2</td>\n</tr>\n</tbody>\n</table>\n",
    );
  });

  it("renders strikethrough", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("~~gone~~")).toBe("<p><s>gone</s></p>\n");
  });

  it("renders an unchecked task-list item as a disabled checkbox", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- [ ] todo")).toContain(
      `<li class="task-list-item"><input class="task-list-item-checkbox" disabled="" type="checkbox"> todo</li>`,
    );
  });

  it("marks a checked task-list item", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- [x] done")).toContain(
      `<li class="task-list-item"><input class="task-list-item-checkbox" checked="" disabled="" type="checkbox"> done</li>`,
    );
  });

  it("autolinks a bare http URL", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("see https://example.com now")).toContain(
      `<a href="https://example.com" target="_blank" rel="noopener noreferrer">https://example.com</a>`,
    );
  });

  it("renders an http image", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("![alt](https://example.com/i.png)")).toBe(
      `<p><img src="https://example.com/i.png" alt="alt"></p>\n`,
    );
  });

  it("rejects a non-http image src", () => {
    // Arrange + Act + Assert — validateLink gates images too.
    expect(renderMarkdown("![x](javascript:alert(1))")).not.toContain("<img");
  });
});

describe("renderMarkdown fenced code", () => {
  it("syntax-highlights fenced code with a known language tag", () => {
    // Arrange
    const src = "```go\nfunc main() {}\n```";
    // Act
    const html = renderMarkdown(src);
    // Assert — hljs classes present and the language class preserved.
    expect(html).toContain(`<code class="hljs lang-go">`);
    expect(html).toContain(`<span class="hljs-keyword">func</span>`);
  });

  it("syntax-highlights fenced protobuf code", () => {
    // Arrange
    const src = "```protobuf\nmessage Foo {}\n```";
    // Act
    const html = renderMarkdown(src);
    // Assert
    expect(html).toContain(`<code class="hljs lang-protobuf">`);
    expect(html).toContain(`<span class="hljs-title class_">Foo</span>`);
  });

  it("falls back to plain escaped text for an unknown language tag", () => {
    // Arrange
    const src = "```klingon\n<qapla> & stuff\n```";
    // Act + Assert — escaped, no hljs token spans.
    expect(renderMarkdown(src)).toContain(
      `<code class="hljs lang-klingon">&lt;qapla&gt; &amp; stuff</code></pre>`,
    );
  });

  it("draws every non-tree fence as the shared code block (codeBlockHtml)", () => {
    // Arrange
    const cases: Array<[string, string]> = [
      ["", "<b>raw</b>"],
      ["klingon", "<qapla> & stuff"],
      ["ts", "class Foo {}"],
    ];
    // Act + Assert
    for (const [lang, body] of cases) {
      expect(renderMarkdown("```" + lang + "\n" + body + "\n```")).toBe(codeBlockHtml(body, lang));
    }
  });

  it("renders a language-less fence as plain escaped text", () => {
    // Arrange
    const src = "```\n<b>raw</b>\n```";
    // Act + Assert
    expect(renderMarkdown(src)).toContain(`<code class="hljs">&lt;b&gt;raw&lt;/b&gt;</code></pre>`);
  });

  it("escapes HTML inside highlighted code (hljs escape guarantee)", () => {
    // Arrange — markup-bearing string literal in a highlighted language.
    const src = "```js\nconst x = \"<img src=x onerror=alert(1)>\";\n```";
    // Act
    const html = renderMarkdown(src);
    // Assert — the markup never appears unescaped.
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img");
  });

  it("keeps markdown syntax literal inside fences", () => {
    // Arrange
    const src = "```\n**not bold**\n```";
    // Act + Assert
    expect(renderMarkdown(src)).toContain("**not bold**");
  });

  it("renders an unterminated fence as a still-open highlighted block (streaming)", () => {
    // Arrange — `py` resolves through hljs's alias table to python.
    const src = "```py\nprint(1)";
    // Act
    const html = renderMarkdown(src);
    // Assert
    expect(html).toContain(`<code class="hljs lang-py">`);
    expect(html).toContain(`<span class="hljs-built_in">print</span>`);
  });
});

describe("renderMarkdown inline", () => {
  it("renders bold and italic without conflating them", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("**b** and *i*")).toBe("<p><strong>b</strong> and <em>i</em></p>\n");
  });

  it("renders inline code with emphasis suppressed inside", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("`*raw*`")).toBe("<p><code>*raw*</code></p>\n");
  });

  it("renders http links with rel=noopener", () => {
    // Arrange + Act
    const html = renderMarkdown("[site](https://example.com)");
    // Assert
    expect(html).toBe(
      `<p><a href="https://example.com" target="_blank" rel="noopener noreferrer">site</a></p>\n`,
    );
  });

  it("does not linkify non-http schemes", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("[x](javascript:alert(1))")).not.toContain("<a ");
  });
});

describe("renderMarkdown safety", () => {
  it("escapes HTML in paragraphs", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("<script>x</script>")).toBe(
      "<p>&lt;script&gt;x&lt;/script&gt;</p>\n",
    );
  });

  it("escapes HTML inside code fences", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("```\n<img src=x>\n```")).toContain("&lt;img src=x&gt;");
  });

  it("escapes HTML in headings and list items", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("# <b>h</b>\n- <i>x</i>")).not.toMatch(/<[bi]>/);
  });
});

// `inline` is the escaped-text inline pass the metaprompt-tree renderer and
// question picker inject; the block pipeline (renderMarkdown) no longer routes
// through it, so it carries its own coverage here.
describe("inline", () => {
  it("suppresses emphasis inside a code span", () => {
    // Arrange + Act + Assert
    expect(inline("`*raw*`")).toBe("<code>*raw*</code>");
  });

  it("renders bold before italic", () => {
    // Arrange + Act + Assert
    expect(inline("**b** and *i*")).toBe("<strong>b</strong> and <em>i</em>");
  });

  it("renders an http link with rel=noopener", () => {
    // Arrange + Act + Assert
    expect(inline("[site](https://example.com)")).toBe(
      `<a href="https://example.com" target="_blank" rel="noopener noreferrer">site</a>`,
    );
  });

  it("does not linkify a non-http scheme", () => {
    // Arrange + Act + Assert
    expect(inline("[x](javascript:alert(1))")).not.toContain("<a ");
  });
});

describe("setInlineMarkdown", () => {
  it("draws inline code as code", () => {
    // Arrange
    const el = document.createElement("span");
    // Act
    setInlineMarkdown(el, "run `make test` now");
    // Assert
    expect(el.querySelector("code")?.textContent).toBe("make test");
  });

  it("escapes raw HTML rather than injecting it", () => {
    // Arrange
    const el = document.createElement("span");
    // Act
    setInlineMarkdown(el, "<img src=x onerror=alert(1)>");
    // Assert
    expect([el.querySelector("img"), el.textContent]).toEqual([null, "<img src=x onerror=alert(1)>"]);
  });

  it("replaces what the element held before", () => {
    // Arrange
    const el = document.createElement("span");
    el.textContent = "old";
    // Act
    setInlineMarkdown(el, "**new**");
    // Assert
    expect(el.innerHTML).toBe("<strong>new</strong>");
  });
});

// THE CALL SITES SHARE THE ONE DRAWER: a source scan, so a site that hand-rolls
// `innerHTML = inline(escapeHtml(...))` again fails here rather than drifting.
describe("setInlineMarkdown's call sites", () => {
  const here = path.dirname(fileURLToPath(import.meta.url));
  it.each(["src/news-digest/news-digest.ts", "src/feed/asks/question.ts", "src/feed/tabs.ts"])(
    "%s draws inline markdown through setInlineMarkdown",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Act / Assert
      expect([source.includes("setInlineMarkdown("), /innerHTML\s*=\s*inline\(/.test(source)]).toEqual([true, false]);
    },
  );
});

describe("renderMarkdown: fences", () => {
  // A fenced tree never reaches renderMarkdown: the daemon lifts it out of its
  // fence into a tree block of its own (daemon/internal/prose), so every fence
  // markdown sees is code.
  it("draws a fence shaped like a tree as a code block", () => {
    // Act
    const html = renderMarkdown(["```", "1 🔧 Fixed the thing", "├── 1.1 Detail one", "```"].join("\n"));
    // Assert
    expect([html.includes("md-code"), html.includes("mp-tree")]).toEqual([true, false]);
  });

  it("draws a plain fence as a code block", () => {
    // Act
    const html = renderMarkdown("```\nplain code\nmore code\n```");
    // Assert
    expect([html.includes("md-code"), html.includes("mp-tree")]).toEqual([true, false]);
  });
});

describe("renderMarkdown: file links", () => {
  it("anchors a bare file name", () => {
    expect(renderMarkdown("[a](AGENTS.md)")).toContain('<a href="AGENTS.md"');
  });

  it("anchors a relative path with a line suffix", () => {
    expect(renderMarkdown("[a](lisp/status.el:42)")).toContain('<a href="lisp/status.el:42"');
  });

  it("anchors an absolute path", () => {
    expect(renderMarkdown("[a](/Users/u/w/a.go)")).toContain('<a href="/Users/u/w/a.go"');
  });

  it("still refuses a mailto link", () => {
    expect(renderMarkdown("[a](mailto:x@y.test)")).not.toContain("<a ");
  });

  it("still refuses a fragment-only link", () => {
    expect(renderMarkdown("[a](#top)")).not.toContain("<a ");
  });

  it("draws a relative image as its alt text, never an img", () => {
    expect(renderMarkdown("![alt text](pic.png)")).not.toContain("<img");
  });

  it("keeps a web image", () => {
    expect(renderMarkdown("![alt](https://example.com/p.png)")).toContain("<img");
  });
});

describe("inline: file links", () => {
  it("anchors a bare file name", () => {
    expect(inline("[a](AGENTS.md)")).toContain('<a href="AGENTS.md"');
  });

  it("leaves a javascript link as text", () => {
    expect(inline("[x](javascript:alert(1))")).not.toContain("<a ");
  });
});

describe("renderMarkdown: bare file names", () => {
  const hrefOf = (md: string): string | null => /<a href="([^"]*)"/.exec(renderMarkdown(md))?.[1] ?? null;

  it.each(["README.md", "foo.ts", "app.tsx", "a.js", "a.mjs", "main.go", "status.el", "x.py", "run.sh", "a.json", "a.yaml", "a.yml", "a.toml", "a.proto", "a.txt", "a.css", "a.html", "lib.rs", "a.c", "a.h", "a.m", "a.swift", "notes.org"])(
    "links %s as a file, href the bare name",
    (name) => {
      expect(hrefOf(`see ${name} now`)).toBe(name);
    },
  );

  it("keeps a real domain a web link", () => {
    expect(hrefOf("see example.com now")).toBe("http://example.com");
  });

  it("links wikipedia.org as a file candidate, href the bare name", () => {
    expect(hrefOf("see wikipedia.org now")).toBe("wikipedia.org");
  });

  const fallbackOf = (md: string): boolean => /data-web-fallback/.test(renderMarkdown(md));

  it.each(["wikipedia.org", "README.md", "run.sh", "x.py", "lib.rs"])(
    "marks %s, whose extension is also a domain ending, for web fallback",
    (name) => {
      expect(fallbackOf(`see ${name} now`)).toBe(true);
    },
  );

  it.each(["foo.ts", "main.go", "status.el", "a.json", "a.proto"])(
    "does not mark %s, whose extension is no domain ending",
    (name) => {
      expect(fallbackOf(`see ${name} now`)).toBe(false);
    },
  );

  it("does not mark an explicit [text](README.md) link", () => {
    expect(fallbackOf("[x](README.md)")).toBe(false);
  });

  it("does not mark a real domain", () => {
    expect(fallbackOf("see example.com now")).toBe(false);
  });

  it("keeps github.io a web link", () => {
    expect(hrefOf("see foo.github.io now")).toBe("http://foo.github.io");
  });

  it("leaves an explicit https URL ending in a file extension a web link", () => {
    expect(hrefOf("see https://example.com/a/README.md now")).toBe("https://example.com/a/README.md");
  });

  it("leaves an explicit http URL alone", () => {
    expect(hrefOf("see http://example.com now")).toBe("http://example.com");
  });

  it("keeps an email address a mailto-free plain text", () => {
    expect(renderMarkdown("mail a@b.md now")).not.toContain('href="a@b.md"');
  });

  it("does not linkify a name with an unlisted extension", () => {
    expect(renderMarkdown("see foo.xyzzy now")).not.toContain("<a ");
  });

  it("shows the file name as the anchor text", () => {
    expect(renderMarkdown("see README.md now")).toContain(">README.md</a>");
  });

  it("leaves a name inside a code span alone", () => {
    expect(renderMarkdown("see `README.md` now")).not.toContain("<a ");
  });
});

describe("code spans that are home-relative paths", () => {
  const anchors = (html: string): string[] => [...html.matchAll(/<a href="([^"]*)"/g)].map((m) => m[1]);

  it("renderMarkdown wraps a ~/ path code span in a file link drawing the file name as code", () => {
    // Act
    const html = renderMarkdown("see `~/a/b.ts` now");
    // Assert
    expect(html).toContain(
      '<a href="~/a/b.ts" title="~/a/b.ts" target="_blank" rel="noopener noreferrer"><code>b.ts</code></a>',
    );
  });

  it("renderMarkdown keeps a line suffix in the href", () => {
    expect(anchors(renderMarkdown("at `~/a/b.go:193`"))).toEqual(["~/a/b.go:193"]);
  });

  it("renderMarkdown leaves a relative path code span plain", () => {
    expect(renderMarkdown("see `lisp/status.el` now")).not.toContain("<a ");
  });

  it("renderMarkdown leaves a ~/ code span with more words plain", () => {
    expect(renderMarkdown("run `~/bin/x --flag`")).not.toContain("<a ");
  });

  it("renderMarkdown does not nest a ~/ code span inside an explicit link", () => {
    expect(anchors(renderMarkdown("[`~/a.ts`](~/b.ts)"))).toEqual(["~/b.ts"]);
  });

  it("renderMarkdown links a ~/ code span inside a list item", () => {
    expect(anchors(renderMarkdown("- one `~/a.el:4`"))).toEqual(["~/a.el:4"]);
  });

  it("inline wraps a ~/ path code span in a file link drawing the file name as code", () => {
    expect(inline("see `~/a/b.ts` now")).toContain(
      '<a href="~/a/b.ts" title="~/a/b.ts" target="_blank" rel="noopener noreferrer"><code>b.ts</code></a>',
    );
  });

  it("inline leaves a non-path code span plain", () => {
    expect(inline("run `agent-repl-send`")).toBe("run <code>agent-repl-send</code>");
  });

  it("inline does not nest a ~/ code span inside an explicit link", () => {
    expect(anchors(inline("[`~/a.ts`](~/b.ts)"))).toEqual(["~/b.ts"]);
  });

  it("both renderers draw the same anchor for the same code span", () => {
    // Arrange
    const src = "see `~/a/b.ts:9` now";
    const anchor = (html: string): string | undefined => /<a [^>]*><code>[^<]*<\/code><\/a>/.exec(html)?.[0];
    // Act + Assert
    expect(anchor(inline(src))).toBe(anchor(renderMarkdown(src)));
  });
});

describe("a file link draws its file name, not its path", () => {
  const drawn = (html: string): string[] =>
    [...html.matchAll(/<a [^>]*>(.*?)<\/a>/g)].map((m) => m[1].replace(/<[^>]*>/g, ""));
  const titles = (html: string): (string | null)[] =>
    [...html.matchAll(/<a ([^>]*)>/g)].map((m) => /title="([^"]*)"/.exec(m[1])?.[1] ?? null);
  const renderers = [
    { renderer: "renderMarkdown", render: (src: string) => renderMarkdown(src) },
    { renderer: "inline", render: (src: string) => inline(src) },
  ];
  const cases = [
    { name: "a ~/ code span with a line", src: "at `~/a/b.go:193`", text: "b.go:193", title: "~/a/b.go:193" },
    { name: "a ~/ code span directory", src: "in `~/a/b/`", text: "b/", title: "~/a/b/" },
    { name: "a path as explicit link text", src: "[~/a/b.ts:4](~/a/b.ts:4)", text: "b.ts:4", title: "~/a/b.ts:4" },
    { name: "a code span path as explicit link text", src: "[`~/a/b.ts`](/Users/u/a/b.ts)", text: "b.ts", title: "~/a/b.ts" },
    { name: "a relative path as explicit link text", src: "[lisp/status.el](lisp/status.el)", text: "status.el", title: "lisp/status.el" },
    { name: "prose link text", src: "[the config](~/a/b.ts)", text: "the config", title: null },
    { name: "a bare file name as link text", src: "[b.ts](~/a/b.ts)", text: "b.ts", title: null },
    { name: "a web link whose text is a path", src: "[a/b.ts](https://x.test/a/b.ts)", text: "a/b.ts", title: null },
  ];
  for (const { renderer, render } of renderers) {
    it.each(cases)(`${renderer} draws $text for $name`, ({ src, text }) => {
      expect(drawn(render(src))).toEqual([text]);
    });
    it.each(cases)(`${renderer} titles $name with $title`, ({ src, title }) => {
      expect(titles(render(src))).toEqual([title]);
    });
  }

  it("keeps the full path as the href", () => {
    expect(renderMarkdown("[~/a/b.ts:4](~/a/b.ts:4)")).toContain('href="~/a/b.ts:4"');
  });
});

describe("renderMarkdown TeX", () => {
  /** The rendered HTML of SRC parsed, for querying. */
  const drawn = (src: string): HTMLElement => {
    const el = document.createElement("div");
    el.innerHTML = renderMarkdown(src);
    return el;
  };

  it.each([
    ["dollars", "the area $\\pi r^2$ grows"],
    ["parentheses", "the area \\(\\pi r^2\\) grows"],
  ])("typesets inline TeX between %s, in its line", (_name, src) => {
    // Act
    const el = drawn(src);

    // Assert
    const eq = el.querySelector("p > eq");
    expect(eq?.querySelector(".katex")).not.toBeNull();
    expect(el.textContent).toContain("grows");
    expect(el.textContent).not.toContain("$");
  });

  it.each([
    ["double dollars", "$$\\int_0^1 x\\,dx$$"],
    ["brackets", "\\[\\int_0^1 x\\,dx\\]"],
    ["a math fence", "```math\n\\int_0^1 x\\,dx\n```"],
  ])("typesets display TeX between %s as its own block", (_name, src) => {
    // Act
    const el = drawn(src);

    // Assert
    expect(el.querySelector("section > eqn .katex-display")).not.toBeNull();
    expect(el.querySelector("pre")).toBeNull();
  });

  it("leaves money as text", () => {
    // Act
    const el = drawn("it costs $5 and $10");

    // Assert
    expect(el.querySelector(".katex")).toBeNull();
    expect(el.textContent?.trim()).toBe("it costs $5 and $10");
  });

  it("leaves TeX in a code span as code", () => {
    // Act
    const el = drawn("write `$x$` for x");

    // Assert
    expect(el.querySelector(".katex")).toBeNull();
    expect(el.querySelector("code")?.textContent).toBe("$x$");
  });

  it("draws a formula KaTeX cannot parse as its marked source, and the rest of the text", () => {
    // Act
    const el = drawn("bad $\\frac{1}{$ then more");

    // Assert
    expect(el.querySelector(".katex-error")?.textContent).toContain("\\frac{1}{");
    expect(el.textContent).toContain("then more");
  });

  it("keeps a TeX-shaped attack inert: KaTeX trusts no HTML", () => {
    // Act
    const el = drawn("$\\href{javascript:alert(1)}{x}$");

    // Assert
    expect(el.querySelector('a[href^="javascript"]')).toBeNull();
  });
});

describe("renderMarkdown: a list item's lead", () => {
  it.each([
    ["a tight list", "- **Term**: words"],
    ["a loose list", "- **Term**: words\n\n- other"],
  ])("marks bold words that open the item in %s", (_name, src) => {
    expect(renderMarkdown(src)).toContain(`<strong class="${LEAD_CLASS}">Term</strong>`);
  });

  it("leaves bold words later in the item unmarked", () => {
    expect(renderMarkdown("- a **Term** here")).toContain("<strong>Term</strong>");
  });

  it("leaves bold words outside a list unmarked", () => {
    expect(renderMarkdown("**Term**: words")).toContain("<strong>Term</strong>");
  });
});
