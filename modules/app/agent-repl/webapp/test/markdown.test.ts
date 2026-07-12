import { describe, expect, it } from "vitest";
import { renderMarkdown } from "../src/markdown.js";

describe("renderMarkdown blocks", () => {
  it("renders headings at their level", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("## Title")).toBe("<h2>Title</h2>");
  });

  it("renders paragraphs with soft line breaks", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("one\ntwo")).toBe("<p>one<br>two</p>");
  });

  it("separates paragraphs on blank lines", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("a\n\nb")).toBe("<p>a</p><p>b</p>");
  });

  it("syntax-highlights fenced code with a known language tag", () => {
    // Arrange
    const src = "```go\nfunc main() {}\n```";
    // Act
    const html = renderMarkdown(src);
    // Assert — hljs classes present and the language class preserved.
    expect(html).toContain(`<code class="hljs lang-go">`);
    expect(html).toContain(`<span class="hljs-keyword">func</span>`);
  });

  it("falls back to plain escaped text for an unknown language tag", () => {
    // Arrange
    const src = "```klingon\n<qapla> & stuff\n```";
    // Act + Assert — escaped, no hljs token spans.
    expect(renderMarkdown(src)).toBe(
      `<pre class="md-code"><code class="hljs lang-klingon">&lt;qapla&gt; &amp; stuff</code></pre>`,
    );
  });

  it("renders a language-less fence as plain escaped text", () => {
    // Arrange
    const src = "```\n<b>raw</b>\n```";
    // Act + Assert
    expect(renderMarkdown(src)).toBe(
      `<pre class="md-code"><code class="hljs">&lt;b&gt;raw&lt;/b&gt;</code></pre>`,
    );
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

  it("renders unordered lists", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- a\n- b")).toBe("<ul><li>a</li><li>b</li></ul>");
  });

  it("renders ordered lists", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("1. a\n2. b")).toBe("<ol><li>a</li><li>b</li></ol>");
  });

  it("splits adjacent lists of different kinds", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("- a\n1. b")).toBe("<ul><li>a</li></ul><ol><li>b</li></ol>");
  });

  it("renders blockquotes", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("> wisdom")).toBe("<blockquote>wisdom</blockquote>");
  });

  it("renders horizontal rules", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("---")).toBe("<hr>");
  });
});

describe("renderMarkdown inline", () => {
  it("renders bold and italic without conflating them", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("**b** and *i*")).toBe("<p><strong>b</strong> and <em>i</em></p>");
  });

  it("renders inline code with emphasis suppressed inside", () => {
    // Arrange + Act + Assert
    expect(renderMarkdown("`*raw*`")).toBe("<p><code>*raw*</code></p>");
  });

  it("renders http links with rel=noopener", () => {
    // Arrange + Act
    const html = renderMarkdown("[site](https://example.com)");
    // Assert
    expect(html).toBe(
      `<p><a href="https://example.com" target="_blank" rel="noopener noreferrer">site</a></p>`,
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
      "<p>&lt;script&gt;x&lt;/script&gt;</p>",
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

describe("metaprompt trees in fences", () => {
  it("renders a plain fence carrying a tree as hanging-indent lines", () => {
    // Arrange
    const text = [
      "```",
      "1 🔧 Fixed the thing",
      "├── 1.1 Detail one",
      "└── 1.2 Detail two",
      "```",
    ].join("\n");
    // Act
    const html = renderMarkdown(text);
    // Assert
    expect(html).toContain(`class="mp-tree"`);
    expect(html).toContain(`<span class="mp-prefix">├── 1.1 <i class="mp-rail"`);
    expect(html).not.toContain("md-code");
  });

  it("keeps a language-tagged fence on the code path even if tree-shaped", () => {
    // Arrange — an explicit language wins over the tree heuristic.
    const text = "```text\n1 🔧 A\n├── 1.1 B\n```";
    // Act
    const html = renderMarkdown(text);
    // Assert
    expect(html).toContain("md-code");
    expect(html).not.toContain("mp-tree");
  });

  it("keeps a plain non-tree fence on the code path", () => {
    // Act
    const html = renderMarkdown("```\nplain code\nmore code\n```");
    // Assert
    expect(html).toContain("md-code");
    expect(html).not.toContain("mp-tree");
  });
});
