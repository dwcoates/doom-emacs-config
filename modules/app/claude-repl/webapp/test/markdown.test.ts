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

  it("renders fenced code with a language class", () => {
    // Arrange
    const src = "```go\nfunc main() {}\n```";
    // Act + Assert
    expect(renderMarkdown(src)).toBe(
      `<pre class="md-code"><code class="lang-go">func main() {}</code></pre>`,
    );
  });

  it("keeps markdown syntax literal inside fences", () => {
    // Arrange
    const src = "```\n**not bold**\n```";
    // Act + Assert
    expect(renderMarkdown(src)).toContain("**not bold**");
  });

  it("renders an unterminated fence as a still-open code block (streaming)", () => {
    // Arrange
    const src = "```py\nprint(1)";
    // Act + Assert
    expect(renderMarkdown(src)).toBe(
      `<pre class="md-code"><code class="lang-py">print(1)</code></pre>`,
    );
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
