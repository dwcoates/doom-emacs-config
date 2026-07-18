import { describe, expect, it } from "vitest";
import { renderPromptBody, splitPromptSegments } from "../src/prompt-body.js";

describe("splitPromptSegments", () => {
  it("returns a single text segment for a fence-free prompt", () => {
    // Arrange
    const prompt = "just some words\nover two lines";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([{ kind: "text", text: "just some words\nover two lines" }]);
  });

  it("lifts a language-tagged fence out as a code segment", () => {
    // Arrange
    const prompt = "```python\ndef foo():\n    pass\n```";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([{ kind: "code", lang: "python", code: "def foo():\n    pass" }]);
  });

  it("keeps the text on either side of a fenced block", () => {
    // Arrange
    const prompt = "before\n```js\nx\n```\nafter";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([
      { kind: "text", text: "before" },
      { kind: "code", lang: "js", code: "x" },
      { kind: "text", text: "after" },
    ]);
  });

  it("splits multiple fenced blocks independently", () => {
    // Arrange
    const prompt = "```py\na\n```\nmid\n```go\nb\n```";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([
      { kind: "code", lang: "py", code: "a" },
      { kind: "text", text: "mid" },
      { kind: "code", lang: "go", code: "b" },
    ]);
  });

  it("treats a language-less fence as literal text", () => {
    // Arrange — no language token, so per the spec it is not a code block.
    const prompt = "```\nplain\n```";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([{ kind: "text", text: "```\nplain\n```" }]);
  });

  it("treats an unterminated opening fence as literal text", () => {
    // Arrange — no closing fence, so the block never forms.
    const prompt = "```python\nnever closed";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([{ kind: "text", text: "```python\nnever closed" }]);
  });

  it("preserves an empty body inside a fence", () => {
    // Arrange
    const prompt = "```python\n```";
    // Act
    const segs = splitPromptSegments(prompt);
    // Assert
    expect(segs).toEqual([{ kind: "code", lang: "python", code: "" }]);
  });

  it("returns one empty text segment for an empty prompt", () => {
    // Arrange / Act / Assert
    expect(splitPromptSegments("")).toEqual([{ kind: "text", text: "" }]);
  });
});

describe("renderPromptBody", () => {
  it("keeps the plain single-<pre> shape for a fence-free prompt", () => {
    // Arrange / Act / Assert — byte-for-byte the bubble's historical shape.
    expect(renderPromptBody("do the thing")).toBe("<pre>do the thing</pre>");
  });

  it("escapes html-special characters in plain prompt text", () => {
    // Arrange / Act / Assert
    expect(renderPromptBody("a < b && c")).toBe("<pre>a &lt; b &amp;&amp; c</pre>");
  });

  it("wraps a fenced block in a highlighted md-code card", () => {
    // Arrange
    const prompt = "```python\ndef foo():\n    pass\n```";
    // Act
    const html = renderPromptBody(prompt);
    // Assert — the same card shape the agent's markdown fences emit.
    expect(html).toContain(`<pre class="md-code"><code class="hljs lang-python">`);
    expect(html).toContain("foo");
  });

  it("syntax-highlights a known language's keywords", () => {
    // Arrange
    const prompt = "```python\ndef foo():\n    pass\n```";
    // Act
    const html = renderPromptBody(prompt);
    // Assert — hljs marks python's `def` as a keyword.
    expect(html).toContain(`<span class="hljs-keyword">def</span>`);
  });

  it("falls back to plain escaped code for an unknown language", () => {
    // Arrange — `made-up` is not a registered hljs language.
    const prompt = "```made-up\n1 < 2 & 3\n```";
    // Act
    const html = renderPromptBody(prompt);
    // Assert — still a code card, but the body is only escaped, not spanned.
    expect(html).toContain(`<pre class="md-code"><code class="hljs lang-made-up">1 &lt; 2 &amp; 3</code></pre>`);
  });

  it("escapes html-special characters inside a code block", () => {
    // Arrange — unknown language so the output is the raw escaped body.
    const prompt = "```text\n<script>\n```";
    // Act
    const html = renderPromptBody(prompt);
    // Assert
    expect(html).toContain("&lt;script&gt;");
    expect(html).not.toContain("<script>");
  });

  it("renders the text around a fenced block as its own <pre>", () => {
    // Arrange
    const prompt = "look:\n```json\n{}\n```\nthanks";
    // Act
    const html = renderPromptBody(prompt);
    // Assert
    expect(html).toContain("<pre>look:</pre>");
    expect(html).toContain(`<pre class="md-code"><code class="hljs lang-json">`);
    expect(html).toContain("<pre>thanks</pre>");
  });
});
