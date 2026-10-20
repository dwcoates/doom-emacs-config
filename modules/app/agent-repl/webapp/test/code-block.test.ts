/** The one code block, and the proof no other module draws its shape itself. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeBlockHtml } from "../src/code-block.js";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

describe("codeBlockHtml", () => {
  it("escapes a language-less body inside pre.md-code", () => {
    expect(codeBlockHtml("<b>x</b>", "")).toBe(`<pre class="md-code"><code class="hljs">&lt;b&gt;x&lt;/b&gt;</code></pre>`);
  });

  it("names the language on the code element", () => {
    expect(codeBlockHtml("x", "klingon")).toBe(`<pre class="md-code"><code class="hljs lang-klingon">x</code></pre>`);
  });

  it("is the only place src writes the code block's shape", () => {
    // Arrange
    const files = [...codeUnder(SRC)];
    // Act
    const handRolled = files
      .filter(([file]) => file !== "code-block.ts")
      .filter(([, code]) => /class="md-code"/.test(code))
      .map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });
});
