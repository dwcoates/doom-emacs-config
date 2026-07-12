import { describe, expect, it } from "vitest";
import { escapeHtml, highlightCode, languageForPath } from "../src/highlight.js";

describe("escapeHtml", () => {
  it("escapes markup-significant characters", () => {
    // Arrange + Act + Assert
    expect(escapeHtml(`<b a="x">&`)).toBe("&lt;b a=&quot;x&quot;&gt;&amp;");
  });
});

describe("languageForPath", () => {
  it("maps a known extension to its hljs language", () => {
    // Arrange + Act + Assert
    expect(languageForPath("/home/u/.config/doom/config.el")).toBe("lisp");
  });

  it("matches extensions case-insensitively", () => {
    // Arrange + Act + Assert
    expect(languageForPath("/docs/README.MD")).toBe("markdown");
  });

  it("returns null for an unknown extension", () => {
    // Arrange + Act + Assert
    expect(languageForPath("/tmp/data.xyz")).toBeNull();
  });

  it("returns null for an extensionless file", () => {
    // Arrange + Act + Assert
    expect(languageForPath("/src/Makefile")).toBeNull();
  });

  it("returns null for a dotfile", () => {
    // Arrange + Act + Assert
    expect(languageForPath("/home/u/.zshrc")).toBeNull();
  });

  it("ignores dots in directory names", () => {
    // Arrange + Act + Assert
    expect(languageForPath("/home/u/.config.d/notes")).toBeNull();
  });
});

describe("highlightCode", () => {
  it("emits hljs token spans for a registered language", () => {
    // Arrange + Act + Assert
    expect(highlightCode("const x = 1;", "typescript")).toContain(
      `<span class="hljs-keyword">const</span>`,
    );
  });

  it("falls back to plain escaped text for an unknown language", () => {
    // Arrange + Act + Assert
    expect(highlightCode("<b> & stuff", "klingon")).toBe("&lt;b&gt; &amp; stuff");
  });

  it("falls back to plain escaped text when no language is given", () => {
    // Arrange + Act + Assert
    expect(highlightCode("<b>", "")).toBe("&lt;b&gt;");
  });
});
