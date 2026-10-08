// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  drawMarkdownOutput,
  isMarkdownOutput,
  looksLikeMarkdown,
} from "../../../src/feed/cards/markdown-output.js";

describe("looksLikeMarkdown", () => {
  it.each([
    ["an ATX heading", "# Summary\nall good"],
    ["a closed fenced code block", "see:\n```ts\nconst x = 1;\n```\n"],
    ["a GFM table", "| a | b |\n|---|---|\n| 1 | 2 |\n"],
    ["a list with inline code", "- run `make test`\n- then `make lint`\n"],
    ["a list with a link", "1. [docs](https://example.com)\n2. done\n"],
  ])("reads %s as markdown", (_name, text) => {
    // Arrange / Act
    const got = looksLikeMarkdown(text);

    // Assert
    expect(got).toBe(true);
  });

  it.each([
    ["plain prose", "The file was written."],
    ["a count", "42 matches"],
    ["a lone dash line", "- one item\n"],
    ["two list items with no inline markup", "- one\n- two\n"],
    ["a single fence line", "```\nnever closed"],
    ["a hash with no space", "#include <stdio.h>"],
    ["snake_case paths", "src/feed_view/tool_call.ts"],
    ["a pipe line with no delimiter row", "a | b | c"],
  ])("reads %s as plain text", (_name, text) => {
    // Arrange / Act
    const got = looksLikeMarkdown(text);

    // Assert
    expect(got).toBe(false);
  });
});

describe("isMarkdownOutput", () => {
  it.each([
    { name: "a markdown text from a call that is not a shell", shell: false, failed: false, want: true },
    { name: "a markdown text a shell printed", shell: true, failed: false, want: false },
    { name: "a markdown text of a failed call", shell: false, failed: true, want: false },
  ])("answers $want for $name", ({ shell, failed, want }) => {
    // Arrange / Act
    const got = isMarkdownOutput("# Title\nbody", shell, failed);

    // Assert
    expect(got).toBe(want);
  });

  it("answers false for plain text from a call that is not a shell", () => {
    // Arrange / Act
    const got = isMarkdownOutput("done", false, false);

    // Assert
    expect(got).toBe(false);
  });
});

describe("drawMarkdownOutput", () => {
  it("renders the markdown formatted", () => {
    // Arrange / Act
    const el = drawMarkdownOutput("# Title\n\n**bold**", "p");

    // Assert
    expect([el.querySelector("h1")?.textContent, el.querySelector("strong")?.textContent]).toEqual(["Title", "bold"]);
  });

  it("wears the markdown output section's classes", () => {
    // Arrange / Act
    const el = drawMarkdownOutput("# Title", "p");

    // Assert
    expect(el.className).toBe("tool-output tool-output-md");
  });

  it("never passes raw HTML through", () => {
    // Arrange / Act
    const el = drawMarkdownOutput('# T\n\n<script>alert(1)</script><img src=x onerror="alert(1)">', "p");

    // Assert
    expect([el.querySelector("script"), el.querySelector("img")]).toEqual([null, null]);
  });

  it("draws a javascript: link as text, never as an anchor", () => {
    // Arrange / Act
    const el = drawMarkdownOutput("# T\n\n[click](javascript:alert(1))", "p");

    // Assert
    expect(el.querySelector("a")).toBeNull();
  });
});
