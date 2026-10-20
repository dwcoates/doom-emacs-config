// @vitest-environment jsdom
/** The one code block, its copy control, and the proof no other module draws its shape itself. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import { CODE_COPY_CLASS, codeBlockHtml, codeOfCopyControl } from "../src/code-block.js";
import { boundCopyText, COPY_CONTROL_CLASS, COPY_KIND_ATTRIBUTE, installCopyControls } from "../src/copy-control.js";
import { renderMarkdown } from "../src/markdown.js";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

let teardown: (() => void) | undefined;

afterEach(() => {
  teardown?.();
  teardown = undefined;
  document.body.innerHTML = "";
});

/** Draw BODY as a code block in the document, and answer its copy control. */
function drawn(body: string, lang = ""): HTMLElement {
  document.body.innerHTML = codeBlockHtml(body, lang);
  const control = document.querySelector<HTMLElement>(`.${CODE_COPY_CLASS}`);
  if (control === null) throw new Error("the code block drew no copy control");
  return control;
}

/** Install the copy controls with the code resolver, recording what each copy wrote. */
function installRecording(): string[] {
  const written: string[] = [];
  teardown = installCopyControls(document, { code: codeOfCopyControl, prompt: boundCopyText }, (text) => {
    written.push(text);
    return true;
  });
  return written;
}

describe("codeBlockHtml", () => {
  it("escapes a language-less body inside pre.md-code", () => {
    // Arrange
    document.body.innerHTML = codeBlockHtml("<b>x</b>", "");
    // Act
    const code = document.querySelector("pre.md-code > code.hljs");
    // Assert
    expect([code?.className, code?.innerHTML]).toEqual(["hljs", "&lt;b&gt;x&lt;/b&gt;"]);
  });

  it("names the language on the code element", () => {
    // Arrange
    document.body.innerHTML = codeBlockHtml("x", "klingon");
    // Act / Assert
    expect(document.querySelector("pre.md-code > code")?.className).toBe("hljs lang-klingon");
  });

  it("draws a code copy control, the one copy control", () => {
    // Arrange
    const control = drawn("x");
    // Act / Assert
    expect([control.classList.contains(COPY_CONTROL_CLASS), control.getAttribute(COPY_KIND_ATTRIBUTE)]).toEqual([
      true,
      "code",
    ]);
  });

  it("draws the copy control as the block's first child, ahead of the code", () => {
    // Arrange
    document.body.innerHTML = codeBlockHtml("x", "");
    // Act
    const children = [...(document.querySelector("pre.md-code")?.children ?? [])];
    // Assert
    expect(children.map((child) => `${child.localName}.${child.classList[child.classList.length - 1]}`)).toEqual([
      `ar-button.${CODE_COPY_CLASS}`,
      "code.hljs",
    ]);
  });

  it("labels the copy control on hover and for assistive technology", () => {
    // Arrange
    const control = drawn("x");
    // Act / Assert
    expect([control.title, control.getAttribute("aria-label")]).toEqual(["Copy code", "Copy code"]);
  });

  it("adds no text to the block: its text is its code alone", () => {
    // Arrange
    drawn("const a = 1;", "typescript");
    // Act / Assert
    expect(document.querySelector("pre.md-code")?.textContent).toBe("const a = 1;");
  });

  it("is what a markdown fence draws, copy control and all", () => {
    // Act
    const html = renderMarkdown("```go\nfmt.Println()\n```");
    // Assert
    expect(html).toBe(codeBlockHtml("fmt.Println()", "go"));
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

describe("codeOfCopyControl", () => {
  it("answers the code of the block the control sits in", () => {
    // Arrange
    const control = drawn("line one\nline two");
    // Act / Assert
    expect(codeOfCopyControl(control)).toEqual({ text: "line one\nline two", context: { lang: "" } });
  });

  it("answers the highlighted code's text, not its markup, and its language", () => {
    // Arrange
    const control = drawn("const a = 1;", "typescript");
    // Act / Assert
    expect(codeOfCopyControl(control)).toEqual({ text: "const a = 1;", context: { lang: "typescript" } });
  });

  it("throws for a control outside a code block", () => {
    // Arrange
    const stray = document.createElement("ar-button");
    document.body.append(stray);
    // Act / Assert
    expect(() => codeOfCopyControl(stray)).toThrow("outside a code block");
  });
});

describe("a click on a code block's copy control", () => {
  it("copies the block's code", () => {
    // Arrange
    const written = installRecording();
    const control = drawn("npm test");
    // Act
    control.click();
    // Assert
    expect(written).toEqual(["npm test"]);
  });

  it("copies the clicked block's code, not another block's", () => {
    // Arrange
    const written = installRecording();
    document.body.innerHTML = codeBlockHtml("first", "") + codeBlockHtml("second", "");
    // Act
    document.querySelectorAll<HTMLElement>(`.${CODE_COPY_CLASS}`)[1]?.click();
    // Assert
    expect(written).toEqual(["second"]);
  });
});
