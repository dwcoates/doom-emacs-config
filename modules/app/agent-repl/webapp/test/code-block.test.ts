// @vitest-environment jsdom
/** The one code block, its copy control, and the proof no other module draws its shape itself. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  CODE_COPIED_ATTRIBUTE,
  CODE_COPIED_MS,
  CODE_COPY_CLASS,
  codeBlockHtml,
  codeOfCopyControl,
  installCodeCopy,
} from "../src/code-block.js";
import { CLICK_THROUGH_SELECTOR } from "../src/expand.js";
import { bubbleClick, SELECTABLE_ATTRIBUTE } from "../src/feed/bubble-selection.js";
import { renderMarkdown } from "../src/markdown.js";
import { codeUnder } from "./files-under.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

let teardown: (() => void) | undefined;

afterEach(() => {
  teardown?.();
  teardown = undefined;
  document.body.innerHTML = "";
  vi.useRealTimers();
});

/** Draw BODY as a code block in the document, and answer its copy control. */
function drawn(body: string, lang = ""): HTMLElement {
  document.body.innerHTML = codeBlockHtml(body, lang);
  const control = document.querySelector<HTMLElement>(`.${CODE_COPY_CLASS}`);
  if (control === null) throw new Error("the code block drew no copy control");
  return control;
}

/** Install the copy click with a WRITE that records what it was handed and answers OUTCOME. */
function installRecording(outcome: boolean): string[] {
  const written: string[] = [];
  teardown = installCodeCopy(document, (text) => {
    written.push(text);
    return outcome;
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

  it("draws the copy control as the block's first child, ahead of the code", () => {
    // Arrange
    document.body.innerHTML = codeBlockHtml("x", "");
    // Act
    const children = [...(document.querySelector("pre.md-code")?.children ?? [])];
    // Assert
    expect(children.map((child) => `${child.localName}.${child.classList[0]}`)).toEqual([
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
      .filter(([, code]) => /class="md-code"|md-code-copy/.test(code))
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
    expect(codeOfCopyControl(control)).toEqual({ text: "line one\nline two", lang: "" });
  });

  it("answers the highlighted code's text, not its markup, and its language", () => {
    // Arrange
    const control = drawn("const a = 1;", "typescript");
    // Act / Assert
    expect(codeOfCopyControl(control)).toEqual({ text: "const a = 1;", lang: "typescript" });
  });

  it("throws for a control outside a code block", () => {
    // Arrange
    const stray = document.createElement("ar-button");
    document.body.append(stray);
    // Act / Assert
    expect(() => codeOfCopyControl(stray)).toThrow("outside a code block");
  });
});

describe("installCodeCopy", () => {
  it("copies the block's code on a click of its copy control", () => {
    // Arrange
    const written = installRecording(true);
    const control = drawn("npm test");
    // Act
    control.click();
    // Assert
    expect(written).toEqual(["npm test"]);
  });

  it("copies the block's code on a click of the control's icon", () => {
    // Arrange
    const written = installRecording(true);
    drawn("npm test");
    // Act
    document.querySelector<SVGElement>(`.${CODE_COPY_CLASS} svg`)?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(written).toEqual(["npm test"]);
  });

  it("copies the clicked block's code, not another block's", () => {
    // Arrange
    const written = installRecording(true);
    document.body.innerHTML = codeBlockHtml("first", "") + codeBlockHtml("second", "");
    // Act
    document.querySelectorAll<HTMLElement>(`.${CODE_COPY_CLASS}`)[1]?.click();
    // Assert
    expect(written).toEqual(["second"]);
  });

  it("copies nothing on a click of the code itself", () => {
    // Arrange
    const written = installRecording(true);
    drawn("npm test");
    // Act
    document.querySelector<HTMLElement>("pre.md-code > code")?.click();
    // Assert
    expect(written).toEqual([]);
  });

  it("marks the control copied once it copied", () => {
    // Arrange
    installRecording(true);
    const control = drawn("x");
    // Act
    control.click();
    // Assert
    expect(control.hasAttribute(CODE_COPIED_ATTRIBUTE)).toBe(true);
  });

  it("clears the copied mark after its moment", () => {
    // Arrange
    vi.useFakeTimers();
    installRecording(true);
    const control = drawn("x");
    control.click();
    // Act
    vi.advanceTimersByTime(CODE_COPIED_MS);
    // Assert
    expect(control.hasAttribute(CODE_COPIED_ATTRIBUTE)).toBe(false);
  });

  it("restarts the copied moment on a second copy", () => {
    // Arrange
    vi.useFakeTimers();
    installRecording(true);
    const control = drawn("x");
    control.click();
    vi.advanceTimersByTime(CODE_COPIED_MS - 1);
    // Act
    control.click();
    vi.advanceTimersByTime(CODE_COPIED_MS - 1);
    // Assert
    expect(control.hasAttribute(CODE_COPIED_ATTRIBUTE)).toBe(true);
  });

  it("records a copy at info, with the code's length and language", async () => {
    // Arrange
    const capture = captureLogRecords();
    installRecording(true);
    const control = drawn("abc", "go");
    // Act
    control.click();
    // Assert
    const record = await forwardedRecord(capture, "code-block.copy");
    expect([record.level.case, record.message, record.context]).toMatchObject([
      "info",
      "a code block's copy control copied its code",
      { characters: 3, lang: "go", outcome: "copied" },
    ]);
  });

  it("records a refused copy at error, and leaves the control unmarked", async () => {
    // Arrange
    const capture = captureLogRecords();
    installRecording(false);
    const control = drawn("abc");
    // Act
    control.click();
    // Assert
    const record = await forwardedRecord(capture, "code-block.copy");
    expect([record.level.case, record.message, record.context, control.hasAttribute(CODE_COPIED_ATTRIBUTE)]).toMatchObject([
      "error",
      "a code block's copy control could not write its code to the clipboard",
      { characters: 3, outcome: "refused" },
      false,
    ]);
  });

  it("copies nothing once uninstalled", () => {
    // Arrange
    const written = installRecording(true);
    const control = drawn("x");
    teardown?.();
    teardown = undefined;
    // Act
    control.click();
    // Assert
    expect(written).toEqual([]);
  });
});

describe("a click on the copy control and the bubble it sits in", () => {
  it("is a click every click owner leaves to the control", () => {
    // Arrange
    const control = drawn("x");
    // Act / Assert
    expect(control.closest(CLICK_THROUGH_SELECTOR)).toBe(control);
  });

  it("never selects the bubble", () => {
    // Arrange
    const row = document.createElement("div");
    row.setAttribute("data-feed-row", "r1");
    row.setAttribute(SELECTABLE_ATTRIBUTE, "");
    row.innerHTML = codeBlockHtml("x", "");
    document.body.append(row);
    const control = row.querySelector(`.${CODE_COPY_CLASS}`);
    if (control === null) throw new Error("the code block drew no copy control");
    // Act
    const click = bubbleClick({ row, target: control, selectedText: "" });
    // Assert
    expect(click).toEqual({ case: "ignore", reason: "interactive" });
  });
});
