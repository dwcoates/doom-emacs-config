// @vitest-environment jsdom
/** The one copy control: its markup, its click, its copied moment, and its record. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  boundCopyControl,
  boundCopyText,
  COPIED_ATTRIBUTE,
  COPIED_MS,
  COPY_CONTROL_CLASS,
  COPY_KIND_ATTRIBUTE,
  copyControlHtml,
  installCopyControls,
} from "../src/copy-control.js";
import { CLICK_THROUGH_SELECTOR } from "../src/expand.js";
import { bubbleClick, SELECTABLE_ATTRIBUTE } from "../src/feed/bubble-selection.js";
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

/** Draw a `code` copy control whose resolver answers TEXT, in a box naming it. */
function drawn(text = "the text"): HTMLElement {
  document.body.innerHTML = `<div data-text="${text}">${copyControlHtml("code", "placed", "Copy it")}</div>`;
  const control = document.querySelector<HTMLElement>(`.${COPY_CONTROL_CLASS}`);
  if (control === null) throw new Error("no copy control drawn");
  return control;
}

/** Install the controls with a resolver reading the box's text and a WRITE answering OUTCOME. */
function installRecording(outcome: boolean): string[] {
  const written: string[] = [];
  teardown = installCopyControls(
    document,
    {
      code: (control) => ({ text: control.parentElement?.getAttribute("data-text") ?? "", context: { lang: "go" } }),
      prompt: boundCopyText,
      response: boundCopyText,
    },
    (text) => {
      written.push(text);
      return outcome;
    },
  );
  return written;
}

describe("copyControlHtml", () => {
  it("draws an ar-button wearing the shared class, the placing class and its kind", () => {
    // Arrange
    const control = drawn();
    // Act / Assert
    expect([control.localName, control.className, control.getAttribute(COPY_KIND_ATTRIBUTE)]).toEqual([
      "ar-button",
      `${COPY_CONTROL_CLASS} placed`,
      "code",
    ]);
  });

  it("labels the control on hover and for assistive technology", () => {
    // Arrange
    const control = drawn();
    // Act / Assert
    expect([control.title, control.getAttribute("aria-label")]).toEqual(["Copy it", "Copy it"]);
  });

  it("holds no text", () => {
    // Act / Assert
    expect(drawn().textContent).toBe("");
  });

  it("is the only place src draws a copy control", () => {
    // Arrange
    const files = [...codeUnder(SRC)];
    // Act
    const handRolled = files
      .filter(([file]) => file !== "copy-control.ts")
      .filter(([, code]) => /data-copies|copy-control-icon/.test(code))
      .map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });
});

describe("boundCopyControl", () => {
  it("is the one copy control, of the kind it was given", () => {
    // Act
    const control = boundCopyControl("prompt", "placed", "Copy it", "x");
    // Assert
    expect([control.localName, control.className, control.getAttribute(COPY_KIND_ATTRIBUTE), control.title]).toEqual([
      "ar-button",
      `${COPY_CONTROL_CLASS} placed`,
      "prompt",
      "Copy it",
    ]);
  });

  it("copies the text bound to it", () => {
    // Arrange
    const written = installRecording(true);
    const control = boundCopyControl("prompt", "placed", "Copy it", "the whole **prompt**");
    document.body.append(control);
    // Act
    control.click();
    // Assert
    expect(written).toEqual(["the whole **prompt**"]);
  });
});

describe("boundCopyText", () => {
  it("answers the text bound to the control, with no further context", () => {
    // Act / Assert
    expect(boundCopyText(boundCopyControl("prompt", "placed", "Copy it", "abc"))).toEqual({ text: "abc", context: {} });
  });

  it("throws for a control with no text bound to it", () => {
    // Act / Assert
    expect(() => boundCopyText(drawn())).toThrow("has no text bound to it");
  });
});

describe("installCopyControls", () => {
  it("copies what the control's kind resolves on a click", () => {
    // Arrange
    const written = installRecording(true);
    const control = drawn("npm test");
    // Act
    control.click();
    // Assert
    expect(written).toEqual(["npm test"]);
  });

  it("copies on a click of the control's icon", () => {
    // Arrange
    const written = installRecording(true);
    drawn("npm test");
    // Act
    document.querySelector(`.${COPY_CONTROL_CLASS} svg`)?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // Assert
    expect(written).toEqual(["npm test"]);
  });

  it("copies nothing on a click beside the control", () => {
    // Arrange
    const written = installRecording(true);
    drawn();
    // Act
    document.querySelector<HTMLElement>("[data-text]")?.click();
    // Assert
    expect(written).toEqual([]);
  });

  it("throws for a control naming no kind it knows", () => {
    // Arrange
    installRecording(true);
    const control = drawn();
    control.setAttribute(COPY_KIND_ATTRIBUTE, "nonsense");
    const errors: unknown[] = [];
    const onError = (event: ErrorEvent): void => {
      errors.push(event.error);
      event.preventDefault();
    };
    window.addEventListener("error", onError);
    // Act
    control.click();
    window.removeEventListener("error", onError);
    // Assert
    expect(errors.map((error) => String(error))).toEqual([expect.stringContaining("CopyKind")]);
  });

  it("marks the control copied once it copied", () => {
    // Arrange
    installRecording(true);
    const control = drawn();
    // Act
    control.click();
    // Assert
    expect(control.hasAttribute(COPIED_ATTRIBUTE)).toBe(true);
  });

  it("clears the copied mark after its moment", () => {
    // Arrange
    vi.useFakeTimers();
    installRecording(true);
    const control = drawn();
    control.click();
    // Act
    vi.advanceTimersByTime(COPIED_MS);
    // Assert
    expect(control.hasAttribute(COPIED_ATTRIBUTE)).toBe(false);
  });

  it("restarts the copied moment on a second copy", () => {
    // Arrange
    vi.useFakeTimers();
    installRecording(true);
    const control = drawn();
    control.click();
    vi.advanceTimersByTime(COPIED_MS - 1);
    // Act
    control.click();
    vi.advanceTimersByTime(COPIED_MS - 1);
    // Assert
    expect(control.hasAttribute(COPIED_ATTRIBUTE)).toBe(true);
  });

  it("records a copy at info, with its kind, length and the resolver's context", async () => {
    // Arrange
    const capture = captureLogRecords();
    installRecording(true);
    const control = drawn("abc");
    // Act
    control.click();
    // Assert
    const record = await forwardedRecord(capture, "copy-control.copy");
    expect([record.level.case, record.message, record.context]).toMatchObject([
      "info",
      "a copy control copied its text",
      { kind: "code", characters: 3, lang: "go", outcome: "copied" },
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
    const record = await forwardedRecord(capture, "copy-control.copy");
    expect([record.level.case, record.message, record.context, control.hasAttribute(COPIED_ATTRIBUTE)]).toMatchObject([
      "error",
      "a copy control could not write its text to the clipboard",
      { kind: "code", characters: 3, outcome: "refused" },
      false,
    ]);
  });

  it("copies nothing once uninstalled", () => {
    // Arrange
    const written = installRecording(true);
    const control = drawn();
    teardown?.();
    teardown = undefined;
    // Act
    control.click();
    // Assert
    expect(written).toEqual([]);
  });
});

describe("a click on a copy control and the bubble it sits in", () => {
  it("is a click every click owner leaves to the control", () => {
    // Arrange
    const control = drawn();
    // Act / Assert
    expect(control.closest(CLICK_THROUGH_SELECTOR)).toBe(control);
  });

  it("never selects the bubble", () => {
    // Arrange
    const row = document.createElement("div");
    row.setAttribute("data-feed-row", "r1");
    row.setAttribute(SELECTABLE_ATTRIBUTE, "");
    row.innerHTML = copyControlHtml("code", "placed", "Copy it");
    document.body.append(row);
    const control = row.querySelector(`.${COPY_CONTROL_CLASS}`);
    if (control === null) throw new Error("no copy control drawn");
    // Act
    const click = bubbleClick({ row, target: control, selectedText: "", clientX: 0 });
    // Assert
    expect(click).toEqual({ case: "ignore", reason: "interactive" });
  });
});
