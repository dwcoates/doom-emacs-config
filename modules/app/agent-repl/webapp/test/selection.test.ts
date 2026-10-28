// @vitest-environment jsdom
/** The one reader of the selection's text, and the proof every reader uses it. */
import { createControl, type Control } from "../src/control.js";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import {
  clearSelectionIn,
  installSelectionClickGuard,
  selectedSpan,
  selectedText,
  selectionTouches,
  type SelectionSource,
} from "../src/selection.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

let teardown: (() => void) | undefined;

afterEach(() => {
  teardown?.();
  teardown = undefined;
  document.body.innerHTML = "";
  document.getSelection()?.removeAllRanges();
});

/** Select the whole of a fresh paragraph holding TEXT. */
function selectParagraph(text: string): void {
  const host = document.createElement("p");
  host.textContent = text;
  document.body.append(host);
  const range = document.createRange();
  range.selectNodeContents(host);
  document.getSelection()?.addRange(range);
}


describe("selectedText", () => {
  it("answers the selected text", () => {
    // Arrange
    selectParagraph("boot_failed");
    // Act / Assert
    expect(selectedText()).toBe("boot_failed");
  });

  it("answers the empty string when the selection is collapsed", () => {
    // Arrange
    selectParagraph("boot_failed");
    document.getSelection()?.collapseToStart();
    // Act / Assert
    expect(selectedText()).toBe("");
  });

  it("answers the empty string when the source has no selection object", () => {
    // Arrange
    const source: SelectionSource = { getSelection: () => null };
    // Act / Assert
    expect(selectedText(source)).toBe("");
  });

  it("reads a document's selection as well as a window's", () => {
    // Arrange
    selectParagraph("frame unreadable");
    // Act / Assert
    expect(selectedText(document)).toBe("frame unreadable");
  });

  it("is the one reader of a selection's text in src", () => {
    // Arrange
    const readers = [...codeUnder(SRC)].filter(([file]) => !`${path.sep}${file}`.endsWith(`${path.sep}selection.ts`));
    // Act
    const handRolled = readers
      .filter(([, code]) => /getSelection\(\)\??\.toString\(/.test(code))
      .map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });
});

/** A button the guard stands in front of, counting the clicks that reach it. */
function clickTarget(): { button: Control; reached: () => number } {
  const button = createControl();
  button.textContent = "Send now";
  document.body.append(button);
  let reached = 0;
  button.addEventListener("click", () => reached++);
  return { button, reached: () => reached };
}

/** A press on TARGET, as the pointer starts a gesture. */
function press(target: Element): void {
  target.dispatchEvent(new Event("pointerdown", { bubbles: true }));
}

/** A release on TARGET: a mouse click (DETAIL 1) or a keyboard one (0). */
function release(target: Element, detail = 1): MouseEvent {
  const event = new MouseEvent("click", { bubbles: true, cancelable: true, detail });
  target.dispatchEvent(event);
  return event;
}

describe("installSelectionClickGuard", () => {
  it("swallows the click that ends a drag-select", () => {
    // Arrange
    teardown = installSelectionClickGuard(document);
    const { button, reached } = clickTarget();
    press(button);
    selectParagraph("frame unreadable");
    // Act
    release(button);
    // Assert
    expect(reached()).toBe(0);
  });

  it("cancels the default action of the click that ends a drag-select", () => {
    // Arrange
    teardown = installSelectionClickGuard(document);
    const { button } = clickTarget();
    press(button);
    selectParagraph("frame unreadable");
    // Act
    const event = release(button);
    // Assert
    expect(event.defaultPrevented).toBe(true);
  });

  it("lets a click through when nothing is selected", () => {
    // Arrange
    teardown = installSelectionClickGuard(document);
    const { button, reached } = clickTarget();
    press(button);
    // Act
    release(button);
    // Assert
    expect(reached()).toBe(1);
  });

  it("lets a click through when the selection predates the press, unchanged", () => {
    // Arrange
    teardown = installSelectionClickGuard(document);
    const { button, reached } = clickTarget();
    selectParagraph("an old highlight");
    press(button);
    // Act
    release(button);
    // Assert
    expect(reached()).toBe(1);
  });

  it("swallows the click when the gesture replaced an older selection", () => {
    // Arrange
    teardown = installSelectionClickGuard(document);
    const { button, reached } = clickTarget();
    selectParagraph("an old highlight");
    press(button);
    document.getSelection()?.removeAllRanges();
    selectParagraph("a new highlight");
    // Act
    release(button);
    // Assert
    expect(reached()).toBe(0);
  });

  it("lets a keyboard click through, a selection standing or not", () => {
    // Arrange
    teardown = installSelectionClickGuard(document);
    const { button, reached } = clickTarget();
    press(button);
    selectParagraph("frame unreadable");
    // Act
    release(button, 0);
    // Assert
    expect(reached()).toBe(1);
  });

  it("records the swallowed click at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    teardown = installSelectionClickGuard(document);
    const { button } = clickTarget();
    press(button);
    selectParagraph("frame unreadable");
    // Act
    release(button);
    // Assert
    const record = await forwardedRecord(capture, "selection.click-swallowed");
    expect([record.level.case, record.context]).toEqual([
      "debug",
      expect.objectContaining({ target: "ar-button", characters: 16 }),
    ]);
  });

  it("guards nothing once uninstalled", () => {
    // Arrange
    installSelectionClickGuard(document)();
    const { button, reached } = clickTarget();
    press(button);
    selectParagraph("frame unreadable");
    // Act
    release(button);
    // Assert
    expect(reached()).toBe(1);
  });
});

/** A fresh paragraph holding TEXT, unselected. */
function paragraph(text: string): HTMLElement {
  const host = document.createElement("p");
  host.textContent = text;
  document.body.append(host);
  return host;
}

/** Select the whole of EL. */
function selectAll(el: Node): void {
  const range = document.createRange();
  range.selectNodeContents(el);
  document.getSelection()?.removeAllRanges();
  document.getSelection()?.addRange(range);
}

describe("selectedSpan", () => {
  it("answers the selected text and the nodes it starts and ends in", () => {
    // Arrange
    const host = paragraph("entropy");
    selectAll(host);
    // Act
    const span = selectedSpan();
    // Assert
    expect([span?.text, span?.anchor === host, span?.focus === host]).toEqual(["entropy", true, true]);
  });

  it("answers null when nothing is selected", () => {
    // Arrange, Act, Assert
    expect(selectedSpan()).toBeNull();
  });

  it("answers null for a source with no selection object", () => {
    // Arrange
    const none: SelectionSource = { getSelection: () => null };
    // Act, Assert
    expect(selectedSpan(none)).toBeNull();
  });
});

describe("selectionTouches", () => {
  it("answers true for a selection inside the root", () => {
    // Arrange
    const host = paragraph("entropy");
    selectAll(host);
    // Act, Assert
    expect(selectionTouches(document.body)).toBe(true);
  });

  it("answers false for a selection elsewhere", () => {
    // Arrange
    const inside = paragraph("inside");
    selectAll(paragraph("elsewhere"));
    // Act, Assert
    expect(selectionTouches(inside)).toBe(false);
  });
});

describe("clearSelectionIn", () => {
  it("drops a selection that touches the root", () => {
    // Arrange
    const host = paragraph("entropy");
    selectAll(host);
    // Act
    const cleared = clearSelectionIn(host);
    // Assert
    expect([cleared, selectedText()]).toEqual([true, ""]);
  });

  it("leaves a selection elsewhere standing", () => {
    // Arrange
    const inside = paragraph("inside");
    selectAll(paragraph("elsewhere"));
    // Act
    const cleared = clearSelectionIn(inside);
    // Assert
    expect([cleared, selectedText()]).toEqual([false, "elsewhere"]);
  });
});
