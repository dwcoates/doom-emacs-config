// @vitest-environment jsdom
/**
 * The copy fallback: the selection reaches the clipboard even when the host's
 * own copy would have handed the reader nothing.
 */
import { afterEach, describe, expect, it } from "vitest";
import { installCopyFallback } from "../src/copy.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

let teardown: (() => void) | undefined;

afterEach(() => {
  teardown?.();
  teardown = undefined;
  document.body.innerHTML = "";
  document.getSelection()?.removeAllRanges();
});

/** A fake `clipboardData`, which jsdom's `ClipboardEvent` does not supply. */
function fakeClipboard(): { written: Map<string, string>; api: DataTransfer } {
  const written = new Map<string, string>();
  const api = {
    setData: (format: string, value: string) => written.set(format, value),
    getData: (format: string) => written.get(format) ?? "",
  } as unknown as DataTransfer;
  return { written, api };
}

/** Dispatch a `copy` carrying CLIPBOARD, and answer whether it was defaulted. */
function dispatchCopy(clipboard: DataTransfer | null): Event {
  const event = new Event("copy", { bubbles: true, cancelable: true });
  Object.defineProperty(event, "clipboardData", { value: clipboard });
  document.dispatchEvent(event);
  return event;
}

/** Select the whole of an element's text, the way a drag-select would. */
function selectText(text: string): void {
  const host = document.createElement("p");
  host.textContent = text;
  document.body.append(host);
  const selection = document.getSelection();
  if (selection === null) throw new Error("jsdom gave the document no selection");
  selection.removeAllRanges();
  const range = document.createRange();
  range.selectNodeContents(host);
  selection.addRange(range);
}

describe("the copy fallback", () => {
  it("writes the selection's text onto the event's clipboard", () => {
    // Arrange
    teardown = installCopyFallback(document);
    selectText("boot_failed: the daemon refused the adoption");
    const clipboard = fakeClipboard();

    // Act
    dispatchCopy(clipboard.api);

    // Assert
    expect(clipboard.written.get("text/plain")).toBe(
      "boot_failed: the daemon refused the adoption",
    );
  });

  it("takes the event's default, so the host cannot overwrite what it wrote", () => {
    // Arrange
    teardown = installCopyFallback(document);
    selectText("a fault sentence");
    const clipboard = fakeClipboard();

    // Act
    const event = dispatchCopy(clipboard.api);

    // Assert
    expect(event.defaultPrevented).toBe(true);
  });

  it("leaves a collapsed selection entirely to the host", () => {
    // Arrange
    teardown = installCopyFallback(document);
    document.getSelection()?.removeAllRanges();
    const clipboard = fakeClipboard();

    // Act
    const event = dispatchCopy(clipboard.api);

    // Assert
    expect([clipboard.written.size, event.defaultPrevented]).toEqual([0, false]);
  });

  it("does nothing when the event carries no clipboard to write to", () => {
    // Arrange
    teardown = installCopyFallback(document);
    selectText("a fault sentence");

    // Act
    const event = dispatchCopy(null);

    // Assert
    expect(event.defaultPrevented).toBe(false);
  });

  it("stops writing once the fallback is torn down", () => {
    // Arrange
    const remove = installCopyFallback(document);
    selectText("a fault sentence");
    remove();
    const clipboard = fakeClipboard();

    // Act
    dispatchCopy(clipboard.api);

    // Assert
    expect(clipboard.written.size).toBe(0);
  });

  it("records the write at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    teardown = installCopyFallback(document);
    selectText("a fault sentence");

    // Act
    dispatchCopy(fakeClipboard().api);

    // Assert
    expect((await forwardedRecord(capture, "copy.fallback")).message).toBe(
      "the copy fallback wrote the selection to the clipboard",
    );
  });
});
