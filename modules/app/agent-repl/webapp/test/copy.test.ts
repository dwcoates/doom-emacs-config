// @vitest-environment jsdom
/**
 * The copy fallback: the selection reaches the clipboard even when the host's
 * own copy would have handed the reader nothing.
 */
import { afterEach, describe, expect, it } from "vitest";
import { installCopyChords, installCopyFallback, isCopyChord, writeClipboardText } from "../src/copy.js";
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

/** A `keydown` of KEY under MODIFIERS, dispatched on the document; answers the event. */
function pressChord(key: string, modifiers: KeyboardEventInit): KeyboardEvent {
  const event = new KeyboardEvent("keydown", { key, ...modifiers, bubbles: true, cancelable: true });
  document.dispatchEvent(event);
  return event;
}

describe("isCopyChord", () => {
  it.each<[string, string, KeyboardEventInit, boolean]>([
    ["C-c", "c", { ctrlKey: true }, true],
    ["Cmd-c", "c", { metaKey: true }, true],
    ["Cmd-c with caps lock", "C", { metaKey: true }, true],
    ["a bare c", "c", {}, false],
    ["Control and Command together", "c", { ctrlKey: true, metaKey: true }, false],
    ["C-M-c", "c", { ctrlKey: true, altKey: true }, false],
    ["Cmd-Shift-c", "c", { metaKey: true, shiftKey: true }, false],
    ["C-v", "v", { ctrlKey: true }, false],
  ])("answers %s", (_name, key, modifiers, expected) => {
    // Arrange
    const event = new KeyboardEvent("keydown", { key, ...modifiers });

    // Act
    const answer = isCopyChord(event);

    // Assert
    expect(answer).toBe(expected);
  });
});

describe("the copy chords", () => {
  it.each<[string, KeyboardEventInit]>([
    ["C-c", { ctrlKey: true }],
    ["Cmd-c", { metaKey: true }],
  ])("%s runs the copy command over a selection", (_name, modifiers) => {
    // Arrange
    let copies = 0;
    teardown = installCopyChords(document, () => ++copies > 0);
    selectText("a fault sentence");

    // Act
    pressChord("c", modifiers);

    // Assert
    expect(copies).toBe(1);
  });

  it("takes the chord's default once the copy is done", () => {
    // Arrange
    teardown = installCopyChords(document, () => true);
    selectText("a fault sentence");

    // Act
    const event = pressChord("c", { metaKey: true });

    // Assert
    expect(event.defaultPrevented).toBe(true);
  });

  it("hands the fallback the selection's text through the copy event", () => {
    // Arrange
    const removeFallback = installCopyFallback(document);
    const clipboard = fakeClipboard();
    const removeChords = installCopyChords(document, () => dispatchCopy(clipboard.api).defaultPrevented);
    teardown = () => {
      removeChords();
      removeFallback();
    };
    selectText("boot_failed: the daemon refused the adoption");

    // Act
    pressChord("c", { ctrlKey: true });

    // Assert
    expect(clipboard.written.get("text/plain")).toBe("boot_failed: the daemon refused the adoption");
  });

  it("leaves a chord over a collapsed selection entirely alone", () => {
    // Arrange
    let copies = 0;
    teardown = installCopyChords(document, () => ++copies > 0);
    document.getSelection()?.removeAllRanges();

    // Act
    const event = pressChord("c", { ctrlKey: true });

    // Assert
    expect([copies, event.defaultPrevented]).toEqual([0, false]);
  });

  it("leaves every other chord alone", () => {
    // Arrange
    let copies = 0;
    teardown = installCopyChords(document, () => ++copies > 0);
    selectText("a fault sentence");

    // Act
    const event = pressChord("v", { ctrlKey: true });

    // Assert
    expect([copies, event.defaultPrevented]).toEqual([0, false]);
  });

  it("stops copying once the chords are torn down", () => {
    // Arrange
    let copies = 0;
    const remove = installCopyChords(document, () => ++copies > 0);
    selectText("a fault sentence");
    remove();

    // Act
    pressChord("c", { ctrlKey: true });

    // Assert
    expect(copies).toBe(0);
  });

  it("records a done copy at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    teardown = installCopyChords(document, () => true);
    selectText("a fault sentence");

    // Act
    pressChord("c", { metaKey: true });

    // Assert
    const record = await forwardedRecord(capture, "copy.chord");
    expect([record.level.case, record.message, record.context]).toMatchObject([
      "debug",
      "a copy chord copied the selection",
      { chord: "Cmd-c", characters: 16, outcome: "copied" },
    ]);
  });

  it("leaves the chord's default to the browser when the browser refuses the copy", () => {
    // Arrange
    teardown = installCopyChords(document, () => false);
    selectText("a fault sentence");

    // Act
    const event = pressChord("c", { ctrlKey: true });

    // Assert
    expect(event.defaultPrevented).toBe(false);
  });

  it("records a refused copy at error", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    teardown = installCopyChords(document, () => false);
    selectText("a fault sentence");

    // Act
    pressChord("c", { ctrlKey: true });

    // Assert
    const record = await forwardedRecord(capture, "copy.chord");
    expect([record.level.case, record.message, record.context]).toMatchObject([
      "error",
      "the browser refused to copy the selection",
      { chord: "C-c", characters: 16, outcome: "refused" },
    ]);
  });
});

describe("writeClipboardText", () => {
  it("writes the text it was handed onto the copy event's clipboard", () => {
    // Arrange
    const clipboard = fakeClipboard();

    // Act
    const written = writeClipboardText(document, "go test ./...", () => {
      dispatchCopy(clipboard.api);
      return true;
    });

    // Assert
    expect([written, clipboard.written.get("text/plain")]).toEqual([true, "go test ./..."]);
  });

  it("writes its text over a standing selection, ahead of the copy fallback", () => {
    // Arrange
    teardown = installCopyFallback(document);
    selectText("the reader's highlight");
    const clipboard = fakeClipboard();

    // Act
    writeClipboardText(document, "the code", () => {
      dispatchCopy(clipboard.api);
      return true;
    });

    // Assert
    expect(clipboard.written.get("text/plain")).toBe("the code");
  });

  it("cancels the copy event's own default", () => {
    // Arrange
    let event: Event | undefined;

    // Act
    writeClipboardText(document, "x", () => {
      event = dispatchCopy(fakeClipboard().api);
      return true;
    });

    // Assert
    expect(event?.defaultPrevented).toBe(true);
  });

  it("answers false when the browser fired no copy event", () => {
    // Act / Assert
    expect(writeClipboardText(document, "x", () => false)).toBe(false);
  });

  it("answers false when the copy event carried no clipboard", () => {
    // Act
    const written = writeClipboardText(document, "x", () => {
      dispatchCopy(null);
      return true;
    });

    // Assert
    expect(written).toBe(false);
  });

  it("stops answering copy events once it returns", () => {
    // Arrange
    writeClipboardText(document, "stale", () => true);
    const clipboard = fakeClipboard();

    // Act
    dispatchCopy(clipboard.api);

    // Assert
    expect(clipboard.written.has("text/plain")).toBe(false);
  });

  it("stops answering copy events when the copy command throws", () => {
    // Arrange
    expect(() =>
      writeClipboardText(document, "stale", () => {
        throw new Error("no copy command");
      }),
    ).toThrow("no copy command");
    const clipboard = fakeClipboard();

    // Act
    dispatchCopy(clipboard.api);

    // Assert
    expect(clipboard.written.has("text/plain")).toBe(false);
  });
});
