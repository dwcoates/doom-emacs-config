import { describe, expect, it } from "vitest";
import { ClipboardWriter, copyAction, isCopyChord, ownsItsKeys, writeSelection } from "../src/copy.js";

/** A KeyChord with no modifiers held, overridden per case. */
function chord(over: Partial<Parameters<typeof isCopyChord>[0]> = {}) {
  return { key: "y", ctrlKey: false, metaKey: false, altKey: false, shiftKey: false, ...over };
}

describe("isCopyChord", () => {
  it("accepts a bare y", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "y" }))).toBe(true);
  });

  it("accepts C-c", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "c", ctrlKey: true }))).toBe(true);
  });

  it("accepts Cmd-C, the platform copy chord", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "c", metaKey: true }))).toBe(true);
  });

  it("rejects a bare c, which is no copy gesture at all", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "c" }))).toBe(false);
  });

  it("rejects a modified y, leaving chords like C-y to their own bindings", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "y", ctrlKey: true }))).toBe(false);
  });

  it("rejects a shifted copy chord, which is a different gesture", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "c", ctrlKey: true, shiftKey: true }))).toBe(false);
  });

  it("rejects an alt-modified copy chord", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "y", altKey: true }))).toBe(false);
  });

  it("accepts an upper-case Y reported by a caps-locked keyboard", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "Y" }))).toBe(true);
  });

  it("rejects a key that is neither c nor y", () => {
    // Arrange + Act + Assert
    expect(isCopyChord(chord({ key: "p" }))).toBe(false);
  });
});

describe("ownsItsKeys", () => {
  it("cedes the keys of the composer textarea", () => {
    // Arrange + Act + Assert
    expect(ownsItsKeys({ tagName: "TEXTAREA", isContentEditable: false })).toBe(true);
  });

  it("cedes the keys of a text input", () => {
    // Arrange + Act + Assert
    expect(ownsItsKeys({ tagName: "INPUT", isContentEditable: false })).toBe(true);
  });

  it("cedes the keys of a contenteditable element", () => {
    // Arrange + Act + Assert
    expect(ownsItsKeys({ tagName: "DIV", isContentEditable: true })).toBe(true);
  });

  it("claims the keys of an ordinary feed element", () => {
    // Arrange + Act + Assert
    expect(ownsItsKeys({ tagName: "PRE", isContentEditable: false })).toBe(false);
  });

  it("claims the keys when the event has no element target", () => {
    // Arrange + Act + Assert
    expect(ownsItsKeys(null)).toBe(false);
  });
});

describe("copyAction", () => {
  const feed = { tagName: "PRE", isContentEditable: false };

  it("copies the highlight on y", () => {
    // Arrange + Act
    const text = copyAction({ event: chord({ key: "y" }), target: feed, selection: "hello" });
    // Assert
    expect(text).toBe("hello");
  });

  it("copies the highlight on C-c", () => {
    // Arrange + Act
    const text = copyAction({
      event: chord({ key: "c", ctrlKey: true }),
      target: feed,
      selection: "hello",
    });
    // Assert
    expect(text).toBe("hello");
  });

  it("copies the highlight verbatim, keeping its surrounding whitespace", () => {
    // Arrange + Act
    const text = copyAction({ event: chord(), target: feed, selection: "  indented\n" });
    // Assert
    expect(text).toBe("  indented\n");
  });

  it("stays inert when nothing is highlighted", () => {
    // Arrange + Act + Assert
    expect(copyAction({ event: chord(), target: feed, selection: "" })).toBeNull();
  });

  it("stays inert when the highlight is whitespace only", () => {
    // Arrange + Act + Assert
    expect(copyAction({ event: chord(), target: feed, selection: " \n " })).toBeNull();
  });

  it("stays inert on a non-copy key even with a highlight", () => {
    // Arrange + Act + Assert
    expect(copyAction({ event: chord({ key: "p" }), target: feed, selection: "hi" })).toBeNull();
  });

  it("leaves y to the composer, where it is a letter and not a chord", () => {
    // Arrange
    const composer = { tagName: "TEXTAREA", isContentEditable: false };
    // Act + Assert
    expect(copyAction({ event: chord({ key: "y" }), target: composer, selection: "hi" })).toBeNull();
  });
});

describe("writeSelection", () => {
  it("hands the text to the Clipboard API", async () => {
    // Arrange
    const seen: string[] = [];
    const writer: ClipboardWriter = {
      writeText: async (text) => {
        seen.push(text);
      },
    };
    // Act
    await writeSelection("hello", writer);
    // Assert
    expect(seen).toEqual(["hello"]);
  });

  it("raises when the embedding exposes no clipboard at all", async () => {
    // Arrange + Act + Assert
    await expect(writeSelection("hello", {})).rejects.toThrow(/no Clipboard API/);
  });

  it("raises the clipboard's own rejection rather than swallowing it", async () => {
    // Arrange
    const writer: ClipboardWriter = {
      writeText: () => Promise.reject(new Error("denied")),
    };
    // Act + Assert
    await expect(writeSelection("hello", writer)).rejects.toThrow("denied");
  });
});
