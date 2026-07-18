import { describe, expect, it } from "vitest";
import { stripMetaSpans } from "../src/meta.js";
import {
  AUTO_CONTINUE_COMMAND,
  hiddenContinueMessage,
  recallMidTask,
  rememberMidTask,
  shouldAutoContinue,
} from "../src/resume-continue.js";

/** Minimal in-memory Storage for the localStorage seam. */
class FakeStorage implements Storage {
  private items = new Map<string, string>();

  get length(): number {
    return this.items.size;
  }

  clear(): void {
    this.items.clear();
  }

  getItem(key: string): string | null {
    return this.items.get(key) ?? null;
  }

  key(index: number): string | null {
    return [...this.items.keys()][index] ?? null;
  }

  removeItem(key: string): void {
    this.items.delete(key);
  }

  setItem(key: string, value: string): void {
    this.items.set(key, value);
  }
}

describe("hiddenContinueMessage", () => {
  it("carries the continue command as its payload", () => {
    // Act / Assert — the command survives inside the wrapper.
    expect(hiddenContinueMessage()).toContain(AUTO_CONTINUE_COMMAND);
  });

  it("empties in the feed once its meta markers are stripped", () => {
    // Act — the feed shows a turn only through stripMetaSpans.
    const shown = stripMetaSpans(hiddenContinueMessage());
    // Assert — nothing is left, so no prompt bubble is drawn.
    expect(shown).toBe("");
  });
});

describe("mid-task marker persistence", () => {
  it("sets the marker while a turn is running", () => {
    // Arrange
    const storage = new FakeStorage();
    // Act
    rememberMidTask(storage, "uuid-1", true);
    // Assert
    expect(recallMidTask(storage, "uuid-1")).toBe(true);
  });

  it("clears the marker when the turn completes", () => {
    // Arrange — a running turn recorded, then finished.
    const storage = new FakeStorage();
    rememberMidTask(storage, "uuid-1", true);
    // Act
    rememberMidTask(storage, "uuid-1", false);
    // Assert
    expect(recallMidTask(storage, "uuid-1")).toBe(false);
  });

  it("skips an empty conversation id so nothing is written under the bare prefix", () => {
    // Arrange
    const storage = new FakeStorage();
    // Act
    rememberMidTask(storage, "", true);
    // Assert — the unkeyable id left no record at all.
    expect(storage.length).toBe(0);
  });

  it("recalls false for an id that never ran a turn", () => {
    // Arrange
    const storage = new FakeStorage();
    // Act / Assert
    expect(recallMidTask(storage, "uuid-unknown")).toBe(false);
  });

  it("recalls false for an empty conversation id", () => {
    // Arrange
    const storage = new FakeStorage();
    // Act / Assert
    expect(recallMidTask(storage, "")).toBe(false);
  });
});

describe("shouldAutoContinue", () => {
  it("resumes when the session was stopped mid-task and its turn is not live", () => {
    // Arrange — the marker outlived the killed turn.
    const storage = new FakeStorage();
    rememberMidTask(storage, "uuid-1", true);
    // Act / Assert
    expect(shouldAutoContinue(storage, "uuid-1", false)).toBe(true);
  });

  it("stays silent when the rehydrated turn is genuinely live", () => {
    // Arrange — a second tab joins a session whose turn still runs.
    const storage = new FakeStorage();
    rememberMidTask(storage, "uuid-1", true);
    // Act / Assert — the live turn is left alone.
    expect(shouldAutoContinue(storage, "uuid-1", true)).toBe(false);
  });

  it("stays silent when no mid-task marker was ever stored", () => {
    // Arrange — a session that ended idle leaves no marker.
    const storage = new FakeStorage();
    // Act / Assert
    expect(shouldAutoContinue(storage, "uuid-1", false)).toBe(false);
  });
});
