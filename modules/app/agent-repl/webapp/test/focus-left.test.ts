// @vitest-environment jsdom
/**
 * focus-left — the one place a page hears that the user's focus left it.
 */
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { FOCUS_LEFT_BLUR, FOCUS_LEFT_HIDDEN, createFocusLeft, pageFocusLeft, type FocusLeftSource } from "../src/focus-left.js";
import { createPresence, type PresenceSource } from "../src/presence.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

let presence: PresenceSource;
let focus: FocusLeftSource;
let told: string[];

beforeEach(() => {
  presence = createPresence(document, true);
  presence.setShown(true);
  focus = createFocusLeft(window, presence);
  told = [];
  focus.onLeft((why) => told.push(why));
});

afterEach(() => {
  focus.dispose();
  presence.dispose();
});

describe("createFocusLeft", () => {
  it("tells its listeners when the window loses the focus", () => {
    // Act
    window.dispatchEvent(new Event("blur"));
    // Assert
    expect(told).toEqual([FOCUS_LEFT_BLUR]);
  });

  it("tells its listeners when the page stops being seen", () => {
    // Act
    presence.setShown(false);
    // Assert
    expect(told).toEqual([FOCUS_LEFT_HIDDEN]);
  });

  it("tells nothing when the page is shown", () => {
    // Arrange
    presence.setShown(false);
    told = [];
    // Act
    presence.setShown(true);
    // Assert
    expect(told).toEqual([]);
  });

  it("tells its listeners why, when told the focus left for a reason it cannot see", () => {
    // Act
    focus.left("Emacs moved its focus");
    // Assert
    expect(told).toEqual(["Emacs moved its focus"]);
  });

  it("tells a listener no more once it unsubscribes", () => {
    // Arrange
    const later: string[] = [];
    const stop = focus.onLeft((why) => later.push(why));
    stop();
    // Act
    focus.left("Emacs moved its focus");
    // Assert
    expect(later).toEqual([]);
  });

  it("stops listening to the window and the presence once disposed", () => {
    // Arrange
    focus.dispose();
    // Act
    window.dispatchEvent(new Event("blur"));
    presence.setShown(false);
    // Assert
    expect(told).toEqual([]);
  });

  it("records each time the focus leaves, naming why", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    focus.left("Emacs moved its focus");
    // Assert
    const record = await forwardedRecord(capture, "page.focus-left");
    expect([record.level.case, record.context?.why, record.context?.listeners]).toEqual(["debug", "Emacs moved its focus", 1]);
  });
});

describe("pageFocusLeft", () => {
  it("is one signal per page", () => {
    // Act / Assert
    expect(pageFocusLeft()).toBe(pageFocusLeft());
  });
});
