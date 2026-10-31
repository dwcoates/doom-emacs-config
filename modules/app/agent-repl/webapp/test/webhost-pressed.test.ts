// @vitest-environment jsdom
/**
 * webhost-pressed — the one place a page hears that the reader pressed the
 * webhost around it.
 */
import { describe, expect, it } from "vitest";
import { createWebhostPresses, pageWebhostPresses } from "../src/webhost-pressed.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

describe("createWebhostPresses", () => {
  it("tells every listener of a press", () => {
    // Arrange
    const presses = createWebhostPresses();
    const told: string[] = [];
    presses.onPressed(() => told.push("a"));
    presses.onPressed(() => told.push("b"));
    // Act
    presses.pressed();
    // Assert
    expect(told).toEqual(["a", "b"]);
  });

  it("stops telling a listener that unsubscribed", () => {
    // Arrange
    const presses = createWebhostPresses();
    let told = 0;
    const unsubscribe = presses.onPressed(() => {
      told += 1;
    });
    unsubscribe();
    // Act
    presses.pressed();
    // Assert
    expect(told).toBe(0);
  });

  it("records each press with how many listeners it told", async () => {
    // Arrange
    const records = captureLogRecords("debug");
    const presses = createWebhostPresses();
    presses.onPressed(() => {});
    // Act
    presses.pressed();
    // Assert
    const record = await forwardedRecord(records, "page.webhost-pressed");
    expect(record.context?.listeners).toBe(1);
  });
});

describe("pageWebhostPresses", () => {
  it("answers one signal per document", () => {
    // Act, Assert
    expect(pageWebhostPresses(document)).toBe(pageWebhostPresses(document));
  });

  it("answers another document its own signal", () => {
    // Arrange
    const other = document.implementation.createHTMLDocument("other");
    // Act, Assert
    expect(pageWebhostPresses(other)).not.toBe(pageWebhostPresses(document));
  });
});
