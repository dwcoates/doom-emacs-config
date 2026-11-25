/**
 * log-capture — the one way a test reads back forwarded log records.
 */
import { describe, expect, it, vi } from "vitest";
import { createLogger } from "../src/log.js";
import { captureLogRecords, forwardedRecord, forwardedRecords, recordFor } from "./log-capture.js";

const log = createLogger("webapp.test.log-capture");

describe("captureLogRecords", () => {
  it("captures what this file's log module forwards", async () => {
    // Arrange
    const capture = captureLogRecords();

    // Act
    log.info("captured", { operation: "log-capture.test.default" });

    // Assert
    expect((await forwardedRecord(capture, "log-capture.test.default")).message).toBe("captured");
  });

  it("drops what falls below its threshold", async () => {
    // Arrange
    const capture = captureLogRecords("info");

    // Act
    log.debug("below", { operation: "log-capture.test.below" });

    // Assert
    expect((await forwardedRecords(capture)).map((r) => r.operation)).not.toContain("log-capture.test.below");
  });

  it("installs into a fresh log module it is handed, which a fresh module graph logs through", async () => {
    // Arrange
    vi.resetModules();
    const fresh = await import("../src/log.js");
    const capture = captureLogRecords("info", fresh);

    // Act
    fresh.createLogger("webapp.test.log-capture.fresh").info("fresh", { operation: "log-capture.test.fresh" });

    // Assert
    expect((await forwardedRecord(capture, "log-capture.test.fresh")).message).toBe("fresh");
  });

  it("binds the test connection, so every record carries an identity", async () => {
    // Arrange
    const capture = captureLogRecords();

    // Act
    log.info("bound", { operation: "log-capture.test.bound" });

    // Assert
    expect((await forwardedRecord(capture, "log-capture.test.bound")).context).toEqual(
      expect.objectContaining({ connection_id: "test-connection" }),
    );
  });
});

describe("recordFor", () => {
  it("throws, naming the operation, when no record went for it", () => {
    // Act + Assert
    expect(() => recordFor([], "log-capture.test.absent")).toThrow("no forwarded record for log-capture.test.absent");
  });
});
