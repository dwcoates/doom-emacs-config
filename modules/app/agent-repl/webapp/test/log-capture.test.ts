/**
 * log-capture — the one way a test reads back forwarded log records.
 */
import { readFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it, vi } from "vitest";
import { log } from "../src/log.js";
import { captureLogRecords, forwardedRecord, forwardedRecords, recordFor } from "./log-capture.js";
import { filesUnder } from "./files-under.js";

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
    fresh.log.info("fresh", { operation: "log-capture.test.fresh" });

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

describe("the one capture", () => {
  it("is the only place a test builds a logger that keeps what it forwards", () => {
    // Arrange: log.test.ts is the logger's own suite, which builds loggers
    // directly to test them; every other suite reads records through here.
    const here = dirname(fileURLToPath(import.meta.url));
    const allowed = new Set(["log-capture.ts", "log.test.ts", "log-capture.test.ts"]);

    // Act
    const handRolled = filesUnder(here)
      .map((path) => relative(here, path))
      .filter((path) => !allowed.has(path))
      .filter((path) => /new\s+(\w+\.)?ForwardingLogger\([\s\S]{0,200}?\.push\(record\)/.test(readFileSync(resolve(here, path), "utf8")));

    // Assert
    expect(handRolled).toEqual([]);
  });
});
