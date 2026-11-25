// @vitest-environment jsdom
//
// The WebKit pages' shared logger (test/webkit/page-log.ts), under jsdom.
// Every test starts on production's logger again (setup.ts, page-state.ts).
import { describe, expect, it } from "vitest";
import { installPageLogger } from "./webkit/page-log";
import { createLogger } from "../src/log.js";

const log = createLogger("webapp.test.page-log");

describe("installPageLogger", () => {
  it("keeps every record as `level operation`, in order", () => {
    // Arrange
    const records = installPageLogger("sample");
    // Act
    log.debug("a debug record", { operation: "page-log.debug" });
    log.error("an error record", { operation: "page-log.error" });
    // Assert
    expect(records()).toEqual(["debug page-log.debug", "error page-log.error"]);
  });

  it("keeps nothing below the page's level", () => {
    // Arrange
    const records = installPageLogger("sample", "info");
    // Act
    log.debug("a debug record", { operation: "page-log.debug" });
    log.info("an info record", { operation: "page-log.info" });
    // Assert
    expect(records()).toEqual(["info page-log.info"]);
  });

  it("answers a copy, so a reader cannot edit what the page kept", () => {
    // Arrange
    const records = installPageLogger("sample");
    log.info("an info record", { operation: "page-log.info" });
    // Act
    records().length = 0;
    // Assert
    expect(records()).toEqual(["info page-log.info"]);
  });
});
