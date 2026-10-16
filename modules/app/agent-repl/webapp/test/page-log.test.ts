// @vitest-environment jsdom
//
// The WebKit pages' shared logger (test/webkit/page-log.ts), under jsdom.
// Every test starts on production's logger again (setup.ts, page-state.ts).
import { describe, expect, it } from "vitest";
import { installPageLogger } from "./webkit/page-log";
import { log } from "../src/log.js";

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

  it("is the one logger setup: no WebKit page installs a logger of its own", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const pages = import.meta.glob("./webkit/*-page.ts", {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;
    const sharing = Object.values(pages).filter((src) => /installPageLogger\(/.test(src));
    // Act
    const own = Object.entries(pages)
      .filter(([, src]) => /\bsetLogger\(|\bbindLogContext\(/.test(src))
      .map(([path]) => path);
    // Assert
    expect([sharing.length >= 5, own]).toEqual([true, []]);
  });
});
