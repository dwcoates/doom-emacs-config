/**
 * THE ONE PAGE LOGGER (page-logger.ts): every WebKit test page installs the
 * webapp's logger through `installPageLogger`, and none spells its own.
 */
import path from "node:path";
import { readdir, readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import { log, setLogger, ForwardingLogger, bindLogContext } from "../../src/log.js";
import { installPageLogger, pageConnectionId } from "./page-logger";

const here = path.dirname(fileURLToPath(import.meta.url));

afterEach(() => {
  setLogger(new ForwardingLogger(() => Promise.resolve("accepted"), () => undefined, {}, "info"));
  bindLogContext({});
});

describe("installPageLogger", () => {
  it("names the page's connection after its file", () => {
    // Act / Assert
    expect(pageConnectionId("feed-zoom")).toBe("webkit-feed-zoom-page");
  });

  it("lands every written record in WRITTEN as its level and operation", () => {
    // Arrange
    const written: string[] = [];
    installPageLogger("probe", { written });
    // Act
    log.debug("a probe record", { operation: "probe.debug" });
    log.error("a probe failure", { operation: "probe.error" });
    // Assert
    expect(written).toEqual(["debug probe.debug", "error probe.error"]);
  });

  it("writes at DEBUG when no level is given", () => {
    // Arrange
    const written: string[] = [];
    installPageLogger("probe", { written });
    // Act
    log.debug("a probe record", { operation: "probe.debug" });
    // Assert
    expect(written).toHaveLength(1);
  });

  it("drops what its level does not admit", () => {
    // Arrange
    const written: string[] = [];
    installPageLogger("probe", { level: "info", written });
    // Act
    log.debug("a probe record", { operation: "probe.debug" });
    // Assert
    expect(written).toEqual([]);
  });

  it("is the only way a test page installs a logger", async () => {
    // Arrange
    const pages = (await readdir(here)).filter((f) => f.endsWith("-page.ts"));
    // Act
    const own: string[] = [];
    for (const page of pages) {
      if (/new ForwardingLogger|setLogger\(/.test(await readFile(path.join(here, page), "utf8"))) own.push(page);
    }
    // Assert
    expect(own).toEqual([]);
  });
});
