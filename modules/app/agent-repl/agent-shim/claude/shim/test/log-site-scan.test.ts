/**
 * The shim's static scan of its log sites (test/log-site-scan.ts): over the
 * whole of `src/`, every call is a site of its own and every scope is one
 * logger's; over fixtures, each fault the scan names.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { siteKey } from "../../../logging/ts/logsite.js";
import { filesUnder } from "./files-under.js";
import { scanLogSites, type ScanSource } from "./log-site-scan.js";

const SOURCE_ROOT = fileURLToPath(new URL("../src/", import.meta.url));

function shimSources(): ScanSource[] {
  return filesUnder(SOURCE_ROOT)
    .filter((file) => file.endsWith(".ts"))
    .map((file) => ({ file: path.relative(SOURCE_ROOT, file), text: readFileSync(file, "utf8") }));
}

const SHIM = scanLogSites(shimSources());

/** Scan one fixture file, and the logger module beside it when asked. */
function scanFixture(files: Record<string, string>, key?: (scope: string, template: string) => string) {
  return scanLogSites(Object.entries(files).map(([file, text]) => ({ file, text })), key);
}

describe("the shim's log sites", () => {
  it("finds every log call of src/", () => {
    // Act / Assert: the inventory the conversion keyed (1,205 sites on 2026-11-20).
    expect(SHIM.sites.length).toBeGreaterThan(1_100);
  });

  it("finds no fault in src/", () => {
    // Act / Assert
    expect(SHIM.faults).toEqual([]);
  });

  it("gives every logger its own scope", () => {
    // Arrange
    const scopes = SHIM.scopes.map(({ scope }) => scope);

    // Act / Assert
    expect(new Set(scopes).size).toBe(scopes.length);
  });

  it("keys a placed site as the runtime keys it", () => {
    // Arrange
    const placed = SHIM.sites.find((site) => site.at.startsWith("store/client.ts:"));

    // Act / Assert
    expect(placed?.key).toBe(siteKey("shim", "shim.store.client", placed?.template ?? ""));
  });
});

describe("scanLogSites", () => {
  it("places a call through a logger bound in its file", () => {
    // Act
    const { sites } = scanFixture({ "a.ts": 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "hello {name}", { name });\n' });

    // Assert
    expect(sites).toEqual([{ at: "a.ts:2", scope: "shim.a", template: "hello {name}", key: siteKey("shim", "shim.a", "hello {name}") }]);
  });

  it("places a call through a logger made with with", () => {
    // Act
    const { sites } = scanFixture({ "a.ts": 'const LOGGER = bindLog("shim.a", {});\nconst TURN = LOGGER.with({ turn });\nTURN.info({}, "x");\nLOGGER.with({}).debug({}, "y");\n' });

    // Assert
    expect(sites.map((site) => site.scope)).toEqual(["shim.a", "shim.a"]);
  });

  it("places a call through a logger imported from the file that built it", () => {
    // Act
    const { sites } = scanFixture({
      "a.ts": 'export const LIFE = bindLog("shim.a", {});\n',
      "b/c.ts": 'import { LIFE as L } from "../a.js";\nL.info({}, "x");\n',
    });

    // Assert
    expect(sites.map((site) => site.scope)).toEqual(["shim.a"]);
  });

  it("reads each branch of a conditional template as a site", () => {
    // Act
    const { sites, faults } = scanFixture({ "a.ts": 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, on ? "shown" : "hidden");\n' });

    // Assert
    expect([sites.map((site) => site.template), faults]).toEqual([["shown", "hidden"], []]);
  });

  it("reads a logger's message as a site of its scope", () => {
    // Act
    const { sites } = scanFixture({ "a.ts": 'const LOGGER = bindLog("shim.a", {});\nconst M = LOGGER.message("broke {why}", { why });\n' });

    // Assert
    expect(sites.map((site) => [site.scope, site.template])).toEqual([["shim.a", "broke {why}"]]);
  });

  it.each([
    ["two calls in one scope write one template", 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "x");\nLOGGER.debug({}, "x");\n', "a.ts:3: writes the template \"x\" that a.ts:2 already writes in the scope shim.a"],
    ["a scope is not a literal", "const LOGGER = bindLog(SCOPE, {});\n", "a.ts:1: bindLog's scope is not a string literal"],
    ["a template is malformed", 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "a {Bad}");\n', "a.ts:2: logsite: the template \"a {Bad}\" names the placeholder {Bad}"],
    ["the values lack a placeholder", 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "a {x} {y}", { x });\n', "a.ts:2: the named values lack {y}"],
    ["the values hold an extra name", 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "a {x}", { x, z });\n', "a.ts:2: the named values hold z"],
    ["a conditional's branches name different placeholders", 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, c ? "a {x}" : "b", { x });\n', "a.ts:2: the branches of a conditional template name different placeholders"],
    ["a message's template is not a literal", 'const LOGGER = bindLog("shim.a", {});\nconst M = LOGGER.message(text);\n', "a.ts:2: the template is not a string literal"],
  ])("names a fault when %s", (_case, text, fault) => {
    // Act
    const { faults } = scanFixture({ "a.ts": text });

    // Assert
    expect(faults).toEqual([expect.stringContaining(fault)]);
  });

  it("names a fault when two loggers are built with one scope", () => {
    // Act
    const { faults } = scanFixture({ "a.ts": 'const A = bindLog("shim.a", {});\n', "b.ts": 'const B = bindLog("shim.a", {});\n' });

    // Assert
    expect(faults).toEqual([expect.stringContaining('b.ts:1: the scope "shim.a" is already built at a.ts:1')]);
  });

  it("names a fault when a call it cannot place writes a template another call writes", () => {
    // Act
    const { faults } = scanFixture({
      "a.ts": 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "same");\n',
      "b.ts": 'ctx.log.debug({}, "same");\n',
    });

    // Assert
    expect(faults).toEqual([expect.stringContaining("b.ts:1: logs through a logger the scan cannot place")]);
  });

  it("names a fault when two sites' keys collide", () => {
    // Act
    const { faults } = scanFixture({ "a.ts": 'const LOGGER = bindLog("shim.a", {});\nLOGGER.info({}, "x");\nLOGGER.info({}, "y");\n' }, () => "000000000000");

    // Assert
    expect(faults).toEqual([expect.stringContaining("a.ts:3: its key 000000000000 collides with another site's")]);
  });

  it("skips a console call", () => {
    // Act
    const { sites } = scanFixture({ "a.ts": 'console.error("x", "y");\n' });

    // Assert
    expect(sites).toEqual([]);
  });
});
