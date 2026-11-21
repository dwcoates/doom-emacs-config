/**
 * The webapp's static scan of its log sites (test/log-site-scan.ts): over the
 * whole of `src/`, every call is a site of its own and every scope is one
 * logger's; over fixtures, each fault the scan names.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { siteKey } from "../../agent-shim/logging/ts/logsite.js";
import { filesUnder } from "./files-under.js";
import { scanLogSites, type ScanSource } from "./log-site-scan.js";

const SOURCE_ROOT = fileURLToPath(new URL("../src/", import.meta.url));

function webappSources(): ScanSource[] {
  return filesUnder(SOURCE_ROOT, [".ts", ".tsx"])
    .filter((file) => !file.endsWith(".d.ts"))
    .map((file) => ({ file: path.relative(SOURCE_ROOT, file), text: readFileSync(file, "utf8") }));
}

const WEBAPP = scanLogSites(webappSources());

/** Scan fixture files, keyed by KEY when a test hands one in. */
function scanFixture(files: Record<string, string>, key?: (scope: string, template: string) => string) {
  return scanLogSites(Object.entries(files).map(([file, text]) => ({ file, text })), key);
}

describe("the webapp's log sites", () => {
  it("finds every log call of src/", () => {
    // Act / Assert: the inventory the conversion keyed (919 sites on 2026-11-20).
    expect(WEBAPP.sites.length).toBeGreaterThan(850);
  });

  it("finds no fault in src/", () => {
    // Act / Assert
    expect(WEBAPP.faults).toEqual([]);
  });

  it("gives every logger its own scope", () => {
    // Arrange
    const scopes = WEBAPP.scopes.map(({ scope }) => scope);

    // Act / Assert
    expect(new Set(scopes).size).toBe(scopes.length);
  });

  it("places every call it finds", () => {
    // Act / Assert
    expect(WEBAPP.sites.filter((site) => site.scope === null).map((site) => site.at)).toEqual([]);
  });
});

describe("scanLogSites", () => {
  it("places a call through a logger bound in its file", () => {
    // Act
    const { sites } = scanFixture({ "a.ts": 'const log = createLogger("webapp.a");\nlog.info("hello {name}", { operation: "o" }, { name });\n' });

    // Assert
    expect(sites).toEqual([{ at: "a.ts:2", scope: "webapp.a", template: "hello {name}", key: siteKey("webapp", "webapp.a", "hello {name}") }]);
  });

  it("reads a .tsx file", () => {
    // Act
    const { sites } = scanFixture({ "a.tsx": 'const log = createLogger("webapp.a");\nconst v = <div onClick={() => log.info("clicked", { operation: "o" })} />;\n' });

    // Assert
    expect(sites.map((site) => site.template)).toEqual(["clicked"]);
  });

  it("reads each branch of a conditional template as a site", () => {
    // Act
    const { sites, faults } = scanFixture({ "a.ts": 'const log = createLogger("webapp.a");\nlog.info(on ? "shown" : "hidden", { operation: "o" });\n' });

    // Assert
    expect([sites.map((site) => site.template), faults]).toEqual([["shown", "hidden"], []]);
  });

  it("reads a logger's message as a site of its scope", () => {
    // Act
    const { sites } = scanFixture({ "a.ts": 'const log = createLogger("webapp.a");\nconst M = log.message("broke {why}", { why });\n' });

    // Assert
    expect(sites.map((site) => [site.scope, site.template])).toEqual([["webapp.a", "broke {why}"]]);
  });

  it.each([
    ["two calls in one scope write one template", 'const log = createLogger("webapp.a");\nlog.info("x", {});\nlog.debug("x", {});\n', 'a.ts:3: writes the template "x" that a.ts:2 already writes in the scope webapp.a'],
    ["a scope is not a literal", "const log = createLogger(SCOPE);\n", "a.ts:1: createLogger's scope is not a string literal"],
    ["a template is not a literal", 'const log = createLogger("webapp.a");\nlog.info(text, {});\n', "a.ts:2: the template is not a string literal"],
    ["a template is malformed", 'const log = createLogger("webapp.a");\nlog.info("a }", {});\n', "a.ts:2: logsite: the template \"a }\" has a lone closing brace"],
    ["the values lack a placeholder", 'const log = createLogger("webapp.a");\nlog.info("a {x} {y}", {}, { x });\n', "a.ts:2: the named values lack {y}"],
    ["the values hold an extra name", 'const log = createLogger("webapp.a");\nlog.info("a {x}", {}, { x, z });\n', "a.ts:2: the named values hold z"],
    ["a conditional's branches name different placeholders", 'const log = createLogger("webapp.a");\nlog.info(c ? "a {x}" : "b", {}, { x });\n', "a.ts:2: the branches of a conditional template name different placeholders"],
  ])("names a fault when %s", (_case, text, fault) => {
    // Act
    const { faults } = scanFixture({ "a.ts": text });

    // Assert
    expect(faults).toEqual([expect.stringContaining(fault)]);
  });

  it("names a fault when two loggers are built with one scope", () => {
    // Act
    const { faults } = scanFixture({ "a.ts": 'const log = createLogger("webapp.a");\n', "b.ts": 'const log = createLogger("webapp.a");\n' });

    // Assert
    expect(faults).toEqual([expect.stringContaining('b.ts:1: the scope "webapp.a" is already built at a.ts:1')]);
  });

  it("names a fault when two sites' keys collide", () => {
    // Act
    const { faults } = scanFixture({ "a.ts": 'const log = createLogger("webapp.a");\nlog.info("x", {});\nlog.info("y", {});\n' }, () => "000000000000");

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
