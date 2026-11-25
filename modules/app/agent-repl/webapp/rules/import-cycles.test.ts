// @vitest-environment node
/**
 * THE WEBAPP'S SOURCES HOLD NO IMPORT CYCLE, and the scan that proves it
 * (`sourceImportGraph`, test/import-cycles.ts) reads every edge a cycle could
 * run through.
 *
 * Inside a cycle a module that reads an import while it is evaluated (a
 * constant built from another module's class name, a table of another
 * module's functions) can read it before it is defined, depending on which
 * module of the cycle loads first. One such read, the folded sections'
 * classes, was already broken when a 44-module cycle was found (2026-11-20).
 */
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { importCycles } from "../app-layers.js";
import { sourceImportGraph } from "./import-cycles.js";

const here = dirname(fileURLToPath(import.meta.url));

describe("the webapp's sources", () => {
  const graph = sourceImportGraph(join(here, "..", "src"));

  it("hold no import cycle", () => {
    // Act
    const cycles = importCycles(graph);
    // Assert
    expect(cycles).toEqual([]);
  });

  it("are read with their edges, so the check above is not vacuous", () => {
    // Act
    const imports = graph.find((m) => m.id === "feed/row-renderers.ts")?.imports;
    // Assert
    expect(imports).toContain("feed/cards/response.ts");
  });
});

describe("sourceImportGraph", () => {
  // The scanned root is a directory of the fixture, so a module can stand outside it.
  let fixture = "";
  let root = "";

  /** Write FILES (path to text, relative to the scanned root) into the fixture. */
  function write(files: Record<string, string>): void {
    for (const [path, text] of Object.entries(files)) {
      mkdirSync(dirname(join(root, path)), { recursive: true });
      writeFileSync(join(root, path), text);
    }
  }

  /** What module ID imports, in the graph of the fixture's root. */
  const importsOf = (id: string): readonly string[] | undefined => sourceImportGraph(root).find((m) => m.id === id)?.imports;

  beforeEach(() => {
    fixture = mkdtempSync(join(tmpdir(), "import-cycles-"));
    root = join(fixture, "src");
    mkdirSync(root);
  });

  afterEach(() => {
    rmSync(fixture, { recursive: true, force: true });
  });

  it.each([
    ["a value import", 'import { b } from "./b.js";'],
    ["a type-only import", 'import type { B } from "./b.js";'],
    ["an import of one type among values", 'import { type B } from "./b.js";'],
    ["a side-effect import", 'import "./b.js";'],
    ["a re-export", 'export { b } from "./b.js";'],
    ["a re-export of everything", 'export * from "./b.js";'],
    ["a type-only re-export", 'export type { B } from "./b.js";'],
    ["an import-equals require", 'import b = require("./b.js");'],
    ["a specifier without an extension", 'import { b } from "./b";'],
    ["a specifier naming the .ts file", 'import { b } from "./b.ts";'],
  ])("reads %s as an edge", (_name, statement) => {
    // Arrange
    write({ "a.ts": statement, "b.ts": "export const b = 1;" });
    // Act
    const got = importsOf("a.ts");
    // Assert
    expect(got).toEqual(["b.ts"]);
  });

  it.each([
    ["a .tsx module", "c/b.tsx", 'import { b } from "./c/b.js";'],
    ["a directory's index", "c/index.ts", 'import { b } from "./c";'],
    ["a parent directory's module", "c/b.ts", 'import { b } from "./c/b.js";'],
  ])("resolves %s", (_name, target, statement) => {
    // Arrange
    write({ "a.ts": statement, [target]: "export const b = 1;" });
    // Act
    const got = importsOf("a.ts");
    // Assert
    expect(got).toEqual([target]);
  });

  it.each([
    ["a dynamic import", 'export const load = () => import("./b.js");'],
    ["a package", 'import { create } from "@bufbuild/protobuf";'],
    ["a query import", 'import css from "./b.css?raw";'],
    ["a module outside the root", 'import { b } from "../outside.js";'],
    ["a file that is no module", 'import data from "./b.json";'],
  ])("reads %s as no edge", (_name, statement) => {
    // Arrange
    write({ "a.ts": statement, "b.ts": "export const b = 1;", "b.json": "{}", "../outside.ts": "export const b = 1;" });
    // Act
    const got = importsOf("a.ts");
    // Assert
    expect(got).toEqual([]);
  });

  it("leaves declaration files out of the graph", () => {
    // Arrange
    write({ "a.ts": "", "env.d.ts": "declare const x: number;" });
    // Act
    const ids = sourceImportGraph(root).map((m) => m.id);
    // Assert
    expect(ids).toEqual(["a.ts"]);
  });

  it("finds a cycle that runs through a type-only import", () => {
    // Arrange
    write({ "a.ts": 'import { b } from "./b.js";\nexport type A = 1;', "b.ts": 'import type { A } from "./a.js";\nexport const b = 1;' });
    // Act
    const cycles = importCycles(sourceImportGraph(root));
    // Assert
    expect(cycles).toEqual([["a.ts", "b.ts"]]);
  });
});
