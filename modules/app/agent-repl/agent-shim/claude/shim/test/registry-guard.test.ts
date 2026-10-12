/**
 * The build-time protobuf-es registry guard (proto/ts/registry-guard.mjs),
 * which the shim's esbuild bundle and the webapp's vite build apply.
 */
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { describe, expect, test } from "vitest";

import { guardRegistrySource, REGISTRY_FILE } from "../../../../proto/ts/registry-guard.mjs";

const require = createRequire(import.meta.url);

/** The installed protobuf-es's own ESM registry.js. */
function installedRegistrySource(): string {
  const entry = require.resolve("@bufbuild/protobuf");
  return readFileSync(join(dirname(entry), "..", "esm", "registry.js"), "utf8");
}

describe("guardRegistrySource", () => {
  test("guards the installed registry's addFile", () => {
    // Arrange
    const source = installedRegistrySource();

    // Act
    const guarded = guardRegistrySource(source);

    // Assert
    expect(guarded).toContain("if (fullyAdded.has(");
  });

  test("declares the fully-added set in every base registry", () => {
    // Arrange
    const source = installedRegistrySource();

    // Act
    const guarded = guardRegistrySource(source);

    // Assert
    expect(guarded).toContain("function createBaseRegistry() {\n    const fullyAdded = new Set();");
  });

  test("refuses a source it has already guarded", () => {
    // Arrange
    const guarded = guardRegistrySource(installedRegistrySource());

    // Act / Assert
    expect(() => guardRegistrySource(guarded)).toThrow(/already mentions fullyAdded/);
  });

  test("refuses a registry whose addFile is not the guarded shape", () => {
    // Arrange
    const source = installedRegistrySource().replace("addFile(file, skipTypes, withDeps)", "addFile(file, opts)");

    // Act / Assert
    expect(() => guardRegistrySource(source)).toThrow(/guarded shape/);
  });

  test("refuses a registry with no createBaseRegistry", () => {
    // Arrange
    const source = installedRegistrySource().replace("function createBaseRegistry()", "function makeRegistry()");

    // Act / Assert
    expect(() => guardRegistrySource(source)).toThrow(/createBaseRegistry/);
  });
});

describe("REGISTRY_FILE", () => {
  test.each([
    ["an esm registry", "/x/node_modules/@bufbuild/protobuf/dist/esm/registry.js", true],
    ["a cjs registry", "/x/node_modules/@bufbuild/protobuf/dist/cjs/registry.js", true],
    ["another protobuf-es module", "/x/node_modules/@bufbuild/protobuf/dist/esm/reflect/nested-types.js", false],
    ["another package's registry", "/x/node_modules/other/dist/esm/registry.js", false],
  ])("matches %s: %s -> %s", (_name, path, want) => {
    // Act / Assert
    expect(REGISTRY_FILE.test(path)).toBe(want);
  });
});
