/** The one esbuild configuration every Node bundle of this package shares. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { bundleOptions } from "../build-options.mjs";

const root = path.join(path.dirname(fileURLToPath(import.meta.url)), "..");

describe("bundleOptions", () => {
  it("bundles one entry into one ESM file for node, resolving from this package", () => {
    // Act.
    const options = bundleOptions("/src/entry.ts", "/out/entry.mjs") as Record<string, unknown>;

    // Assert.
    expect(options.entryPoints).toEqual(["/src/entry.ts"]);
    expect(options.outfile).toBe("/out/entry.mjs");
    expect(options.bundle).toBe(true);
    expect(options.platform).toBe("node");
    expect(options.format).toBe("esm");
    expect(options.nodePaths).toEqual([path.join(root, "node_modules")]);
    expect((options.plugins as unknown[]).length).toBe(1);
  });

  it.each(["build.mjs", "test/fakes/build-store-server.mjs"])("%s builds through it", (script) => {
    // Act.
    const source = readFileSync(path.join(root, script), "utf8");

    // Assert: the shared options, and no esbuild configuration of its own.
    expect(source).toContain("...bundleOptions(");
    expect(source).not.toMatch(/nodePaths\s*:/);
  });
});
