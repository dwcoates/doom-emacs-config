/**
 * The scenario goldens stay what a fresh drive produces (goldens.ts).
 *
 * BOTH DIRECTIONS: every scenario's golden matches its drive byte for byte, and
 * the goldens directory holds a golden for no scenario the table does not
 * drive. `AGENT_REPL_SCENARIO_GOLDENS_WRITE=1` rewrites them instead.
 */
import { existsSync, mkdirSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import { dirname, join, relative } from "node:path";

import { describe, expect, it } from "vitest";

import { driveGolden, GOLDEN_SCENARIOS, goldenName, GOLDENS_DIR, GOLDENS_WRITE_ENV, renderMeta, type GoldenOutput } from "./goldens.js";

const writing = process.env[GOLDENS_WRITE_ENV] === "1";

/** Every file under dir, by its path relative to dir. */
function filesUnder(dir: string): Map<string, string> {
  const files = new Map<string, string>();
  const walk = (at: string): void => {
    for (const name of readdirSync(at).sort()) {
      const path = join(at, name);
      if (statSync(path).isDirectory()) walk(path);
      else files.set(relative(dir, path), readFileSync(path, "utf8"));
    }
  };
  if (existsSync(dir)) walk(dir);
  return files;
}

/** The seam a golden's meta.json already states, or null when there is none yet. */
function existingSeam(dir: string): unknown {
  const path = join(dir, "meta.json");
  return existsSync(path) ? (JSON.parse(readFileSync(path, "utf8")) as { seam?: unknown }).seam ?? null : null;
}

function write(dir: string, output: GoldenOutput): void {
  const seam = existingSeam(dir);
  rmSync(dir, { recursive: true, force: true });
  const put = (path: string, content: string): void => {
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, content);
  };
  put(join(dir, "frames.json"), output.frames);
  put(join(dir, "meta.json"), renderMeta(output, seam));
  for (const [path, content] of output.tree) put(join(dir, "tree", path), content);
}

describe("the scenario goldens", () => {
  it.each(GOLDEN_SCENARIOS.map((scenario) => [scenario.prompt, scenario] as const))(
    "%s — its golden is what a fresh drive produces",
    async (_prompt, scenario) => {
      // Arrange.
      const dir = join(GOLDENS_DIR, goldenName(scenario.prompt));

      // Act.
      const output = await driveGolden(scenario);

      // Assert.
      if (writing) {
        write(dir, output);
        return;
      }
      expect(readFileSync(join(dir, "frames.json"), "utf8")).toEqual(output.frames);
      expect(readFileSync(join(dir, "meta.json"), "utf8")).toEqual(renderMeta(output, existingSeam(dir)));
      expect(filesUnder(join(dir, "tree"))).toEqual(new Map(output.tree));
    },
  );

  it("holds a golden for no scenario the table does not drive", () => {
    // Arrange.
    const driven = new Set(GOLDEN_SCENARIOS.map((scenario) => goldenName(scenario.prompt)));

    // Act.
    const stray = readdirSync(GOLDENS_DIR).filter((name) => !driven.has(name));

    // Assert.
    expect(stray).toEqual([]);
  });
});
