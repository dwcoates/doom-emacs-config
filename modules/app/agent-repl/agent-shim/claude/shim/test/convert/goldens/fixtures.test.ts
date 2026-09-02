/**
 * The committed captures themselves: the fixture contract, asserted.
 *
 * A golden suite is only as trustworthy as the corpus under it, so this file
 * checks the CORPUS rather than the converter: every scenario directory is
 * complete, no quarantined run was ever committed as a golden, and the manifest
 * names every scenario the suites fold.
 */
import { describe, expect, it } from "vitest";
import { existsSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { CAPTURES, captureMeta, scenarioNames, sdkMessages } from "./harness.js";

const MANIFEST = join(CAPTURES, "MANIFEST.md");

describe("the committed captures", () => {
  it("commits at least the sixty-nine scenarios the capture run produced", () => {
    expect(scenarioNames().length).toBeGreaterThanOrEqual(69);
  });

  it("never commits a quarantined or in-flight run as a golden", () => {
    for (const name of scenarioNames()) {
      expect(name).not.toMatch(/^_(failed|inflight)/);
    }
  });

  it.each(scenarioNames())("%s carries a stream", (scenario) => {
    expect(existsSync(join(CAPTURES, scenario, "stream.jsonl"))).toBe(true);
  });

  it.each(scenarioNames())("%s carries the run's meta", (scenario) => {
    expect(existsSync(join(CAPTURES, scenario, "meta.json"))).toBe(true);
  });

  it.each(scenarioNames())("%s names itself in its meta", (scenario) => {
    expect(captureMeta(scenario).scenario).toBe(scenario);
  });

  it.each(scenarioNames())("%s recorded SDK messages for the fold to consume", (scenario) => {
    expect(sdkMessages(scenario).length).toBeGreaterThan(0);
  });

  it.each(scenarioNames())("%s has a manifest row", (scenario) => {
    expect(readFileSync(MANIFEST, "utf8")).toContain(`| \`${scenario}\` |`);
  });

  it("keeps every scenario under the three-megabyte fixture ceiling", () => {
    for (const scenario of scenarioNames()) {
      expect(statSync(join(CAPTURES, scenario, "stream.jsonl")).size).toBeLessThan(3_000_000);
    }
  });
});
