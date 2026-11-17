/**
 * The owner suite for the shared TypeScript warnings-sink contract at
 * `agent-shim/logging/ts/warnings.ts`, asserted against
 * `proto/vocab/log-warnings-sink.json`, the cross-language contract Go and
 * elisp answer too.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { isWarningsLevel, warningsPath, warningsSink, type RecordLevel } from "../../../logging/ts/warnings.js";

interface WarningsFixture {
  warnings_levels: RecordLevel[];
  log_levels: RecordLevel[];
  paths: { path: string; warnings: string }[];
  sinks: { name: string; warnings: string }[];
}

const fixture = JSON.parse(
  readFileSync(fileURLToPath(new URL("../../../../proto/vocab/log-warnings-sink.json", import.meta.url)), "utf8"),
) as WarningsFixture;

describe("the shared warnings-sink contract", () => {
  it.each(fixture.warnings_levels)("routes a %s record to the warnings file", (level) => {
    // Arrange / Act / Assert
    expect(isWarningsLevel(level)).toBe(true);
  });

  it.each(fixture.log_levels)("routes a %s record to the log", (level) => {
    // Arrange / Act / Assert
    expect(isWarningsLevel(level)).toBe(false);
  });

  it("refuses an unknown level", () => {
    // Arrange / Act / Assert
    expect(() => isWarningsLevel("fatal" as RecordLevel)).toThrow('invalid level "fatal"');
  });

  it.each(fixture.paths.map((c) => [c.path, c.warnings] as const))("names the warnings file of %s", (path, want) => {
    // Arrange / Act / Assert
    expect(warningsPath(path)).toBe(want);
  });

  it.each(fixture.sinks.map((c) => [c.name, c.warnings] as const))("names the warnings twin of the %s sink", (name, want) => {
    // Arrange / Act / Assert
    expect(warningsSink(name)).toBe(want);
  });
});
