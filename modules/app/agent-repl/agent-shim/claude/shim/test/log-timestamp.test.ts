/**
 * The owner suite for the shared TypeScript log timestamp renderer at
 * `agent-shim/logging/ts/timestamp.ts`, which the shim and the webapp both
 * compile. It lives in this package because the shim's program already spans
 * `agent-repl/`; the webapp asserts only that its emitted records conform.
 *
 * Every assertion is made against `proto/vocab/log-timestamp.json`, the
 * cross-language contract, so a TypeScript-side drift away from Go or elisp
 * fails here rather than in a log nobody can interleave.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { logTimestamp } from "../../../logging/ts/timestamp.js";

interface TimestampFixture {
  zone: string;
  fractional_digits: number;
  pattern: string;
  example: { instant: string; rendered_in_utc: string };
}

const fixture = JSON.parse(
  readFileSync(fileURLToPath(new URL("../../../../proto/vocab/log-timestamp.json", import.meta.url)), "utf8"),
) as TimestampFixture;

describe("the shared log timestamp renderer", () => {
  it("matches the cross-language fixture pattern", () => {
    // Arrange: a whole second, which toISOString would render with milliseconds only.
    const at = new Date(2026, 6, 28, 12, 34, 56, 0);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered).toMatch(new RegExp(fixture.pattern));
  });

  it("renders the fixture example exactly when the local zone is UTC", () => {
    // Arrange: only a UTC-zoned machine can produce the fixture's rendering,
    // so the assertion is scoped to that case rather than skipped everywhere.
    const at = new Date(fixture.example.instant);
    if (at.getTimezoneOffset() !== 0) {
      expect(logTimestamp(at)).toMatch(new RegExp(fixture.pattern));
      return;
    }

    // Act + Assert
    expect(logTimestamp(at)).toBe(fixture.example.rendered_in_utc);
  });

  it("renders the local wall clock rather than UTC", () => {
    // Arrange
    const at = new Date(2026, 6, 28, 12, 34, 56, 789);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered.slice(0, 23)).toBe("2026-07-28T12:34:56.789");
  });

  it("round-trips back to the instant it was given", () => {
    // Arrange: a wall clock written with a mismatched offset would not survive
    // this, which is what makes it a real check that the zone is the local one.
    const at = new Date(2026, 6, 28, 12, 34, 56, 789);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(new Date(rendered).getTime()).toBe(at.getTime());
  });

  it("carries a numeric offset instead of a Z suffix", () => {
    // Arrange
    const at = new Date(2026, 6, 28, 12, 34, 56, 789);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered.endsWith("Z")).toBe(false);
  });

  it("pads to the fixture's fractional width because instants resolve to milliseconds", () => {
    // Arrange
    const at = new Date(2026, 6, 28, 12, 34, 56, 7);

    // Act
    const fraction = logTimestamp(at).slice(20, 20 + fixture.fractional_digits);

    // Assert
    expect(fraction).toBe("007000");
  });
});
