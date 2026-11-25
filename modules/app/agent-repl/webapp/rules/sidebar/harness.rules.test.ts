/**
 * THE SIDEBAR TESTS READ A SECTION'S LIST THROUGH THE HARNESS'S ONE SPELLING
 * (`drawnEntries`, test/sidebar/harness.ts): the sidebar test files read for a
 * hand-rolled copy.
 */
import { describe, expect, it } from "vitest";

describe("drawnEntries", () => {
  it("is the one spelling the sidebar tests read a section's list by", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const sources = import.meta.glob(["../../test/sidebar/*.test.ts", "./*.test.ts"], { query: "?raw", import: "default", eager: true }) as Record<string, string>;

    // Act
    const handRolled = Object.entries(sources)
      .filter(([, source]) => /getAttribute\("data-roster-row"\) \?\? `\[\$\{el\.getAttribute\("data-subsection"\)/.test(source))
      .map(([file]) => file);

    // Assert
    expect(handRolled).toEqual([]);
  });
});
