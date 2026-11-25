/**
 * The rule cases of `src/rpc/streams.ts`: a stream's reopen waits through
 * its one backoff helper.
 */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

describe("watchStream's reopen waits", () => {
  it("every backoff wait goes through the one helper", () => {
    // Arrange
    const source = readFileSync(new URL("../../src/rpc/streams.ts", import.meta.url), "utf8");

    // Act
    const waits = source.match(/await waitBackoff\(/g) ?? [];

    // Assert
    expect(waits.length).toBe(1);
  });
});
