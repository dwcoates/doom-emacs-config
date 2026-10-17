import { describe, expect, it } from "vitest";

import { loopParkFields } from "../../src/engine/loop-park.js";

describe("loopParkFields", () => {
  it("states the wait, the message and how long the loop has been parked", () => {
    // Arrange
    const park = { at: "handling" as const, message: "assistant", sinceMs: 1_000 };

    // Act
    const fields = loopParkFields(park, 1_750);

    // Assert
    expect(fields).toEqual({ parked_at: "handling", parked_message: "assistant", parked_ms: 750 });
  });

  it("states nothing when no loop was running", () => {
    // Act
    const fields = loopParkFields(undefined, 1_750);

    // Assert
    expect(fields).toEqual({});
  });
});
