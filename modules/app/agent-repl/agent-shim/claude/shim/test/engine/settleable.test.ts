/** engine/settleable.ts — a promise and its resolver, held together. */
import { describe, expect, it } from "vitest";
import { settleable } from "../../src/engine/settleable.js";

describe("settleable", () => {
  it("settles its promise with the value it is resolved with", async () => {
    // Arrange
    const pending = settleable<string>();

    // Act
    pending.resolve("landed");

    // Assert
    await expect(pending.promise).resolves.toBe("landed");
  });

  it("keeps its first value when resolved again", async () => {
    // Arrange
    const pending = settleable<string>();

    // Act
    pending.resolve("first");
    pending.resolve("second");

    // Assert
    await expect(pending.promise).resolves.toBe("first");
  });

  it("stays pending until it is resolved", async () => {
    // Arrange
    const pending = settleable();
    let settled = false;
    void pending.promise.then(() => {
      settled = true;
    });

    // Act
    await Promise.resolve();

    // Assert
    expect(settled).toBe(false);
  });
});
