import { describe, expect, it } from "vitest";
import { boundaryChange } from "../../src/engine/boundary-change.js";

describe("boundaryChange", () => {
  it("holds nothing until a change waits", () => {
    // Arrange
    const slot = boundaryChange<string, string>();
    // Act / Assert
    expect(slot.take()).toBeUndefined();
  });

  it("resolves the waiting call with what the boundary answers", async () => {
    // Arrange
    const slot = boundaryChange<string, string>();
    const waiting = slot.wait("high", "superseded");
    // Act
    const taken = slot.take();
    taken?.resolve(`applied ${taken.value}`);
    // Assert
    expect(await waiting).toBe("applied high");
  });

  it("answers an earlier waiter with the superseded answer when a later change replaces it", async () => {
    // Arrange
    const slot = boundaryChange<string, string>();
    const first = slot.wait("low", "unused");
    // Act
    void slot.wait("high", "replaced");
    // Assert
    expect([await first, slot.take()?.value]).toEqual(["replaced", "high"]);
  });

  it("empties the slot when the change is taken", () => {
    // Arrange
    const slot = boundaryChange<string, string>();
    void slot.wait("high", "superseded");
    // Act
    slot.take();
    // Assert
    expect(slot.take()).toBeUndefined();
  });
});

describe("the boundary-change call sites", () => {
  it("leaves no hand-rolled pending slot in the session engine", async () => {
    // Arrange
    const { readFileSync } = await import("node:fs");
    const source = readFileSync(new URL("../../src/engine/session.ts", import.meta.url), "utf8");
    // Act / Assert
    expect([source.includes("boundaryChange<"), /let pending(Model|Effort)\b/.test(source)]).toEqual([true, false]);
  });
});
