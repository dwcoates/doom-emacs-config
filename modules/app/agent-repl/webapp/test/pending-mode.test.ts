import { describe, it, expect } from "vitest";
import { PendingPermissionMode } from "../src/pending-mode.js";

describe("PendingPermissionMode", () => {
  it("carries nothing outbound before any pick", () => {
    // Arrange + Act
    const pending = new PendingPermissionMode();
    // Assert
    expect(pending.outbound).toBe("");
  });

  it("carries a pick outbound so the next prompt applies it", () => {
    // Arrange
    const pending = new PendingPermissionMode();
    // Act
    pending.pick("acceptEdits");
    // Assert
    expect(pending.outbound).toBe("acceptEdits");
  });

  it("displays the live mode when nothing is pending", () => {
    // Arrange
    const pending = new PendingPermissionMode();
    // Act
    const shown = pending.settle("default");
    // Assert
    expect(shown).toBe("default");
  });

  it("keeps displaying the pick while the daemon still reports the old mode", () => {
    // Arrange
    const pending = new PendingPermissionMode();
    pending.pick("plan");
    // Act
    const shown = pending.settle("default");
    // Assert
    expect(shown).toBe("plan");
  });

  it("spends the pick once the daemon reports that mode in force", () => {
    // Arrange
    const pending = new PendingPermissionMode();
    pending.pick("plan");
    // Act
    pending.settle("plan");
    // Assert — spent, so no later prompt re-applies it.
    expect(pending.outbound).toBe("");
  });

  it("keeps sending the pick after a submit that never took effect", () => {
    // Arrange — the pick rode a prompt, but the daemon reports the old mode.
    const pending = new PendingPermissionMode();
    pending.pick("plan");
    // Act
    pending.settle("default");
    // Assert — still outbound, so the choice is not silently dropped.
    expect(pending.outbound).toBe("plan");
  });

  it("replaces an unspent pick with a newer one", () => {
    // Arrange
    const pending = new PendingPermissionMode();
    pending.pick("plan");
    // Act
    pending.pick("acceptEdits");
    // Assert
    expect(pending.outbound).toBe("acceptEdits");
  });
});
