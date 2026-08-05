import { describe, expect, it } from "vitest";
import { captureResyncSnapshot, type ResyncSnapshotSource } from "../src/resync-snapshot.js";

function state(): ResyncSnapshotSource {
  return { cwd: "/workspace", sessionId: "s-old", controllerGenerationId: "g-old" };
}

describe("captureResyncSnapshot", () => {
  it("connect recovery captures the current conversation watermark and identity", () => {
    // Arrange
    const current = state();
    const lastSeq = 7117;
    // Act
    const request = captureResyncSnapshot(current, lastSeq);
    current.sessionId = "s-new";
    current.controllerGenerationId = "g-new";
    // Assert — a delayed request retains the identity that authorized it.
    expect(request).toEqual({
      workspace: "/workspace",
      fromSeq: 7117,
      sessionId: "s-old",
      controllerGenerationId: "g-old",
    });
  });

  it("vendor rotation captures zero with the rotated workspace identity", () => {
    // Arrange — main.ts sends zero after it adopted the rotation's WorkspaceState.
    const current = { cwd: "/workspace", sessionId: "s-rotated", controllerGenerationId: "g-rotated" };
    // Act
    const request = captureResyncSnapshot(current, 0);
    current.sessionId = "s-later";
    current.controllerGenerationId = "g-later";
    // Assert — the rotation replay cannot be rebound to the later controller.
    expect(request).toEqual({
      workspace: "/workspace",
      fromSeq: 0,
      sessionId: "s-rotated",
      controllerGenerationId: "g-rotated",
    });
  });
});
