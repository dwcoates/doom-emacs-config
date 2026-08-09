import { describe, expect, it } from "vitest";
import { captureResyncSnapshot, type ResyncSnapshotSource } from "../src/resync-snapshot.js";

function state(): ResyncSnapshotSource {
  return { cwd: "/workspace", fences: new Map([["/workspace", "f-old"]]) };
}

describe("captureResyncSnapshot", () => {
  it("connect recovery captures the current conversation watermark and fence", () => {
    // Arrange
    const current = state();
    const lastSeq = 7117;
    // Act
    const request = captureResyncSnapshot(current, lastSeq);
    (current.fences as Map<string, string>).set("/workspace", "f-new");
    // Assert — a delayed request retains the fence that authorized it.
    expect(request).toEqual({ workspace: "/workspace", fromSeq: 7117, fence: "f-old" });
  });

  it("vendor rotation captures zero with the rotated workspace fence", () => {
    // Arrange — main.ts sends zero after it adopted the rotation's WorkspaceState.
    const fences = new Map([["/workspace", "f-rotated"]]);
    const current: ResyncSnapshotSource = { cwd: "/workspace", fences };
    // Act
    const request = captureResyncSnapshot(current, 0);
    fences.set("/workspace", "f-later");
    // Assert — the rotation replay cannot be rebound to the later controller.
    expect(request).toEqual({ workspace: "/workspace", fromSeq: 0, fence: "f-rotated" });
  });

  it("a workspace the store holds no fence for yields an empty fence", () => {
    // Arrange — nothing has established what current means for this workspace.
    const current: ResyncSnapshotSource = { cwd: "/workspace", fences: new Map() };
    // Act
    const request = captureResyncSnapshot(current, 5);
    // Assert — the daemon refuses it rather than replaying against a guess.
    expect(request).toEqual({ workspace: "/workspace", fromSeq: 5, fence: "" });
  });

  it("reads the fence of its OWN workspace, not another workspace's", () => {
    // Arrange — the map is keyed by workspace; a peer's fence must not leak in.
    const current: ResyncSnapshotSource = {
      cwd: "/workspace",
      fences: new Map([
        ["/other", "f-other"],
        ["/workspace", "f-mine"],
      ]),
    };
    // Act
    const request = captureResyncSnapshot(current, 1);
    // Assert
    expect(request.fence).toBe("f-mine");
  });
});
