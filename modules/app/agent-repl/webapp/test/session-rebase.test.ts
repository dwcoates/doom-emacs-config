import { describe, it, expect } from "vitest";

import { SessionRebase, claudeSessionIdOf, type SessionRebaseLogLevel } from "../src/session-rebase.js";
import type { AdapterEffect, SessionViewInput, WorkspaceStatusInput } from "../src/state-adapter.js";

/** A `session-view` effect announcing (or withholding) a vendor session uuid. */
function sessionView(claudeSessionId: string, sessionId = "s1"): AdapterEffect {
  const value: SessionViewInput = {
    workspace: "/ws",
    sessionId,
    model: "",
    slug: "",
    title: "",
    totalTokens: 0,
    totalCostUsd: 0,
    contextWindow: 0,
    permissionMode: "",
    shimAttached: true,
    claudeSessionId,
    cwd: "/ws",
    configDir: "",
    models: [],
  };
  return { kind: "session-view", value };
}

/** A `workspace-state` effect: the daemon's ruling on the owning session. */
function workspaceState(sessionId: string): AdapterEffect {
  const value: WorkspaceStatusInput = {
    workspace: "/ws",
    sessionId,
    fence: "f1",
    state: "ready",
    turnActive: false,
    liveTaskCount: 0,
    causeKind: "test",
    causeSeq: 1,
    atMs: 1,
    connectivity: "operational",
    sessionStatus: "ready",
    controllerGenerationId: "g1",
    activeFaults: [],
    mergeLeaseHeld: false,
    mergeStatus: null,
    mergeDequeueOffer: null,
  };
  return { kind: "workspace-state", value };
}

interface Harness {
  rebase: SessionRebase;
  logs: Array<[SessionRebaseLogLevel, string]>;
}

function harness(): Harness {
  const logs: Array<[SessionRebaseLogLevel, string]> = [];
  return { rebase: new SessionRebase({ log: (level, message) => logs.push([level, message]) }), logs };
}

describe("SessionRebase", () => {
  it("reads a first uuid as an adoption rather than a rotation", () => {
    // Arrange — a fresh mount's first SessionView has no retired space behind
    // it, and rebasing there would wipe the history the connect resync landed.
    const h = harness();
    // Act
    const verdict = h.rebase.observe("uuid-a");
    // Assert
    expect(verdict).toBe("adopted");
  });

  it("reads a changed uuid under a live view as a rotation", () => {
    // Arrange
    const h = harness();
    h.rebase.observe("uuid-a");
    // Act
    const verdict = h.rebase.observe("uuid-b");
    // Assert
    expect(verdict).toBe("rotated");
  });

  it("reads the same uuid re-announced as unchanged", () => {
    // Arrange — every SessionView carries the uuid, so most announcements are
    // repeats and must cost the feed nothing.
    const h = harness();
    h.rebase.observe("uuid-a");
    // Act
    const verdict = h.rebase.observe("uuid-a");
    // Assert
    expect(verdict).toBe("unchanged");
  });

  it("treats an absent uuid as silence, never as a retraction", () => {
    // Arrange — a pre-init SessionView carries no uuid at all.
    const h = harness();
    h.rebase.observe("uuid-a");
    // Act
    const verdict = h.rebase.observe("");
    // Assert
    expect([verdict, h.rebase.claudeSessionId]).toEqual(["unchanged", "uuid-a"]);
  });

  it("logs the rotation loudly, naming both uuids", () => {
    // Arrange — a rebase discards drawn conversation; unexplained is the one
    // thing it must not be.
    const h = harness();
    h.rebase.observe("uuid-a");
    // Act
    h.rebase.observe("uuid-b");
    // Assert
    expect(h.logs).toEqual([["warn", expect.stringMatching(/uuid-a -> uuid-b/) as unknown as string]]);
  });

  it("logs nothing for a first adoption", () => {
    // Arrange
    const h = harness();
    // Act
    h.rebase.observe("uuid-a");
    // Assert
    expect(h.logs).toEqual([]);
  });

  it("reads the first uuid after a forget as an adoption again", () => {
    // Arrange — swapTo rebinds onto a DIFFERENT daemon session whose store was
    // reset with the swap; its first uuid has nothing to rebase.
    const h = harness();
    h.rebase.observe("uuid-a");
    h.rebase.forget();
    // Act
    const verdict = h.rebase.observe("uuid-b");
    // Assert
    expect(verdict).toBe("adopted");
  });
});

describe("claudeSessionIdOf", () => {
  it("reads the uuid the owning session's view announces", () => {
    // Arrange
    const effects = [sessionView("uuid-a")];
    // Act
    const id = claudeSessionIdOf(effects, "s1");
    // Assert
    expect(id).toBe("uuid-a");
  });

  it("reports no uuid for a batch that announces none", () => {
    // Arrange — a conversation-only frame says nothing about identity.
    const effects: AdapterEffect[] = [{ kind: "ignored", shape: "whatever" }];
    // Act
    const id = claudeSessionIdOf(effects, "s1");
    // Assert
    expect(id).toBe("");
  });

  it("ignores a pre-init session view that carries no uuid", () => {
    // Arrange — an empty announcement must not mask the real one beside it.
    const effects = [sessionView("uuid-a"), sessionView("")];
    // Act
    const id = claudeSessionIdOf(effects, "s1");
    // Assert
    expect(id).toBe("uuid-a");
  });

  it("ignores a retired session's view in a snapshot catalog", () => {
    // Arrange — a snapshot fans out one view per session the daemon holds for
    // the workspace, retired ones included, and the dead one sorts last.
    const effects = [sessionView("uuid-live"), sessionView("uuid-dead", "s-dead")];
    // Act
    const id = claudeSessionIdOf(effects, "s1");
    // Assert
    expect(id).toBe("uuid-live");
  });

  it("takes the owner from the batch's own workspace ruling", () => {
    // Arrange — a cold snapshot: the store has ruled on nothing yet, and the
    // WorkspaceState in this very batch names the session the workspace owns.
    const effects = [workspaceState("s-live"), sessionView("uuid-dead", "s-dead"), sessionView("uuid-live", "s-live")];
    // Act
    const id = claudeSessionIdOf(effects, "");
    // Assert
    expect(id).toBe("uuid-live");
  });

  it("announces nothing when no ruling names an owner", () => {
    // Arrange — views with no WorkspaceState behind them and no owner held:
    // nothing can be shown to describe this workspace.
    const effects = [sessionView("uuid-dead", "s-dead")];
    // Act
    const id = claudeSessionIdOf(effects, "");
    // Assert
    expect(id).toBe("");
  });

  it("drops a uuid announced before a rotation in the same batch", () => {
    // Arrange — the view belongs to the session the workspace stops owning
    // three effects later.
    const effects = [sessionView("uuid-old"), workspaceState("s-new")];
    // Act
    const id = claudeSessionIdOf(effects, "s1");
    // Assert
    expect(id).toBe("");
  });
});
