import { describe, it, expect } from "vitest";

import { SessionRebase, claudeSessionIdOf, type SessionRebaseLogLevel } from "../src/session-rebase.js";
import type { AdapterEffect, SessionViewInput } from "../src/state-adapter.js";

/** A `session-view` effect announcing (or withholding) a vendor session uuid. */
function sessionView(claudeSessionId: string): AdapterEffect {
  const value: SessionViewInput = {
    workspace: "/ws",
    sessionId: "s1",
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
  };
  return { kind: "session-view", value };
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
  it("reads the uuid a session-view effect announces", () => {
    // Arrange
    const effects = [sessionView("uuid-a")];
    // Act
    const id = claudeSessionIdOf(effects);
    // Assert
    expect(id).toBe("uuid-a");
  });

  it("reports no uuid for a batch that announces none", () => {
    // Arrange — a conversation-only frame says nothing about identity.
    const effects: AdapterEffect[] = [{ kind: "ignored", shape: "whatever" }];
    // Act
    const id = claudeSessionIdOf(effects);
    // Assert
    expect(id).toBe("");
  });

  it("ignores a pre-init session view that carries no uuid", () => {
    // Arrange — an empty announcement must not mask the real one beside it.
    const effects = [sessionView("uuid-a"), sessionView("")];
    // Act
    const id = claudeSessionIdOf(effects);
    // Assert
    expect(id).toBe("uuid-a");
  });
});
