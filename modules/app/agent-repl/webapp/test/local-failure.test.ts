/**
 * local-failure — the webapp's OWN classifier, covering the one category it is
 * allowed to classify (its own machinery, the daemon's unreachability chief
 * among it) and the arm-band partition that keeps it from classifying anything
 * else. One edge per test.
 */
import { describe, expect, it } from "vitest";
import {
  CLIENT_FAILURE_ARMS,
  bootFailedFailure,
  clientFailureUuid,
  commandRejectionUnclassifiedFailure,
  commandUnsentFailure,
  controlPlaneFailure,
  daemonReachableFailure,
  daemonUnreachableFailure,
  frameUndecodableFailure,
  isClientArm,
  staleBundleFailure,
  workspaceGoneFailure,
} from "../src/local-failure.js";
import { failureKindName, failureSide } from "../src/failure-card.js";

describe("the arm-band partition", () => {
  it.each(CLIENT_FAILURE_ARMS)("counts %s as an arm this frontend may mint", (arm) => {
    // Arrange / Act — a daemon arm minted from this side would mean the
    // frontend re-classified something the daemon already decided.
    // Assert
    expect(isClientArm(arm)).toBe(true);
  });

  it("rejects a daemon arm as one of its own", () => {
    // Arrange / Act — the other direction: `errors.proto` splits the vocabulary
    // by field number, and neither producer may set the other's arms.
    // Assert
    expect(isClientArm("shimRejected")).toBe(false);
  });
});

describe("locally-classified failures", () => {
  it.each(CLIENT_FAILURE_ARMS)("puts %s on the machinery side", (arm) => {
    // Arrange — a transport fault implicates nothing about the account, so a
    // vendor-side local failure would be this end guessing at something only
    // the daemon can see.
    const byArm: Record<string, () => { view: { kind: import("../src/frontend-proto.js").FailureKind } }> = {
      daemonUnreachable: () => daemonUnreachableFailure(1006, ""),
      workspaceGone: () => workspaceGoneFailure("/ws"),
      bootFailed: () => bootFailedFailure(new Error("boom")),
      controlPlaneFailed: () => controlPlaneFailure("the login request", new Error("boom")),
      frameUndecodable: () => frameUndecodableFailure(new Error("bad"), "{"),
      staleBundle: () => staleBundleFailure("lease mismatch"),
      commandUnsent: () => commandUnsentFailure("submitPrompt"),
      commandRejectionUnclassified: () =>
        commandRejectionUnclassifiedFailure("hibernateWorkspace", "no"),
    };
    // Act
    const card = byArm[arm]();
    // Assert
    expect(failureSide(card.view.kind)).toBe("machinery");
  });

  it("derives its uuid from the arm, so a repeat reconciles in place", () => {
    // Arrange / Act — a reconnect loop appending a card per attempt would bury
    // the feed under its own alarm.
    const first = daemonUnreachableFailure(1006, "");
    const second = daemonUnreachableFailure(1006, "again");
    // Assert
    expect(first.uuid).toBe(second.uuid);
  });

  it("gives two DIFFERENT conditions different uuids", () => {
    // Arrange / Act
    // Assert
    expect(daemonUnreachableFailure(1006, "").uuid).not.toBe(staleBundleFailure("x").uuid);
  });
});

describe("the daemon-unreachable window", () => {
  it("names a clean close as the daemon closing the connection", () => {
    // Arrange / Act — a 1000 is a shutting-down server, not a network drop, and
    // reporting both as one thing is what made "reconnecting…" the answer to
    // every transport fault.
    const card = daemonUnreachableFailure(1000, "bye");
    // Assert
    expect(card.view.message).toBe("the daemon closed the connection; reconnecting");
  });

  it("names an abnormal close as a lost connection", () => {
    // Arrange / Act
    const card = daemonUnreachableFailure(1006, "");
    // Assert
    expect(card.view.message).toBe("lost the connection to the daemon; reconnecting");
  });

  it("carries the close code as the arm's own typed evidence", () => {
    // Arrange / Act — the code is the only thing distinguishing the two, so it
    // rides the arm rather than being flattened into prose alone.
    const card = daemonUnreachableFailure(1006, "");
    // Assert
    expect(card.view.kind.kind).toEqual({
      case: "daemonUnreachable",
      value: expect.objectContaining({ closeCode: 1006 }),
    });
  });

  it("opens while the link is down", () => {
    // Arrange / Act
    const card = daemonUnreachableFailure(1006, "");
    // Assert
    expect(card.view.lifecycle).toEqual({ case: "open" });
  });

  it("resolves with the stamp when the socket comes back", () => {
    // Arrange / Act — a retraction, not a card: the store takes the open one
    // down rather than settling it in place.
    const card = daemonReachableFailure(1700000000000);
    // Assert
    expect(card.view.lifecycle).toEqual({ case: "resolved", resolvedAtMs: 1700000000000 });
  });

  it("retracts onto the SAME uuid the open card holds", () => {
    // Arrange / Act
    // Assert
    expect(daemonReachableFailure(1).uuid).toBe(daemonUnreachableFailure(1006, "").uuid);
  });
});

describe("the terminal client failures", () => {
  it("never offers a workspace-gone card a resolution", () => {
    // Arrange / Act — unlike a dropped connection, there is nothing to come
    // back, so an "open" card would invite a wait that never ends.
    const card = workspaceGoneFailure("/ws");
    // Assert
    expect(card.view.lifecycle).toEqual({ case: "terminal" });
  });

  it("never offers a stale-bundle card a resolution", () => {
    // Arrange / Act — deliberately unresolvable: a self-clearing version would
    // hide a page that is silently wrong.
    const card = staleBundleFailure("lease mismatch");
    // Assert
    expect(card.view.lifecycle).toEqual({ case: "terminal" });
  });
});

describe("the control-plane failure", () => {
  it("names the action in the user's terms", () => {
    // Arrange / Act — "POST /accounts/switch failed" explains nothing to the
    // person who clicked a menu item.
    const card = controlPlaneFailure("the account switch", new Error("boom"));
    // Assert
    expect(card.view.message).toBe("the account switch failed");
  });

  it("carries the thrown cause as the evidence", () => {
    // Arrange / Act
    const card = controlPlaneFailure("the login request", new Error("boom"));
    // Assert
    expect(card.view.detail).toBe("boom");
  });

  it("keys two DIFFERENT actions onto two cards", () => {
    // Arrange / Act — keying every control-plane failure alike would let a
    // failed login overwrite a failed remediation.
    const login = controlPlaneFailure("the login request", new Error("a"));
    const account = controlPlaneFailure("the account switch", new Error("b"));
    // Assert
    expect(login.uuid).not.toBe(account.uuid);
  });
});

describe("the undecodable-frame failure", () => {
  it("leads with the consequence rather than the decoder's complaint", () => {
    // Arrange / Act — the decoder's words are evidence, not prose.
    const card = frameUndecodableFailure(new Error("bad json"), "");
    // Assert
    expect(card.view.message).toBe(
      "a message from the daemon could not be read and was skipped",
    );
  });

  it("puts the frame head beside the cause in the evidence", () => {
    // Arrange / Act
    const card = frameUndecodableFailure(new Error("bad json"), '{"snap');
    // Assert
    expect(card.view.detail).toBe('bad json — frame head: {"snap');
  });

  it("carries the frame head on the arm as typed evidence", () => {
    // Arrange / Act
    const card = frameUndecodableFailure(new Error("bad json"), '{"snap');
    // Assert
    expect(card.view.kind.kind).toEqual({
      case: "frameUndecodable",
      value: expect.objectContaining({ frameHead: '{"snap' }),
    });
  });
});

describe("the refusal failures", () => {
  it("says outright that it could not classify a bare refusal", () => {
    // Arrange / Act — this end names the refusal rather than picking a kind on
    // the daemon's behalf.
    const card = commandRejectionUnclassifiedFailure("hibernateWorkspace", "no lease");
    // Assert
    expect(failureKindName(card.view.kind)).toBe("commandRejectionUnclassified");
  });

  it("leads with the daemon's own words verbatim", () => {
    // Arrange / Act — it decided the refusal, and its sentence is the closest
    // thing to an account there is.
    const card = commandRejectionUnclassifiedFailure("hibernateWorkspace", "no lease");
    // Assert
    expect(card.view.message).toBe("no lease");
  });

  it("falls back to naming the command when the daemon gave no words", () => {
    // Arrange / Act
    const card = commandRejectionUnclassifiedFailure("hibernateWorkspace", "");
    // Assert
    expect(card.view.message).toBe("hibernateWorkspace was refused");
  });

  it("distinguishes a command that never left the page from a refusal", () => {
    // Arrange / Act — nothing was decided, so the operation can be retried.
    const card = commandUnsentFailure("submitPrompt");
    // Assert
    expect(failureKindName(card.view.kind)).toBe("commandUnsent");
  });

  it("keys an unsent command by the command, so two stay two cards", () => {
    // Arrange / Act
    // Assert
    expect(commandUnsentFailure("interrupt").uuid).toBe(
      clientFailureUuid("commandUnsent", "interrupt"),
    );
  });
});
