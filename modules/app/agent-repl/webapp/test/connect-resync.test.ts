import { describe, it, expect } from "vitest";
import { ConnectResync, isIdentityMismatch, type ConnectResyncLogLevel } from "../src/connect-resync.js";

interface Sent {
  workspace: string;
  fromSeq: number;
  fence: string;
}

interface Harness {
  trigger: ConnectResync;
  sent: Sent[];
  logs: Array<[ConnectResyncLogLevel, string]>;
}

/** A trigger whose resync always succeeds, recording what it was asked to send. */
function snapshot(workspace: string, fromSeq: number, fence = "f-current") {
  return { workspace, fromSeq, fence };
}

function harness(resync?: (sent: Sent) => Promise<void>): Harness {
  const sent: Sent[] = [];
  const logs: Array<[ConnectResyncLogLevel, string]> = [];
  const trigger = new ConnectResync({
    resync: (request) => {
      sent.push(request);
      return resync ? resync(request) : Promise.resolve();
    },
    log: (level, message) => logs.push([level, message]),
  });
  return { trigger, sent, logs };
}

describe("ConnectResync", () => {
  it("asks for history once the connect snapshot lands", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 0)]);
  });

  it("carries the store's through-seq watermark as from_seq", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 7117));
    // Assert
    expect(h.sent[0]?.fromSeq).toBe(7117);
  });

  it("does not ask again on the snapshot its own resync provokes", () => {
    // Arrange — the daemon answers a ResyncCmd with a fresh StateSnapshot.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 0));
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toHaveLength(1);
  });

  it("asks nothing before any snapshot has landed", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(false, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("holds fire while the workspace is still unknown", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("fires on the first frame that supplies the workspace after a bare snapshot", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("", 0));
    // Act
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });

  it("asks nothing before any socket has connected", () => {
    // Arrange
    const h = harness();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("asks again on a reconnect", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 0));
    h.trigger.onDisconnect();
    // Act
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 42));
    // Assert
    expect(h.sent).toEqual([
      snapshot("/ws", 0),
      snapshot("/ws", 42),
    ]);
  });

  it("asks nothing on a snapshot that arrives after the socket dropped", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.onDisconnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("reports a refused resync loudly", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no live session")));
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    await Promise.resolve();
    // Assert
    expect(h.logs.filter(([level]) => level === "error")).toHaveLength(1);
  });

  it("keeps a delayed old client's exact fence", () => {
    // Arrange — the client snapshot carries the fence that was current when
    // this webview rendered. A replacement can publish a newer fence before the
    // old webview's request reaches the daemon.
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 13, "f-old"));
    h.trigger.observe(true, snapshot("/ws", 0, "f-new"));
    // Assert — the server can now classify the delayed request as superseded;
    // it is never silently rebound to the newer controller.
    expect(h.sent).toEqual([snapshot("/ws", 13, "f-old")]);
  });
});

/**
 * ADOPT-AND-RETRY ON A CHANGED DAEMON IDENTITY — the zombie-page repair.
 *
 * Every trigger used to be a SOCKET event, so a page whose socket never cycled
 * was never told to catch up: a hidden webview whose timers WebKit froze, a
 * close that was never dispatched, or a reconnect whose snapshot landed after
 * this connection's one resync had already fired. The page then held a store
 * belonging to a daemon that no longer existed, with nothing in its own
 * lifecycle left to ask again.
 */
describe("ConnectResync daemon identity", () => {
  it("pins the first daemon boot id it is told about", () => {
    // Arrange
    const h = harness();
    // Act
    const rearmed = h.trigger.observeDaemonIdentity("boot-1");
    // Assert
    expect(rearmed).toBe(false);
    expect(h.trigger.bootId).toBe("boot-1");
  });

  it("re-arms a resync when the daemon identity changes", () => {
    // Arrange — this connection has already spent its one resync.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observeDaemonIdentity("boot-1");
    h.trigger.observe(true, snapshot("/ws", 40));
    h.sent.length = 0;
    // Act — the same socket is served a snapshot from a DIFFERENT daemon.
    h.trigger.observeDaemonIdentity("boot-2");
    h.trigger.observe(false, snapshot("/ws", 40));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 40)]);
  });

  it("asks from the applied high-water mark rather than replaying everything", () => {
    // Arrange
    const h = harness();
    h.trigger.observeDaemonIdentity("boot-1");
    // Act
    h.trigger.observeDaemonIdentity("boot-2");
    h.trigger.observe(false, snapshot("/ws", 7117));
    // Assert — a bounce that changed no build left every applied item valid.
    expect(h.sent[0]?.fromSeq).toBe(7117);
  });

  it("adopts the live identity so the resync it provokes is an ordinary match", () => {
    // Arrange
    const h = harness();
    h.trigger.observeDaemonIdentity("boot-1");
    h.trigger.observeDaemonIdentity("boot-2");
    // Act — the fresh snapshot the resync provokes carries the adopted id.
    const rearmedAgain = h.trigger.observeDaemonIdentity("boot-2");
    // Assert
    expect(rearmedAgain).toBe(false);
    expect(h.trigger.bootId).toBe("boot-2");
  });

  it("does not re-arm when the daemon identity is unchanged", () => {
    // Arrange — the ordinary case: every snapshot of one daemon's lifetime.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observeDaemonIdentity("boot-1");
    h.trigger.observe(true, snapshot("/ws", 40));
    h.sent.length = 0;
    // Act
    h.trigger.observeDaemonIdentity("boot-1");
    h.trigger.observe(false, snapshot("/ws", 40));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("refuses a snapshot that carried no daemon boot id", () => {
    // Arrange — the daemon stamps a DaemonView on every connect snapshot, so
    // an absent one is a malformed frame rather than a tolerated gap.
    const h = harness();
    // Act, Assert
    expect(() => h.trigger.observeDaemonIdentity("")).toThrow(/empty daemon boot id/);
  });
});

/**
 * THE VISIBILITY TRIGGER's half of the repair. Becoming visible is itself
 * evidence a throttled page may be behind, and the forced check takes the same
 * single dispatch path every other resync does.
 */
describe("ConnectResync forceResync", () => {
  it("re-arms a connection that has already spent its resync", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    h.sent.length = 0;
    // Act
    h.trigger.forceResync("visibilitychange_visible");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });

  it("still refuses to ask before a workspace is known", () => {
    // Arrange — Manager.Resync looks the workspace up by exact key, so an
    // empty one is a loud nack rather than a defaulted match.
    const h = harness();
    // Act
    h.trigger.forceResync("webview_focus");
    h.trigger.observe(false, snapshot("", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("asks once per force rather than on every later frame", () => {
    // Arrange
    const h = harness();
    h.trigger.forceResync("webview_focus");
    h.trigger.observe(false, snapshot("/ws", 3));
    h.sent.length = 0;
    // Act
    h.trigger.observe(false, snapshot("/ws", 3));
    // Assert
    expect(h.sent).toEqual([]);
  });
});

/**
 * THE TWO OUT-OF-BAND TRIGGERS SHARE ONE ARM. A changed daemon identity and the
 * page coming back into view both mean "this page may be behind and no socket
 * event is going to say so", and both need the snapshot precondition satisfied
 * as well as the arm — there is no connect snapshot coming for a socket that
 * never cycled, so an arm without it waits for one forever.
 */
describe("ConnectResync re-arm", () => {
  it("arms an identity change and a forced check identically", () => {
    // Arrange — two triggers, same starting point.
    const viaIdentity = harness();
    const viaForce = harness();
    viaIdentity.trigger.observeDaemonIdentity("boot-1");

    // Act.
    viaIdentity.trigger.observeDaemonIdentity("boot-2");
    viaForce.trigger.forceResync("webview_focus");
    viaIdentity.trigger.observe(false, snapshot("/ws", 12));
    viaForce.trigger.observe(false, snapshot("/ws", 12));

    // Assert — the same request from both.
    expect(viaForce.sent).toEqual(viaIdentity.sent);
  });

  // THE PRECONDITION IS THE HALF A THIRD TRIGGER WOULD FORGET. Arming alone
  // waits for a connect snapshot that is never coming on a socket that never
  // cycled, so the resync would sit armed forever and the page would simply
  // stay behind — silently. Both triggers must satisfy it without one.
  it("needs no connect snapshot to fire", () => {
    // Arrange — a trigger that has never seen a connect snapshot at all.
    const h = harness();

    // Act.
    h.trigger.forceResync("visibilitychange_visible");
    h.trigger.observe(false, snapshot("/ws", 12));

    // Assert.
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });

  // AND A FRESH SOCKET IS NOT AN OUT-OF-BAND TRIGGER. onConnect resets the
  // precondition rather than satisfying it, because a real connect snapshot IS
  // coming and firing ahead of it would ask before the store knows a workspace.
  it("leaves a fresh connection waiting for its own snapshot", () => {
    // Arrange.
    const h = harness();

    // Act.
    h.trigger.onConnect();
    h.trigger.observe(false, snapshot("/ws", 12));

    // Assert.
    expect(h.sent).toEqual([]);
  });
});

/** The daemon's refusal prose for a page naming a superseded identity. */
const MISMATCH =
  "resync rejected: command superseded by the current workspace generation " +
  "request_fence=f-old rejection_cause=identity_mismatch";

interface MismatchHarness extends Harness {
  adoptions: string[];
}

/**
 * A trigger whose FIRST resync is refused for identity mismatch. `adopted` is
 * the live identity the store hands back at the retry edge; null models a page
 * that cannot yet name one.
 */
function mismatchHarness(adopted: Sent | null, alwaysReject = false): MismatchHarness {
  const sent: Sent[] = [];
  const logs: Array<[ConnectResyncLogLevel, string]> = [];
  const adoptions: string[] = [];
  const trigger = new ConnectResync({
    resync: (request) => {
      sent.push(request);
      if (alwaysReject || sent.length === 1) return Promise.reject(new Error(MISMATCH));
      return Promise.resolve();
    },
    log: (level, message) => logs.push([level, message]),
    adoptIdentity: (rejection) => {
      adoptions.push(rejection);
      return adopted;
    },
  });
  return { trigger, sent, logs, adoptions };
}

describe("ConnectResync identity mismatch", () => {
  it("retries once with the adopted live identity", async () => {
    // Arrange
    const h = mismatchHarness(snapshot("/ws", 12, "f-live"));
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 12, "f-stale"));
    for (let i = 0; i < 4; i++) await Promise.resolve();
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12, "f-stale"), snapshot("/ws", 12, "f-live")]);
  });

  it("re-reads the identity only after a mismatch refusal", async () => {
    // Arrange
    const h = mismatchHarness(snapshot("/ws", 12, "f-live"));
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 12, "f-stale"));
    for (let i = 0; i < 4; i++) await Promise.resolve();
    // Assert
    expect(h.adoptions).toEqual([`Error: ${MISMATCH}`]);
  });

  it("does not retry a second time when the adopted identity is also superseded", async () => {
    // Arrange — every send is refused, so the retry mismatches too.
    const h = mismatchHarness(snapshot("/ws", 12, "f-live"), true);
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 12, "f-stale"));
    for (let i = 0; i < 8; i++) await Promise.resolve();
    // Assert
    expect(h.sent.length).toBe(2);
  });

  it("re-arms for the next snapshot when there is no live identity to adopt", async () => {
    // Arrange
    const h = mismatchHarness(null);
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12, "f-stale"));
    for (let i = 0; i < 4; i++) await Promise.resolve();
    // Act — the next snapshot carries the daemon's own account of who is live.
    h.trigger.observe(true, snapshot("/ws", 12, "f-live"));
    // Assert
    expect(h.sent[1]).toEqual(snapshot("/ws", 12, "f-live"));
  });

  it("does not retry an ordinary refusal that is not an identity mismatch", async () => {
    // Arrange
    const sent: Sent[] = [];
    const trigger = new ConnectResync({
      resync: (request) => {
        sent.push(request);
        return Promise.reject(new Error("resync rejected: workspace unknown"));
      },
      adoptIdentity: () => snapshot("/ws", 12, "f-live"),
    });
    trigger.onConnect();
    // Act
    trigger.observe(true, snapshot("/ws", 12, "f-stale"));
    for (let i = 0; i < 4; i++) await Promise.resolve();
    // Assert
    expect(sent.length).toBe(1);
  });
});

describe("isIdentityMismatch", () => {
  it("recognizes the daemon's rejection cause token", () => {
    // Arrange / Act
    const verdict = isIdentityMismatch(MISMATCH);
    // Assert
    expect(verdict).toBe(true);
  });

  it("does not claim an unrelated refusal", () => {
    // Arrange / Act
    const verdict = isIdentityMismatch("resync rejected: workspace unknown");
    // Assert
    expect(verdict).toBe(false);
  });
});
