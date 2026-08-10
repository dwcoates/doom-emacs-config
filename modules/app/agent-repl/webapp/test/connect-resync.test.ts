import { describe, it, expect } from "vitest";
import {
  ConnectResync,
  isIdentityMismatch,
  resyncBackoffMs,
  RESYNC_BACKOFF_BASE_MS,
  RESYNC_BACKOFF_MAX_MS,
  RESYNC_FAILURE_CEILING,
  type ConnectResyncLogLevel,
  type ConnectResyncOptions,
} from "../src/connect-resync.js";

interface Sent {
  workspace: string;
  fromSeq: number;
  fence: string;
}

interface Harness {
  trigger: ConnectResync;
  sent: Sent[];
  logs: Array<[ConnectResyncLogLevel, string]>;
  /** Advance the injected clock past a backoff window. */
  advance: (ms: number) => void;
}

/**
 * Drain the microtask queue so a dispatched resync's promise settles.
 *
 * REQUIRED WHEREVER A SECOND RESYNC IS EXPECTED: only one may be in flight at
 * a time, so a want-resync raised before the first settles coalesces into the
 * dirty flag rather than going out. A test that re-arms without settling is
 * asserting the flood.
 */
async function flush(): Promise<void> {
  for (let i = 0; i < 8; i++) await Promise.resolve();
}

/** A trigger whose resync always succeeds, recording what it was asked to send. */
function snapshot(workspace: string, fromSeq: number, fence = "f-current") {
  return { workspace, fromSeq, fence };
}

function harness(resync?: (sent: Sent) => Promise<void>, onGiveUp?: ConnectResyncOptions["onGiveUp"]): Harness {
  const sent: Sent[] = [];
  const logs: Array<[ConnectResyncLogLevel, string]> = [];
  let clock = 1_000;
  const trigger = new ConnectResync({
    resync: (request) => {
      sent.push(request);
      return resync ? resync(request) : Promise.resolve();
    },
    log: (level, message) => logs.push([level, message]),
    now: () => clock,
    // No jitter, so a delay assertion names an exact number.
    random: () => 1,
    latestSnapshot: () => (sent.length === 0 ? null : sent[sent.length - 1]),
    onGiveUp,
  });
  return {
    trigger,
    sent,
    logs,
    advance: (ms) => {
      clock += ms;
    },
  };
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

  it("re-arms a resync when the daemon identity changes", async () => {
    // Arrange — this connection has already spent its one resync.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observeDaemonIdentity("boot-1");
    h.trigger.observe(true, snapshot("/ws", 40));
    await flush();
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
  it("re-arms a connection that has already spent its resync", async () => {
    // Arrange — the first resync must SETTLE before a second can go out.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
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
  let clock = 1_000;
  const trigger = new ConnectResync({
    now: () => clock,
    random: () => 1,
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
  return {
    trigger,
    sent,
    logs,
    adoptions,
    advance: (ms) => {
      clock += ms;
    },
  };
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
    // A refused resync is charged to the backoff like any other, so the clock
    // moves past that window before the re-armed request may go out.
    h.advance(RESYNC_BACKOFF_MAX_MS);
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

/**
 * THE BOUND ON THE FLOOD.
 *
 * Recovery is visibility-independent by design, which removed the accidental
 * bound WebKit's throttling used to supply: a background page re-armed a
 * resync every heartbeat whether or not the last was answered, producing a
 * command queue 5,069 deep whose entries settled 420-550 seconds after they
 * were sent. These are the four rules that make it finite.
 */
describe("ConnectResync single in-flight", () => {
  it("coalesces a want-resync raised while one is in flight", () => {
    // Arrange — a resync that never settles is what the heartbeat re-armed on.
    const h = harness(() => new Promise<void>(() => {}));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    // Act — the heartbeat fires again before the first was answered.
    h.trigger.forceResync("recovery_heartbeat");
    const dispatched = h.trigger.observe(false, snapshot("/ws", 12));
    // Assert — one request, not two.
    expect(dispatched).toBe(false);
    expect(h.sent).toHaveLength(1);
  });

  it("spends the coalesced request when the in-flight one acks", async () => {
    // Arrange
    let settle: (() => void) | undefined;
    const h = harness(
      () =>
        new Promise<void>((resolve) => {
          settle = resolve;
        }),
    );
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Act
    settle?.();
    await flush();
    // Assert — the coalesced want is honored exactly once.
    expect(h.sent).toHaveLength(2);
  });

  it("holds only one coalesced request however many wants arrive", async () => {
    // Arrange
    let settle: (() => void) | undefined;
    const h = harness(
      () =>
        new Promise<void>((resolve) => {
          settle = resolve;
        }),
    );
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    for (let i = 0; i < 20; i++) {
      h.trigger.forceResync("recovery_heartbeat");
      h.trigger.observe(false, snapshot("/ws", 12));
    }
    // Act
    settle?.();
    await flush();
    // Assert — a dirty FLAG, not a queue.
    expect(h.sent).toHaveLength(2);
  });
});

describe("ConnectResync backoff", () => {
  it("refuses a retry inside the backoff window after a failure", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    // Act — the heartbeat arrives before the window elapsed.
    h.trigger.forceResync("recovery_heartbeat");
    const dispatched = h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(dispatched).toBe(false);
  });

  it("allows the retry once the first backoff window has elapsed", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    // Act
    h.advance(RESYNC_BACKOFF_BASE_MS);
    h.trigger.forceResync("recovery_heartbeat");
    const dispatched = h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(dispatched).toBe(true);
  });

  it("grows the window with each consecutive failure", async () => {
    // Arrange — two failures in a row; the first window no longer suffices.
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    h.advance(RESYNC_BACKOFF_BASE_MS);
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    await flush();
    // Act — the SAME elapsed time that sufficed after one failure.
    h.advance(RESYNC_BACKOFF_BASE_MS);
    h.trigger.forceResync("recovery_heartbeat");
    const dispatched = h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(dispatched).toBe(false);
  });

  it("resets the window after a resync acks", async () => {
    // Arrange — one failure, then a success.
    let reject = true;
    const h = harness(() => (reject ? Promise.reject(new Error("no ack")) : Promise.resolve()));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    reject = false;
    h.advance(RESYNC_BACKOFF_BASE_MS);
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    await flush();
    h.sent.length = 0;
    // Act — no time passes at all after the success.
    h.trigger.forceResync("recovery_heartbeat");
    const dispatched = h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(dispatched).toBe(true);
  });
});

describe("resyncBackoffMs", () => {
  it("doubles from the base with each consecutive failure", () => {
    // Arrange / Act
    const second = resyncBackoffMs(2, 1);
    // Assert
    expect(second).toBe(RESYNC_BACKOFF_BASE_MS * 2);
  });

  it("caps the delay however many failures precede it", () => {
    // Arrange / Act
    const late = resyncBackoffMs(30, 1);
    // Assert
    expect(late).toBe(RESYNC_BACKOFF_MAX_MS);
  });

  it("jitters the delay down by up to half", () => {
    // Arrange / Act — a fleet that lost one daemon must not re-ask in lockstep.
    const jittered = resyncBackoffMs(1, 0);
    // Assert
    expect(jittered).toBe(RESYNC_BACKOFF_BASE_MS / 2);
  });
});

/** Drive a trigger to its give-up ceiling, one failed resync per window. */
async function exhaust(h: Harness): Promise<void> {
  for (let i = 0; i < RESYNC_FAILURE_CEILING; i++) {
    h.advance(RESYNC_BACKOFF_MAX_MS);
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
  }
}

describe("ConnectResync give-up ceiling", () => {
  it("surfaces the banner after the ceiling of consecutive failures", async () => {
    // Arrange
    const giveUps: number[] = [];
    const h = harness(
      () => Promise.reject(new Error("no ack")),
      (failures) => giveUps.push(failures),
    );
    h.trigger.onConnect();
    // Act
    await exhaust(h);
    // Assert
    expect(giveUps).toEqual([RESYNC_FAILURE_CEILING]);
  });

  it("stops asking once it has given up", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    await exhaust(h);
    h.sent.length = 0;
    // Act
    h.advance(RESYNC_BACKOFF_MAX_MS);
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(true, snapshot("/ws", 12));
    // Assert — silence, not spinning.
    expect(h.sent).toEqual([]);
  });

  it("asks again when the banner's retry affordance is used", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    await exhaust(h);
    h.sent.length = 0;
    // Act
    h.trigger.retryNow();
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });

  it("reports the given-up state so the banner is not retracted under it", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    // Act
    await exhaust(h);
    // Assert
    expect(h.trigger.isGivenUp).toBe(true);
  });
});

describe("ConnectResync reconnect", () => {
  it("resyncs immediately on a fresh socket despite an unelapsed backoff", async () => {
    // Arrange — a failure left a window this connect does not owe.
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    h.sent.length = 0;
    // Act — no time passes; the socket simply came back.
    h.trigger.onDisconnect();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });

  it("resyncs ONCE on a fresh socket rather than on every frame after it", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    h.trigger.onDisconnect();
    // Act
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    h.sent.length = 0;
    h.trigger.observe(true, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("clears a given-up page when the socket comes back", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no ack")));
    h.trigger.onConnect();
    await exhaust(h);
    // Act
    h.trigger.onDisconnect();
    h.trigger.onConnect();
    // Assert — a new socket is new evidence, not a retry.
    expect(h.trigger.isGivenUp).toBe(false);
  });

  it("drops a request whose socket died so the next connection is not blocked", () => {
    // Arrange — an in-flight resync that can never settle.
    const h = harness(() => new Promise<void>(() => {}));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    h.sent.length = 0;
    // Act
    h.trigger.onDisconnect();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });
});
