import { describe, it, expect } from "vitest";
import {
  ConnectResync,
  isIdentityMismatch,
  isRetiredReplayMark,
  RESYNC_REANCHOR_CEILING,
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
  /**
   * Run the in-flight resync's settle deadline, as the browser would when the
   * ack never came. Returns whether one was armed to run.
   */
  fireDeadline: () => boolean;
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
  // The settle deadline runs on injected timers rather than the runner's, so a
  // test fires it deliberately and no suite waits out a real one.
  const pending = new Map<number, () => void>();
  let nextHandle = 1;
  const trigger = new ConnectResync({
    timers: {
      setTimeout: (callback) => {
        const handle = nextHandle++;
        pending.set(handle, callback);
        return handle;
      },
      clearTimeout: (handle) => {
        pending.delete(handle);
      },
    },
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
    fireDeadline: () => {
      const next = [...pending.entries()][0];
      if (next === undefined) return false;
      pending.delete(next[0]);
      next[1]();
      return true;
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
    // This harness exercises the refusal path, which settles on its own; the
    // deadline seam is covered against the harness above.
    fireDeadline: () => false,
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

// A fence rotation is PROOF the refusals this page collected were earned under
// an identity the daemon has since retired — not a guess that it might be
// behind. The observed failure: a bounce republishes a workspace with no
// controller generation, this page resyncs with that fence and is refused
// (identity_mismatch) a fraction of a second before the real generation is
// published. The socket never cycles and the boot id never changes, so without
// this edge nothing ever asks again and the status chrome stays frozen.
describe("ConnectResync fence rotation", () => {
  it("asks again after a rotation on a connection that never cycled", async () => {
    // Arrange — one resync already went out under the retired fence.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12, "s1|"));
    await flush();
    h.sent.length = 0;
    // Act
    h.trigger.observeFenceRotation("WorkspaceState adopted a new fence", "f-new");
    h.trigger.observe(false, snapshot("/ws", 12, "s1|g1"));
    // Assert — the new request carries the fence this page now holds.
    expect(h.sent).toEqual([snapshot("/ws", 12, "s1|g1")]);
  });

  it("does not re-arm on a rotation back to a fence it has already resynced under", async () => {
    // Arrange — one resync went out under f-a; the workspace then rotated to
    // f-b and straight back to f-a, as a flapping workspace does.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12, "f-a"));
    await flush();
    h.trigger.observeFenceRotation("rotated away", "f-b");
    h.trigger.observe(false, snapshot("/ws", 12, "f-b"));
    await flush();
    h.sent.length = 0;
    // Act — back to the fence this page has already asked under.
    h.trigger.observeFenceRotation("rotated back", "f-a");
    h.trigger.observe(false, snapshot("/ws", 12, "f-a"));
    await flush();
    // Assert — the request f-a owed was already sent and answered.
    expect(h.sent).toEqual([]);
  });

  it("bounds the resyncs a flapping fence can provoke", async () => {
    // Arrange — the self-sustaining loop, driven: fifty rotations between two
    // fences, each one the kind of edge that used to re-arm unconditionally.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12, "f-a"));
    await flush();
    h.sent.length = 0;
    // Act
    for (let i = 0; i < 50; i++) {
      const fence = i % 2 === 0 ? "f-b" : "f-a";
      h.trigger.observeFenceRotation("flap", fence);
      h.trigger.observe(false, snapshot("/ws", 12, fence));
      await flush();
    }
    // Assert — exactly one, for the one fence this page had not yet asked
    // under. The loop cannot sustain itself.
    expect(h.sent).toEqual([snapshot("/ws", 12, "f-b")]);
  });

  it("discharges a page held at its give-up ceiling by the retired fence", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("identity_mismatch")));
    h.trigger.onConnect();
    await exhaust(h);
    // Act
    h.trigger.observeFenceRotation("WorkspaceState adopted a new fence", "f-new");
    // Assert
    expect(h.trigger.isGivenUp).toBe(false);
  });

  it("leaves an in-flight request in flight, so the single-in-flight bound holds", async () => {
    // Arrange — a resync that has neither acked nor failed.
    const h = harness(() => new Promise<void>(() => {}));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12, "s1|"));
    h.sent.length = 0;
    // Act
    h.trigger.observeFenceRotation("WorkspaceState adopted a new fence", "f-new");
    h.trigger.observe(false, snapshot("/ws", 12, "s1|g1"));
    // Assert — coalesced into the dirty flag rather than sent beside it.
    expect(h.sent).toEqual([]);
  });
});

/**
 * THE BOUND ON THE LOG, which is a different flood from the bound on the
 * requests above.
 *
 * Suppressing a want-resync is the mechanism working, and it happens on
 * essentially every ingested frame: one observed boot window wrote ~28,000
 * `already in flight` lines, hundreds per millisecond, into the console AND
 * into the client_log telemetry the daemon persists. The run carries exactly
 * two facts — that coalescing started, and how big it got — so it gets exactly
 * two lines.
 */
describe("ConnectResync suppression logging", () => {
  /** Every per-suppression line this run produced, in order. */
  function coalesceLines(h: Harness): string[] {
    return h.logs.filter(([, m]) => m.includes("decision=coalesce")).map(([, m]) => m);
  }

  /** Every end-of-run summary line. */
  function summaryLines(h: Harness): string[] {
    return h.logs.filter(([, m]) => m.includes("decision=summary")).map(([, m]) => m);
  }

  /** A trigger with one resync in flight that settles only when told to. */
  function inFlight(): { h: Harness; settle: () => void } {
    let resolve: (() => void) | undefined;
    const h = harness(
      () =>
        new Promise<void>((r) => {
          resolve = r;
        }),
    );
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    return { h, settle: () => resolve?.() };
  }

  it("logs the first suppression of a run", () => {
    // Arrange
    const { h } = inFlight();
    // Act
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(coalesceLines(h)).toHaveLength(1);
  });

  it("logs nothing for the suppressions after the first", () => {
    // Arrange
    const { h } = inFlight();
    // Act — the flood: many wants against one unsettled request.
    for (let i = 0; i < 500; i++) {
      h.trigger.forceResync("recovery_heartbeat");
      h.trigger.observe(false, snapshot("/ws", 12));
    }
    // Assert — one line for five hundred suppressions.
    expect(coalesceLines(h)).toHaveLength(1);
  });

  it("reports the suppressed total when the in-flight request settles", async () => {
    // Arrange
    const { h, settle } = inFlight();
    for (let i = 0; i < 500; i++) {
      h.trigger.forceResync("recovery_heartbeat");
      h.trigger.observe(false, snapshot("/ws", 12));
    }
    // Act
    settle();
    await flush();
    // Assert
    expect(summaryLines(h)).toEqual([
      "resync: coalesced 500 want-resync(s) ws=/ws outcome=acked decision=summary",
    ]);
  });

  it("writes no summary for a request that suppressed nothing", async () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    // Assert
    expect(summaryLines(h)).toEqual([]);
  });

  it("starts a fresh run after the previous one was summarized", async () => {
    // Arrange — one run, closed out by its ack.
    const { h, settle } = inFlight();
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    settle();
    await flush();
    // Act — a want suppressed against the request the ack spent.
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert — the new run announces itself rather than staying silent.
    expect(coalesceLines(h)).toHaveLength(2);
  });

  it("reports the suppressed total when the socket dies mid-flight", () => {
    // Arrange
    const { h } = inFlight();
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Act
    h.trigger.onDisconnect();
    // Assert — a run cut short still accounts for itself.
    expect(summaryLines(h)).toEqual([
      "resync: coalesced 1 want-resync(s) ws=/ws outcome=socket_lost decision=summary",
    ]);
  });
});

/**
 * THE OTHER HALF OF THE SAME LOG BOUND, and the one production actually hit.
 *
 * A workspace whose resync keeps failing sits inside a backoff window while
 * want-resyncs keep arriving on every ingested frame. That branch used to write
 * a line per want with no rate limit at all, so the master workspace's page
 * produced 3,887 identical `backing off` lines — hundreds sharing a single
 * millisecond — which is the ~25 client_log records/second that flooded the
 * daemon. A deferral run carries the same two facts a coalescing run does.
 */
describe("ConnectResync backoff logging", () => {
  /** Every per-deferral line this run produced. */
  function deferLines(h: Harness): string[] {
    return h.logs.filter(([, m]) => m.includes("decision=defer")).map(([, m]) => m);
  }

  function summaryLines(h: Harness): string[] {
    return h.logs.filter(([, m]) => m.includes("decision=summary")).map(([, m]) => m);
  }

  /** A trigger sitting inside a backoff window after one failed resync. */
  async function backingOff(): Promise<Harness> {
    const h = harness(() => Promise.reject(new Error("server: unavailable")));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 12));
    await flush();
    return h;
  }

  it("logs the first deferral of a backoff run", async () => {
    // Arrange
    const h = await backingOff();
    // Act
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(deferLines(h)).toHaveLength(1);
  });

  it("logs nothing for the deferrals after the first", async () => {
    // Arrange
    const h = await backingOff();
    // Act — the flood: a want-resync per ingested frame inside one window.
    for (let i = 0; i < 500; i++) {
      h.trigger.forceResync("recovery_heartbeat");
      h.trigger.observe(false, snapshot("/ws", 12));
    }
    // Assert — one line for five hundred deferrals.
    expect(deferLines(h)).toHaveLength(1);
  });

  it("reports the deferred total when the backoff window expires", async () => {
    // Arrange
    const h = await backingOff();
    for (let i = 0; i < 500; i++) {
      h.trigger.forceResync("recovery_heartbeat");
      h.trigger.observe(false, snapshot("/ws", 12));
    }
    // Act — the window closes and the deferred want finally dispatches.
    h.advance(RESYNC_BACKOFF_MAX_MS);
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(summaryLines(h)).toEqual([
      "resync: coalesced 500 want-resync(s) ws=/ws outcome=dispatched decision=summary",
    ]);
  });

  it("starts a fresh deferral run after the previous one was summarized", async () => {
    // Arrange — one run, closed out by the dispatch that ended its window.
    const h = await backingOff();
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    h.advance(RESYNC_BACKOFF_MAX_MS);
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    await flush();
    // Act — the second failure opens a second window.
    h.trigger.forceResync("recovery_heartbeat");
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert — the new run announces itself rather than staying silent.
    expect(deferLines(h)).toHaveLength(2);
  });
});

/**
 * THE WEDGE THIS BOUNDS, observed live (workspace marcos-pr-remediation, daemon
 * pid 36279): a resync went out over a socket that stayed up and its ack never
 * came back. With `inFlight` cleared only by a settle or a socket event, every
 * later want-resync coalesced into a settle that never happened — no resync,
 * therefore no snapshot, therefore an expiring snapshot lease and a page
 * force-reloaded into the same wedge.
 */
describe("ConnectResync settle deadline", () => {
  /** A trigger with one resync dispatched whose promise never settles. */
  function stuck(): Harness {
    const h = harness(() => new Promise<void>(() => {}));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 4194));
    return h;
  }

  it("discharges a flight whose ack never arrives", () => {
    // Arrange
    const h = stuck();
    // Act
    h.fireDeadline();
    // Assert
    expect(h.trigger.isInFlight).toBe(false);
  });

  it("lets the next want-resync go out instead of coalescing forever", () => {
    // Arrange — the recovery heartbeat's want-resync, which used to be
    // swallowed for the life of the connection.
    const h = stuck();
    h.fireDeadline();
    // Act — past the backoff the discharge charged.
    h.advance(RESYNC_BACKOFF_MAX_MS);
    h.trigger.observe(false, snapshot("/ws", 4194));
    // Assert
    expect(h.sent).toHaveLength(2);
  });

  it("reports the discharge at error level, naming the request it dropped", () => {
    // Arrange
    const h = stuck();
    // Act
    h.fireDeadline();
    // Assert
    expect(
      h.logs.filter(
        ([level, message]) =>
          level === "error" && message.includes("unsettled after") && message.includes("from_seq=4194"),
      ),
    ).toHaveLength(1);
  });

  it("charges the discharge to the backoff, so a mute daemon is asked less often", () => {
    // Arrange
    const h = stuck();
    // Act
    h.fireDeadline();
    h.trigger.observe(false, snapshot("/ws", 4194));
    // Assert — the immediate retry is deferred, not dispatched.
    expect(h.sent).toHaveLength(1);
  });

  it("reaches the give-up ceiling, which is the loud end of the loop", async () => {
    // Arrange — a daemon that never answers must end at the banner rather than
    // at an unbounded run of discharges.
    const givenUp: Array<[number, string]> = [];
    const h = harness(() => new Promise<void>(() => {}), (failures, cause) => givenUp.push([failures, cause]));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 4194));
    // Act
    for (let i = 0; i < RESYNC_FAILURE_CEILING; i++) {
      h.fireDeadline();
      h.advance(RESYNC_BACKOFF_MAX_MS);
      h.trigger.observe(false, snapshot("/ws", 4194));
    }
    // Assert
    expect(givenUp).toHaveLength(1);
    expect(givenUp[0][1]).toContain("settle_deadline_exceeded");
  });

  it("does not fire once the flight settled normally", async () => {
    // Arrange — an acked resync must leave no armed deadline behind to charge
    // a failure to the request that follows it.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 4194));
    await flush();
    // Act + Assert
    expect(h.fireDeadline()).toBe(false);
  });
});

describe("a settle that arrives after its flight was discharged", () => {
  /** A resync the test settles by hand, after its deadline has already run. */
  function lateHarness(): { h: Harness; ack: () => void } {
    let ack = (): void => {};
    const h = harness(() => new Promise<void>((resolve) => {
      ack = resolve;
    }));
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 4194));
    h.fireDeadline();
    return { h, ack };
  }

  it("is ignored rather than applied to whatever is in flight now", async () => {
    // Arrange — the discharged request is still on the wire; nothing cancels it.
    const { h, ack } = lateHarness();
    h.advance(RESYNC_BACKOFF_MAX_MS);
    h.trigger.observe(false, snapshot("/ws", 4194));
    // Act — the ORIGINAL request finally acks, mid-flight of its successor.
    ack();
    await flush();
    // Assert — the successor still owns the flight it is in.
    expect(h.trigger.isInFlight).toBe(true);
  });

  it("is counted, because it is the evidence the deadline is what the page waits past", async () => {
    // Arrange
    const { h, ack } = lateHarness();
    // Act
    ack();
    await flush();
    // Assert
    expect(h.trigger.lateSettleCount).toBe(1);
  });

  it("is reported once, with the running count", async () => {
    // Arrange
    const { h, ack } = lateHarness();
    // Act
    ack();
    await flush();
    // Assert
    expect(
      h.logs.filter(([level, message]) => level === "warn" && message.includes("late_settles=1")),
    ).toHaveLength(1);
  });
});

// ---------------------------------------------------------------------------
// THE RETIRED REPLAY MARK: re-anchor, never re-ask.
//
// A vendor session uuid rotation restarts the conversation's store seq space at
// 1, so a page that survived the bounce holds a mark counted in a space that no
// longer exists. The daemon used to floor such a mark and serve everything above
// the floor — the WHOLE conversation, on every bounce, for every workspace —
// and now refuses it. These pin what this end does with that refusal.
// ---------------------------------------------------------------------------

/** The daemon's refusal prose for a mark counted in a retired seq space. */
const RETIRED_MARK =
  "resync rejected: the replay mark counts in a RETIRED store seq space " +
  'from_seq=1060 live_last_seq=12 rejection_cause="retired_seq_space"';

interface ReanchorHarness extends Harness {
  reanchors: string[];
}

/**
 * A trigger whose resyncs are ALWAYS refused as retired marks. `started` models
 * whether this page could actually begin a tail re-anchor.
 */
function reanchorHarness(started = true): ReanchorHarness {
  const sent: Sent[] = [];
  const logs: Array<[ConnectResyncLogLevel, string]> = [];
  const reanchors: string[] = [];
  let clock = 1_000;
  const trigger = new ConnectResync({
    resync: (request) => {
      sent.push(request);
      return Promise.reject(new Error(RETIRED_MARK));
    },
    reanchor: (cause) => {
      reanchors.push(cause);
      return started;
    },
    log: (level, message) => logs.push([level, message]),
    now: () => clock,
    random: () => 1,
  });
  return {
    trigger,
    sent,
    logs,
    reanchors,
    advance: (ms) => {
      clock += ms;
    },
    fireDeadline: () => false,
  };
}

describe("a replay mark the daemon refuses as retired", () => {
  it("is recognized by its cause token", () => {
    // Arrange / Act / Assert — the token is the stable half of the sentence.
    expect(isRetiredReplayMark(RETIRED_MARK)).toBe(true);
  });

  it("is not mistaken for an identity mismatch", () => {
    // Arrange / Act / Assert — the two refusals take opposite repairs.
    expect(isIdentityMismatch(RETIRED_MARK)).toBe(false);
  });

  it("re-anchors the page instead of retrying the mark", async () => {
    // Arrange
    const h = reanchorHarness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 1060));
    await flush();
    // Assert
    expect(h.reanchors).toHaveLength(1);
  });

  it("does not send a second resync, because the repair is a tail page", async () => {
    // Arrange — a re-anchor drops this page's mark to zero, and a page holding
    // zero asks the PAGER for a tail rather than the daemon for a delta.
    const h = reanchorHarness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 1060));
    await flush();
    // Assert
    expect(h.sent).toHaveLength(1);
  });

  it("bounds re-anchors so a page that never converges is not left cycling", async () => {
    // Arrange — every mark this page offers is refused as retired.
    const h = reanchorHarness();
    h.trigger.onConnect();
    // Act — press it far past the ceiling.
    for (let i = 0; i < RESYNC_REANCHOR_CEILING + 4; i++) {
      h.advance(RESYNC_BACKOFF_MAX_MS);
      // A real trigger re-arms between refusals (a fence rotation, a visibility
      // wake); the re-anchor deliberately does not, so the loop must supply it.
      h.trigger.forceResync("test re-arm");
      h.trigger.observe(false, snapshot("/ws", 1060));
      await flush();
    }
    // Assert
    expect(h.reanchors).toHaveLength(RESYNC_REANCHOR_CEILING);
  });

  it("charges the refusal to the backoff once the ceiling is spent", async () => {
    // Arrange
    const h = reanchorHarness();
    h.trigger.onConnect();
    // Act
    for (let i = 0; i < RESYNC_REANCHOR_CEILING + 1; i++) {
      h.advance(RESYNC_BACKOFF_MAX_MS);
      // A real trigger re-arms between refusals (a fence rotation, a visibility
      // wake); the re-anchor deliberately does not, so the loop must supply it.
      h.trigger.forceResync("test re-arm");
      h.trigger.observe(false, snapshot("/ws", 1060));
      await flush();
    }
    // Assert
    expect(
      h.logs.filter(([level, message]) => level === "error" && message.includes("not converging")),
    ).toHaveLength(1);
  });

  it("charges the refusal to the backoff when the page cannot re-anchor at all", async () => {
    // Arrange — no workspace to ask a tail page for.
    const h = reanchorHarness(false);
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 1060));
    await flush();
    // Assert — a silently-stopped page is the failure this branch refuses.
    expect(
      h.logs.filter(([, message]) => message.includes("could not start a tail re-anchor")),
    ).toHaveLength(1);
  });

  it("says the conversation is being REPLACED rather than extended", async () => {
    // Arrange
    const h = reanchorHarness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 1060));
    await flush();
    // Assert
    expect(h.logs.some(([, message]) => message.includes("REPLACED from a tail page"))).toBe(true);
  });
});

// A RETIRED MARK THE PAGE PROVES FOR ITSELF, rather than one it pays a round
// trip to be told about. The daemon's rule is `from_seq > live_last_seq`, and
// `live_last_seq` is the `through_seq` every feed batch already carries — so
// the page holds both numbers the instant the connect snapshot lands. Measured
// on a live bounce, the refusal it can derive from them arrived 1,753ms after
// it could have been derived, and was the largest single segment of the whole
// post-bounce chain.
describe("a replay mark the page itself proves retired", () => {
  it("re-anchors without sending the resync that would earn the refusal", () => {
    // Arrange — a live connection that has not yet asked for its delta.
    const h = reanchorHarness();
    h.trigger.onConnect();

    // Act — the page rules on the snapshot's head before dispatching anything.
    const took = h.trigger.observeRetiredSeqSpace(snapshot("/ws", 1060));

    // Assert — the repair is under way and NOTHING went to the daemon.
    expect(took).toBe(true);
    expect(h.reanchors).toHaveLength(1);
    expect(h.sent).toHaveLength(0);
  });

  it("needs no refusal, no heartbeat and no external force to do it", () => {
    // Arrange — nothing will drive this page but the snapshot it just ingested:
    // no settle can arrive (nothing was sent), no timer is armed, and no
    // recovery-SLO force is applied anywhere in this test.
    const h = reanchorHarness();
    h.trigger.onConnect();

    // Act
    h.trigger.observeRetiredSeqSpace(snapshot("/ws", 1060));

    // Assert — the SLO force is a safety net, and a net that is load-bearing
    // is not a net. The page repaired itself on its own evidence.
    expect(h.reanchors).toEqual(["retired_seq_space_observed_locally"]);
  });

  it("disarms the pending resync so no delta goes out under the dead mark", () => {
    // Arrange
    const h = reanchorHarness();
    h.trigger.onConnect();

    // Act — re-anchor, then let an ordinary want-resync fire as every ingested
    // frame does.
    h.trigger.observeRetiredSeqSpace(snapshot("/ws", 1060));
    h.trigger.observe(false, snapshot("/ws", 1060));

    // Assert — still nothing sent: the pager owns the repair now, and a delta
    // request would only race it to the same tail page.
    expect(h.sent).toHaveLength(0);
  });

  it("obeys the same re-anchor ceiling as the daemon's refusal does", () => {
    // Arrange — a page whose head keeps coming in below its mark.
    const h = reanchorHarness();
    h.trigger.onConnect();

    // Act — press well past the ceiling.
    let taken = 0;
    for (let i = 0; i < RESYNC_REANCHOR_CEILING + 4; i++) {
      if (h.trigger.observeRetiredSeqSpace(snapshot("/ws", 1060))) taken++;
    }

    // Assert — one bound, shared with the refusal path, because it is the same
    // decision reached from cheaper evidence.
    expect(taken).toBe(RESYNC_REANCHOR_CEILING);
    expect(h.reanchors).toHaveLength(RESYNC_REANCHOR_CEILING);
  });

  it("leaves an in-flight resync alone rather than breaking the single-flight bound", async () => {
    // Arrange — a resync is genuinely on the wire and will genuinely settle.
    const pending: Array<() => void> = [];
    const sent: Sent[] = [];
    const reanchors: string[] = [];
    const trigger = new ConnectResync({
      resync: (request) => {
        sent.push(request);
        return new Promise<void>((resolve) => {
          pending.push(resolve);
        });
      },
      reanchor: (cause) => {
        reanchors.push(cause);
        return true;
      },
    });
    trigger.onConnect();
    trigger.observe(true, snapshot("/ws", 1060));
    expect(sent).toHaveLength(1);

    // Act — the local proof lands while that request is outstanding.
    const took = trigger.observeRetiredSeqSpace(snapshot("/ws", 1060));

    // Assert — declined. The request was really sent and will really settle;
    // forgetting it here is exactly what would let two resyncs be in flight.
    expect(took).toBe(false);
    expect(reanchors).toHaveLength(0);
    for (const resolve of pending) resolve();
    await flush();
  });

  it("says nothing for a page with no workspace to name", () => {
    // Arrange — before the snapshot supplies the routing key, there is no
    // request to make and no page to re-anchor.
    const h = reanchorHarness();
    h.trigger.onConnect();

    // Act / Assert
    expect(h.trigger.observeRetiredSeqSpace(snapshot("", 1060))).toBe(false);
    expect(h.reanchors).toHaveLength(0);
  });
});
