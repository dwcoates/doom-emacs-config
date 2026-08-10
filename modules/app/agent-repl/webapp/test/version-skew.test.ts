import { describe, it, expect } from "vitest";

import {
  FORCED_RELOAD_AT_KEY,
  FORCED_RELOAD_COOLDOWN_MS,
  FORCED_RELOAD_COUNT_KEY,
  HEALTHY_UPTIME_MS,
  MAX_FORCED_RELOADS,
  STALE_BUNDLE_EXPIRY_CYCLES,
  type DaemonBuild,
  VersionSkewGuard,
  type VersionSkewLogLevel,
} from "../src/version-skew.js";
import { DEFAULT_SNAPSHOT_TIMEOUT_MS } from "../src/ws.js";


/**
 * A daemon build named by its version string. The mtime is fixed at 1 for
 * every build here: these tests exercise the guard's DECISION mechanics, and a
 * second varying field would only obscure which one drove an outcome. The
 * empty name is the malformed frame, whose mtime is 0 too.
 */
function buildOf(version: string): DaemonBuild {
  return { version, binaryMtimeMs: version === "" ? 0 : 1 };
}

/** A `Storage` backed by a Map, so a test never touches the runner's own. */
class FakeStorage implements Storage {
  private readonly items = new Map<string, string>();
  /** When set, every access throws it — the sandboxed-iframe failure. */
  throwOn: Error | null = null;

  get length(): number {
    return this.items.size;
  }
  clear(): void {
    this.items.clear();
  }
  getItem(key: string): string | null {
    if (this.throwOn !== null) throw this.throwOn;
    return this.items.get(key) ?? null;
  }
  key(index: number): string | null {
    return [...this.items.keys()][index] ?? null;
  }
  removeItem(key: string): void {
    this.items.delete(key);
  }
  setItem(key: string, value: string): void {
    if (this.throwOn !== null) throw this.throwOn;
    this.items.set(key, value);
  }
}

interface Harness {
  guard: VersionSkewGuard;
  storage: FakeStorage;
  logs: Array<[VersionSkewLogLevel, string]>;
  reloads: number;
  refusals: string[];
  /** Deferred reloads waiting for a `settle()`. */
  deferred: Array<() => void>;
  settle: () => void;
  now: number;
}

/**
 * A guard whose reload is deferred into `deferred` until `settle()` runs it.
 *
 * STORAGE and START are parameters because a page RELOAD is modelled as a new
 * guard over the SAME session storage at a later clock — that pair is the only
 * memory a reloaded page inherits, and the cross-reload bounds are exactly the
 * thing under test.
 */
function harness(storage: FakeStorage = new FakeStorage(), startedAtMs = 1_000_000): Harness {
  const h: Harness = {
    guard: undefined as unknown as VersionSkewGuard,
    storage,
    logs: [],
    reloads: 0,
    refusals: [],
    deferred: [],
    settle: () => {
      const pending = h.deferred.splice(0);
      for (const fn of pending) fn();
    },
    now: startedAtMs,
  };
  h.guard = new VersionSkewGuard({
    reload: () => {
      h.reloads++;
    },
    storage,
    onReloadRefused: (detail) => h.refusals.push(detail),
    now: () => h.now,
    defer: (fn) => h.deferred.push(fn),
    log: (level, message) => h.logs.push([level, message]),
  });
  return h;
}

describe("boot-id pinning", () => {
  it("pins the first boot id it successfully adopts", () => {
    // Arrange — a fresh page belongs to whichever daemon process it first
    // managed to ingest, and that adoption is never a skew.
    const h = harness();
    // Act
    const outcome = h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Assert
    expect(outcome).toBe("pinned");
    expect(h.guard.build?.version).toBe("boot-a");
  });

  it("never reloads on the first adoption", () => {
    // Arrange
    const h = harness();
    // Act
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    h.settle();
    // Assert — there is no retired daemon behind a first pin.
    expect(h.reloads).toBe(0);
  });

  it("rejects an adopted snapshot that carried no daemon build identity", () => {
    // Arrange — the daemon stamps a DaemonView on every connect snapshot, so
    // an absent one is a malformed frame rather than an older daemon.
    const h = harness();
    // Act + Assert
    expect(() => h.guard.observeSnapshotAdoption(buildOf(""))).toThrow(/empty daemon build identity/);
  });

  it("logs the missing build identity at error level before throwing", () => {
    // Arrange
    const h = harness();
    // Act
    expect(() => h.guard.observeSnapshotAdoption(buildOf(""))).toThrow();
    // Assert — the canonical channel carries the diagnosis, not only the stack.
    expect(h.logs).toContainEqual([
      "error",
      "version skew: adopted snapshot carried no daemon build identity, which the daemon stamps on every connect snapshot",
    ]);
  });
});

describe("a changed daemon build", () => {
  it("reloads the page for a fresh bundle", () => {
    // Arrange — a redeployed daemon may be serving a new bundle, and this
    // page's own code is the only part a reconnect cannot refresh.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act
    const outcome = h.guard.observeSnapshotAdoption(buildOf("boot-b"));
    h.settle();
    // Assert
    expect(outcome).toBe("reloaded");
    expect(h.reloads).toBe(1);
  });

  it("names both boot ids in the log that precedes the reload", () => {
    // Arrange — an unexplained reload is indistinguishable from a crash.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act
    h.guard.observeSnapshotAdoption(buildOf("boot-b"));
    // Assert
    expect(h.logs).toContainEqual([
      "warn",
      "version skew: daemon build changed boot-a@1 -> boot-b@1; reloading page for a fresh bundle",
    ]);
  });

  it("defers the reload rather than tearing the page down mid-ingest", () => {
    // Arrange
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act — no settle(): the caller's frame is still on the stack.
    h.guard.observeSnapshotAdoption(buildOf("boot-b"));
    // Assert
    expect(h.reloads).toBe(0);
    expect(h.deferred).toHaveLength(1);
  });

  it("schedules one reload however many skewed snapshots arrive", () => {
    // Arrange — snapshots keep arriving every 15s while the reload is pending.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    h.guard.observeSnapshotAdoption(buildOf("boot-b"));
    // Act
    const second = h.guard.observeSnapshotAdoption(buildOf("boot-c"));
    h.settle();
    // Assert
    expect(second).toBe("none");
    expect(h.reloads).toBe(1);
  });

  it("spends no forced-reload budget, which belongs to the expiry trigger", () => {
    // Arrange — boot-id skew cannot loop on its own, so it must not consume the
    // cooldown that protects the presumptive trigger.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act
    h.guard.observeSnapshotAdoption(buildOf("boot-b"));
    // Assert
    expect(h.storage.getItem(FORCED_RELOAD_AT_KEY)).toBeNull();
  });
});

describe("an unchanged daemon boot id", () => {
  it("never reloads, however many snapshots repeat it", () => {
    // Arrange — a snapshot lands every 15 seconds and all of them repeat the id.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act
    const outcome = h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    h.settle();
    // Assert
    expect(outcome).toBe("none");
    expect(h.reloads).toBe(0);
  });
});

describe("consecutive snapshot-lease expiries", () => {
  it("tolerates a single expiry, which is an ordinary restart window", () => {
    // Arrange — the daemon being down longer than the lease is not skew.
    const h = harness();
    // Act
    const outcome = h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(outcome).toBe("none");
    expect(h.reloads).toBe(0);
  });

  it("reloads once the cycle budget is spent with no adoption between", () => {
    // Arrange — a page served snapshots it still cannot adopt is stale code.
    const h = harness();
    // Act
    let outcome: string = "none";
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) outcome = h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(outcome).toBe("reloaded");
    expect(h.reloads).toBe(1);
  });

  it("stamps the forced reload so a later one meets the cooldown", () => {
    // Arrange
    const h = harness();
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Assert
    expect(h.storage.getItem(FORCED_RELOAD_AT_KEY)).toBe(String(h.now));
  });

  it("counts consecutively, so an adoption between expiries clears the evidence", () => {
    // Arrange — adoption is the only proof this bundle can ingest this daemon.
    const h = harness();
    h.guard.observeLeaseExpiry();
    h.guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act
    const outcome = h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(outcome).toBe("none");
    expect(h.reloads).toBe(0);
  });
});

describe("the forced-reload cooldown", () => {
  it("refuses a second forced reload inside the cooldown window", () => {
    // Arrange — a page reloaded 30s ago whose fresh bundle still cannot adopt.
    const h = harness();
    h.storage.setItem(FORCED_RELOAD_AT_KEY, String(h.now - 30_000));
    // Act
    let outcome: string = "none";
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) outcome = h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(outcome).toBe("refused");
    expect(h.reloads).toBe(0);
  });

  it("surfaces the refusal to the user instead of churning silently", () => {
    // Arrange
    const h = harness();
    h.storage.setItem(FORCED_RELOAD_AT_KEY, String(h.now - 30_000));
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Assert — the detail says how recently the reload it is refusing happened.
    expect(h.refusals).toHaveLength(1);
    expect(h.refusals[0]).toContain("reloaded 30s ago");
  });

  it("logs the refusal at error level, because it means a real defect", () => {
    // Arrange
    const h = harness();
    h.storage.setItem(FORCED_RELOAD_AT_KEY, String(h.now - 30_000));
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Assert
    expect(h.logs.filter(([level, message]) => level === "error" && message.includes("refusing a second forced reload"))).toHaveLength(1);
  });

  it("resets the cycle count on a refusal, so one card costs a full run of expiries", () => {
    // Arrange — a refusal per lease would bury the feed under its own alarm.
    const h = harness();
    h.storage.setItem(FORCED_RELOAD_AT_KEY, String(h.now - 30_000));
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Act
    const next = h.guard.observeLeaseExpiry();
    // Assert
    expect(next).toBe("none");
    expect(h.refusals).toHaveLength(1);
  });

  it("refuses a second forced reload one full expiry-cycle RUN after the first", () => {
    // Arrange — the reachability regression. A wedged page takes
    // STALE_BUNDLE_EXPIRY_CYCLES leases to force its next reload, so a cooldown
    // shorter than that run could never be met and the refusal card, which is
    // the diagnosis, never fired.
    const h = harness();
    const runMs = STALE_BUNDLE_EXPIRY_CYCLES * DEFAULT_SNAPSHOT_TIMEOUT_MS;
    h.storage.setItem(FORCED_RELOAD_AT_KEY, String(h.now - runMs));
    // Act
    let outcome: string = "none";
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) outcome = h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(outcome).toBe("refused");
    expect(h.reloads).toBe(0);
  });

  it("keeps the cooldown strictly longer than one full expiry-cycle run", () => {
    // Arrange / Act / Assert — the derived constant's whole invariant, asserted
    // on the relation rather than on the 135_000 it happens to evaluate to.
    expect(FORCED_RELOAD_COOLDOWN_MS).toBeGreaterThan(
      STALE_BUNDLE_EXPIRY_CYCLES * DEFAULT_SNAPSHOT_TIMEOUT_MS,
    );
  });

  it("allows the reload once the window has elapsed", () => {
    // Arrange — a stale stamp from an earlier, unrelated skew.
    const h = harness();
    h.storage.setItem(FORCED_RELOAD_AT_KEY, String(h.now - FORCED_RELOAD_COOLDOWN_MS));
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(h.reloads).toBe(1);
    expect(h.refusals).toEqual([]);
  });
});

describe("session-storage failure", () => {
  it("surfaces a read that throws rather than reloading unguarded", () => {
    // Arrange — a sandboxed webview denies storage access outright.
    const h = harness();
    h.storage.throwOn = new Error("SecurityError");
    // Act + Assert — the guard cannot hold, so the reload must not happen.
    expect(() => {
      for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    }).toThrow(/SecurityError/);
    expect(h.reloads).toBe(0);
  });

  it("logs the storage read failure through the canonical channel", () => {
    // Arrange
    const h = harness();
    h.storage.throwOn = new Error("SecurityError");
    // Act
    expect(() => {
      for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    }).toThrow();
    // Assert
    expect(
      h.logs.filter(([level, message]) => level === "error" && message.includes("from session storage failed")),
    ).toHaveLength(1);
  });

  it("surfaces a write that throws, since a lost stamp is a lost cooldown", () => {
    // Arrange — reads succeed (no stamp yet) and only the write is denied.
    const h = harness();
    const storage = h.storage;
    storage.setItem = () => {
      throw new Error("QuotaExceededError");
    };
    // Act + Assert
    expect(() => {
      for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    }).toThrow(/QuotaExceededError/);
    expect(h.reloads).toBe(0);
  });

  it("refuses to act on an unparsable stamp instead of reloading blind", () => {
    // Arrange — only this module writes the key, so garbage is corruption.
    const h = harness();
    h.storage.setItem(FORCED_RELOAD_AT_KEY, "not-a-number");
    // Act + Assert
    expect(() => {
      for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    }).toThrow(/unparsable value/);
  });
});

describe("the default reload seam", () => {
  it("never reloads synchronously inside the observing call", async () => {
    // Arrange — the real defer is a microtask, so the frame ingest that decided
    // on the reload finishes first.
    let reloads = 0;
    const guard = new VersionSkewGuard({
      reload: () => {
        reloads++;
      },
      storage: new FakeStorage(),
      onReloadRefused: () => {
        throw new Error("unexpected refusal");
      },
    });
    guard.observeSnapshotAdoption(buildOf("boot-a"));
    // Act
    guard.observeSnapshotAdoption(buildOf("boot-b"));
    const duringCall = reloads;
    await Promise.resolve();
    // Assert
    expect(duringCall).toBe(0);
    expect(reloads).toBe(1);
  });
});

describe("the default clock seam", () => {
  it("stamps the forced reload with the real wall clock", () => {
    // Arrange — no injected `now`, so the stamp must come from Date.now().
    const storage = new FakeStorage();
    const guard = new VersionSkewGuard({
      reload: () => undefined,
      storage,
      onReloadRefused: () => {
        throw new Error("unexpected refusal");
      },
      defer: () => undefined,
    });
    const before = Date.now();
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) guard.observeLeaseExpiry();
    // Assert
    expect(Number(storage.getItem(FORCED_RELOAD_AT_KEY))).toBeGreaterThanOrEqual(before);
  });
});

describe("an ordinary bounce, which changes no build", () => {
  // THE REGRESSION THIS GUARDS. The trigger used to be `DaemonView.bootId`,
  // which changes on EVERY daemon restart. So a routine bounce reloaded every
  // open page; each reloaded page came back with an empty conversation store
  // and asked the daemon to replay the whole segment from seq 0 — hundreds of
  // events per workspace, per bounce, to recover history the page had been
  // holding a moment earlier.
  //
  // Recovery after a bounce must be INCREMENTAL, and a page that survives is
  // the precondition for that: it still holds its applied history, so it can
  // ask for the delta since its own high-water mark.

  it("does not reload when the daemon restarts on the same build", () => {
    // Arrange — the same build, adopted twice, is what a bounce looks like to
    // a page: the socket dropped, a new process answered, nothing was
    // redeployed.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    // Act
    const outcome = h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    h.settle();
    // Assert
    expect(outcome).toBe("none");
    expect(h.reloads).toBe(0);
  });

  it("still reloads when a rebuild moves the binary mtime under one version", () => {
    // Arrange — a locally rebuilt daemon keeps its version string, so the
    // mtime is the only thing that moves. Comparing both fields is what makes
    // a developer's own rebuild as detectable as a release.
    const h = harness();
    h.guard.observeSnapshotAdoption({ version: "v1.2.3", binaryMtimeMs: 1_000 });
    // Act
    const outcome = h.guard.observeSnapshotAdoption({ version: "v1.2.3", binaryMtimeMs: 2_000 });
    h.settle();
    // Assert
    expect(outcome).toBe("reloaded");
    expect(h.reloads).toBe(1);
  });

  it("still reloads when a release moves the version under one mtime", () => {
    // Arrange — the mirror case, so neither field can be the only one checked.
    const h = harness();
    h.guard.observeSnapshotAdoption({ version: "v1.2.3", binaryMtimeMs: 1_000 });
    // Act
    const outcome = h.guard.observeSnapshotAdoption({ version: "v1.2.4", binaryMtimeMs: 1_000 });
    h.settle();
    // Assert
    expect(outcome).toBe("reloaded");
    expect(h.reloads).toBe(1);
  });

  it("clears the stale-bundle evidence on a same-build re-adoption", () => {
    // Arrange — adoption is the proof this bundle can ingest this daemon's
    // frames, and a bounce's re-adoption proves it just as well as a first one.
    // Without this, expiries accumulated across bounces would eventually force
    // a reload on a page that was working perfectly.
    const h = harness();
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    h.guard.observeLeaseExpiry();
    // Act
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    h.guard.observeLeaseExpiry();
    h.settle();
    // Assert — one expiry short of the threshold, because the re-adoption
    // reset the count.
    expect(h.reloads).toBe(0);
  });
});

/**
 * THE PRODUCTION LOOP (workspace marcos-pr-remediation, daemon pid 36279): a
 * page reloaded, adopted exactly one snapshot, wedged again, expired twice, and
 * reloaded — over and over, each cycle flashing the reconnect banner. The
 * single adoption cleared the expiry counter, and the cycle ran LONGER than the
 * cooldown, so neither existing bound could ever be reached. The cross-reload
 * COUNT is what makes the loop terminate in the refusal instead.
 */
describe("a reload loop whose cycles outrun the cooldown", () => {
  /** One wedge cycle: a fresh page that adopts once, then expires its lease out. */
  function wedgeCycle(storage: FakeStorage, atMs: number): Harness {
    const h = harness(storage, atMs);
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    h.settle();
    return h;
  }

  it("terminates in a refusal rather than reloading forever", () => {
    // Arrange — cycles spaced beyond the cooldown, which is the case the
    // cooldown alone cannot catch.
    const storage = new FakeStorage();
    const spacingMs = FORCED_RELOAD_COOLDOWN_MS + 10_000;
    let at = 1_000_000;
    for (let i = 0; i < MAX_FORCED_RELOADS; i++) {
      wedgeCycle(storage, at);
      at += spacingMs;
    }
    // Act — the page that comes back from the last permitted reload.
    const last = wedgeCycle(storage, at);
    // Assert
    expect(last.reloads).toBe(0);
    expect(last.refusals).toHaveLength(1);
  });

  it("names the reload count in the refusal it surfaces", () => {
    // Arrange
    const storage = new FakeStorage();
    storage.setItem(FORCED_RELOAD_COUNT_KEY, String(MAX_FORCED_RELOADS));
    const h = harness(storage);
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Assert
    expect(h.refusals[0]).toContain(`reloaded ${MAX_FORCED_RELOADS} times`);
  });

  it("counts each forced reload in session storage, which is all a fresh page inherits", () => {
    // Arrange
    const storage = new FakeStorage();
    const h = harness(storage);
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    h.settle();
    // Assert
    expect(storage.getItem(FORCED_RELOAD_COUNT_KEY)).toBe("1");
  });

  it("refuses to reload on a count it cannot trust", () => {
    // Arrange — only this module writes the key, so a negative count is
    // corruption that would RAISE the ceiling if it were tolerated.
    const storage = new FakeStorage();
    storage.setItem(FORCED_RELOAD_COUNT_KEY, "-4");
    const h = harness(storage);
    // Act + Assert
    expect(() => {
      for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    }).toThrow(/unusable value/);
  });
});

describe("the forced-reload history", () => {
  it("is discharged by a page that stays current for a full cooldown", () => {
    // Arrange — a page that really recovered must not be condemned by a skew
    // it already repaired.
    const storage = new FakeStorage();
    storage.setItem(FORCED_RELOAD_COUNT_KEY, String(MAX_FORCED_RELOADS));
    const h = harness(storage);
    h.now += HEALTHY_UPTIME_MS;
    // Act
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    // Assert
    expect(storage.getItem(FORCED_RELOAD_COUNT_KEY)).toBe("0");
  });

  it("survives the single adoption a wedged page manages before wedging again", () => {
    // Arrange — discharging on ANY adoption is what would reinstate the loop.
    const storage = new FakeStorage();
    storage.setItem(FORCED_RELOAD_COUNT_KEY, String(MAX_FORCED_RELOADS));
    const h = harness(storage);
    // Act — adopted immediately after the page loaded, as the wedged page did.
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    // Assert
    expect(storage.getItem(FORCED_RELOAD_COUNT_KEY)).toBe(String(MAX_FORCED_RELOADS));
  });
});

/**
 * The seam main.ts reads to withhold the "lost the connection to the daemon"
 * card while the page tears itself down: a self-initiated reload closes the
 * socket, and that close is not news about the daemon.
 */
describe("the self-reload flag", () => {
  it("is clear on a page that has decided on no reload", () => {
    // Arrange
    const h = harness();
    // Act
    h.guard.observeSnapshotAdoption(buildOf("v1.2.3"));
    // Assert
    expect(h.guard.isReloading).toBe(false);
  });

  it("is set from the moment a reload is scheduled, before it runs", () => {
    // Arrange — the socket dies during the deferred teardown, so the flag has
    // to be true BEFORE `settle()`, not after.
    const h = harness();
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Assert
    expect(h.guard.isReloading).toBe(true);
  });

  it("stays clear when the reload was refused, so the card is not withheld", () => {
    // Arrange — a refused page is NOT going anywhere, and a disconnect it
    // suffers afterwards is a real one the user must be told about.
    const storage = new FakeStorage();
    storage.setItem(FORCED_RELOAD_COUNT_KEY, String(MAX_FORCED_RELOADS));
    const h = harness(storage);
    // Act
    for (let i = 0; i < STALE_BUNDLE_EXPIRY_CYCLES; i++) h.guard.observeLeaseExpiry();
    // Assert
    expect(h.guard.isReloading).toBe(false);
  });
});
