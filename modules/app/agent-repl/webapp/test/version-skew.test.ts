import { describe, it, expect } from "vitest";

import {
  FORCED_RELOAD_AT_KEY,
  FORCED_RELOAD_COOLDOWN_MS,
  STALE_BUNDLE_EXPIRY_CYCLES,
  VersionSkewGuard,
  type VersionSkewLogLevel,
} from "../src/version-skew.js";
import { DEFAULT_SNAPSHOT_TIMEOUT_MS } from "../src/ws.js";

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

/** A guard whose reload is deferred into `deferred` until `settle()` runs it. */
function harness(): Harness {
  const storage = new FakeStorage();
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
    now: 1_000_000,
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
    const outcome = h.guard.observeSnapshotAdoption("boot-a");
    // Assert
    expect(outcome).toBe("pinned");
    expect(h.guard.bootId).toBe("boot-a");
  });

  it("never reloads on the first adoption", () => {
    // Arrange
    const h = harness();
    // Act
    h.guard.observeSnapshotAdoption("boot-a");
    h.settle();
    // Assert — there is no retired daemon behind a first pin.
    expect(h.reloads).toBe(0);
  });

  it("rejects an adopted snapshot that carried no daemon boot id", () => {
    // Arrange — the daemon stamps a DaemonView on every connect snapshot, so
    // an absent one is a malformed frame rather than an older daemon.
    const h = harness();
    // Act + Assert
    expect(() => h.guard.observeSnapshotAdoption("")).toThrow(/empty daemon boot id/);
  });

  it("logs the missing boot id at error level before throwing", () => {
    // Arrange
    const h = harness();
    // Act
    expect(() => h.guard.observeSnapshotAdoption("")).toThrow();
    // Assert — the canonical channel carries the diagnosis, not only the stack.
    expect(h.logs).toContainEqual([
      "error",
      "version skew: adopted snapshot carried no daemon boot id, which the daemon stamps on every connect snapshot",
    ]);
  });
});

describe("a changed daemon boot id", () => {
  it("reloads the page for a fresh bundle", () => {
    // Arrange — a restarted daemon may be a redeployed one, and this page's own
    // code is the only part a reconnect cannot refresh.
    const h = harness();
    h.guard.observeSnapshotAdoption("boot-a");
    // Act
    const outcome = h.guard.observeSnapshotAdoption("boot-b");
    h.settle();
    // Assert
    expect(outcome).toBe("reloaded");
    expect(h.reloads).toBe(1);
  });

  it("names both boot ids in the log that precedes the reload", () => {
    // Arrange — an unexplained reload is indistinguishable from a crash.
    const h = harness();
    h.guard.observeSnapshotAdoption("boot-a");
    // Act
    h.guard.observeSnapshotAdoption("boot-b");
    // Assert
    expect(h.logs).toContainEqual([
      "warn",
      "version skew: daemon boot id changed boot-a -> boot-b; reloading page for a fresh bundle",
    ]);
  });

  it("defers the reload rather than tearing the page down mid-ingest", () => {
    // Arrange
    const h = harness();
    h.guard.observeSnapshotAdoption("boot-a");
    // Act — no settle(): the caller's frame is still on the stack.
    h.guard.observeSnapshotAdoption("boot-b");
    // Assert
    expect(h.reloads).toBe(0);
    expect(h.deferred).toHaveLength(1);
  });

  it("schedules one reload however many skewed snapshots arrive", () => {
    // Arrange — snapshots keep arriving every 15s while the reload is pending.
    const h = harness();
    h.guard.observeSnapshotAdoption("boot-a");
    h.guard.observeSnapshotAdoption("boot-b");
    // Act
    const second = h.guard.observeSnapshotAdoption("boot-c");
    h.settle();
    // Assert
    expect(second).toBe("none");
    expect(h.reloads).toBe(1);
  });

  it("spends no forced-reload budget, which belongs to the expiry trigger", () => {
    // Arrange — boot-id skew cannot loop on its own, so it must not consume the
    // cooldown that protects the presumptive trigger.
    const h = harness();
    h.guard.observeSnapshotAdoption("boot-a");
    // Act
    h.guard.observeSnapshotAdoption("boot-b");
    // Assert
    expect(h.storage.getItem(FORCED_RELOAD_AT_KEY)).toBeNull();
  });
});

describe("an unchanged daemon boot id", () => {
  it("never reloads, however many snapshots repeat it", () => {
    // Arrange — a snapshot lands every 15 seconds and all of them repeat the id.
    const h = harness();
    h.guard.observeSnapshotAdoption("boot-a");
    // Act
    const outcome = h.guard.observeSnapshotAdoption("boot-a");
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
    h.guard.observeSnapshotAdoption("boot-a");
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
    guard.observeSnapshotAdoption("boot-a");
    // Act
    guard.observeSnapshotAdoption("boot-b");
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
