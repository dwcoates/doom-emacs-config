/**
 * VERSION SKEW: what this page does when the code it is running is older than
 * the daemon it is talking to.
 *
 * The webapp bundle is loaded ONCE into an Emacs xwidget webview and then
 * outlives arbitrarily many daemon redeploys. `WsClient` reconnects across a
 * restart perfectly well — the transport was never the problem. What breaks is
 * INGEST: a page built against an older `frontend.v1` surface receives the new
 * daemon's connect `StateSnapshot`, fails to adopt it, and its own 45-second
 * snapshot lease expires and closes the socket. That loop has no exit. The
 * observed symptom is a permanent "reconnecting" card in front of a daemon that
 * is up, healthy, and pushing frames the page cannot read.
 *
 * Nothing on the wire can fix that, because the page's own code is the stale
 * part. The only repair is fetching the bundle again, which is what this module
 * decides to do.
 *
 * # Two independent triggers
 *
 * 1. BOOT ID SKEW — the precise signal. Every connect snapshot carries a
 *    `DaemonView` whose `bootId` identifies the daemon PROCESS. The page pins
 *    the first boot id it successfully adopts; a later snapshot bearing a
 *    different one means the daemon restarted underneath a live page, which is
 *    exactly the moment a redeploy could have changed the bundle. Reload.
 *
 *    This cannot loop: the reloaded page pins the NEW boot id on its own first
 *    adoption, so the skew it reloaded for is not representable a second time.
 *
 * 2. REPEATED LEASE EXPIRY — the belt-and-suspenders signal, for the wedged
 *    case where the page cannot adopt at all and therefore never learns a boot
 *    id to compare. Adoption is the ONLY proof that this bundle can ingest this
 *    daemon's frames, so consecutive expiries with no adoption between them are
 *    presumptive evidence of stale code. Reloading beats churning forever.
 *
 * # Why the second trigger needs a cooldown and the first does not
 *
 * Trigger 2's premise ("this bundle is stale") can be WRONG. If the freshly
 * fetched bundle also cannot adopt, the page would reload every couple of lease
 * expiries forever and the real defect — a daemon pushing something no build can
 * ingest — would be hidden behind the churn it caused. So a forced reload stamps
 * `sessionStorage`, and a second one inside the cooldown is REFUSED in favour of
 * a loud error card. Reaching that refusal is itself the diagnosis.
 */

/** How this module reports itself; mirrors the app's client-log levels. */
export type VersionSkewLogLevel = "info" | "warn" | "error";

/**
 * Consecutive snapshot-lease expiries, with no successful adoption between
 * them, that condemn the running bundle as stale.
 *
 * Two, not one: a single expiry is an ordinary daemon restart window (the
 * daemon is down for longer than the lease and the page is merely waiting), and
 * reloading on that would turn every routine restart into a page reload. Two in
 * a row means the page reconnected, was served a snapshot, and STILL could not
 * become current — which a longer wait does not fix.
 */
export const STALE_BUNDLE_EXPIRY_CYCLES = 2;

/** Minimum wall-clock interval between two FORCED (trigger 2) reloads. */
export const FORCED_RELOAD_COOLDOWN_MS = 60_000;

/** `sessionStorage` key holding the epoch-ms stamp of the last forced reload. */
export const FORCED_RELOAD_AT_KEY = "agent-repl.forced-reload-at-ms";

/** What one observed event caused this module to do. */
export type VersionSkewOutcome =
  /** Nothing; the page keeps running. */
  | "none"
  /** The FIRST boot id was pinned, which is never a skew. */
  | "pinned"
  /** A reload was requested (deferred, never synchronous). */
  | "reloaded"
  /** A forced reload was refused by the cooldown and reported instead. */
  | "refused";

export interface VersionSkewOptions {
  /** Hard page reload. Injectable so tests never navigate the runner. */
  reload: () => void;
  /** Where the forced-reload stamp lives; `sessionStorage` in the browser. */
  storage: Storage;
  /**
   * Surfaces a refused forced reload to the user. Required, not optional: the
   * refusal means the FRESH bundle cannot adopt either, and a console line
   * nobody opens is not a report of that.
   */
  onReloadRefused: (detail: string) => void;
  /** Wall clock, epoch ms. */
  now?: () => number;
  /**
   * Runs the reload at a safe point. Defaults to a microtask so the reload
   * never tears the page down in the middle of the frame ingest that decided
   * on it, and never blocks the caller.
   */
  defer?: (fn: () => void) => void;
  log?: (level: VersionSkewLogLevel, message: string) => void;
}

export class VersionSkewGuard {
  /** The daemon boot id this page belongs to; "" before the first adoption. */
  private pinnedBootId = "";
  /** Snapshot-lease expiries since the last successful adoption. */
  private expiriesSinceAdoption = 0;
  /** A reload is already scheduled; further observations must not stack one. */
  private reloading = false;

  constructor(private readonly opts: VersionSkewOptions) {}

  /** The pinned daemon boot id, for a caller's own bookkeeping. */
  get bootId(): string {
    return this.pinnedBootId;
  }

  /**
   * Rule on the `DaemonView.bootId` of a snapshot this page just ADOPTED.
   *
   * Adoption, not arrival: a snapshot that failed to ingest proves nothing
   * about this bundle, and pinning its boot id would let the wedged page
   * believe it had caught up.
   *
   * An empty boot id is an invariant violation, not a tolerated absence — the
   * daemon stamps a `DaemonView` on every connect snapshot, so a missing one
   * means the frame is malformed and no skew decision can be made from it.
   */
  observeSnapshotAdoption(bootId: string): VersionSkewOutcome {
    if (bootId === "") {
      this.opts.log?.(
        "error",
        "version skew: adopted snapshot carried no daemon boot id, which the daemon stamps on every connect snapshot",
      );
      throw new Error("version-skew: adopted snapshot carried an empty daemon boot id");
    }
    // Adoption is the only proof this bundle can ingest this daemon's frames,
    // so it is the only thing that clears the stale-bundle evidence.
    this.expiriesSinceAdoption = 0;
    if (this.pinnedBootId === "") {
      this.pinnedBootId = bootId;
      this.opts.log?.("info", `version skew: pinned daemon boot id ${bootId}`);
      return "pinned";
    }
    if (bootId === this.pinnedBootId) return "none";
    const previous = this.pinnedBootId;
    // LOUD: the page is about to discard itself, and an unexplained reload is
    // indistinguishable from a crash to the person watching it happen.
    this.opts.log?.(
      "warn",
      `version skew: daemon boot id changed ${previous} -> ${bootId}; reloading page for a fresh bundle`,
    );
    return this.scheduleReload();
  }

  /**
   * Report one snapshot-lease expiry (`WsStateFreshness` reached `expired`).
   *
   * Counted whether or not a boot id is pinned. A page that adopted long ago
   * and can no longer adopt is in exactly the same wedged state as one that
   * never adopted at all, and the pin says nothing about which.
   */
  observeLeaseExpiry(): VersionSkewOutcome {
    this.expiriesSinceAdoption++;
    if (this.expiriesSinceAdoption < STALE_BUNDLE_EXPIRY_CYCLES) {
      this.opts.log?.(
        "warn",
        `version skew: snapshot lease expired without adoption (${this.expiriesSinceAdoption}/${STALE_BUNDLE_EXPIRY_CYCLES})`,
      );
      return "none";
    }
    return this.forceReload();
  }

  /**
   * The cooldown-guarded reload for trigger 2. A refusal is reported to the
   * user and RESETS the counter, so the next refusal costs another full run of
   * expiry cycles rather than one card per lease.
   */
  private forceReload(): VersionSkewOutcome {
    const now = (this.opts.now ?? Date.now)();
    const last = this.readForcedReloadStamp();
    if (last !== null && now - last < FORCED_RELOAD_COOLDOWN_MS) {
      const sinceMs = now - last;
      const detail =
        `reloaded ${Math.round(sinceMs / 1000)}s ago and still cannot ingest the daemon's state ` +
        `after ${this.expiriesSinceAdoption} snapshot-lease expiries`;
      this.expiriesSinceAdoption = 0;
      this.opts.log?.(
        "error",
        `version skew: refusing a second forced reload within ${FORCED_RELOAD_COOLDOWN_MS}ms — ${detail}`,
      );
      this.opts.onReloadRefused(detail);
      return "refused";
    }
    this.writeForcedReloadStamp(now);
    this.opts.log?.(
      "warn",
      `version skew: ${this.expiriesSinceAdoption} consecutive snapshot-lease expiries without adoption; reloading page for a fresh bundle`,
    );
    return this.scheduleReload();
  }

  /** Fire-and-forget the reload at a safe point; never blocks the caller. */
  private scheduleReload(): VersionSkewOutcome {
    if (this.reloading) return "none";
    this.reloading = true;
    const defer = this.opts.defer ?? ((fn: () => void) => queueMicrotask(fn));
    defer(() => {
      this.opts.reload();
    });
    return "reloaded";
  }

  /** The last forced-reload stamp, or null when this page has never forced one. */
  private readForcedReloadStamp(): number | null {
    let raw: string | null;
    try {
      raw = this.opts.storage.getItem(FORCED_RELOAD_AT_KEY);
    } catch (err) {
      this.opts.log?.(
        "error",
        `version skew: reading ${FORCED_RELOAD_AT_KEY} from session storage failed: ${String(err)}`,
      );
      throw err;
    }
    if (raw === null) return null;
    const parsed = Number(raw);
    if (!Number.isFinite(parsed)) {
      // Only this module writes the key, so an unparsable value is corruption
      // rather than an expected input — say so instead of reloading blind.
      this.opts.log?.(
        "error",
        `version skew: ${FORCED_RELOAD_AT_KEY} holds the unparsable value ${JSON.stringify(raw)}`,
      );
      throw new Error(`version-skew: ${FORCED_RELOAD_AT_KEY} holds an unparsable value ${JSON.stringify(raw)}`);
    }
    return parsed;
  }

  private writeForcedReloadStamp(atMs: number): void {
    try {
      this.opts.storage.setItem(FORCED_RELOAD_AT_KEY, String(atMs));
    } catch (err) {
      // Without the stamp the cooldown cannot hold, and a reload whose guard is
      // gone is the reload loop this module exists to prevent.
      this.opts.log?.(
        "error",
        `version skew: writing ${FORCED_RELOAD_AT_KEY} to session storage failed: ${String(err)}`,
      );
      throw err;
    }
  }
}
