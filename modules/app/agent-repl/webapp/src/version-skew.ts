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
 * 1. BUILD SKEW — the precise signal. Every connect snapshot carries a
 *    `DaemonView` naming the daemon's BUILD: its version string and the mtime
 *    of the binary serving the page. The page pins the first build it
 *    successfully adopts; a later snapshot bearing a different one means a
 *    redeploy happened under a live page, and a page cannot be redeployed in
 *    place. Reload.
 *
 *    This cannot loop: the reloaded page pins the NEW build on its own first
 *    adoption, so the skew it reloaded for is not representable a second time.
 *
 *    IT USED TO BE THE BOOT ID, AND THAT WAS THE WRONG QUESTION. `bootId`
 *    changes on every daemon restart, redeploy or not — so an ordinary bounce
 *    reloaded every open page, and each reloaded page came back with an EMPTY
 *    conversation store and asked the daemon to replay the whole segment from
 *    seq 0. Hundreds of events per workspace, per bounce, to recover history
 *    the page had been holding a moment earlier.
 *
 *    The build identity is the one that actually answers "could this bundle
 *    have gone stale". A bounce that redeploys nothing leaves it unchanged, the
 *    page keeps its applied store, and recovery is the delta since its
 *    high-water mark rather than the entire conversation. A bounce that DOES
 *    redeploy changes it, and the reload happens exactly as before.
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

// The snapshot LEASE is the clock every trigger-2 decision runs on, so the
// cooldown below is derived from it rather than from a second number that
// would drift out of relation with it. `ws.ts` imports nothing from here, so
// this direction carries no cycle.
import { DEFAULT_SNAPSHOT_TIMEOUT_MS } from "./ws.js";

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

/**
 * Minimum wall-clock interval between two FORCED (trigger 2) reloads.
 *
 * DERIVED, never restated as a literal. A full run of expiry cycles takes
 * `STALE_BUNDLE_EXPIRY_CYCLES * DEFAULT_SNAPSHOT_TIMEOUT_MS` of wall clock —
 * 90s at today's values — so a cooldown shorter than that can never be reached:
 * the second forced reload always arrives after it has lapsed, the refusal
 * never fires, and the wedged page reloads forever instead of producing the
 * loud error card that IS the diagnosis. The invariant is therefore that the
 * cooldown must EXCEED one full expiry-cycle run, so a second forced reload
 * inside it is provably futile rather than merely impatient. The +1 buys that
 * strict margin by deriving it from the same two constants, so a change to
 * either the cycle count or the lease keeps the relation rather than silently
 * unreachable-ing the refusal again.
 */
export const FORCED_RELOAD_COOLDOWN_MS =
  (STALE_BUNDLE_EXPIRY_CYCLES + 1) * DEFAULT_SNAPSHOT_TIMEOUT_MS;

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

/**
 * The daemon BUILD a page is pinned to.
 *
 * Two fields rather than one because a redeploy can change either: the version
 * string moves on a released build, and the binary mtime moves on a locally
 * rebuilt one that kept its version. Comparing both means a developer's own
 * rebuild is caught as surely as a release is.
 */
export interface DaemonBuild {
  /** `DaemonView.daemonVersion`. */
  version: string;
  /** `DaemonView.daemonBinaryMtimeMs`. */
  binaryMtimeMs: number;
}

/** Whether two adopted snapshots describe the same daemon build. */
function sameBuild(a: DaemonBuild, b: DaemonBuild): boolean {
  return a.version === b.version && a.binaryMtimeMs === b.binaryMtimeMs;
}

/** How a build reads in a log line. */
function renderBuild(build: DaemonBuild): string {
  return `${build.version || "unknown"}@${build.binaryMtimeMs}`;
}

export class VersionSkewGuard {
  /** The daemon build this page belongs to; null before the first adoption. */
  private pinnedBuild: DaemonBuild | null = null;
  /** Snapshot-lease expiries since the last successful adoption. */
  private expiriesSinceAdoption = 0;
  /** A reload is already scheduled; further observations must not stack one. */
  private reloading = false;

  constructor(private readonly opts: VersionSkewOptions) {}

  /** The pinned daemon build, for a caller's own bookkeeping. */
  get build(): DaemonBuild | null {
    return this.pinnedBuild;
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
  observeSnapshotAdoption(build: DaemonBuild): VersionSkewOutcome {
    if (build.version === "" && build.binaryMtimeMs === 0) {
      this.opts.log?.(
        "error",
        "version skew: adopted snapshot carried no daemon build identity, which the daemon stamps on every connect snapshot",
      );
      throw new Error("version-skew: adopted snapshot carried an empty daemon build identity");
    }
    // Adoption is the only proof this bundle can ingest this daemon's frames,
    // so it is the only thing that clears the stale-bundle evidence.
    this.expiriesSinceAdoption = 0;
    if (this.pinnedBuild === null) {
      this.pinnedBuild = build;
      this.opts.log?.("info", `version skew: pinned daemon build ${renderBuild(build)}`);
      return "pinned";
    }
    if (sameBuild(build, this.pinnedBuild)) {
      // THE ORDINARY BOUNCE LANDS HERE, and landing here is the whole point:
      // the page survives, keeps its applied conversation store, and recovers
      // by asking for the delta since its own high-water mark.
      return "none";
    }
    const previous = this.pinnedBuild;
    // LOUD: the page is about to discard itself, and an unexplained reload is
    // indistinguishable from a crash to the person watching it happen.
    this.opts.log?.(
      "warn",
      `version skew: daemon build changed ${renderBuild(previous)} -> ${renderBuild(build)}; reloading page for a fresh bundle`,
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
