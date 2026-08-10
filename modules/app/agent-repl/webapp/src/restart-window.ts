/**
 * RestartWindow — the bounded quiet period a client keeps after the daemon
 * announces that it is going down ON PURPOSE.
 *
 * THE DEFECT THIS ENDS: a full backend bounce (store + sidecar + daemon
 * restart + shim roll) is routine, wanted, and over in seconds — and every one
 * of them painted the blue "lost the connection to the daemon" card plus a
 * degraded status line, because the only thing this end ever learned was that
 * its socket died. A dead socket is the SAME observation for a crash and for a
 * deploy; the difference is a fact only the daemon holds, and now sends.
 *
 * Three properties make suppressing on that fact safe, and they are the same
 * three the Emacs frontend's expected-restart window rests on:
 *
 *   1. It is opened only by an explicit announcement from the daemon.
 *   2. It is BOUNDED. The announced outage is a PREDICTION, not a promise; a
 *      daemon that never comes back has its window expire, and every honest
 *      alarm that was suppressed resumes at that moment.
 *   3. It closes early on success: a reconnect that reaches a resynced socket
 *      ends the window then and there, rather than leaving the page quiet for
 *      the rest of a hint that turned out to be generous.
 *
 * WHAT IS SUPPRESSED IS THE ALARM, NEVER THE EVENT. The page still dials,
 * still resyncs, still recovers in the background. Only the rendering of the
 * disconnect changes: a quiet one-line indicator instead of a failure card.
 *
 * The clock is injected, so tests state the moment rather than waiting for it.
 */

/** How this module reports itself; mirrors the app's client-log levels. */
export type RestartWindowLogLevel = "info" | "warn" | "error";

/**
 * The daemon's announcement, as this end consumes it. It is deliberately NOT
 * the wire type — the window reasons in milliseconds, the wire states whole
 * seconds — so `announcementFromView` below is the one place the two meet and
 * the class itself stays free of protojson concerns.
 */
export interface RestartAnnouncement {
  /** Why the backend is bouncing. Display-grade; never parsed. */
  cause: string;
  /** How long the daemon expects to be gone, in ms. Must be positive. */
  expectedOutageMs: number;
  /** Whether the session shims roll too (a longer settle after revival). */
  stopShims: boolean;
  /**
   * When the daemon minted the announcement (epoch ms). The window is measured
   * from the MINT, not from receipt, so a notice that took a second to arrive
   * shortens the quiet period instead of restarting its clock.
   */
  atMs: number;
}

/**
 * Adapt the decoded `restartPending` frame onto the window's input.
 *
 * It exists as a named function rather than an inline object literal at the
 * frame handler so the wiring itself is testable: the unit under test is
 * "a decoded frame becomes the announcement the window opens on", and a test
 * that rebuilt that mapping by hand would pass while the real handler drifted.
 *
 * The seconds-to-milliseconds conversion is the only transformation. Nothing
 * is defaulted or repaired here — the wire decoder has already refused a
 * non-positive hint or a missing mint time, and `announce` refuses them again
 * on its own terms.
 */
export function announcementFromView(view: {
  cause: string;
  expectedOutageSeconds: number;
  stopShims: boolean;
  announcedAtMs: number;
}): RestartAnnouncement {
  return {
    cause: view.cause,
    expectedOutageMs: view.expectedOutageSeconds * 1000,
    stopShims: view.stopShims,
    atMs: view.announcedAtMs,
  };
}

/**
 * The hard ceiling on a quiet window, whatever the daemon asked for. A client
 * clamps rather than trusts: an announcement is an input from another process,
 * and "stay quiet for an hour" must not be expressible even by accident.
 */
export const MAX_QUIET_WINDOW_MS = 5 * 60_000;

/** What the page shows while the window is open. Unobtrusive by construction. */
export const RESTARTING_INDICATOR = "backend restarting…";

export interface RestartWindowOptions {
  /** Epoch-ms clock, injected so tests own the passage of time. */
  now: () => number;
  log?: (level: RestartWindowLogLevel, message: string) => void;
}

interface OpenWindow {
  cause: string;
  stopShims: boolean;
  /** Epoch-ms after which the window is over and honest alarms resume. */
  expiresAtMs: number;
}

export class RestartWindow {
  private open: OpenWindow | null = null;

  constructor(private readonly opts: RestartWindowOptions) {}

  /**
   * Open (or replace) the quiet window from an announcement.
   *
   * A malformed announcement is REFUSED rather than defaulted: a window opened
   * on a value nobody stated is a window nobody can reason about, and the
   * failure mode of getting it wrong is a page that stays silent through a
   * real outage. Refusal leaves the page in its ordinary, honest mode.
   */
  announce(announcement: RestartAnnouncement): boolean {
    const { cause, expectedOutageMs, atMs } = announcement;
    if (!(expectedOutageMs > 0) || !Number.isFinite(expectedOutageMs)) {
      this.opts.log?.(
        "error",
        `restart-window: REFUSING an announcement with a non-positive outage hint (${expectedOutageMs})`,
      );
      return false;
    }
    if (!(atMs > 0) || !Number.isFinite(atMs)) {
      this.opts.log?.("error", `restart-window: REFUSING an announcement with no mint time (${atMs})`);
      return false;
    }
    const quietMs = Math.min(expectedOutageMs, MAX_QUIET_WINDOW_MS);
    const expiresAtMs = atMs + quietMs;
    if (expiresAtMs <= this.opts.now()) {
      this.opts.log?.(
        "warn",
        `restart-window: announcement already expired on arrival (cause=${cause}); alarms stay honest`,
      );
      return false;
    }
    this.open = { cause, stopShims: announcement.stopShims, expiresAtMs };
    this.opts.log?.(
      "info",
      `restart-window: quiet until ${expiresAtMs} (cause=${cause} stop_shims=${announcement.stopShims})`,
    );
    return true;
  }

  /**
   * Whether the page is in restarting mode RIGHT NOW.
   *
   * Expiry is evaluated here rather than on a timer: a page in a backgrounded
   * webview may not run timers at all, and a window whose only bound was a
   * timer that never fired would suppress alarms forever.
   */
  isRestarting(): boolean {
    if (this.open === null) return false;
    if (this.opts.now() >= this.open.expiresAtMs) {
      const { cause } = this.open;
      this.open = null;
      this.opts.log?.(
        "warn",
        `restart-window: EXPIRED with no revived daemon (cause=${cause}); honest alarms resume`,
      );
      return false;
    }
    return true;
  }

  /** The quiet indicator, or null when there is nothing to say. */
  indicator(): string | null {
    return this.isRestarting() ? RESTARTING_INDICATOR : null;
  }

  /**
   * Whether an expected-disconnect alarm should be withheld right now. Named
   * separately from `isRestarting` because it is the question every call site
   * actually asks, and because withholding is the decision worth logging.
   */
  suppressesDisconnectAlarm(): boolean {
    return this.isRestarting();
  }

  /**
   * The socket came back and carries authoritative state again: the bounce is
   * over ahead of its predicted end, so the window closes now.
   */
  onResynced(): void {
    if (this.open === null) return;
    const { cause } = this.open;
    this.open = null;
    this.opts.log?.("info", `restart-window: closed by a resynced socket (cause=${cause})`);
  }
}
