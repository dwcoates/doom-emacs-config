/**
 * The client-local failures: what the topbar's warning chip lists beside the
 * daemon's pushed warnings, when this page's own machinery stopped working.
 *
 * WHAT IT HOLDS AND WHAT IT DOES NOT. Exactly the six `FailureKind` arms a
 * FRONTEND may mint — the failures of its own machinery, which the daemon
 * definitionally cannot observe. The daemon's own eleven arms never come here:
 * a shim that died or a session that was superseded reaches the user through
 * the footer's disconnected and blocked families and the topbar's pushed
 * warnings. These are the residue those pushes cannot carry, because when one
 * fires the push may be exactly what is broken — so the chip draws them from
 * here, whether or not a topbar push ever arrived.
 *
 * NO DOM. This module keeps the standing set and says when it changed; the
 * topbar's warning chip is the one place it is drawn (`src/topbar/warnings.ts`).
 * The chip is the ONE place the webapp makes an error visible — no overlay, no
 * banner, no corner card.
 *
 * ONE ENTRY PER ARM. A repeat report of the same arm REPLACES its entry rather
 * than stacking: a reconnect loop that appended a row per attempt would bury
 * the chip's list under its own alarm, and the second report of a condition is
 * the same condition.
 *
 * RETRACTION IS PER ARM AND IS THE FILER'S CALL. `daemon_unreachable` is
 * window-shaped and `watchStream` retracts it on the first successful push, so
 * a flapping link leaves no ghost. `workspace_gone` and `stale_bundle` are
 * deliberately unretractable — there is nothing to come back, and a
 * self-clearing version of either would hide a page that is silently wrong.
 * This module enforces nothing about which: a caller that never retracts gets
 * a standing entry, which is the correct outcome for those two.
 *
 * SUPPRESSION IS FOR ANNOUNCED FAILURES ONLY. When the daemon says "I am
 * restarting and will be back in 8 s", the streams dying is the announcement
 * coming true, not news — so `suppress(arm, untilMs)` holds that arm back for
 * exactly the window the daemon named. The report is STILL LOGGED, so nothing
 * is erased; only the alarm is withheld, and only until the instant the daemon
 * itself gave. A report after expiry stands normally, which is the whole point:
 * an outage that overran its announcement is news again.
 */
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { log } from "../log.js";
import { reportClientFailure } from "../rpc/link.js";
import { requireCase } from "../rpc/strict.js";
import { isClientFailureArm, type ClientFailureArm, type FailureSink } from "./sink.js";

export {
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  staleBundle,
  workspaceGone,
} from "./sink.js";

export interface Handle {
  dispose(): void;
}

/** One standing client-local failure, as the warning chip draws it. */
export interface LocalFailure {
  readonly arm: ClientFailureArm;
  /** The sentence its row leads with, chosen by the arm. */
  readonly headline: string;
  /** The arm's own typed evidence as label/value pairs, empty values dropped. */
  readonly evidence: ReadonlyArray<readonly [string, string]>;
}

/** What the warning chip reads: the standing set, and word of each change. */
export interface LocalFailureView {
  /** The failures standing now, in the order they were first filed. */
  standing(): readonly LocalFailure[];
  /** Call LISTENER after every change to the standing set; answers the unsubscribe. */
  subscribe(listener: () => void): () => void;
}

/** The app's FailureSink: files, retracts and suppresses; the chip draws it. */
export type LocalFailures = FailureSink &
  LocalFailureView &
  Handle & {
    /** Hold ARM back until UNTILMS. Always implemented here. */
    suppress(arm: ClientFailureArm, untilMs: number): void;
  };

/**
 * The sentence each arm's row leads with.
 *
 * COMPOSED HERE, and this is the one place in the webapp that is allowed to
 * compose a sentence rather than draw one the daemon composed — because the
 * daemon does not know about these failures at all. Six fixed strings, one per
 * arm, chosen by the arm and never assembled from evidence.
 */
export const ARM_HEADLINE: Readonly<Record<ClientFailureArm, string>> = {
  daemonUnreachable: "lost the connection to the daemon; reconnecting",
  workspaceGone: "this workspace no longer exists on the daemon",
  bootFailed: "this page could not start",
  controlPlaneFailed: "a request this page made outside the streams failed",
  frameUndecodable: "a frame could not be read and was skipped, so conversation may be missing",
  staleBundle: "this page cannot read the daemon's state",
};

/**
 * The evidence rows a failure shows, per arm, as label/value pairs read
 * straight off the arm's own typed fields.
 *
 * VERBATIM, NEVER INTERPRETED. Each of these is the producer's own account —
 * a close code, a decode failure, the head of the frame — put where whoever
 * debugs this can read it, not explained. An empty value is omitted, because
 * the protos state outright that several of these are allowed to be empty and
 * an empty row reads as missing data.
 */
export function evidenceRows(kind: FailureKind): ReadonlyArray<readonly [string, string]> {
  const arm = requireCase(kind.kind, "FailureKind.kind");
  const rows = ((): ReadonlyArray<readonly [string, string]> => {
    switch (arm.case) {
      case "daemonUnreachable":
        return [
          ["close code", String(arm.value.closeCode)],
          ["close reason", arm.value.closeReason],
        ];
      case "workspaceGone":
        return [];
      case "bootFailed":
        return [["cause", arm.value.cause]];
      case "controlPlaneFailed":
        return [
          ["request", arm.value.what],
          ["cause", arm.value.cause],
        ];
      case "frameUndecodable":
        return [
          ["cause", arm.value.cause],
          ["frame", arm.value.frameHead],
        ];
      case "staleBundle":
        return [["detail", arm.value.detail]];
      default:
        // A daemon-minted arm has no evidence here: neither producer may set
        // the other's arms, and `report` refuses one before it gets this far.
        return [];
    }
  })();
  return rows.filter(([, value]) => value !== "");
}

/**
 * Create the app's client-local failure set.
 *
 * Empty while the page is healthy, so the chip it feeds costs nothing until
 * something is filed.
 */
export function createLocalFailures(): LocalFailures {
  log.debug("creating the warning chip's client-local failure set", {
    operation: "warning-chip.create",
  });
  const entries = new Map<ClientFailureArm, LocalFailure>();
  /** Per arm, the instant its suppression window ends. */
  const suppressedUntil = new Map<ClientFailureArm, number>();
  const listeners = new Set<() => void>();

  const changed = (): void => {
    for (const listener of [...listeners]) listener();
  };

  return {
    standing(): readonly LocalFailure[] {
      return [...entries.values()];
    },

    subscribe(listener: () => void): () => void {
      listeners.add(listener);
      return () => {
        listeners.delete(listener);
      };
    },

    report(kind: FailureKind): void {
      const arm = requireCase(kind.kind, "FailureKind.kind").case;
      if (!isClientFailureArm(arm)) {
        // The daemon never mints one of these, and a frontend never mints the
        // daemon's. Refusing loudly is the whole point of splitting the
        // vocabulary by producer.
        log.error(`the warning chip was handed the non-client failure arm '${arm}'`, {
          operation: "warning-chip.foreign-arm",
          context: { arm },
        });
        return;
      }
      const until = suppressedUntil.get(arm);
      if (until !== undefined && Date.now() < until) {
        // LOGGED, NOT LISTED. The failure is real and the record of it must
        // survive; what is withheld is the alarm, for the window the daemon
        // itself named.
        log.info(`the ${arm} failure is suppressed for an announced outage`, {
          operation: "warning-chip.suppressed",
          context: { arm, until_ms: until },
        });
        return;
      }
      const replacing = entries.has(arm);
      const write = replacing ? log.debug : log.error;
      write(`${replacing ? "replacing" : "filing"} the ${arm} failure in the warning chip`, {
        operation: replacing ? "warning-chip.replace" : "warning-chip.report",
        context: { arm },
      });
      entries.set(arm, { arm, headline: ARM_HEADLINE[arm], evidence: evidenceRows(kind) });
      changed();
      // THE CHIP LISTS IT; THE FOOTER HEARS ABOUT IT TOO (the audit's N2 row
      // 11: a footer left saying `working` under a dead link kept saying
      // `working`). Only the two arms that describe THIS page's link to the
      // daemon are relayed -- a boot failure, a departed workspace or a stale
      // bundle is not a link this footer can speak for. AFTER the entry is
      // filed and drawn, so the relay can never cost the chip its row: the
      // footer redrawing is somebody else's code running inside this call.
      if (arm === "daemonUnreachable") {
        reportClientFailure("daemon_unreachable_card", ARM_HEADLINE.daemonUnreachable);
      }
      if (arm === "frameUndecodable") {
        reportClientFailure("frame_undecodable_card", ARM_HEADLINE.frameUndecodable);
      }
    },

    suppress(arm: ClientFailureArm, untilMs: number): void {
      log.info(`suppressing the ${arm} failure until an announced instant`, {
        operation: "warning-chip.suppress",
        context: { arm, until_ms: untilMs },
      });
      suppressedUntil.set(arm, untilMs);
      // An entry already standing for this arm belongs to the moments BEFORE
      // the announcement was read; the window it is now inside says it is
      // expected, so it comes down with the rest.
      if (entries.delete(arm)) changed();
    },

    retract(arm: ClientFailureArm): void {
      // Retracting is the filer saying the condition is over, which ends any
      // window it was inside: a link that came back early must not stay muted
      // for the remainder of an outage that did not happen.
      suppressedUntil.delete(arm);
      if (!entries.delete(arm)) return;
      log.info(`retracting the ${arm} failure from the warning chip`, {
        operation: "warning-chip.retract",
        context: { arm },
      });
      changed();
    },

    dispose(): void {
      log.debug("disposing the warning chip's client-local failure set", {
        operation: "warning-chip.dispose",
      });
      const had = entries.size > 0;
      entries.clear();
      suppressedUntil.clear();
      if (had) changed();
      listeners.clear();
    },
  };
}
