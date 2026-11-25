/**
 * The client-local failures: what this page records when its own machinery
 * stopped working.
 *
 * WHAT IT HOLDS AND WHAT IT DOES NOT. Exactly the six `FailureKind` arms a
 * FRONTEND may mint — the failures of its own machinery, which the daemon
 * definitionally cannot observe. The daemon's own eleven arms never come here:
 * a shim that died or a session that was superseded reaches the user through
 * the footer's disconnected and blocked families.
 *
 * NO SURFACE LISTS THEM (owner ruling, 2026-11-25: the topbar draws no
 * warnings). Filing one records it at ERROR with its arm's own evidence, and
 * the three arms that describe this page's link to the daemon are relayed to
 * the footer, which draws them as cards.
 *
 * ONE STANDING FAILURE PER ARM. A repeat report of the same arm REPLACES the
 * standing one rather than stacking: a reconnect loop would otherwise record
 * an ERROR and relay a card per attempt, and the second report of a condition
 * is the same condition, recorded at DEBUG.
 *
 * RETRACTION IS PER ARM AND IS THE FILER'S CALL. `daemon_unreachable` is
 * window-shaped and `watchStream` retracts it on the first successful push.
 * `workspace_gone` and `stale_bundle` are deliberately unretractable — there
 * is nothing to come back.
 *
 * SUPPRESSION IS FOR ANNOUNCED FAILURES ONLY. When the daemon says "I am
 * restarting and will be back in 8 s", the streams dying is the announcement
 * coming true, not news — so `suppress(arm, untilMs)` holds that arm back for
 * exactly the window the daemon named: the report is logged at INFO and
 * relays no card. A report after expiry stands normally, which is the whole
 * point: an outage that overran its announcement is news again.
 */
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { createLogger } from "../log.js";
import { reportClientFailure } from "../rpc/link.js";
import { requireCase } from "../rpc/strict.js";
import { isClientFailureArm, type ClientFailureArm, type FailureSink } from "./sink.js";

const log = createLogger("webapp.failure.local");

export {
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  frameUndrawable,
  staleBundle,
  workspaceGone,
} from "./sink.js";

export interface Handle {
  dispose(): void;
}

/** The app's FailureSink: files, retracts and suppresses. */
export type LocalFailures = FailureSink &
  Handle & {
    /** Hold ARM back until UNTILMS. Always implemented here. */
    suppress(arm: ClientFailureArm, untilMs: number): void;
    /** Whether ARM is held back right now. Always implemented here. */
    suppressing(arm: ClientFailureArm): boolean;
    /** The arms standing now, in the order they were first filed. */
    standingArms(): readonly ClientFailureArm[];
  };

/**
 * The sentence each arm is stated with, in its record and on the footer's
 * card.
 *
 * COMPOSED HERE, and this is the one place in the webapp that is allowed to
 * compose a sentence rather than draw one the daemon composed — because the
 * daemon does not know about these failures at all. Fixed strings, one per
 * arm, chosen by the arm and never assembled from evidence.
 */
export const ARM_HEADLINE: Readonly<Record<ClientFailureArm, string>> = {
  daemonUnreachable: "lost the connection to the daemon; reconnecting",
  workspaceGone: "this workspace no longer exists on the daemon",
  bootFailed: "this page could not start",
  controlPlaneFailed: "a request this page made outside the streams failed",
  frameUndecodable: "a frame could not be read and was skipped, so conversation may be missing",
  staleBundle: "this page cannot read the daemon's state",
  frameUndrawable: "this page failed to draw a frame and is drawing the stream afresh",
};

/**
 * The evidence a failure's record carries, per arm, as label/value pairs read
 * straight off the arm's own typed fields.
 *
 * VERBATIM, NEVER INTERPRETED. Each of these is the producer's own account —
 * a close code, a decode failure, the head of the frame — put where whoever
 * debugs this can read it, not explained. An empty value is omitted, because
 * the protos state outright that several of these are allowed to be empty and
 * an empty field reads as missing data.
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
      case "frameUndrawable":
        return [
          ["stream", arm.value.stream],
          ["cause", arm.value.cause],
        ];
      default:
        // A daemon-minted arm has no evidence here: neither producer may set
        // the other's arms, and `report` refuses one before it gets this far.
        return [];
    }
  })();
  return rows.filter(([, value]) => value !== "");
}

/**
 * The record context of KIND's evidence: each label, spaces as underscores,
 * to its value.
 */
export function evidenceContext(kind: FailureKind): Record<string, string> {
  return Object.fromEntries(evidenceRows(kind).map(([label, value]) => [label.replaceAll(" ", "_"), value]));
}

/** Create the page's client-local failures. */
export function createLocalFailures(): LocalFailures {
  log.debug("creating the client-local failures", { operation: "failure.create" });
  /** The arms standing now. */
  const standing = new Set<ClientFailureArm>();
  /** Per arm, the instant its suppression window ends. */
  const suppressedUntil = new Map<ClientFailureArm, number>();

  return {
    report(kind: FailureKind): void {
      const arm = requireCase(kind.kind, "FailureKind.kind").case;
      if (!isClientFailureArm(arm)) {
        // The daemon never mints one of these, and a frontend never mints the
        // daemon's. Refusing loudly is the whole point of splitting the
        // vocabulary by producer.
        log.error("a client-local failure was filed under the non-client arm '{arm}'", {
          operation: "failure.foreign-arm",
          context: {},
        }, { arm });
        return;
      }
      const evidence = evidenceContext(kind);
      const until = suppressedUntil.get(arm);
      if (until !== undefined && Date.now() < until) {
        // RECORDED, NOT RELAYED. The failure is real and the record of it
        // must survive; what is withheld is the footer's card, for the window
        // the daemon itself named.
        log.info("the {arm} failure is suppressed for an announced outage", {
          operation: "failure.suppressed",
          context: { until_ms: until, ...evidence },
        }, { arm });
        return;
      }
      if (standing.has(arm)) {
        log.debug("the {arm} failure is reported again while it stands", { operation: "failure.replace", context: evidence }, { arm });
        return;
      }
      standing.add(arm);
      log.error("filing the {arm} failure: {headline}", { operation: "failure.report", context: evidence }, { arm, headline: ARM_HEADLINE[arm] });
      // THE FOOTER HEARS ABOUT IT (the audit's N2 row 11: a footer left
      // saying `working` under a dead link kept saying `working`). Only the
      // arms that describe THIS page's link to the daemon are relayed -- a
      // boot failure, a departed workspace or a stale bundle is not a link
      // this footer can speak for.
      if (arm === "daemonUnreachable") {
        reportClientFailure("daemon_unreachable_card", ARM_HEADLINE.daemonUnreachable);
      }
      if (arm === "frameUndecodable") {
        reportClientFailure("frame_undecodable_card", ARM_HEADLINE.frameUndecodable);
      }
      if (arm === "frameUndrawable") {
        reportClientFailure("frame_undrawable_card", ARM_HEADLINE.frameUndrawable);
      }
    },

    standingArms(): readonly ClientFailureArm[] {
      return [...standing];
    },

    suppressing(arm: ClientFailureArm): boolean {
      const until = suppressedUntil.get(arm);
      return until !== undefined && Date.now() < until;
    },

    suppress(arm: ClientFailureArm, untilMs: number): void {
      log.info("suppressing the {arm} failure until an announced instant", {
        operation: "failure.suppress",
        context: { until_ms: untilMs },
      }, { arm });
      suppressedUntil.set(arm, untilMs);
      // A failure already standing for this arm belongs to the moments BEFORE
      // the announcement was read; the window it is now inside says it is
      // expected, so it stands down with the rest.
      standing.delete(arm);
    },

    retract(arm: ClientFailureArm): void {
      // A RETRACT LEAVES AN ANNOUNCED WINDOW STANDING. A deploy's handover
      // moves the page's streams one at a time, so one stream reading again
      // is not the outage over: ending the window there filed a
      // daemonUnreachable card for the next stream the same handover moved
      // (2026-10-22). The window ends at the instant the daemon announced, and
      // an outage that overruns it is news again.
      if (!standing.delete(arm)) return;
      log.info("retracting the {arm} failure", {
        operation: "failure.retract",
        context: {},
      }, { arm });
    },

    dispose(): void {
      log.debug("disposing the client-local failures", { operation: "failure.dispose" });
      standing.clear();
      suppressedUntil.clear();
    },
  };
}
