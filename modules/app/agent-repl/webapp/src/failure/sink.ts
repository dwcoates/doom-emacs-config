/**
 * Where the webapp reports its OWN machinery failing.
 *
 * THE SIX CLIENT-LOCAL ARMS AND NO OTHERS. `frontend.v1.FailureKind` splits by
 * producer: the daemon mints arms 1–11 (it is the only thing that can see the
 * shim, the store or the vendor), and a frontend mints arms 12–17 — the
 * failures of its own machinery, which the daemon definitionally cannot
 * observe. A failure this end wants to report that has no arm up there is a
 * failure this end has no standing to classify.
 *
 * REPORT AND RETRACT, KEYED ON THE ARM. A reconnect loop that appended a row
 * per attempt would bury the warning chip under its own alarm, so a repeat
 * report of the SAME arm replaces its row. `daemon_unreachable` is WINDOW-SHAPED and is
 * retracted when the link comes back — a settled "lost the connection;
 * reconnecting" beside a visibly live feed reads as a standing fault.
 * `workspace_gone` and `stale_bundle` are deliberately unretractable: there is
 * nothing to come back, and a self-clearing version would hide a page that is
 * silently wrong.
 */
import { create } from "@bufbuild/protobuf";
import {
  FailureKindSchema,
  type FailureKind,
} from "../../../proto/gen/ts/frontend/v1/failure_pb";

/**
 * The arm names a frontend may mint, spelled once.
 *
 * These are the generated arm names, so a renamed arm fails the build here
 * rather than producing a row no surface has an entry for.
 */
export const CLIENT_FAILURE_ARMS = [
  "daemonUnreachable",
  "workspaceGone",
  "bootFailed",
  "controlPlaneFailed",
  "frameUndecodable",
  "staleBundle",
] as const;

export type ClientFailureArm = (typeof CLIENT_FAILURE_ARMS)[number];

/** Whether ARM belongs to the band a frontend is allowed to mint. */
export function isClientFailureArm(arm: string): arm is ClientFailureArm {
  return (CLIENT_FAILURE_ARMS as readonly string[]).includes(arm);
}

export interface FailureSink {
  /** File (or replace) the warning chip's row for KIND's arm. */
  report(kind: FailureKind): void;
  /** Remove ARM's row, if one stands. */
  retract(arm: ClientFailureArm): void;
  /**
   * Hold ARM's row back until UNTILMS, because the failure is EXPECTED.
   *
   * The one caller is the lifecycle's shutdown announcement: the daemon has
   * said it is going away and for how long, so the streams dying inside that
   * window is the announced event rather than news. Suppression hides the
   * ROW and nothing else — the report is still logged, so a suppressed window
   * is visible to whoever reads the log rather than erased.
   *
   * OPTIONAL on the interface because a sink that only collects (a test's, a
   * headless one) has no row to hold back; `createLocalFailures` implements it.
   */
  suppress?(arm: ClientFailureArm, untilMs: number): void;
}

// ---------------------------------------------------------------------------
// THE MINTING HELPERS.
//
// They live beside the interface rather than in `local.ts` because
// `src/rpc/streams.ts` mints two of them (frame_undecodable on a push it could
// not read, daemon_unreachable on a stream that ended) and must not pull the
// failure set in to do it. `local.ts` re-exports them, so a component that
// files and holds still imports one module.
// ---------------------------------------------------------------------------

/**
 * The link to the daemon is down. WINDOW-SHAPED: retracted on the first
 * successful push after it, never settled in place.
 *
 * The close code and reason are the arm's OWN typed evidence rather than
 * prose, because they are the only thing distinguishing a daemon that
 * restarted cleanly from a network drop. Reporting both as one thing is what
 * made "reconnecting..." the webapp's answer to every transport fault.
 */
export function daemonUnreachable(closeCode: number, closeReason: string): FailureKind {
  return create(FailureKindSchema, {
    kind: { case: "daemonUnreachable", value: { closeCode, closeReason } },
  });
}

/**
 * A push could not be read and was skipped. Conversation may be missing as a
 * result, which is why it is a warning-chip row rather than a log line.
 */
export function frameUndecodable(cause: string, frameHead: string): FailureKind {
  return create(FailureKindSchema, {
    kind: { case: "frameUndecodable", value: { cause, frameHead } },
  });
}

/**
 * The frontend could not start at all — the one failure that cannot be carried
 * the way the others are, since the machinery that would carry it is the
 * machinery that failed to build.
 */
export function bootFailed(cause: string): FailureKind {
  return create(FailureKindSchema, { kind: { case: "bootFailed", value: { cause } } });
}

/**
 * A control-plane request issued outside the component streams (a login, an
 * account switch) failed. WHAT names the request in the frontend's own words,
 * so repeats of DIFFERENT requests do not reconcile onto one row.
 */
export function controlPlaneFailed(what: string, cause: string): FailureKind {
  return create(FailureKindSchema, {
    kind: { case: "controlPlaneFailed", value: { what, cause } },
  });
}

/**
 * The workspace this page is addressed to no longer exists on the daemon.
 * NEVER RETRACTS: unlike a dropped connection, there is nothing to come back.
 */
export function workspaceGone(): FailureKind {
  return create(FailureKindSchema, { kind: { case: "workspaceGone", value: {} } });
}

/**
 * This page cannot read the daemon's state and reloading did not fix it.
 * Deliberately loud and DELIBERATELY UNRETRACTABLE: the only exit is
 * restarting the view, and a self-clearing version would hide a page that is
 * silently wrong.
 */
export function staleBundle(detail: string): FailureKind {
  return create(FailureKindSchema, { kind: { case: "staleBundle", value: { detail } } });
}

/**
 * The arms whose row DISAPPEARS when its window closes, rather than settling.
 *
 * Only `daemon_unreachable`: it reports a transport link that was momentarily
 * down and is now up again, so once the link is back the row names a
 * condition that no longer exists beside a feed that is visibly live. The
 * other five either never resolve (`workspace_gone`, `stale_bundle`) or are
 * retracted by whoever filed them.
 */
export const RETRACTABLE_ON_RECONNECT: readonly ClientFailureArm[] = ["daemonUnreachable"];
