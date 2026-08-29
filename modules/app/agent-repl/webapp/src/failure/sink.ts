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
 * REPORT AND RETRACT, KEYED ON THE ARM. A reconnect loop that appended a card
 * per attempt would bury the page under its own alarm, so a repeat report of
 * the SAME arm replaces its card. `daemon_unreachable` is WINDOW-SHAPED and is
 * retracted when the link comes back — a settled "lost the connection;
 * reconnecting" beside a visibly live feed reads as a standing fault.
 * `workspace_gone` and `stale_bundle` are deliberately unretractable: there is
 * nothing to come back, and a self-clearing version would hide a page that is
 * silently wrong.
 */
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";

/**
 * The arm names a frontend may mint, spelled once.
 *
 * These are the generated arm names, so a renamed arm fails the build here
 * rather than producing a card no surface has an entry for.
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
  /** File (or replace) the card for KIND's arm. */
  report(kind: FailureKind): void;
  /** Remove ARM's card, if one stands. */
  retract(arm: ClientFailureArm): void;
}
