/**
 * local-failure — the webapp's OWN classifier, and the only one it is allowed
 * to have.
 *
 * A fact is classified by the process that is first to hold both the failure
 * and its cause. The daemon holds every conversation-plane fact, so everything
 * on that plane arrives already classified and this end renders it without
 * inspecting it. What the daemon definitionally CANNOT report is its own
 * unreachability — that is this file's whole subject.
 *
 * # The namespace partition
 *
 * Every type minted here carries the reserved `client.` prefix, and the daemon
 * never emits one. That makes a violation detectable by string inspection
 * rather than by review: a bare type appearing from this side means the
 * frontend re-classified something the daemon already decided, and a
 * `client.`-prefixed type on the wire means a frontend's failure was laundered
 * through the daemon.
 */

import type { SystemFailureCard } from "./store.js";

/** The namespace reserved for FRONTEND-classified failures. */
export const CLIENT_PREFIX = "client.";

/** The closed vocabulary of failures this frontend classifies for itself. */
export const CLIENT_FAILURE_TYPES = [
  /** The socket to the daemon closed and reconnection is under way. */
  "client.daemon_unreachable",
  /** The daemon answered the existence probe with "no such session". */
  "client.session_gone",
  /** The webapp could not boot at all. */
  "client.boot_failed",
] as const;
export type ClientFailureType = (typeof CLIENT_FAILURE_TYPES)[number];

/** Reports whether a type belongs to the frontend's namespace. */
export function isClientType(t: string): boolean {
  return t.startsWith(CLIENT_PREFIX);
}

/**
 * Mint a locally-classified failure card.
 *
 * Always INTERNAL class: everything this end can legitimately classify is
 * agent-repl's own machinery failing to reach itself. Nothing about the
 * account is ever implicated by a transport fault, so an API-class local
 * failure would be this frontend guessing at something only the daemon can
 * see.
 *
 * `uuid` is derived from the type so a repeated report of the SAME condition
 * reconciles onto one card instead of stacking. A reconnect loop that
 * appended a card per attempt would bury the feed under its own alarm.
 */
export function clientFailure(
  type: ClientFailureType,
  message: string,
  sourceDetail = "",
  resolvedAtMs = 0,
): SystemFailureCard {
  return {
    kind: "failure",
    errorClass: "INTERNAL",
    errorType: type,
    message,
    sourceDetail,
    resolvedAtMs,
    uuid: `local:${type}`,
  };
}

/**
 * Classify a WebSocket close as the daemon-unreachable failure.
 *
 * The close code and reason are read rather than discarded, because they are
 * the ONLY evidence distinguishing a daemon that restarted (a clean 1000/1001
 * from a shutting-down server) from a network drop (an abnormal 1006 the
 * browser synthesizes with no reason at all). Reporting both as one thing is
 * what made "reconnecting…" the webapp's answer to every transport fault.
 */
export function daemonUnreachableFailure(code: number, reason: string): SystemFailureCard {
  const detail = reason === "" ? `close=${code}` : `close=${code} ${reason}`;
  const message =
    code === 1000 || code === 1001
      ? "the daemon closed the connection; reconnecting"
      : "lost the connection to the daemon; reconnecting";
  return clientFailure("client.daemon_unreachable", message, detail);
}

/** The resolved twin of the above, stamped when the socket comes back. */
export function daemonReachableFailure(atMs: number): SystemFailureCard {
  return clientFailure(
    "client.daemon_unreachable",
    "reconnected to the daemon",
    "",
    atMs,
  );
}

/**
 * Classify the definitive "not listed" answer to the session-existence probe.
 *
 * Unlike a close, this never resolves: the session the view was bound to is
 * gone, and no amount of waiting brings that one back.
 */
export function sessionGoneFailure(sessionId: string): SystemFailureCard {
  return clientFailure(
    "client.session_gone",
    "this session no longer exists on the daemon",
    `session=${sessionId}`,
  );
}
