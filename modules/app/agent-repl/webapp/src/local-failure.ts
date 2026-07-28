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
  /**
   * A user-initiated call to the daemon's HTTP control plane failed.
   *
   * These used to reach `clog` and stop there: a failed account switch, a
   * refused login and a remediation that never dispatched each produced a
   * console line the user will never open, and no other sign whatsoever.
   * The user pressed a button and the interface said nothing.
   *
   * The command plane is NOT this: a rejected `SubmitPromptCmd` comes back
   * as a daemon-classified `CommandAck.err`, and re-classifying it here
   * would be this end deciding something the daemon already decided.
   */
  "client.control_plane_failed",
  /**
   * An inbound `frontend.v1` frame could not be decoded, and this end skipped
   * it to keep going.
   *
   * The bootstrap socket is the one place a decode refusal is SURVIVABLE: the
   * live session socket re-throws, because a frame it cannot read means the
   * store it feeds is already a lie. The bootstrap connection only waits for a
   * snapshot to hang `createSession` off, so skipping a bad frame and waiting
   * for the next one is legitimate — but the skip is not free. Bootstrap
   * frames carry StateSnapshots, progress views included, so a frame this end
   * silently dropped is state the user is now missing with no sign of it.
   *
   * The refusal is classified HERE and not by the daemon because the daemon
   * sent a frame it believes is well-formed; only the receiver can observe
   * that it could not read it.
   */
  "client.frame_undecodable",
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
 * Classify a failed control-plane call as the frontend's own failure.
 *
 * `what` names the action in the user's terms ("the account switch"), not the
 * endpoint's — a card reading "POST /accounts/switch failed" explains nothing
 * to the person who clicked a menu item.
 *
 * The uuid carries `what`, so two different failed actions are two cards
 * while a retried one reconciles onto its own. Keying every control-plane
 * failure alike would let a failed login overwrite a failed remediation.
 */
export function controlPlaneFailure(what: string, err: unknown): SystemFailureCard {
  return {
    ...clientFailure(
      "client.control_plane_failed",
      `${what} failed`,
      err instanceof Error ? err.message : String(err),
    ),
    uuid: `local:client.control_plane_failed:${what}`,
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
 * Classify a frame this end could not decode as the frontend's own failure.
 *
 * The message is GENERIC on purpose: the reader is being told that a frame was
 * dropped, and the decoder's own complaint is evidence, not prose. The frame
 * head goes in `sourceDetail` beside it, so the debugger gets the bytes and
 * the user gets the sentence.
 *
 * One uuid for every occurrence, inherited from `clientFailure`: a daemon
 * emitting a shape this build cannot read will emit it repeatedly, and a card
 * per frame would bury the feed under the same fact.
 */
export function frameUndecodableFailure(err: unknown, frameHead: string): SystemFailureCard {
  const cause = err instanceof Error ? err.message : String(err);
  return clientFailure(
    "client.frame_undecodable",
    "a message from the daemon could not be read and was skipped",
    frameHead === "" ? cause : `${cause} — frame head: ${frameHead}`,
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
