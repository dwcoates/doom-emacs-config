/**
 * The CONVERSATION REBASE: what this end does when the vendor retires one
 * transcript identity and starts another under a live view.
 *
 * A `/clear` makes the SDK mint a new session uuid mid-stream. Every seq the
 * daemon reports is a position in the STORE SEQ SPACE that uuid keys, and the
 * new space starts again at 1 — so the instant the uuid rotates, every seq this
 * end holds becomes a number counted against a conversation that no longer
 * exists:
 *
 * - `store.lastSeq` (the feed's rank for arriving items, and the `from_seq` a
 *   resync asks with) sits far above every seq the new space will produce for a
 *   long while: the observed webview held 1060 while the new space had reached
 *   12, so the clear itself ranked ABOVE a thousand items that preceded it and
 *   a reconnect asked the daemon for history past the end of the world;
 * - the paint attestation's painted-through watermark is in the same space, so
 *   the acks that follow a rotation carry a seq the daemon can only read as
 *   stale.
 *
 * None of that is recoverable by waiting: nothing in the new space will ever
 * climb past the retired space's marks. The rotation is a REBASE, and the
 * client's answer is to drop what it holds and ask for the new space from the
 * floor — which is exactly the daemon's own answer on its side of the wire
 * (registry cursors and replay floor reset to zero, resubscribe from the new
 * space's beginning).
 *
 * WHAT THIS MODULE IS. The decision and its log, nothing else. The effects —
 * emptying the store, resetting the attestation, sending the resync — straddle
 * the frame's ingest (the wipe must happen BEFORE the new space's items land,
 * the resync AFTER the workspace key is known), so they stay at the call site
 * where that ordering is visible.
 *
 * HOW THE ROTATION IS LEARNED. `SessionView.claude_session_id`, which the
 * daemon re-pushes on the rotation itself (server/sessioncontrollers.go
 * AdoptVendorSessionID) from the shim's re-handshake — before it resubscribes,
 * so the announcement precedes the new space's first conversation push on the
 * connection. The re-requested resync makes that ordering a convenience rather
 * than a dependency: an item that somehow arrived first is dropped by the wipe
 * and comes back in the replay.
 */

import type { AdapterEffect } from "./state-adapter.js";

/** How this module reports itself; mirrors the app's client-log levels. */
export type SessionRebaseLogLevel = "info" | "warn" | "error";

/**
 * What one observed vendor session uuid means for this end.
 *
 * - `unchanged` — the same conversation (or none announced): do nothing.
 * - `adopted` — the FIRST uuid this view has seen. Not a rotation: there is no
 *   retired space behind it, and treating a fresh mount's first `SessionView`
 *   as a rebase would wipe the history the connect resync just delivered.
 * - `rotated` — the conversation identity changed under a live view.
 */
export type RebaseVerdict = "unchanged" | "adopted" | "rotated";

export interface SessionRebaseOptions {
  log?: (level: SessionRebaseLogLevel, message: string) => void;
}

/**
 * The vendor session uuid an ingest batch announces, or "" when it announces
 * none. A pre-init `SessionView` legitimately carries no uuid and must never be
 * read as one; the last announcement in the batch wins, as it does in the
 * store.
 */
export function claudeSessionIdOf(effects: readonly AdapterEffect[]): string {
  let id = "";
  for (const effect of effects) {
    if (effect.kind === "session-view" && effect.value.claudeSessionId !== "") {
      id = effect.value.claudeSessionId;
    }
  }
  return id;
}

export class SessionRebase {
  /** The vendor session uuid this view currently belongs to; "" before the first. */
  private current = "";

  constructor(private readonly opts: SessionRebaseOptions = {}) {}

  /** The uuid this view currently belongs to, for a caller's own bookkeeping. */
  get claudeSessionId(): string {
    return this.current;
  }

  /**
   * Rule on one announced uuid, adopting it. An empty announcement is
   * `unchanged` and never clears what is already known — a pre-init frame is
   * silence, not a retraction.
   */
  observe(claudeSessionId: string): RebaseVerdict {
    if (claudeSessionId === "" || claudeSessionId === this.current) return "unchanged";
    const previous = this.current;
    this.current = claudeSessionId;
    if (previous === "") return "adopted";
    // LOUD, and naming both: a rebase discards drawn conversation, which is the
    // last thing that should happen unexplained in a log.
    this.opts.log?.(
      "warn",
      `session rebase: vendor session uuid rotated ${previous} -> ${claudeSessionId} — ` +
        "the conversation retired its store seq space, so this end drops the retired space's " +
        "items and seq marks and re-requests the new space from the daemon's replay floor",
    );
    return "rotated";
  }

  /**
   * Forget the current identity, for a view rebound onto a DIFFERENT daemon
   * session (`swapTo`). The successor's first uuid is a first adoption, not a
   * rotation: its store was reset with the swap and has nothing to rebase.
   */
  forget(): void {
    this.current = "";
  }
}
