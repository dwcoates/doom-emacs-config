/**
 * Auto-resume of a session that was stopped mid-task.
 *
 * A workspace can be torn down while the agent is still working — the user
 * closes it (`SPC o C`), the daemon evicts the session, the process is
 * killed. The running turn dies with the process, so when the conversation
 * is later re-opened (`SPC o c`, `/workspace-open`) the rehydrated session
 * reports no live turn and simply idles, leaving the interrupted task
 * half-done.
 *
 * This module lets the frontend recover that on its own, with no host or
 * daemon involvement. It persists — per durable CLI conversation id — a
 * "mid-task" marker that tracks whether a turn is running: a turn that
 * never completes leaves the marker set, which is exactly the signal that
 * the session was stopped mid-task. On the next fresh join the frontend
 * reads the marker and, when the rehydrated turn is not live, silently
 * sends a continue nudge so the agent picks the task back up.
 *
 * The nudge is bracketed by the host's meta markers (see `meta.ts`), so
 * `stripMetaSpans` empties it in the feed — no prompt bubble is drawn —
 * while the daemon forwards it verbatim to the agent like any other
 * user message.
 */

import { META_CLOSE, META_OPEN } from "./meta.js";

const KEY_PREFIX = "agent-repl.midtask.";

/**
 * The command the auto-resume sends to pick a stopped task back up. The
 * user's own words for this feature ("…by sending … `/continue`"), kept as
 * a named constant so the wire content has one home.
 */
export const AUTO_CONTINUE_COMMAND = "/continue";

/**
 * The hidden user-message content that resumes an interrupted task:
 * {@link AUTO_CONTINUE_COMMAND} bracketed by the host's meta markers.
 * `stripMetaSpans` empties it, so the feed draws no bubble, while the
 * daemon forwards it to the agent unchanged.
 */
export function hiddenContinueMessage(): string {
  return `${META_OPEN}${AUTO_CONTINUE_COMMAND}${META_CLOSE}`;
}

/**
 * Record whether claudeSessionId currently has a turn running. Set while a
 * turn is in flight and cleared when it completes, so a turn the process
 * never finishes leaves the marker standing — the durable trace of a
 * mid-task stop. An empty id (a pre-init or fake session) is unkeyable and
 * skipped, never written under the bare prefix.
 */
export function rememberMidTask(
  storage: Storage,
  claudeSessionId: string,
  active: boolean,
): void {
  if (claudeSessionId === "") return;
  if (active) {
    storage.setItem(KEY_PREFIX + claudeSessionId, "1");
  } else {
    storage.removeItem(KEY_PREFIX + claudeSessionId);
  }
}

/**
 * Whether claudeSessionId was stopped mid-task — its marker outlived the
 * turn that set it. `false` for an empty id or an id that never ran a turn.
 */
export function recallMidTask(storage: Storage, claudeSessionId: string): boolean {
  if (claudeSessionId === "") return false;
  return storage.getItem(KEY_PREFIX + claudeSessionId) !== null;
}

/**
 * Whether a just-restored session should silently send the continue nudge.
 *
 * Two conditions both hold for a genuine mid-task reopen:
 * - the marker is present (the conversation was stopped while a turn ran),
 * - the rehydrated turn is NOT live (`turnInFlight` false).
 *
 * The live-turn guard is what separates a mid-task reopen from a second tab
 * joining a session whose turn is genuinely still running: that join
 * reports `turnInFlight` true and is left alone, so the nudge never lands
 * on top of a turn already in progress.
 */
export function shouldAutoContinue(
  storage: Storage,
  claudeSessionId: string,
  turnInFlight: boolean,
): boolean {
  return !turnInFlight && recallMidTask(storage, claudeSessionId);
}
