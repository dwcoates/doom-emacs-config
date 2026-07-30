/**
 * Client-side session rebind: the "session gone" escape hatch.
 *
 * The daemon makes its s_ ids survive restarts (registry + rehydration), so a
 * failed existence probe should be rare — the id is genuinely unresolvable
 * (transcript pruned, fake session, registry lost). Even then the CONVERSATION
 * usually still exists, and the daemon can find it. This module remembers the
 * one thing the daemon cannot infer from a dead session id — WHERE the session
 * was rooted — and trades it for a fresh session bound to the same
 * conversation, so the live view rebinds instead of dead-ending. The headless
 * remediation analyst is the caller's LAST resort, reached only when rehydrate
 * (daemon-side) and rebind (here) have both failed.
 *
 * THIS MODULE USED TO PERSIST THE VENDOR CONVERSATION UUID, and traded it back
 * on `CreateSessionCmd.resume_claude_session_id`. That made the browser a
 * second authority on which conversation a workspace owns — the same mistake
 * the Emacs frontend made in its state.el, and the same failure: a stored
 * pointer that goes stale silently opens a FRESH conversation on top of an
 * intact transcript. The daemon owns (config_dir, cwd) -> conversation; it
 * holds the registry, the checkpoints, and the transcripts on disk. So the
 * rebind now sends the cwd and the INTENT to continue, and the daemon decides
 * which conversation that is. See `ResumeMode` in frontend.proto.
 */

import { log } from "./wslog.js";

/** Where a gone session was rooted — all a rebind needs. */
export interface ResumeKeys {
  cwd: string;
}

const KEY_PREFIX = "agent-repl.resume.";

/**
 * Persist the rebind record for sessionId. A record with no cwd cannot be
 * rebound from and is skipped, so an earlier usable record is never
 * overwritten by an empty one.
 */
export function rememberResumeKeys(storage: Storage, sessionId: string, keys: ResumeKeys): void {
  if (keys.cwd === "") return;
  storage.setItem(KEY_PREFIX + sessionId, JSON.stringify(keys));
}

/** Recall sessionId's rebind record; null when absent or unusable. */
export function recallResumeKeys(storage: Storage, sessionId: string): ResumeKeys | null {
  const raw = storage.getItem(KEY_PREFIX + sessionId);
  if (raw === null) {
    // Normal/expected: nothing was ever stored for this id (or it was
    // never a resumable session). The caller escalates straight to
    // remediation — this is just a breadcrumb for WHY.
    log("info", `rebind: no resume record for ${sessionId} — nothing to rebind with`, { operation: "rebind.resume-record-missing", context: { agent_repl_session_id: sessionId } });
    return null;
  }
  let parsed: Partial<ResumeKeys>;
  try {
    parsed = JSON.parse(raw) as Partial<ResumeKeys>;
  } catch (err) {
    // Unusable is as good as absent for the recovery path, but never
    // silent: a corrupt record means something wrote garbage here, and
    // that garbage now blocks rebind for sessionId.
    log(
      "error",
      `rebind: corrupt resume record for ${sessionId} (${String(err)}) — falling through to remediation`,
      { operation: "rebind.resume-record-corrupt", context: { agent_repl_session_id: sessionId, cause: err } },
    );
    return null;
  }
  if (typeof parsed.cwd !== "string" || parsed.cwd === "") {
    // Also the shape a PRE-MIGRATION record has: those stored a
    // claudeSessionId alongside the cwd, and one written before the cwd was
    // recorded has nothing this path can use. Loud rather than silent,
    // because it blocks rebind for sessionId either way.
    log(
      "error",
      `rebind: resume record for ${sessionId} lacks cwd (parsed=${JSON.stringify(parsed)}) — falling through to remediation`,
      { operation: "rebind.resume-record-invalid", context: { agent_repl_session_id: sessionId, parsed } },
    );
    return null;
  }
  return { cwd: parsed.cwd };
}

/** Creates a session rooted at cwd, continuing its conversation. */
export type SessionCreator = (args: { cwd: string }) => Promise<string>;

/**
 * Rebind a gone session: create a successor rooted where the old one was and
 * return its id. Returns null when no record was ever stored (nothing to
 * rebind with — the caller escalates to remediation); rejects when the create
 * fails (same escalation, but loudly distinguishable from "nothing stored").
 * The record migrates to the successor id so a SECOND loss rebinds too.
 *
 * WHICH conversation the successor lands on is the daemon's decision, not
 * this module's: the create carries the cwd and asks to CONTINUE.
 *
 * The creator is injected because session creation is a `CreateSessionCmd` on
 * the command plane, not the POST /sessions this used to issue: it needs a
 * WebSocket the caller owns, and this stays a pure keys-and-storage unit.
 */
export async function rebindSession(
  sessionId: string,
  storage: Storage,
  createSession: SessionCreator,
): Promise<string | null> {
  const keys = recallResumeKeys(storage, sessionId);
  if (keys === null) return null;
  const successor = await createSession({ cwd: keys.cwd });
  rememberResumeKeys(storage, successor, keys);
  storage.removeItem(KEY_PREFIX + sessionId);
  return successor;
}
