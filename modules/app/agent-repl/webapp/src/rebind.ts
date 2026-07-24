/**
 * Client-side session rebind: the "session gone" escape hatch.
 *
 * The daemon makes its s_ ids survive restarts (registry + rehydration),
 * so a failed existence probe should be rare — the
 * id is genuinely unresolvable (transcript pruned, fake session,
 * registry lost). Even then the CONVERSATION usually still exists under
 * its durable CLI uuid, which every hello frame carries. This module
 * persists that uuid (plus the cwd POST /sessions needs) per daemon
 * session id, and trades it for a fresh session via
 * POST /sessions {cwd, resume} so the live view rebinds instead of
 * dead-ending. The headless remediation analyst is the caller's LAST
 * resort, reached only when rehydrate (daemon-side) and rebind (here)
 * have both failed.
 */

import { log } from "./wslog.js";

/** What a rebind needs to re-create the conversation's session. */
export interface ResumeKeys {
  claudeSessionId: string;
  cwd: string;
}

const KEY_PREFIX = "agent-repl.resume.";

/**
 * Persist the resume keys for sessionId. A hello without a
 * claude_session_id (pre-init) carries nothing durable and is skipped —
 * an earlier, filled-in record must not be overwritten by it.
 */
export function rememberResumeKeys(storage: Storage, sessionId: string, keys: ResumeKeys): void {
  if (keys.claudeSessionId === "") return;
  storage.setItem(KEY_PREFIX + sessionId, JSON.stringify(keys));
}

/** Recall sessionId's resume keys; null when absent or unusable. */
export function recallResumeKeys(storage: Storage, sessionId: string): ResumeKeys | null {
  const raw = storage.getItem(KEY_PREFIX + sessionId);
  if (raw === null) {
    // Normal/expected: nothing was ever stored for this id (or it was
    // never a resumable session). The caller escalates straight to
    // remediation — this is just a breadcrumb for WHY.
    log("info", `rebind: no resume record for ${sessionId} — nothing to rebind with`);
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
    );
    return null;
  }
  if (typeof parsed.claudeSessionId !== "string" || parsed.claudeSessionId === "") {
    log(
      "error",
      `rebind: resume record for ${sessionId} lacks claudeSessionId (parsed=${JSON.stringify(parsed)}) — falling through to remediation`,
    );
    return null;
  }
  return {
    claudeSessionId: parsed.claudeSessionId,
    cwd: typeof parsed.cwd === "string" ? parsed.cwd : "",
  };
}

/** Creates a session from the stored resume keys, returning its new id. */
export type SessionCreator = (args: {
  cwd: string;
  resumeClaudeSessionId: string;
}) => Promise<string>;

/**
 * Rebind a gone session: create a successor from the stored resume keys and
 * return its id. Returns null when no keys were ever stored (nothing to
 * rebind with — the caller escalates to remediation); rejects when the create
 * fails (same escalation, but loudly distinguishable from "nothing stored").
 * The keys migrate to the successor id so a SECOND loss rebinds too.
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
  const successor = await createSession({
    cwd: keys.cwd,
    resumeClaudeSessionId: keys.claudeSessionId,
  });
  rememberResumeKeys(storage, successor, keys);
  storage.removeItem(KEY_PREFIX + sessionId);
  return successor;
}
