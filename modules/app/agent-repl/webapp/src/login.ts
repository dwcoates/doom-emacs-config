/**
 * Claude account login, requested from the topbar.
 *
 * The browser cannot run the login itself and neither can the daemon: the
 * Claude OAuth flow is an interactive TUI that needs a controlling
 * terminal. Emacs is the only TTY host in the system, so the button's job
 * is to ask the daemon (POST /sessions/{id}/login) to hand the request to
 * Emacs over the sentinel side channel, where the login opens in a vterm.
 *
 * WHICH account gets logged into is not this module's business: it is
 * derived host-side from the session's cwd (~/.claude-chesscom under
 * $MULTI_REPO_ROOT, ~/.claude elsewhere), exactly as the account the
 * session's own CLI already runs under is derived.
 */

/** Notice shown once Emacs has been asked to open the login. */
export const LOGIN_OPENING = "opening login in Emacs";

/** Notice shown when the request never reached Emacs. */
export const LOGIN_FAILED = "login request failed";

/** Lifecycle of the login the topbar requested. */
export type LoginPhase = "opening" | "failed";

/** Topbar text for a login phase. */
export function loginNotice(phase: LoginPhase): string {
  switch (phase) {
    case "opening":
      return LOGIN_OPENING;
    case "failed":
      return LOGIN_FAILED;
  }
}

/**
 * Ask the daemon to open the interactive Claude login for this session's
 * account.
 *
 * Rejects on any non-2xx. A login that never opened must be surfaced: a
 * button that silently does nothing is worse than no button, because the
 * user would sit waiting on a terminal that is never coming.
 */
export async function requestLogin(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<LoginPhase> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/login`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
  });
  if (!resp.ok) {
    throw new Error(`POST /sessions/${sessionId}/login: ${resp.status} ${await resp.text()}`);
  }
  return "opening";
}
