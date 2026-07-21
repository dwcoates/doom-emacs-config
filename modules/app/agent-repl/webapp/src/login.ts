/**
 * The Claude account login: everything about it EXCEPT the renderer.
 *
 * WHY A TERMINAL AND NOT A FORM
 *
 * The login is a full-screen TUI, and it is gated behind stateful prompts
 * (theme onboarding on a fresh config root, folder trust on an untrusted
 * cwd) before it ever reaches the OAuth step. Nothing can reliably scrape
 * that, which is why the login used to be exiled to an Emacs vterm where a
 * human could read it. The GUI keeps the human and moves the terminal: the
 * daemon owns the pty, the webapp renders it, and no code anywhere parses
 * the TUI. It survives a Claude Code redesign for the same reason the vterm
 * did.
 *
 * The xterm.js binding lives in login-terminal.ts, deliberately apart from
 * this module: xterm is a browser bundle that cannot even be imported under
 * the test runner, so keeping it out of here is what lets the wire protocol
 * below be tested at all.
 *
 * WHICH account gets logged into is not this module's business. The daemon
 * reads it off the session's own recorded CLAUDE_CONFIG_DIR, the same value
 * the session's CLI already runs under.
 */

/** Notice shown while the login terminal is up. */
export const LOGIN_OPEN = "login terminal open";

/** Notice shown when the login never opened. */
export const LOGIN_FAILED = "login failed to open";

/** Notice shown once the login child is gone. */
export const LOGIN_CLOSED = "login closed";

/** Lifecycle of the login the topbar requested. */
export type LoginPhase = "open" | "closed" | "failed";

/** Topbar text for a login phase. */
export function loginNotice(phase: LoginPhase): string {
  switch (phase) {
    case "open":
      return LOGIN_OPEN;
    case "closed":
      return LOGIN_CLOSED;
    case "failed":
      return LOGIN_FAILED;
  }
}

/** The account a login was opened against, as the daemon reports it. */
export interface LoginOpened {
  /** CLAUDE_CONFIG_DIR, empty for the CLI's own default root. */
  account: string;
  /** Path of the terminal socket to attach to. */
  terminal_url: string;
}

/**
 * Ask the daemon to open the login terminal for this session's account.
 *
 * Rejects on any non-2xx. A login that never opened must be surfaced: a
 * button that silently does nothing is worse than no button, because the
 * user would sit waiting on a terminal that is never coming.
 */
export async function requestLogin(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<LoginOpened> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/login`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
  });
  if (!resp.ok) {
    throw new Error(
      `POST /sessions/${sessionId}/login: ${resp.status} ${await resp.text()}`,
    );
  }
  return (await resp.json()) as LoginOpened;
}

/**
 * Ask the daemon to end the login terminal for this session's account.
 *
 * Closing one that is not running is a success, so this rejects only on a
 * real failure.
 */
export async function closeLogin(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<void> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/login`, {
    method: "DELETE",
  });
  if (!resp.ok) {
    throw new Error(
      `DELETE /sessions/${sessionId}/login: ${resp.status} ${await resp.text()}`,
    );
  }
}

/** The URL of a session's login terminal socket. */
export function loginTerminalUrl(wsBase: string, sessionId: string): string {
  return `${wsBase}/sessions/${sessionId}/login/terminal`;
}

/**
 * The geometry control frame, the only thing this socket sends as text.
 *
 * Not cosmetic: the TUI hard-wraps at the column count and the OAuth URL
 * runs ~350 characters, so a narrow terminal splits it across five lines.
 */
export function resizeFrame(rows: number, cols: number): string {
  return JSON.stringify({ resize: { rows, cols } });
}

/** Keystrokes go to the child as raw bytes, never as text. */
export function encodeKeystrokes(chunk: string): Uint8Array {
  return new TextEncoder().encode(chunk);
}

/**
 * Terminal output as the renderer wants it.
 *
 * The socket is set to `arraybuffer`, so the daemon's binary frames arrive
 * as ArrayBuffer. A string is still accepted rather than dropped: dropping
 * screen bytes would leave the user staring at a half-drawn prompt with no
 * hint anything went wrong.
 */
export function decodeTerminalChunk(data: unknown): Uint8Array | string | null {
  if (data instanceof ArrayBuffer) return new Uint8Array(data);
  if (typeof data === "string") return data;
  return null;
}

/**
 * Names the runtime type of a payload `decodeTerminalChunk` could not
 * render, for the caller's log line.
 *
 * A dropped screen chunk is otherwise invisible: the OAuth screen just stops
 * updating with no error anywhere, so the log line needs enough detail
 * (constructor name, or `typeof` for primitives) to point at what actually
 * arrived over the socket.
 */
export function describeChunkType(data: unknown): string {
  if (data === null) return "null";
  if (data === undefined) return "undefined";
  const ctorName = (data as { constructor?: { name?: string } }).constructor
    ?.name;
  return ctorName ?? typeof data;
}
