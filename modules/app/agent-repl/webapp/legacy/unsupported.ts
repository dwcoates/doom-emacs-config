/**
 * Unsupported slash commands: spotting the CLI's refusal, and offering to
 * engineer it away.
 *
 * A slash command the CLI implements only as an interactive terminal panel
 * cannot run in this session — the daemon drives the CLI headlessly, so
 * the command's own dispatcher refuses it and the turn ends with nothing
 * but a refusal string. The refusal reaches us as the result frame's
 * `result_text`, since the CLI answers these locally and never queries the
 * model at all.
 *
 * That is a gap in the GUI, not a verdict: the data behind the command
 * still exists. So the feed renders the refusal with a button that asks
 * the daemon to have Emacs open a workspace which builds the feature
 * properly. The daemon relays, Emacs generates.
 */

/**
 * The CLI's refusal wordings, both of which mean "this environment cannot
 * run it" and so both of which are worth a workspace.
 *
 * `cmd_unavailable_headless`: the command is a known builtin the headless
 * session never registered, so it never resolved at all.
 *
 * `cmd_local_jsx_headless`: the command DID resolve, but it is an
 * interactive panel with no terminal to draw in.
 *
 * Deliberately NOT matched: "isn't available in this session.", which the
 * CLI emits for a command disabled by POLICY. Nothing is missing there and
 * a workspace could not add it back, so treating it as a support gap would
 * send an agent to fight a deliberate setting.
 */
const REFUSAL_PATTERNS: readonly RegExp[] = [
  /^\/([A-Za-z0-9:_-]+) isn't available in this environment\.$/,
  /^\/([A-Za-z0-9:_-]+) opens an interactive panel and isn't available in this environment\./,
];

/**
 * The command name a refusal names, or null when the text is not a
 * refusal. Anchored at the start so an ordinary answer that merely quotes
 * a refusal cannot be mistaken for one.
 */
export function parseUnsupportedCommand(resultText: string | undefined): string | null {
  if (!resultText) return null;
  const text = resultText.trim();
  for (const re of REFUSAL_PATTERNS) {
    const m = re.exec(text);
    if (m) return m[1];
  }
  return null;
}

/**
 * Ask the daemon to have Emacs open a workspace that adds graphical
 * support for `command`.
 *
 * Resolves to the workspace name Emacs was asked for. Rejects on any
 * non-2xx: a request that never landed must be surfaced, never swallowed
 * into a button that silently did nothing.
 */
export async function requestSupportWorkspace(
  httpBase: string,
  sessionId: string,
  command: string,
  fetchFn: typeof fetch = fetch,
): Promise<string> {
  const resp = await fetchFn(`${httpBase}/sessions/${encodeURIComponent(sessionId)}/add-support`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ command }),
  });
  if (!resp.ok) {
    throw new Error(`POST /add-support: ${resp.status} ${await resp.text()}`);
  }
  const body = (await resp.json()) as { workspace: string };
  return body.workspace;
}
