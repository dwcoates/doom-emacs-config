/**
 * Harness-injected ("meta") spans inside a user turn.
 *
 * Not every word of a `user-turn` frame was typed by the user. The Emacs
 * host prepends a read-directive pointing at the metaprompt file on every
 * Nth send, and a workspace-generation first send additionally carries an
 * autonomous-execution preamble and a wrap-up gate ("…invoke the
 * /workspace-merge skill…"). The agent must receive all of it; the person
 * reading the feed wants only their own prompt back.
 *
 * The host brackets each injected span with the inert HTML-comment markers
 * below (see `agent-repl--meta-wrap` in core.el, and shared/protocol.md
 * §2.3). Marking happens at the injection site, so the frontend never has
 * to recognize — or stay in sync with — the injected text itself.
 */

/** Opening marker of a harness-injected span (mirrors `agent-repl--meta-open`). */
export const META_OPEN = "<!--agent-repl:meta-->";

/** Closing marker of a harness-injected span (mirrors `agent-repl--meta-close`). */
export const META_CLOSE = "<!--/agent-repl:meta-->";

/**
 * TEXT with every marked span (markers included) removed, trimmed.
 *
 * An unpaired opening marker is left verbatim rather than swallowing the
 * rest of the turn: a truncated span is a host bug, and showing it beats
 * silently eating the user's prompt along with it.
 */
export function stripMetaSpans(text: string): string {
  let out = "";
  let cursor = 0;
  for (;;) {
    const open = text.indexOf(META_OPEN, cursor);
    if (open === -1) break;
    const close = text.indexOf(META_CLOSE, open + META_OPEN.length);
    if (close === -1) break;
    out += text.slice(cursor, open);
    cursor = close + META_CLOSE.length;
  }
  out += text.slice(cursor);
  return out.trim();
}
