/**
 * connection-id — a loaded document's own identity, for correlating its
 * records against the daemon's. Both of the webapp's documents mint one: a
 * workspace page, and the webhost.
 */

/**
 * This page's own identity, for correlating its records against the daemon's.
 *
 * A page, not a session: it is minted per load and says nothing about which
 * conversation the workspace owns. `crypto.randomUUID` is the browser's, with
 * a time-plus-entropy fallback for a context that does not expose it (an old
 * webview, a non-secure origin) — an unidentifiable page's logs are worth less
 * than a page that refuses to boot over the identifier.
 */
export function mintConnectionId(): string {
  const uuid = globalThis.crypto?.randomUUID?.();
  if (uuid !== undefined) return uuid;
  return `web-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
}
