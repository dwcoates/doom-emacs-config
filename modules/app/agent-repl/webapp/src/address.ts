/**
 * The page ADDRESS: what this webapp instance renders, read from its own URL,
 * and the scoped daemon socket that address opens.
 *
 * A page is addressed by WORKSPACE — `?workspace=<absolute directory path>`,
 * URL-encoded — and rides the daemon's workspace-scoped stream. The parameter
 * carries the RAW directory path rather than a hash or an opaque handle: the
 * daemon's connection scope and every `FrontendCommand` already route on that
 * string, so a handle would make the browser the only participant keyed
 * differently, and the routing key stays greppable across a URL bar, a daemon
 * log and a frame alike.
 *
 * A workspace address is STABLE FOR THE PAGE'S WHOLE LIFE. No session id is
 * ever written into the URL, so a reload, a bookmark, a restored tab and a
 * remounted webview all attach to the same workspace, and a session rotating
 * underneath the view never turns the URL into a stale-identity attach. Which
 * session a workspace owns is the daemon's ruling, re-read from every arriving
 * frame.
 *
 * `?session=<id>` addresses a page at ONE session instead, for a viewer that
 * has a session and no workspace to render (the config explainer's popup). A
 * page given neither is UNADDRESSED: it creates a session over the command
 * plane and renders that, holding the resulting id as internal state only.
 */

/** The absolute-directory-path parameter naming the workspace to render. */
export const WORKSPACE_PARAM = "workspace";
/** The session-id parameter naming a single session to render. */
export const SESSION_PARAM = "session";

/** What a page renders, and therefore which scoped socket it opens. */
export type PageAddress =
  | { readonly kind: "workspace"; readonly workspace: string }
  | { readonly kind: "session"; readonly sessionId: string }
  | { readonly kind: "unaddressed" };

/**
 * Read the page's address off its query parameters.
 *
 * A `workspace` that is not an absolute directory path is REFUSED here, loudly
 * and immediately, rather than handed to the daemon: the daemon would reject it
 * too, but as a failed WebSocket handshake several layers from the malformed
 * URL that caused it.
 *
 * @throws when a `workspace` value is present but names no absolute directory.
 */
export function pageAddress(params: URLSearchParams): PageAddress {
  const workspace = params.get(WORKSPACE_PARAM);
  if (workspace !== null && workspace !== "") {
    if (!workspace.startsWith("/")) {
      throw new Error(
        `?${WORKSPACE_PARAM}=${workspace} is not an absolute directory path; ` +
          "a page addresses its workspace by full path",
      );
    }
    return { kind: "workspace", workspace };
  }
  const sessionId = params.get(SESSION_PARAM);
  if (sessionId !== null && sessionId !== "") return { kind: "session", sessionId };
  return { kind: "unaddressed" };
}

/**
 * The daemon WebSocket URL serving `address`, under the `wsBase` origin
 * (`ws://host:port`).
 *
 * @throws for an unaddressed page: there is no scoped socket to open until its
 * session exists, and guessing one would attach the page to a conversation
 * nobody asked for.
 */
export function scopedStreamUrl(wsBase: string, address: PageAddress): string {
  switch (address.kind) {
    case "workspace":
      return `${wsBase}/workspace-stream?${WORKSPACE_PARAM}=${encodeURIComponent(address.workspace)}`;
    case "session":
      return `${wsBase}/sessions/${encodeURIComponent(address.sessionId)}/stream`;
    case "unaddressed":
      throw new Error("an unaddressed page has no scoped stream; create its session first");
  }
}

/**
 * How `address` names itself in a log record and a diagnostic message.
 */
export function addressLabel(address: PageAddress): string {
  switch (address.kind) {
    case "workspace":
      return `workspace ${address.workspace}`;
    case "session":
      return `session ${address.sessionId}`;
    case "unaddressed":
      return "unaddressed";
  }
}
