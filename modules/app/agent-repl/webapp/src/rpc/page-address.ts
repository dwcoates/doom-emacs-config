/**
 * The page's address: which workspace this webview is showing, and whether the
 * browser-local dev composer is on.
 *
 * THE WORKSPACE IS REQUIRED AND HAS NO DEFAULT. Identities are daemon-minted
 * opaque tokens — a client never derives one from a path and never guesses —
 * so a page opened without `?workspace=` is addressed at nothing and cannot
 * mount anything. That is a BOOT FAILURE, which is why this throws rather than
 * returning a "no workspace" state nothing downstream could draw.
 *
 * `&composer=1` turns on the browser-local composer. Production runs
 * composer-less: the root composer is host-native (Emacs), and this flag
 * exists for developing the composer surface in an ordinary browser tab.
 *
 * Any other query parameter is IGNORED, not refused: the address is what this
 * page reads out of the URL, and an unrecognized parameter is somebody else's
 * (a cache buster, a host's own bookkeeping), never a view instruction.
 */
export interface PageAddress {
  /** The daemon-minted workspace id, URL-decoded and echoed verbatim after. */
  workspaceId: string;
  /** Whether the browser-local dev composer is enabled. */
  composer: boolean;
}

/** Read the page address out of a `location.search` string. */
export function pageAddress(search: string): PageAddress {
  const params = new URLSearchParams(search);
  const workspaceId = params.get("workspace");
  if (workspaceId === null || workspaceId === "") {
    throw new Error("the page address carries no ?workspace=<id>; there is nothing to show");
  }
  return { workspaceId, composer: params.get("composer") === "1" };
}
