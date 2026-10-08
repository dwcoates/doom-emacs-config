/**
 * The page's address: which workspace this webview is showing, where that
 * workspace lives on the daemon's host, and whether the browser-local dev
 * composer is on.
 *
 * `?workspace=<id>&dir=<dir>` — BOTH REQUIRED, both URL-encoded. The id is the
 * identity (daemon-minted, opaque, compared byte-wise, never derived from a
 * path); the dir is the normalized worktree directory the ref carries for
 * display and for opening files. A page missing either is addressed at nothing
 * and cannot mount anything, which is a BOOT FAILURE — hence a throw rather
 * than a "no workspace" state nothing downstream could draw.
 *
 * `&log_level=<level>` carries the daemon process's effective
 * `AGENT_REPL_LOG_LEVEL` into the browser, whose JavaScript cannot read process
 * environment directly. A missing value uses the contract's `info` level for
 * ordinary browser development; an invalid value is a boot failure.
 * `&log_level_until=<unix seconds>` is the end of that level's window: a level
 * other than info holds only inside it (proto/vocab/log-level-window.json), so
 * a page reloaded from an address whose window has ended boots at info.
 *
 * `&composer=1` turns on the browser-local composer. Production runs
 * composer-less: the root composer is host-native (Emacs), and this flag
 * exists for developing the composer surface in an ordinary browser tab.
 *
 * Any other query parameter is IGNORED, not refused: the address is what this
 * page reads out of the URL, and an unrecognized parameter is somebody else's
 * (a cache buster, a host's own bookkeeping), never a view instruction.
 */
import { parseClientLogLevel, type ClientLogLevel } from "../log.js";

export interface PageAddress {
  /** The daemon-minted workspace id, URL-decoded and echoed verbatim after. */
  workspaceId: string;
  /** The workspace's normalized worktree directory. Display, not identity. */
  workspaceDir: string;
  /** Whether the browser-local dev composer is enabled. */
  composer: boolean;
  /** The effective `AGENT_REPL_LOG_LEVEL` delivered by the host. */
  logLevel: ClientLogLevel;
  /** The end of that level's window, Unix seconds as delivered; undefined when absent. */
  logLevelUntil: string | undefined;
}

/** Read the page address out of a `location.search` string. */
export function pageAddress(search: string): PageAddress {
  const params = new URLSearchParams(search);
  const workspaceId = params.get("workspace");
  if (workspaceId === null || workspaceId === "") {
    throw new Error("the page address carries no ?workspace=<id>; there is nothing to show");
  }
  const workspaceDir = params.get("dir");
  if (workspaceDir === null || workspaceDir === "") {
    throw new Error("the page address carries no &dir=<dir>; the workspace ref cannot be built");
  }
  return {
    workspaceId,
    workspaceDir,
    composer: params.get("composer") === "1",
    logLevel: parseClientLogLevel(params.get("log_level")),
    logLevelUntil: params.get("log_level_until") ?? undefined,
  };
}
