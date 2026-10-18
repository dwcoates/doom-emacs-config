/**
 * document-runtime — what each of the webapp's two documents sets up the same
 * way: a workspace page, and the webhost that draws the sidebar over them.
 *
 * Both forward every log record to the daemon over their own connection, as
 * their own source (`ClientLogRequest.source`), and both make their text
 * copyable and selectable. Done here once, so the two cannot drift.
 */
import { installCopyChords, installCopyFallback } from "./copy.js";
import type { LevelWindow } from "../../agent-shim/logging/ts/level-window.js";
import { ForwardingLogger, log, setLogger, type ClientLogLevel } from "./log.js";
import type { AgentReplClient } from "./rpc/client.js";
import { clientLogSink, type ClientLogSource } from "./rpc/client-log-sink.js";
import { installSelectionClickGuard } from "./selection.js";

/**
 * Install the document's logger: every record from THRESHOLD up forwarded to
 * the daemon over GETCLIENT's client as SOURCE's. INSTALL defaults to the
 * page-wide logger slot. Answers the logger installed.
 */
export function installForwardingLogger(
  getClient: () => AgentReplClient,
  source: ClientLogSource,
  threshold: ClientLogLevel | LevelWindow = "info",
  install: (logger: ForwardingLogger) => void = setLogger,
): ForwardingLogger {
  const logger = new ForwardingLogger(clientLogSink(getClient, source), undefined, {}, threshold);
  install(logger);
  return logger;
}

/**
 * Make every surface of DOC copyable (copy.ts's fallback and copy chords) and
 * every drag-select activate nothing (selection.ts's click guard). Answers the
 * uninstall.
 */
export function installTextAffordances(doc: Document): () => void {
  const removeCopy = installCopyFallback(doc);
  const removeChords = installCopyChords(doc);
  const removeGuard = installSelectionClickGuard(doc);
  return () => {
    removeCopy();
    removeChords();
    removeGuard();
  };
}

/**
 * When WIN's document unloads for good (a `pagehide` that is not `persisted`),
 * quiet CTX and run TEARDOWN. Answers the stop. The webhost's end; a workspace
 * page ends through page-teardown.ts (`endPageAtPagehide`), which aborts every
 * request its transport made.
 *
 * WHY (2026-10-17). An unloaded document's script went on running in WebKit:
 * a workspace page whose iframe had been replaced redialled its WatchFeed
 * every few seconds for ten minutes beside its successor, failing every time
 * (`d.onConnect is not a function`: an unloaded document no longer upgrades
 * its custom elements). The webhost holds standing streams of its own (the
 * sidebar's roster), so at its unload it quiets its context, which cancels
 * every stream with no reconnect and refuses every call (rpc/context.ts), and
 * TEARDOWN stops its own timers. A document kept for the back/forward cache
 * (`persisted`) is left running, since it can be shown again.
 */
export function quietAtUnload(win: Window, ctx: { quiesce(): void }, teardown: () => void): () => void {
  const onHide = (event: PageTransitionEvent): void => {
    if (event.persisted) return;
    log.info("the document is unloading; it stops talking to the daemon", {
      operation: "document.unload-quiet",
      context: {},
    });
    ctx.quiesce();
    teardown();
  };
  win.addEventListener("pagehide", onHide);
  return () => win.removeEventListener("pagehide", onHide);
}
