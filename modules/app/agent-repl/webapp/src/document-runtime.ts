/**
 * document-runtime — what each of the webapp's two documents sets up the same
 * way: a workspace page, and the webhost that draws the sidebar over them.
 *
 * Both forward every log record to the daemon over their own connection, as
 * their own source (`ClientLogRequest.source`), and both make their text
 * copyable and selectable. Done here once, so the two cannot drift.
 */
import { codeOfCopyControl } from "./code-block.js";
import { boundCopyText, installCopyControls } from "./copy-control.js";
import { installCopyChords, installCopyFallback } from "./copy.js";
import type { LevelWindow } from "../../agent-shim/logging/ts/level-window.js";
import { ForwardingLogger, setLogger, type ClientLogLevel } from "./log.js";
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
 * Make every surface of DOC copyable (copy.ts's fallback and copy chords, and
 * copy-control.ts's copy controls) and every drag-select activate
 * nothing (selection.ts's click guard). Answers the uninstall.
 */
export function installTextAffordances(doc: Document): () => void {
  const removeCopy = installCopyFallback(doc);
  const removeChords = installCopyChords(doc);
  const removeCodeCopy = installCopyControls(doc, { code: codeOfCopyControl, prompt: boundCopyText, response: boundCopyText });
  const removeGuard = installSelectionClickGuard(doc);
  return () => {
    removeCopy();
    removeChords();
    removeCodeCopy();
    removeGuard();
  };
}
