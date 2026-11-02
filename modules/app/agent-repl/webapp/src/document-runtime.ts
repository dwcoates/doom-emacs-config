/**
 * document-runtime — what each of the webapp's two documents sets up the same
 * way: a workspace page, and the webhost that draws the sidebar over them.
 *
 * Both begin as a page, and forward every log record to the daemon over their own connection, as
 * their own source (`ClientLogRequest.source`), and both make their text
 * copyable and selectable. Done here once, so the two cannot drift.
 */
import { codeOfBlockControl } from "./code-block.js";
import { boundCopyText, installCopyControls } from "./copy-control.js";
import { installCopyChords, installCopyFallback } from "./copy.js";
import type { LevelWindow } from "../../agent-shim/logging/ts/level-window.js";
import { mintConnectionId } from "./connection-id.js";
import { bindLogContext, defaultConsole, ForwardingLogger, setLogger, type ClientLogLevel, type RuntimeLogContext } from "./log.js";
import { beginPage } from "./page-teardown.js";
import { createAgentReplClient, type AgentReplClient } from "./rpc/client.js";
import { clientLogSink, type ClientLogSource } from "./rpc/client-log-sink.js";
import type { DaemonOrigin } from "./rpc/daemon-origin.js";
import { createPageTransport, type PageClient } from "./rpc/transport.js";
import { installSelectionClickGuard } from "./selection.js";

/** What a document's runtime is made of (`startPageRuntime`). */
export interface PageRuntimeSpec {
  /** The daemon the document's calls go to. */
  readonly daemon: string | DaemonOrigin;
  /** Which document the connection's requests say they came from. */
  readonly kind: PageClient;
  /** Whose records the document forwards. */
  readonly source: ClientLogSource;
  /** The identity every record carries besides the connection. */
  readonly context?: Omit<RuntimeLogContext, "connection_id">;
  /** The document's live log threshold. */
  readonly level: ClientLogLevel | LevelWindow;
  /** The document's client; one on the page's transport, on the minted connection, when absent. */
  readonly client?: AgentReplClient;
  /** Install the document's logger; the page-wide logger slot when absent. */
  readonly installLogger?: (logger: ForwardingLogger) => void;
  /** Told every record written, after the console has it. */
  readonly onRecord?: (level: ClientLogLevel, line: string) => void;
}

/**
 * Begin the document as a page and install its logger, per SPEC: its
 * connection minted, its client made, its records bound to the connection and
 * SPEC's context, and every one forwarded through the client. Answers the
 * connection and the client.
 *
 * THE PAGE BEGINS FIRST: every request its transport makes carries the page's
 * end (page-teardown.ts), which `beginPage` renews, so it must precede the
 * transport. NOTHING HERE LOGS, so it is safe to run before the logger exists.
 */
export function startPageRuntime(spec: PageRuntimeSpec): { connectionId: string; client: AgentReplClient } {
  beginPage();
  const connectionId = mintConnectionId();
  const client = spec.client ?? createAgentReplClient(createPageTransport(spec.daemon, spec.kind, connectionId));
  bindLogContext({ ...spec.context, connection_id: connectionId });
  installForwardingLogger(() => client, spec.source, spec.level, spec.installLogger ?? setLogger, spec.onRecord);
  return { connectionId, client };
}

/**
 * Install the document's logger: every record from THRESHOLD up forwarded to
 * the daemon over GETCLIENT's client as SOURCE's. INSTALL defaults to the
 * page-wide logger slot. ON_RECORD, when given, is told every record written
 * (its level and its JSON line) after the console has it. Answers the logger
 * installed.
 */
export function installForwardingLogger(
  getClient: () => AgentReplClient,
  source: ClientLogSource,
  threshold: ClientLogLevel | LevelWindow = "info",
  install: (logger: ForwardingLogger) => void = setLogger,
  onRecord?: (level: ClientLogLevel, line: string) => void,
): ForwardingLogger {
  const write =
    onRecord === undefined
      ? defaultConsole
      : (level: ClientLogLevel, line: string): void => {
          defaultConsole(level, line);
          onRecord(level, line);
        };
  const logger = new ForwardingLogger(clientLogSink(getClient, source), write, {}, threshold);
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
  const removeCodeCopy = installCopyControls(doc, { code: codeOfBlockControl, prompt: boundCopyText, response: boundCopyText });
  const removeGuard = installSelectionClickGuard(doc);
  return () => {
    removeCopy();
    removeChords();
    removeCodeCopy();
    removeGuard();
  };
}
