/**
 * document — WHAT EACH OF EMACS'S TWO WEB VIEWS STARTS THE SAME WAY.
 *
 * Emacs keeps two web views: the host, which holds every workspace's page
 * (host.ts), and the sidebar beside it, in a window of its own that runs the
 * frame's whole height (sidebar-page.ts). Neither owns a workspace, so each
 * dials the daemon over its own connection, registers as a page of its own on
 * it (`WatchPage`, with no workspace), and logs as the webhost
 * (`ClientLogRequest.source.webhost`), which the daemon files centrally. Each
 * record names the document that wrote it (`webhost_document`), so the host's
 * and the sidebar's records stay apart in the one central log.
 *
 * Started here once, so the two documents cannot drift.
 */
import type { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { createTicker } from "../clock.js";
import { createLocalFailures, type LocalFailures } from "../failure/local.js";
import type { ClientLogLevel, ForwardingLogger } from "../log.js";
import { installTextAffordances, startPageRuntime } from "../document-runtime.js";
import { endPageAtPagehide, stopAtPageTeardown } from "../page-teardown.js";
import { createAgentReplClient, type AgentReplClient } from "../rpc/client.js";
import { createDaemonContext, type DaemonContext } from "../rpc/context.js";
import { createPageTransport } from "../rpc/transport.js";
import { webhostOriginAt, type DaemonOrigin } from "../rpc/daemon-origin.js";
import { readWebappBuild } from "../webapp-build.js";
import { installLoadCensus, type LoadCensus } from "./load-census.js";

/** Which of Emacs's two web views a document is. */
export type WebhostDocument = "host" | "sidebar";

/** What the start may have injected, for a suite that drives it. */
export interface WebhostDocumentDeps {
  /** The daemon client; the real one dials the document's own origin. */
  client?: AgentReplClient;
  /** Install the document's logger; the real one is the page-wide logger. */
  installLogger?: (logger: ForwardingLogger) => void;
  /** Install the load census; the real one times the document itself. */
  installCensus?: (doc: Document) => LoadCensus;
  /** The document's live log threshold; info when absent. */
  level?: LevelWindow;
  /** The daemon the document dials (`setDaemon` moves it); its own origin when absent. */
  daemon?: DaemonOrigin;
  /** A client for the daemon at an address; one on that daemon's host origin when absent. */
  clientAt?: (address: string) => AgentReplClient;
  /** The webapp build the document was loaded from; read off its own entry script when absent. */
  webappBuild?: () => string;
  /** Told every record the document writes, after the console has it. */
  onRecord?: (level: ClientLogLevel, line: string) => void;
}

/** A started document: its daemon context and its own failures. */
export interface StartedWebhostDocument {
  readonly ctx: DaemonContext;
  readonly failures: LocalFailures;
  /** Stop the document: its census, its pagehide watch, and its context's requests. */
  dispose(): void;
}

/**
 * Start DOCUMENT's runtime in DOC and WIN: its connection, its logger, its
 * failures, its load census and its daemon context.
 *
 * THE DOCUMENT ENDS AT ITS UNLOAD as a workspace page does (page-teardown.ts):
 * every request its transport made is aborted, so its standing streams neither
 * answer nor reopen into a document that is gone.
 */
export function startWebhostDocument(
  document: WebhostDocument,
  doc: Document,
  win: Window,
  deps: WebhostDocumentDeps = {},
): StartedWebhostDocument {
  const { connectionId, client } = startPageRuntime({
    daemon: deps.daemon ?? win.location.origin,
    kind: "webhost",
    source: { case: "webhost", value: {} },
    context: { webhost_document: document },
    level: deps.level ?? "info",
    client: deps.client,
    installLogger: deps.installLogger,
    onRecord: deps.onRecord,
  });
  const failures = createLocalFailures();
  // THE LOAD THE DOCUMENT PUTS ON ITS WEB PROCESS (load-census.ts), once its
  // records have somewhere to go.
  const census = (deps.installCensus ?? installLoadCensus)(doc);
  // Text in either document copies and selects exactly as it does in a page.
  installTextAffordances(doc);
  // THE DOCUMENT STATES ITS BUILD on its page stream: a handover no longer
  // reloads it, so a deploy that built another reloads it instead.
  const webhostBuild = (deps.webappBuild ?? (() => readWebappBuild(doc)))();
  const ctx = createDaemonContext({
    client,
    ticker: createTicker(),
    failures,
    page: connectionId,
    webhostBuild,
    // THE ORIGIN THE TRANSPORT READS: the context reopens every standing
    // stream when it moves (rpc/daemon-origin.ts).
    daemon: deps.daemon,
    // A WORKSPACE A HANDOVER HAS NOT MOVED YET is acted on at the daemon
    // that still serves it, which a not_yet_adopted refusal names.
    clientAt:
      deps.clientAt ??
      ((address) => createAgentReplClient(createPageTransport(webhostOriginAt(address), "webhost", connectionId))),
  });
  const unwatch = endPageAtPagehide(win);
  stopAtPageTeardown(() => census.stop());
  return {
    ctx,
    failures,
    dispose() {
      census.stop();
      unwatch();
      ctx.quiesce();
    },
  };
}
