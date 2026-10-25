/**
 * The webhost's sidebar: drawn ONCE, here, for every workspace page.
 *
 * The sidebar is the same in every workspace's page, and every page used to
 * draw its own copy. A copy in a page that had gone cold was out of date, so
 * switching to it showed a stale sidebar that then redrew (the selected row
 * jumped), and every copy redrew on every sidebar update while being 91% of
 * its page's elements (docs/investigations/2026-10-13-page-cost-by-component.md).
 * Drawn here, there is one copy, it is always current, and a switch changes
 * only which page is shown beside it.
 *
 * SO THE WEBHOST TALKS TO THE DAEMON. It opens its own connection to the
 * daemon Emacs last named (rpc/daemon-origin.ts: the one that served it, until
 * a handover moves the workspaces on), registers as a page of its own
 * on that connection (`WatchPage`, with no workspace), and logs as the webhost
 * (`ClientLogRequest.source.webhost`), which the daemon files centrally. Its
 * failures go to every page's warning chip (host.ts `setFailures`), the
 * webapp's one error surface.
 *
 * WHERE IT IS DRAWN. Each page still lays out a sidebar column, kept empty
 * while the webhost's sidebar is docked over it (embed.ts), so the page's main
 * column and docked gate sit where they always did. The sidebar is drawn over
 * that column, ending above the gate band of the page on screen (host.ts
 * `SIDEBAR_BOTTOM_PROPERTY`).
 */
import type { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { createTicker } from "../clock.js";
import { mintConnectionId } from "../connection-id.js";
import { createLocalFailures } from "../failure/local.js";
import { bootFailed } from "../failure/sink.js";
import { bindLogContext, log, setLogger, type ForwardingLogger } from "../log.js";
import { installForwardingLogger, installTextAffordances } from "../document-runtime.js";
import { beginPage, endPageAtPagehide, stopAtPageTeardown } from "../page-teardown.js";
import { createAgentReplClient, type AgentReplClient } from "../rpc/client.js";
import { createDaemonContext } from "../rpc/context.js";
import { createPageTransport } from "../rpc/transport.js";
import type { DaemonOrigin } from "../rpc/daemon-origin.js";
import { readWebappBuild } from "../webapp-build.js";
import { mountWebhostSidebar } from "../sidebar/sidebar.js";
import type { Host } from "./host.js";
import { installLoadCensus, type LoadCensus } from "./load-census.js";

/** The id the webhost's sidebar element wears: the page's own, so one stylesheet draws both. */
export const WEBHOST_SIDEBAR_ID = "ws-sidebar";

/** What the start may have injected, for a suite that drives it. */
export interface WebhostSidebarStartDeps {
  /** The daemon client; the real one dials the webhost's own origin. */
  client?: AgentReplClient;
  /** Install the webhost's logger; the real one is the page-wide logger. */
  installLogger?: (logger: ForwardingLogger) => void;
  /** Mount the sidebar; the real one opens the roster stream. */
  mount?: typeof mountWebhostSidebar;
  /** Install the load census; the real one times the webhost's own document. */
  installCensus?: (doc: Document) => LoadCensus;
  /** The webhost's live log threshold (`Host.setLogLevel`); info when absent. */
  level?: LevelWindow;
  /** The daemon the sidebar dials (`Host.setDaemon` moves it); the webhost's own origin when absent. */
  daemon?: DaemonOrigin;
  /** The webapp build the webhost was loaded from; read off its own entry script when absent. */
  webappBuild?: () => string;
}

/**
 * Start the sidebar in NAV for the webhost HOST serves from WIN's origin.
 *
 * A sidebar that cannot start leaves the pages working: the failure is logged
 * and listed in every page's warning chip, never thrown past the webhost.
 * Answers the stop, which closes the sidebar and its connection.
 */
export function startWebhostSidebar(
  doc: Document,
  win: Window,
  host: Host,
  nav: HTMLElement,
  deps: WebhostSidebarStartDeps = {},
): { dispose(): void } {
  // THE WEBHOST ENDS AT ITS UNLOAD as a workspace page does (page-teardown.ts):
  // every request its transport made is aborted, so its standing streams (the
  // sidebar's roster) neither answer nor reopen into a document that is gone.
  beginPage();
  const connectionId = mintConnectionId();
  const client =
    deps.client ??
    createAgentReplClient(createPageTransport(deps.daemon ?? win.location.origin, "webhost", connectionId));
  bindLogContext({ connection_id: connectionId });
  installForwardingLogger(() => client, { case: "webhost", value: {} }, deps.level ?? "info", deps.installLogger ?? setLogger);

  // THE WEBHOST'S FAILURES ARE LISTED IN EVERY PAGE'S CHIP: the whole set,
  // every time it changes.
  const failures = createLocalFailures();
  failures.subscribe(() => {
    host.setFailures(failures.standing());
  });

  // THE LOAD EVERY PAGE PUTS ON THE ONE WEB PROCESS (load-census.ts), once
  // the webhost's records have somewhere to go.
  const census = (deps.installCensus ?? installLoadCensus)(doc);

  let stop = (): void => undefined;
  try {
    log.info("the webhost is starting its sidebar", { operation: "webhost.sidebar-start", context: {} });
    // Text in the sidebar copies and selects exactly as it does in a page.
    installTextAffordances(doc);
    // THE WEBHOST STATES ITS BUILD on its page stream: a handover no longer
    // reloads it, so a deploy that built another reloads it instead.
    const webhostBuild = (deps.webappBuild ?? (() => readWebappBuild(doc)))();
    const ctx = createDaemonContext({ client, ticker: createTicker(), failures, page: connectionId, webhostBuild });
    // An unloaded webhost stops at once: its requests, and its census.
    const unwatch = endPageAtPagehide(win);
    stopAtPageTeardown(() => census.stop());
    const sidebar = (deps.mount ?? mountWebhostSidebar)(nav, ctx, {
      // THE PAGES RESERVE THE COLUMN ONLY ONCE THERE IS A SIDEBAR TO FILL IT,
      // so a webhost whose sidebar never draws leaves the pages' layout as a
      // page on its own would have it.
      revealed: () => {
        log.info("the webhost's sidebar drew; docking it over every page", {
          operation: "webhost.sidebar-docked",
          context: {},
        });
        host.setSidebarDocked(true);
      },
    });
    // A SWITCH MOVES THE RAIL'S HIGHLIGHT WITH THE PAGE (sidebar.ts
    // `markShown`), not a daemon round trip later.
    const stopShown = host.onShown((id) => {
      sidebar.markShown(id);
    });
    stop = () => {
      unwatch();
      stopShown();
      sidebar.dispose();
      ctx.quiesce();
    };
  } catch (err) {
    const cause = err instanceof Error ? `${err.name}: ${err.message}` : String(err);
    log.error(`the webhost's sidebar failed to start: ${cause}`, {
      operation: "webhost.sidebar-start-failed",
      context: { cause },
    });
    failures.report(bootFailed(cause));
  }
  return {
    dispose: () => {
      census.stop();
      stop();
    },
  };
}
