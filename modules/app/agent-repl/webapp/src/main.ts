/**
 * THE BOOT.
 *
 * It does the smallest thing that makes every later mount possible, in the one
 * order the dependencies allow, and it mounts exactly one component before
 * adoption: the topbar, whose warning chip is the only surface that can report
 * the boot itself going wrong.
 *
 * THE ORDER IS FORCED, not chosen, and the forcing constraint is that
 * the canonical `log` methods REFUSE to emit without an installed sink:
 *   1. the page address, because everything below is addressed to a workspace
 *      and a page without one has nothing to show — and it logs nothing;
 *   2. the transport and client, because the LOGGER's sink is an rpc on that
 *      client — and neither logs;
 *   3. the logger, bound to this page's identity, BEFORE the first thing that
 *      logs;
 *   4. the shell, which logs, so a broken `index.html` fails by name here
 *      rather than inside a component's first draw;
 *   5. the client-local failures and the topbar that draws them in its
 *      warning chip, which also log, so there is somewhere to put a boot
 *      failure before anything that can fail is started;
 *   6. everything else.
 *
 * Steps 4 and 5 used to come before step 3, which meant `boot` threw on its
 * own first log line every single time and the page came up empty. See the
 * comment inside `boot`.
 *
 * A THROW ANYWHERE IN HERE IS `boot_failed` — the one failure that cannot be
 * carried the way the others are, since the machinery that would carry it is
 * the machinery that failed to build. It is drawn from whatever exists at the
 * time: the warning chip if step 5 got that far, and the emergency console path if
 * it did not, which is the documented exception to "no direct console".
 */
import { pageRestyleTurns } from "./host/restyle-turn.js";
import { installGateDock } from "./feed/gate-dock.js";
import { installAskTray } from "./feed/ask-tray.js";
import "./styles.css";
// The chat's serif (styles.css, `.chat-col`), bundled so every machine draws
// the same face: the optical-size axis, upright and italic.
import "@fontsource-variable/source-serif-4/opsz.css";
import "@fontsource-variable/source-serif-4/opsz-italic.css";
// KaTeX's layout and fonts, for the TeX markdown.ts typesets.
import "katex/dist/katex.min.css";
import { createTicker } from "./clock.js";
import { createComposerGate, mountComposer } from "./composer/composer.js";
import { createRowRenderers } from "./feed/renderers.js";
import { mountFeed } from "./feed/feed.js";
import { mountChat } from "./chat/chat.js";
import { landChatAtEnd } from "./scroll.js";
import { mountFooter } from "./footer/footer.js";
import { installResponseUsageHover } from "./feed/response-usage.js";
import { pageFocusLeft } from "./focus-left.js";
import { adoptAtBoot, startLifecycle } from "./lifecycle/lifecycle.js";
import { createRevealRelay } from "./lifecycle/reveal-relay.js";
import { mountLoginOverlay } from "./login/login.js";
import { answerLoginRequests } from "./login/request.js";
import { mountNewsDigest } from "./news-digest/news-digest.js";
import { mountImageOverlay } from "./image-overlay/image-overlay.js";
import { drawCommandPanel } from "./panels/panels.js";
import { liveUnshownAnimationsEnv, pauseAnimationsWhileUnshown } from "./host/unshown-animations.js";
import { mountSidebar } from "./sidebar/sidebar.js";
import { mountTopbar } from "./topbar/topbar.js";
import { installNavigationGuard, installProseLinkRouting } from "./link.js";
import { bootFailed } from "./failure/sink.js";
import { createLocalFailures, type LocalFailures } from "./failure/local.js";
import { LEVEL_WINDOW_OPERATION, bindLogContext, log } from "./log.js";
import { LevelWindow, selectionContext, selectionNote, selectLevel } from "../../agent-shim/logging/ts/level-window.js";
import { installPagePresenceLog } from "./page-presence.js";
import { installPageCensus } from "./page-census.js";
import { pageAfter } from "./every.js";
import { beginPage, endPageAtPagehide, stopAtPageTeardown } from "./page-teardown.js";
import { connectToHost } from "./host/embed.js";
import { installForwardingLogger, installTextAffordances } from "./document-runtime.js";
import { mintConnectionId } from "./connection-id.js";
import { watchShownEdge } from "./host/shown-edge.js";
import { createWebhostFailureView, mergeFailureViews } from "./failure/webhost.js";
import { createEditorIngressView } from "./held/editor-ingress.js";
import { createStreamCostMeter, regionNodes } from "./stream-cost.js";
import { pagePresence } from "./presence.js";
import { installPageDrawnReport } from "./page-drawn-report.js";
import { installLatencyReport, REPORT_LATENCIES_RESPONSE, reportLatenciesRequest } from "./latency-report.js";
import { callUnary } from "./rpc/unary.js";
import { pageSwitchLatencyEnv, watchPageBoot, watchSwitchLatency } from "./switch-latency.js";
import { watchReloadBlank } from "./reload-blank.js";
import { watchPageStall } from "./page-stall.js";
import { createPromptEcho, installPromptEcho } from "./prompt-echo.js";
import { createPageSeen } from "./page-seen.js";
import { createAgentReplClient } from "./rpc/client.js";
import { createAppContext, type AppContext } from "./rpc/context.js";
import { isHostAddress, pageAddress } from "./rpc/page-address.js";
import { createPageTransport } from "./rpc/transport.js";
import { workspaceRef } from "./rpc/workspace-ref.js";
import type { SubmitPromptCommandPanel } from "../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import type { FeedId } from "../../proto/gen/ts/frontend/v1/feed_pb";
import { shellElements } from "./shell.js";
import { composerClosedFor, installColorValues } from "./vocab.js";
import { childrenMatching } from "./dom.js";

export async function boot(): Promise<void> {
  let failures: LocalFailures | null = null;
  // HOISTED SO A FAILED BOOT CAN STOP DIALING. The page's one stream is opened
  // by `createAppContext`, which is several steps ABOVE `adoptAtBoot` — so a
  // boot that fails at adoption, or at any mount after it, leaves that stream
  // reopening on backoff against a daemon the page has already given up on,
  // forever, behind a `boot_failed` warning. See the catch.
  let opened: AppContext | null = null;
  try {
    // The status colors' values come from the shared vocabulary, never from
    // styles.css, so they go in before anything is drawn.
    installColorValues(document);
    // THE LOGGER GOES IN BEFORE THE FIRST THING THAT LOGS, AND THAT ORDER IS
    // THE WHOLE OF THIS BLOCK'S SHAPE.
    //
    // The canonical `log` methods REFUSE to emit without an installed sink --
    // `emit` throws "the
    // webapp logger is not installed" rather than discarding the record --
    // and TWO of the boot's own steps log as their first statement:
    // `shellElements` announces the shell it is resolving, and the failure
    // surface announced its mount. Both used to run before `setLogger`, so
    // `boot` threw at the one moment that surface was still null: the catch
    // below had nothing to draw on, the failure went to `console.error`, and
    // THE PAGE CAME UP EMPTY -- no stream opened, no failure shown, nothing
    // said. It could never have booted at all.
    //
    // Found by a headless run of the real webview, which is the first thing
    // in this repo to look at the RUNNING webapp: `boot` has no test of its
    // own, and `test/setup.ts` installs a logger for every suite, so the one
    // condition this failed under is the only one no suite creates.
    //
    // Everything above `setLogger` is silent by construction, and must stay
    // so: `pageAddress`, `workspaceRef`, the transport, the client,
    // `mintConnectionId` and `bindLogContext` do not log, which is what
    // makes them safe to run before the sink exists. Anything added here
    // that logs reintroduces exactly this defect.
    const address = pageAddress(window.location.search);
    const workspace = workspaceRef(address.workspaceId, address.workspaceDir);

    // THE PAGE ENDS AT ITS UNLOAD (page-teardown.ts): every request this
    // transport makes is aborted then, and nothing it asked for answers into
    // a page that is gone.
    beginPage();
    endPageAtPagehide(window);
    const connectionId = mintConnectionId();
    const transport = createPageTransport(window.location.origin, "page", connectionId);
    const client = createAgentReplClient(transport);

    bindLogContext({
      connection_id: connectionId,
      workspace_id: workspace.id,
      workspace_dir: workspace.dir,
    });
    // A level other than info is a window (proto/vocab/log-level-window.json):
    // a page reloaded from an address whose window has ended boots at info.
    const levelSelection = selectLevel(address.logLevel, address.logLevelUntil, Date.now(), "log_level");
    const levelWindow = new LevelWindow(levelSelection);
    installForwardingLogger(() => client, { case: "workspace", value: workspace }, levelWindow);
    const levelNote = selectionNote(levelSelection);
    if (levelNote !== null) log.info(levelNote, { operation: LEVEL_WINDOW_OPERATION, context: selectionContext(levelSelection) });

    // AND THE SHELL IS RESOLVED INSIDE THE TRY, not above it, because it
    // logs and therefore has to come after the sink. Its failure now goes
    // through `reportBootFailure` like every other one, which for a page
    // missing its own `#topbar` is still the console path -- there is no
    // warning chip to draw on -- but it is at least LOGGED now rather than
    // thrown past the reporter.
    const shell = shellElements(document);

    // Joined before anything draws, so the host's first word (shown or not)
    // reaches the page before the rail decides what it can skip.
    const webhostFailures = createWebhostFailureView();
    // The editor's own held-prompt ingress entries, which the held section
    // draws while this page cannot reach the daemon (held/section.ts).
    const editorIngress = createEditorIngressView();
    connectToHost({
      doc: document,
      win: window,
      presence: pagePresence(),
      webhostFailures,
      levelWindow,
      editorIngress,
      focusLeft: pageFocusLeft(),
      restyleTurns: pageRestyleTurns(document),
    });

    // The page's presence — hidden/shown, focused/blurred, resized, and the
    // repaint after each — is recorded from here on (src/page-presence.ts).
    installPagePresenceLog();
    // THE PAGE STATES ITS OWN SIZE (page-census.ts): the webhost holding it
    // cannot read a cross-origin page's document.
    stopAtPageTeardown(installPageCensus(document, pageAfter));

    // THE TOPBAR IS MOUNTED HERE, BEFORE ANY STREAM, because its warning chip
    // is the ONE place this page shows an error -- including the ones that
    // stop a stream from ever opening (a refused adoption, a dead link). The
    // stream itself waits for `watch`, after adoption. `failures` is set only
    // once the chip that draws it exists, so a failure the catch files is
    // always one the page can show.
    const localFailures = createLocalFailures();
    // The chip lists the webhost's failures too: it has no chip of its own.
    const topbar = mountTopbar(shell.topbar, { failures: mergeFailureViews(localFailures, webhostFailures) });
    failures = localFailures;

    // Copying is a page-wide affordance, not a component's: it is installed
    // once, here, so every surface mounted below is copyable from its first
    // paint. See src/copy.ts for why the page carries a fallback at all. So is
    // selecting: a click that ends a drag-select activates nothing, on any
    // surface (src/selection.ts).
    installTextAffordances(document);

    const ticker = createTicker();
    const ctx = createAppContext({
      client,
      workspace,
      ticker,
      failures: localFailures,
      seen: createPageSeen(workspace.id, pagePresence()),
      composerEnabled: address.composer,
      page: connectionId,
      costs: createStreamCostMeter({
        now: () => performance.now(),
        every: (fn, ms) => {
          const timer = window.setInterval(fn, ms);
          return () => window.clearInterval(timer);
        },
        nodes: () => regionNodes(document),
        presence: () => ({ shown: pagePresence().shown() }),
      }),
    });
    opened = ctx;
    // The daemon declares this workspace open (and the editor shows its tab)
    // only once this page has drawn, which only the page can say.
    installPageDrawnReport(ctx, document);
    // THE PAGE'S OWN LATENCIES (switch-latency.ts) go to the daemon's always-on
    // metrics: its boot from here to its conversation drawn, and every switch
    // to it Emacs stamped.
    const bootedAtMs = performance.now();
    const latencies = installLatencyReport({
      workspace: ctx.workspace,
      send: (batch) =>
        callUnary(ctx, "ReportLatencies", (client) => client.reportLatencies(reportLatenciesRequest(batch)), REPORT_LATENCIES_RESPONSE),
      after: (ms, fn) => {
        const timer = window.setTimeout(fn, ms);
        return () => window.clearTimeout(timer);
      },
      onAttached: (fn) => ctx.streams.onAttached(fn),
    });
    const latencyEnv = pageSwitchLatencyEnv(document);
    // EACH WATCHER LIVES AS LONG AS THE PAGE, so each stop is the page's
    // (page-teardown.ts): a page-stall sampler left running after a boot test
    // kept firing into later files (2026-10-24, a merge gate's run).
    stopAtPageTeardown(watchPageBoot(bootedAtMs, latencyEnv, (kind, seconds) => latencies.record(kind, seconds)));
    if (pagePresence().embedded) {
      stopAtPageTeardown(watchSwitchLatency(pagePresence(), latencyEnv, (kind, seconds) => latencies.record(kind, seconds)));
      stopAtPageTeardown(watchReloadBlank(pagePresence(), latencyEnv, (kind, seconds) => latencies.record(kind, seconds)));
    }
    // A STALL OF THIS PAGE'S MAIN THREAD while it is seen (page-stall.ts).
    stopAtPageTeardown(watchPageStall(
      pagePresence(),
      {
        now: () => performance.now(),
        every: (fn, ms) => {
          const timer = window.setInterval(fn, ms);
          return () => window.clearInterval(timer);
        },
      },
      (kind, seconds) => latencies.record(kind, seconds),
    ));
    // A PROMPT'S ECHO ends at its row drawn here (prompt-echo.ts).
    const promptEcho = createPromptEcho(latencyEnv.wallNow, (kind, seconds) => latencies.record(kind, seconds));
    installPromptEcho(promptEcho);
    log.info("the webapp booted", {
      operation: "main.boot",
      context: {
        connection_id: connectionId,
        composer_enabled: address.composer,
        origin: window.location.origin,
      },
    });

    // ADOPTION COMES BEFORE EVERY STREAM. A daemon that has just joined
    // refuses every per-workspace rpc with `not_yet_adopted` until its
    // rendezvous completes, so a page that opened its views first would spend
    // the whole rendezvous filing refusals for calls that were merely early.
    // A terminal refusal throws `AdoptionFailed`, which the catch below mints
    // as `boot_failed` — the page has no workspace to show and says so.
    // A DEFERRED TRANSFER ENDS THE BOOT QUIETLY: the incumbent kept this
    // workspace, and Emacs is reloading the page onto it. So does a MOVE: the
    // page booted on the daemon its workspace left, and Emacs is reloading it
    // onto the successor.
    const adopted = await adoptAtBoot(ctx);
    if (adopted === "deferred" || adopted === "moved") {
      ctx.quiesce();
      return;
    }

    // THE LIFECYCLE COMES NEXT, BEFORE ANY VIEW. `transferring_away` is the
    // `transferred` push arriving as an ANSWER, and it can come back at the
    // very first rpc a view makes — so the move hook this module registers has
    // to exist before the first view mount, or that first refusal would find
    // no handler and the page would keep drawing for a workspace it has lost.
    //
    // THE NEWS DIGEST OVERLAY IS MOUNTED WITH IT, because the lifecycle's
    // daemon stream is what carries the digest's standing.
    const newsDigest = mountNewsDigest(shell.newsDigest, ctx, { feedScroll: shell.feedScroll });
    const imageOverlay = mountImageOverlay(shell.imageOverlay, ctx, { feedScroll: shell.feedScroll });
    // A ROW THE DAEMON ASKS THIS PAGE TO REVEAL (a queue entry opening an async
    // merge's bubble here) can arrive before the feed below is mounted; the
    // relay holds it until the feed binds its reveal.
    const reveals = createRevealRelay();
    startLifecycle(ctx, { drainBannerHost: shell.drainBanner, newsDigest, reveals, promptEcho });

    // The dev-mode composer is the only shell element the boot itself reveals;
    // production runs composer-less, so the host ships hidden.
    if (address.composer) shell.composer.hidden = false;

    // THE MOUNT ORDER IS index.html's OWN ORDER, top to bottom, with two
    // forced exceptions: the login overlay is mounted BEFORE the topbar starts
    // watching, because the topbar's account control opens it; and the feed is
    // mounted before the footer, because the footer's jump rows reveal feed
    // rows.
    const gate = createComposerGate();
    // A dev-mode composer's `/status`-style answer is a PANEL, and it is drawn
    // beside the composer that asked for it rather than as a feed row: the
    // panel answers one submission, not the conversation. The feed's own
    // `command_panel` rows are the daemon's, and travel the ordinary row path.
    const showPanel = (panel: SubmitPromptCommandPanel): void => {
      for (const stale of childrenMatching(shell.composer, "[data-panel]")) stale.remove();
      shell.composer.append(drawCommandPanel(panel, ctx));
    };
    const login = mountLoginOverlay(shell.loginOverlay, ctx);

    topbar.watch(ctx, { openLogin: (control) => login.open(control) });
    // A FEED ROW'S "sign in" (an outcome marker's account cause) opens this
    // same overlay: one login flow, whichever control asked for it.
    answerLoginRequests((control) => login.open(control));

    // A CHAT THREAD'S PAGE MOUNTS THE CHAT where an agent workspace's mounts
    // its feed (the daemon names which, AdoptWebWorkspace). Everything else on
    // the page -- the topbar, the footer, the sidebar -- is the same page.
    // Only an agent workspace's feed is a footer's (`workspaceFeed`): a
    // thread's page mounts none, so no jump row can ever reach the chat.
    const workspaceFeed = adopted.page === "thread" ? null : mountFeed(shell.feed, ctx, {
      renderers: createRowRenderers(ctx),
      // The root feed's held section draws in the tail region after the rows.
      held: { host: shell.holdTray, editorIngress },
      // The image overlay this feed's watch draws.
      imageOverlay,
      // The docked gate is drawn outside the scroll box; a choice clicked in
      // it holds the feed's view as a click in the feed does.
      clickZones: [shell.gateDock],
      // The jump-to-tail control draws over the feed's bottom edge.
      jumpSlot: shell.feedJump,
      // PER-BUBBLE COMPOSERS ARE DEV-MODE ONLY (R7): production's root
      // composer is host-native, and a webview that submits nothing has no
      // business drawing a text box inside a bubble either.
      composerFactory: address.composer
        ? (host, bubble) =>
            mountComposer(host, ctx, { feed: bubble, gate, onPanel: showPanel })
        : undefined,
    });
    const feed: PageBody = workspaceFeed ?? mountThreadBody(shell, ctx);
    reveals.bind((row) => feed.revealRow(row));

    // A SWITCH TO THIS WORKSPACE RETURNS THE FEED TO ITS TAIL (owner ruling,
    // 2026-10-02), whichever path made it. IN THE WEBHOST the sidebar is the
    // webhost's, drawn once for every page, and the webhost showing this page
    // is the switch; a page on its own draws its own sidebar, which watches the
    // roster's `current`, the one fact every switch path ends in.
    if (pagePresence().embedded) {
      watchShownEdge(pagePresence(), () => feed.workspaceSelected());
    } else {
      mountSidebar(shell.sidebar, ctx, {
        workspaceSelected: () => feed.workspaceSelected(),
        // A page's own rail ends above its docked gate band (styles.css body grid).
        gateBand: () => shell.gateDock.getBoundingClientRect().height,
      });
    }

    // A STANDING GATE DOCKS AT THE BOTTOM EDGE while Emacs hides the input
    // window over it; both read the daemon's one cold-gate standing.
    installGateDock(shell.feed, shell.gateDock);
    // AN OPEN QUESTION OR PERMISSION IS DRAWN BELOW THE HELD PROMPTS, the
    // scroll zone's last region.
    installAskTray(shell.feed, shell.askTray);

    // PROSE LINKS ROUTE LIKE STRUCTURED ONES. A markdown anchor in a bubble or
    // the held section would otherwise navigate the webview away from the
    // conversation; one delegated interceptor over the whole scroll zone (feed
    // + held section) cancels the click and hands it to OpenExternal / OpenInEditor,
    // exactly as the structured links do. Hung here, at the feed/markdown seam,
    // once — every markdown-bearing surface lives inside `feedScroll`.
    installProseLinkRouting(ctx, shell.feedScroll);
    // AND NO OUTWARD LINK ANYWHERE ON THE PAGE NAVIGATES THE WEBVIEW: what no
    // surface routed opens externally too (link.ts, installNavigationGuard).
    installNavigationGuard(ctx, document);

    // A PAGE THE WEBHOST IS NOT SHOWING RUNS NO ANIMATIONS, and resumes each
    // where it would have been when shown (host/unshown-animations.ts).
    if (pagePresence().embedded) pauseAnimationsWhileUnshown(liveUnshownAnimationsEnv(document, pagePresence()));

    // A CHAT THREAD'S PAGE HAS NO FOOTER (owner request, 2026-10-25): its
    // composer is Emacs's and no webview composer is gated on a status, so
    // the strip has nothing to say there. Its box stays hidden.
    if (workspaceFeed === null) {
      shell.footer.hidden = true;
      log.info("a chat thread's page mounts no footer", { operation: "main.thread-no-footer" });
    } else {
      mountWorkspaceFooter(shell, ctx, workspaceFeed, gate);
    }

    if (address.composer) {
      mountComposer(shell.composer, ctx, { gate, onPanel: showPanel });
    }

  } catch (err) {
    // THE PAGE GOES QUIET BEFORE IT REPORTS. There is no workspace to show and
    // nothing on this page can get one back, so the one stream is cancelled and
    // not reopened — a dead page that keeps dialing costs the daemon a
    // connection and buries its own `boot_failed` warning under a reopen
    // loop's error records.
    opened?.quiesce();
    reportBootFailure(err, failures);
    throw err;
  }
}

/**
 * An agent workspace's footer: its status strip, the response usage a hovered
 * bubble shows in it, and the webview composer's gate, which is the footer's
 * own color.
 */
function mountWorkspaceFooter(
  shell: ReturnType<typeof shellElements>,
  ctx: AppContext,
  feed: { selectDetachedWork(id: FeedId): Promise<boolean> },
  gate: ReturnType<typeof createComposerGate>,
): void {
  const footer = mountFooter(shell.footer, ctx, {
    selectDetachedWork: (id) => feed.selectDetachedWork(id),
    presence: pagePresence(),
  });
  // A HOVERED RESPONSE'S USAGE IS READ IN THE FOOTER (owner request,
  // 2026-10-25): the bubble draws no figure of its own, and the footer's
  // activity cell shows it while the pointer is on the bubble.
  installResponseUsageHover(shell.feed, (usage) => footer.showResponseUsage(usage), pageFocusLeft());
  // THE GATE IS THE FOOTER'S OWN COLOR (owner ruling, 2026-09-28). A
  // composer is closed exactly when the footer's status arm is blue (the
  // workspace is unusable: an agent-repl fault, a network fault, a close) —
  // render-colors.json#composer_closed_colors; a merge in flight holds
  // what is submitted, so purple leaves it open —
  // and the sentence it shows is the footer's status arm rather than a
  // second vocabulary. Every other arm is a usable workspace, turquoise
  // included — a vendor fault's prompts are held until the vendor serves
  // (owner ruling, 2026-10-02) — so its composer is open.
  footer.onStatus((statusCase, substatusCase) => {
    const closed = composerClosedFor(statusCase, substatusCase);
    gate.set(closed ? "closed" : "open", closed ? statusCase : undefined);
  });
}

/** The body a thread page mounts, as the boot wires a feed's. */
interface PageBody {
  /** This workspace was switched to: show the conversation's end. */
  workspaceSelected(): void;
  /** The daemon asked for a feed row to be revealed (WebWorkspaceRevealRow). */
  revealRow(id: FeedId): Promise<boolean>;
}

/**
 * A chat thread's body: the chat, drawn in the feed's own box. A thread has no
 * feed rows, and the daemon reveals no row of it: that ask reaching it is a
 * defect, thrown as one (`noFeedRow`). It has no footer, so no jump row asks.
 */
function mountThreadBody(shell: ReturnType<typeof shellElements>, ctx: AppContext): PageBody {
  mountChat(shell.feed, ctx, { scroll: shell.feedScroll, shown: () => pagePresence().shown() });
  return {
    workspaceSelected: () => {
      landChatAtEnd(shell.feedScroll, "workspaceSelected");
    },
    revealRow: (id) => noFeedRow(id, "main.thread-reveal-row", "the daemon asked a chat thread to reveal a feed row"),
  };
}

/** Refuse an ask for feed row ID on a chat thread's page, which has none. */
function noFeedRow(id: FeedId, operation: string, said: string): Promise<boolean> {
  log.error(said, { operation, context: { id: id.value } });
  return Promise.reject(new Error(`a chat thread has no feed row ${id.value}: ${said}`));
}

/**
 * File the boot failure wherever there is still something to file it in.
 *
 * The warning chip's failures when the topbar that draws them is mounted, and
 * the emergency console path when it is not — which is the case for a failure
 * in the page address or the transport, before there is any surface at all.
 * Logging through `log.error()` is not available either: the logger's own
 * sink is built inside the block that just threw.
 */
function reportBootFailure(err: unknown, failures: LocalFailures | null): void {
  const cause = err instanceof Error ? `${err.name}: ${err.message}` : String(err);
  if (failures !== null) {
    failures.report(bootFailed(cause));
    log.error(`the webapp failed to boot: ${cause}`, {
      operation: "main.boot-failed",
      context: { cause },
    });
    return;
  }
  // PRE-LOGGER BOOTSTRAP FAILURE: the documented exception to "no direct
  // console". There is no chip to draw on and no logger to route through.
  console.error(`the webapp failed to boot before it could report anything: ${cause}`);
}

// THE BOOT IS ASYNCHRONOUS because adoption is an rpc, so its failure cannot
// be a bare throw any more. `boot` has already drawn and logged the failure by
// the time it rejects; rethrowing from a microtask re-raises it as the uncaught
// error the browser reports, which is exactly the loudness the synchronous
// throw used to have — and is not the same thing as swallowing it.
//
// THE HOST PAGE IS THIS SAME BUNDLE AT `?host=1` (src/host/host.ts), loaded
// through a dynamic import so it reaches nothing of the workspace page it does
// not need, while staying reachable from this one entry: the entry's hash is
// the build's identity (bin/build-frontend.sh), and it covers the host too.
const started = isHostAddress(window.location.search)
  ? import("./host/main.js").then((host) => {
      host.startHost(document, window);
    })
  : boot();
started.catch((err: unknown) => {
  queueMicrotask(() => {
    throw err;
  });
});
