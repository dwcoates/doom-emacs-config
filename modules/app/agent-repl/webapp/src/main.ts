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
import "./styles.css";
import { createTicker } from "./clock.js";
import { installCopyFallback } from "./copy.js";
import { createComposerGate, mountComposer } from "./composer/composer.js";
import { createRowRenderers } from "./feed/renderers.js";
import { mountFeed } from "./feed/feed.js";
import { mountFooter } from "./footer/footer.js";
import { adoptAtBoot, startLifecycle } from "./lifecycle/lifecycle.js";
import { mountLoginOverlay } from "./login/login.js";
import { drawCommandPanel } from "./panels/panels.js";
import { createPromptWaveDriver } from "./prompt-wave-driver.js";
import { mountSidebar } from "./sidebar/sidebar.js";
import { mountTopbar } from "./topbar/topbar.js";
import { mountHoldTray } from "./tray/tray.js";
import { installProseLinkRouting } from "./link.js";
import { bootFailed } from "./failure/sink.js";
import { createLocalFailures, type LocalFailures } from "./failure/local.js";
import { ForwardingLogger, bindLogContext, log, setLogger, type ClientLogSink } from "./log.js";
import { createAgentReplClient, type AgentReplClient } from "./rpc/client.js";
import { createAppContext, type AppContext } from "./rpc/context.js";
import { reportClientFailure } from "./rpc/link.js";
import { pageAddress } from "./rpc/page-address.js";
import { createDaemonTransport } from "./rpc/transport.js";
import { workspaceRef } from "./rpc/workspace-ref.js";
import type { SubmitPromptCommandPanel } from "../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import type { WorkspaceRef } from "../../proto/gen/ts/workspace/v1/workspace_pb";
import { shellElements } from "./shell.js";
import { composerClosedFor } from "./vocab.js";

/**
 * This page's own identity, for correlating its records against the daemon's.
 *
 * A page, not a session: it is minted per load and says nothing about which
 * conversation the workspace owns. `crypto.randomUUID` is the browser's, with
 * a time-plus-entropy fallback for a context that does not expose it (an old
 * webview, a non-secure origin) — an unidentifiable page's logs are worth less
 * than a page that refuses to boot over the identifier.
 */
function mintConnectionId(): string {
  const uuid = globalThis.crypto?.randomUUID?.();
  if (uuid !== undefined) return uuid;
  return `web-${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 10)}`;
}

/**
 * The logger's sink: one `ClientLog` call per record.
 *
 * DELIBERATELY NOT THROUGH `callUnary`. That helper logs the call it is
 * making, so routing the log sink through it would log every log — and its
 * strict check would refuse a `ClientLogResponse` by throwing into the
 * logger's own failure path. This is the one rpc the app makes without it; the
 * rejection is what `ForwardingLogger` counts.
 */
export function clientLogSink(
  getClient: () => AgentReplClient,
  workspace: WorkspaceRef,
): ClientLogSink {
  return async (record) => {
    let response;
    try {
      response = await getClient().clientLog({ workspace, record });
    } catch (err) {
      // THE SINK ITSELF IS DOWN. `ForwardingLogger` counts the rejection and
      // says so on the console exactly once, and nothing else knew (the
      // audit's N2 row 10) -- so the footer is told, with a FIXED line. The
      // report logs, that log is forwarded through this same sink, and it
      // fails again: an identical repeat is dropped by `reportClientFailure`,
      // which is what makes that loop terminate. The rejection is rethrown
      // unchanged, because the logger's own count is still its to keep.
      reportClientFailure("client_log_failed", "ClientLog forwarding failed");
      throw err;
    }
    // `unknown_workspace` is TERMINAL, not a failure: this page's workspace has
    // been closed or forgotten and the daemon has nowhere to file the record.
    // The logger reads it as the cue to stop forwarding; every other arm is a
    // refusal of one record, which leaves forwarding alone.
    return response.result.case === "error" &&
      response.result.value.cause.case === "unknownWorkspace"
      ? "workspace_departed"
      : "accepted";
  };
}

export async function boot(): Promise<void> {
  let failures: LocalFailures | null = null;
  // HOISTED SO A FAILED BOOT CAN STOP DIALING. The page's one stream is opened
  // by `createAppContext`, which is several steps ABOVE `adoptAtBoot` — so a
  // boot that fails at adoption, or at any mount after it, leaves that stream
  // reopening on backoff against a daemon the page has already given up on,
  // forever, behind a `boot_failed` warning. See the catch.
  let opened: AppContext | null = null;
  try {
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

    const transport = createDaemonTransport(window.location.origin);
    const client = createAgentReplClient(transport);

    const connectionId = mintConnectionId();
    bindLogContext({
      connection_id: connectionId,
      workspace_id: workspace.id,
      workspace_dir: workspace.dir,
    });
    setLogger(
      new ForwardingLogger(
        clientLogSink(() => client, workspace),
        undefined,
        {},
        address.logLevel,
      ),
    );

    // AND THE SHELL IS RESOLVED INSIDE THE TRY, not above it, because it
    // logs and therefore has to come after the sink. Its failure now goes
    // through `reportBootFailure` like every other one, which for a page
    // missing its own `#topbar` is still the console path -- there is no
    // warning chip to draw on -- but it is at least LOGGED now rather than
    // thrown past the reporter.
    const shell = shellElements(document);

    // THE TOPBAR IS MOUNTED HERE, BEFORE ANY STREAM, because its warning chip
    // is the ONE place this page shows an error -- including the ones that
    // stop a stream from ever opening (a refused adoption, a dead link). The
    // stream itself waits for `watch`, after adoption. `failures` is set only
    // once the chip that draws it exists, so a failure the catch files is
    // always one the page can show.
    const localFailures = createLocalFailures();
    const topbar = mountTopbar(shell.topbar, { failures: localFailures });
    failures = localFailures;

    // Copying is a page-wide affordance, not a component's: it is installed
    // once, here, so every surface mounted below is copyable from its first
    // paint. See src/copy.ts for why the page carries a fallback at all.
    installCopyFallback(document);

    const ticker = createTicker();
    const ctx = createAppContext({
      client,
      workspace,
      ticker,
      failures: localFailures,
      composerEnabled: address.composer,
      page: connectionId,
    });
    opened = ctx;
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
    await adoptAtBoot(ctx);

    // THE LIFECYCLE COMES NEXT, BEFORE ANY VIEW. `transferring_away` is the
    // `transferred` push arriving as an ANSWER, and it can come back at the
    // very first rpc a view makes — so the move hook this module registers has
    // to exist before the first view mount, or that first refusal would find
    // no handler and the page would keep drawing for a workspace it has lost.
    startLifecycle(ctx, { drainBannerHost: shell.drainBanner });

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
      for (const stale of shell.composer.querySelectorAll(":scope > [data-panel]")) stale.remove();
      shell.composer.append(drawCommandPanel(panel, ctx));
    };
    const login = mountLoginOverlay(shell.loginOverlay, ctx);

    mountSidebar(shell.sidebar, ctx);
    topbar.watch(ctx, { openLogin: (control) => login.open(control) });

    const feed = mountFeed(shell.feed, ctx, {
      renderers: createRowRenderers(ctx),
      // PER-BUBBLE COMPOSERS ARE DEV-MODE ONLY (R7): production's root
      // composer is host-native, and a webview that submits nothing has no
      // business drawing a text box inside a bubble either.
      composerFactory: address.composer
        ? (host, bubble) =>
            mountComposer(host, ctx, { feed: bubble, gate, onPanel: showPanel })
        : undefined,
    });

    mountHoldTray(shell.holdTray, ctx, { promptHeld: (turn) => feed.promptHeld(turn) });

    // PROSE LINKS ROUTE LIKE STRUCTURED ONES. A markdown anchor in a bubble or
    // the hold tray would otherwise navigate the webview away from the
    // conversation; one delegated interceptor over the whole scroll zone (feed
    // + tray) cancels the click and hands it to OpenExternal / OpenInEditor,
    // exactly as the structured links do. Hung here, at the feed/markdown seam,
    // once — every markdown-bearing surface lives inside `feedScroll`.
    installProseLinkRouting(ctx, shell.feedScroll);

    // KEEP THE PROMPT WAVE RUNNING WHILE THE WEBVIEW IS HIDDEN. WebKit suspends
    // the wave's compositor animation whenever the xwidget reports itself
    // hidden (Emacs not frontmost), so a JS timer hand-paints it then. Page-
    // global, started once the feed that holds the bubbles is mounted; see
    // prompt-wave-driver.ts for the whole account.
    createPromptWaveDriver().start();

    const footer = mountFooter(shell.footer, ctx, {
      selectDetachedWork: (id) => feed.selectDetachedWork(id),
      paints: feed.paints,
      followingTail: feed.followingTail,
    });
    // THE GATE IS THE FOOTER'S OWN COLOR (owner ruling, 2026-09-28). A
    // composer is closed exactly when the footer's status arm is blue (the
    // workspace is unusable: disconnected, closing, blocked) or purple (a
    // merge in flight holds it) — render-colors.json#composer_closed_colors —
    // and the sentence it shows is the footer's status arm rather than a
    // second vocabulary. Every other arm is a usable workspace, turquoise
    // included, so its composer is open.
    footer.onStatus((statusCase) => {
      const closed = composerClosedFor(statusCase);
      gate.set(closed ? "closed" : "open", closed ? statusCase : undefined);
    });

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
void boot().catch((err: unknown) => {
  queueMicrotask(() => {
    throw err;
  });
});
