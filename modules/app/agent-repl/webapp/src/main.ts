/**
 * THE BOOT.
 *
 * It does the smallest thing that makes every later mount possible, in the one
 * order the dependencies allow, and it mounts exactly one component: the
 * failure overlay, which is the only surface that can report the boot itself
 * going wrong.
 *
 * THE ORDER IS FORCED, not chosen:
 *   1. the shell, so a broken `index.html` fails by name here rather than
 *      inside a component's first draw;
 *   2. the page address, because everything below is addressed to a workspace
 *      and a page without one has nothing to show;
 *   3. the transport and client, because the LOGGER's sink is an rpc;
 *   4. the failure overlay, so there is somewhere to put a boot failure before
 *      anything that can fail is started;
 *   5. the logger, bound to this page's identity;
 *   6. everything else.
 *
 * A THROW ANYWHERE IN HERE IS `boot_failed` — the one failure that cannot be
 * carried the way the others are, since the machinery that would carry it is
 * the machinery that failed to build. It is drawn from whatever exists at the
 * time: the overlay if step 4 got that far, and the emergency console path if
 * it did not, which is the documented exception to "no direct console".
 */
import "./styles.css";
import { createTicker } from "./clock.js";
import { createComposerGate, mountComposer } from "./composer/composer.js";
import { createRowRenderers } from "./feed/renderers.js";
import { mountFeed } from "./feed/feed.js";
import { mountFooter } from "./footer/footer.js";
import { adoptAtBoot, startLifecycle } from "./lifecycle/lifecycle.js";
import { mountLoginOverlay } from "./login/login.js";
import { drawCommandPanel } from "./panels/panels.js";
import { mountSidebar } from "./sidebar/sidebar.js";
import { mountTopbar } from "./topbar/topbar.js";
import { mountHoldTray } from "./tray/tray.js";
import { bootFailed } from "./failure/sink.js";
import { mountFailureOverlay, type FailureOverlayHandle } from "./failure/overlay.js";
import { ForwardingLogger, bindLogContext, log, setLogger, type ClientLogSink } from "./log.js";
import { createAgentReplClient, type AgentReplClient } from "./rpc/client.js";
import { createAppContext } from "./rpc/context.js";
import { pageAddress } from "./rpc/page-address.js";
import { createDaemonTransport } from "./rpc/transport.js";
import { workspaceRef } from "./rpc/workspace-ref.js";
import type { SubmitPromptCommandPanel } from "../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import type { WorkspaceRef } from "../../proto/gen/ts/workspace/v1/workspace_pb";
import { shellElements } from "./shell.js";

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
function clientLogSink(getClient: () => AgentReplClient, workspace: WorkspaceRef): ClientLogSink {
  return async (record) => {
    await getClient().clientLog({ workspace, record });
  };
}

export async function boot(): Promise<void> {
  const shell = shellElements(document);
  let overlay: FailureOverlayHandle | null = null;
  try {
    const address = pageAddress(window.location.search);
    const workspace = workspaceRef(address.workspaceId, address.workspaceDir);

    const transport = createDaemonTransport(window.location.origin);
    const client = createAgentReplClient(transport);

    overlay = mountFailureOverlay(shell.failureOverlay);

    const connectionId = mintConnectionId();
    bindLogContext({
      connection_id: connectionId,
      workspace_id: workspace.id,
      workspace_dir: workspace.dir,
    });
    setLogger(new ForwardingLogger(clientLogSink(() => client, workspace)));

    const ticker = createTicker();
    const ctx = createAppContext({
      client,
      workspace,
      ticker,
      failures: overlay,
      composerEnabled: address.composer,
      page: connectionId,
    });
    log("info", "the webapp booted", {
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
    // forced exceptions: the login overlay is mounted BEFORE the topbar,
    // because the topbar's account control opens it; and the feed is mounted
    // before the footer, because the footer's jump rows reveal feed rows.
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
    mountTopbar(shell.topbar, ctx, { openLogin: (control) => login.open(control) });

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

    mountHoldTray(shell.holdTray, ctx);

    const footer = mountFooter(shell.footer, ctx, { revealRow: (id) => feed.revealRow(id) });
    // THE GATE IS THE FOOTER'S OWN WORD (R7). A composer closes while the
    // workspace is merging, closing, or disconnected, and the sentence it
    // shows is the footer's status arm rather than a second vocabulary this
    // end invented for the same three states.
    footer.onStatus((statusCase) => {
      const closed =
        statusCase === "merging" || statusCase === "closing" || statusCase === "disconnected";
      gate.set(closed ? "closed" : "open", closed ? statusCase : undefined);
    });

    if (address.composer) {
      mountComposer(shell.composer, ctx, { gate, onPanel: showPanel });
    }

  } catch (err) {
    reportBootFailure(err, overlay);
    throw err;
  }
}

/**
 * File the boot failure wherever there is still something to file it in.
 *
 * The overlay when it exists, and the emergency console path when it does not
 * — which is the case for a failure in the page address or the transport,
 * before there is any surface at all. Logging through `log()` is not available
 * either: the logger's own sink is built inside the block that just threw.
 */
function reportBootFailure(err: unknown, overlay: FailureOverlayHandle | null): void {
  const cause = err instanceof Error ? `${err.name}: ${err.message}` : String(err);
  if (overlay !== null) {
    overlay.report(bootFailed(cause));
    log("error", `the webapp failed to boot: ${cause}`, {
      operation: "main.boot-failed",
      context: { cause },
    });
    return;
  }
  // PRE-LOGGER BOOTSTRAP FAILURE: the documented exception to "no direct
  // console". There is no overlay to draw on and no logger to route through.
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
