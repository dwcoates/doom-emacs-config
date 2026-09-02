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
import { bootFailed } from "./failure/sink.js";
import { mountFailureOverlay, type FailureOverlayHandle } from "./failure/overlay.js";
import { ForwardingLogger, bindLogContext, log, setLogger, type ClientLogSink } from "./log.js";
import { createAgentReplClient, type AgentReplClient } from "./rpc/client.js";
import { createAppContext } from "./rpc/context.js";
import { pageAddress } from "./rpc/page-address.js";
import { createDaemonTransport } from "./rpc/transport.js";
import { workspaceRef } from "./rpc/workspace-ref.js";
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

export function boot(): void {
  const shell = shellElements(document);
  let overlay: FailureOverlayHandle | null = null;
  try {
    const address = pageAddress(window.location.search);
    const workspace = workspaceRef(address.workspaceId, address.workspaceDir);

    const transport = createDaemonTransport(window.location.origin);
    let client = createAgentReplClient(transport);

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
    });
    log("info", "the webapp booted", {
      operation: "main.boot",
      context: {
        connection_id: connectionId,
        composer_enabled: address.composer,
        origin: window.location.origin,
      },
    });

    // The dev-mode composer is the only shell element the boot itself reveals;
    // production runs composer-less, so the host ships hidden.
    if (address.composer) shell.composer.hidden = false;

    // WIRING: mountTopbar(shell.topbar, ctx, { openLogin })
    // WIRING: mountSidebar(shell.sidebar, ctx)
    // WIRING: mountFeed(shell.feed, ctx, { renderers, composerFactory })
    // WIRING: mountHoldTray(shell.holdTray, ctx)
    // WIRING: mountFooter(shell.footer, ctx, { revealRow })
    // WIRING: mountComposer(shell.composer, ctx, { gate, onPanel })
    // WIRING: mountLoginOverlay(shell.loginOverlay, ctx)
    // WIRING: startLifecycle(ctx, { drainBannerHost: shell.drainBanner })
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

boot();
