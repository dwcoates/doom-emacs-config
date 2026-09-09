/**
 * footer — the mount: one standing `WatchFooter` stream, one whole-view redraw
 * per push, and the ONE piece of state this component keeps.
 *
 * THE VIEW IS DRAWN WHOLE, EVERY PUSH. `FooterView` replaces its unit whole and
 * the client accumulates nothing across pushes, so a push rebuilds both the
 * strip and the open panel rather than diffing them. That is also why nothing
 * here remembers a figure between pushes: whatever is not in the current view
 * is not on the screen.
 *
 * THE ONE PIECE OF LOCAL STATE IS WHICH PANEL IS OPEN. Every panel arrives
 * fully resolved on every push and the daemon never learns the selection, so
 * opening one is a redraw and not a round trip. It survives pushes (a redraw
 * must not close a panel the reader opened) and it survives a reload, in
 * `localStorage` per workspace, behind try/catch — a webview-local preference,
 * which R14 allows and which is the only thing this component persists.
 *
 * `onStatus` EXISTS FOR THE COMPOSERS. R7 disables a bubble composer while the
 * footer reads merging, closing or disconnected, and the footer's stream is the
 * one place that fact arrives — so the status ARM is published to subscribers
 * rather than read back out of the DOM. A subscriber joining late is told the
 * current arm immediately, because a composer mounted mid-merge must not wait
 * for the next push to learn it should be closed.
 */
import { create } from "@bufbuild/protobuf";
import {
  WatchFooterRequestSchema,
  WatchFooterResponseSchema,
  type WatchFooterRequest,
  type WatchFooterResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FooterView } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import type { Handle } from "../failure/overlay.js";
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, requireMessage } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { drawFooterExpanded, FOOTER_PANELS, type FooterPanel } from "./expanded.js";
import { drawFooterStrip } from "./strip.js";

/** Where the open panel is remembered, per workspace. */
export function panelStorageKey(workspaceId: string): string {
  return `agent-repl.footer.panel.${workspaceId}`;
}

export interface FooterDeps {
  /** Bring a feed row into view; the panels' jump rows call it. */
  readonly revealRow: (id: FeedId) => Promise<boolean>;
}

export interface FooterHandle extends Handle {
  /**
   * Observe the status ARM on every push, and immediately on subscribe when a
   * push has already landed. Returns its unsubscriber.
   */
  onStatus(fn: (statusCase: string) => void): () => void;
}

/** The stream's request: the workspace, echoed from the page's one ref. */
export function buildWatchFooterRequest(ctx: AppContext): WatchFooterRequest {
  return create(WatchFooterRequestSchema, { workspace: ctx.workspace });
}

/** Mount the footer into HOST. */
export function mountFooter(host: HTMLElement, ctx: AppContext, deps: FooterDeps): FooterHandle {
  log("info", "mounting the footer", { operation: "footer.mount", context: {} });
  host.setAttribute("data-component", "footer");

  const statusListeners = new Set<(statusCase: string) => void>();
  let selection: FooterPanel | null = readSelection(ctx);
  let view: FooterView | null = null;
  let statusCase: string | null = null;
  let disposed = false;

  const watch: StreamHandle = watchStream<WatchFooterResponse>(ctx, {
    name: "WatchFooter",
    schema: WatchFooterResponseSchema,
    open: (_client, signal) => ctx.streams.watch("footer", buildWatchFooterRequest(ctx), signal),
    onPush: (response) => {
      view = requireMessage(response.footer, "WatchFooterResponse.footer");
      draw();
      publishStatus();
    },
  });

  return {
    onStatus(fn: (statusCase: string) => void): () => void {
      statusListeners.add(fn);
      // A composer mounted after the first push must not wait for the next one.
      if (statusCase !== null) fn(statusCase);
      return () => {
        statusListeners.delete(fn);
      };
    },
    dispose(): void {
      if (disposed) return;
      disposed = true;
      log("info", "disposing the footer", { operation: "footer.dispose", context: {} });
      watch.cancel();
      // Every clock this component started hangs off the host's subtree.
      stopTicking(host);
      host.replaceChildren();
    },
  };

  /**
   * Redraw the whole footer: the open panel above, the strip below.
   *
   * The old subtree's clock subscriptions are dropped BEFORE it is discarded,
   * or every push would leave one ticking against a detached element.
   */
  function draw(): void {
    if (disposed || view === null) return;
    const strip = requireMessage(view.strip, "FooterView.strip");
    const expanded = requireMessage(view.expanded, "FooterView.expanded");

    const dock = document.createElement("div");
    dock.className = "pfooter";
    dock.setAttribute("role", "status");
    dock.setAttribute("aria-live", "polite");

    const panel = drawFooterExpanded(expanded, selection, { ctx, revealRow: deps.revealRow });
    if (panel !== null) dock.appendChild(panel);
    dock.appendChild(drawFooterStrip(strip, { ctx, selection, onSelect: select }));

    stopTicking(host);
    host.replaceChildren(dock);
  }

  /**
   * Open a panel, or close it when it is already open.
   *
   * A second click on the open chip closes the section: the chip is the only
   * affordance the strip has, so it has to be both the opener and the closer.
   */
  function select(panel: FooterPanel): void {
    selection = selection === panel ? null : panel;
    log("debug", `the footer panel selection is now ${selection ?? "none"}`, {
      operation: "footer.select-panel",
      context: { panel: selection },
    });
    writeSelection(ctx, selection);
    draw();
  }

  /** Tell every subscriber which status arm this push carried. */
  function publishStatus(): void {
    if (view === null) return;
    const strip = requireMessage(view.strip, "FooterView.strip");
    const status = requireMessage(strip.status, "FooterStrip.status");
    statusCase = requireCase(status.status, "FooterStatus.status").case;
    for (const fn of [...statusListeners]) fn(statusCase);
  }
}

/**
 * The remembered panel, or null.
 *
 * BEHIND try/catch: a webview with site data disabled throws on the accessor
 * itself, and a lost preference is not worth a footer that fails to mount. A
 * stored value that is not a panel name (an older bundle's spelling, a hand-
 * edited entry) is discarded rather than trusted into the panel switch.
 */
export function readSelection(ctx: AppContext): FooterPanel | null {
  try {
    const stored = window.localStorage.getItem(panelStorageKey(ctx.workspace.id));
    if (stored === null) return null;
    if (!FOOTER_PANELS.includes(stored as FooterPanel)) {
      log("warn", `discarding an unrecognized stored footer panel: ${stored}`, {
        operation: "footer.selection-unrecognized",
        context: { stored },
      });
      return null;
    }
    return stored as FooterPanel;
  } catch (err) {
    log("warn", `could not read the footer's panel preference: ${String(err)}`, {
      operation: "footer.selection-read-failed",
      context: { cause: err },
    });
    return null;
  }
}

/** Remember the panel, or forget it when nothing is open. */
export function writeSelection(ctx: AppContext, selection: FooterPanel | null): void {
  try {
    const key = panelStorageKey(ctx.workspace.id);
    if (selection === null) {
      window.localStorage.removeItem(key);
      return;
    }
    window.localStorage.setItem(key, selection);
  } catch (err) {
    log("warn", `could not store the footer's panel preference: ${String(err)}`, {
      operation: "footer.selection-write-failed",
      context: { cause: err },
    });
  }
}
