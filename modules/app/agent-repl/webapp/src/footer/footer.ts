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
 * THE DAEMON MAY MOVE IT, ONCE PER GENERATION. `FooterView.focus` names the
 * panel the daemon wants in front of the reader when detached work starts; the
 * daemon has already picked it, so the name is applied verbatim. It is an EDGE:
 * a push whose generation this page has already applied changes nothing, so
 * between launches the reader's own clicks stand.
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
import type { Handle } from "../failure/local.js";
import { frameUndecodable } from "../failure/sink.js";
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { onClientVerdict, standingClientFailure } from "../rpc/link.js";
import { isMalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { drawFooterExpanded, FOOTER_PANELS, type FooterPanel } from "./expanded.js";
import { publishCompactionProgress } from "./progress.js";
import { drawClientDisconnectedStrip, drawFooterStrip, footerStatusActivity } from "./strip.js";
import { createStopControls } from "./stop.js";

/** Where the open panel is remembered, per workspace. */
export function panelStorageKey(workspaceId: string): string {
  return `agent-repl.footer.panel.${workspaceId}`;
}

export interface FooterDeps {
  /** The reader picked a detached-work item: scroll to its card (feed.ts). */
  readonly selectDetachedWork: (id: FeedId) => Promise<boolean>;
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
  log.info("mounting the footer", { operation: "footer.mount", context: {} });
  host.setAttribute("data-component", "footer");

  const statusListeners = new Set<(statusCase: string) => void>();
  let selection: FooterPanel | null = readSelection(ctx);
  // THE SECOND PIECE OF LOCAL STATE, and for the same reason as the first: it
  // is the reader's, not the daemon's. A stop's answer -- the note, the
  // refusal, the confirm challenge -- is drawn at the control that made the
  // call, and this view is rebuilt on EVERY push. Building the controls here
  // and appending the same elements each draw is what lets an answer survive
  // the push the stop itself caused. See `StopControls`.
  const stops = createStopControls(ctx);
  let view: FooterView | null = null;
  // The last focus generation this page applied; see `applyFocus`.
  let appliedFocus: bigint | null = null;
  let statusCase: string | null = null;
  let disposed = false;

  // THE CLIENT'S OWN VERDICT OVERLAYS THE DAEMON'S (owner ruling, 2026-09-13).
  // Subscribing here rather than polling in `draw` is what makes a verdict
  // REACH the screen: nothing else redraws this component between pushes, and
  // a link that is down produces no pushes by definition.
  // ONLY THE DRAWING. The status ARM published to R7's composer gate stays the
  // daemon's -- see `publishStatus` for why a client verdict must not close a
  // composer.
  const unsubscribeFromVerdict = onClientVerdict(() => {
    if (disposed) return;
    // A REDRAW OF AN UNREADABLE LAST VIEW IS ONE UNREADABLE FRAME, and it gets
    // the treatment `watchStream` gives one: logged, filed, and skipped. It
    // cannot be allowed to throw, because the caller here is whatever reported
    // or cleared the verdict -- an rpc, whose own answer would then be
    // mislabelled a transport failure by its call site.
    try {
      draw();
    } catch (err) {
      if (!isMalformedView(err)) throw err;
      log.error(`the footer's last view could not be redrawn: ${err.detail}`, {
        operation: "footer.verdict-redraw-undecodable",
        context: { path: err.path, cause: err.detail },
      });
      ctx.failures.report(frameUndecodable(err.detail, `FooterView at ${err.path}`));
    }
  });

  const watch: StreamHandle = watchStream<WatchFooterResponse>(ctx, {
    name: "WatchFooter",
    schema: WatchFooterResponseSchema,
    open: (_client, signal) => ctx.streams.watch("footer", buildWatchFooterRequest(ctx), signal),
    onPush: (response) => {
      view = requireMessage(response.footer, "WatchFooterResponse.footer");
      applyFocus(view);
      draw();
      publishStatus();
      publishProgress();
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
      log.info("disposing the footer", { operation: "footer.dispose", context: {} });
      unsubscribeFromVerdict();
      watch.cancel();
      // The page's compaction line belongs to the stream that just stopped.
      publishCompactionProgress(null);
      // Every clock this component started hangs off the host's subtree.
      stopTicking(host);
      host.replaceChildren();
    },
  };

  /**
   * Redraw the whole footer: the strip on top, then the divider, then the open
   * panel under it (owner ruling, 2026-09-13). The strip is the dock's face and
   * the expanded section hangs off its underside, so the section never rises
   * above the strip or overlays it.
   *
   * The DIVIDER is drawn only when a panel is open, because it partitions two
   * sections and a closed footer has one. It is the dock's own child rather
   * than a border on either neighbour, so it spans the dock edge to edge.
   *
   * The old subtree's clock subscriptions are dropped BEFORE it is discarded,
   * or every push would leave one ticking against a detached element.
   */
  function draw(): void {
    if (disposed) return;
    // WHILE A CLIENT VERDICT STANDS IT WINS, and the daemon's last pushed view
    // is not drawn at all. A push landing under a standing verdict is not
    // evidence the link is up -- the footer stream and the verb that failed are
    // different calls -- so the verdict is lifted by `clearClientFailures`,
    // never by a redraw. See `src/rpc/link.ts`.
    const verdict = standingClientFailure();
    if (view === null) {
      // NOTHING PUSHED YET. With a verdict standing the strip is the client's
      // three cells alone; without one there is nothing to draw at all.
      if (verdict === null) return;
      const bare = document.createElement("div");
      bare.className = "pfooter";
      bare.setAttribute("role", "status");
      bare.setAttribute("aria-live", "polite");
      bare.setAttribute("data-client-verdict", verdict.kind);
      bare.appendChild(drawClientDisconnectedStrip(verdict.substatus, verdict.activity));
      stopTicking(host);
      host.replaceChildren(bare);
      return;
    }
    const strip = requireMessage(view.strip, "FooterView.strip");
    const expanded = requireMessage(view.expanded, "FooterView.expanded");

    const dock = document.createElement("div");
    dock.className = "pfooter";
    dock.setAttribute("role", "status");
    dock.setAttribute("aria-live", "polite");

    // THE SHEET IS HANDED THE STRIP'S OWN ACTIVITY. The tokens sheet expands
    // the usage line the strip is drawing, and handing the message across is
    // what keeps the two from disagreeing about a figure.
    const panel = drawFooterExpanded(expanded, selection, {
      ctx,
      stops,
      selectDetachedWork: deps.selectDetachedWork,
      activity: footerStatusActivity(requireMessage(strip.status, `FooterStrip.status`)),
    });
    const stripDeps = { ctx, selection, onSelect: select, stops };
    dock.appendChild(
      verdict === null
        ? drawFooterStrip(strip, stripDeps)
        : drawClientDisconnectedStrip(verdict.substatus, verdict.activity, {
            strip,
            deps: stripDeps,
          }),
    );
    if (verdict !== null) dock.setAttribute("data-client-verdict", verdict.kind);
    if (panel !== null) {
      dock.appendChild(drawFooterDivider());
      dock.appendChild(panel);
    }

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
    log.debug(`the footer panel selection is now ${selection ?? "none"}`, {
      operation: "footer.select-panel",
      context: { panel: selection },
    });
    writeSelection(ctx, selection);
    draw();
  }

  /**
   * Open the panel the daemon's focus names, when this page has not applied
   * its generation yet.
   *
   * APPLIED ONCE, THEN THE READER'S. The generation is compared for equality
   * only, never ordered: an unseen one is applied (opening the section if it
   * was closed) and remembered, and a repeat of it is ignored, so a click
   * between launches is never overridden by the pushes that follow it. The
   * choice is persisted exactly as a click's is, so a reload restores it.
   */
  function applyFocus(pushed: FooterView): void {
    const focus = pushed.focus;
    if (focus === undefined || focus.generation === appliedFocus) return;
    const panel: FooterPanel = requireCase(focus.panel, "FooterExpandedFocus.panel").case;
    appliedFocus = focus.generation;
    selection = panel;
    log.info(`the daemon focused the footer on the ${panel} panel`, {
      operation: "footer.focus-applied",
      context: { panel, generation: String(focus.generation) },
    });
    writeSelection(ctx, selection);
  }

  /** Tell every subscriber which status arm this push carried. */
  function publishStatus(): void {
    // THE CLIENT'S VERDICT IS NOT PUBLISHED TO THE COMPOSER GATE, and that is
    // deliberate. R7 closes the composer on a PUSHED `disconnected` because
    // the daemon says the session is gone; a client verdict says the last
    // thing this page sent did not arrive -- and the only thing that lifts it
    // is the user sending something that does. Closing the composer over it
    // would take away the retry the strip's own line invites.
    if (view === null) return;
    const strip = requireMessage(view.strip, "FooterView.strip");
    const status = requireMessage(strip.status, "FooterStrip.status");
    statusCase = requireCase(status.status, "FooterStatus.status").case;
    for (const fn of [...statusListeners]) fn(statusCase);
  }

  /**
   * Republish the compaction line this push carried, for whoever is WAITING on
   * the compaction rather than reading the strip.
   *
   * The cold gate's "compact and resume" latches its card inert for as long as
   * the compaction runs, and the daemon's own phase line is already arriving
   * here. Publishing it is what lets that card draw the daemon's sentence
   * instead of composing one of its own or saying nothing at all. Any other
   * activity arm — and an absent activity — publishes `null`: the subscriber
   * then shows nothing.
   *
   * Called AFTER `draw()`, which has already walked the same view, so a
   * malformed strip is reported by the drawing rather than here.
   */
  function publishProgress(): void {
    if (view === null) return;
    const strip = requireMessage(view.strip, "FooterView.strip");
    const activity = footerStatusActivity(requireMessage(strip.status, "FooterStrip.status"));
    const kind = activity?.kind;
    publishCompactionProgress(kind?.case === "compaction" ? kind.value.text : null);
  }
}

/**
 * The single line partitioning the strip from the expanded section.
 *
 * A PRESENTATIONAL ELEMENT and not a border on either neighbour: a border on
 * the section would be inset by whatever padding that section carries, and the
 * ruling asks for a line that spans the footer's full width so the two sections
 * are fully partitioned. It paints `--border-strong`, one step darker than the
 * `--border` the section's own row delimiters take.
 */
export function drawFooterDivider(): HTMLElement {
  const divider = document.createElement("div");
  divider.className = "footer-divider";
  divider.setAttribute("role", "presentation");
  return divider;
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
      log.warn(`discarding an unrecognized stored footer panel: ${stored}`, {
        operation: "footer.selection-unrecognized",
        context: { stored },
      });
      return null;
    }
    return stored as FooterPanel;
  } catch (err) {
    log.warn(`could not read the footer's panel preference: ${String(err)}`, {
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
    log.warn(`could not store the footer's panel preference: ${String(err)}`, {
      operation: "footer.selection-write-failed",
      context: { cause: err },
    });
  }
}
