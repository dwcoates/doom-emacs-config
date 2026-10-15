/**
 * sidebar — the workspaces rail: THE ONE GLOBAL STREAM, drawn whole.
 *
 * `WatchWorkspaceRoster` carries no workspace. Every webview — one per
 * workspace — watches the same roster, and the daemon's stream order is the
 * only order, because there is no outside publisher to reconcile against. The
 * roster is ALWAYS WHOLE, never a delta, so a push throws the previous DOM
 * away rather than reconciling it: a workspace that left is simply absent from
 * the next push, and deletion is row omission, never an event.
 *
 * THE RAIL SHIPS HIDDEN and reveals itself on its FIRST PUSH, so a session
 * that never receives a roster keeps the single-column layout instead of
 * reserving a fifth of the window for an empty dock.
 *
 * NOTHING ABOUT THE VIEW IS LOCAL. Which sections are folded and which rows
 * have their detail open are the daemon's, carried
 * on the roster push, so the sidebar looks the same in every workspace's page
 * (owner rulings, 2026-10-06; `view.ts`). A page that still carries the
 * preferences it once stored in `localStorage` starts from the daemon's view
 * instead, says so once at INFO, and drops what it stored.
 */
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { createDrawnStatusLog } from "../drawn-status.js";
import { log } from "../log.js";
import { drawTimingEnv, timeRosterDraw, type DrawTimingEnv, type MarkPhase } from "./draw-timing.js";
import type { AppContext, DaemonContext } from "../rpc/context.js";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { requireCase, requireMessage, unreachablePushArm } from "../rpc/strict.js";
import { watchStream } from "../rpc/streams.js";
import { AttentionRegistry, type BlinkTimers } from "./attention.js";
import { createDropdowns } from "./dropdowns.js";
import { createRowHover } from "./row-hover.js";
import { createSidebarView } from "./view.js";
import type { SidebarContext } from "./context.js";
import { drawWorkspaceRoster } from "./roster.js";
import { placeOpenRowDetails } from "./row.js";
import { installBandFit } from "./band-fit.js";
import { applySearch, createRailSearch, type RailSearch } from "./search.js";
import { createSelectionEdge } from "./selection-edge.js";
import { installWallPhase, type WallPhaseEnv } from "./wall-phase.js";
import { createUnseenGate, unseenEnv, type UnseenEnv } from "./unseen.js";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

/**
 * The `localStorage` key the rail's preferences lived under before the view
 * was the daemon's. Read only to retire it.
 */
export const PREFS_KEY = "agent-repl.sidebar";

/** What a mount may have injected, for a suite that drives the cadence. */
export interface SidebarDeps {
  /**
   * Where the retired preferences may linger. Defaults to the page's
   * `localStorage`.
   */
  storage?: Storage | null;
  /** The blink timers. Defaults to the page's own. */
  timers?: BlinkTimers;
  /**
   * Told when this page's workspace becomes the selected one
   * (`selection-edge.ts`): the feed returns to its tail. Omitted by a mount
   * with no feed to move.
   */
  workspaceSelected?: () => void;
  /**
   * What the wall-clock seek of the rail's looping animations reads
   * (`wall-phase.ts`). Defaults to the page.
   */
  wallPhase?: WallPhaseEnv;
  /** What the unseen gate reads (`unseen.ts`). Defaults to the page. */
  unseen?: UnseenEnv;
  /** What a roster draw's timing reads (`draw-timing.ts`). Defaults to the page. */
  drawTiming?: DrawTimingEnv;
}

/**
 * Drop the preferences a page stored before the view was the daemon's.
 *
 * NOTHING IS MIGRATED: the daemon's view stands, and a page's old folds and
 * grouping are not pushed into it. Their presence is said once at INFO, so a
 * reader of the log knows why this page's sidebar changed under them. Every
 * access is guarded: `localStorage` throws outright in some embeddings, and a
 * failure here costs nothing but the tidy-up.
 */
export function retireStoredPrefs(storage: Storage | null): void {
  if (storage === null) return;
  try {
    const stored = storage.getItem(PREFS_KEY);
    if (stored === null) return;
    storage.removeItem(PREFS_KEY);
    log.info("the daemon's sidebar view supersedes the preferences this page stored; they are dropped", {
      operation: "sidebar.prefs.superseded",
      context: { stored },
    });
  } catch (err) {
    log.warn(`the sidebar could not retire its stored preferences: ${String(err)}`, {
      operation: "sidebar.prefs.retire-failed",
      context: { cause: err },
    });
  }
}

/** The page's storage, or null where reaching for it throws. */
function pageStorage(): Storage | null {
  try {
    return globalThis.localStorage;
  } catch (err) {
    log.warn(`the sidebar has no usable local storage: ${String(err)}`, {
      operation: "sidebar.prefs.unavailable",
      context: { cause: err },
    });
    return null;
  }
}

/**
 * Mount the rail on HOST and keep it drawn.
 *
 * The stream is STANDING: it never concludes on its own, so `dispose()` is the
 * only thing that closes it — and closing it stops nothing daemon-side, since
 * the roster is the daemon's whether anyone is watching or not.
 */
export function mountSidebar(host: HTMLElement, ctx: AppContext, deps: SidebarDeps = {}): Handle {
  return mountSidebarIn(host, ctx, { case: "page", ctx }, deps);
}

/**
 * Mount the ONE sidebar the webhost draws for every workspace page.
 *
 * It owns no workspace, so there is no page of its own to be seen or switched
 * to: every push is drawn, the stream always stands (the webhost is on screen
 * whenever any page is), and a child created from the form is a child of the
 * roster's `current`, the workspace Emacs is standing in.
 */
export function mountWebhostSidebar(host: HTMLElement, ctx: DaemonContext, deps: WebhostSidebarDeps = {}): Handle {
  return mountSidebarIn(host, ctx, { case: "webhost" }, deps);
}

/** What the webhost's sidebar may have injected, beside the shared deps. */
export type WebhostSidebarDeps = Pick<SidebarDeps, "storage" | "timers" | "wallPhase"> & {
  /** Told once, when the first push reveals the sidebar. */
  revealed?: () => void;
};

/** Where a sidebar is drawn, and what that placement needs. */
type SidebarPlacement =
  /** In a workspace page on its own (dev mode, the webapp's own tests). */
  | { case: "page"; ctx: AppContext }
  /** In the webhost, once for every workspace page. */
  | { case: "webhost" };

function mountSidebarIn(
  host: HTMLElement,
  ctx: DaemonContext,
  placement: SidebarPlacement,
  deps: SidebarDeps & WebhostSidebarDeps,
): Handle {
  log.debug("mounting the workspaces rail", { operation: "sidebar.mount", context: { placement: placement.case } });

  retireStoredPrefs(deps.storage === undefined ? pageStorage() : deps.storage);
  const view = createSidebarView();
  const drawTiming = deps.drawTiming ?? drawTimingEnv();
  const attention = new AttentionRegistry(deps.timers);
  // THE ROSTER IS WHERE A SWITCH TO THIS WORKSPACE IS STATED, whichever path
  // made it, so the one edge every switch crosses is watched here.
  const selectionEdge =
    placement.case !== "page" || deps.workspaceSelected === undefined
      ? null
      : createSelectionEdge(placement.ctx.workspace.id, deps.workspaceSelected);
  /** The workspace the roster last named current, for the webhost's create form. */
  let current: WorkspaceRef | null = null;

  /** Teardowns the CURRENT drawing owns; replaced wholesale on every push. */
  let disposers: Array<() => void> = [];
  const clear = (): void => {
    for (const fn of disposers) fn();
    disposers = [];
  };

  const openDetails = new Set<string>();
  const dropdowns = createDropdowns();
  const sc: SidebarContext = {
    ctx,
    parentWorkspace: () => (placement.case === "page" ? placement.ctx.workspace : current),
    view,
    openDetails,
    drawnStatus: createDrawnStatusLog("sidebar row", "sidebar.drawn-status"),
    dropdowns,
    rowHover: createRowHover(dropdowns, openDetails),
    attention,
    onDispose: (fn) => {
      disposers.push(fn);
    },
  };

  const body = document.createElement("div");
  body.className = "sb-scroll";
  // THE BAND FIT (band-fit.ts): the Hidden Workspaces and Recently Merged
  // viewports, re-fitted after every draw and on every resize of the rail or
  // a pane.
  const bandFit = installBandFit(body);
  // THE SEARCH NARROWS THE DRAWN ROWS (search.ts): on every keystroke, and
  // again after every push, since a push throws the narrowed drawing away.
  const search = createRailSearch((query) => {
    const outcome = applySearch(body, query);
    log.debug("narrowed the rail to the search", {
      operation: "sidebar.search",
      context: { query, ...outcome },
    });
    bandFit.refit();
  });
  const head = drawRailHead(search);
  host.replaceChildren(head.element, search.bar, body);
  // THE RAIL'S LOOPS RUN ON THE WALL CLOCK (wall-phase.ts), so neither a
  // redraw nor a switch to another workspace's page restarts one.
  const wallPhase = installWallPhase(host, deps.wallPhase);

  // A FIXED PANEL DOES NOT TRAVEL WITH ITS ROW. It is anchored to the row's
  // rectangle at the moment it was placed, so anything that moves that
  // rectangle -- a resized window, the rail's own scroller, the feed's, any
  // scroller on the page -- has to place it again. Captured, because a scroll
  // inside a nested scroller does not bubble to the window.
  const replace = (): void => {
    placeOpenRowDetails(body);
  };
  window.addEventListener("resize", replace);
  window.addEventListener("scroll", replace, true);
  // ANY CLICK IN THE RAIL OUTSIDE AN OPEN DROPDOWN CLOSES IT (owner ruling,
  // 2026-10-06), empty space included; `dropdowns.ts` is the one rule.
  const dismiss = (event: MouseEvent): void => {
    sc.dropdowns.dismissOutside(event.target);
  };
  host.addEventListener("click", dismiss);
  // THE RAIL SAYS WHERE THE POINTER IS (row-hover.ts): a row drawn under a
  // resting pointer never sees it leave, but the host, which no draw
  // replaces, sees every move and the pointer leaving the rail.
  const pointerMoved = (event: MouseEvent): void => {
    sc.rowHover.pointerAt(event.target);
  };
  const pointerLeft = (): void => {
    sc.rowHover.pointerAt(null);
  };
  host.addEventListener("mousemove", pointerMoved);
  host.addEventListener("mouseleave", pointerLeft);

  // ONLY A PAGE'S OWN SIDEBAR IS GATED: the webhost's is the one on screen.
  const unseen =
    placement.case === "page"
      ? createUnseenGate(placement.ctx.workspace.id, drawRoster, deps.unseen ?? unseenEnv(placement.ctx.seen))
      : null;
  const stream = watchStream(ctx, {
    name: "WatchWorkspaceRoster",
    schema: WatchWorkspaceRosterResponseSchema,
    open: (_client, signal) => ctx.streams.watch("roster", {}, signal),
    plannedEnding: (response) => response.push.case === "ending",
    // A run that ended forgets what `current` was: the next run's first push
    // is a baseline, so a reconnect restating it is never taken for a switch.
    onEnd: () => selectionEdge?.reset(),
    onPush: (response) => {
      const push = requireCase(response.push, "WatchWorkspaceRosterResponse.push");
      if (push.case !== "roster") {
        // A TOP-LEVEL push arm this build cannot draw is forward-compat skew,
        // not a contract violation: skipped quietly by the stream pipeline.
        // (`ending` never reaches here: the stream pipeline consumes it.)
        return unreachablePushArm("WatchWorkspaceRosterResponse.push", push.case);
      }
      const roster = requireMessage(push.value, "WatchWorkspaceRosterResponse.roster");
      current =
        roster.current === undefined
          ? null
          : requireMessage(roster.current.workspace, "WorkspaceRoster.current.workspace");
      // ONLY A SEEN PAGE DRAWS (unseen.ts): an unseen one keeps the newest
      // roster and draws it once when it is seen again.
      if (unseen === null) drawRoster(roster);
      else unseen.offer(roster);
      // EVERY push moves the selection edge, drawn or held, so the edge's
      // baseline is the roster the daemon last stated, not the last one drawn.
      selectionEdge?.observe(roster);
    },
  });

  /** Draw one whole roster into the rail, recording what the draw cost. */
  function drawRoster(roster: WorkspaceRoster): void {
    timeRosterDraw(body, placement.case, (mark) => drawRosterNow(roster, mark), drawTiming);
  }

  /** Draw one whole roster into the rail, marking each phase's end. */
  function drawRosterNow(roster: WorkspaceRoster, mark: MarkPhase): void {
    // The teardowns come down BEFORE the draw, so a ticking age about to be
    // replaced cannot paint a node already detached.
    clear();
    // The blink pass brackets the draw: markers are re-attached to the phases
    // already running, and any marker the daemon cleared is forgotten.
    attention.beginPass();
    const drawn = drawWorkspaceRoster(roster, sc);
    attention.endPass();
    mark("build");
    body.replaceChildren(drawn);
    mark("mount");
    // THE LAYOUT IS MEASURED ON ITS OWN: the hooks below read rectangles, so
    // the style and layout the mount caused are paid here either way.
    void body.offsetHeight;
    mark("layout");
    applySearch(body, search.query());
    // A detail panel is fixed-positioned so it can leave the rail, which
    // means it can only be measured once it is ON the page: a row drawn
    // already-expanded is placed here, after the draw is in the document.
    placeOpenRowDetails(body);
    // THE FIRST PUSH REVEALS THE RAIL, and nothing else ever does.
    if (host.hidden) {
      host.hidden = false;
      deps.revealed?.();
    }
    // Fitted once the rail is shown, so the band measures the rail it is in.
    bandFit.refit();
    // Seeked once the draw is in the document, where its animations run.
    wallPhase.seek();
    mark("hooks");
  }

  return {
    dispose(): void {
      log.debug("disposing the workspaces rail", { operation: "sidebar.dispose" });
      stream.cancel();
      unseen?.dispose();
      bandFit.dispose();
      wallPhase.dispose();
      window.removeEventListener("resize", replace);
      window.removeEventListener("scroll", replace, true);
      host.removeEventListener("click", dismiss);
      host.removeEventListener("mousemove", pointerMoved);
      host.removeEventListener("mouseleave", pointerLeft);
      sc.rowHover.dispose();
      clear();
      attention.dispose();
      host.replaceChildren();
      host.hidden = true;
    },
  };
}

/** The rail's header. */
export interface RailHead {
  element: HTMLElement;
}

/** The rail's header: the search toggle and its title. */
export function drawRailHead(search: RailSearch): RailHead {
  const head = document.createElement("div");
  head.className = "sb-head";
  head.appendChild(search.toggle);

  const title = document.createElement("span");
  title.className = "sb-title";
  title.textContent = "Workspaces";
  head.appendChild(title);
  return { element: head };
}
