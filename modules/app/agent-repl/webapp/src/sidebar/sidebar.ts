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
 * NOTHING ABOUT THE VIEW IS LOCAL. Which grouping is shown, which sections
 * are folded and which rows have their detail open are the daemon's, carried
 * on the roster push, so the sidebar looks the same in every workspace's page
 * (owner rulings, 2026-10-06; `view.ts`). A page that still carries the
 * preferences it once stored in `localStorage` starts from the daemon's view
 * instead, says so once at INFO, and drops what it stored.
 */
import { createControl, type Control } from "../control.js";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { createDrawnStatusLog } from "../drawn-status.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, requireMessage, unreachablePushArm } from "../rpc/strict.js";
import { watchStream } from "../rpc/streams.js";
import { AttentionRegistry, type BlinkTimers } from "./attention.js";
import { createDropdowns } from "./dropdowns.js";
import { changeView } from "./view-change.js";
import { createSidebarView, GROUPING_VIEW_KEY } from "./view.js";
import type { Grouping, SidebarContext } from "./context.js";
import { drawRosterShown, drawWorkspaceRoster } from "./roster.js";
import { placeOpenRowDetails } from "./row.js";
import { installMergedFit } from "./merged-fit.js";
import { createSelectionEdge } from "./selection-edge.js";

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
  log.debug("mounting the workspaces rail", { operation: "sidebar.mount" });

  retireStoredPrefs(deps.storage === undefined ? pageStorage() : deps.storage);
  const view = createSidebarView();
  const attention = new AttentionRegistry(deps.timers);
  // THE ROSTER IS WHERE A SWITCH TO THIS WORKSPACE IS STATED, whichever path
  // made it, so the one edge every switch crosses is watched here.
  const selectionEdge =
    deps.workspaceSelected === undefined
      ? null
      : createSelectionEdge(ctx.workspace.id, deps.workspaceSelected);

  /** Teardowns the CURRENT drawing owns; replaced wholesale on every push. */
  let disposers: Array<() => void> = [];
  const clear = (): void => {
    for (const fn of disposers) fn();
    disposers = [];
  };

  const sc: SidebarContext = {
    ctx,
    view,
    openDetails: new Set<string>(),
    drawnStatus: createDrawnStatusLog("sidebar row", "sidebar.drawn-status"),
    dropdowns: createDropdowns(),
    attention,
    tasks: [],
    onDispose: (fn) => {
      disposers.push(fn);
    },
  };

  const body = document.createElement("div");
  body.className = "sb-scroll";
  const head = drawRailHead(sc);
  host.replaceChildren(head.element, body);
  // THE RECENTLY MERGED FIT (merged-fit.ts): re-fitted after every draw and on
  // every resize of the rail or a pane.
  const mergedFit = installMergedFit(body);

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
      // The teardowns come down BEFORE the draw, so a ticking age about to be
      // replaced cannot paint a node already detached.
      clear();
      // The blink pass brackets the draw: markers are re-attached to the phases
      // already running, and any marker the daemon cleared is forgotten.
      attention.beginPass();
      const drawn = drawWorkspaceRoster(roster, sc);
      attention.endPass();
      body.replaceChildren(drawn);
      // The picker is a copy of the grouping too, drawn once at mount.
      head.paintGrouping(sc.view.track(GROUPING_VIEW_KEY, drawRosterShown(roster.shown, "WorkspaceRoster.shown"), head.paintGrouping));
      // A detail panel is fixed-positioned so it can leave the rail, which
      // means it can only be measured once it is ON the page: a row drawn
      // already-expanded is placed here, after the draw is in the document.
      placeOpenRowDetails(body);
      // THE FIRST PUSH REVEALS THE RAIL, and nothing else ever does.
      host.hidden = false;
      // Fitted once the rail is shown, so the band measures the rail it is in.
      mergedFit.refit();
      selectionEdge?.observe(roster);
    },
  });

  return {
    dispose(): void {
      log.debug("disposing the workspaces rail", { operation: "sidebar.dispose" });
      stream.cancel();
      mergedFit.dispose();
      window.removeEventListener("resize", replace);
      window.removeEventListener("scroll", replace, true);
      host.removeEventListener("click", dismiss);
      clear();
      attention.dispose();
      host.replaceChildren();
      host.hidden = true;
    },
  };
}

/** The rail's header, and how to paint its picker. */
export interface RailHead {
  element: HTMLElement;
  /** Light the picker button for GROUPING. */
  paintGrouping: (grouping: Grouping) => void;
}

/**
 * The rail's header: its title and the grouping picker.
 *
 * The picker SELECTS between two resolved views — it hides one pane and shows
 * the other — so switching costs no round trip to draw and derives nothing.
 * Which one is shown is the daemon's view state: the click paints at once and
 * asks (`view-change.ts`), and the push lights every page's picker alike. It
 * is drawn once at mount, outside what a push replaces, so each push paints
 * it again.
 */
export function drawRailHead(sc: SidebarContext): RailHead {
  const head = document.createElement("div");
  head.className = "sb-head";

  const title = document.createElement("span");
  title.className = "sb-title";
  title.textContent = "Workspaces";
  head.appendChild(title);

  const views = document.createElement("div");
  views.className = "sb-views";
  const buttons = new Map<Grouping, Control>();
  for (const grouping of ["repository", "task"] as const) {
    const button = createControl();
    button.className = "sb-view-btn";
    button.setAttribute("data-grouping-pick", grouping);
    button.textContent = grouping === "repository" ? "Repo" : "Task";
    button.addEventListener("click", (event) => {
      event.preventDefault();
      selectGrouping(grouping, sc, button);
    });
    buttons.set(grouping, button);
    views.appendChild(button);
  }
  head.appendChild(views);
  return {
    element: head,
    paintGrouping: (grouping) => {
      for (const [name, button] of buttons) button.classList.toggle("active", name === grouping);
    },
  };
}

/** Show one grouping in every page: painted here at once, asked of the daemon. */
function selectGrouping(grouping: Grouping, sc: SidebarContext, button: Control): void {
  log.info("switching the rail's grouping for every page", {
    operation: "sidebar.grouping",
    context: { grouping },
  });
  void changeView(sc, {
    key: GROUPING_VIEW_KEY,
    value: grouping,
    control: button,
    outlivesPush: true,
    change: {
      case: "showGrouping",
      value: {
        grouping: grouping === "task" ? { case: "task", value: {} } : { case: "repository", value: {} },
      },
    },
  });
}
