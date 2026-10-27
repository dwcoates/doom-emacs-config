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
import { WINDOW_TIMERS, type Timers } from "../timers.js";
import { AttentionRegistry } from "./attention.js";
import { createDropdowns } from "./dropdowns.js";
import { createRowHover } from "./row-hover.js";
import { createRecentlyDeployed, installRecentlyDeployedCollapse } from "./recently-deployed.js";
import { pageFocusLeft, type FocusLeft } from "../focus-left.js";
import { installTransients } from "./transients.js";
import { createArrangeHold } from "./arrange.js";
import { createSidebarView } from "./view.js";
import type { RailContext } from "./context.js";
import { disposeWorkspaceRoster, drawWorkspaceRoster } from "./roster.js";
import { nextFrame } from "../frame.js";
import { createRowReveal } from "./reveal.js";
import { guardMalformedNow } from "../rpc/guard.js";
import { placeOpenRowDetails } from "./row.js";
import { installBandFit } from "./band-fit.js";
import { createAppSwitcher, disposeThreadList, drawThreadList, showsChat, type AppSwitcher } from "./threads.js";
import { applySearch, createRailSearch, type RailSearch } from "./search.js";
import { createChatSearch } from "./chat-search.js";
import { createSelectionEdge } from "./selection-edge.js";
import { installWallPhase, type WallPhaseEnv } from "./wall-phase.js";
import { createUnseenGate, unseenEnv, type UnseenEnv } from "./unseen.js";
import { placeChildren } from "./children.js";
import { queryAll } from "../dom.js";
import { createSelectionFade } from "./selection-fade.js";
import { createRailScrollKeep } from "./scroll-keep.js";
import { createSelectionScroll, RAIL_BODY_CLASS } from "./selection-scroll.js";
import type { RosterVersion, WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

/** The webhost's sidebar, which the webhost also tells what it shows. */
export interface WebhostSidebarHandle extends Handle {
  /**
   * The webhost now shows workspace ID's page: move the highlight to its row
   * now, without waiting for the daemon's roster (owner ruling, 2026-10-17).
   */
  markShown(id: string): void;
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
  timers?: Timers;
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
  /** Schedules a callback for the next animation frame, where a push is drawn. Defaults to the page's. */
  frame?: (callback: () => void) => void;
  /**
   * The height a standing gate band takes from under the rail, in px, which
   * comes out of Deploys and Hidden Workspaces and never the Open section
   * (`band-fit.ts`). Omitted: no gate band is ever under the rail.
   */
  gateBand?: () => number;
  /** Where the page hears that the user's focus left it (`focus-left.ts`). Defaults to the page's. */
  focusLeft?: FocusLeft;
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
 * to: every push is drawn, and the stream always stands (the webhost is on
 * screen whenever any page is).
 */
export function mountWebhostSidebar(
  host: HTMLElement,
  ctx: DaemonContext,
  deps: WebhostSidebarDeps = {},
): WebhostSidebarHandle {
  return mountSidebarIn(host, ctx, { case: "webhost" }, deps);
}

/** What the webhost's sidebar may have injected, beside the shared deps. */
export type WebhostSidebarDeps = Pick<SidebarDeps, "storage" | "timers" | "wallPhase" | "frame" | "gateBand"> & {
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
): WebhostSidebarHandle {
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
  /** The workspace the roster last named current. */
  let current: WorkspaceRef | null = null;
  /**
   * THE PAGE ON SCREEN, AHEAD OF THE ROSTER. A switch reaches the webhost's
   * page directly from Emacs, but the rail's highlight waited for the
   * daemon's select, its whole roster, and a full redraw: 82ms after the page
   * typically and 308ms at p90, worse under load (switch bench, 2026-10-17).
   * The webhost now tells the rail which page it shows (`markShown`), the
   * highlight moves at once, and every draw keeps it there until a roster
   * names that workspace current, so a push sent before the daemon took the
   * select never moves it back.
   */
  let shownAhead: string | null = null;

  // THE SELECTED ROW IS SCROLLED INTO ITS SECTION'S VIEW (selection-scroll.ts),
  // once per selection, after a draw has fitted the bands and whenever the
  // webhost moves the highlight ahead of the roster.
  const selectionScroll = createSelectionScroll();
  // A REDRAW NEVER MOVES THE RAIL (scroll-keep.ts): every viewport is read
  // before a draw and put back once the bands are fitted.
  const scrollKeep = createRailScrollKeep();

  const openDetails = new Set<string>();
  const dropdowns = createDropdowns();
  // A ROW DRAWN WHEN IT COMES INTO VIEW (reveal.ts) is drawn after the
  // roster's own draw ran the search and placed the open panels, so both are
  // run again over the rail.
  const reveal = createRowReveal(() => {
    applySearch(body, search.query());
    chatSearch.changed(search.query(), chatShown);
    chatSearch.apply(body);
    placeOpenRowDetails(body);
  });
  const body = document.createElement("div");
  body.className = RAIL_BODY_CLASS;
  // THE ARRANGEMENT'S OWN ELEMENTS OUTLIVE EVERY PUSH (arrange.ts): the layer
  // a drag's copy and a name popup are drawn in, and the slot a refused
  // arrangement is said in.
  const arrangeLayer = document.createElement("div");
  arrangeLayer.className = "sb-arrange-layer";
  const arrangeStatus = document.createElement("div");
  arrangeStatus.className = "sb-arrange-status";
  const rail: RailContext = {
    ctx,
    view,
    openDetails,
    drawnStatus: createDrawnStatusLog("sidebar row", "sidebar.drawn-status"),
    dropdowns,
    rowHover: createRowHover(dropdowns, openDetails),
    attention,
    reveal,
    selectionFade: createSelectionFade(body),
    shownAhead: () => shownAhead,
    arrange: { hold: createArrangeHold((roster) => drawRoster(roster)), layer: arrangeLayer, status: arrangeStatus },
    // A CHANGE IS FITTED AT ONCE: the rows shown or let go are the Deploys
    // band's lines (band-fit.ts).
    recentlyDeployed: createRecentlyDeployed(body, () => bandFit.refit()),
  };
  // THE BAND FIT (band-fit.ts): the Open, Hidden Workspaces and Recently
  // Merged viewports, re-fitted after every draw, on every resize of the rail
  // or a pane, and when the local date turns over.
  // A RESIZE KEEPS THE SELECTED ROW IN VIEW (selection-scroll.ts): a gate
  // docking under the rail shrinks its sections with no push and no switch.
  const bandFit = installBandFit(body, ctx.ticker, {
    onResized: () => selectionScroll.resized(body),
    gateBand: deps.gateBand,
  });
  // Every viewport's scroll, the reader's or the rail's, notes whether the
  // selected row still shows, which decides whether a resize brings it back.
  body.addEventListener("scroll", () => selectionScroll.scrolled(body), true);
  // THE SEARCH NARROWS THE DRAWN ROWS (search.ts): on every keystroke, and
  // again after every push and every row drawn as it came into view, since
  // either draws rows the query has not narrowed.
  // WHETHER CHAT IS SHOWN is the last drawn roster's, and decides whether
  // the search also asks the chats (chat-search.ts).
  let chatShown = false;
  const chatSearch = createChatSearch({
    ctx,
    timers: deps.timers ?? WINDOW_TIMERS,
    root: () => body,
    applied: () => bandFit.refit(),
  });
  const search = createRailSearch((query) => {
    const outcome = applySearch(body, query);
    chatSearch.changed(query, chatShown);
    chatSearch.apply(body);
    log.debug("narrowed the rail to the search", {
      operation: "sidebar.search",
      context: { query, ...outcome },
    });
    bandFit.refit();
  });
  const head = drawRailHead(search, createAppSwitcher(ctx, dropdowns));
  host.replaceChildren(head.element, search.bar, arrangeStatus, body, arrangeLayer);
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
    rail.dropdowns.dismissOutside(event.target);
  };
  host.addEventListener("click", dismiss);
  // THE RAIL LETS GO OF WHAT THE POINTER RAISED (transients.ts) the moment
  // the user leaves it: the page's focus gone, the page hidden, the selection
  // moved. The host, which no draw replaces, also says where the pointer is
  // (row-hover.ts): a row drawn under a resting pointer never sees it leave,
  // but the host sees every move and the pointer leaving the rail.
  const transients = installTransients(host, { dropdowns, rowHover: rail.rowHover, win: window });
  // "RECENTLY DEPLOYED" COLLAPSES THE MOMENT THE USER IS DONE WITH IT
  // (recently-deployed.ts): the focus gone from the page, or a workspace
  // outside its rows clicked.
  const removeRecentlyDeployedCollapse = installRecentlyDeployedCollapse(host, rail.recentlyDeployed, deps.focusLeft ?? pageFocusLeft());

  // ONLY A PAGE'S OWN SIDEBAR IS GATED: the webhost's is the one on screen.
  const unseen =
    placement.case === "page"
      ? createUnseenGate(placement.ctx.workspace.id, (roster) => rail.arrange.hold.offer(roster), deps.unseen ?? unseenEnv(placement.ctx.seen))
      : null;
  const stream = watchStream(ctx, {
    name: "WatchWorkspaceRoster",
    schema: WatchWorkspaceRosterResponseSchema,
    open: (_client, signal) => ctx.streams.watch("roster", {}, signal),
    plannedEnding: (response) => response.push.case === "ending",
    // A run that ended forgets what `current` was: the next run's first push
    // is a baseline, so a reconnect restating it is never taken for a switch.
    onEnd: () => {
      selectionEdge?.reset();
      rail.arrange.hold.reset();
    },
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
      // THE ROSTER CAUGHT UP with the page on screen: it decides again.
      if (shownAhead !== null && current?.id === shownAhead) {
        log.debug("the roster named the shown workspace current; the rail follows the roster again", {
          operation: "sidebar.shown-confirmed",
          context: { workspace: shownAhead },
        });
        shownAhead = null;
      }
      transients.selected(shownAhead ?? current?.id ?? null);
      // ONLY A SEEN PAGE DRAWS (unseen.ts): an unseen one keeps the newest
      // roster and draws it once when it is seen again.
      // A PUSH WAITS WHILE THE PAGE HOLDS ITS OWN DRAWING of an arrangement
      // it dragged or asked for (arrange.ts), so the moved item is never put
      // back for an instant.
      if (unseen === null) rail.arrange.hold.offer(roster);
      else unseen.offer(roster);
      // EVERY push moves the selection edge, drawn or held, so the edge's
      // baseline is the roster the daemon last stated, not the last one drawn.
      selectionEdge?.observe(roster);
    },
  });

  // AT MOST ONE DRAW PER FRAME, OF THE NEWEST ROSTER (2026-10-15). Pushes
  // arrive in bursts (ten inside one second at a page's boot), and each used
  // to draw at once, so a burst paid for every roster in it while only the
  // last was ever seen. A push now replaces the one waiting, and the frame
  // draws whichever is newest.
  const frame = deps.frame ?? nextFrame;
  let waiting: WorkspaceRoster | null = null;
  let disposed = false;

  /** Draw ROSTER on the next frame, replacing any roster still waiting for it. */
  function drawRoster(roster: WorkspaceRoster): void {
    const scheduled = waiting !== null;
    waiting = roster;
    if (scheduled) return;
    frame(() => {
      const newest = waiting;
      waiting = null;
      if (disposed || newest === null) return;
      // The push's stream returned before this frame, so a roster the draw
      // finds malformed is filed here, as the stream would have filed it.
      guardMalformedNow(ctx, "sidebar.roster-draw", () =>
        timeRosterDraw(body, placement.case, (mark) => drawRosterNow(newest, mark), drawTiming),
      );
    });
  }

  /** Draw one roster into the rail, marking each phase's end. */
  function drawRosterNow(roster: WorkspaceRoster, mark: MarkPhase): void {
    // The blink pass brackets the draw: markers are re-attached to the phases
    // already running, and any marker the daemon cleared is forgotten.
    attention.beginPass();
    scrollKeep.capture(body, chatShown ? "chat" : "workspaces");
    // THE TITLE IS THE APP SWITCHER, and the body is the app the roster shows
    // (threads.ts): a thread on screen shows Chat.
    const switcher = requireMessage(roster.appSwitcher, "WorkspaceRoster.app_switcher");
    head.switcher.update(switcher);
    let drawn: HTMLElement;
    chatShown = showsChat(switcher);
    if (chatShown) {
      drawn = drawThreadList(requireMessage(roster.threads, "WorkspaceRoster.threads"), rail);
    } else {
      disposeThreadList(rail);
      drawn = drawWorkspaceRoster(roster, rail);
    }
    attention.endPass();
    head.showVersion(roster.version);
    mark("build");
    placeChildren(body, [drawn]);
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
    // Restored once the bands are fitted, so a viewport is clamped only by
    // the height the draw leaves it, and before the follow, which may move it.
    scrollKeep.restore(body, chatShown ? "chat" : "workspaces");
    // Followed once the bands are fitted, so it scrolls the viewport they drew.
    selectionScroll.follow(body);
    // Seeked once the draw is in the document, where its animations run.
    wallPhase.seek();
    mark("hooks");
  }

  return {
    markShown(id: string): void {
      shownAhead = id;
      transients.selected(id);
      let rows = 0;
      for (const row of queryAll<HTMLElement>(body, "[data-roster-row]")) {
        const highlighted = row.getAttribute("data-roster-row") === id;
        row.classList.toggle("current", highlighted);
        if (highlighted) {
          row.setAttribute("data-current", "true");
          rows += 1;
        } else {
          row.removeAttribute("data-current");
        }
      }
      rail.selectionFade.highlight(id);
      selectionScroll.follow(body);
      log.debug("the webhost showed a page; the rail highlights its row ahead of the roster", {
        operation: "sidebar.shown-ahead",
        context: { workspace: id, rows },
      });
    },
    dispose(): void {
      log.debug("disposing the workspaces rail", { operation: "sidebar.dispose" });
      disposed = true;
      stream.cancel();
      unseen?.dispose();
      bandFit.dispose();
      wallPhase.dispose();
      window.removeEventListener("resize", replace);
      window.removeEventListener("scroll", replace, true);
      host.removeEventListener("click", dismiss);
      transients.dispose();
      removeRecentlyDeployedCollapse();
      rail.rowHover.dispose();
      disposeWorkspaceRoster(rail);
      disposeThreadList(rail);
      chatSearch.dispose();
      reveal.dispose();
      attention.dispose();
      host.replaceChildren();
      host.hidden = true;
    },
  };
}

/** The rail's header. */
export interface RailHead {
  element: HTMLElement;
  /** State the roster's agent-repl version at the head's right; none hides it. */
  showVersion(version: RosterVersion | undefined): void;
  /** The title, which is the app switcher (threads.ts). */
  switcher: AppSwitcher;
}

/** The rail's header: the search toggle, its title (the app switcher), and the agent-repl version. */
export function drawRailHead(search: RailSearch, switcher: AppSwitcher): RailHead {
  const head = document.createElement("div");
  head.className = "sb-head";
  // THE APPS MENU HANGS FROM THE TITLE'S ANCHOR (threads.ts, styles.css),
  // so the toggle, the title and the version stay adjacent, in drawn order.
  head.append(search.toggle, switcher.element);

  // THE VERSION IS THE DAEMON'S (frontend.v1.WorkspaceRoster.version), drawn
  // verbatim; UNSET until the daemon has resolved one, when nothing is drawn.
  const version = document.createElement("span");
  version.className = "sb-version";
  version.hidden = true;
  head.appendChild(version);
  return {
    element: head,
    switcher,
    showVersion(next: RosterVersion | undefined): void {
      version.hidden = next === undefined;
      version.textContent = next?.text ?? "";
    },
  };
}
