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
 * WHAT IS LOCAL, AND ONLY WHAT IS LOCAL (R14). Three things are webview
 * preference and no element of the view: which grouping is shown, which
 * sections are folded, and which rows have their detail open. They live in
 * `localStorage` behind try/catch — a rail that cannot remember a fold must
 * still draw — and nothing else is persisted client-side.
 */
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, requireMessage, unreachablePushArm } from "../rpc/strict.js";
import { watchStream } from "../rpc/streams.js";
import { AttentionRegistry, type BlinkTimers } from "./attention.js";
import type { Grouping, SidebarContext, SidebarPrefs } from "./context.js";
import { drawWorkspaceRoster } from "./roster.js";
import { placeOpenRowDetails } from "./row.js";
import { createSelectionEdge } from "./selection-edge.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

/** The `localStorage` key the rail's preferences live under. */
export const PREFS_KEY = "agent-repl.sidebar";

/** The shape stored under `PREFS_KEY`. Absent fields take their defaults. */
interface StoredPrefs {
  grouping?: Grouping;
  folded?: Record<string, boolean>;
  expanded?: Record<string, boolean>;
}

/** What a mount may have injected, for a suite that drives the cadence. */
export interface SidebarDeps {
  /** Where preferences persist. Defaults to the page's `localStorage`. */
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
 * The webview-local preference set, persisted behind try/catch.
 *
 * EVERY ACCESS IS GUARDED, both directions. `localStorage` throws outright in
 * some embeddings — a private window, a webview with site data disabled — and
 * a preference is worth exactly nothing next to the rail drawing at all, so a
 * failure here costs the memory and is logged once, never raised.
 */
export function createSidebarPrefs(storage: Storage | null = pageStorage()): SidebarPrefs {
  let state: StoredPrefs = read(storage);

  const persist = (): void => {
    if (storage === null) return;
    try {
      storage.setItem(PREFS_KEY, JSON.stringify(state));
    } catch (err) {
      log.warn(`the sidebar could not persist its preferences: ${String(err)}`, {
        operation: "sidebar.prefs.write-failed",
        context: { cause: err },
      });
    }
  };

  return {
    grouping(): Grouping {
      return state.grouping === "task" ? "task" : "repository";
    },
    setGrouping(grouping: Grouping): void {
      state = { ...state, grouping };
      persist();
    },
    isFolded(key: string, defaultFolded = false): boolean {
      return state.folded?.[key] ?? defaultFolded;
    },
    setFolded(key: string, folded: boolean): void {
      state = { ...state, folded: { ...state.folded, [key]: folded } };
      persist();
    },
    isExpanded(id: string): boolean {
      return state.expanded?.[id] ?? false;
    },
    setExpanded(id: string, expanded: boolean): void {
      state = { ...state, expanded: { ...state.expanded, [id]: expanded } };
      persist();
    },
  };
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

/** Read what was stored, treating anything unreadable as nothing stored. */
function read(storage: Storage | null): StoredPrefs {
  if (storage === null) return {};
  try {
    const raw = storage.getItem(PREFS_KEY);
    if (raw === null) return {};
    const parsed: unknown = JSON.parse(raw);
    if (typeof parsed !== "object" || parsed === null) return {};
    return parsed;
  } catch (err) {
    log.warn(`the sidebar could not read its preferences: ${String(err)}`, {
      operation: "sidebar.prefs.read-failed",
      context: { cause: err },
    });
    return {};
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

  const prefs = createSidebarPrefs(deps.storage === undefined ? pageStorage() : deps.storage);
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
    prefs,
    attention,
    tasks: [],
    onDispose: (fn) => {
      disposers.push(fn);
    },
  };

  const body = document.createElement("div");
  body.className = "sb-scroll";
  const head = drawRailHead(prefs, body);
  host.replaceChildren(head, body);

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
      // A detail panel is fixed-positioned so it can leave the rail, which
      // means it can only be measured once it is ON the page: a row drawn
      // already-expanded is placed here, after the draw is in the document.
      placeOpenRowDetails(body);
      // THE FIRST PUSH REVEALS THE RAIL, and nothing else ever does.
      host.hidden = false;
      selectionEdge?.observe(roster);
    },
  });

  return {
    dispose(): void {
      log.debug("disposing the workspaces rail", { operation: "sidebar.dispose" });
      stream.cancel();
      window.removeEventListener("resize", replace);
      window.removeEventListener("scroll", replace, true);
      clear();
      attention.dispose();
      host.replaceChildren();
      host.hidden = true;
    },
  };
}

/**
 * The rail's header: its title and the grouping picker.
 *
 * The picker SELECTS between two resolved views — it hides one pane and shows
 * the other — so switching costs no round trip and derives nothing. It is the
 * same segmented control the rail has always carried.
 */
export function drawRailHead(prefs: SidebarPrefs, body: HTMLElement): HTMLElement {
  const head = document.createElement("div");
  head.className = "sb-head";

  const title = document.createElement("span");
  title.className = "sb-title";
  title.textContent = "Workspaces";
  head.appendChild(title);

  const views = document.createElement("div");
  views.className = "sb-views";
  const buttons = new Map<Grouping, HTMLButtonElement>();
  for (const grouping of ["repository", "task"] as const) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "sb-view-btn";
    button.setAttribute("data-grouping-pick", grouping);
    button.textContent = grouping === "repository" ? "Repo" : "Task";
    button.addEventListener("click", (event) => {
      event.preventDefault();
      selectGrouping(grouping, prefs, body, buttons);
    });
    buttons.set(grouping, button);
    views.appendChild(button);
  }
  head.appendChild(views);
  paintPicker(prefs.grouping(), buttons);
  return head;
}

/** Show one grouping's pane, hide the other, and remember the choice. */
function selectGrouping(
  grouping: Grouping,
  prefs: SidebarPrefs,
  body: HTMLElement,
  buttons: ReadonlyMap<Grouping, HTMLButtonElement>,
): void {
  log.info("switching the rail's grouping", {
    operation: "sidebar.grouping",
    context: { grouping },
  });
  prefs.setGrouping(grouping);
  for (const pane of body.querySelectorAll<HTMLElement>("[data-grouping]")) {
    pane.hidden = pane.getAttribute("data-grouping") !== grouping;
  }
  paintPicker(grouping, buttons);
}

function paintPicker(
  grouping: Grouping,
  buttons: ReadonlyMap<Grouping, HTMLButtonElement>,
): void {
  for (const [name, button] of buttons) button.classList.toggle("active", name === grouping);
}
