/**
 * The sidebar page's start: the workspaces rail, in a web view of its own.
 *
 * Emacs shows the sidebar in a window of its own at the frame's left edge,
 * running the frame's whole height beside the feed and the composer
 * (lisp/sidebar-view.el). On the NS port one web view shows in one window
 * only, so the sidebar cannot be part of the host (host.ts), which shows in
 * the feed's window: it is the webapp's own bundle loaded at `?sidebar=1`
 * (main.ts), in a second web view, and the rail fills it.
 *
 * Emacs reaches the page through `window.agentReplSidebar`, as it reaches the
 * host through `window.agentReplHost`, and in the same shape: every call
 * states an end state and answers a JSON string naming the call, its outcome
 * and any notes the page made since the last answer. A call made before the
 * page has started is queued on `window.__agentReplSidebarPending` by the
 * call's own script and run here, in order, the moment the page starts.
 *
 * The page talks to the daemon over its own connection (document.ts). Its
 * warnings and errors are listed behind the rail's version (sidebar/
 * notices.ts), since the pages' warning chip is in another web view.
 */
import { LevelWindow, parseWindowLevel, type AppliedLevel, type WindowLevel } from "../../../agent-shim/logging/ts/level-window.js";
import { bootFailed } from "../failure/sink.js";
import { FOCUS_LEFT_EMACS, pageFocusLeft, type FocusLeftSource } from "../focus-left.js";
import { log } from "../log.js";
import { createDaemonOrigin, isDaemonOrigin, type DaemonOrigin } from "../rpc/daemon-origin.js";
import { mountWebhostSidebar } from "../sidebar/sidebar.js";
import { createSidebarNotices, type SidebarNotices } from "../sidebar/notices.js";
import { installColorValues } from "../vocab.js";
import { startWebhostDocument, type WebhostDocumentDeps } from "./document.js";
import "./sidebar-page.css";

/** The id the rail's element wears: the page's own, so one stylesheet draws both. */
export const SIDEBAR_ID = "ws-sidebar";

/** The class the sidebar page's root wears. */
export const SIDEBAR_ROOT_CLASS = "agent-repl-sidebar";

/** How many notes an answer may carry; older ones are dropped first. */
const NOTE_LIMIT = 20;

/** What Emacs calls on the sidebar page. Every call answers a JSON string. */
export interface SidebarPage {
  /** Highlight workspace ID's row: the page the host shows now. */
  markShown(id: string): string;
  /**
   * Point the page's daemon calls at ORIGIN, the daemon its workspaces moved
   * to: a handover does not reload the sidebar. Answers `retargeted`,
   * `unchanged`, or `refused` for anything that is not an http origin.
   */
  setDaemon(origin: string): string;
  /**
   * Record at LEVEL until UNTILMS (epoch ms): a log level window. Info, with
   * no end, closes it. A profiling session calls it from Emacs.
   */
  setLogLevel(level: string, untilMs: number | null): string;
  /**
   * Emacs moved its focus (the selected window changed, or the frame lost the
   * focus): the rail lets go of what the pointer raised. A key the web view
   * forwards to Emacs selects another window without the page ever losing
   * the focus, so the page cannot see this for itself.
   */
  focusMoved(): string;
}

/** A call Emacs made before the page started, queued on the page. */
type PendingCall = (page: SidebarPage) => unknown;

declare global {
  interface Window {
    agentReplSidebar?: SidebarPage;
    __agentReplSidebarPending?: PendingCall[];
  }
}

/** What the start may have injected, for a suite that drives it. */
export interface StartSidebarPageDeps extends WebhostDocumentDeps {
  /** Mount the rail; the real one opens the roster stream. */
  mount?: typeof mountWebhostSidebar;
  /** The page's notices; a fresh set when absent. */
  notices?: SidebarNotices;
  /** The page's focus-left signal, which `focusMoved` fires; the document's own when absent. */
  focusLeft?: FocusLeftSource;
}

/** Turn the page in DOC and WIN into the sidebar, and hand it to Emacs. */
export function startSidebarPage(doc: Document, win: Window, deps: StartSidebarPageDeps = {}): SidebarPage {
  doc.body.replaceChildren();
  doc.documentElement.classList.add(SIDEBAR_ROOT_CLASS);
  // THE STATUS COLORS' VALUES come from the shared vocabulary, never from
  // styles.css: every status dot is in the rail.
  installColorValues(doc);
  const nav = doc.createElement("nav");
  nav.id = SIDEBAR_ID;
  nav.setAttribute("data-component", "sidebar");
  nav.hidden = true;
  doc.body.append(nav);

  const level = deps.level ?? LevelWindow.fixed("info");
  // THE PAGE'S DAEMON starts as the one that served it, and moves with its
  // workspaces at a handover (`setDaemon`).
  const daemon: DaemonOrigin = deps.daemon ?? createDaemonOrigin(win.location.origin);
  const notices = deps.notices ?? createSidebarNotices();
  const focusLeft = deps.focusLeft ?? pageFocusLeft();
  const started = startWebhostDocument("sidebar", doc, win, {
    ...deps,
    level,
    daemon,
    onRecord: (recordLevel, line) => {
      notices.record(recordLevel, line);
      deps.onRecord?.(recordLevel, line);
    },
  });

  let markShown = (_id: string): void => undefined;
  try {
    log.info("the sidebar page is starting", { operation: "sidebar-page.start", context: {} });
    const rail = (deps.mount ?? mountWebhostSidebar)(nav, started.ctx, {
      focusLeft,
      notices: { failures: started.failures, notices },
      revealed: () => {
        log.info("the sidebar page drew its rail", { operation: "sidebar-page.revealed", context: {} });
      },
    });
    markShown = (id) => rail.markShown(id);
  } catch (err) {
    const cause = err instanceof Error ? `${err.name}: ${err.message}` : String(err);
    log.error(`the sidebar failed to start: ${cause}`, {
      operation: "sidebar-page.start-failed",
      context: { cause },
    });
    started.failures.report(bootFailed(cause));
  }

  let notes: { level: "info" | "warn"; text: string }[] = [];
  const note = (noteLevel: "info" | "warn", text: string): void => {
    notes.push({ level: noteLevel, text });
    if (notes.length > NOTE_LIMIT) notes = notes.slice(notes.length - NOTE_LIMIT);
  };
  const answer = (op: string, id: string | null, outcome: string, extra: Record<string, unknown> = {}): string => {
    const taken = notes;
    notes = [];
    return JSON.stringify({ op, id, outcome, ...extra, notes: taken });
  };

  const page: SidebarPage = {
    markShown(id) {
      if (typeof id !== "string" || id === "") {
        note("warn", `markShown was handed ${JSON.stringify(id)}, which is no workspace id`);
        return answer("markShown", null, "refused");
      }
      markShown(id);
      return answer("markShown", id, "marked");
    },
    setDaemon(origin) {
      if (!isDaemonOrigin(origin)) {
        note("warn", `setDaemon was handed ${JSON.stringify(origin)}, which is no daemon origin; the sidebar's calls stay where they were`);
        return answer("setDaemon", null, "refused");
      }
      return answer("setDaemon", null, daemon.set(origin) ? "retargeted" : "unchanged", { origin });
    },
    setLogLevel(name, untilMs) {
      let parsed: WindowLevel;
      try {
        parsed = parseWindowLevel(name, "the sidebar's log level");
      } catch (err) {
        note("warn", (err as Error).message);
        return answer("setLogLevel", null, "refused");
      }
      let applied: AppliedLevel;
      try {
        applied = applyLevel(level, parsed, untilMs);
      } catch (err) {
        note("warn", (err as Error).message);
        return answer("setLogLevel", null, "refused");
      }
      switch (applied.outcome) {
        case "closed":
          return answer("setLogLevel", null, "closed");
        case "expired":
          note("warn", `a ${parsed} log level window that ended at ${applied.untilMs} has nothing left to open`);
          return answer("setLogLevel", null, "refused");
        case "opened":
          return answer("setLogLevel", null, "opened", { level: parsed, until_ms: applied.untilMs });
      }
    },
    focusMoved() {
      focusLeft.left(FOCUS_LEFT_EMACS);
      return answer("focusMoved", null, "sent");
    },
  };
  win.agentReplSidebar = page;
  const pending = win.__agentReplSidebarPending ?? [];
  delete win.__agentReplSidebarPending;
  for (const call of pending) call(page);
  return page;
}

/** Apply LEVEL until UNTILMS to the page's level window, as every source applies one. */
function applyLevel(window: LevelWindow, level: WindowLevel, untilMs: number | null): AppliedLevel {
  return window.apply(level, untilMs);
}
