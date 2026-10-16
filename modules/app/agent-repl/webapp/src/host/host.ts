/**
 * host — THE ONE WEB VIEW EMACS SHOWS, holding every workspace's page.
 *
 * Emacs keeps exactly one WKWebView on screen, at this page, and asks it to
 * hold one iframe per open workspace (`ensure`), to show the one the user
 * stands in (`show`), and to drop one whose workspace closed (`remove`). Why
 * one web view instead of one per workspace is protocol.ts's account: an
 * iframe of a page on screen is never suspended, so every workspace page is
 * live and current before the user ever switches to it.
 *
 * EMACS OWNS THE DESIRED STATE; THIS PAGE APPLIES IT. Every call states an end
 * state (this workspace's page is at this URL; this one is shown; this one's
 * input window paints this color), so calling again is harmless, and a page
 * that reloads is handed the state it lost the moment it says hello.
 *
 * EVERY CALL ANSWERS. Emacs reads each answer back and records it, which is
 * how what this module does reaches the logs: an answer names the call, the
 * workspace, the outcome, and any notes the host made since the last answer
 * (a message it could not read, say). Answers are JSON strings because that
 * is what a script evaluation can hand Emacs. (The webhost's sidebar has a
 * daemon connection and a logger of its own, host/sidebar.ts; this module
 * applies Emacs's calls and uses neither.)
 *
 * THE SIDEBAR IS DRAWN OVER THE PAGES, once (host/sidebar.ts). This module
 * tells every page whether it is docked and which of the webhost's failures
 * stand, and ends the sidebar above the gate band of the page on screen.
 */
import {
  parseWindowLevel,
  type AppliedLevel,
  type LevelWindow,
  type WindowLevel,
} from "../../../agent-shim/logging/ts/level-window.js";
import { selectedText } from "../selection.js";
import {
  type WebhostFailure,
  HOST_CHANNEL,
  onHostChannel,
  parsePageToHost,
  type DrawnWhat,
  type HostToPage,
  type PageToHost,
} from "./protocol.js";

/** What the host needs from its environment, injectable for tests. */
export interface HostEnv {
  readonly doc: Document;
  readonly win: Window;
  /** Where the workspace iframes go; the document's body when omitted. */
  readonly frames?: HTMLElement;
  /** The webhost's own live log threshold, which `setLogLevel` raises. */
  readonly level: LevelWindow;
  /** Epoch ms now; the default is the document's clock. */
  readonly now?: () => number;
  /** An element's laid-out box; `getBoundingClientRect` when omitted. */
  readonly measure?: (element: Element) => { width: number; height: number };
}

/**
 * The root custom property the webhost's sidebar ends above: the gate band of
 * the page on screen, in pixels (host/protocol.ts `gateBand`).
 */
export const SIDEBAR_BOTTOM_PROPERTY = "--webhost-sidebar-bottom";

/** The slot the docked gate takes, as Emacs last told it. */
interface GateDockState {
  readonly pixels: number;
  readonly background: string | null;
}

/** One workspace's page, and everything the host knows and holds for it. */
interface Frame {
  readonly id: string;
  url: string;
  readonly iframe: HTMLIFrameElement;
  /** Desired state Emacs gave, replayed to the page on every hello. */
  inputBackground: string | null;
  gateDock: GateDockState | null;
  /** Facts the page pushed, which Emacs reads back. Reset when it reloads. */
  drawn: DrawnWhat | null;
  selection: { text: string | null; focused: boolean };
  editing: boolean;
  /** How tall the page's docked gate band is, as it last reported. */
  gateBand: number;
}

/** The host's API, as Emacs calls it through `window.agentReplHost`. */
export interface Host {
  /** Hold workspace ID's page at URL: create it, re-point it, or leave it. */
  ensure(id: string, url: string): string;
  /** Show workspace ID's page and hide every other. */
  show(id: string): string;
  /** Drop workspace ID's page. */
  remove(id: string): string;
  /** Load workspace ID's page again from its URL. */
  reload(id: string): string;
  /** Tell ID's page the background its Emacs input window paints. */
  setInputBackground(id: string, background: string): string;
  /** Tell ID's page the slot its docked gate takes. */
  setGateDock(id: string, pixels: number, background: string | null): string;
  /** Ask ID's page to close its standing news digest. */
  dismissDigest(id: string): string;
  /** Whether ID's page has drawn its conversation. */
  drawn(id: string): string;
  /** The shown page's selection and focus. */
  selection(): string;
  /** Whether a text field of the shown page holds the keyboard. */
  editing(): boolean;
  /** Every workspace id the host holds a page for. */
  ids(): string;
  /** Tell every page whether the webhost's sidebar is docked over it. */
  setSidebarDocked(docked: boolean): void;
  /** Tell every page the webhost's own failures standing now. */
  setFailures(failures: readonly WebhostFailure[]): void;
  /**
   * Record at LEVEL, the webhost and every page, until UNTILMS (epoch ms): a
   * log level window, replayed to a page that loads before it ends. Info, with
   * no end, closes it. A profiling session calls it from Emacs.
   */
  setLogLevel(level: string, untilMs: number | null): string;
  /** Stop listening and drop every page. */
  dispose(): void;
}

/** The class every workspace iframe wears; the stylesheet stacks them. */
const FRAME_CLASS = "workspace-frame";
/** The attribute naming the workspace an iframe holds. */
export const FRAME_WORKSPACE_ATTRIBUTE = "data-workspace-id";
/** The attribute the shown iframe wears. */
export const FRAME_SHOWN_ATTRIBUTE = "data-shown";

/**
 * One note the host made for Emacs to record, at the level it deserves: INFO
 * for what a working webhost does (a page cooling, a page from another build
 * mid-rollout), WARN for what it should never see.
 */
export interface HostNote {
  readonly level: "info" | "warn";
  readonly text: string;
}

/** The longest a run of notes grows before the oldest are dropped. */
const NOTE_LIMIT = 50;

/** Build the host over ENV and start listening to its pages. */
export function createHost(env: HostEnv): Host {
  const { doc, win } = env;
  const frames = new Map<string, Frame>();
  const frameParent = env.frames ?? doc.body;
  let shownId: string | null = null;
  let sidebarDocked = false;
  let failures: readonly WebhostFailure[] = [];
  let notes: HostNote[] = [];
  const now = env.now ?? Date.now;
  /** The log level window every page is told, while one stands. */
  let logLevel: { level: WindowLevel; untilMs: number } | null = null;

  const note = (level: HostNote["level"], text: string): void => {
    notes.push({ level, text });
    if (notes.length > NOTE_LIMIT) notes = notes.slice(notes.length - NOTE_LIMIT);
  };

  const answer = (op: string, id: string | null, outcome: string, extra: Record<string, unknown> = {}): string => {
    const taken = notes;
    notes = [];
    return JSON.stringify({ op, id, outcome, ...extra, notes: taken });
  };

  const post = (frame: Frame, message: HostToPage): void => {
    const target = frame.iframe.contentWindow;
    // A page not yet attached has no window; it says hello once it boots and
    // is handed everything then.
    if (target === null) return;
    target.postMessage(message, "*");
  };

  const presenceOf = (frame: Frame): HostToPage => ({
    channel: HOST_CHANNEL,
    kind: "presence",
    shown: frame.id === shownId,
  });

  /** Whether the sidebar is docked over the pages, as every page is told. */
  const dockMessage = (): HostToPage => ({ channel: HOST_CHANNEL, kind: "sidebarDock", docked: sidebarDocked });
  /** The webhost's failures, as every page is told. */
  const failuresMessage = (): HostToPage => ({ channel: HOST_CHANNEL, kind: "webhostFailures", failures });
  /** Tell every page MESSAGE. */
  const broadcast = (message: HostToPage): void => {
    for (const frame of frames.values()) post(frame, message);
  };

  /** End the sidebar above the gate band of the page on screen. */
  const placeSidebar = (): void => {
    const shown = shownId === null ? undefined : frames.get(shownId);
    doc.documentElement.style.setProperty(SIDEBAR_BOTTOM_PROPERTY, `${shown?.gateBand ?? 0}px`);
  };

  const sendState = (frame: Frame): void => {
    post(frame, presenceOf(frame));
    post(frame, dockMessage());
    post(frame, failuresMessage());
    if (frame.inputBackground !== null) {
      post(frame, { channel: HOST_CHANNEL, kind: "inputBackground", background: frame.inputBackground });
    }
    if (frame.gateDock !== null) {
      post(frame, {
        channel: HOST_CHANNEL,
        kind: "gateDock",
        pixels: frame.gateDock.pixels,
        background: frame.gateDock.background,
      });
    }
    if (logLevel !== null && logLevel.untilMs > now()) {
      post(frame, { channel: HOST_CHANNEL, kind: "logLevel", level: logLevel.level, untilMs: logLevel.untilMs });
    }
  };

  const resetFacts = (frame: Frame): void => {
    frame.drawn = null;
    frame.selection = { text: "", focused: false };
    frame.editing = false;
  };

  const frameOf = (source: MessageEventSource | null): Frame | null => {
    if (source === null) return null;
    for (const frame of frames.values()) {
      if (frame.iframe.contentWindow === source) return frame;
    }
    return null;
  };

  const take = (frame: Frame, message: PageToHost): void => {
    switch (message.kind) {
      case "hello":
        resetFacts(frame);
        sendState(frame);
        return;
      case "drawn":
        frame.drawn = message.what;
        return;
      case "selection":
        frame.selection = { text: message.text, focused: message.focused };
        return;
      case "editing":
        frame.editing = message.editing;
        return;
      case "gateBand":
        frame.gateBand = message.pixels;
        if (frame.id === shownId) placeSidebar();
        return;
    }
  };

  const onMessage = (event: MessageEvent): void => {
    if (!onHostChannel(event.data)) return;
    const frame = frameOf(event.source);
    if (frame === null) {
      note("warn", `a message on the host channel came from no workspace page: ${JSON.stringify(event.data)}`);
      return;
    }
    const message = parsePageToHost(event.data);
    if (message === null) {
      // A PAGE FROM ANOTHER BUILD, which a rollout makes for the moment between
      // re-pointing the pages and reloading the webhost: expected, and over by
      // itself once both run the same build.
      note("info", `workspace ${frame.id} sent a host message this build cannot read: ${JSON.stringify(event.data)}`);
      return;
    }
    take(frame, message);
  };
  win.addEventListener("message", onMessage);

  const measure = env.measure ?? ((element: Element) => element.getBoundingClientRect());

  /**
   * Show FRAME, or hide it.
   *
   * A HIDDEN PAGE KEEPS THE SIZE IT WAS HIDDEN AT. Every frame fills the web
   * view, and a hidden one is only invisible, so a web view resize (Emacs
   * giving the view a gated workspace's input space) re-laid out every
   * workspace's page: fifteen pages for the one on screen, and a switch into
   * or out of a gated workspace took twice as long (2026-10-16). A frame is
   * fixed at its laid-out size when hidden, and fills the view again when
   * shown, so a resize reaches the page on screen alone, and a page shown at
   * another size takes it as it is shown.
   */
  const applyShown = (frame: Frame, shown: boolean): void => {
    const style = frame.iframe.style;
    if (shown) {
      frame.iframe.setAttribute(FRAME_SHOWN_ATTRIBUTE, "");
      style.removeProperty("width");
      style.removeProperty("height");
      return;
    }
    const box = measure(frame.iframe);
    style.width = `${box.width}px`;
    style.height = `${box.height}px`;
    frame.iframe.removeAttribute(FRAME_SHOWN_ATTRIBUTE);
  };

  const create = (id: string, url: string): Frame => {
    const iframe = doc.createElement("iframe");
    iframe.className = FRAME_CLASS;
    iframe.setAttribute(FRAME_WORKSPACE_ATTRIBUTE, id);
    iframe.title = id;
    // The page copies to the clipboard (copy.ts), which a cross-origin frame
    // may do only when its embedder allows it.
    iframe.allow = "clipboard-read; clipboard-write";
    iframe.src = url;
    const frame: Frame = {
      id,
      url,
      iframe,
      inputBackground: null,
      gateDock: null,
      drawn: null,
      selection: { text: "", focused: false },
      editing: false,
      gateBand: 0,
    };
    frames.set(id, frame);
    frameParent.append(iframe);
    // Laid out first, so a frame created hidden is fixed at the size it fills.
    applyShown(frame, id === shownId);
    return frame;
  };

  const withFrame = (op: string, id: string, act: (frame: Frame) => string): string => {
    const frame = frames.get(id);
    return frame === undefined ? answer(op, id, "absent") : answer(op, id, act(frame));
  };

  return {
    ensure(id, url) {
      const frame = frames.get(id);
      if (frame === undefined) {
        create(id, url);
        return answer("ensure", id, "created");
      }
      if (frame.url === url) return answer("ensure", id, "unchanged");
      frame.url = url;
      resetFacts(frame);
      frame.iframe.src = url;
      return answer("ensure", id, "navigated");
    },
    show(id) {
      const next = frames.get(id);
      if (next === undefined) return answer("show", id, "absent");
      if (shownId === id) return answer("show", id, "unchanged");
      const previous = shownId === null ? undefined : frames.get(shownId);
      shownId = id;
      if (previous !== undefined) {
        applyShown(previous, false);
        post(previous, presenceOf(previous));
      }
      applyShown(next, true);
      post(next, presenceOf(next));
      placeSidebar();
      // The keyboard follows the page on screen: Emacs focused this web view,
      // so the focus within it moves to the page now shown.
      if (doc.hasFocus()) next.iframe.focus();
      return answer("show", id, "shown", { previous: previous?.id ?? null });
    },
    remove(id) {
      return withFrame("remove", id, (frame) => {
        frames.delete(id);
        frame.iframe.remove();
        if (shownId === id) {
          shownId = null;
          placeSidebar();
        }
        return "removed";
      });
    },
    reload(id) {
      return withFrame("reload", id, (frame) => {
        resetFacts(frame);
        frame.iframe.src = frame.url;
        return "reloaded";
      });
    },
    setInputBackground(id, background) {
      return withFrame("setInputBackground", id, (frame) => {
        frame.inputBackground = background;
        post(frame, { channel: HOST_CHANNEL, kind: "inputBackground", background });
        return "sent";
      });
    },
    setGateDock(id, pixels, background) {
      return withFrame("setGateDock", id, (frame) => {
        frame.gateDock = { pixels, background };
        post(frame, { channel: HOST_CHANNEL, kind: "gateDock", pixels, background });
        return "sent";
      });
    },
    dismissDigest(id) {
      return withFrame("dismissDigest", id, (frame) => {
        post(frame, { channel: HOST_CHANNEL, kind: "dismissDigest" });
        return "sent";
      });
    },
    drawn(id) {
      const frame = frames.get(id);
      return answer("drawn", id, frame === undefined ? "absent" : "present", { drawn: frame?.drawn ?? null });
    },
    selection() {
      // TEXT SELECTED IN THE WEBHOST ITSELF (its sidebar) is the selection:
      // the pages cannot see it, and while it stands the user is not selecting
      // in a page.
      const own = selectedText(win);
      if (own !== "") return answer("selection", null, "webhost", { text: own, focused: doc.hasFocus() });
      const frame = shownId === null ? undefined : frames.get(shownId);
      if (frame === undefined) return answer("selection", null, "none-shown", { text: null, focused: false });
      return answer("selection", frame.id, "present", {
        text: frame.selection.text,
        focused: frame.selection.focused,
      });
    },
    editing() {
      const frame = shownId === null ? undefined : frames.get(shownId);
      return frame?.editing ?? false;
    },
    setSidebarDocked(docked) {
      sidebarDocked = docked;
      broadcast(dockMessage());
    },
    setFailures(next) {
      failures = next;
      broadcast(failuresMessage());
    },
    setLogLevel(name, untilMs) {
      let level: WindowLevel;
      try {
        level = parseWindowLevel(name, "the webhost's log level");
      } catch (err) {
        note("warn", (err as Error).message);
        return answer("setLogLevel", null, "refused");
      }
      let applied: AppliedLevel;
      try {
        applied = env.level.apply(level, untilMs);
      } catch (err) {
        note("warn", (err as Error).message);
        return answer("setLogLevel", null, "refused");
      }
      switch (applied.outcome) {
        case "closed":
          logLevel = null;
          broadcast({ channel: HOST_CHANNEL, kind: "logLevel", level: "info", untilMs: null });
          return answer("setLogLevel", null, "closed");
        case "expired":
          note("warn", `a ${level} log level window that ended at ${applied.untilMs} has nothing left to open`);
          return answer("setLogLevel", null, "refused");
        case "opened":
          logLevel = { level: applied.level, untilMs: applied.untilMs };
          broadcast({ channel: HOST_CHANNEL, kind: "logLevel", level: applied.level, untilMs: applied.untilMs });
          return answer("setLogLevel", null, "opened", { level, until_ms: applied.untilMs, pages: frames.size });
      }
    },
    ids() {
      return answer("ids", null, "listed", { ids: [...frames.keys()], shown: shownId });
    },
    dispose() {
      win.removeEventListener("message", onMessage);
      for (const frame of frames.values()) frame.iframe.remove();
      frames.clear();
      shownId = null;
    },
  };
}

/** Whether a text field holds the focus, asking the shown page when an iframe does.
 * The answer behind `xwHasFocus` (main.ts). */
export function hostHasFocus(doc: Document, host: Host): boolean {
  const active = doc.activeElement;
  if (active === null) return false;
  if (active.nodeName === "IFRAME") return host.editing();
  return active.nodeName === "INPUT" || active.nodeName === "TEXTAREA";
}
