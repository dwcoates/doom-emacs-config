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
import { endPage } from "../page-teardown.js";
import {
  parseWindowLevel,
  type AppliedLevel,
  type LevelWindow,
  type WindowLevel,
} from "../../../agent-shim/logging/ts/level-window.js";
import { selectedText } from "../selection.js";
import {
  type HeldIngressWireEntry,
  type WebhostFailure,
  HOST_CHANNEL,
  onHostChannel,
  parseHeldIngressEntry,
  parsePageToHost,
  type DrawnWhat,
  type HostToPage,
  type PageToHost,
} from "./protocol.js";
import { isDaemonOrigin, type DaemonOrigin } from "../rpc/daemon-origin.js";
import { createListeners } from "../listeners.js";

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
  /** The daemon the webhost's own calls go to (`setDaemon` moves it). */
  readonly daemon: DaemonOrigin;
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
  /** The editor's held-prompt ingress entries for the workspace; null until stated. */
  heldIngress: readonly HeldIngressWireEntry[] | null;
  /** Facts the page pushed, which Emacs reads back. Reset when it reloads. */
  drawn: DrawnWhat | null;
  selection: { text: string | null; focused: boolean };
  editing: boolean;
  /** How tall the page's docked gate band is, as it last reported. */
  gateBand: number;
  /**
   * Whether the page now in the frame has said hello, so a message posted to
   * it is heard. False from a load or navigation until its hello.
   */
  attached: boolean;
  /**
   * The start of the user's switch to this page (switchStartedAtMs) while
   * the page that must measure it has not said hello yet: handed over with
   * the presence its hello replays, then forgotten.
   */
  pendingSwitchStartedAtMs: number | null;
  /**
   * When the reload that loads the page now in the frame began
   * (reloadStartedAtMs), while that page has not said hello yet: handed over
   * with the presence its hello replays, then forgotten. Null for a page no
   * reload loaded.
   */
  pendingReloadStartedAtMs: number | null;
}

/** The host's API, as Emacs calls it through `window.agentReplHost`. */
export interface Host {
  /**
   * Hold workspace ID's page at URL: create it, re-point it, or leave it.
   * Given a LANE, a page created now loads only once every page loading
   * before it in that lane has drawn (see `createHost`'s pacing), or at once
   * if it is shown first. Without one it loads at once. A page re-pointed
   * at a new URL loads at once in a fresh frame, whatever LANE says.
   * RELOAD_STARTED_AT_MS is when the reload that asks for the page began
   * (the presence message's reloadStartedAtMs), null when no reload asks: it
   * is handed to a page this call loads, and to nothing else.
   */
  ensure(id: string, url: string, lane?: string | null, reloadStartedAtMs?: number | null): string;
  /**
   * Show workspace ID's page and hide every other. SWITCH_STARTED_AT_MS is
   * when the user's switch that asked for it began (the presence message's
   * switchStartedAtMs), null when the show is not a switch the user made.
   */
  show(id: string, switchStartedAtMs?: number | null): string;
  /** Tell LISTENER the id of every page shown from now on; answers the stop. */
  onShown(listener: (id: string) => void): () => void;
  /** Drop workspace ID's page. */
  remove(id: string): string;
  /**
   * Point the webhost's own daemon calls (its sidebar's) at ORIGIN, the
   * daemon its workspaces moved to: a handover no longer reloads the
   * webhost. Answers `retargeted`, `unchanged`, or `refused` for anything
   * that is not an http origin.
   */
  setDaemon(origin: string): string;
  /**
   * Load workspace ID's page again from its URL. RELOAD_STARTED_AT_MS is when
   * the reload began, handed to the page it loads.
   */
  reload(id: string, reloadStartedAtMs?: number | null): string;
  /** Tell ID's page the background its Emacs input window paints. */
  setInputBackground(id: string, background: string): string;
  /** Tell ID's page the slot its docked gate takes. */
  setGateDock(id: string, pixels: number, background: string | null): string;
  /**
   * Tell ID's page the editor's held-prompt ingress entries for its
   * workspace, the whole list. An entry that is not one (host/protocol.ts
   * `parseHeldIngressEntry`) refuses the call whole, answered `refused`.
   */
  setHeldIngress(id: string, entries: unknown): string;
  /** Ask ID's page to close its standing news digest. */
  dismissDigest(id: string): string;
  /**
   * Tell every page Emacs moved its focus (the selected window changed, or
   * the frame lost the focus), so each lets go of what the pointer raised.
   */
  focusMoved(): string;
  /** Whether ID's page has drawn its conversation. */
  drawn(id: string): string;
  /**
   * End every page and then the webhost's own, for the reload Emacs is about
   * to make: each page's frame is removed, which ends its document now (its
   * `pagehide` aborts its requests), so no stream of a page the reload tears
   * down is read as a cut link. Answers `retired` with how many pages ended.
   */
  retireAll(): string;
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

/**
 * How long a page loading in turn may take to say it has drawn before the
 * next one in its lane starts anyway: a page that never says so is recorded
 * at warn, and the pages after it are not held behind it.
 */
export const IN_TURN_BOUND_MS = 10_000;

/** One lane of pages loaded in turn: its queue, oldest first, and the page in flight. */
interface Lane {
  readonly name: string;
  readonly waiting: string[];
  loading: string | null;
  /** The bound on `loading` (`IN_TURN_BOUND_MS`). */
  bound: number | null;
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
  /**
   * PAGES LOADED IN TURN load one after another in their LANE, each once the
   * one before it has drawn (owner rulings, 2026-10-17). Every page booting at
   * once after a host load stalled the web process for up to 1.8s; a fixed gap
   * between them is too slow on an idle machine and lets them pile up on a hot
   * one. Pacing on the page's own `drawn` is as fast as the machine allows.
   * The lanes run side by side, so as many pages load at once as there are
   * lanes: Emacs hands the pages after the current workspace to one and those
   * before it to another, each nearest first.
   */
  const lanes = new Map<string, Lane>();
  /**
   * THE SHOWN PAGE STAYS ON SCREEN UNTIL ITS REPLACEMENT HAS DRAWN. A page
   * re-pointed while it is shown (a handover moving its workspace to the
   * successor) loads in a hidden frame of its own, its replacement, and the
   * two change places when the replacement says it has drawn, so the person
   * looking at the page never sees it blank. Keyed by workspace id; the
   * bound is `IN_TURN_BOUND_MS`, after which the replacement is shown drawn
   * or not, so a replacement that never draws cannot hold a dead page on
   * screen.
   */
  const incoming = new Map<string, { fresh: Frame; bound: number }>();
  /** Who is told every page shown (`onShown`): the webhost's sidebar. */
  const shownListeners = createListeners<[string]>();
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

  const presenceOf = (
    frame: Frame,
    switchStartedAtMs: number | null = null,
    reloadStartedAtMs: number | null = null,
  ): HostToPage => ({
    channel: HOST_CHANNEL,
    kind: "presence",
    shown: frame.id === shownId,
    ...(switchStartedAtMs === null ? {} : { switchStartedAtMs }),
    ...(reloadStartedAtMs === null ? {} : { reloadStartedAtMs }),
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
    // A SWITCH THE PAGE MISSED WHILE IT LOADED is handed over once, here.
    const switchStartedAtMs = frame.id === shownId ? frame.pendingSwitchStartedAtMs : null;
    frame.pendingSwitchStartedAtMs = null;
    // SO IS THE RELOAD THAT LOADED IT, shown or not: the page decides, once it
    // has drawn, whether anyone saw it blank.
    const reloadStartedAtMs = frame.pendingReloadStartedAtMs;
    frame.pendingReloadStartedAtMs = null;
    post(frame, presenceOf(frame, switchStartedAtMs, reloadStartedAtMs));
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
    if (frame.heldIngress !== null) {
      post(frame, { channel: HOST_CHANNEL, kind: "heldIngress", entries: frame.heldIngress });
    }
  };

  /** Start loading FRAME at its URL. */
  const load = (frame: Frame): void => {
    frame.iframe.src = frame.url;
  };

  /** The lane named NAME, made empty the first time it is named. */
  const laneNamed = (name: string): Lane => {
    const known = lanes.get(name);
    if (known !== undefined) return known;
    const lane: Lane = { name, waiting: [], loading: null, bound: null };
    lanes.set(name, lane);
    return lane;
  };

  /** Start LANE's next waiting page, when none of LANE's is loading. */
  const advance = (lane: Lane): void => {
    if (lane.loading !== null) return;
    const next = lane.waiting.shift();
    if (next === undefined) return;
    const frame = frames.get(next);
    if (frame === undefined) {
      advance(lane);
      return;
    }
    lane.loading = next;
    load(frame);
    lane.bound = win.setTimeout(() => {
      note(
        "warn",
        `page ${next} loading in turn in lane ${lane.name} did not say it had drawn within ${IN_TURN_BOUND_MS}ms; the next page starts`,
      );
      finishTurn(next);
    }, IN_TURN_BOUND_MS);
  };

  /** ID's turn is over (it drew, or it is gone): its lane starts the next. */
  const finishTurn = (id: string): void => {
    for (const lane of lanes.values()) {
      if (lane.loading !== id) continue;
      if (lane.bound !== null) win.clearTimeout(lane.bound);
      lane.bound = null;
      lane.loading = null;
      advance(lane);
    }
  };

  /** Take ID out of the lane it waits in; whether it was waiting. */
  const unqueue = (id: string): boolean => {
    for (const lane of lanes.values()) {
      const at = lane.waiting.indexOf(id);
      if (at >= 0) {
        lane.waiting.splice(at, 1);
        return true;
      }
    }
    return false;
  };

  /**
   * End FRAME's document and its place in the lanes: the frame leaves the
   * page, a lane it waited in forgets it, and a lane it was loading in starts
   * its next page. Shared by a removed page and a re-pointed one, which both
   * end the page now in the frame.
   */
  /**
   * THE WINDOWS OF PAGES THIS HOST REMOVED. A page's message can be on its way
   * when its iframe goes -- a replacement swapped in, a reload retiring every
   * page -- and it then arrives from a window no frame names any more. Such a
   * message is the page's last word, read as that at info; a message from a
   * window that never was a page here is still the warning it was.
   */
  const removedWindows = new WeakSet<MessageEventSource>();

  /** Take FRAME's iframe out of the document, remembering its window. */
  const removeFrame = (frame: Frame): void => {
    const removed = frame.iframe.contentWindow;
    if (removed !== null) removedWindows.add(removed);
    frame.iframe.remove();
  };

  const retire = (frame: Frame): void => {
    removeFrame(frame);
    unqueue(frame.id);
    finishTurn(frame.id);
  };

  /** Load ID now if it is still waiting its turn (it is being shown). */
  const loadNow = (frame: Frame): boolean => {
    if (!unqueue(frame.id)) return false;
    load(frame);
    return true;
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
    for (const { fresh } of incoming.values()) {
      if (fresh.iframe.contentWindow === source) return fresh;
    }
    return null;
  };

  /** Drop ID's replacement, unshown: a later re-point or a removal supersedes it. */
  const dropIncoming = (id: string): void => {
    const waiting = incoming.get(id);
    if (waiting === undefined) return;
    win.clearTimeout(waiting.bound);
    incoming.delete(id);
    removeFrame(waiting.fresh);
  };

  /** ID's replacement takes the old page's place, on screen if it is shown. */
  const swapIn = (id: string): void => {
    const waiting = incoming.get(id);
    if (waiting === undefined) return;
    win.clearTimeout(waiting.bound);
    incoming.delete(id);
    const old = frames.get(id);
    frames.set(id, waiting.fresh);
    if (old !== undefined) retire(old);
    applyShown(waiting.fresh, id === shownId);
    post(waiting.fresh, presenceOf(waiting.fresh));
    if (id === shownId) placeSidebar();
  };

  const take = (frame: Frame, message: PageToHost): void => {
    switch (message.kind) {
      case "hello":
        resetFacts(frame);
        frame.attached = true;
        sendState(frame);
        return;
      case "drawn":
        frame.drawn = message.what;
        finishTurn(frame.id);
        if (incoming.get(frame.id)?.fresh === frame) swapIn(frame.id);
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
      if (event.source !== null && removedWindows.has(event.source)) {
        note("info", `a page this host already removed sent its last host message; it was dropped: ${JSON.stringify(event.data)}`);
        return;
      }
      // THE SOURCE AND ORIGIN SAY WHICH STRANGER IT WAS: a null source is a
      // window WebKit had already detached when the message was dispatched,
      // and the origin names the daemon whose page sent it (2026-10-25: 351
      // of these across a handover whose adopts were retried every 10s, with
      // nothing to tell them apart).
      const source = event.source === null ? "null" : "an unknown window";
      note(
        "warn",
        `a message on the host channel came from no workspace page: source=${source} origin=${JSON.stringify(event.origin)} data=${JSON.stringify(event.data)}`,
      );
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

  /**
   * Give TO everything Emacs told FROM that a page is replayed on hello, and
   * the gate band FROM last reported: a page re-pointed into a fresh frame
   * keeps what the page it replaces held.
   */
  const carryDesiredState = (from: Frame, to: Frame): void => {
    to.inputBackground = from.inputBackground;
    to.gateDock = from.gateDock;
    to.heldIngress = from.heldIngress;
    to.gateBand = from.gateBand;
  };

  /** A frame for ID at URL, in the page and laid out SHOWN or hidden, not yet loading. */
  const newFrame = (id: string, url: string, shown: boolean): Frame => {
    const iframe = doc.createElement("iframe");
    iframe.className = FRAME_CLASS;
    iframe.setAttribute(FRAME_WORKSPACE_ATTRIBUTE, id);
    iframe.title = id;
    // The page copies to the clipboard (copy.ts), which a cross-origin frame
    // may do only when its embedder allows it.
    iframe.allow = "clipboard-read; clipboard-write";
    const frame: Frame = {
      id,
      url,
      iframe,
      inputBackground: null,
      gateDock: null,
      heldIngress: null,
      drawn: null,
      selection: { text: "", focused: false },
      editing: false,
      gateBand: 0,
      attached: false,
      pendingSwitchStartedAtMs: null,
      pendingReloadStartedAtMs: null,
    };
    frameParent.append(iframe);
    // Laid out first, so a frame created hidden is fixed at the size it fills.
    applyShown(frame, shown);
    return frame;
  };

  const create = (id: string, url: string, lane: string | undefined): Frame => {
    const frame = newFrame(id, url, id === shownId);
    frames.set(id, frame);
    if (lane !== undefined) {
      const queue = laneNamed(lane);
      queue.waiting.push(id);
      advance(queue);
    } else {
      load(frame);
    }
    return frame;
  };

  /**
   * Answer OP on ID's page as ACT does it. A replacement waiting to take the
   * page's place (`incoming`) is given the same call, so it comes on screen
   * holding everything the page it replaces was told.
   */
  const withFrame = (op: string, id: string, act: (frame: Frame) => string): string => {
    const frame = frames.get(id);
    if (frame === undefined) return answer(op, id, "absent");
    const outcome = act(frame);
    const waiting = incoming.get(id);
    if (waiting !== undefined) act(waiting.fresh);
    return answer(op, id, outcome);
  };

  return {
    ensure(id, url, lane, reloadStartedAtMs = null) {
      const frame = frames.get(id);
      const laneName = lane ?? undefined;
      if (frame === undefined) {
        create(id, url, laneName).pendingReloadStartedAtMs = reloadStartedAtMs;
        const queued = laneName !== undefined && laneNamed(laneName).waiting.includes(id);
        return answer("ensure", id, queued ? "queued" : "created");
      }
      if (frame.url === url && !incoming.has(id)) return answer("ensure", id, "unchanged");
      const pending = incoming.get(id);
      if (pending !== undefined && pending.fresh.url === url) return answer("ensure", id, "unchanged");
      dropIncoming(id);
      if (id === shownId) {
        // THE SHOWN PAGE STAYS UNTIL ITS REPLACEMENT HAS DRAWN (`incoming`).
        const fresh = newFrame(id, url, false);
        carryDesiredState(frame, fresh);
        fresh.pendingReloadStartedAtMs = reloadStartedAtMs;
        const bound = win.setTimeout(() => {
          note("warn", `page ${id}'s replacement did not say it had drawn within ${IN_TURN_BOUND_MS}ms; it is shown as it is`);
          swapIn(id);
        }, IN_TURN_BOUND_MS);
        incoming.set(id, { fresh, bound });
        load(fresh);
        return answer("ensure", id, "replacing");
      }
      // A RE-POINTED PAGE IS A NEW PAGE, IN A NEW FRAME, LOADED AT ONCE.
      //
      // Setting the old frame's `src` left its document running until the new
      // one committed, so a page re-pointed while it loaded still booted
      // against the address it was leaving: on the 2026-10-22 11:14 deploy a
      // page the replay had handed the departing daemon's address booted there
      // 140ms after Emacs re-pointed it, and adopted there. Removing the frame
      // ends that document now, so nothing of the old address can run.
      //
      // And a re-point is a handover's move, whose adoption waits on this
      // page calling the successor, so it never waits a lane's turn: the page
      // loads now, and the lane it held or waited in goes on without it, as
      // it does for a removed page.
      retire(frame);
      const fresh = create(id, url, undefined);
      carryDesiredState(frame, fresh);
      fresh.pendingSwitchStartedAtMs = frame.pendingSwitchStartedAtMs;
      fresh.pendingReloadStartedAtMs = reloadStartedAtMs;
      return answer("ensure", id, "navigated");
    },
    show(id, switchStartedAtMs = null) {
      const next = frames.get(id);
      if (next === undefined) return answer("show", id, "absent");
      if (shownId === id) return answer("show", id, "unchanged");
      const previous = shownId === null ? undefined : frames.get(shownId);
      shownId = id;
      // A PAGE STILL WAITING ITS TURN loads now: it is the one wanted.
      const jumped = loadNow(next);
      if (previous !== undefined) {
        applyShown(previous, false);
        previous.pendingSwitchStartedAtMs = null;
        post(previous, presenceOf(previous));
      }
      applyShown(next, true);
      // A LISTENING PAGE IS TOLD THE SWITCH NOW; one still loading is told
      // with the presence its hello replays.
      if (next.attached) {
        post(next, presenceOf(next, switchStartedAtMs));
      } else {
        next.pendingSwitchStartedAtMs = switchStartedAtMs;
        post(next, presenceOf(next));
      }
      placeSidebar();
      // THE SIDEBAR'S HIGHLIGHT MOVES WITH THE PAGE, in the same turn.
      shownListeners.notify(id);
      // The keyboard follows the page on screen: Emacs focused this web view,
      // so the focus within it moves to the page now shown.
      if (doc.hasFocus()) next.iframe.focus();
      return answer("show", id, "shown", { previous: previous?.id ?? null, ...(jumped ? { jumped: true } : {}) });
    },
    onShown(listener) {
      return shownListeners.add(listener);
    },
    remove(id) {
      return withFrame("remove", id, (frame) => {
        dropIncoming(id);
        frames.delete(id);
        retire(frame);
        if (shownId === id) {
          shownId = null;
          placeSidebar();
        }
        return "removed";
      });
    },
    setDaemon(origin) {
      if (!isDaemonOrigin(origin)) {
        note("warn", `setDaemon was handed ${JSON.stringify(origin)}, which is no daemon origin; the webhost's calls stay where they were`);
        return answer("setDaemon", null, "refused");
      }
      return answer("setDaemon", null, env.daemon.set(origin) ? "retargeted" : "unchanged", { origin });
    },
    reload(id, reloadStartedAtMs = null) {
      return withFrame("reload", id, (frame) => {
        dropIncoming(id);
        resetFacts(frame);
        frame.attached = false;
        frame.pendingSwitchStartedAtMs = null;
        frame.pendingReloadStartedAtMs = reloadStartedAtMs;
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
    setHeldIngress(id, entries) {
      return withFrame("setHeldIngress", id, (frame) => {
        const parsed = Array.isArray(entries) ? entries.map(parseHeldIngressEntry) : null;
        if (parsed === null || !parsed.every((e): e is HeldIngressWireEntry => e !== null)) {
          note("warn", `setHeldIngress was handed entries that are not ingress entries: ${JSON.stringify(entries)}`);
          return "refused";
        }
        frame.heldIngress = parsed;
        post(frame, { channel: HOST_CHANNEL, kind: "heldIngress", entries: parsed });
        return "sent";
      });
    },
    dismissDigest(id) {
      return withFrame("dismissDigest", id, (frame) => {
        post(frame, { channel: HOST_CHANNEL, kind: "dismissDigest" });
        return "sent";
      });
    },
    focusMoved() {
      broadcast({ channel: HOST_CHANNEL, kind: "focusMoved" });
      return answer("focusMoved", null, "sent");
    },
    retireAll() {
      // THE RELOAD'S TEARDOWN IS THE HOST'S OWN ACT, so it ends the pages
      // before Emacs navigates: the navigation cancels every frame's loads
      // before their documents see `pagehide`, and each page read its
      // streams' ends as a cut link and drew the unreachable chip
      // (2026-10-24).
      const pages = frames.size;
      for (const frame of frames.values()) {
        dropIncoming(frame.id);
        retire(frame);
      }
      frames.clear();
      shownId = null;
      note("info", `retired ${pages} page(s); the reload Emacs asked for follows`);
      endPage();
      return answer("retireAll", null, "retired", { pages });
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
      for (const id of [...incoming.keys()]) dropIncoming(id);
      for (const frame of frames.values()) frame.iframe.remove();
      frames.clear();
      shownId = null;
      for (const lane of lanes.values()) {
        if (lane.bound !== null) win.clearTimeout(lane.bound);
      }
      lanes.clear();
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
