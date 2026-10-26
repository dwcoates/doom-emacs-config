/**
 * embed — A WORKSPACE PAGE'S SIDE OF THE HOST PROTOCOL.
 *
 * A page that boots inside the host's iframe (host/protocol.ts) says hello,
 * takes what the host sends (whether it is shown, and the commands Emacs gives
 * it), and pushes the facts Emacs reads back: that its conversation is drawn,
 * its selection, and whether a text field holds its keyboard. A page in no
 * host does none of it.
 *
 * Only messages from the PARENT window are taken: the host is the page's
 * parent, and nothing else may steer it.
 */
import { CONVERSATION_DRAWN_ATTRIBUTE } from "../feed/conversation-drawn.js";
import { DOCK_BACKGROUND_PROPERTY, DOCK_HEIGHT_PROPERTY } from "../feed/gate-dock.js";

/** The id of the page's docked gate band (index.html). */
const GATE_DOCK_ID = "gate-dock";
import { LEVEL_WINDOW_OPERATION, log } from "../log.js";
import type { AppliedLevel, LevelWindow, WindowLevel } from "../../../agent-shim/logging/ts/level-window.js";
import { closeStandingNewsDigest } from "../news-digest/news-digest.js";
import type { PresenceSource } from "../presence.js";
import type { FocusLeftSource } from "../focus-left.js";
import type { WebhostFailureView } from "../failure/webhost.js";
import { HOST_CHANNEL, onHostChannel, parseHostToPage, type HostToPage, type PageToHost } from "./protocol.js";
import { pageMutationObserver } from "../dom.js";
import { fromJson, type JsonObject } from "@bufbuild/protobuf";
import { UserSaidSchema } from "../../../proto/gen/ts/conversation/v1/user_pb";
import type { EditorIngressEntry, EditorIngressView } from "../held/editor-ingress.js";
import type { HeldIngressWireEntry } from "./protocol.js";

/**
 * The editor's ingress entries, decoded. An entry whose `said` is not a
 * UserSaid is an ERROR naming its key and left out: the editor wrote what the
 * daemon would quarantine, and drawing a guess at it would show words nobody
 * typed.
 */
export function decodeHeldIngress(wire: readonly HeldIngressWireEntry[]): EditorIngressEntry[] {
  const out: EditorIngressEntry[] = [];
  for (const entry of wire) {
    try {
      out.push({
        key: entry.key,
        queuedAtMs: Date.parse(entry.queuedAt),
        said: fromJson(UserSaidSchema, entry.said as JsonObject),
      });
    } catch (err) {
      log.error("an editor ingress entry carried a said this page cannot read; it is not drawn", {
        operation: "page.held-ingress-undecodable",
        context: { key: entry.key, cause: (err as Error).message },
      });
    }
  }
  return out;
}

/** What the link needs from its environment, injectable for tests. */
export interface EmbedEnv {
  readonly doc: Document;
  readonly win: Window;
  readonly presence: PresenceSource;
  /** Where the webhost's own failures are listed for this page's warning chip. */
  readonly webhostFailures: WebhostFailureView;
  /** The page's live log threshold, which the host raises (`logLevel`). */
  readonly levelWindow: LevelWindow;
  /** The editor's held-prompt ingress entries, which the host hands over (`heldIngress`). */
  readonly editorIngress: EditorIngressView;
  /** The page's focus-left signal, which Emacs moving its focus fires (`focusMoved`). */
  readonly focusLeft: FocusLeftSource;
}

/** Why the focus left the page when Emacs moved its own. */
export const FOCUS_LEFT_EMACS = "Emacs moved its focus";

/**
 * The class the page's root wears while the webhost draws the sidebar over the
 * page's sidebar column, which the page then keeps empty and reserved.
 */
export const WEBHOST_SIDEBAR_CLASS = "webhost-sidebar";

/** The installed link; `dispose` removes every listener. */
export interface HostLink {
  dispose(): void;
}

/** Whether ELEMENT is a text field: what Emacs leaves the keyboard to. */
function isTextField(element: Element | null): boolean {
  return element !== null && (element.nodeName === "INPUT" || element.nodeName === "TEXTAREA");
}

/** Connect the page in ENV to its host; null when the page is in no host. */
export function connectToHost(env: EmbedEnv): HostLink | null {
  const { doc, win, presence, webhostFailures, levelWindow, editorIngress, focusLeft } = env;
  if (win.parent === win) return null;
  const host = win.parent;
  const send = (message: PageToHost): void => {
    host.postMessage(message, "*");
  };

  const apply = (message: HostToPage): void => {
    const root = doc.documentElement.style;
    switch (message.kind) {
      case "presence":
        if (message.shown !== presence.shown()) {
          log.info(message.shown ? "the host showed this page" : "the host hid this page", {
            operation: "page.host-presence",
            context: { shown: message.shown, visibility: doc.visibilityState },
          });
        }
        // THE RELOAD FIRST, so a show it rides with already knows it.
        if (message.reloadStartedAtMs !== undefined) presence.setReloadStartedAt(message.reloadStartedAtMs);
        presence.setShown(message.shown, message.switchStartedAtMs ?? null);
        return;
      case "inputBackground":
        root.setProperty(DOCK_BACKGROUND_PROPERTY, message.background);
        return;
      case "gateDock":
        root.setProperty(DOCK_HEIGHT_PROPERTY, `${message.pixels}px`);
        if (message.background !== null) root.setProperty(DOCK_BACKGROUND_PROPERTY, message.background);
        return;
      case "sidebarDock":
        if (doc.documentElement.classList.contains(WEBHOST_SIDEBAR_CLASS) !== message.docked) {
          log.info(message.docked ? "the webhost docked its sidebar over this page" : "the webhost undocked its sidebar", {
            operation: "page.webhost-sidebar-dock",
            context: { docked: message.docked },
          });
        }
        doc.documentElement.classList.toggle(WEBHOST_SIDEBAR_CLASS, message.docked);
        return;
      case "webhostFailures":
        webhostFailures.set(message.failures);
        return;
      case "heldIngress":
        editorIngress.set(decodeHeldIngress(message.entries));
        return;
      case "logLevel":
        applyLogLevel(message.level, message.untilMs);
        return;
      case "focusMoved":
        focusLeft.left(FOCUS_LEFT_EMACS);
        return;
      case "dismissDigest": {
        // Escape in Emacs's composer (or a sent prompt) closing the digest is
        // a person's action, recorded at INFO; a page with none standing has
        // nothing to record above DEBUG.
        if (closeStandingNewsDigest(doc)) {
          log.info("escape in Emacs closed the standing news digest", {
            operation: "page.host-dismissed-digest",
            context: {},
          });
        } else {
          log.debug("the host asked to close a news digest none stands", {
            operation: "page.host-dismiss-digest-none",
            context: {},
          });
        }
        return;
      }
    }
  };

  /** Raise the page to LEVEL until UNTILMS, or back to info. */
  const applyLogLevel = (level: WindowLevel, untilMs: number | null): void => {
    let applied: AppliedLevel;
    try {
      applied = levelWindow.apply(level, untilMs);
    } catch (err) {
      log.error("the host asked for a log level window the page refused", {
        operation: LEVEL_WINDOW_OPERATION,
        context: { level, until_ms: untilMs, cause: (err as Error).message },
      });
      return;
    }
    switch (applied.outcome) {
      case "closed":
        if (applied.closed !== null) {
          log.info("the host closed this page's log level window; back at info", {
            operation: LEVEL_WINDOW_OPERATION,
            context: { from_level: applied.closed.from, outcome: "window_closed" },
          });
        }
        return;
      case "expired":
        log.debug("the host's log level window had already ended when it arrived", {
          operation: LEVEL_WINDOW_OPERATION,
          context: { level, until_ms: untilMs, outcome: "expired" },
        });
        return;
      case "opened":
        log.info("the host raised this page's log level for its window", {
          operation: LEVEL_WINDOW_OPERATION,
          context: { level, until: Math.floor(applied.untilMs / 1000), outcome: "window_opened" },
        });
    }
  };

  const onMessage = (event: MessageEvent): void => {
    if (event.source !== host || !onHostChannel(event.data)) return;
    const message = parseHostToPage(event.data);
    if (message === null) {
      // A WEBHOST FROM ANOTHER BUILD, which a rollout makes for a moment:
      // expected, and over by itself once both run the same build.
      log.info("the host sent a message this build cannot read", {
        operation: "page.host-message-unreadable",
        context: { message: JSON.stringify(event.data) },
      });
      return;
    }
    apply(message);
  };

  const reportSelection = (): void => {
    const selection = win.getSelection();
    send({
      channel: HOST_CHANNEL,
      kind: "selection",
      text: selection === null ? null : selection.toString(),
      focused: doc.hasFocus(),
    });
  };

  let editing = false;
  const reportEditing = (now: boolean): void => {
    if (now === editing) return;
    editing = now;
    send({ channel: HOST_CHANNEL, kind: "editing", editing: now });
  };
  const onFocusIn = (event: FocusEvent): void => {
    reportEditing(isTextField(event.target instanceof Element ? event.target : null));
  };
  // `focusout` fires while the leaving element is still the active one; the
  // element taking the focus is its `relatedTarget` (null when the focus
  // leaves the page).
  const onFocusOut = (event: FocusEvent): void => {
    reportEditing(isTextField(event.relatedTarget instanceof Element ? event.relatedTarget : null));
  };

  const reportDrawn = (): void => {
    const what = doc.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
    if (what === "page" || what === "refused") send({ channel: HOST_CHANNEL, kind: "drawn", what });
  };
  // THE GATE BAND'S HEIGHT, as laid out: the webhost ends its sidebar above it.
  // A dock that is hidden measures 0, which is the band being gone.
  let gateBand = -1;
  const reportGateBand = (pixels: number): void => {
    const rounded = Math.round(pixels);
    if (rounded === gateBand) return;
    gateBand = rounded;
    send({ channel: HOST_CHANNEL, kind: "gateBand", pixels: rounded });
  };
  const gateDock = doc.getElementById(GATE_DOCK_ID);
  const gateWatch =
    gateDock === null
      ? null
      : new ResizeObserver(() => {
          reportGateBand(gateDock.getBoundingClientRect().height);
        });
  gateWatch?.observe(gateDock as Element);

  const drawnWatch = pageMutationObserver(doc, reportDrawn);
  drawnWatch.observe(doc.documentElement, { attributes: true, attributeFilter: [CONVERSATION_DRAWN_ATTRIBUTE] });

  win.addEventListener("message", onMessage);
  doc.addEventListener("selectionchange", reportSelection);
  win.addEventListener("focus", reportSelection);
  win.addEventListener("blur", reportSelection);
  doc.addEventListener("focusin", onFocusIn);
  doc.addEventListener("focusout", onFocusOut);

  send({ channel: HOST_CHANNEL, kind: "hello" });
  reportDrawn();
  if (gateDock !== null) reportGateBand(gateDock.getBoundingClientRect().height);
  log.info("the page joined its host", { operation: "page.host-joined", context: {} });

  return {
    dispose() {
      gateWatch?.disconnect();
      drawnWatch.disconnect();
      win.removeEventListener("message", onMessage);
      doc.removeEventListener("selectionchange", reportSelection);
      win.removeEventListener("focus", reportSelection);
      win.removeEventListener("blur", reportSelection);
      doc.removeEventListener("focusin", onFocusIn);
      doc.removeEventListener("focusout", onFocusOut);
    },
  };
}
