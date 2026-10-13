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
import { log } from "../log.js";
import { closeStandingNewsDigest } from "../news-digest/news-digest.js";
import type { PresenceSource } from "../presence.js";
import { HOST_CHANNEL, onHostChannel, parseHostToPage, type HostToPage, type PageToHost } from "./protocol.js";

/** What the link needs from its environment, injectable for tests. */
export interface EmbedEnv {
  readonly doc: Document;
  readonly win: Window;
  readonly presence: PresenceSource;
}

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
  const { doc, win, presence } = env;
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
        if (message.warm !== presence.warm() && !message.shown) {
          log.info(message.warm ? "the host warmed this page" : "the host cooled this page", {
            operation: "page.host-warmth",
            context: { warm: message.warm },
          });
        }
        presence.setShown(message.shown);
        presence.setWarm(message.warm);
        return;
      case "inputBackground":
        root.setProperty(DOCK_BACKGROUND_PROPERTY, message.background);
        return;
      case "gateDock":
        root.setProperty(DOCK_HEIGHT_PROPERTY, `${message.pixels}px`);
        if (message.background !== null) root.setProperty(DOCK_BACKGROUND_PROPERTY, message.background);
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

  const onMessage = (event: MessageEvent): void => {
    if (event.source !== host || !onHostChannel(event.data)) return;
    const message = parseHostToPage(event.data);
    if (message === null) {
      log.warn("the host sent a message this build cannot read", {
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
  const drawnWatch = new MutationObserver(reportDrawn);
  drawnWatch.observe(doc.documentElement, { attributes: true, attributeFilter: [CONVERSATION_DRAWN_ATTRIBUTE] });

  win.addEventListener("message", onMessage);
  doc.addEventListener("selectionchange", reportSelection);
  win.addEventListener("focus", reportSelection);
  win.addEventListener("blur", reportSelection);
  doc.addEventListener("focusin", onFocusIn);
  doc.addEventListener("focusout", onFocusOut);

  send({ channel: HOST_CHANNEL, kind: "hello" });
  reportDrawn();
  log.info("the page joined its host", { operation: "page.host-joined", context: {} });

  return {
    dispose() {
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
