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
import { LEVEL_WINDOW_OPERATION, log } from "../log.js";
import type { AppliedLevel, LevelWindow, WindowLevel } from "../../../agent-shim/logging/ts/level-window.js";
import { BUBBLE_OVERLAY_ATTRIBUTE, closeStandingBubbleOverlay } from "../bubble/overlay.js";
import { closeStandingImageOverlay } from "../image-overlay/image-overlay.js";
import { closeStandingNewsDigest } from "../news-digest/news-digest.js";
import type { PresenceSource } from "../presence.js";
import type { QuietSource } from "../quiet.js";
import { FOCUS_LEFT_EMACS, type FocusLeftSource } from "../focus-left.js";
import type { WebhostFailureView } from "../failure/webhost.js";
import { HOST_CHANNEL, onHostChannel, parseHostToPage, type HostToPage, type PageToHost } from "./protocol.js";
import { pageMutationObserver } from "../dom.js";
import { fromJson, type JsonObject } from "@bufbuild/protobuf";
import { UserSaidSchema } from "../../../proto/gen/ts/conversation/v1/user_pb";
import type { EditorIngressEntry, EditorIngressView } from "../held/editor-ingress.js";
import type { EditorNotices } from "../footer/editor-notice.js";
import type { HeldIngressWireEntry } from "./protocol.js";
import type { RestyleTurnsSource } from "./restyle-turn.js";
import type { WebhostPressSource } from "../webhost-pressed.js";
import { onResponseHighlightWords } from "../feed/response-highlight.js";

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
  /** The page's lid-closed quiet state, which the host's `quiet` sets. */
  readonly quiet: QuietSource;
  /** Where the webhost's own failures are listed for this page's warning chip. */
  readonly webhostFailures: WebhostFailureView;
  /** The page's live log threshold, which the host raises (`logLevel`). */
  readonly levelWindow: LevelWindow;
  /** The editor's held-prompt ingress entries, which the host hands over (`heldIngress`). */
  readonly editorIngress: EditorIngressView;
  /** The editor's composer notices, which the host hands over (`editorNotice`). */
  readonly editorNotices: Pick<EditorNotices, "raise">;
  /** The page's focus-left signal, which Emacs moving its focus fires (`focusMoved`). */
  readonly focusLeft: FocusLeftSource;
  /** The page's webhost-press signal, which the reader pressing the webhost fires (`webhostPressed`). */
  readonly webhostPresses: WebhostPressSource;
  /** The page's restyle turns, which ask the host and which its grant runs (`restyleGranted`). */
  readonly restyleTurns: RestyleTurnsSource;
}

export { FOCUS_LEFT_EMACS };

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
  const { doc, win, presence, quiet, webhostFailures, levelWindow, editorIngress, editorNotices, focusLeft, webhostPresses, restyleTurns } = env;
  if (win.parent === win) return null;
  const host = win.parent;
  const send = (message: PageToHost): void => {
    host.postMessage(message, "*");
  };
  restyleTurns.connect((kind) => send({ channel: HOST_CHANNEL, kind }));

  const apply = (message: HostToPage): void => {
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
      case "webhostFailures":
        webhostFailures.set(message.failures);
        return;
      case "heldIngress":
        editorIngress.set(decodeHeldIngress(message.entries));
        return;
      case "editorNotice":
        editorNotices.raise(message.text, Date.now());
        return;
      case "logLevel":
        applyLogLevel(message.level, message.untilMs);
        return;
      case "focusMoved":
        focusLeft.left(FOCUS_LEFT_EMACS);
        return;
      case "quiet":
        if (message.quiet !== quiet.quiet()) {
          log.info(message.quiet ? "the lid closed: the page went quiet" : "the lid opened: the page left the quiet state", {
            operation: "page.quiet",
            context: { quiet: message.quiet },
          });
        }
        quiet.set(message.quiet);
        return;
      case "webhostPressed":
        webhostPresses.pressed();
        return;
      case "restyleGranted":
        restyleTurns.granted();
        return;
      case "dismissDigest": {
        // Escape in Emacs's composer (or a sent prompt) closing the digest is
        // a person's action, recorded at INFO; a page with none standing has
        // nothing to record above DEBUG.
        // The same escape closes a bubble expanded over the webapp, and an image opened large over the feed.
        if (closeStandingBubbleOverlay(doc)) {
          log.info("escape in Emacs closed the standing bubble overlay", {
            operation: "page.host-dismissed-bubble-overlay",
            context: {},
          });
        }
        if (closeStandingImageOverlay(doc)) {
          log.info("escape in Emacs closed the standing image overlay", {
            operation: "page.host-dismissed-image-overlay",
            context: {},
          });
        }
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
  const drawnWatch = pageMutationObserver(doc, reportDrawn);
  drawnWatch.observe(doc.documentElement, { attributes: true, attributeFilter: [CONVERSATION_DRAWN_ATTRIBUTE] });
  // A BUBBLE EXPANDED OVER THE WEBAPP: the webhost lifts this page's iframe
  // above its own layers (the gate's band, the load button) while the root
  // says one stands.
  let overlayOpen = false;
  const reportOverlay = (): void => {
    const now = doc.documentElement.hasAttribute(BUBBLE_OVERLAY_ATTRIBUTE);
    if (now === overlayOpen) return;
    overlayOpen = now;
    send({ channel: HOST_CHANNEL, kind: "bubbleOverlay", open: now });
  };
  const overlayWatch = pageMutationObserver(doc, reportOverlay);
  overlayWatch.observe(doc.documentElement, { attributes: true, attributeFilter: [BUBBLE_OVERLAY_ATTRIBUTE] });

  // A PRESS ON THE PAGE closes the sidebar's popups over it (sidebar-popups.ts),
  // which the webhost draws and this page's document cannot see.
  const onPointerDown = (): void => {
    send({ channel: HOST_CHANNEL, kind: "pressed" });
  };

  win.addEventListener("message", onMessage);
  doc.addEventListener("pointerdown", onPointerDown, { capture: true });
  const stopHighlightWords = onResponseHighlightWords(doc, (words) => {
    send({ channel: HOST_CHANNEL, kind: "responseHighlight", words });
  });
  doc.addEventListener("selectionchange", reportSelection);
  win.addEventListener("focus", reportSelection);
  win.addEventListener("blur", reportSelection);
  doc.addEventListener("focusin", onFocusIn);
  doc.addEventListener("focusout", onFocusOut);

  send({ channel: HOST_CHANNEL, kind: "hello" });
  reportDrawn();
  reportOverlay();
  log.info("the page joined its host", { operation: "page.host-joined", context: {} });

  return {
    dispose() {
      drawnWatch.disconnect();
      overlayWatch.disconnect();
      win.removeEventListener("message", onMessage);
      doc.removeEventListener("pointerdown", onPointerDown, { capture: true });
      stopHighlightWords();
      doc.removeEventListener("selectionchange", reportSelection);
      win.removeEventListener("focus", reportSelection);
      win.removeEventListener("blur", reportSelection);
      doc.removeEventListener("focusin", onFocusIn);
      doc.removeEventListener("focusout", onFocusOut);
    },
  };
}
