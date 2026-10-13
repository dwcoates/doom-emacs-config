/**
 * protocol — THE MESSAGES BETWEEN THE HOST PAGE AND A WORKSPACE PAGE.
 *
 * WHY THERE IS A HOST. Emacs used to give every workspace a WKWebView of its
 * own. WebKit suspends a web view nobody can see about half a second after it
 * loads, so a preloaded workspace page sat frozen until the user switched to
 * it and then replayed everything it had missed before it could paint: a
 * first switch took 0.7-2.6s (measured 2026-10-12). Now ONE web view, the
 * host page (host.ts), stays on screen and shows every workspace's page as an
 * iframe at the workspace's own origin. An iframe of a visible page is never
 * suspended, so every workspace page stays live and current, and a switch only
 * changes which iframe is shown.
 *
 * WHY MESSAGES. Each workspace page keeps its own origin (one per workspace,
 * for WebKit's per-host connection budget; lisp/frontend.el), so the host
 * cannot reach into it. The two speak only through `postMessage`, and only
 * these messages, each tagged with {@link HOST_CHANNEL} so nothing else that
 * might post to a window is mistaken for one.
 *
 *   host -> page   presence, and the commands Emacs gives a workspace page;
 *   page -> host   hello (replay my state), and the facts Emacs reads back.
 *
 * Emacs reads the host synchronously (a script evaluation answers at once), so
 * every fact Emacs reads about a page is PUSHED to the host by the page when
 * it changes, and the host answers from that mirror.
 */

/** The tag every message of this protocol carries. */
export const HOST_CHANNEL = "agent-repl-host";

/** What a page reports drawn: an opening page, or a refused open. */
export type DrawnWhat = "page" | "refused";

/** host -> page: whether the host is showing this page now, and whether it is warm. */
interface PresenceMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "presence";
  readonly shown: boolean;
  /** Shown, or among the pages switched to most recently and lately (host.ts). */
  readonly warm: boolean;
}

/** host -> page: the background the workspace's Emacs input window paints. */
interface InputBackgroundMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "inputBackground";
  readonly background: string;
}

/** host -> page: the slot a docked gate takes, the input window's. */
interface GateDockMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "gateDock";
  readonly pixels: number;
  /** The input window's background, or null when Emacs has none to give. */
  readonly background: string | null;
}

/** host -> page: close the standing news digest, as its close control does. */
interface DismissDigestMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "dismissDigest";
}

/** Every message the host sends a page. */
export type HostToPage = PresenceMessage | InputBackgroundMessage | GateDockMessage | DismissDigestMessage;

/** page -> host: the page booted; send it everything it should know. */
interface HelloMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "hello";
}

/** page -> host: the page's conversation is on screen. */
interface DrawnMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "drawn";
  readonly what: DrawnWhat;
}

/** page -> host: the page's selection, and whether the page has focus. */
interface SelectionMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "selection";
  /** The selected text; null only when the page has no selection object. */
  readonly text: string | null;
  readonly focused: boolean;
}

/** page -> host: whether a text field holds the page's keyboard focus. */
interface EditingMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "editing";
  readonly editing: boolean;
}

/** Every message a page sends the host. */
export type PageToHost = HelloMessage | DrawnMessage | SelectionMessage | EditingMessage;

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

/**
 * DATA as a host-to-page message, or null when it is not one of ours. A message
 * that carries our channel but not a shape this build knows is ALSO null; the
 * receiver logs it, because a host and a page from two different builds are
 * the one way that happens.
 */
export function parseHostToPage(data: unknown): HostToPage | null {
  if (!isRecord(data) || data.channel !== HOST_CHANNEL) return null;
  switch (data.kind) {
    case "presence":
      return typeof data.shown === "boolean" && typeof data.warm === "boolean"
        ? { channel: HOST_CHANNEL, kind: "presence", shown: data.shown, warm: data.warm }
        : null;
    case "inputBackground":
      return typeof data.background === "string"
        ? { channel: HOST_CHANNEL, kind: "inputBackground", background: data.background }
        : null;
    case "gateDock":
      return typeof data.pixels === "number" &&
        Number.isFinite(data.pixels) &&
        (data.background === null || typeof data.background === "string")
        ? { channel: HOST_CHANNEL, kind: "gateDock", pixels: data.pixels, background: data.background }
        : null;
    case "dismissDigest":
      return { channel: HOST_CHANNEL, kind: "dismissDigest" };
    default:
      return null;
  }
}

/** DATA as a page-to-host message, or null when it is not one of ours. */
export function parsePageToHost(data: unknown): PageToHost | null {
  if (!isRecord(data) || data.channel !== HOST_CHANNEL) return null;
  switch (data.kind) {
    case "hello":
      return { channel: HOST_CHANNEL, kind: "hello" };
    case "drawn":
      return data.what === "page" || data.what === "refused"
        ? { channel: HOST_CHANNEL, kind: "drawn", what: data.what }
        : null;
    case "selection":
      return (data.text === null || typeof data.text === "string") && typeof data.focused === "boolean"
        ? { channel: HOST_CHANNEL, kind: "selection", text: data.text, focused: data.focused }
        : null;
    case "editing":
      return typeof data.editing === "boolean" ? { channel: HOST_CHANNEL, kind: "editing", editing: data.editing } : null;
    default:
      return null;
  }
}

/** Whether DATA carries this protocol's channel, whatever its shape. */
export function onHostChannel(data: unknown): boolean {
  return isRecord(data) && data.channel === HOST_CHANNEL;
}
