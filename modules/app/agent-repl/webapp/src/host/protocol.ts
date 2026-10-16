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
 *   host -> page   presence, the commands Emacs gives a workspace page, and
 *                  what the webhost's own sidebar needs the page to know
 *                  (that it is docked over the page, and its failures);
 *   page -> host   hello (replay my state), the facts Emacs reads back, and
 *                  how tall the page's docked gate band is.
 *
 * Emacs reads the host synchronously (a script evaluation answers at once), so
 * every fact Emacs reads about a page is PUSHED to the host by the page when
 * it changes, and the host answers from that mirror.
 */

import { isClientFailureArm, type ClientFailureArm } from "../failure/sink.js";
import type { WindowLevel } from "../../../agent-shim/logging/ts/level-window.js";

/** The tag every message of this protocol carries. */
export const HOST_CHANNEL = "agent-repl-host";

/** What a page reports drawn: an opening page, or a refused open. */
export type DrawnWhat = "page" | "refused";

/** host -> page: whether the host is showing this page now. */
interface PresenceMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "presence";
  readonly shown: boolean;
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

/**
 * host -> page: whether the webhost draws the sidebar over the page's sidebar
 * column. While it does, the page keeps that column empty and reserved, so its
 * main column and docked gate sit exactly where they would beside its own
 * sidebar. Sent once the webhost's sidebar has drawn, and replayed on hello.
 */
interface SidebarDockMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "sidebarDock";
  readonly docked: boolean;
}

/** One failure of the webhost's own machinery, as a warning chip row draws it. */
export interface WebhostFailure {
  readonly arm: ClientFailureArm;
  readonly headline: string;
  readonly evidence: ReadonlyArray<readonly [string, string]>;
}

/**
 * host -> page: every failure of the webhost's own machinery standing now (its
 * sidebar's stream or verbs). The topbar's warning chip is the webapp's one
 * error surface and the webhost has no topbar, so every page lists these in
 * its own chip. The whole set, every time it changes, and replayed on hello.
 */
interface WebhostFailuresMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "webhostFailures";
  readonly failures: readonly WebhostFailure[];
}

/**
 * host -> page: the log level the page records at. A level other than info is
 * a window ending at UNTILMS (epoch ms; proto/vocab/log-level-window.json); info,
 * with no end, closes any window. A profiling session raises every page this
 * way while it runs, and the host replays a standing window to a page that
 * says hello before it ends.
 */
interface LogLevelMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "logLevel";
  readonly level: WindowLevel;
  readonly untilMs: number | null;
}

/** Every message the host sends a page. */
export type HostToPage =
  | LogLevelMessage
  | PresenceMessage
  | InputBackgroundMessage
  | GateDockMessage
  | DismissDigestMessage
  | SidebarDockMessage
  | WebhostFailuresMessage;

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

/**
 * page -> host: how tall the page's docked gate band is now, in CSS pixels; 0
 * while no gate stands. The band spans the whole window's width under the
 * sidebar column, so the webhost ends its sidebar above the band of the page
 * on screen, exactly where the page's own sidebar would end.
 */
interface GateBandMessage {
  readonly channel: typeof HOST_CHANNEL;
  readonly kind: "gateBand";
  readonly pixels: number;
}

/** Every message a page sends the host. */
export type PageToHost = HelloMessage | DrawnMessage | SelectionMessage | EditingMessage | GateBandMessage;

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
      return typeof data.shown === "boolean"
        ? { channel: HOST_CHANNEL, kind: "presence", shown: data.shown }
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
    case "logLevel": {
      if (!isWindowLevel(data.level)) return null;
      if (data.level === "info") {
        return data.untilMs === null ? { channel: HOST_CHANNEL, kind: "logLevel", level: "info", untilMs: null } : null;
      }
      return typeof data.untilMs === "number" && Number.isFinite(data.untilMs)
        ? { channel: HOST_CHANNEL, kind: "logLevel", level: data.level, untilMs: data.untilMs }
        : null;
    }
    case "sidebarDock":
      return typeof data.docked === "boolean" ? { channel: HOST_CHANNEL, kind: "sidebarDock", docked: data.docked } : null;
    case "webhostFailures": {
      if (!Array.isArray(data.failures)) return null;
      const failures = data.failures.map(parseWebhostFailure);
      return failures.every((f): f is WebhostFailure => f !== null)
        ? { channel: HOST_CHANNEL, kind: "webhostFailures", failures }
        : null;
    }
    default:
      return null;
  }
}

/** Whether VALUE names a log level of the shared vocabulary. */
function isWindowLevel(value: unknown): value is WindowLevel {
  return value === "debug" || value === "info" || value === "warn" || value === "error";
}

/** DATA as one webhost failure, or null when it is not one. */
function parseWebhostFailure(data: unknown): WebhostFailure | null {
  if (!isRecord(data) || typeof data.arm !== "string" || !isClientFailureArm(data.arm)) return null;
  if (typeof data.headline !== "string" || !Array.isArray(data.evidence)) return null;
  const evidence: Array<readonly [string, string]> = [];
  for (const row of data.evidence) {
    if (!Array.isArray(row) || row.length !== 2 || typeof row[0] !== "string" || typeof row[1] !== "string") return null;
    evidence.push([row[0], row[1]]);
  }
  return { arm: data.arm, headline: data.headline, evidence };
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
    case "gateBand":
      return typeof data.pixels === "number" && Number.isFinite(data.pixels) && data.pixels >= 0
        ? { channel: HOST_CHANNEL, kind: "gateBand", pixels: data.pixels }
        : null;
    default:
      return null;
  }
}

/** Whether DATA carries this protocol's channel, whatever its shape. */
export function onHostChannel(data: unknown): boolean {
  return isRecord(data) && data.channel === HOST_CHANNEL;
}
