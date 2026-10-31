/**
 * editor-notice — the editor's composer notices, drawn as transient footer
 * lines (frontend.v1.FooterActivityTransientEditorNotice).
 *
 * THE COMPOSER HAS NO MODE LINE (owner ruling, 2026-10-31), so what its mode
 * line used to say (a refused or unanswered send, a held-prompt edit
 * beginning, the count of prompts waiting for the daemon) is a transient
 * footer line instead. Those notices are raised mostly when the daemon cannot
 * be reached, so the daemon can never be the one to push them: Emacs hands
 * each to the page through the web view host (host/protocol.ts
 * `editorNotice`), and the page composes the transient itself.
 *
 * ONE WINDOW. The notice's expiry is the instant it arrived plus
 * frontend.v1.FooterTransientWindow, the same window the daemon stamps its own
 * transients with, so an editor notice and a daemon transient always last
 * equally long.
 *
 * IT JOINS THE TRANSIENT TIER, NOT A TIER OF ITS OWN: the newest transient
 * wins (`withEditorNotice`), and a salient line outranks both, exactly as the
 * proto's precedence says.
 */
import { create } from "@bufbuild/protobuf";
import {
  FooterActivityTransientSchema,
  FooterTransientWindow,
  type FooterActivityTransient,
  type FooterActivityTransientOverEnduring,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { createListeners } from "../listeners.js";
import { log } from "../log.js";

/** The one transient window, in milliseconds (frontend.v1.FooterTransientWindow). */
export const TRANSIENT_WINDOW_MS: number = FooterTransientWindow.MS;

/** One notice the editor raised, with the instant the page received it. */
export interface EditorNotice {
  readonly text: string;
  /** When the page received it, epoch ms on the page's clock. */
  readonly atMs: number;
}

/** The editor's newest notice, as the page last heard it. */
export interface EditorNotices {
  /** The newest notice, or null when the editor never raised one. */
  current(): EditorNotice | null;
  /** Record TEXT as the newest notice, received at AT_MS. */
  raise(text: string, atMs: number): void;
  /** Be told every new notice; answers the unsubscribe. */
  subscribe(listener: () => void): () => void;
}

/** No notice, until the editor raises one. */
export function createEditorNotices(): EditorNotices {
  let newest: EditorNotice | null = null;
  const listeners = createListeners();
  return {
    current: () => newest,
    raise(text, atMs) {
      log.info("the editor raised a composer notice; drawing it as a transient footer line", {
        operation: "footer.editor-notice",
        context: { text, at_ms: atMs, expires_at_ms: atMs + TRANSIENT_WINDOW_MS },
      });
      newest = { text, atMs };
      listeners.notify();
    },
    subscribe(listener) {
      return listeners.add(listener);
    },
  };
}

/** NOTICE as the transient the proto names for it, expiring after the one window. */
export function editorNoticeTransient(notice: EditorNotice): FooterActivityTransient {
  return create(FooterActivityTransientSchema, {
    at: { atMs: BigInt(notice.atMs) },
    expiry: { expiresAtMs: BigInt(notice.atMs + TRANSIENT_WINDOW_MS) },
    kind: { case: "editorNotice", value: { text: notice.text } },
  });
}

/**
 * U with NOTICE as its transient when NOTICE is the newer of the two (the
 * transient tier's own rule: the newest wins). U is returned unchanged when
 * there is no notice, or the daemon's transient is as new or newer.
 */
export function withEditorNotice(
  u: FooterActivityTransientOverEnduring,
  notice: EditorNotice | null,
): FooterActivityTransientOverEnduring {
  if (notice === null) return u;
  const daemonAtMs = u.transient?.at?.atMs;
  if (daemonAtMs !== undefined && Number(daemonAtMs) >= notice.atMs) return u;
  return { ...u, transient: editorNoticeTransient(notice) };
}

/** NOTICE's text while it is live at NOW_MS, else null; for the client-verdict strip. */
export function liveEditorNotice(notice: EditorNotice | null, nowMs: number): EditorNotice | null {
  if (notice === null || nowMs >= notice.atMs + TRANSIENT_WINDOW_MS) return null;
  return notice;
}
