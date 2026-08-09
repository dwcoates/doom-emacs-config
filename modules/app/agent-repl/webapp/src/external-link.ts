/**
 * Every hyperlink in this page opens in the external browser, never in the
 * webview showing this page.
 *
 * WHY THIS EXISTS. The gui frontend IS this page, mounted inside Emacs as an
 * xwidget. Response bubbles render markdown anchors (markdown.ts emits them
 * with `target="_blank"`), and a click on one makes WebKit either navigate the
 * webview away from the app or ask Emacs for a second xwidget buffer. Either
 * way the conversation the user was reading is replaced by a web page inside
 * an editor.
 *
 * The click is therefore CANCELLED before it can become a navigation, and the
 * URL is posted to the daemon, which launches the browser
 * (daemon/internal/externalbrowser). Cancelling at the click is what makes an
 * in-webview navigation impossible rather than merely unlikely: there is no
 * "navigate back afterwards" recovery to race with, because no navigation is
 * ever started.
 *
 * SCOPE. Only http/https anchors are claimed. In-page anchors ("#…") and the
 * app's own control elements are left alone: they are UI gestures, not
 * hyperlinks, and hijacking them would break the page.
 */
import { log } from "./wslog.js";

/** Opens one URL outside the webview. Rejects when the daemon refused it. */
export type ExternalOpener = (url: string) => Promise<void>;

/** The fetch shape this module needs, so tests pass a stub. */
export type FetchFn = (input: string, init?: RequestInit) => Promise<Response>;

/** Daemon route that hands a URL to the pinned external browser profile. */
export const OPEN_EXTERNAL_PATH = "/open-external";

/**
 * Build the opener that posts URL to the daemon's external-browser route.
 * httpBase is the daemon origin main.ts already resolved for its other calls.
 */
export function makeExternalOpener(httpBase: string, fetchFn: FetchFn): ExternalOpener {
  return async (url: string): Promise<void> => {
    const resp = await fetchFn(`${httpBase}${OPEN_EXTERNAL_PATH}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ url }),
    });
    if (!resp.ok) throw new Error(`POST ${OPEN_EXTERNAL_PATH}: ${resp.status}`);
  };
}

/**
 * The anchor a click lands on, when the click should open externally.
 *
 * Returns null when the event must be left alone: a modified click (the user
 * asked their own platform to do something with it), a non-primary button, an
 * already-cancelled event, a target outside any anchor, or an anchor whose
 * href is not an http/https destination.
 */
export function externalAnchorFor(e: MouseEvent): HTMLAnchorElement | null {
  if (e.defaultPrevented) return null;
  if (e.button !== 0) return null;
  if (e.metaKey || e.ctrlKey || e.shiftKey || e.altKey) return null;
  const target = e.target as Element | null;
  if (!target || typeof target.closest !== "function") return null;
  const anchor = target.closest("a[href]") as HTMLAnchorElement | null;
  if (!anchor) return null;
  const href = anchor.getAttribute("href") ?? "";
  if (!/^https?:\/\//i.test(href)) return null;
  return anchor;
}

/**
 * Claim every http/https anchor click under ROOT for the external browser.
 *
 * Listens in the CAPTURE phase so the cancellation lands before any bubble- or
 * feed-level handler can act on the same click, and returns the listener's
 * removal function so a caller (a test, a teardown) can unclaim.
 */
export function installExternalLinkInterceptor(
  root: EventTarget,
  open: ExternalOpener,
): () => void {
  const onClick = (evt: Event): void => {
    const e = evt as MouseEvent;
    const anchor = externalAnchorFor(e);
    if (!anchor) return;
    // Cancel FIRST: the webview must not navigate even if the open below
    // fails, because a failed open is reported as an error while a navigation
    // has already destroyed the page the user was reading.
    e.preventDefault();
    e.stopPropagation();
    const url = anchor.getAttribute("href") ?? "";
    void open(url).catch((err: unknown) => {
      // A link that went nowhere must say so: the user just clicked expecting
      // a browser window, and silence would read as a dead rail.
      log("error", `opening ${url} in the external browser failed: ${String(err)}`, {
        operation: "external-link.open-failed",
        context: { url, cause: err },
      });
    });
  };
  root.addEventListener("click", onClick, true);
  return () => {
    root.removeEventListener("click", onClick, true);
  };
}
