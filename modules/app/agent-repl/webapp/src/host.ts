/**
 * The Emacs host's bridge into the webapp.
 *
 * The gui frontend is this page mounted inside Emacs as an xwidget, and
 * the only channel Emacs has into it is evaluating JavaScript against the
 * live document. So everything the host must be able to trigger is planted
 * on `window` under a name both sides agree on, and nowhere else — the
 * lisp end of the contract is `agent-repl--frontend-webview-execute-script'
 * (frontend.el), which calls the hook by exactly the name exported here.
 *
 * Keep the surface a single named global per host action: a host script is
 * written as a string in lisp, so a DOM query written there instead would
 * couple frontend.el to this page's markup and drift silently the moment a
 * class or id changes.
 */
import { ScrollTail, parkAtTail } from "./scroll.js";

/**
 * Name of the global that snaps the feed to its newest message.
 * `agent-repl-frontend-tail-hook' (frontend.el) MUST match this string.
 */
export const TAIL_HOOK = "agentReplParkAtTail";

/** What a hook is planted on: `window`, or a plain object under test. */
export type HostGlobal = Record<string, unknown>;

/**
 * Plant the tail hook on TARGET, parking FEED at its newest message.
 *
 * Emacs fires it on every switch TO the workspace, so a feed the user
 * left scrolled up in history is back at the newest message the instant
 * the workspace is on screen, rather than showing stale middle-of-history
 * content until the next turn arrives.
 */
export function installHostTailHook(target: HostGlobal, feed: ScrollTail): void {
  target[TAIL_HOOK] = (): void => {
    parkAtTail(feed);
  };
}

/**
 * Name of the global that closes every open topbar dropdown.
 * `agent-repl-frontend-close-menus-hook' (frontend.el) MUST match this
 * string.
 */
export const CLOSE_MENUS_HOOK = "agentReplCloseTopbarMenus";

/**
 * Plant the close-menus hook on TARGET, dismissing every open topbar
 * overlay through CLOSE when the host fires it.
 *
 * The page's own outside-click handler already closes its dropdowns on a
 * click anywhere INSIDE the document, but the composer the GUI puts focus
 * into is a separate Emacs window the webview cannot see — so Emacs fires
 * this hook when the user clicks that input window, closing the header and
 * bubble dropdowns that would otherwise hang open behind it.
 */
export function installHostCloseMenusHook(target: HostGlobal, close: () => void): void {
  target[CLOSE_MENUS_HOOK] = (): void => {
    close();
  };
}

/**
 * Name of the global that nudges the page's text size.
 * `agent-repl-frontend-text-size-hook' (frontend.el) MUST match this
 * string.
 */
export const TEXT_SCALE_HOOK = "agentReplAdjustTextScale";

/**
 * The root font size, in px, that a scale of 1 maps to. The feed is laid
 * out almost entirely in `rem`, so scaling this one number grows or
 * shrinks every run of text together. 16 is the WKWebView default, so the
 * page looks identical at scale 1 whether or not the host has ever fired
 * the hook.
 */
export const TEXT_SCALE_BASE_PX = 16;

/** Floor on the scale factor, so a held key can't shrink text to nothing. */
export const TEXT_SCALE_MIN = 0.5;

/** Ceiling on the scale factor, so a held key can't blow text off screen. */
export const TEXT_SCALE_MAX = 3;

/** The single mutable field the text-scale hook writes. */
export interface TextScaleRoot {
  style: { fontSize: string };
}

/** Clamp SCALE into the supported range, keeping text legible and on screen. */
export function clampTextScale(scale: number): number {
  return Math.min(TEXT_SCALE_MAX, Math.max(TEXT_SCALE_MIN, scale));
}

/**
 * The px font size for SCALE, rounded to thousandths.
 *
 * The scale accumulates fine-grained deltas, so the raw product carries
 * binary-float dust (1.2 * 16 lands as 19.200000000000003). Sub-pixel
 * precision is already more than the render needs, so the emitted string
 * is rounded there rather than showing that dust in the DOM.
 */
export function textScalePx(scale: number): number {
  return Math.round(scale * TEXT_SCALE_BASE_PX * 1e3) / 1e3;
}

/**
 * Plant the text-scale hook on TARGET, sizing ROOT's font when fired.
 *
 * The page owns the current scale — Emacs has no per-webview state of its
 * own to keep it in — so the hook holds it in a closure that starts at 1
 * and accumulates every nudge. The host fires it with either a finite
 * number DELTA (added to the current scale, then clamped) or the string
 * `"reset"` (back to 1); anything else leaves the scale untouched. The
 * new scale is returned from the hook so a caller can read where it landed.
 */
export function installHostTextScaleHook(target: HostGlobal, root: TextScaleRoot): void {
  let scale = 1;
  target[TEXT_SCALE_HOOK] = (arg: unknown): number => {
    if (arg === "reset") {
      scale = 1;
    } else {
      const delta = typeof arg === "number" ? arg : Number(arg);
      if (Number.isFinite(delta)) {
        scale = clampTextScale(scale + delta);
      }
    }
    root.style.fontSize = `${textScalePx(scale)}px`;
    return scale;
  };
}
