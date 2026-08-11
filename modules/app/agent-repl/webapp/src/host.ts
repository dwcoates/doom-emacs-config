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
import { TailFollow } from "./scroll.js";

/**
 * Name of the global that snaps the feed to its newest message.
 * `agent-repl-frontend-tail-hook' (frontend.el) MUST match this string.
 */
export const TAIL_HOOK = "agentReplParkAtTail";

/** What a hook is planted on: `window`, or a plain object under test. */
export type HostGlobal = Record<string, unknown>;

/**
 * Plant the tail hook on TARGET, parking the feed at its newest message
 * through TAIL, the page's single owner of the follow decision.
 *
 * Emacs fires it on every switch TO the workspace, so a feed the user
 * left scrolled up in history is back at the newest message the instant
 * the workspace is on screen, rather than showing stale middle-of-history
 * content until the next turn arrives.
 *
 * IT RESUMES FOLLOWING, NOT JUST SCROLLS. The switch is an explicit "show me
 * the newest", and a bare scrollTop write was not enough to keep it. The
 * switch also relayouts the webview, asynchronously relative to the lisp that
 * fired this, so the snap and the resize land in either order — and content
 * that arrives after the snap (a deferred item upgrading, a board mounting)
 * grows the feed beneath a scrollTop that stays put. Latching the follow is
 * what re-parks through both, so the switch RELIABLY lands at the bottom
 * rather than near it.
 */
export function installHostTailHook(target: HostGlobal, tail: TailFollow): void {
  target[TAIL_HOOK] = (): void => {
    tail.park();
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
 * Name of the global that repairs this page's daemon connection.
 * `agent-repl-frontend-recover-hook' (lisp/webview-recovery.el) MUST match
 * this string.
 */
export const RECOVER_HOOK = "agentReplRecoverNow";

/**
 * Plant the recovery hook on TARGET, running RECOVER when the host fires it.
 *
 * WHY THE HOST DRIVES THIS AT ALL: background-recovery.ts repairs a hidden
 * page on a `setInterval` heartbeat, but this embedder SUSPENDS a hidden
 * xwidget webview's timers (see ws.ts above `ensureConnected`), so that
 * heartbeat does not tick in exactly the page it exists to repair. Emacs's
 * timers do run, and its script channel reaches a hidden webview, so Emacs
 * fires this hook on the daemon link-up edge.
 *
 * This is a call INTO the existing repair path and holds no logic of its own:
 * a host-fired repair and a user-arrives repair are the same code, differing
 * only in the reason they name.
 */
export function installHostRecoverHook(
  target: HostGlobal,
  recover: (reason: string) => void,
): void {
  target[RECOVER_HOOK] = (reason: unknown): void => {
    recover(typeof reason === "string" && reason !== "" ? reason : "host_recover");
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
