/**
 * What every topbar element is handed: the app context, the shared reveal
 * layer, the client-local failures the warning chip lists, and the door to the
 * login overlay.
 *
 * The reveal layer is here rather than passed element by element because a
 * reveal is a property of the STRIP, not of the control that opened it: only
 * one may be open, and it must outlive the redraw its own push causes.
 */
import type { LocalFailure } from "../failure/local.js";
import type { AppContext } from "../rpc/context.js";
import type { RevealLayer } from "./reveal.js";

/**
 * What the warning chip needs to draw the client-local failures on its own —
 * before the first topbar push, or with no app context at all, because the
 * failure that stopped the page may be exactly what kept both from arriving.
 */
export interface WarningChipContext {
  /** The one reveal layer under the strip. */
  readonly reveals: RevealLayer;
  /** The client-local failures standing now (src/failure/local.ts). */
  readonly localFailures: () => readonly LocalFailure[];
}

export interface TopbarContext extends WarningChipContext {
  readonly ctx: AppContext;
  /** Raise the login overlay (the logged-out account chip's click). */
  readonly openLogin: (control: HTMLElement) => void;
}
