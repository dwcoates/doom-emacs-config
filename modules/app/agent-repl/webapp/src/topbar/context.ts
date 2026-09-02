/**
 * What every topbar element is handed: the app context, the shared reveal
 * layer, and the door to the login overlay.
 *
 * The reveal layer is here rather than passed element by element because a
 * reveal is a property of the STRIP, not of the control that opened it: only
 * one may be open, and it must outlive the redraw its own push causes.
 */
import type { AppContext } from "../rpc/context.js";
import type { RevealLayer } from "./reveal.js";

export interface TopbarContext {
  readonly ctx: AppContext;
  /** The one reveal layer under the strip. */
  readonly reveals: RevealLayer;
  /** Raise the login overlay (the logged-out account chip's click). */
  openLogin(control: HTMLElement): void;
}
