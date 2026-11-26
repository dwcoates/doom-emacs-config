/**
 * What every topbar element is handed: the app context, the shared reveal
 * layer, the door to the login overlay, and the switch to another workspace.
 *
 * The reveal layer is here rather than passed element by element because a
 * reveal is a property of the STRIP, not of the control that opened it: only
 * one may be open, and it must outlive the redraw its own push causes.
 */
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { AppContext } from "../rpc/context.js";
import type { RevealLayer } from "./reveal.js";

export interface TopbarContext {
  /** The one reveal layer under the strip. */
  readonly reveals: RevealLayer;
  readonly ctx: AppContext;
  /** Raise the login overlay (the logged-out account chip's click). */
  readonly openLogin: (control: HTMLElement) => void;
  /**
   * Select TARGET, drawing a refusal at HOST: the summary's child menu's
   * click. The mount wires it to the feed's one jump (`jumpToWorkspace`), so
   * the strip's modules import no feed module.
   */
  readonly selectWorkspace: (host: HTMLElement, target: WorkspaceRef) => Promise<void>;
}
