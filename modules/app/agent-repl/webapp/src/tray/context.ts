/**
 * context — the seam every tray drawing function is handed.
 *
 * The tray is WHOLE-LIST-REPLACED on every push: `DaemonHoldTray` carries the
 * items entire, so a redraw throws the previous DOM away rather than
 * reconciling it. Anything a card attached to the world outside its own
 * element — a ticker subscription for the queued-at age — must therefore come
 * down with that element, and `onDispose` is the one channel for saying so.
 * Without it a card that lived for two pushes would leave two subscriptions
 * ticking a node no longer in the document.
 *
 * It is a SEPARATE MODULE from `tray.ts` because the cards import it and
 * `tray.ts` imports the cards; declaring it here is what keeps that a tree
 * rather than a cycle.
 */
import type { AppContext } from "../rpc/context.js";
import type { ClassifierUpdateForms } from "./classifier-update.js";

/** What a tray card is told about the app it is drawing into. */
export interface TrayContext {
  /** The app-wide capabilities: the client to call, the workspace, the clock. */
  ctx: AppContext;
  /**
   * Register a teardown the NEXT whole-tray redraw (and `dispose()`) runs.
   * Every ticker subscription a card opens is registered here.
   */
  onDispose(fn: () => void): void;
  /**
   * Every held turn's "Update classifier" form. It OUTLIVES the push, unlike
   * everything else here: the tray hands the same registry to every drawing,
   * so a form being typed into survives a redraw (classifier-update.ts).
   */
  classifierForms: ClassifierUpdateForms;
}
