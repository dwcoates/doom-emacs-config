/**
 * viewed — the roster row's DISPLAY MODE.
 *
 * THE MODE IS THE CONTRACT, not a decoration. `frontend.v1.RosterRowViewed`
 * states it once, for every surface that draws a workspace: ABSENT is FULL and
 * PRESENT is PARTIAL, and PARTIAL means "you have already read this
 * workspace's last turn result". The Emacs tab-bar implements the same two
 * words off the same message — full is the whole `[N] <name>` entry in the
 * status colour, partial is the bracket alone — and a divergence between the
 * two surfaces is a defect rather than a taste difference.
 *
 * WHAT THIS DRAWS, AND WHAT IT LEAVES ALONE. In partial mode the row's NAME
 * text and its status dot recede to the muted colour (owner request,
 * 2026-10-12: a viewed workspace stays in its grouping, and the grey dot is
 * what sets it apart). The badges, the when-column and the highlight are
 * untouched. The marker only rides a turn-end arm, so greying the dot never
 * hides live work.
 *
 * THE WIRE'S MARKER IS THE WHOLE ANSWER. The daemon derives the marker from
 * the workspace's read-result fact in the same render that resolves the
 * row's status, so every push carries a status and a marker that already
 * agree. This end keeps no memory and applies no rule of its own: a row that
 * comes back from `idle_async` to a turn end whose result the user has
 * already read arrives PARTIAL on the very push that changes its status, and
 * drawing it FULL there would claim an unread result that is not.
 */
import { log } from "../log.js";

/** The two modes, in the tab-bar's own words. */
export type ViewedMode = "full" | "partial";

/**
 * The mode a row draws in, and THE ONE PLACE either mode is decided.
 *
 * `viewed` is the wire's marker (present = the daemon holds this row
 * PARTIAL); `arm` is the row's status oneof case, carried for the record only.
 */
export function viewedMode(workspaceId: string, arm: string, viewed: boolean): ViewedMode {
  if (!viewed) return "full";
  log.debug("a row is drawn PARTIAL: the user has already read its result", {
    operation: "sidebar.viewed.partial",
    context: { workspace: workspaceId, arm },
  });
  return "partial";
}
