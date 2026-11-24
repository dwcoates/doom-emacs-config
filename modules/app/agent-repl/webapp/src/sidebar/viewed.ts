/**
 * viewed — the roster row's DISPLAY MODE.
 *
 * THE MODE IS THE CONTRACT, not a decoration. `frontend.v1.RosterRowViewed`
 * states it once, for every surface that draws a workspace: ABSENT is FULL and
 * PRESENT is PARTIAL, and PARTIAL means "you have interacted with this
 * workspace since its last turn started". The Emacs tab-bar implements the same two
 * words off the same message — full is the whole `[N] <name>` entry in the
 * status colour, partial is the bracket alone — and a divergence between the
 * two surfaces is a defect rather than a taste difference.
 *
 * WHAT THIS DRAWS, AND WHAT IT LEAVES ALONE. In partial mode the row's NAME
 * text recedes to the muted colour, where in full mode it wears its status
 * tone (`name-tint.ts`), and nothing else changes: the status dot
 * keeps its tone (owner ruling, 2026-10-13: a viewed done workspace is green
 * like any other), and the badges, the when-column and the highlight are
 * untouched. The mode says what the user has seen; it never says what the
 * workspace is doing, and the dot is the thing that says that.
 *
 * THE WIRE'S MARKER IS THE WHOLE ANSWER. The daemon derives the marker from
 * the user's interactions (agentrepl.v1.ReportUserInteraction) in the same
 * render that resolves the row's status, so every push carries a status and
 * a marker that already agree. This end keeps no memory and applies no rule of
 * its own: a turn's start arrives FULL on the very push that carries it,
 * and only the user's next interaction draws it PARTIAL again.
 */
import { createLogger } from "../log.js";

const log = createLogger("webapp.sidebar.viewed");

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
