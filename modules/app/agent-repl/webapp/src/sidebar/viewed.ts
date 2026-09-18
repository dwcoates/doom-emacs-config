/**
 * viewed — the roster row's DISPLAY MODE, and the one rule that restores it.
 *
 * THE MODE IS THE CONTRACT, not a decoration. `frontend.v1.RosterRowViewed`
 * states it once, for every surface that draws a workspace: ABSENT is FULL and
 * PRESENT is PARTIAL, PARTIAL means "you have already seen this workspace's
 * information", and the ONLY thing that restores FULL is a change to the row's
 * status. The Emacs tab-bar implements the same two words off the same message
 * — full is the whole `[N] <name>` entry in the status colour, partial is the
 * bracket alone — and a divergence between the two surfaces is a defect rather
 * than a taste difference.
 *
 * WHAT THIS DRAWS, AND WHAT IT LEAVES ALONE. In partial mode the row's NAME
 * text recedes to the muted colour and nothing else changes: the status dot
 * keeps its tone, the badges, the when-column and the highlight are untouched.
 * The mode says what the user has seen; it never says what the workspace is
 * doing, and the dot is the thing that says that.
 *
 * WHY A REGISTRY RATHER THAN A PURE READ OF THE WIRE. The daemon clears the
 * marker on any status change, and this end applies the SAME rule itself
 * rather than waiting to be told twice: a row whose status arm differs from
 * the one this page last drew is drawn FULL, marker or no marker. That is the
 * one deliberate piece of memory in an otherwise stateless renderer — it
 * remembers nothing but the arm it last drew per workspace, and it derives no
 * view from it, it only refuses to draw a stale one.
 *
 * WHY THE PASS IS BRACKETED. Both groupings are drawn on every push (one
 * hidden), so ONE workspace is drawn TWICE per pass and must resolve to the
 * same mode both times. The arms seen during a pass are therefore staged and
 * committed at `endPass`, never mid-draw, and a workspace that left the roster
 * is forgotten there — so a row that comes back is a first sight again, with
 * no arm to have "changed" from.
 */
import { log } from "../log.js";

/** The two modes, in the tab-bar's own words. */
export type ViewedMode = "full" | "partial";

/**
 * The status arms this page last drew, by `WorkspaceRef.id`.
 *
 * A drawing pass is bracketed: `beginPass()`, one `modeFor()` per row drawn,
 * then `endPass()`.
 */
export class ViewedRegistry {
  /** The arm each workspace was last drawn with, committed at `endPass`. */
  private readonly drawn = new Map<string, string>();

  /** The arms seen in the CURRENT pass, staged until it closes. */
  private pass = new Map<string, string>();

  /** Open a drawing pass. Anything staged by an abandoned pass is dropped. */
  beginPass(): void {
    this.pass = new Map();
  }

  /**
   * The mode ROW should draw in, and THE ONE PLACE either mode is decided.
   *
   * `viewed` is the wire's marker (present = the daemon holds this row
   * PARTIAL); `arm` is the row's status oneof case. A status change wins over
   * the marker in every case, with no exceptions: the daemon clears the marker
   * on exactly the same edge, so the two answers can only differ for the one
   * push where this page has already seen the new status and the marker has
   * not yet been dropped — and drawing PARTIAL there would be drawing a mode
   * the roster no longer means.
   *
   * Idempotent within a pass: the second grouping's copy of a row is handed
   * the same arm and resolves to the same mode, because the pass's arms are
   * staged rather than committed here.
   */
  modeFor(workspaceId: string, arm: string, viewed: boolean): ViewedMode {
    const previous = this.drawn.get(workspaceId);
    const changed = previous !== undefined && previous !== arm;
    this.pass.set(workspaceId, arm);
    if (changed) {
      log.debug("a row's status changed, so it is drawn FULL again", {
        operation: "sidebar.viewed.restored",
        context: { workspace: workspaceId, from: previous, to: arm, wire_viewed: viewed },
      });
      return "full";
    }
    if (!viewed) return "full";
    log.debug("a row is drawn PARTIAL: the user has already seen it", {
      operation: "sidebar.viewed.partial",
      context: { workspace: workspaceId, arm },
    });
    return "partial";
  }

  /** Close the pass: the arms it drew become the arms to compare against. */
  endPass(): void {
    this.drawn.clear();
    for (const [id, arm] of this.pass) this.drawn.set(id, arm);
    this.pass = new Map();
  }

  /** Forget everything — the rail is going away. */
  dispose(): void {
    this.drawn.clear();
    this.pass = new Map();
  }
}
