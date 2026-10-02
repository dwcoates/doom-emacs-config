/**
 * selection-edge — THE MOMENT THIS PAGE'S WORKSPACE BECOMES THE SELECTED ONE.
 *
 * Every way of switching workspaces — Emacs's tab chords and pickers, a
 * sidebar row click in any webview, a merge-queue entry — is the one
 * `SelectWorkspace` verb, and the daemon states its result as
 * `WorkspaceRoster.current` (`frontend.v1.RosterCurrentWorkspace`). A switch
 * TO this page's workspace is therefore exactly one edge of that field: from
 * naming another workspace, or none, to naming this one. No switch path can
 * reach the workspace without crossing it, and nothing else crosses it.
 *
 * WHAT IS NOT A SWITCH:
 * - Re-selecting the current workspace changes no `current` (Emacs re-asserts
 *   after a sidebar click, a reconnect or a relink), so the field restates
 *   itself and no edge fires.
 * - Coming back to Emacs from another application changes nothing the daemon
 *   holds, so no push arrives at all.
 * - The FIRST push of a run is a BASELINE, not an edge: the page cannot know
 *   what `current` was before it was watching, and a feed's first paint is
 *   placed by its own cause (`initialPlacement`). A run that ends forgets its
 *   baseline (`reset`), so a reconnect restating `current` fires nothing.
 */
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";

/** What the roster stream feeds the edge. */
export interface SelectionEdge {
  /** Observe one whole roster push. */
  observe(roster: WorkspaceRoster): void;
  /** Forget the baseline: the run that carried it ended. */
  reset(): void;
}

/** The baseline before any push of the current run. */
const UNOBSERVED = Symbol("unobserved");

/**
 * Watch `WorkspaceRoster.current` for the edge into OWN (this page's
 * workspace id), calling ARRIVED once per edge.
 */
export function createSelectionEdge(own: string, arrived: () => void): SelectionEdge {
  let previous: string | null | typeof UNOBSERVED = UNOBSERVED;
  return {
    observe(roster: WorkspaceRoster): void {
      const current =
        roster.current === undefined
          ? null
          : requireMessage(roster.current.workspace, "WorkspaceRoster.current.workspace").id;
      const before = previous;
      previous = current;
      if (before === UNOBSERVED || current !== own || before === own) return;
      log.info("this workspace was switched to; the feed returns to its tail", {
        operation: "sidebar.workspace-selected",
        context: { workspace: own, from: before },
      });
      arrived();
    },
    reset(): void {
      previous = UNOBSERVED;
    },
  };
}
