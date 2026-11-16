/**
 * children-fold — a workspace row's children fold, asked from the row's status
 * mark.
 *
 * It is ONE MORE ACCESS POINT TO A FOLD THE DAEMON HOLDS, with the effects a
 * subsection's fold has (`section-fold.ts`): a collapsed row's children are
 * hidden in the sidebar, their tabs are folded off the Emacs tab bar, and the
 * daemon stands their sessions down. So it folds through the same gesture
 * (`sectionFold`) and the same tracking (`trackDaemonFold`) every section and
 * subsection does. UNLIKE THEIRS, A COLLAPSE OVER A CURRENT CHILD IS ASKED
 * (owner request, 2026-11-16): the daemon selects the parent and folds, so the
 * row never carries `HOLDS_CURRENT_ATTRIBUTE` and the gesture never refuses.
 */
import type { RosterOpenRowChildrenFold } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { workspaceChildrenFoldKey, type SidebarContext } from "./context.js";
import { sectionFold, trackDaemonFold } from "./section-fold.js";

/** The attribute a row's status mark carries while it folds the row's children. */
export const CHILDREN_FOLD_CONTROL_ATTRIBUTE = "data-children-fold";

/** What the wire says of a row's children's fold. */
export interface DrawnChildrenFold {
  collapsed: boolean;
}

/** Read FOLD at PATH. EVERY arm is named; an unset fold or collapse is malformed. */
export function drawRosterChildrenFold(fold: RosterOpenRowChildrenFold, path: string): DrawnChildrenFold {
  const arm = requireCase(fold.fold, `${path}.fold`);
  switch (arm.case) {
    case "collapsed":
      return { collapsed: true };
    case "expanded": {
      const collapse = requireCase(arm.value.collapse, `${path}.fold.expanded.collapse`);
      switch (collapse.case) {
        case "collapsible":
          return { collapsed: false };
        case "holdsCurrent":
          // The daemon still says a current child is below, but the collapse is asked anyway: it selects the parent.
          return { collapsed: false };
        default: {
          const other: { case: string } = collapse;
          return unreachableArm(`${path}.fold.expanded.collapse`, other.case);
        }
      }
    }
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.fold`, other.case);
    }
  }
}

/**
 * Fold ROW's children as FOLD says, unless this page's own ask is still in
 * flight, and make MARK, the row's status mark, the control that asks for the
 * other fold. A click on MARK folds and selects nothing.
 */
export function wireChildrenFold(
  row: HTMLElement,
  mark: HTMLElement,
  sc: SidebarContext,
  workspace: WorkspaceRef,
  fold: RosterOpenRowChildrenFold,
  path: string,
): void {
  const drawn = drawRosterChildrenFold(fold, path);
  const foldKey = workspaceChildrenFoldKey(workspace.id);
  trackDaemonFold(row, foldKey, drawn.collapsed, sc);
  mark.setAttribute(CHILDREN_FOLD_CONTROL_ATTRIBUTE, "");
  const gesture = sectionFold(sc, foldKey, { case: "workspace", value: workspace });
  mark.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    gesture(row, mark);
  });
}
