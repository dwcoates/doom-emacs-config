/**
 * family-refusal — the words for the two refusals that keep a workspace
 * family in one list of the roster (agentrepl.v1 family.proto).
 *
 * A child workspace is coordinated by its parent and is drawn and tabbed right
 * after it, and a workspace landing into another needs that branch to stay, so
 * the daemon refuses taking a parent out of the Open section while a child or a
 * workspace landing into it is open (`WorkspaceHasOpenChildren`), and refuses making an open child
 * of a parent that is hidden or closed (`WorkspaceParentNotOpen`). Every verb
 * that can be refused either way says it in these words, so the sidebar's
 * menu and the feed's cold gate cannot word one refusal two ways.
 */
import type {
  WorkspaceHasOpenChildren,
  WorkspaceParentNotOpen,
} from "../../../proto/gen/ts/agentrepl/v1/family_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "./malformed.js";
import { requireCase, requireMessage, unreachableArm } from "./strict.js";

/** How a workspace is named in a refusal: its directory's last part, else its id. */
function label(ref: WorkspaceRef): string {
  const base = ref.dir
    .split("/")
    .filter((part) => part !== "")
    .pop();
  return base ?? ref.id;
}

/**
 * The open children's and the open workspaces landing into it's refs, at PATH,
 * children first. The contract says there is always one.
 */
function openRefs(u: WorkspaceHasOpenChildren, path: string): WorkspaceRef[] {
  const open = [...u.children, ...u.landingInto];
  if (open.length === 0)
    throw new MalformedView(
      `${path}.children`,
      "a has_open_children refusal names no child and no workspace landing into it",
    );
  return open;
}

/**
 * GESTURE ("close", "hide", "kill", "nuke") was refused because the workspace
 * still has open children or open workspaces landing into it (U, at PATH): they
 * are named, and closing them first is what makes the gesture possible.
 */
export function hasOpenChildrenSentence(
  u: WorkspaceHasOpenChildren,
  path: string,
  gesture: string,
): string {
  const open = openRefs(u, path);
  const one = open.length === 1;
  const where =
    u.landingInto.length > 0
      ? "under or landing into this workspace"
      : "under this workspace";
  return `${gesture} refused: ${open.map(label).join(", ")} ${one ? "is" : "are"} still open ${where}; close ${one ? "it" : "them"} first`;
}

/**
 * A merge that would close a workspace with open children (U, at PATH) was
 * refused: every open child is to be resolved and closed before a one-shot
 * merge is asked for.
 */
export function mergeHasOpenChildrenSentence(
  u: WorkspaceHasOpenChildren,
  path: string,
): string {
  const parent = label(requireMessage(u.parent, `${path}.parent`));
  const what =
    u.landingInto.length > 0
      ? "workspaces under or landing into it"
      : "child workspaces";
  return `${parent} still has open ${what} (${openRefs(u, path).map(label).join(", ")}); resolve and close all of them before a one-shot merge`;
}

/**
 * GESTURE ("reopen", "show", "create a child of") was refused because the
 * parent (U, at PATH) is hidden, closed, or closing under a queued merge:
 * showing or reopening the parent first is what makes the gesture possible,
 * and a closing parent's merge is what stands in the way.
 */
export function parentNotOpenSentence(
  u: WorkspaceParentNotOpen,
  path: string,
  gesture: string,
): string {
  const parent = label(requireMessage(u.parent, `${path}.parent`));
  const state = requireCase(u.state, `${path}.state`);
  switch (state.case) {
    case "hidden":
      return `${gesture} refused: its parent ${parent} is hidden; show the parent first`;
    case "closed":
      return `${gesture} refused: its parent ${parent} is closed; reopen the parent first`;
    case "closing":
      return `${gesture} refused: its parent ${parent} closes once its queued merge lands`;
    default: {
      const other: { case: string } = state;
      return unreachableArm(`${path}.state`, other.case);
    }
  }
}
