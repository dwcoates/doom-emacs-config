/**
 * family-refusal — the words for the two refusals that keep a workspace
 * family in one list of the roster (agentrepl.v1 family.proto).
 *
 * A child workspace merges into its parent and is drawn and tabbed right after
 * it, so the daemon refuses taking a parent out of the Open section while a
 * child is open (`WorkspaceHasOpenChildren`), and refuses making an open child
 * of a parent that is hidden or closed (`WorkspaceParentNotOpen`). Every verb
 * that can be refused either way says it in these words, so the sidebar's
 * menu and the feed's cold gate cannot word one refusal two ways.
 */
import type { WorkspaceHasOpenChildren, WorkspaceParentNotOpen } from "../../../proto/gen/ts/agentrepl/v1/family_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "./malformed.js";
import { requireCase, requireMessage, unreachableArm } from "./strict.js";

/** How a workspace is named in a refusal: its directory's last part, else its id. */
function label(ref: WorkspaceRef): string {
  const base = ref.dir.split("/").filter((part) => part !== "").pop();
  return base ?? ref.id;
}

/** The open children's labels, at PATH. The contract says there is always one. */
function childLabels(u: WorkspaceHasOpenChildren, path: string): string {
  if (u.children.length === 0) throw new MalformedView(`${path}.children`, "a has_open_children refusal names no child");
  return u.children.map(label).join(", ");
}

/**
 * GESTURE ("close", "hide", "kill", "nuke") was refused because the workspace
 * still has open children (U, at PATH): they are named, and closing them first
 * is what makes the gesture possible.
 */
export function hasOpenChildrenSentence(u: WorkspaceHasOpenChildren, path: string, gesture: string): string {
  const children = childLabels(u, path);
  const one = u.children.length === 1;
  return `${gesture} refused: ${children} ${one ? "is" : "are"} still open under this workspace; close ${one ? "it" : "them"} first`;
}

/**
 * A merge that would close a workspace with open children (U, at PATH) was
 * refused: every open child is to be resolved and closed before a one-shot
 * merge is asked for.
 */
export function mergeHasOpenChildrenSentence(u: WorkspaceHasOpenChildren, path: string): string {
  const parent = label(requireMessage(u.parent, `${path}.parent`));
  return `${parent} still has open child workspaces (${childLabels(u, path)}); resolve and close all of them before a one-shot merge`;
}

/**
 * GESTURE ("reopen", "show", "create a child of") was refused because the
 * parent (U, at PATH) is hidden, closed, or closing under a queued merge:
 * showing or reopening the parent first is what makes the gesture possible,
 * and a closing parent's merge is what stands in the way.
 */
export function parentNotOpenSentence(u: WorkspaceParentNotOpen, path: string, gesture: string): string {
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
