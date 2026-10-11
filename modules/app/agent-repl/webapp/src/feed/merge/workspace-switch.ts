/**
 * workspace-switch — the button that takes the reader to the MERGE WORKSPACE a
 * detached merge runs in.
 *
 * Two elements of the requesting workspace's bubble carry it: the head
 * (`FeedMergeHead.workspace_switch`, for the bubble's whole life) and each
 * agentic tab (`FeedMergeTab*.workspace_switch`, in place of the rows that tab
 * never streams across workspaces). They are two components and two wire
 * messages, drawn by this one function from the same two facts: the label the
 * daemon composed, and the workspace to switch to, absent once the daemon
 * removed it after 15 calendar days idle, when the button is drawn disabled.
 *
 * THE CLICK IS AN ORDINARY WORKSPACE SWITCH: `SelectWorkspace`, through the
 * merge bubble's one jump (`jumpToWorkspace`), which revives a closed merge
 * workspace. It is a CONTROL, so a click on the head's button never folds the
 * bubble.
 */
import { createControl } from "../../control.js";
import { guardMalformed } from "../../rpc/guard.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedMergeTabWorkspaceSwitch,
  FeedMergeWorkspaceSwitch,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { jumpToWorkspace, type JumpSite } from "./jump.js";

/** Where a switch's jump is recorded and its refusal drawn. */
const SWITCH_JUMP: JumpSite = {
  describe: "a merge workspace switch",
  operation: "merge.workspace-switch",
  refusalClass: "merge-workspace-switch-refusal",
};

/** What either switch element draws: its label, and its target (null once removed). */
export interface SwitchFacts {
  readonly label: string;
  readonly target: WorkspaceRef | null;
}

/**
 * Either switch element, read to its facts. The head's and a tab's switch are
 * two messages of one shape, so they are read by one function.
 */
export function switchFacts(sw: FeedMergeWorkspaceSwitch | FeedMergeTabWorkspaceSwitch, path: string): SwitchFacts {
  const label = requireMessage(sw.label, `${path}.label`).text;
  const availability = requireCase(sw.availability, `${path}.availability`);
  switch (availability.case) {
    case "available":
      return { label, target: requireMessage(availability.value.target, `${path}.availability.available.target`) };
    case "removed":
      return { label, target: null };
    default:
      return unreachableArm(`${path}.availability`, armName(availability));
  }
}

/** The switch: a button that selects the merge workspace, disabled once it is gone. */
export function drawMergeWorkspaceSwitch(facts: SwitchFacts, rc: RowContext, placement: "head" | "tab"): HTMLElement {
  const el = document.createElement("span");
  el.className = `merge-workspace-switch merge-workspace-switch-${placement}`;
  const button = createControl();
  button.className = "merge-workspace-switch-button";
  button.textContent = facts.label;
  el.append(button);
  const target = facts.target;
  if (target === null) {
    el.setAttribute("data-removed", "");
    button.disabled = true;
    return el;
  }
  button.addEventListener("click", () => {
    void guardMalformed(rc.ctx, "feed.merge.workspace-switch", jumpToWorkspace(el, rc, target, SWITCH_JUMP));
  });
  return el;
}
