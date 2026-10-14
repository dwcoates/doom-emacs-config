/**
 * workspace-switch — the button on an async merge bubble's head that takes the
 * reader to the MERGE WORKSPACE the merge runs in
 * (`FeedMergeModeAsync.workspace_switch`). The daemon sets it once that workspace is
 * visible (revealed, or landed) and leaves it unset while the workspace is
 * hidden; the agentic tabs stream the merge workspace's turns, so they carry no
 * switch of their own. It draws two facts: the label the daemon composed, and
 * the workspace to switch to, absent once the daemon removed it after 15
 * calendar days idle, when the button is drawn disabled.
 *
 * THE CLICK IS AN ORDINARY WORKSPACE SWITCH: `SelectWorkspace`, through the
 * merge bubble's one jump (`jumpToWorkspace`), which revives a closed merge
 * workspace. It is a CONTROL, so a click on the head's button never folds the
 * bubble.
 */
import { createControl } from "../../control.js";
import { guardMalformed } from "../../rpc/guard.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { FeedMergeWorkspaceSwitch } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
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

/** What the switch draws: its label, and its target (null once removed). */
export interface SwitchFacts {
  readonly label: string;
  readonly target: WorkspaceRef | null;
}

/** The head's switch, read to its facts. */
export function switchFacts(sw: FeedMergeWorkspaceSwitch, path: string): SwitchFacts {
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
export function drawMergeWorkspaceSwitch(facts: SwitchFacts, rc: RowContext): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-workspace-switch merge-workspace-switch-head";
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
