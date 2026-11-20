/**
 * nest — a workspace row dropped onto another row becomes that workspace's
 * CHILD (agentrepl.v1 SetWorkspaceParent, the `child_of` arm).
 *
 * THE DROP IS THE RETARGET'S REQUEST, never a local reparent. Who a
 * workspace's parent is is durable daemon state, and the daemon tells the
 * retargeted workspace's agent in the same operation, so the gesture asks and
 * the roster's next push draws the new family. Where the workspace's merge
 * lands is left as it is (`landing_target` unset): its branch was cut from
 * there.
 *
 * THE DROP IS HELD LIKE AN ARRANGEMENT (`arrange.ts` `sendHeld`): the page
 * keeps the row where the drop put it until a push at or above the revision
 * the daemon answered (`SetWorkspaceParentSuccess.roster_revision`) arrives.
 */
import { create } from "@bufbuild/protobuf";
import {
  SetWorkspaceParentRequestSchema,
  SetWorkspaceParentResponseSchema,
  type SetWorkspaceParentError,
  type SetWorkspaceParentRequest,
  type SetWorkspaceParentResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_parent_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { log } from "../log.js";
import { parentNotOpenSentence } from "../rpc/family-refusal.js";
import { unreachableArm } from "../rpc/strict.js";
import { sendHeld, successRevision } from "./arrange.js";
import type { SidebarContext } from "./context.js";
import type { CauseOf } from "./verbs.js";

/** SetWorkspaceParent making WORKSPACE a child of PARENT, its landing kept. */
export function buildNestRequest(workspace: WorkspaceRef, parent: WorkspaceRef): SetWorkspaceParentRequest {
  return create(SetWorkspaceParentRequestSchema, {
    workspace,
    parent: { case: "childOf", value: { parent } },
  });
}

/**
 * SetWorkspaceParent's arms of its own. `unknown_workspace`,
 * `workspace_ref_mismatch`, `transferring_away` and `not_yet_adopted` are the
 * cross-cutting four, worded once in `rpc/refusal.ts`.
 */
export function setWorkspaceParentRefusal(cause: CauseOf<SetWorkspaceParentError>): string {
  switch (cause.case) {
    case "unknownParent":
      return "the daemon no longer has the workspace it was dropped onto";
    case "parentRefMismatch":
      return `the new parent's directory disagrees with the registry's: ${cause.value.registryDir}`;
    case "cycle":
      return "a workspace cannot become a child of itself or of one of its own children";
    case "noMergeLayout":
      return "this workspace has no merge layout, so it cannot be nested under another";
    case "coldGate":
      return "this workspace waits at its cold gate; answer the gate, then nest it";
    case "parentNotOpen":
      return parentNotOpenSentence(cause.value, "SetWorkspaceParentError.parent_not_open", "nesting");
    case "unknownLandingTarget":
      return "the workspace it lands into cannot take its merge";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("SetWorkspaceParentError.cause", other.case);
    }
  }
}

/**
 * Ask the daemon to make WORKSPACE a child of PARENT, with CONTROL the element
 * a refusal is drawn beside, holding the page's drawing until the answered
 * revision arrives (`sendHeld`). Answers whether the change took.
 */
export async function sendNest(
  sc: SidebarContext,
  control: HTMLElement,
  workspace: WorkspaceRef,
  parent: WorkspaceRef,
): Promise<boolean> {
  const context = { workspace: workspace.id, parent: parent.id };
  log.info("nesting a workspace under the one it was dropped onto", { operation: "sidebar.nest.send", context });
  return sendHeld<SetWorkspaceParentResponse>(sc, control, {
    rpc: "SetWorkspaceParent",
    call: (client) => client.setWorkspaceParent(buildNestRequest(workspace, parent)),
    schema: SetWorkspaceParentResponseSchema,
    refusalText: (cause) => setWorkspaceParentRefusal(cause as CauseOf<SetWorkspaceParentError>),
    revision: successRevision,
    context,
  });
}
