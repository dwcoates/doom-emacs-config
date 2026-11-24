/**
 * nest — a workspace row dropped onto another row becomes that workspace's
 * CHILD (agentrepl.v1 SetWorkspaceParent, the `child_of` arm), and a nested
 * row dropped among the top-level rows becomes TOP-LEVEL there (the
 * `top_level` arm, then the placement the drop asked for).
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
 *
 * A DROP AT THE TOP LEVEL IS TWO ASKS UNDER ONE HOLD (`sendHeldSteps`). The
 * retarget leaves the freed workspace where its old family was filed, and
 * only a placement can stand it where it was dropped. The placement is asked
 * only once the retarget took: asked before it, it would place the family's
 * root, the old parent, instead. A refused placement leaves the workspace
 * top-level where the daemon filed it, its refusal drawn.
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
import { createLogger } from "../log.js";
import { parentNotOpenSentence } from "../rpc/family-refusal.js";
import { unreachableArm } from "../rpc/strict.js";
import { arrangementVerb, heldStep, sendHeldSteps, successRevision, type ArrangeChange, type HeldVerb } from "./arrange.js";
import type { SidebarContext } from "./context.js";
import type { CauseOf } from "./verbs.js";

const log = createLogger("webapp.sidebar.nest");

/** SetWorkspaceParent making WORKSPACE a child of PARENT, its landing kept. */
export function buildNestRequest(workspace: WorkspaceRef, parent: WorkspaceRef): SetWorkspaceParentRequest {
  return create(SetWorkspaceParentRequestSchema, {
    workspace,
    parent: { case: "childOf", value: { parent } },
  });
}

/** SetWorkspaceParent making WORKSPACE top-level, its landing kept. */
export function buildUnnestRequest(workspace: WorkspaceRef): SetWorkspaceParentRequest {
  return create(SetWorkspaceParentRequestSchema, {
    workspace,
    parent: { case: "topLevel", value: {} },
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

/** The held verb asking SetWorkspaceParent for REQUEST, its refusal recorded with CONTEXT. */
function parentVerb(request: SetWorkspaceParentRequest, context: Record<string, unknown>): HeldVerb<SetWorkspaceParentResponse> {
  return {
    rpc: "SetWorkspaceParent",
    call: (client) => client.setWorkspaceParent(request),
    schema: SetWorkspaceParentResponseSchema,
    refusalText: (cause) => setWorkspaceParentRefusal(cause as CauseOf<SetWorkspaceParentError>),
    revision: successRevision,
    context,
  };
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
  return sendHeldSteps(sc, control, [heldStep(parentVerb(buildNestRequest(workspace, parent), context))]);
}

/**
 * Ask the daemon to make WORKSPACE top-level and then to stand it as
 * PLACEMENT says, a `placeWorkspace` of WORKSPACE itself, under one hold (see
 * the file comment), with CONTROL the element a refusal is drawn beside.
 * Answers whether both took.
 */
export async function sendUnnest(
  sc: SidebarContext,
  control: HTMLElement,
  workspace: WorkspaceRef,
  placement: ArrangeChange,
): Promise<boolean> {
  if (placement?.case !== "placeWorkspace" || placement.value.workspace?.id !== workspace.id) {
    throw new Error("invariant: a workspace made top-level is placed by a placement of itself");
  }
  const context = { workspace: workspace.id, into: placement.value.into?.case, at: placement.value.at?.case };
  log.info("making a nested workspace top-level where it was dropped", { operation: "sidebar.nest.unnest", context });
  return sendHeldSteps(sc, control, [
    heldStep(parentVerb(buildUnnestRequest(workspace), context)),
    heldStep(arrangementVerb(placement)),
  ]);
}
