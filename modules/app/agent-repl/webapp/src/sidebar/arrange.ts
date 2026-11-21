/**
 * arrange — how the repository grouping is ARRANGED, and the one way a
 * gesture changes it (agentrepl.v1 ArrangeSidebar).
 *
 * THE ARRANGEMENT IS THE DAEMON'S AND THE TAB BAR'S. Repositories, a
 * repository's subsections, and which subsection a workspace sits in are
 * durable daemon state, and the Emacs tab bar lists the open workspaces in
 * the same order the repository grouping draws them. So a drag here is a
 * request, never a local reorder that could drift from the bar.
 *
 * THE GESTURE IS INSTANT AND HELD. A drag moves the dragged element in this
 * page's DOM as it goes, so the drop already shows the new order. Every
 * roster push is a whole new drawing, though, and drawing one that predates
 * the change would put the moved item back for an instant. So the page HOLDS
 * its drawing (`ArrangeHold`, the shared `hold.ts`) from the moment a drag starts until a push at or
 * above the revision the daemon answered with (`ArrangeSidebarSuccess
 * .roster_revision`) arrives. A refused or failed ask, a cancelled drag, or
 * the stream ending releases the hold at once, and the newest push held back
 * is drawn.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  ArrangeSidebarRequestSchema,
  ArrangeSidebarResponseSchema,
  type ArrangeSidebarError,
  type ArrangeSidebarRequest,
  type ArrangeSidebarResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_arrange_sidebar_pb";
import { createDrawingHold } from "../hold.js";
import { log } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";
import type { ArrangeHold, SidebarContext } from "./context.js";
import type { RosterStreams } from "./roster-streams.js";
import { fireVerb, type VerbCall, type VerbResponse } from "./verbs.js";

/** One ArrangeSidebar change: the request's change arm. */
export type ArrangeChange = MessageInitShape<typeof ArrangeSidebarRequestSchema>["change"];

/** ArrangeSidebar, built from one change. */
export function buildArrangeSidebarRequest(change: ArrangeChange): ArrangeSidebarRequest {
  return create(ArrangeSidebarRequestSchema, { change });
}

/**
 * ArrangeSidebar's arms of its own. `unknown_workspace`,
 * `workspace_ref_mismatch`, `transferring_away` and `not_yet_adopted` are the
 * cross-cutting four, worded once in `rpc/refusal.ts`.
 */
export function arrangeSidebarRefusal(
  cause: NonNullable<ArrangeSidebarError["cause"]> & { case: string },
): string {
  switch (cause.case) {
    case "unknownRepository":
      return "the daemon no longer has that repository";
    case "unknownSubsection":
      return "the daemon no longer has that subsection";
    case "blankName":
      return "a subsection needs a name";
    case "otherRepository":
      return "a workspace can only move within its own repository";
    case "anchorElsewhere":
      return "the sidebar changed under the drag; try again";
    case "noOpenWorkspace":
      return "a subsection an agent creates needs an open workspace";
    case "mainWorkspaceNotSortable":
      return "the main workspace is arranged by hand, never sorted by an agent";
    case "noRepositoryBlock":
      return "the sidebar changed under the drag; try again";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("ArrangeSidebarError.cause", other.case);
    }
  }
}

/**
 * Ask the daemon for one arrangement change, with CONTROL the element a
 * refusal is drawn beside. While the ask is in flight the page's drawing is
 * held (`ArrangeHold.await`), and the answer settles the hold. Answers whether
 * the change took.
 */
export async function sendArrangement(sc: SidebarContext, control: HTMLElement, change: ArrangeChange): Promise<boolean> {
  log.info("arranging the sidebar", {
    operation: "sidebar.arrange.send",
    context: { change: change?.case },
  });
  return sendHeld<ArrangeSidebarResponse>(sc, control, {
    rpc: "ArrangeSidebar",
    call: (client) => client.arrangeSidebar(buildArrangeSidebarRequest(change)),
    schema: ArrangeSidebarResponseSchema,
    refusalText: (cause) => arrangeSidebarRefusal(cause as never),
    revision: successRevision,
  });
}

/** A held verb's response whose success names the roster revision carrying its change. */
export interface RevisionedResponse {
  result:
    | { case: "success"; value: { rosterRevision: bigint } }
    | { case: "error"; value: unknown }
    | { case: undefined; value?: undefined };
}

/**
 * The roster revision RESPONSE's success names, null for a refusal: THE ONE
 * reading of `roster_revision` every held verb whose success carries one
 * hands `sendHeld`.
 */
export function successRevision(response: RevisionedResponse): bigint | null {
  return response.result.case === "success" ? response.result.value.rosterRevision : null;
}

/**
 * A verb that changes what the sidebar draws, and whose success answers the
 * revision of the roster that carries the change (REVISION, null for a
 * response that names none).
 */
export interface HeldVerb<Res extends VerbResponse> extends Omit<VerbCall<Res>, "sc" | "idempotent" | "took"> {
  revision: (response: Res) => bigint | null;
}

/**
 * THE ONE WAY A GESTURE THAT REARRANGES THE SIDEBAR ASKS THE DAEMON: VERB,
 * with CONTROL the element a refusal is drawn beside. While the ask is in
 * flight the page's drawing is held (`ArrangeHold.await`), and the answer
 * settles the hold: at the revision a success names, at once otherwise.
 * Answers whether the change took.
 */
export async function sendHeld<Res extends VerbResponse>(
  sc: SidebarContext,
  control: HTMLElement,
  verb: HeldVerb<Res>,
): Promise<boolean> {
  sc.arrange.hold.await();
  let revision: bigint | null = null;
  const took = await fireVerb(control, {
    ...verb,
    sc,
    took: (response: Res) => {
      revision = verb.revision(response);
    },
  });
  sc.arrange.hold.settle(took ? revision : null);
  return took;
}


/**
 * The hold, drawing through DRAW, letting go at the roster revision an
 * arrangement answered (`ArrangeSidebarSuccess.roster_revision`), which is
 * compared against the PUSH's revision (`RosterPush.revision`): holding every
 * stream of the push at R is holding the roster as of R.
 */
export function createArrangeHold(draw: (streams: RosterStreams) => void): ArrangeHold {
  return createDrawingHold({ draw, revisionOf: (streams) => streams.revision, operation: "sidebar.arrange.hold-released" });
}
