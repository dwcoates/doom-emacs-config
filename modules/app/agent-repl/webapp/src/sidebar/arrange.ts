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
 * its drawing (`ArrangeHold`) from the moment a drag starts until a push at or
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
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { log } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
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
  return sendHeld(sc, control, {
    rpc: "ArrangeSidebar",
    call: (client) => client.arrangeSidebar(buildArrangeSidebarRequest(change)),
    schema: ArrangeSidebarResponseSchema,
    refusalText: (cause) => arrangeSidebarRefusal(cause as never),
    revision: (response: ArrangeSidebarResponse) =>
      response.result.case === "success" ? response.result.value.rosterRevision : null,
  });
}

/**
 * A verb that changes what the sidebar draws, and whose success answers the
 * revision of the roster that carries the change (REVISION, null for a
 * response that names none).
 */
export interface HeldVerb<Res extends VerbResponse> extends Omit<VerbCall<Res>, "sc" | "outlivesPush" | "took"> {
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
    outlivesPush: true,
    took: (response: Res) => {
      revision = verb.revision(response);
    },
  });
  sc.arrange.hold.settle(took ? revision : null);
  return took;
}

/**
 * The page's hold on its own drawing while an arrangement is dragged or
 * asked for.
 *
 * A push offered while the hold stands is kept (the newest only, since a
 * roster is always whole) and drawn when the hold lets go: at once when the
 * hold is released, or when a push at or above the awaited revision arrives.
 */
export interface ArrangeHold {
  /** Draw ROSTER now, or keep it for when the hold lets go. */
  offer(roster: WorkspaceRoster): void;
  /** A drag started: hold every push until it ends. */
  drag(): void;
  /** A drag ended with nothing asked: let go. */
  cancel(): void;
  /** An ask is in flight: keep holding until it settles. */
  await(): void;
  /**
   * The ask settled. REVISION is the roster revision the change arrived at,
   * or null when the ask did not take (or there is nothing to wait for): the
   * hold then lets go at once.
   */
  settle(revision: bigint | null): void;
  /** The stream run ended: an awaited revision is no longer coming. */
  reset(): void;
  /** Whether the drawing is held right now. */
  holding(): boolean;
}

/**
 * The hold, drawing through DRAW.
 *
 * LETTING GO ALWAYS DRAWS, the newest roster held back or else the one last
 * drawn: a drag rearranged the page's own DOM, and a drawing the daemon did
 * not take (a refusal, a cancelled drag) must not outlive the hold. Drawing a
 * roster again is what every push does anyway.
 */
export function createArrangeHold(draw: (roster: WorkspaceRoster) => void): ArrangeHold {
  type State = { case: "free" } | { case: "held" } | { case: "awaiting"; revision: bigint };
  let state: State = { case: "free" };
  /** The newest roster offered while held, null when none was. */
  let kept: WorkspaceRoster | null = null;
  /** The roster last drawn, null before the first. */
  let drawn: WorkspaceRoster | null = null;

  const drawNow = (roster: WorkspaceRoster): void => {
    drawn = roster;
    draw(roster);
  };

  const letGo = (why: string): void => {
    state = { case: "free" };
    const roster = kept ?? drawn;
    kept = null;
    log.debug("the sidebar's arrangement hold let go", {
      operation: "sidebar.arrange.hold-released",
      context: { why, revision: roster === null ? null : String(roster.revision) },
    });
    if (roster !== null) drawNow(roster);
  };

  return {
    offer(roster) {
      if (state.case === "free") {
        drawNow(roster);
        return;
      }
      kept = roster;
      if (state.case === "awaiting" && roster.revision >= state.revision) letGo("a push carried the arrangement");
    },
    drag() {
      state = { case: "held" };
    },
    cancel() {
      letGo("the drag was cancelled");
    },
    await() {
      state = { case: "held" };
    },
    settle(revision) {
      if (revision === null) {
        letGo("the arrangement did not take");
        return;
      }
      // A CHANGE THAT LEFT THE ROSTER AS IT WAS publishes no new push (the
      // daemon drops a duplicate), so a revision already drawn or held is
      // the arrangement arriving, not one still to come.
      if ((drawn !== null && drawn.revision >= revision) || (kept !== null && kept.revision >= revision)) {
        letGo("a push already carried the arrangement");
        return;
      }
      state = { case: "awaiting", revision };
    },
    reset() {
      // A DRAG IN PROGRESS KEEPS ITS HOLD: the dragged element is in the DOM
      // the next push would throw away. An awaited revision belongs to the
      // run that ended, and a successor daemon numbers its own.
      if (state.case === "awaiting") letGo("the roster stream ended");
    },
    holding: () => state.case !== "free",
  };
}
