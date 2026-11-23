/**
 * view-change — the ONE way a gesture changes the sidebar's view state.
 *
 * Every view change — a fold or the grouping shown — is painted
 * on this page at once and held (`view.ts`), then asked of the daemon with
 * UpdateSidebarView, whose roster push carries it to every page. A refused or
 * failed ask releases the hold and repaints from the wire, and the verb runner
 * draws why beside the control.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  UpdateSidebarViewRequestSchema,
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewError,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { createLogger } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import type { ViewValue } from "./view.js";
import { fireVerb } from "./verbs.js";

const log = createLogger("webapp.sidebar.view-change");

/** One view change: the held piece, its asked value, and the request. */
export interface ViewChange<T extends ViewValue> {
  /** The `view.ts` key the change is held under. */
  key: string;
  /** The value painted at once and asked for. */
  value: T;
  /** The request's change arm. */
  change: MessageInitShape<typeof UpdateSidebarViewRequestSchema>["change"];
  /** The control the gesture landed on, where a refusal is drawn. */
  control: HTMLElement;
}

/** One held piece of a view change: its `view.ts` key and its asked value. */
export interface ViewPiece {
  key: string;
  value: ViewValue;
}

/**
 * A view change holding several pieces at once, which ONE request changes
 * together: a drag of a rule sizes the sections on both of its sides.
 */
export interface ViewChanges {
  /** Every piece painted at once and asked for. */
  pieces: readonly ViewPiece[];
  /** The request's change arm. */
  change: MessageInitShape<typeof UpdateSidebarViewRequestSchema>["change"];
  /** The control the gesture landed on, where a refusal is drawn. */
  control: HTMLElement;
}

/**
 * Paint, hold and ask. Answers once the ask has settled, with whether it
 * took; a failed ask has already been released and repainted.
 */
export async function changeView<T extends ViewValue>(
  sc: SidebarContext,
  spec: ViewChange<T>,
): Promise<boolean> {
  return changeViews(sc, { pieces: [{ key: spec.key, value: spec.value }], change: spec.change, control: spec.control });
}

/**
 * Paint and hold every piece, and ask for them in one request. Answers once
 * the ask has settled, with whether it took; a failed ask has already
 * released every piece and repainted it.
 */
export async function changeViews(sc: SidebarContext, spec: ViewChanges): Promise<boolean> {
  log.debug("changing the sidebar view for every page", {
    operation: "sidebar.view.change",
    context: { pieces: spec.pieces.map((piece) => `${piece.key}=${String(piece.value)}`).join(" "), change: spec.change?.case },
  });
  const tokens = spec.pieces.map((piece) => ({ key: piece.key, token: sc.view.ask(piece.key, piece.value) }));
  const took = await fireVerb(spec.control, {
    sc,
    rpc: "UpdateSidebarView",
    call: (client) => client.updateSidebarView(create(UpdateSidebarViewRequestSchema, { change: spec.change })),
    schema: UpdateSidebarViewResponseSchema,
    refusalText: (cause) => updateSidebarViewRefusal(cause as never),
  });
  if (!took) for (const { key, token } of tokens) sc.view.abandon(key, token);
  return took;
}

/**
 * UpdateSidebarView's arms of its own. `unknown_workspace` is one of the
 * cross-cutting four, worded once in `rpc/refusal.ts`.
 */
export function updateSidebarViewRefusal(
  cause: NonNullable<UpdateSidebarViewError["cause"]> & { case: string },
): string {
  switch (cause.case) {
    case "unknownRepository":
      return "the daemon no longer has that repository";
    case "unknownSubsection":
      return "the daemon no longer has that subsection";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("UpdateSidebarViewError.cause", other.case);
    }
  }
}
