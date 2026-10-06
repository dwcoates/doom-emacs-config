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
import { log } from "../log.js";
import { unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import type { ViewValue } from "./view.js";
import { fireVerb } from "./verbs.js";

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
  /** True for a control drawn once, outside what a push replaces. */
  outlivesPush?: boolean;
}

/**
 * Paint, hold and ask. Answers once the ask has settled, with whether it
 * took; a failed ask has already been released and repainted.
 */
export async function changeView<T extends ViewValue>(
  sc: SidebarContext,
  spec: ViewChange<T>,
): Promise<boolean> {
  log.debug("changing the sidebar view for every page", {
    operation: "sidebar.view.change",
    context: { key: spec.key, value: spec.value, change: spec.change?.case },
  });
  const token = sc.view.ask(spec.key, spec.value);
  const took = await fireVerb(spec.control, {
    sc,
    rpc: "UpdateSidebarView",
    call: (client) => client.updateSidebarView(create(UpdateSidebarViewRequestSchema, { change: spec.change })),
    schema: UpdateSidebarViewResponseSchema,
    refusalText: (cause) => updateSidebarViewRefusal(cause as never),
    outlivesPush: spec.outlivesPush,
  });
  if (!took) sc.view.abandon(spec.key, token);
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
    case "unknownTask":
      return "the daemon no longer has that task";
    default: {
      const other: { case: string } = cause;
      return unreachableArm("UpdateSidebarViewError.cause", other.case);
    }
  }
}
