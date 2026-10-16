/**
 * jump — a merge bubble's cross-workspace jump: `SelectWorkspace`, echoed
 * verbatim, answered at the element that was clicked.
 *
 * A cross-workspace click is `SelectWorkspace` and nothing else (R8): the
 * editor switches, the roster stream reflects it, and the bubble draws
 * nothing on success. The refusal is the click's own answer, so it lands AT
 * THE CLICKED ELEMENT, with the four cross-cutting causes worded by the shared
 * `crossCuttingSentence` so a transferring daemon reads the same here as it
 * does in the rail. A merge queue entry and a merge workspace's switch are the
 * two jumps; each names itself through its `JumpSite`.
 *
 * A MALFORMED VIEW IS NOT A TRANSPORT FAILURE and travels up loudly rather
 * than being drawn as "could not be reached", which would state the wrong
 * thing about the daemon.
 */
import { create } from "@bufbuild/protobuf";
import { log } from "../../log.js";
import { isMalformedView } from "../../rpc/malformed.js";
import { crossCuttingSentence } from "../../rpc/refuse.js";
import { SELECT_WORKSPACE_EXPECTED_ARMS } from "../../rpc/refusal.js";
import { requireCase, unreachableArm } from "../../rpc/strict.js";
import { callUnary } from "../../rpc/unary.js";
import {
  SelectWorkspaceRequestSchema,
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { childrenMatching } from "../../dom.js";

/** Where one kind of jump is recorded and how its refusal is drawn. */
export interface JumpSite {
  /** What was clicked, for the record ("a merge queue entry"). */
  readonly describe: string;
  /** The record's operation; its answers append "-expected-answer" and "-refused". */
  readonly operation: string;
  /** The class the refusal drawn at the clicked element wears. */
  readonly refusalClass: string;
}

/** Select WORKSPACE from HOST, drawing a refusal inside HOST. */
export async function jumpToWorkspace(
  host: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
  site: JumpSite,
): Promise<void> {
  for (const stale of childrenMatching(host, `.${site.refusalClass}`)) stale.remove();
  log.info(`selecting a workspace from ${site.describe}`, {
    operation: site.operation,
    context: { workspace: workspace.id },
  });
  let response: SelectWorkspaceResponse;
  try {
    response = await callUnary(
      rc.ctx,
      "SelectWorkspace",
      (client) => client.selectWorkspace(create(SelectWorkspaceRequestSchema, { workspace })),
      SelectWorkspaceResponseSchema,
    );
  } catch (err) {
    if (isMalformedView(err)) throw err;
    host.append(drawRefusal(site, "transport", "the daemon could not be reached"));
    return;
  }
  const result = requireCase(response.result, "SelectWorkspaceResponse.result");
  switch (result.case) {
    case "success":
      // Nothing is drawn: the roster stream carries the new `current`.
      return;
    case "error": {
      const cause = requireCase(result.value.cause, "SelectWorkspaceError.cause");
      const expected = SELECT_WORKSPACE_EXPECTED_ARMS[cause.case];
      if (expected !== undefined) {
        // An expected answer, not a refusal: nothing is drawn at the element.
        log.info(`SelectWorkspace was answered ${cause.case}: ${expected}`, {
          operation: `${site.operation}-expected-answer`,
          context: { arm: cause.case, workspace: workspace.id },
        });
        return;
      }
      const sentence =
        crossCuttingSentence("SelectWorkspace", cause) ?? "that workspace could not be selected";
      log.warn(`SelectWorkspace was refused: ${sentence}`, {
        operation: `${site.operation}-refused`,
        context: { arm: cause.case, workspace: workspace.id },
      });
      host.append(drawRefusal(site, cause.case, sentence));
      return;
    }
    default:
      return unreachableArm("SelectWorkspaceResponse.result", armName(result));
  }
}

/** The refusal, inside the element that made the call. */
function drawRefusal(site: JumpSite, arm: string, text: string): HTMLElement {
  const el = document.createElement("div");
  el.className = `refusal ${site.refusalClass}`;
  el.setAttribute("data-arm", arm);
  el.textContent = text;
  return el;
}
