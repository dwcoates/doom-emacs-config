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
 * two jumps; each names itself through its `JumpSite`. A replay header's
 * target is a third, and it OPENS the workspace first (`openAndJumpToWorkspace`):
 * the target checkout's workspace may be closed, and selecting a closed
 * workspace brings nothing up. An open of a workspace already up reconciles
 * and starts nothing.
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
  OpenWorkspaceRequestSchema,
  OpenWorkspaceResponseSchema,
  type OpenWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import {
  SelectWorkspaceRequestSchema,
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FeedId } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { childrenMatching } from "../../dom.js";
import { openWorkspaceRefusal } from "../../sidebar/verbs.js";

/** Where one kind of jump is recorded and how its refusal is drawn. */
export interface JumpSite {
  /** What was clicked, for the record ("a merge queue entry"). */
  readonly describe: string;
  /** The record's operation; its answers append "-expected-answer" and "-refused". */
  readonly operation: string;
  /** The class the refusal drawn at the clicked element wears. */
  readonly refusalClass: string;
}

/**
 * Select WORKSPACE from HOST, drawing a refusal inside HOST. REVEAL, when
 * given, is a row of the workspace's root feed its page brings on screen and
 * opens once it is selected (`SelectWorkspaceRequest.reveal`).
 */
export async function jumpToWorkspace(
  host: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
  site: JumpSite,
  reveal?: FeedId,
): Promise<void> {
  for (const stale of childrenMatching(host, `.${site.refusalClass}`)) stale.remove();
  log.info(`selecting a workspace from ${site.describe}`, {
    operation: site.operation,
    context: { workspace: workspace.id, reveal: reveal?.value ?? "none" },
  });
  let response: SelectWorkspaceResponse;
  try {
    response = await callUnary(
      rc.ctx,
      "SelectWorkspace",
      (client) =>
        client.selectWorkspace(
          create(SelectWorkspaceRequestSchema, reveal === undefined ? { workspace } : { workspace, reveal }),
        ),
      SelectWorkspaceResponseSchema,
    );
  } catch (err) {
    refuseTransport(host, site, "SelectWorkspace", workspace, err);
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
      refuse(host, site, "SelectWorkspace", workspace, cause.case, sentence);
      return;
    }
    default:
      return unreachableArm("SelectWorkspaceResponse.result", armName(result));
  }
}

/**
 * Open WORKSPACE from HOST, then select it (`jumpToWorkspace`), drawing a
 * refusal of either inside HOST. A refused open selects nothing.
 */
export async function openAndJumpToWorkspace(
  host: HTMLElement,
  rc: RowContext,
  workspace: WorkspaceRef,
  site: JumpSite,
): Promise<void> {
  for (const stale of childrenMatching(host, `.${site.refusalClass}`)) stale.remove();
  log.info(`opening a workspace from ${site.describe}`, {
    operation: `${site.operation}-open`,
    context: { workspace: workspace.id },
  });
  let response: OpenWorkspaceResponse;
  try {
    response = await callUnary(
      rc.ctx,
      "OpenWorkspace",
      (client) => client.openWorkspace(create(OpenWorkspaceRequestSchema, { workspace })),
      OpenWorkspaceResponseSchema,
    );
  } catch (err) {
    refuseTransport(host, site, "OpenWorkspace", workspace, err);
    return;
  }
  const result = requireCase(response.result, "OpenWorkspaceResponse.result");
  switch (result.case) {
    case "success":
      return jumpToWorkspace(host, rc, workspace, site);
    case "error": {
      const cause = requireCase(result.value.cause, "OpenWorkspaceError.cause");
      const sentence =
        crossCuttingSentence("OpenWorkspace", cause) ??
        openWorkspaceRefusal(cause);
      refuse(host, site, "OpenWorkspace", workspace, cause.case, sentence);
      return;
    }
    default:
      return unreachableArm("OpenWorkspaceResponse.result", armName(result));
  }
}

/** Record RPC's refusal of WORKSPACE under SITE, and draw it inside HOST. */
function refuse(
  host: HTMLElement,
  site: JumpSite,
  rpc: string,
  workspace: WorkspaceRef,
  arm: string,
  sentence: string,
): void {
  log.warn(`${rpc} was refused: ${sentence}`, {
    operation: `${site.operation}-refused`,
    context: { rpc, arm, workspace: workspace.id },
  });
  host.append(drawRefusal(site, arm, sentence));
}

/**
 * Record RPC's transport failure under SITE and draw it inside HOST. A
 * malformed answer is not one, and travels up.
 */
function refuseTransport(host: HTMLElement, site: JumpSite, rpc: string, workspace: WorkspaceRef, err: unknown): void {
  if (isMalformedView(err)) throw err;
  log.error(`${rpc} failed at the transport: ${String(err)}`, {
    operation: `${site.operation}-failed`,
    context: { rpc, workspace: workspace.id, cause: err },
  });
  host.append(drawRefusal(site, "transport", "the daemon could not be reached"));
}

/** The refusal, inside the element that made the call. */
function drawRefusal(site: JumpSite, arm: string, text: string): HTMLElement {
  const el = document.createElement("div");
  el.className = `refusal ${site.refusalClass}`;
  el.setAttribute("data-arm", arm);
  el.textContent = text;
  return el;
}
