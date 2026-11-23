/**
 * jump — a feed's cross-workspace jump: `SelectInEditor`, echoed verbatim,
 * answered at the element that was clicked.
 *
 * A cross-workspace click is `SelectInEditor` and nothing else (R8), the same
 * select a rail row's click makes (`buildSelectInEditorRequest`): the editor
 * makes the switch, the roster stream reflects it, and the bubble draws
 * nothing on success. A page never sends `SelectWorkspace`, whose select the
 * frame only follows once no minibuffer is active, so the rail could draw the
 * workspace selected while Emacs still showed another. The refusal is the click's own answer, so it lands AT
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
import { createLogger } from "../../log.js";
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
  SelectInEditorResponseSchema,
  type SelectInEditorResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_in_editor_pb";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FeedId } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { armName } from "../renderers.js";
import type { AppContext } from "../../rpc/context.js";
import { childrenMatching } from "../../dom.js";
import { noteSelectRoute } from "../../sidebar/row.js";
import { buildSelectInEditorRequest, openWorkspaceRefusal } from "../../sidebar/verbs.js";

const log = createLogger("webapp.feed.merge.jump");

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
 * opens (`SelectInEditorRequest.reveal`). THE JUMP IS A LINK FOLLOWED FROM
 * THIS PAGE, so the select names the page's own workspace as the one it was
 * followed from (`SelectInEditorRequest.followed_from`), which its editor
 * counts viewed.
 */
export async function jumpToWorkspace(
  host: HTMLElement,
  ctx: AppContext,
  workspace: WorkspaceRef,
  site: JumpSite,
  reveal?: FeedId,
): Promise<void> {
  for (const stale of childrenMatching(host, `.${site.refusalClass}`)) stale.remove();
  log.info("selecting a workspace from {describe}", {
    operation: site.operation,
    context: { workspace: workspace.id, reveal: reveal?.value ?? "none" },
  }, { describe: site.describe });
  let response: SelectInEditorResponse;
  try {
    response = await callUnary(
      ctx,
      "SelectInEditor",
      (client) => client.selectInEditor(buildSelectInEditorRequest(workspace, reveal, ctx.workspace)),
      SelectInEditorResponseSchema,
    );
  } catch (err) {
    refuseTransport(host, site, "SelectInEditor", workspace, err);
    return;
  }
  const result = requireCase(response.result, "SelectInEditorResponse.result");
  switch (result.case) {
    case "success":
      // Nothing is drawn: the roster stream carries the new `current`.
      noteSelectRoute(response, workspace);
      return;
    case "error": {
      const cause = requireCase(result.value.cause, "SelectInEditorError.cause");
      const expected = SELECT_WORKSPACE_EXPECTED_ARMS[cause.case];
      if (expected !== undefined) {
        // An expected answer, not a refusal: nothing is drawn at the element.
        log.info("SelectInEditor was answered {case}: {expected}", {
          operation: `${site.operation}-expected-answer`,
          context: { arm: cause.case, workspace: workspace.id },
        }, { case: cause.case, expected });
        return;
      }
      const sentence =
        crossCuttingSentence("SelectInEditor", cause) ?? "that workspace could not be selected";
      refuse(host, site, "SelectInEditor", workspace, cause.case, sentence);
      return;
    }
    default:
      return unreachableArm("SelectInEditorResponse.result", armName(result));
  }
}

/**
 * Open WORKSPACE from HOST, then select it (`jumpToWorkspace`), drawing a
 * refusal of either inside HOST. A refused open selects nothing.
 */
export async function openAndJumpToWorkspace(
  host: HTMLElement,
  ctx: AppContext,
  workspace: WorkspaceRef,
  site: JumpSite,
): Promise<void> {
  for (const stale of childrenMatching(host, `.${site.refusalClass}`)) stale.remove();
  log.info("opening a workspace from {describe}", {
    operation: `${site.operation}-open`,
    context: { workspace: workspace.id },
  }, { describe: site.describe });
  let response: OpenWorkspaceResponse;
  try {
    response = await callUnary(
      ctx,
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
      return jumpToWorkspace(host, ctx, workspace, site);
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

/** Whether a jump's target is open or closed, as the daemon served it. */
export type JumpStanding = "open" | "closed";

/**
 * The jump a click on a target of STANDING makes: an open workspace is
 * selected, and a closed one is reopened and then selected, as the sidebar's
 * closed entries are. PATH names the served field the standing came from, for
 * the loud failure an unknown standing is.
 */
export function jumpForStanding(standing: JumpStanding, path: string): typeof jumpToWorkspace {
  switch (standing) {
    case "open":
      return jumpToWorkspace;
    case "closed":
      return openAndJumpToWorkspace;
    default: {
      const other: string = standing;
      return unreachableArm(path, other);
    }
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
  log.warn("{rpc} was refused: {sentence}", {
    operation: `${site.operation}-refused`,
    context: { arm, workspace: workspace.id },
  }, { rpc, sentence });
  host.append(drawRefusal(site, arm, sentence));
}

/**
 * Record RPC's transport failure under SITE and draw it inside HOST. A
 * malformed answer is not one, and travels up.
 */
function refuseTransport(host: HTMLElement, site: JumpSite, rpc: string, workspace: WorkspaceRef, err: unknown): void {
  if (isMalformedView(err)) throw err;
  log.error("{rpc} failed at the transport: {err}", {
    operation: `${site.operation}-failed`,
    context: { workspace: workspace.id, cause: err },
  }, { rpc, err: String(err) });
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
