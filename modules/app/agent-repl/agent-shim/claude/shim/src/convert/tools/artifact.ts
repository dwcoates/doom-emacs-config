/**
 * convert/tools/artifact.ts — a local file published as a hosted page.
 *
 * # THE OUTPUT IS TYPED; NEVER PARSE ITS PROSE
 *
 * `ArtifactOutput` is a union of two typed shapes — a publish answers with
 * `url`/`path`/`title`, a list answers with an `artifacts` array — so the arm is
 * read off the FIELDS, not off the sentence the model was shown. This is the
 * correction recorded at vetting item 1, and it is the whole reason the settled
 * bubble can offer a real link.
 *
 * # A list is a quiet read
 *
 * `AgentArtifactListed` is empty ON PURPOSE: nothing in this product draws a
 * listing, and the arm records only that the read happened. The vendor's typed
 * rows are there if that ever changes.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, bool, failureOf, str, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-artifact",
  operation: "shim.convert.tools.artifact",
});

/** Which action was asked, as the call spelled it. */
function actOf(call: PendingCall): conversationv1.AgentArtifactStart["act"] {
  if (str(call.input, "action") === "list") {
    return {
      case: "list",
      value: create(conversationv1.AgentArtifactListSchema, {
        limit: uint(call.input, "limit"),
        scope: str(call.input, "scope"),
      }),
    };
  }
  const filePath = str(call.input, "file_path");
  if (filePath === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "an artifact publish names no file to render; the unit cannot restate what is being published",
    );
  }
  return {
    case: "publish",
    value: create(conversationv1.AgentArtifactPublishSchema, {
      filePath: filePath ?? "",
      favicon: str(call.input, "favicon"),
      title: str(call.input, "title"),
      // PRESENCE IS THE FACT: an existing url means this publish is a redeploy.
      updatesUrl: str(call.input, "url"),
      label: str(call.input, "label"),
      description: str(call.input, "description"),
      force: bool(call.input, "force") ?? false,
    }),
  };
}

/** The one wrapper every frame of this unit shares. */
function item(
  result: conversationv1.AgentArtifact["result"],
): conversationv1.AgentActivity["item"] {
  return { case: "artifact", value: create(conversationv1.AgentArtifactSchema, { result }) };
}

export const artifactConverter: ToolConverter = {
  kind: "artifact",
  // AgentArtifact declares no progress arm.
  carriesProgress: false,

  start(call) {
    const act = actOf(call);
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act: act.case }, "an artifact call was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentArtifactStartSchema, {
        act,
        startedAtMs: BigInt(Math.trunc(call.startedAtMs)),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the artifact call never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentArtifactFailureSchema, {
          failure: failureOf(call, outcome),
          // THE ACT THAT FAILED, restated whole from the call: a replay serves
          // this failure with no start beside it, and a failed publish's card
          // is its favicon, title and file, which only the call states.
          act: actOf(call),
        }),
      });
    }
    const output = asRecord(outcome.structured);
    if (output === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled artifact call carried no typed output; the answered act cannot be told from nothing",
      );
      return undefined;
    }
    if (arr(output, "artifacts") !== undefined) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "an artifact listing was read");
      return item({
        case: "success",
        value: create(conversationv1.AgentArtifactSuccessSchema, {
          outcome: {
            case: "listed",
            value: create(conversationv1.AgentArtifactListedSchema, {}),
          },
        }),
      });
    }
    const url = str(output, "url");
    if (url === undefined) {
      // The url IS the published page; a publish arm without one would draw a
      // link to nowhere.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled artifact publish named no url; the page it claims to have published is unreachable",
      );
      return undefined;
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "an artifact page is live");
    return item({
      case: "success",
      value: create(conversationv1.AgentArtifactSuccessSchema, {
        outcome: {
          case: "published",
          value: create(conversationv1.AgentArtifactPublishedSchema, {
            url,
            title: str(output, "title"),
          }),
        },
      }),
    });
  },
};
