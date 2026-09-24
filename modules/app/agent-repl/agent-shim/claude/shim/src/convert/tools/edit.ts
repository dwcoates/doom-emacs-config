/**
 * convert/tools/edit.ts — the agent replaced a matched string inside a file.
 *
 * # The vendor already diffed this one
 *
 * Unlike a write, an edit's result carries `structuredPatch` — the vendor's own
 * hunks, computed against the file as it actually was — so this converter
 * CARRIES them rather than re-deriving anything. `old_string`/`new_string` are
 * deliberately not carried anywhere: they would be a second, unresolved
 * description of the change the hunks already state.
 *
 * The post-terminal `diagnostics` arm of `AgentEdit` is NOT produced here.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, hunksOf, settle, str } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools.edit" });

/** The path an edit acted on. */
function readPath(path: string): conversationv1.ReadPath {
  return create(conversationv1.ReadPathSchema, { path });
}

/** The file the CALLER named. */
function requestedPath(call: PendingCall): string | undefined {
  return str(call.input, "file_path");
}

/** The edit's `start` arm. */
function editStart(call: PendingCall, path: string): conversationv1.AgentEditStart {
  return create(conversationv1.AgentEditStartSchema, {
    path: readPath(path),
    startedAt: startedAt(call.startedAtMs),
  });
}

/** The edit's `success` arm, or `undefined` when no path can be stated. */
function editSuccess(
  call: PendingCall,
  outcome: ToolOutcome,
): conversationv1.AgentEditSuccess | undefined {
  const record = asRecord(outcome.structured);
  if (record === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "an edit settled with no typed output; no success frame is produced",
    );
    return undefined;
  }
  const path = str(record, "filePath") ?? requestedPath(call);
  if (path === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "an edit settled with no path at all; no success frame is produced",
    );
    return undefined;
  }
  return create(conversationv1.AgentEditSuccessSchema, {
    path: readPath(path),
    patch: hunksOf(record["structuredPatch"]),
    userModified: bool(record, "userModified") ?? false,
    settledAt: settle(call, outcome),
  });
}

/** The agent editing a file. */
export const editConverter: ToolConverter = {
  kind: "edit",
  carriesProgress: true,

  start(call) {
    const path = requestedPath(call);
    if (path === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "an edit was announced with no file path; no start frame is produced",
      );
      return undefined;
    }
    return {
      case: "edit",
      value: create(conversationv1.AgentEditSchema, {
        result: { case: "start", value: editStart(call, path) },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "an edit failed");
      // THE SETTLED FRAME STANDS ALONE: it restates what the call named, since
      // the start it upserts over is gone once it lands. A call that named
      // nothing has no start either, so it gets no failure frame, exactly as
      // it got no announcement.
      const path = requestedPath(call);
      if (path === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "an edit failed with no file path to restate; no failure frame is produced",
        );
        return undefined;
      }
      return {
        case: "edit",
        value: create(conversationv1.AgentEditSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentEditFailureSchema, {
              error: failureOf(call, outcome),
              path: readPath(path),
            }),
          },
        }),
      };
    }
    const success = editSuccess(call, outcome);
    if (success === undefined) return undefined;
    return {
      case: "edit",
      value: create(conversationv1.AgentEditSchema, {
        result: { case: "success", value: success },
      }),
    };
  },

  progress(beat) {
    return {
      case: "edit",
      value: create(conversationv1.AgentEditSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};
