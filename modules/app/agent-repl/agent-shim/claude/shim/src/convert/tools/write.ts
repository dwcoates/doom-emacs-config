/**
 * convert/tools/write.ts — the agent handed over a file's whole new contents.
 *
 * # THE PRODUCER DIFFS
 *
 * The vendor hands a write `originalFile` (null for a creation) and `content`,
 * and its own `structuredPatch` is empty for the create case. Nothing upstream
 * states what CHANGED, so the shim diffs the two versions here, once, at the
 * moment the change is recorded — see {@link diffHunks}. A card showing the
 * whole new file instead of the change would be unreadable for a one-line
 * write, and a consumer re-diffing later would be diffing a file that has
 * moved on.
 *
 * The post-terminal `diagnostics` arm of `AgentWrite` is NOT produced here: it
 * arrives as a separate vendor record after the result and is joined to this
 * unit by adjacency elsewhere.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, diffHunks, failureOf, settle, str } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools.write" });

/** The path a write acted on. */
function readPath(path: string): conversationv1.ReadPath {
  return create(conversationv1.ReadPathSchema, { path });
}

/** The file the CALLER named. */
function requestedPath(call: PendingCall): string | undefined {
  return str(call.input, "file_path");
}

/** The write's `start` arm. NOTHING is said here about create versus update. */
function writeStart(call: PendingCall, path: string): conversationv1.AgentWriteStart {
  return create(conversationv1.AgentWriteStartSchema, {
    path: readPath(path),
    startedAt: startedAt(call.startedAtMs),
  });
}

/** Whether the file existed beforehand, as the vendor's own `type` states it. */
function writeOutcome(type: string | undefined): conversationv1.AgentWriteSuccess["outcome"] | undefined {
  if (type === "create") {
    return { case: "created", value: create(conversationv1.AgentWriteCreatedSchema, {}) };
  }
  if (type === "update") {
    return { case: "updated", value: create(conversationv1.AgentWriteUpdatedSchema, {}) };
  }
  return undefined;
}

/** The write's `success` arm, or `undefined` when the vendor left it unstatable. */
function writeSuccess(
  call: PendingCall,
  outcome: ToolOutcome,
): conversationv1.AgentWriteSuccess | undefined {
  const record = asRecord(outcome.structured);
  if (record === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a write settled with no typed output; no success frame is produced",
    );
    return undefined;
  }
  const path = str(record, "filePath") ?? requestedPath(call);
  if (path === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a write settled with no path at all; no success frame is produced",
    );
    return undefined;
  }
  const written = writeOutcome(str(record, "type"));
  if (written === undefined) {
    // WITHOUT THIS ARM A CREATION IS DRAWN AS A REWRITE OF NOTHING. Guessing
    // it from a null `originalFile` would be the producer inventing the
    // distinction the vendor is supposed to state.
    LOGGER.debug(
      { tool_use_id: call.toolUseId, write_type: str(record, "type") ?? "unstated" },
      "a write stated neither create nor update; no success frame is produced",
    );
    return undefined;
  }
  const content = str(record, "content");
  if (content === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a write stated no written content, so nothing can be diffed; no success frame is produced",
    );
    return undefined;
  }
  // `originalFile` is null for a creation, which diffs against the empty file:
  // every line is then an addition, exactly as the proto describes.
  const original = str(record, "originalFile") ?? "";
  return create(conversationv1.AgentWriteSuccessSchema, {
    path: readPath(path),
    outcome: written,
    patch: diffHunks(original, content),
    userModified: bool(record, "userModified") ?? false,
    settledAt: settle(outcome),
  });
}

/** The agent writing a file. */
export const writeConverter: ToolConverter = {
  kind: "write",
  carriesProgress: true,

  start(call) {
    const path = requestedPath(call);
    if (path === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a write was announced with no file path; no start frame is produced",
      );
      return undefined;
    }
    return {
      case: "write",
      value: create(conversationv1.AgentWriteSchema, {
        result: { case: "start", value: writeStart(call, path) },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a write failed");
      // THE SETTLED FRAME STANDS ALONE: it restates what the call named, since
      // the start it upserts over is gone once it lands. A call that named
      // nothing has no start either, so it gets no failure frame, exactly as
      // it got no announcement.
      const path = requestedPath(call);
      if (path === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a write failed with no file path to restate; no failure frame is produced",
        );
        return undefined;
      }
      return {
        case: "write",
        value: create(conversationv1.AgentWriteSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentWriteFailureSchema, {
              error: failureOf(outcome),
              path: readPath(path),
            }),
          },
        }),
      };
    }
    const success = writeSuccess(call, outcome);
    if (success === undefined) return undefined;
    return {
      case: "write",
      value: create(conversationv1.AgentWriteSchema, {
        result: { case: "success", value: success },
      }),
    };
  },

  progress(beat) {
    return {
      case: "write",
      value: create(conversationv1.AgentWriteSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};
