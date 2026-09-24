/**
 * convert/tools/worktree.ts — into an isolated tree, and back out of it.
 *
 * # ONE CONVERTER, TWO TOOL NAMES
 *
 * `EnterWorktree` and `ExitWorktree` are two vendor calls of one unit kind, and
 * the act is read off the NAME. Unlike plan mode there is no coalescing
 * anywhere: each settled act is its own divider in the feed, because the two
 * moments can be far apart and everything between them happened inside the
 * tree.
 *
 * # What was ASKED and what HAPPENED are separate facts
 *
 * The exit's start carries the request (`keep` / `remove`, with the discard
 * flag), and the exit's success restates what the vendor says actually became
 * of the tree. A removal that was asked for and refused would otherwise be
 * indistinguishable from one that happened.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, settle, str, strOr, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-worktree",
  operation: "shim.convert.tools.worktree",
});

/** The vendor's two spellings of this unit's two acts. */
const ENTER = "EnterWorktree";
const EXIT = "ExitWorktree";

/** Which act a call is, by the name the agent used. */
function actNameOf(call: PendingCall): "enter" | "exit" | undefined {
  if (call.toolName === ENTER) return "enter";
  if (call.toolName === EXIT) return "exit";
  LOGGER.debug(
    { tool: call.toolName, tool_use_id: call.toolUseId },
    "a worktree unit was built from a tool name that is neither the enter nor the exit call",
  );
  return undefined;
}

/** What was asked for the tree on the way out. */
function exitActionOf(call: PendingCall): conversationv1.AgentWorktreeExit["action"] {
  const action = str(call.input, "action");
  if (action === "remove") {
    return {
      case: "remove",
      value: create(conversationv1.AgentWorktreeExitRemoveSchema, {
        discardChanges: bool(call.input, "discard_changes") ?? false,
      }),
    };
  }
  if (action === "keep") {
    return { case: "keep", value: create(conversationv1.AgentWorktreeExitKeepSchema, {}) };
  }
  // Defaulting to either arm would state a request the agent never made.
  LOGGER.debug(
    { tool_use_id: call.toolUseId, action },
    "a worktree exit named no recognized action; what was asked for the tree is left unstated",
  );
  return { case: undefined };
}

/** What became of the tree, as the vendor says it happened. */
function exitOutcomeOf(
  output: Record<string, unknown>,
  toolUseId: string,
): conversationv1.AgentWorktreeExited["outcome"] {
  const action = str(output, "action");
  if (action === "remove") {
    return {
      case: "removed",
      value: create(conversationv1.AgentWorktreeRemovedSchema, {
        // UNSET rather than zero: "the vendor stated no figure" is not "none
        // were discarded".
        discardedFiles: uint(output, "discardedFiles"),
        discardedCommits: uint(output, "discardedCommits"),
      }),
    };
  }
  if (action === "keep") {
    return { case: "kept", value: create(conversationv1.AgentWorktreeKeptSchema, {}) };
  }
  LOGGER.debug(
    { tool_use_id: toolUseId, action },
    "a settled worktree exit named no recognized action; what became of the tree is left unstated",
  );
  return { case: undefined };
}

/** The one wrapper every frame of this unit shares. */
function item(state: conversationv1.AgentWorktree["state"]): conversationv1.AgentActivity["item"] {
  return { case: "worktree", value: create(conversationv1.AgentWorktreeSchema, { state }) };
}

export const worktreeConverter: ToolConverter = {
  kind: "worktree",
  // AgentWorktree declares no progress arm.
  carriesProgress: false,

  start(call) {
    const act = actNameOf(call);
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act }, "a worktree call was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentWorktreeStartSchema, {
        act:
          act === "enter"
            ? {
                case: "enter",
                value: create(conversationv1.AgentWorktreeEnterSchema, {
                  name: str(call.input, "name"),
                  path: str(call.input, "path"),
                }),
              }
            : act === "exit"
              ? {
                  case: "exit",
                  value: create(conversationv1.AgentWorktreeExitSchema, {
                    action: exitActionOf(call),
                  }),
                }
              : { case: undefined },
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the worktree call never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentWorktreeFailureSchema, { error: failureOf(call, outcome) }),
      });
    }
    const act = actNameOf(call);
    if (act === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled worktree call names no act; no success frame is produced",
      );
      return undefined;
    }
    const output = asRecord(outcome.structured);
    const path = str(output, "worktreePath");
    if (output === undefined || path === undefined) {
      // The tree's path is the whole subject of both arms — the divider names
      // it, and a frame without it says the session moved somewhere unnamed.
      LOGGER.debug(
        { tool_use_id: call.toolUseId, act },
        "a settled worktree call named no worktree path; no success frame is produced",
      );
      return undefined;
    }
    LOGGER.logVerbose({ tool_use_id: call.toolUseId, act }, "the worktree call returned");
    return item({
      case: "success",
      value: create(conversationv1.AgentWorktreeSuccessSchema, {
        act:
          act === "enter"
            ? {
                case: "entered",
                value: create(conversationv1.AgentWorktreeEnteredSchema, {
                  path,
                  branch: str(output, "worktreeBranch"),
                  message: strOr(output, "message"),
                }),
              }
            : {
                case: "exited",
                value: create(conversationv1.AgentWorktreeExitedSchema, {
                  outcome: exitOutcomeOf(output, call.toolUseId),
                  originalCwd: strOr(output, "originalCwd"),
                  path,
                  branch: str(output, "worktreeBranch"),
                  tmuxSessionName: str(output, "tmuxSessionName"),
                  message: strOr(output, "message"),
                }),
              },
        settledAt: settle(call, outcome),
      }),
    });
  },
};
