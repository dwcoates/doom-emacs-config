/**
 * convert/tools/bash.ts — the agent ran a shell command.
 *
 * # A BACKGROUNDED COMMAND DID NOT END, IT MOVED
 *
 * The vendor returns the SAME shape for a command that finished and for one it
 * launched into the background: `stdout`/`stderr` empty, `interrupted` false,
 * and a `backgroundTaskId`. Settling the unit on that receipt would say the
 * work concluded when it had not — so this converter answers `undefined` for
 * it, the unit stays open, and the detached-work frame naming this unit is what
 * says where the work went. That is why `AgentBash` has no "backgrounded" arm:
 * two producers of one fact could disagree about whether the work left.
 *
 * # A nonzero exit is still the SUCCESS arm
 *
 * The failure arm is for a call that could not be performed. A command that ran
 * and failed ran, and what it printed is the answer the caller wanted.
 *
 * # `termination` is UNSET here, deliberately
 *
 * `AgentBashCompleted.termination` is documented as SET FOR A DETACHED SHELL
 * and UNSET FOR A FOREGROUND COMMAND. `BashOutput` carries no exit code and no
 * signal for a foreground call, so this producer states none — reporting
 * `exited{code: 0}` would be inventing a status the shell never reported.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, settle, str, strOr, uint } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools.bash" });

/** What was run, restated on every frame so each stands alone. */
function bashCommand(call: PendingCall, line: string): conversationv1.AgentBashCommand {
  return create(conversationv1.AgentBashCommandSchema, {
    line,
    // UNSET when the agent wrote none: a consumer then draws the line alone.
    description: str(call.input, "description"),
    // CONSENT-RELEVANT. The caller's own flag is the only statement of it.
    sandbox:
      bool(call.input, "dangerouslyDisableSandbox") === true
        ? {
            case: "sandboxDisabled",
            value: create(conversationv1.AgentBashSandboxDisabledSchema, {}),
          }
        : { case: "sandboxed", value: create(conversationv1.AgentBashSandboxedSchema, {}) },
  });
}

/** The command line, which no frame can be stated without. */
function requestedLine(call: PendingCall): string | undefined {
  return str(call.input, "command");
}

/** Where the whole output was spilled, when the producer kept it. */
function spilled(
  record: Record<string, unknown>,
  sizeBytes: number,
): conversationv1.AgentBashSpilledOutput | undefined {
  const path = str(record, "persistedOutputPath");
  // WITHOUT A PATH A TRUNCATION IS A DEAD END, and that is a real state: the
  // omitted bytes are simply gone, which the proto spells as an unset field.
  if (path === undefined) return undefined;
  return create(conversationv1.AgentBashSpilledOutputSchema, {
    path,
    sizeBytes: BigInt(sizeBytes),
  });
}

/** Whether everything the command said is carried inline. */
function textExtent(
  record: Record<string, unknown>,
  stdout: string,
  stderr: string,
): conversationv1.AgentBashOutputText["extent"] {
  const total = uint(record, "persistedOutputSize");
  if (total === undefined) {
    return { case: "whole", value: create(conversationv1.AgentBashOutputWholeSchema, {}) };
  }
  // SUBTRACTED ONCE, HERE. The vendor declares the total; the proto carries the
  // omitted figure, and it is clamped so a total that trails the inline bytes
  // never becomes a negative "fewer bytes not shown".
  const inline = Buffer.byteLength(stdout, "utf8") + Buffer.byteLength(stderr, "utf8");
  const omitted = total <= inline ? 0 : total - inline;
  return {
    case: "partial",
    value: create(conversationv1.AgentBashOutputPartialSchema, {
      bytesOmitted: BigInt(omitted),
      spilled: spilled(record, total),
    }),
  };
}

/** What the command said, in the form it said it. */
function bashOutput(
  call: PendingCall,
  record: Record<string, unknown>,
): conversationv1.AgentBashOutput | undefined {
  if (bool(record, "isImage") === true) {
    // NO MEDIA TYPE EXISTS ANYWHERE IN `BashOutput`, and `AgentBashOutputImage`
    // requires one so a consumer need not sniff the bytes. Naming a type the
    // vendor never stated would be inventing a fact, and an empty one would be
    // the sentinel this contract forbids — so no terminal is produced.
    LOGGER.log(
      { level: "error", tool_use_id: call.toolUseId },
      "a command produced image output with no stated media type; no terminal frame is produced",
    );
    return undefined;
  }
  const stdout = strOr(record, "stdout");
  const stderr = strOr(record, "stderr");
  return create(conversationv1.AgentBashOutputSchema, {
    form: {
      case: "text",
      value: create(conversationv1.AgentBashOutputTextSchema, {
        stdout,
        stderr,
        extent: textExtent(record, stdout, stderr),
      }),
    },
  });
}

/** How the command ended: run to completion, or cut short. */
function bashOutcome(
  record: Record<string, unknown>,
  output: conversationv1.AgentBashOutput,
): conversationv1.AgentBashSuccess["outcome"] {
  if (bool(record, "interrupted") === true) {
    const timeoutMs = uint(record, "timedOutAfterMs");
    return {
      case: "interrupted",
      value: create(conversationv1.AgentBashInterruptedSchema, {
        output,
        cause:
          timeoutMs === undefined
            ? {
                case: "byUser",
                value: create(conversationv1.AgentBashInterruptedByUserSchema, {}),
              }
            : {
                case: "timedOut",
                value: create(conversationv1.AgentBashInterruptedByTimeoutSchema, {
                  timeoutMs: BigInt(timeoutMs),
                }),
              },
      }),
    };
  }
  return {
    case: "completed",
    value: create(conversationv1.AgentBashCompletedSchema, {
      output,
      // UNSET: no producer states a termination for a foreground command.
      termination: undefined,
    }),
  };
}

/** The agent running a shell command. */
export const bashConverter: ToolConverter = {
  kind: "bash",
  carriesProgress: true,

  start(call) {
    const line = requestedLine(call);
    if (line === undefined) {
      LOGGER.log(
        { level: "error", tool_use_id: call.toolUseId },
        "a command was announced with no command line; no start frame is produced",
      );
      return { case: undefined };
    }
    return {
      case: "bash",
      value: create(conversationv1.AgentBashSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentBashStartSchema, {
            command: bashCommand(call, line),
            startedAt: startedAt(call.startedAtMs),
          }),
        },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a shell call could not be performed");
      return {
        case: "bash",
        value: create(conversationv1.AgentBashSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentBashFailureSchema, { error: failureOf(outcome) }),
          },
        }),
      };
    }
    const record = asRecord(outcome.structured);
    if (record === undefined) {
      LOGGER.log(
        { level: "warn", tool_use_id: call.toolUseId },
        "a command settled with no typed output; no terminal frame is produced",
      );
      return undefined;
    }
    const backgroundTaskId = str(record, "backgroundTaskId");
    if (backgroundTaskId !== undefined && backgroundTaskId !== "") {
      LOGGER.logVerbose(
        { tool_use_id: call.toolUseId, background_task_id: backgroundTaskId },
        "the command MOVED rather than ended; its detached-work frame settles it, not this receipt",
      );
      return undefined;
    }
    const line = requestedLine(call);
    if (line === undefined) {
      LOGGER.log(
        { level: "error", tool_use_id: call.toolUseId },
        "a command settled with no command line to restate; no terminal frame is produced",
      );
      return undefined;
    }
    const output = bashOutput(call, record);
    if (output === undefined) return undefined;
    return {
      case: "bash",
      value: create(conversationv1.AgentBashSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentBashSuccessSchema, {
            command: bashCommand(call, line),
            outcome: bashOutcome(record, output),
            settledAt: settle(outcome),
          }),
        },
      }),
    };
  },

  progress(beat) {
    return {
      case: "bash",
      value: create(conversationv1.AgentBashSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};
