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
 * The vendor marks such a result an error FOR THE MODEL, and a nonzero exit
 * often carries no typed output at all — the whole result is the bare string
 * "Error: Exit code 7\n…". So the ending is read from the structured field when
 * there is one and from the returned TEXT when there is not, which is the same
 * rule the file plane applies to the same capture. Only a result that states an
 * ending NOWHERE is a failure.
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
import { settledAt, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, num, resultText, settle, str, strOr, uint } from "./support.js";

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

/**
 * The image an answering `tool_result` carried: the bytes and their media type.
 *
 * The shim carries an inlined image by REFERENCE, as the data URL the bytes
 * already are (convert/blocks.ts imageBlock), so the payload is read back out of
 * that URL here. A block naming a FETCHABLE url carries no bytes at all and a
 * payload that does not decode carries none either: both answer `undefined`
 * rather than half an image, and the caller refuses loudly.
 */
function resultImage(
  content: conversationv1.ToolResultContent | undefined,
): { data: Uint8Array; mediaType: string } | undefined {
  for (const block of content?.blocks ?? []) {
    if (block.block.case !== "image") continue;
    const image = block.block.value;
    if (image.mediaType === "" || image.location.case !== "url") continue;
    const match = /^data:[^,]*;base64,(.*)$/s.exec(image.location.value.url);
    if (match === null) continue;
    const data = Buffer.from(match[1], "base64");
    if (data.length === 0) continue;
    return { data: new Uint8Array(data), mediaType: image.mediaType };
  }
  return undefined;
}

/** What the command said, in the form it said it. */
function bashOutput(
  call: PendingCall,
  record: Record<string, unknown>,
  outcome: ToolOutcome,
): conversationv1.AgentBashOutput | undefined {
  if (bool(record, "isImage") === true) {
    // THE PICTURE IS IN THE ANSWERING RESULT, NOT IN `BashOutput`. The vendor's
    // Output object states only THAT the output was an image; the bytes and
    // their media type arrive as the `tool_result`'s own image content block,
    // which is the only place either is stated. `AgentBashOutputImage` requires
    // the media type so a consumer need not sniff the bytes, and this is where
    // it comes from.
    const image = resultImage(outcome.content);
    if (image === undefined) {
      // NAMING A TYPE THE VENDOR NEVER STATED WOULD BE INVENTING A FACT, and an
      // empty one would be the sentinel this contract forbids — so no terminal
      // is produced, exactly as before this arm could ever be filled.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a command produced image output the result carries no loadable bytes for; no terminal frame is produced",
      );
      return undefined;
    }
    return create(conversationv1.AgentBashOutputSchema, {
      form: {
        case: "image",
        value: create(conversationv1.AgentBashOutputImageSchema, image),
      },
    });
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
/**
 * The exit status a foreground result STATED, when it stated one.
 *
 * `exitCode` is the PRECISE datum when the vendor gives one: a number, not
 * prose to parse. `returnCodeInterpretation` ("exited with code 3") is prose
 * ABOUT that same status, so it is read only as a fallback for a result that
 * carries no `exitCode` — camelCase before the vendor's snake_case spelling,
 * `return_code_interpretation`, since the disk carries both. A result that
 * states neither says nothing about how the command ended, which is why
 * `termination` stays unset for it rather than being synthesized as a zero.
 */
function exitedCode(record: Record<string, unknown> | undefined): number | undefined {
  const exitCode = num(record, "exitCode");
  if (exitCode !== undefined) return Math.trunc(exitCode);
  const interpretation = str(record, "returnCodeInterpretation") ?? str(record, "return_code_interpretation");
  if (interpretation === undefined) return undefined;
  return firstSignedInt(interpretation);
}

/** The first signed decimal in a sentence, or `undefined` when it holds none. */
function firstSignedInt(text: string): number | undefined {
  const match = /(-?\d+)/.exec(text);
  return match === null ? undefined : Number(match[1]);
}

/**
 * The ending the vendor spelled in the text it returned to the model.
 *
 * A nonzero exit arrives with NO typed output at all — the result is the bare
 * string "Error: Exit code 7\npartway\nto stderr" — so the command's own
 * verdict on itself survives only here. The statement is a LINE OF ITS OWN, so
 * the marker is read only where it OPENS a line: a passing command that merely
 * printed "make: recipe returned exit code 3" said nothing about its ending,
 * and reading that as an exit would invent a status the shell never reported.
 */
function statedExitFromText(text: string): number | undefined {
  const markers = ["error: exit code", "exited with code", "exit code"];
  for (const line of text.split("\n")) {
    const trimmed = line.trim().toLowerCase();
    for (const marker of markers) {
      if (trimmed.startsWith(marker)) return firstSignedInt(trimmed.slice(marker.length));
    }
  }
  return undefined;
}

/**
 * The exit status the result STATED, wherever it stated one.
 *
 * The structured field is PREFERRED: a result that carries one has told us the
 * status outright. The returned text is read only as a fallback, and only for a
 * result the vendor marked an error — a command that succeeded states its
 * ending in the structured fields, so mining a passing command's OUTPUT for a
 * status could only misread what it printed.
 */
function statedExit(outcome: ToolOutcome): number | undefined {
  const structured = exitedCode(asRecord(outcome.structured));
  if (structured !== undefined) return structured;
  if (!outcome.isError) return undefined;
  return statedExitFromText(resultText(outcome.content));
}

function bashOutcome(
  record: Record<string, unknown>,
  output: conversationv1.AgentBashOutput,
  exited: number | undefined,
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
      // UNSET unless the result STATED a status. A foreground call ordinarily
      // carries none, and synthesizing `exited(0)` would be the shim inventing
      // a fact nothing reported; a non-zero exit, however, IS stated -- in
      // `exitCode` or, failing that, `returnCodeInterpretation` -- and dropping
      // it would lose the command's own verdict on itself.
      ...(exited === undefined
        ? { termination: undefined }
        : {
            termination: create(conversationv1.AgentBashTerminationSchema, {
              how: {
                case: "exited",
                value: create(conversationv1.AgentBashExitedSchema, { code: exited }),
              },
            }),
          }),
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
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a command was announced with no command line; no start frame is produced",
      );
      return undefined;
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
    // A NON-ZERO EXIT IS THE COMMAND'S OWN VERDICT ON ITSELF, NOT A FAILURE OF
    // THE CALL. The vendor marks such a result an error FOR THE MODEL (`grep`
    // found nothing, a test failed) and states the ending in
    // `returnCodeInterpretation`. Reading `isError` alone drew every non-zero
    // exit as `AgentBashFailure`, which tells the user the shell broke when the
    // shell did exactly what it was asked.
    const exited = statedExit(outcome);
    if (outcome.isError && exited === undefined) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a shell call could not be performed");
      // THE SETTLED FRAME STANDS ALONE: it restates what the call named, since
      // the start it upserts over is gone once it lands. A call that named
      // nothing has no start either, so it gets no failure frame, exactly as
      // it got no announcement.
      const line = requestedLine(call);
      if (line === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a shell call failed with no command line to restate; no failure frame is produced",
        );
        return undefined;
      }
      return {
        case: "bash",
        value: create(conversationv1.AgentBashSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentBashFailureSchema, {
              error: failureOf(outcome),
              command: bashCommand(call, line),
            }),
          },
        }),
      };
    }
    const typed = asRecord(outcome.structured);
    // A STATED EXIT IS ENOUGH TO SETTLE. The golden nonzero-exit result carries
    // no typed output whatsoever, and refusing a terminal for it would leave the
    // unit open forever over a command that plainly ran and ended.
    const record = typed ?? (exited === undefined ? undefined : {});
    if (record === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a command settled with no typed output; no terminal frame is produced",
      );
      return undefined;
    }
    if (exited !== undefined) {
      LOGGER.logVerbose(
        { tool_use_id: call.toolUseId, exit_code: exited },
        "a command exited non-zero; it COMPLETED, and the code is its own verdict",
      );
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
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a command settled with no command line to restate; no terminal frame is produced",
      );
      return undefined;
    }
    const output = bashOutput(call, record, outcome);
    if (output === undefined) return undefined;
    return {
      case: "bash",
      value: create(conversationv1.AgentBashSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentBashSuccessSchema, {
            command: bashCommand(call, line),
            outcome: bashOutcome(record, output, exited),
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

  /**
   * A FOREGROUND COMMAND THE TURN'S STOP CUT SHORT.
   *
   * The vendor returns NO `tool_result` for the call a stop landed inside — the
   * captured `interrupt` session records the stop as a bare
   * `[Request interrupted by user]` user line and nothing else — so without this
   * the unit never settles, and the card goes on drawing a running shell inside
   * a turn that ended minutes ago.
   *
   * `AgentBashInterrupted.cause = by_user` is the arm the contract already has
   * for exactly this, and this is what fills it on the stream plane: a stop is
   * not a failure of the call, so it rides the SUCCESS arm, precisely as a
   * vendor-reported `interrupted: true` result does.
   *
   * OUTPUT IS LEFT UNSET, deliberately. A foreground command's output arrives
   * once, whole, at a return this call never reached; an empty text arm would
   * tell the reader the command printed nothing, which is a different claim from
   * never having been told what it printed.
   */
  cut(call, atMs) {
    const line = requestedLine(call);
    if (line === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a stopped turn left a command open with no command line to restate; no terminal frame is produced",
      );
      return undefined;
    }
    return {
      case: "bash",
      value: create(conversationv1.AgentBashSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentBashSuccessSchema, {
            command: bashCommand(call, line),
            outcome: {
              case: "interrupted",
              value: create(conversationv1.AgentBashInterruptedSchema, {
                cause: {
                  case: "byUser",
                  value: create(conversationv1.AgentBashInterruptedByUserSchema, {}),
                },
              }),
            },
            settledAt: settledAt(atMs),
          }),
        },
      }),
    };
  },
};
