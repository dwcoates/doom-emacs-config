/**
 * The bash converter. The one that must never be got wrong is the BACKGROUNDED
 * receipt: the vendor returns the same shape for a command that finished and
 * one it launched, and settling on the latter would say work ended when it
 * moved. Both corpus results drive that pair directly.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { bashConverter, shellCommandOf } from "../../../src/convert/tools/bash.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const AGENT = create(conversationv1.AgentIdSchema, { value: "session-1" });

function corpusResult(name: string): Record<string, unknown> {
  const path = fileURLToPath(
    new URL(`../../../../../../testdata/corpus/tool-results/${name}.jsonl`, import.meta.url),
  );
  const line = readFileSync(path, "utf8").split("\n").find((entry) => entry.trim() !== "");
  return (JSON.parse(line as string) as { toolUseResult: Record<string, unknown> }).toolUseResult;
}

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_bash",
    toolName: "Bash",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT,
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 1_700_000_001_000 };
}

/** A result whose only account of what happened is the text shown to the model. */
function outcomeSaying(text: string, isError: boolean, structured: unknown = text): ToolOutcome {
  return {
    content: create(conversationv1.ToolResultContentSchema, {
      blocks: [
        create(conversationv1.ToolResultContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
        }),
      ],
    }),
    isError,
    structured,
    settledAtMs: 1_700_000_001_000,
  };
}

function successOf(item: ReturnType<typeof bashConverter.settle>): conversationv1.AgentBashSuccess {
  return (item?.value as conversationv1.AgentBash).result.value as conversationv1.AgentBashSuccess;
}

function textOf(success: conversationv1.AgentBashSuccess): conversationv1.AgentBashOutputText {
  const completed = success.outcome.value as conversationv1.AgentBashCompleted;
  return completed.output?.form.value as conversationv1.AgentBashOutputText;
}

/** A result answering with one inlined base64 image block, as the vendor does. */
function outcomeShowingImage(mediaType: string, data: string): ToolOutcome {
  return {
    content: create(conversationv1.ToolResultContentSchema, {
      blocks: [
        create(conversationv1.ToolResultContentBlockSchema, {
          block: {
            case: "image",
            value: create(conversationv1.ImageBlockSchema, {
              mediaType,
              location: {
                case: "url",
                value: create(conversationv1.ImageBlockUrlSchema, {
                  url: `data:${mediaType};base64,${data}`,
                }),
              },
            }),
          },
        }),
      ],
    }),
    isError: false,
    structured: { stdout: data, stderr: "", interrupted: false, isImage: true },
    settledAtMs: 1_700_000_001_000,
  };
}

/** A result naming an image only by a fetchable url — no bytes anywhere. */
function outcomeShowingRemoteImage(mediaType: string): ToolOutcome {
  return {
    content: create(conversationv1.ToolResultContentSchema, {
      blocks: [
        create(conversationv1.ToolResultContentBlockSchema, {
          block: {
            case: "image",
            value: create(conversationv1.ImageBlockSchema, {
              mediaType,
              location: {
                case: "url",
                value: create(conversationv1.ImageBlockUrlSchema, {
                  url: "https://example.invalid/shot.png",
                }),
              },
            }),
          },
        }),
      ],
    }),
    isError: false,
    structured: { stdout: "", stderr: "", interrupted: false, isImage: true },
    settledAtMs: 1_700_000_001_000,
  };
}

/** The output a settled, completed bash item carried. */
function completedOutput(
  item: ReturnType<typeof bashConverter.settle>,
): conversationv1.AgentBashOutput | undefined {
  const bash = item?.value as conversationv1.AgentBash | undefined;
  if (bash?.result.case !== "success") return undefined;
  const outcomeArm = bash.result.value.outcome;
  if (outcomeArm.case !== "completed") return undefined;
  return outcomeArm.value.output;
}

describe("bashConverter.start", () => {
  it("announces the command line verbatim, from the corpus call", () => {
    // Arrange.
    const pending = call({ command: "pwd; ls | head" });

    // Act.
    const item = bashConverter.start(pending);

    // Assert.
    const start = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashStart;
    expect(start.command?.line).toBe("pwd; ls | head");
    expect(start.startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("leaves the description UNSET when the agent wrote none", () => {
    // Arrange, Act.
    const item = bashConverter.start(call({ command: "ls" }));

    // Assert.
    const start = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashStart;
    expect(start.command?.description).toBeUndefined();
  });

  it("says SANDBOXED for an ordinary command", () => {
    // Arrange, Act.
    const item = bashConverter.start(call({ command: "ls" }));

    // Assert.
    const start = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashStart;
    expect(start.command?.sandbox.case).toBe("sandboxed");
  });

  it("says SANDBOX DISABLED when the caller deliberately turned it off", () => {
    // Arrange, Act.
    const item = bashConverter.start(call({ command: "ls", dangerouslyDisableSandbox: true }));

    // Assert.
    const start = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashStart;
    expect(start.command?.sandbox.case).toBe("sandboxDisabled");
  });

  it("produces NO message when the call carried no command line", () => {
    // Arrange, Act.
    const item = bashConverter.start(call({ description: "does nothing" }));

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("bashConverter.settle", () => {
  it("settles the corpus foreground command as COMPLETED, with its stdout whole", () => {
    // Arrange.
    const result = corpusResult("bash");
    const pending = call({ command: "rg scroll" });

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result)));

    // Assert.
    expect(success.outcome.case).toBe("completed");
    expect(textOf(success).stdout).toBe(result["stdout"]);
    expect(textOf(success).extent.case).toBe("whole");
  });

  it("states NO termination for a foreground command, because no producer states one", () => {
    // Arrange.
    const pending = call({ command: "ls" });

    // Act.
    const success = successOf(
      bashConverter.settle(pending, outcome({ stdout: "a", stderr: "", interrupted: false })),
    );

    // Assert.
    expect((success.outcome.value as conversationv1.AgentBashCompleted).termination).toBeUndefined();
  });

  it("does NOT settle the corpus backgrounded launch: the command moved, it did not end", () => {
    // Arrange.
    const pending = call({ command: "npm test", run_in_background: true });

    // Act.
    const item = bashConverter.settle(pending, outcome(corpusResult("bash-background")));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("keeps stderr apart from stdout rather than interleaving them", () => {
    // Arrange.
    const pending = call({ command: "ls /nope" });

    // Act.
    const success = successOf(
      bashConverter.settle(pending, outcome({ stdout: "out", stderr: "err", interrupted: false })),
    );

    // Assert.
    expect(textOf(success).stdout).toBe("out");
    expect(textOf(success).stderr).toBe("err");
  });

  it("SUBTRACTS the omitted bytes and names the spill when the output was too large", () => {
    // Arrange.
    const pending = call({ command: "cat big" });
    const result = {
      stdout: "abcde",
      stderr: "",
      interrupted: false,
      persistedOutputPath: "/tmp/out.txt",
      persistedOutputSize: 1_005,
    };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result)));

    // Assert.
    expect(textOf(success).extent.value).toEqual(
      create(conversationv1.AgentBashOutputPartialSchema, {
        bytesOmitted: 1_000n,
        spilled: create(conversationv1.AgentBashSpilledOutputSchema, {
          path: "/tmp/out.txt",
          sizeBytes: 1_005n,
        }),
      }),
    );
  });

  it("leaves the spill UNSET when the producer kept nothing: the omitted bytes are gone", () => {
    // Arrange.
    const pending = call({ command: "cat big" });
    const result = { stdout: "abcde", stderr: "", interrupted: false, persistedOutputSize: 1_005 };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result)));

    // Assert.
    const partial = textOf(success).extent.value as conversationv1.AgentBashOutputPartial;
    expect(partial.spilled).toBeUndefined();
  });

  it("says INTERRUPTED BY USER when a person stopped it", () => {
    // Arrange.
    const pending = call({ command: "sleep 999" });

    // Act.
    const success = successOf(
      bashConverter.settle(pending, outcome({ stdout: "", stderr: "", interrupted: true })),
    );

    // Assert.
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(success.outcome.case).toBe("interrupted");
    expect(interrupted.cause.case).toBe("byUser");
  });

  it("says TIMED OUT with the CONFIGURED limit when the command outlived its timeout", () => {
    // Arrange.
    const pending = call({ command: "sleep 999", timeout: 5_000 });
    const result = { stdout: "", stderr: "", interrupted: true, timedOutAfterMs: 5_000 };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result)));

    // Assert.
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(interrupted.cause.value).toEqual(
      create(conversationv1.AgentBashInterruptedByTimeoutSchema, { timeoutMs: 5_000n }),
    );
  });

  it("says COMPLETED with the exit code when a non-zero run is marked an error", () => {
    // A NON-ZERO EXIT IS THE COMMAND'S OWN VERDICT ON ITSELF. The vendor marks
    // the result an error FOR THE MODEL and states the ending in
    // `returnCodeInterpretation`; reading `isError` alone drew every such run
    // as `AgentBashFailure`, which tells the user the shell broke.
    // Arrange.
    const pending = call({ command: "exit 3" });
    const result = {
      stdout: "",
      stderr: "boom\n",
      interrupted: false,
      returnCodeInterpretation: "exited with code 3",
    };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result, true)));

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(success.outcome.case).toBe("completed");
    expect(completed.termination?.how.value).toEqual(
      create(conversationv1.AgentBashExitedSchema, { code: 3 }),
    );
  });

  it("leaves termination UNSET when the result states no status at all", () => {
    // The honest shape for the foreground path: nothing reported how the shell
    // ended, and a synthesized `exited(0)` would be a fact the shim invented.
    // Arrange.
    const pending = call({ command: "echo hi" });

    // Act.
    const success = successOf(
      bashConverter.settle(pending, outcome({ stdout: "hi\n", stderr: "", interrupted: false })),
    );

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(completed.termination).toBeUndefined();
  });

  it("still says FAILED when an errored result states no exit status", () => {
    // The coverage the change above must not erase: a call that could not be
    // performed at all has no status to report, and it is still a failure.
    // Arrange.
    const pending = call({ command: "nope" });

    // Act.
    const item = bashConverter.settle(pending, outcome({ stdout: "", stderr: "" }, true));

    // Assert.
    expect((item?.value as conversationv1.AgentBash).result.case).toBe("failure");
  });

  it("restates the command on the failure arm, so the settled frame stands alone", () => {
    // Arrange.
    const pending = call({ command: "nope", description: "try it" });

    // Act.
    const item = bashConverter.settle(pending, outcome({ stdout: "", stderr: "" }, true));

    // Assert.
    const failure = (item?.value as conversationv1.AgentBash).result.value as conversationv1.AgentBashFailure;
    expect({ line: failure.command?.line, description: failure.command?.description }).toEqual({
      line: "nope",
      description: "try it",
    });
  });

  it("produces NO failure frame for a call that named no command, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(bashConverter.settle(call({}), outcome({ stdout: "", stderr: "" }, true))).toBeUndefined();
  });

  it("produces NO frame for IMAGE output whose result carries no image block", () => {
    // Arrange: `isImage` alone states no media type and no bytes, and naming
    // either would be inventing a fact the vendor never stated.
    const pending = call({ command: "screencapture -" });

    // Act.
    const item = bashConverter.settle(
      pending,
      outcome({ stdout: "iVBORw0KG", stderr: "", interrupted: false, isImage: true }),
    );

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries IMAGE output as the image arm, read off the answering result block", () => {
    // Arrange.
    const pending = call({ command: "screencapture -" });

    // Act.
    const item = bashConverter.settle(
      pending,
      outcomeShowingImage("image/png", "iVBORw=="),
    );

    // Assert.
    const output = completedOutput(item);
    expect(output?.form.case).toBe("image");
    const image = output?.form.value as conversationv1.AgentBashOutputImage;
    expect(image.mediaType).toBe("image/png");
    expect(Buffer.from(image.data).toString("binary")).toBe("\u0089PNG");
  });

  it("produces NO frame for an image block whose payload does not decode to bytes", () => {
    // Arrange: an empty payload is no picture, and half an image is worse than
    // none.
    const pending = call({ command: "screencapture -" });

    // Act.
    const item = bashConverter.settle(pending, outcomeShowingImage("image/png", ""));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame for an image the result names only by fetchable url", () => {
    // Arrange: a url block carries no bytes at all, and AgentBashOutputImage is
    // the bytes.
    const pending = call({ command: "screencapture -" });

    // Act.
    const item = bashConverter.settle(pending, outcomeShowingRemoteImage("image/png"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("a NONZERO EXIT is still the success arm: the command ran and answered", () => {
    // Arrange.
    const pending = call({ command: "false" });

    // Act.
    const item = bashConverter.settle(
      pending,
      outcome({ stdout: "", stderr: "boom", interrupted: false }),
    );

    // Assert.
    expect((item?.value as conversationv1.AgentBash).result.case).toBe("success");
  });

  it("reads the exit the GOLDEN states in its text, which carries no typed output", () => {
    // The captured bash-nonzero-exit shape: the vendor marks the result an
    // error and its whole account is the bare string it returned, so the
    // command's own verdict on itself survives only there. The file plane mines
    // it, and the two planes must agree on the same capture.
    // Arrange.
    const pending = call({ command: "bash fail.sh" });

    // Act.
    const success = successOf(
      bashConverter.settle(
        pending,
        outcomeSaying("Exit code 7\npartway\nto stderr", true, "Error: Exit code 7\npartway\nto stderr"),
      ),
    );

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(success.outcome.case).toBe("completed");
    expect(completed.termination?.how.value).toEqual(
      create(conversationv1.AgentBashExitedSchema, { code: 7 }),
    );
  });

  it("PREFERS the structured status over the one the returned text names", () => {
    // A result that states its ending outright has told us the status; the text
    // is a fallback for results that carry no typed output at all.
    // Arrange.
    const pending = call({ command: "./run" });
    const structured = {
      stdout: "",
      stderr: "",
      interrupted: false,
      returnCodeInterpretation: "exited with code 3",
    };

    // Act.
    const success = successOf(
      bashConverter.settle(pending, outcomeSaying("Exit code 7", true, structured)),
    );

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(completed.termination?.how.value).toEqual(
      create(conversationv1.AgentBashExitedSchema, { code: 3 }),
    );
  });

  it("reads the structured `exitCode` when the result carries no interpretation prose", () => {
    // The precise datum, alone, is enough: it need not be accompanied by
    // `returnCodeInterpretation` to be trusted.
    // Arrange.
    const pending = call({ command: "exit 4" });
    const result = { stdout: "", stderr: "", interrupted: false, exitCode: 4 };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result, true)));

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(completed.termination?.how.value).toEqual(
      create(conversationv1.AgentBashExitedSchema, { code: 4 }),
    );
  });

  it("reads `return_code_interpretation`, the vendor's snake_case spelling, alone", () => {
    // The disk carries both spellings of the same field; the snake_case one is
    // read exactly where the camelCase one would be, when it is all that is
    // there.
    // Arrange.
    const pending = call({ command: "exit 5" });
    const result = {
      stdout: "",
      stderr: "",
      interrupted: false,
      return_code_interpretation: "exited with code 5",
    };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result, true)));

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(completed.termination?.how.value).toEqual(
      create(conversationv1.AgentBashExitedSchema, { code: 5 }),
    );
  });

  it("PREFERS the structured `exitCode` over `returnCodeInterpretation` when they disagree", () => {
    // Pinned precedence: `exitCode` is the precise datum, the interpretation
    // string is prose ABOUT it, so `exitCode` wins whenever a result somehow
    // carries both. This must never drift back toward reading the prose first.
    // Arrange.
    const pending = call({ command: "./run" });
    const result = {
      stdout: "",
      stderr: "",
      interrupted: false,
      exitCode: 9,
      returnCodeInterpretation: "exited with code 3",
    };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result, true)));

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(completed.termination?.how.value).toEqual(
      create(conversationv1.AgentBashExitedSchema, { code: 9 }),
    );
  });

  it("still says FAILED when an errored result's TEXT names no exit either", () => {
    // Mining the text must not turn a call that could not be performed into a
    // command that ran: text naming no ending yields no status.
    // Arrange, Act.
    const item = bashConverter.settle(
      call({ command: "nope" }),
      outcomeSaying("Error: EACCES: permission denied", true),
    );

    // Assert.
    expect((item?.value as conversationv1.AgentBash).result.case).toBe("failure");
  });

  it("reads NO exit from a marker the vendor did not put at the start of a line", () => {
    // The statement is a line of its own, so words inside a line of output are
    // prose the command printed, not a verdict on how it ended.
    // Arrange, Act.
    const item = bashConverter.settle(
      call({ command: "make" }),
      outcomeSaying("make: recipe returned exit code 2 for the stale target\nError: EACCES", true),
    );

    // Assert.
    expect((item?.value as conversationv1.AgentBash).result.case).toBe("failure");
  });

  it("never mines the OUTPUT of a command the vendor did not mark an error", () => {
    // A passing command that printed the words said nothing about its ending,
    // and reading them would invent a status the shell never reported.
    // Arrange.
    const pending = call({ command: "cat notes.txt" });
    const structured = { stdout: "we saw exit code 3 last week\n", stderr: "", interrupted: false };

    // Act.
    const success = successOf(
      bashConverter.settle(pending, outcomeSaying("we saw exit code 3 last week", false, structured)),
    );

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(success.outcome.case).toBe("completed");
    expect(completed.termination).toBeUndefined();
  });

  it("claims ZERO omitted bytes when the persisted total trails the inline output", () => {
    // Arrange.
    const pending = call({ command: "cat small" });
    const result = { stdout: "abcde", stderr: "", interrupted: false, persistedOutputSize: 2 };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result)));

    // Assert.
    const partial = textOf(success).extent.value as conversationv1.AgentBashOutputPartial;
    expect(partial.bytesOmitted).toBe(0n);
  });

  it("states NO termination when the interpretation prose names no number at all", () => {
    // Arrange.
    const pending = call({ command: "true" });
    const result = {
      stdout: "",
      stderr: "",
      interrupted: false,
      returnCodeInterpretation: "completed normally",
    };

    // Act.
    const success = successOf(bashConverter.settle(pending, outcome(result)));

    // Assert.
    const completed = success.outcome.value as conversationv1.AgentBashCompleted;
    expect(completed.termination).toBeUndefined();
  });

  it("produces NO terminal frame when a settled command has no command line to restate", () => {
    // Arrange.
    const pending = call({});

    // Act.
    const item = bashConverter.settle(pending, outcome({ stdout: "", stderr: "", interrupted: false }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("carries the failure arm when the CALL could not be performed at all", () => {
    // Arrange, Act.
    const item = bashConverter.settle(call({ command: "rm -rf /" }), outcome("denied", true));

    // Assert.
    const bash = item?.value as conversationv1.AgentBash;
    expect(bash.result.case).toBe("failure");
    expect((bash.result.value as conversationv1.AgentBashFailure).error?.settledAt?.atMs).toBe(
      1_700_000_001_000n,
    );
  });
});

describe("bashConverter.progress", () => {
  it("relays the vendor's beat on the command's own progress arm", () => {
    // Arrange, Act.
    const item = bashConverter.progress?.(toolProgress(1_700_000_000_500));

    // Assert.
    expect((item?.value as conversationv1.AgentBash).result.case).toBe("progress");
  });
});

describe("bashConverter.cut", () => {
  // THE STOP IS THE ONLY THING THAT WILL EVER SETTLE THESE. The vendor returns
  // no `tool_result` for a call a stop landed inside, so without the cut the
  // unit stays open forever and the card draws a running shell inside a turn
  // that ended.
  it("settles a held command on the interrupted arm, caused BY THE USER", () => {
    // Arrange, Act.
    const item = bashConverter.cut?.(
      call({ command: "tail -f /var/log/system.log" }),
      1_700_000_002_000,
    );

    // Assert.
    const success = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(success.outcome.case).toBe("interrupted");
    expect(interrupted.cause.case).toBe("byUser");
  });

  it("restates the command line the cut call was announced with", () => {
    // Arrange, Act.
    const item = bashConverter.cut?.(call({ command: "sleep 600" }), 1_700_000_002_000);

    // Assert: every frame stands alone, a cut one included.
    const success = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashSuccess;
    expect(success.command?.line).toBe("sleep 600");
  });

  it("stamps the instant the stop landed, not the instant the call began", () => {
    // Arrange, Act.
    const item = bashConverter.cut?.(call({ command: "sleep 600" }), 1_700_000_002_000);

    // Assert.
    const success = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashSuccess;
    expect(success.settledAt?.atMs).toBe(1_700_000_002_000n);
  });

  it("leaves the OUTPUT unset, because the call never reached its return", () => {
    // Arrange, Act.
    const item = bashConverter.cut?.(call({ command: "sleep 600" }), 1_700_000_002_000);

    // Assert: an empty text arm would say the command printed nothing, which is
    // a different claim from never having been told what it printed.
    const success = (item?.value as conversationv1.AgentBash).result
      .value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(interrupted.output).toBeUndefined();
  });

  it("produces NO frame for a call announced with no command line to restate", () => {
    // Arrange: a malformed announcement. A frame with no command line states
    // nothing a reader could act on, and the store refuses an unset arm.
    const item = bashConverter.cut?.(call({}), 1_700_000_002_000);

    // Assert.
    expect(item).toBeUndefined();
  });
});

describe("shellCommandOf", () => {
  it("restates the call's command line", () => {
    // Arrange, Act.
    const command = shellCommandOf(call({ command: "sleep 3" }));

    // Assert.
    expect(command?.line).toBe("sleep 3");
  });

  it("names no command for a call that named no command line", () => {
    // Arrange, Act, Assert.
    expect(shellCommandOf(call({}))).toBeUndefined();
  });
});
