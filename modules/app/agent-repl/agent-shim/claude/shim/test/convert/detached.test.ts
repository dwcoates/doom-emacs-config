/**
 * WORK WE STOPPED BEING ABLE TO SEE.
 *
 * The producers here are called by the ENGINE, not by the fold: the only
 * stream-plane evidence of disappearance is a task leaving the
 * `background_tasks_changed` level, and reading that requires DIFFING the level,
 * which the contract forbids the fold to do. So these tests pin the SHAPE — the
 * arm a reader is told, which is the whole point of `lost` — and the ruling
 * itself belongs to whoever holds the live set.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import {
  lostAgentEntry,
  lostBashEntry,
  lostSubagentEntry,
  outputPathFromProse,
  wentSilent,
} from "../../src/convert/detached.js";
import { toolResultText } from "../../src/convert/entries.js";
import { foldContext, MAIN_AGENT } from "./fold-harness.js";

const RUN = create(conversationv1.AgentActivityIdSchema, { value: "run-1" });

/** The start a lost shell run's terminal restates. */
function start(): conversationv1.AgentBashStart {
  return create(conversationv1.AgentBashStartSchema, {
    command: create(conversationv1.AgentBashCommandSchema, { line: "tail -f log" }),
    startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: 5n }),
  });
}

describe("wentSilent", () => {
  it("is the arm for a run that produced nothing past the silence ruling", () => {
    expect(wentSilent().how.case).toBe("wentSilent");
  });
});

describe("lostBashEntry", () => {
  it("settles the run as interrupted by loss, restating the recorded command", () => {
    const entry = lostBashEntry(foldContext(), MAIN_AGENT, RUN, start(), wentSilent());

    const frame = entry?.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(success.command?.line).toBe("tail -f log");
    expect(interrupted.cause.case).toBe("lost");
  });

  it("states not_observed rather than claiming output it did not see", () => {
    // LANDING 5: distinct from empty text (a command that printed nothing) and
    // from a `partial` omission of zero bytes, which claimed we had seen all
    // none of what it printed.
    const entry = lostBashEntry(foldContext(), MAIN_AGENT, RUN, start(), wentSilent());

    const frame = entry?.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(interrupted.output?.form.case).toBe("notObserved");
  });

  it("refuses to invent a command when the record holds none", () => {
    const entry = lostBashEntry(
      foldContext(),
      MAIN_AGENT,
      RUN,
      create(conversationv1.AgentBashStartSchema, {}),
      wentSilent(),
    );

    expect(entry).toBeUndefined();
  });

  it("keys the row as the run's TERMINAL, which supersedes nothing", () => {
    // A run's rows are a SEQUENCE — start, deltas, terminal — so a terminal
    // sharing the start's key would erase the output the run produced.
    const entry = lostBashEntry(foldContext(), MAIN_AGENT, RUN, start(), wentSilent());

    expect(entry?.upsertKey).toBe("bash:run-1:terminal");
  });
});

describe("lostSubagentEntry", () => {
  it("settles the spawn unit's failure cause as lost, never as an error", () => {
    const entry = lostSubagentEntry(foldContext(), MAIN_AGENT, RUN, wentSilent());

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const activity = update.value as conversationv1.AgentActivity;
    const subagent = activity.item.value as conversationv1.AgentSubagent;
    const failed = subagent.result.value as conversationv1.AgentSubagentFailure;
    expect(failed.cause.case).toBe("lost");
    expect(failed.error).toBeUndefined();
  });
});

describe("lostAgentEntry", () => {
  it("closes the agent's own book with the lost arm", () => {
    const entry = lostAgentEntry(foldContext(), MAIN_AGENT, wentSilent());

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const failure = frame?.result.value as conversationv1.AgentFailure;
    expect(failure.failure.case).toBe("lost");
  });

  it("marks a keep-alive turn's loss never-served, like every other row of it", () => {
    const entry = lostAgentEntry(foldContext({ keepalive: true }), MAIN_AGENT, wentSilent());

    expect(entry.keepalive).toBe(true);
  });
});

describe("outputPathFromProse", () => {
  // `toolUseResult` on a backgrounded Bash carries `backgroundTaskId` and
  // NOTHING else (testdata/corpus/tool-results/bash-background.jsonl), so this
  // sentence is the vendor's only statement of where the output accumulates at
  // announcement time. Reading it wrong leaves every detached shell announced
  // with no output and its readability unset.

  it("reads the spool path out of the vendor's captured sentence", () => {
    // Arrange. Byte-for-byte the shape the corpus capture carries.
    const content = toolResultText(
      "Command running in background with ID: bvif9m46l. Output is being written to: " +
        "/private/tmp/claude-501/x/f2c3c473/tasks/bvif9m46l.output. You will be notified when it " +
        "completes. To check interim output, use Read on that file path.",
    );

    // Act, Assert.
    expect(outputPathFromProse(content)).toBe(
      "/private/tmp/claude-501/x/f2c3c473/tasks/bvif9m46l.output",
    );
  });

  it("stops at the sentence's period rather than swallowing it into the path", () => {
    // Arrange.
    const content = toolResultText("Output is being written to: /tmp/a.output. Then more prose.");

    // Act, Assert.
    expect(outputPathFromProse(content)).toBe("/tmp/a.output");
  });

  it("yields NO path when the vendor said something else, rather than a wrong one", () => {
    // A reworded sentence must degrade to an announcement with no output — the
    // task_notification still supplies the path at the end — never to a guess.
    // Arrange.
    const content = toolResultText("Command running in background with ID: b1.");

    // Act, Assert.
    expect(outputPathFromProse(content)).toBeUndefined();
  });

  it("yields no path when there is no content at all", () => {
    // Arrange, Act, Assert.
    expect(outputPathFromProse(undefined)).toBeUndefined();
  });
});
