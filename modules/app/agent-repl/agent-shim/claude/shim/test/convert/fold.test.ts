/**
 * The FOLD, driven with the real corpus.
 *
 * One test per behavior the contract names, and every input is either a real
 * anonymized capture from `testdata/corpus` or a sequence assembled from those
 * captures the way the SDK emits one. A converter that agrees only with our own
 * idea of the vendor's shapes fails here.
 */
import { writeSync } from "node:fs";
import { describe, expect, it, vi } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import { EMPTY_FOLD_OUTPUT, createFold, type FoldOutput } from "../../src/convert/fold.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import { activityOf, foldContext, residueOf, streamMessage } from "./fold-harness.js";
import { goldenContext, scenarioNames as captureNames, sdkMessages } from "./goldens/harness.js";
import { driveScenario, expectDroveCleanly } from "../fake/harness.js";

const mockedWriteSync = vi.mocked(writeSync);

/** An assistant message the SDK would emit for one API response. */
function assistant(
  messageId: string,
  content: unknown[],
  extra: Record<string, unknown> = {},
): SdkMessage {
  return {
    type: "assistant",
    uuid: `uuid-${messageId}`,
    session_id: "session-1",
    parent_tool_use_id: null,
    message: {
      id: messageId,
      type: "message",
      role: "assistant",
      model: "claude-opus-5",
      content,
      stop_reason: null,
      usage: {
        input_tokens: 10,
        cache_creation_input_tokens: 3224,
        cache_read_input_tokens: 21755,
        output_tokens: 36,
        output_tokens_details: { reasoning_tokens: 30 },
      },
      ...(extra.message as Record<string, unknown> | undefined),
    },
    // `message` is spread INTO the message above, never over it: a top-level
    // spread would replace the whole API message with the override.
    ...Object.fromEntries(Object.entries(extra).filter(([key]) => key !== "message")),
  } as unknown as SdkMessage;
}

/** A user record carrying one tool result, as the SDK emits one. */
function toolResult(toolUseId: string, structured: unknown, isError = false): SdkMessage {
  return {
    type: "user",
    uuid: `uuid-result-${toolUseId}`,
    session_id: "session-1",
    parent_tool_use_id: null,
    tool_use_result: structured,
    message: {
      role: "user",
      content: [{ type: "tool_result", tool_use_id: toolUseId, content: "ok", is_error: isError }],
    },
  } as unknown as SdkMessage;
}

describe("EMPTY_FOLD_OUTPUT", () => {
  it("produces no rows, which is the exempt set's answer", () => {
    expect(EMPTY_FOLD_OUTPUT).toEqual({ entries: [] });
  });
});

describe("prose", () => {
  it("opens a response unit on the content block's start", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());

    const output = fold.onSdkMessage(
      streamMessage("stream_event-content_block_start"),
      foldContext(),
    );

    const activity = activityOf(output.entries[0]);
    expect(activity?.item.case).toBe("response");
    expect((activity?.item.value as conversationv1.AgentResponse).result.case).toBe("start");
  });

  it("identifies a block as <message.id>:<block_index>", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());

    const output = fold.onSdkMessage(
      streamMessage("stream_event-content_block_start"),
      foldContext(),
    );

    // The corpus's block_start carries index 1 of msg_011CdKQJaCBXrp4nizdg3fXW.
    expect(activityOf(output.entries[0])?.activityId?.value).toBe(
      "msg_011CdKQJaCBXrp4nizdg3fXW:1",
    );
  });

  it("forwards a text delta as a DELTA, never as the whole text", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());

    const output = fold.onSdkMessage(
      streamMessage("stream_event-content_block_delta-text"),
      foldContext(),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const update = response.result.value as conversationv1.AgentResponseUpdate;
    expect(update.newMarkdown).toBe("hi");
  });

  it("settles a prose block from the assistant message, whole and authored by the model", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("assistant"), foldContext());

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const success = response.result.value as conversationv1.AgentResponseSuccess;
    expect(success.prose?.markdown).toBe("hi");
    expect(success.authorship.case).toBe("fromModel");
  });

  it("carries the record's own timestamp as the settle instant, not the wall clock", () => {
    // A settle instant stamped from the wall clock at conversion time is not
    // replay-stable; the record's own timestamp is. The corpus record's
    // timestamp is 2026-07-23T17:42:47.752Z; the deliberately-different nowMs
    // must NOT win.
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("assistant"), foldContext({ nowMs: 4242 }));

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const success = response.result.value as conversationv1.AgentResponseSuccess;
    expect(success.settledAt?.atMs).toBe(1784828567752n);
  });

  it("falls back to the live clock for a settled prose block whose record has no timestamp", () => {
    // The last resort ONLY: a record the vendor gave no timestamp keeps the one
    // instant we observed rather than dropping onto the daemon's compose-time Now.
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-no-ts", [{ type: "text", text: "hi" }]),
      foldContext({ nowMs: 4242 }),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const success = response.result.value as conversationv1.AgentResponseSuccess;
    expect(success.settledAt?.atMs).toBe(4242n);
  });

  it("draws a vendor-synthesized notice as a notice, never as the agent's answer", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-notice", [{ type: "text", text: "API Error: overloaded" }], {
        error: "overloaded",
      }),
      foldContext(),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const success = response.result.value as conversationv1.AgentResponseSuccess;
    expect(success.authorship.case).toBe("synthesizedNotice");
  });

  it("settles a max-tokens stop as a response FAILURE carrying what was said", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-cut", [{ type: "text", text: "half a sen" }], {
        message: { stop_reason: "max_tokens" },
      }),
      foldContext(),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const failure = response.result.value as conversationv1.AgentResponseFailure;
    expect(response.result.case).toBe("failure");
    expect(failure.reason?.reason.case).toBe("maxTokens");
  });

  it("settles an interrupted message as aborted", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-abort", [{ type: "text", text: "half" }], { aborted: true }),
      foldContext(),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const failure = response.result.value as conversationv1.AgentResponseFailure;
    expect(failure.reason?.reason.case).toBe("aborted");
  });

  it("carries the record's own timestamp as the settle instant on a failed prose block", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-cut", [{ type: "text", text: "half a sen" }], {
        message: { stop_reason: "max_tokens" },
        timestamp: "2026-07-23T17:42:47.752Z",
      }),
      foldContext({ nowMs: 4242 }),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const failure = response.result.value as conversationv1.AgentResponseFailure;
    expect(failure.settledAt?.atMs).toBe(1784828567752n);
  });

  it("falls back to the live clock on a failed prose block whose record has no timestamp", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-cut", [{ type: "text", text: "half a sen" }], {
        message: { stop_reason: "max_tokens" },
      }),
      foldContext({ nowMs: 4242 }),
    );

    const response = activityOf(output.entries[0])?.item.value as conversationv1.AgentResponse;
    const failure = response.result.value as conversationv1.AgentResponseFailure;
    expect(failure.settledAt?.atMs).toBe(4242n);
  });
});

describe("reasoning", () => {
  /** The corpus's block_start, re-aimed at a reasoning block of index 0. */
  function thinkingStart(): SdkMessage {
    const start = streamMessage("stream_event-content_block_start") as unknown as Record<
      string,
      unknown
    >;
    return {
      ...start,
      event: { type: "content_block_start", index: 0, content_block: { type: "thinking" } },
    } as unknown as SdkMessage;
  }

  it("opens a thinking unit on a thinking block's start", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());

    const output = fold.onSdkMessage(thinkingStart(), foldContext());

    expect(activityOf(output.entries[0])?.item.case).toBe("thinking");
  });

  it("forwards a thinking delta as reasoning text", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());

    const output = fold.onSdkMessage(
      streamMessage("stream_event-content_block_delta-thinking"),
      foldContext(),
    );

    const thinking = activityOf(output.entries[0])?.item.value as conversationv1.AgentThinking;
    const update = thinking.result.value as conversationv1.AgentThinkingUpdate;
    expect(update.reasoning.case).toBe("text");
  });

  it("settles a redacted reasoning block as WITHHELD, so nothing is drawn for it", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-think", [{ type: "redacted_thinking" }]),
      foldContext(),
    );

    const thinking = activityOf(output.entries[0])?.item.value as conversationv1.AgentThinking;
    const success = thinking.result.value as conversationv1.AgentThinkingSuccess;
    expect(success.reasoning.case).toBe("withheld");
  });

  it("relays the vendor's live thinking-token estimate onto the open block", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());
    fold.onSdkMessage(thinkingStart(), foldContext());

    const output = fold.onSdkMessage(streamMessage("thinking_tokens"), foldContext());

    const thinking = activityOf(output.entries[0])?.item.value as conversationv1.AgentThinking;
    const update = thinking.result.value as conversationv1.AgentThinkingUpdate;
    expect(update.estimated?.estimatedTokens).toBe(7n);
  });

  it("consumes a signature delta, which carries no conversation content", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("stream_event-message_start"), foldContext());

    const output = fold.onSdkMessage(
      streamMessage("stream_event-content_block_delta-signature"),
      foldContext(),
    );

    expect(output.entries).toHaveLength(0);
  });
});

describe("usage", () => {
  it("rides the FIRST content block's unit", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-usage", [
        { type: "text", text: "first" },
        { type: "text", text: "second" },
      ]),
      foldContext(),
    );

    expect(activityOf(output.entries[0])?.usage).toBeDefined();
    expect(activityOf(output.entries[1])?.usage).toBeUndefined();
  });

  it("maps the vendor's counters onto the canonical token shape", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-usage", [{ type: "text", text: "x" }]),
      foldContext(),
    );

    const usage = activityOf(output.entries[0])?.usage;
    expect(usage?.inputHits?.read).toBe(21755n);
    expect(usage?.inputMisses?.written).toBe(3224n);
    expect(usage?.inputMisses?.unwritten).toBe(10n);
    expect(usage?.outputTokens).toBe(36n);
    expect(usage?.outputThinkingTokens).toBe(30n);
  });
});

describe("tool calls", () => {
  it("opens a typed unit for a modelled tool", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-tool", [
        { type: "tool_use", id: "toolu_1", name: "Read", input: { file_path: "/tmp/a" } },
      ]),
      foldContext(),
    );

    expect(activityOf(output.entries[0])?.item.case).toBe("read");
  });

  it("identifies a tool unit by the vendor's own tool_use_id", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-tool", [
        { type: "tool_use", id: "toolu_1", name: "Read", input: { file_path: "/tmp/a" } },
      ]),
      foldContext(),
    );

    expect(activityOf(output.entries[0])?.activityId?.value).toBe("toolu_1");
  });

  it("settles a tool unit from its result, under the SAME identity", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-tool", [
        { type: "tool_use", id: "toolu_1", name: "Read", input: { file_path: "/tmp/a" } },
      ]),
      foldContext(),
    );

    const output = fold.onSdkMessage(
      toolResult("toolu_1", {
        type: "text",
        file: { filePath: "/tmp/a", content: "x", numLines: 1, startLine: 1, totalLines: 1 },
      }),
      foldContext(),
    );

    const read = activityOf(output.entries[0])?.item.value as conversationv1.AgentRead;
    expect(activityOf(output.entries[0])?.activityId?.value).toBe("toolu_1");
    expect(read.result.case).toBe("success");
  });

  it("drops an exempt tool entirely — no unit, and never AgentUnmodeled", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-exempt", [{ type: "tool_use", id: "toolu_x", name: "TaskList", input: {} }]),
      foldContext(),
    );

    expect(output.entries).toHaveLength(0);
  });

  it("consumes an exempt tool's RESULT too, rather than residuing it", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-exempt", [{ type: "tool_use", id: "toolu_x", name: "TaskList", input: {} }]),
      foldContext(),
    );

    const output = fold.onSdkMessage(toolResult("toolu_x", { tasks: [] }), foldContext());

    expect(output.entries).toHaveLength(0);
  });

  it("produces nothing for a tool the engine's gate owns", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-ask", [
        { type: "tool_use", id: "toolu_q", name: "AskUserQuestion", input: { questions: [] } },
      ]),
      foldContext(),
    );

    expect(output.entries).toHaveLength(0);
  });

  it("makes an unknown tool an unmodeled unit", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-unknown", [{ type: "tool_use", id: "toolu_u", name: "StructuredOutput", input: {} }]),
      foldContext(),
    );

    expect(activityOf(output.entries[0])?.item.case).toBe("unmodeled");
  });

  it("makes an MCP server's tool an ordinary MCP tool call, never unmodeled", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-mcp", [
        { type: "tool_use", id: "toolu_m", name: "mcp__Slack__send", input: { text: "hi" } },
      ]),
      foldContext(),
    );

    expect(activityOf(output.entries[0])?.item.case).toBe("mcpToolCall");
  });

  it("resolves an MCP tool's address by LOOKUP against the names the session knows", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-mcp", [{ type: "tool_use", id: "toolu_m", name: "mcp__Slack__send", input: {} }]),
      foldContext({ mcpServerNames: ["Slack"] }),
    );

    const mcp = activityOf(output.entries[0])?.item.value as conversationv1.AgentMcpToolCall;
    const start = mcp.result.value as conversationv1.AgentMcpToolCallStart;
    expect(start.tool?.address?.server).toBe("Slack");
  });

  it("leaves the MCP address unset when no known name matches, rather than guessing", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-mcp", [{ type: "tool_use", id: "toolu_m", name: "mcp__Slack__send", input: {} }]),
      foldContext({ mcpServerNames: ["Gmail"] }),
    );

    const mcp = activityOf(output.entries[0])?.item.value as conversationv1.AgentMcpToolCall;
    const start = mcp.result.value as conversationv1.AgentMcpToolCallStart;
    expect(start.tool?.address).toBeUndefined();
  });

  it("relays a progress beat on a kind that declares the arm", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-tool", [
        { type: "tool_use", id: "toolu_1", name: "Read", input: { file_path: "/tmp/a" } },
      ]),
      foldContext(),
    );

    const output = fold.onSdkMessage(
      {
        type: "tool_progress",
        tool_use_id: "toolu_1",
        tool_name: "Read",
        parent_tool_use_id: null,
        elapsed_time_seconds: 3,
        uuid: "uuid-beat",
        session_id: "session-1",
      } as unknown as SdkMessage,
      foldContext(),
    );

    const read = activityOf(output.entries[0])?.item.value as conversationv1.AgentRead;
    expect(read.result.case).toBe("progress");
  });
});

describe("the shell that moved rather than ended", () => {
  it("announces the detachment and does NOT settle the unit", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-bash", [
        {
          type: "tool_use",
          id: "toolu_b",
          name: "Bash",
          input: { command: "sleep 100", run_in_background: true },
        },
      ]),
      foldContext(),
    );

    const output = fold.onSdkMessage(
      toolResult("toolu_b", { stdout: "", stderr: "", interrupted: false, backgroundTaskId: "b1" }),
      foldContext(),
    );

    // The run's START row rides ahead of the announcement (the shim writes a
    // detached shell's lifecycle), and the spool's claim follows it; nothing
    // settles the unit itself.
    const frame =
      output.entries[1]?.item.kind === "frame" ? output.entries[1].item.frame : undefined;
    expect(frame?.result.case).toBe("detachedWork");
    expect(output.entries.map((entry) => entry.item.kind)).toEqual(["bash_run", "frame", "shell_run_claim"]);
  });

  it("claims the spool of a shell its RESULT moved, so a restarted sidecar can find it", () => {
    // A Ctrl-B or a timeout moves a foreground shell, and its result may be
    // the only record that says so. A sidecar restarted after it rewinds no
    // further than the turn in progress, so the claim in the store is how it
    // reads the spool again (2026-09-30: a run orphaned this way stayed open).
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-bash", [
        { type: "tool_use", id: "toolu_b", name: "Bash", input: { command: "sleep 100" } },
      ]),
      foldContext(),
    );

    const output = fold.onSdkMessage(
      toolResult("toolu_b", { stdout: "", stderr: "", interrupted: false, backgroundTaskId: "b1", backgroundedByUser: true }),
      foldContext(),
    );

    const claim = output.entries.flatMap((entry) => (entry.item.kind === "shell_run_claim" ? [entry.item.claim] : []))[0];
    expect(claim?.vendorTaskId).toBe("b1");
    expect(claim?.run?.value).toBe("toolu_b");
  });

  it("claims no spool for a shell that ENDED", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-bash", [{ type: "tool_use", id: "toolu_b", name: "Bash", input: { command: "true" } }]),
      foldContext(),
    );

    const output = fold.onSdkMessage(
      toolResult("toolu_b", { stdout: "", stderr: "", interrupted: false }),
      foldContext(),
    );

    expect(output.entries.some((entry) => entry.item.kind === "shell_run_claim")).toBe(false);
  });

  it("harvests the cause from the BASH TOOL RESULT, not from the task stream", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-bash", [
        { type: "tool_use", id: "toolu_b", name: "Bash", input: { command: "sleep 100" } },
      ]),
      foldContext(),
    );

    const output = fold.onSdkMessage(
      toolResult("toolu_b", {
        stdout: "",
        stderr: "",
        interrupted: false,
        backgroundTaskId: "b1",
        timedOutAfterMs: 120_000,
      }),
      foldContext(),
    );

    const frame = output.entries.flatMap((entry) =>
      entry.item.kind === "frame" ? [entry.item.frame] : [],
    )[0];
    const detached = frame?.result.value as conversationv1.AgentDetachedWork;
    const origin = detached.origin.value as conversationv1.DetachedWorkDetached;
    expect(origin.cause.case).toBe("timedOut");
  });
});

describe("detached work", () => {
  it("announces an AGENT task that left the turn", () => {
    // The corpus's only real `task_started` is a `local_bash` one, so the
    // AGENT case is that record with its `task_type` changed — a field value,
    // not an invented shape.
    const fold = createFold();
    const agentTask = {
      ...(streamMessage("task_started") as unknown as Record<string, unknown>),
      task_type: "local_agent",
    } as unknown as SdkMessage;

    const output = fold.onSdkMessage(agentTask, foldContext());

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    expect(frame?.result.case).toBe("detachedWork");
  });

  it("announces NOTHING for a shell task's start, and only claims its spool", () => {
    // `task_started` says neither why the work left the turn nor anything the
    // announcement needs: the Bash result is the only record that states the
    // cause, and announcing `requested` here put a wrong-cause announcement on
    // the stream ahead of the right one. It does state the spool's task id and
    // the run, which is the claim the sidecar reads the spool by.
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("task_started"), foldContext());

    expect(output.entries.map((entry) => entry.item.kind)).toEqual(["shell_run_claim"]);
  });

  it("consumes the background-task LEVEL without recording it", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("background_tasks_changed"), foldContext());

    expect(output.entries).toHaveLength(0);
  });

  it("settles an async run on its notification, with the thin usage the vendor gives", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("task_notification"), foldContext());

    const settled = output.entries.find((entry) => activityOf(entry)?.item.case === "subagent");
    const subagent = activityOf(settled)?.item.value as conversationv1.AgentSubagent;
    const success = subagent.result.value as conversationv1.AgentSubagentSuccess;
    expect(success.totals?.usage.case).toBe("totalOnly");
  });

  it("drops a skip_transcript task from every announcement", () => {
    const fold = createFold();
    const started = streamMessage("task_started") as unknown as Record<string, unknown>;

    const output = fold.onSdkMessage(
      { ...started, skip_transcript: true } as unknown as SdkMessage,
      foldContext(),
    );

    expect(output.entries).toHaveLength(0);
  });
});

describe("a subagent resumed by a send whose spawn this fold never saw", () => {
  /** A fold holding an open `SendMessage` call, as a restarted process's does. */
  const foldWithOpenSend = (): ReturnType<typeof createFold> => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-send", [{ type: "tool_use", id: "toolu_send", name: "SendMessage", input: { to: "a5583", message: "go on" } }]),
      foldContext(),
    );
    return fold;
  };
  const resumed = {
    type: "system",
    subtype: "task_started",
    uuid: "uuid-resume",
    session_id: "session-1",
    task_id: "a5583",
    tool_use_id: "toolu_send",
    task_type: "local_agent",
    description: "resumed",
  } as unknown as SdkMessage;
  const SPAWN = create(conversationv1.AgentIdSchema, { value: "toolu_spawn" });

  it("names the resumed task as awaiting the store's answer", () => {
    // Arrange.
    const fold = foldWithOpenSend();

    // Act, Assert.
    expect(fold.taskAwaitingAgent(resumed, foldContext())).toBe("a5583");
  });

  const COMMISSION = create(conversationv1.AgentSubagentPromptSchema, { text: "go", description: "fix the shim" });

  /** The subagent kind the resume's announcement states. */
  const announcedSubagent = (output: FoldOutput): conversationv1.DetachedWorkKindSubagent | undefined => {
    const frame = output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const kind = (frame?.result.value as conversationv1.AgentDetachedWork).kind?.kind;
    return kind?.case === "subagent" ? kind.value : undefined;
  };

  it("announces the agent and the commission the store named", () => {
    // Arrange.
    const fold = foldWithOpenSend();
    fold.learnTaskAgent("a5583", { kind: "found", agent: SPAWN, commission: COMMISSION });

    // Act.
    const output = fold.onSdkMessage(resumed, foldContext());

    // Assert.
    const subagent = announcedSubagent(output);
    expect({ agent: subagent?.agentId?.value, description: subagent?.commission?.description }).toEqual({
      agent: "toolu_spawn",
      description: "fix the shim",
    });
  });

  it("announces with no commission, at ERROR, when the store recorded none", () => {
    // Arrange.
    const fold = foldWithOpenSend();
    fold.learnTaskAgent("a5583", { kind: "found", agent: SPAWN, commission: undefined });
    const before = logSinkMark();

    // Act.
    const output = fold.onSdkMessage(resumed, foldContext());

    // Assert.
    expect(announcedSubagent(output)?.commission).toBeUndefined();
    expect(
      logRecordsSince(before)
        .filter((record) => record.level === "error")
        .map((record) => [record.message, record.context.store_answer]),
    ).toEqual([
      [
        "a subagent announcement states no commission: this shim holds no record of the spawn",
        "found toolu_spawn, with no recorded commission",
      ],
    ]);
  });

  it("knows the task's agent once the store named it", () => {
    // Arrange.
    const fold = foldWithOpenSend();

    // Act.
    fold.learnTaskAgent("a5583", { kind: "found", agent: SPAWN, commission: COMMISSION });

    // Assert.
    expect(fold.taskAgent("a5583")).toEqual({ kind: "named", agent: SPAWN });
  });

  it("refuses the announcement with the store's answer when it named none", () => {
    // Arrange.
    const fold = foldWithOpenSend();
    fold.learnTaskAgent("a5583", { kind: "not_found" });
    const before = logSinkMark();

    // Act.
    const output = fold.onSdkMessage(resumed, foldContext());

    // Assert.
    expect(output.entries).toEqual([]);
    expect(
      logRecordsSince(before)
        .filter((record) => record.level === "error")
        .map((record) => record.context.store_answer),
    ).toEqual(["not_found: no agent of this session's lineage is paired with the locator"]);
  });
});

describe("hooks", () => {
  it("opens a hook unit on the vendor's own firing record", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("hook_started"), foldContext());

    expect(activityOf(output.entries[0])?.item.case).toBe("hook");
  });

  it("settles a succeeded hook with the duration the shim spanned", () => {
    const fold = createFold();
    fold.onSdkMessage(streamMessage("hook_started"), foldContext({ nowMs: 1_000 }));
    const response = streamMessage("hook_response") as unknown as Record<string, unknown>;

    const output = fold.onSdkMessage(
      { ...response, hook_id: "329470c7-5cbb-430d-be65-3fac50b869fb" } as unknown as SdkMessage,
      foldContext({ nowMs: 1_250 }),
    );

    const hook = activityOf(output.entries[0])?.item.value as conversationv1.AgentHook;
    const succeeded = hook.result.value as conversationv1.AgentHookSucceeded;
    expect(hook.result.case).toBe("succeeded");
    expect(succeeded.durationMs).toBe(250n);
  });

  it("never synthesizes a turn terminal from hook activity", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("hook_response"), foldContext());

    expect(output.turnEnded).toBeUndefined();
  });
});

describe("session facts", () => {
  it("records the LIVE rate-limit status, which is not the sampled allowance", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("rate_limit_event"), foldContext());

    const update =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    expect(update?.update.case).toBe("rateLimitStatus");
  });

  it("converts the vendor's seconds to millis and its fraction to a percent", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("rate_limit_event"), foldContext());

    const update =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    const status = update?.update.value as conversationv1.SessionRateLimitStatus;
    // The corpus states resetsAt 1785542400 (seconds) and utilization 0.79.
    expect(status.resetsAtMs).toBe(1_785_542_400_000n);
    expect(status.utilizationPercent).toBeCloseTo(79);
  });

  it("carries the status and window as typed arms, never as vendor strings", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("rate_limit_event"), foldContext());

    const update =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    const status = update?.update.value as conversationv1.SessionRateLimitStatus;
    expect(status.status.case).toBe("allowedWarning");
    expect(status.rateLimitType?.window.case).toBe("overage");
  });

  it("leaves every field the vendor omitted UNSET", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("rate_limit_event"), foldContext());

    const update =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    const status = update?.update.value as conversationv1.SessionRateLimitStatus;
    // The corpus event states no error code and no purchase flags.
    expect(status.errorCode).toBeUndefined();
    expect(status.canUserPurchaseCredits).toBeUndefined();
  });

  it("records nothing for a status that carries no conversation fact", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("status"), foldContext());

    expect(output.entries).toHaveLength(0);
  });

  it("leaves the FAILED cut to the engine, which holds the compaction it asked for", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      {
        type: "system",
        subtype: "status",
        status: null,
        compact_result: "failed",
        compact_error: "out of budget",
        uuid: "uuid-compact-failed",
        session_id: "session-1",
      } as unknown as SdkMessage,
      foldContext(),
    );

    expect(output.entries).toHaveLength(0);
  });

  /** The `conversation_reset` a `/clear` puts on the stream. */
  function reset(): SdkMessage {
    return {
      type: "conversation_reset",
      new_conversation_id: "announced-and-never-used",
      uuid: "uuid-reset",
      session_id: "session-1",
    } as unknown as SdkMessage;
  }

  /** The `system:init` that follows a reset, naming the id it rotated TO. */
  function initNaming(sessionId: string): SdkMessage {
    return {
      type: "system",
      subtype: "init",
      uuid: "uuid-init-2",
      session_id: sessionId,
    } as unknown as SdkMessage;
  }

  it("records a conversation reset as an identity rotation", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(reset(), foldContext());

    const rotated =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    expect(rotated?.update.case).toBe("identityRotated");
  });

  it("holds the clear's cut at the reset, which does not name the cut", () => {
    // The reset carries only uuids the FILE plane never sees; the identity both
    // planes can spell is the session it rotated to, and the reset does not
    // state it.
    const fold = createFold();

    const output = fold.onSdkMessage(reset(), foldContext());

    expect(output.entries).toHaveLength(1);
  });

  it("cuts the conversation when the init names the session the clear rotated to", () => {
    const fold = createFold();
    fold.onSdkMessage(reset(), foldContext());

    const output = fold.onSdkMessage(initNaming("session-2"), foldContext());

    const frame =
      output.entries.at(-1)?.item.kind === "frame"
        ? (output.entries.at(-1)?.item as { frame: conversationv1.AgentFrame }).frame
        : undefined;
    const cut = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.ContextCut;
    expect(cut.cut.case).toBe("cleared");
  });

  it("keys the released clear on the session it rotated to, as the sidecar spells it", () => {
    // ONE CLEAR IS ONE ROW. The sidecar reads the `/clear` envelope out of
    // `<session-2>.jsonl` and keys `session:context_cut:session-2`; a key minted
    // from this plane's own reset uuid could never collide with it, and the feed
    // drew "context cleared" twice.
    const fold = createFold();
    fold.onSdkMessage(reset(), foldContext());

    const output = fold.onSdkMessage(initNaming("session-2"), foldContext());

    expect(output.entries.at(-1)?.upsertKey).toBe("session:context_cut:session-2");
  });

  it("holds the clear's cut when the init that follows names no session", () => {
    // A row keyed on nothing would collide with every other unidentified cut,
    // and the file plane still writes this clear from the envelope on disk.
    const fold = createFold();
    fold.onSdkMessage(reset(), foldContext());

    const output = fold.onSdkMessage(initNaming(""), foldContext());

    expect(output.entries.some((entry) => entry.upsertKey.startsWith("session:context_cut:"))).toBe(
      false,
    );
  });

  it("releases one held clear once, so a later init draws no second divider", () => {
    const fold = createFold();
    fold.onSdkMessage(reset(), foldContext());
    fold.onSdkMessage(initNaming("session-2"), foldContext());

    const output = fold.onSdkMessage(initNaming("session-2"), foldContext());

    expect(output.entries.some((entry) => entry.upsertKey.startsWith("session:context_cut:"))).toBe(
      false,
    );
  });

  it("records the session's MCP servers from its opening record", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      {
        type: "system",
        subtype: "init",
        mcp_servers: [{ name: "Slack", status: "connected" }],
        uuid: "uuid-init",
        session_id: "session-1",
      } as unknown as SdkMessage,
      foldContext(),
    );

    const update =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    expect(update?.update.case).toBe("mcpServer");
  });
});

describe("permission", () => {
  /** The vendor's own auto-denial record. */
  function denial(): SdkMessage {
    return {
      type: "system",
      subtype: "permission_denied",
      tool_name: "Bash",
      tool_use_id: "toolu_d",
      decision_reason_type: "rule",
      decision_reason: "a deny rule matched",
      message: "denied by policy",
      uuid: "uuid-denied",
      session_id: "session-1",
    } as unknown as SdkMessage;
  }

  it("records a policy denial that never had an open ask", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(denial(), foldContext());

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const permission = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.AgentPermission;
    const success = permission.result.value as conversationv1.AgentPermissionSuccess;
    const denied = success.decision.value as conversationv1.AgentPermissionDenied;
    expect(denied.by.case).toBe("policy");
  });

  it("records a CLASSIFIER denial as undecidable rather than as policy", () => {
    // Nobody refused: the deciding machinery could not answer. Drawn as policy
    // it implies a rule that does not exist, and it is the only denial here
    // that retrying may resolve.
    const fold = createFold();
    const message = {
      ...(denial() as unknown as Record<string, unknown>),
      decision_reason_type: "classifier",
      decision_reason: "the classifier could not reach a verdict",
    } as unknown as SdkMessage;

    const output = fold.onSdkMessage(message, foldContext());

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const permission = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.AgentPermission;
    const success = permission.result.value as conversationv1.AgentPermissionSuccess;
    const denied = success.decision.value as conversationv1.AgentPermissionDenied;
    expect(denied.by.case).toBe("undecidable");
  });

  it("carries the classifier's own account as the undecidable detail", () => {
    const fold = createFold();
    const message = {
      ...(denial() as unknown as Record<string, unknown>),
      decision_reason_type: "classifier",
      decision_reason: "the classifier could not reach a verdict",
    } as unknown as SdkMessage;

    const output = fold.onSdkMessage(message, foldContext());

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const permission = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.AgentPermission;
    const success = permission.result.value as conversationv1.AgentPermissionSuccess;
    const denied = success.decision.value as conversationv1.AgentPermissionDenied;
    const undecidable = denied.by.value as conversationv1.AgentPermissionDeniedForWantOfDecider;
    expect(undecidable.detail).toBe("the classifier could not reach a verdict");
  });

  it("retires a denied call's unit with failure and NO content", () => {
    // THE RULING (project lead, 2026-09-01, final): starts are NOT deferred, so
    // the gated unit is already on the stream and must reach a terminal like
    // every other. A denial RETIRES it: the `failure` arm settles with content
    // UNSET (the producer observed no error content) and `settled_at` stamped.
    // The vendor's own tool_result for a denial is the deny sentence the MODEL
    // was shown -- `toolUseResult` is a bare "Error: ..." string there rather
    // than the tool's Output object -- which is why neither is carried. It is
    // drawn denied through the permission unit, whose id IS this unit's
    // AgentActivityId, so no `denied` cause on AgentToolFailure is needed.
    const fold = createFold();
    const denied = foldContext({ deniedCall: (toolUseId) => toolUseId === "toolu_d" });
    fold.onSdkMessage(
      assistant("msg-denied", [
        { type: "tool_use", id: "toolu_d", name: "Bash", input: { command: "rm -rf /" } },
      ]),
      denied,
    );

    const output = fold.onSdkMessage(
      toolResult("toolu_d", "Error: the user denied this call", true),
      denied,
    );

    expect(output.entries).toHaveLength(1);
    const activity = activityOf(output.entries[0]);
    expect(activity?.activityId?.value).toBe("toolu_d");
    if (activity?.item.case !== "bash" || activity.item.value.result.case !== "failure") {
      throw new Error("a denied call did not settle its unit's failure arm");
    }
    const error = activity.item.value.result.value.error;
    expect(error?.content).toBeUndefined();
    expect(error?.settledAt).toBeDefined();
  });

  it("still settles a call the shim did NOT deny", () => {
    // The suppression is about denial, not about errors: a call that ran and
    // failed settles exactly as it always did.
    const fold = createFold();
    const allowed = foldContext({ deniedCall: () => false });
    fold.onSdkMessage(
      assistant("msg-ok", [
        { type: "tool_use", id: "toolu_ok", name: "Bash", input: { command: "false" } },
      ]),
      allowed,
    );

    const output = fold.onSdkMessage(toolResult("toolu_ok", { stdout: "" }, true), allowed);

    expect(output.entries.length).toBeGreaterThan(0);
  });

  it("produces nothing when the ENGINE's gate is already holding that ask", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      denial(),
      foldContext({ pendingAsk: () => ({ kind: "permission" }) }),
    );

    expect(output.entries).toHaveLength(0);
  });

  it("joins consent to the work it gates by the gated call's own id", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(denial(), foldContext());

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const permission = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.AgentPermission;
    expect(permission.gatedCall?.value).toBe("toolu_d");
  });
});

describe("the terminal of a turn the vendor absorbed", () => {
  it("concludes the absorbed turn as COMPLETED", () => {
    // Arrange
    const fold = createFold();

    // Act
    const output = fold.concludeAbsorbedTurn(foldContext(), "absorbed-adopted-1");

    // Assert
    expect(output.turnEnded?.frame.result.case === "success" ? output.turnEnded.frame.result.value.outcome.case : "").toBe(
      "completed",
    );
  });

  it("names the last top-level prose the absorbed turn produced as its ANSWER", () => {
    // Arrange
    const fold = createFold();
    fold.onSdkMessage(assistant("msg-v", [{ type: "text", text: "the hand-back is in" }]), foldContext());

    // Act
    const output = fold.concludeAbsorbedTurn(foldContext(), "absorbed-adopted-1");

    // Assert
    const success = output.turnEnded?.frame.result.value as conversationv1.AgentSuccess;
    expect((success.outcome.value as conversationv1.AgentCompleted).answer?.value).toBe("msg-v:0");
  });

  it("keys the terminal row by the coordinate it was given", () => {
    // Arrange
    const fold = createFold();

    // Act
    const output = fold.concludeAbsorbedTurn(foldContext(), "absorbed-adopted-1");

    // Assert
    expect(output.entries.map((entry) => entry.source.vendorUuid)).toEqual(["absorbed-adopted-1"]);
  });
});

describe("the turn's terminal", () => {
  it("completes a turn from the vendor's own result record", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("result_success"), foldContext());

    expect(output.turnEnded).toBeDefined();
    const success = output.turnEnded?.frame.result.value as conversationv1.AgentSuccess;
    expect(success.outcome.case).toBe("completed");
  });

  it("names the last top-level prose as the agent's ANSWER", () => {
    const fold = createFold();
    fold.onSdkMessage(assistant("msg-a", [{ type: "text", text: "the answer" }]), foldContext());

    const output = fold.onSdkMessage(streamMessage("result_success"), foldContext());

    const success = output.turnEnded?.frame.result.value as conversationv1.AgentSuccess;
    const completed = success.outcome.value as conversationv1.AgentCompleted;
    expect(completed.answer?.value).toBe("msg-a:0");
  });

  it("names a block cut at the output ceiling as the ANSWER, since it had spoken", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-cut", [{ type: "text", text: "The answer begins and then stops mid-" }], {
        message: { stop_reason: "max_tokens" },
      }),
      foldContext(),
    );

    const output = fold.onSdkMessage(streamMessage("result_success"), foldContext());

    const success = output.turnEnded?.frame.result.value as conversationv1.AgentSuccess;
    const completed = success.outcome.value as conversationv1.AgentCompleted;
    expect(completed.answer?.value).toBe("msg-cut:0");
  });

  it("names no answer for a failed block that said nothing", () => {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-refused", [{ type: "text", text: "" }], { message: { stop_reason: "refusal" } }),
      foldContext(),
    );

    const output = fold.onSdkMessage(streamMessage("result_success"), foldContext());

    const success = output.turnEnded?.frame.result.value as conversationv1.AgentSuccess;
    const completed = success.outcome.value as conversationv1.AgentCompleted;
    expect(completed.answer).toBeUndefined();
  });

  it("carries the terminal in the rows too, since the feed's stop notice has no other source", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("result_success"), foldContext());

    expect(output.entries).toHaveLength(1);
  });

  it("relays an aborted turn as interrupted by the user", () => {
    const fold = createFold();
    const result = streamMessage("result_success") as unknown as Record<string, unknown>;

    const output = fold.onSdkMessage(
      { ...result, terminal_reason: "aborted_streaming" } as unknown as SdkMessage,
      foldContext(),
    );

    const success = output.turnEnded?.frame.result.value as conversationv1.AgentSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentInterrupted;
    expect(interrupted.cause.case).toBe("byUser");
  });

  it("relays a stop-hook refusal as its own failure arm", () => {
    const fold = createFold();
    const result = streamMessage("result_success") as unknown as Record<string, unknown>;

    const output = fold.onSdkMessage(
      { ...result, terminal_reason: "stop_hook_prevented" } as unknown as SdkMessage,
      foldContext(),
    );

    const failure = output.turnEnded?.frame.result.value as conversationv1.AgentFailure;
    expect(failure.failure.case).toBe("stopHookPrevented");
  });

  it("relays a recorded API failure with the vendor's own taxonomy", () => {
    const fold = createFold();
    const result = streamMessage("result_success") as unknown as Record<string, unknown>;

    const output = fold.onSdkMessage(
      {
        ...result,
        terminal_reason: "api_error",
        api_error_status: 429,
        errors: ["rate limited"],
      } as unknown as SdkMessage,
      foldContext(),
    );

    const failure = output.turnEnded?.frame.result.value as conversationv1.AgentFailure;
    const api = failure.failure.value as conversationv1.ApiRequestFailed;
    expect(failure.failure.case).toBe("apiRequestFailed");
    expect(api.kind.case).toBe("rateLimited");
    expect(failure.errors).toEqual(["rate limited"]);
  });

  it("falls back on the result SUBTYPE when the vendor stated no terminal reason", () => {
    const fold = createFold();
    const result = streamMessage("result_success") as unknown as Record<string, unknown>;

    const output = fold.onSdkMessage(
      {
        ...result,
        terminal_reason: undefined,
        subtype: "error_max_turns",
        is_error: true,
      } as unknown as SdkMessage,
      foldContext(),
    );

    const failure = output.turnEnded?.frame.result.value as conversationv1.AgentFailure;
    expect(failure.failure.case).toBe("maxTurns");
  });
});

describe("keep-alive turns", () => {
  it("marks every row of a keep-alive turn never-served", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("assistant"), foldContext({ keepalive: true }));

    expect(output.entries[0]?.keepalive).toBe(true);
  });
});

describe("residue", () => {
  it("records a vendor record no converter owns rather than dropping it", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("notification"), foldContext());

    expect(residueOf(output.entries[0])?.unservedItem.case).toBe("vendorSpecific");
  });

  it("spells a system record's residue kind as system/<subtype>, the cross-plane spelling", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("notification"), foldContext());

    const residue = residueOf(output.entries[0]);
    const specific = residue?.unservedItem.value as { kind: string };
    expect(specific.kind).toBe("system/notification");
  });

  it("never throws on a malformed record: it lands as residue with the defect logged", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      {
        type: "assistant",
        uuid: "uuid-broken",
        parent_tool_use_id: null,
        message: {},
      } as unknown as SdkMessage,
      foldContext(),
    );

    expect(residueOf(output.entries[0])).toBeDefined();
  });

  it("reports a converter defect through the engine's fault channel", () => {
    const fold = createFold();
    const reported: { kind: string; detail: string }[] = [];

    fold.onSdkMessage(
      {
        type: "system",
        subtype: "hook_started",
        uuid: "uuid-hook-broken",
        session_id: "session-1",
        hook_id: "",
        hook_name: "PreToolUse:Read",
        hook_event: "PreToolUse",
      } as unknown as SdkMessage,
      foldContext({ reportFault: (kind, detail) => reported.push({ kind, detail }) }),
    );

    expect(reported[0]?.kind).toBe("converter_defect");
  });

  it("reports nothing through the fault channel when the message converts", () => {
    const fold = createFold();
    const reported: string[] = [];

    fold.onSdkMessage(
      streamMessage("notification"),
      foldContext({ reportFault: (_kind, detail) => reported.push(detail) }),
    );

    expect(reported).toEqual([]);
  });

  it("records the vendor's own answer to a local slash command as vendor-specific residue", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      {
        type: "system",
        subtype: "local_command_output",
        content: "/usage: 40%",
        uuid: "uuid-local",
        session_id: "session-1",
      } as unknown as SdkMessage,
      foldContext(),
    );

    expect(residueOf(output.entries[0])?.unservedItem.case).toBe("vendorSpecific");
  });
});

describe("the user record that is not a user", () => {
  it("drops the vendor's echo of a prompt: the shim's own AgentPrompt row is served (R15)", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("user"), foldContext());

    expect(output.entries).toHaveLength(0);
  });

  it("produces no terminal for a result whose call this shim never saw announced", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(toolResult("toolu_unknown", {}), foldContext());

    expect(output.entries).toHaveLength(0);
  });
});

describe("attribution", () => {
  it("carries a subagent's frames under the subagent's OWN identity", () => {
    const fold = createFold();
    const subagent = create(conversationv1.AgentIdSchema, { value: "agent-7" });

    const output = fold.onSdkMessage(
      assistant("msg-sub", [{ type: "text", text: "sub prose" }], {
        parent_tool_use_id: "toolu_spawn",
      }),
      foldContext({ subagentFor: () => subagent }),
    );

    expect(output.entries[0]?.agentId?.value).toBe("agent-7");
  });

  it("mints a subagent's book from its SPAWNING CALL when the engine knows no id", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-sub", [{ type: "text", text: "sub prose" }], {
        parent_tool_use_id: "toolu_spawn",
      }),
      foldContext(),
    );

    // The pinned SDK stream states NO agent id anywhere, so the spawning call's
    // own id is the subagent's book until a ruling gives it a real producer —
    // minted in ONE function so that ruling changes one line.
    expect(output.entries[0]?.agentId?.value).toBe("toolu_spawn");
  });
});

describe("the fold's attachment arm", () => {
  /** One attachment record as the STREAM hands it over. */
  const attachmentMessage = (type: string, uuid: string | undefined): SdkMessage =>
    ({
      type: "attachment",
      ...(uuid === undefined ? {} : { uuid }),
      attachment: { type },
    }) as unknown as SdkMessage;

  it("residues a tool-availability delta as VENDOR_SPECIFIC, not unknown", () => {
    // Arrange.
    const fold = createFold();

    // Act.
    const output = fold.onSdkMessage(
      attachmentMessage("deferred_tools_delta", "att-1"),
      foldContext(),
    );

    // Assert.
    const item = output.entries[0]?.item;
    expect(item?.kind === "residue" ? item.residue.unservedItem.case : undefined).toBe(
      "vendorSpecific",
    );
  });

  it("spells the residue kind attachment/<type>, the sidecar's own spelling", () => {
    // Arrange.
    const fold = createFold();

    // Act.
    const output = fold.onSdkMessage(
      attachmentMessage("agent_listing_delta", "att-2"),
      foldContext(),
    );

    // Assert.
    const item = output.entries[0]?.item;
    const residue = item?.kind === "residue" ? item.residue.unservedItem : undefined;
    expect(residue?.case === "vendorSpecific" ? residue.value.kind : undefined).toBe(
      "attachment/agent_listing_delta",
    );
  });

  it("keys the residue by the RECORD'S OWN UUID, so both planes collide on one row", () => {
    // Arrange.
    const fold = createFold();

    // Act.
    const output = fold.onSdkMessage(
      attachmentMessage("deferred_tools_delta", "att-3"),
      foldContext(),
    );

    // Assert.
    expect(output.entries[0]?.upsertKey).toBe("residue:att-3");
  });

  it("lands a uuid-less attachment UNPARSED, since neither plane could key it", () => {
    // Arrange.
    const fold = createFold();

    // Act.
    const output = fold.onSdkMessage(
      attachmentMessage("deferred_tools_delta", undefined),
      foldContext(),
    );

    // Assert.
    const item = output.entries[0]?.item;
    expect(item?.kind === "residue" ? item.residue.unservedItem.case : undefined).toBe("unparsed");
  });

  it("never throws on an attachment, whatever it carries", () => {
    // Arrange.
    const fold = createFold();

    // Act, Assert.
    expect(() =>
      fold.onSdkMessage({ type: "attachment", uuid: "att-4" } as unknown as SdkMessage, foldContext()),
    ).not.toThrow();
  });
});

describe("a skill's declared allowances, from the acknowledgement to the settled unit", () => {
  /** The skill DOCUMENT, as the vendor injects it: `isMeta`, joined by `sourceToolUseID`. */
  function skillDocument(toolUseId: string): SdkMessage {
    return {
      type: "user",
      uuid: `uuid-doc-${toolUseId}`,
      session_id: "session-1",
      parent_tool_use_id: null,
      isMeta: true,
      sourceToolUseID: toolUseId,
      message: { role: "user", content: [{ type: "text", text: "# the skill" }] },
    } as unknown as SdkMessage;
  }

  /** The invocation, its acknowledgement, then its document. */
  function foldSkill(structured: unknown) {
    const fold = createFold();
    fold.onSdkMessage(
      assistant("msg-skill", [
        { type: "tool_use", id: "toolu_s", name: "Skill", input: { skill: "debug-logs" } },
      ]),
      foldContext(),
    );
    fold.onSdkMessage(toolResult("toolu_s", structured), foldContext());
    const output = fold.onSdkMessage(skillDocument("toolu_s"), foldContext());
    const use = activityOf(output.entries[0])?.item.value as conversationv1.AgentSkillUse;
    return use.result.value as conversationv1.AgentSkillUseSuccess;
  }

  it("carries the allowances the acknowledgement declared onto the frame the document settles", () => {
    // Arrange, Act: the shape the skill-invocation capture states.
    const success = foldSkill({ success: true, commandName: "debug-logs", allowedTools: ["Read", "Glob"] });

    // Assert.
    expect(success.allowedTools?.toolNames).toEqual(["Read", "Glob"]);
  });

  it("leaves the allowances UNSET when the acknowledgement declared none", () => {
    // Arrange, Act.
    const success = foldSkill({ success: true, commandName: "debug-logs" });

    // Assert.
    expect(success.allowedTools).toBeUndefined();
  });
});

/**
 * THE VENDOR'S API ERROR CLASS, carried from the records that state it to the
 * terminal that needs it.
 *
 * The `result` record carries the HTTP status alone. The class and the retry
 * delay ride `api_retry` (corpus: every `"subtype":"api_retry"` line) and a
 * failed assistant message's `error`, both of which arrive BEFORE the terminal,
 * so the fold remembers the last one of the turn and the terminal reads it.
 */
describe("the vendor's API failure class reaching the terminal", () => {
  function apiRetry(errorClass: string, retryDelayMs: number): SdkMessage {
    return {
      type: "system",
      subtype: "api_retry",
      attempt: 1,
      max_retries: 10,
      retry_delay_ms: retryDelayMs,
      error_status: null,
      error: errorClass,
      uuid: "uuid-api-retry",
      session_id: "session-1",
    } as unknown as SdkMessage;
  }

  function apiResult(status: number | null): SdkMessage {
    return {
      type: "result",
      subtype: "error_during_execution",
      is_error: true,
      terminal_reason: "api_error",
      errors: ["the vendor said so"],
      api_error_status: status,
      uuid: "uuid-api-result",
      session_id: "session-1",
    } as unknown as SdkMessage;
  }

  function apiKindOf(messages: readonly SdkMessage[]): conversationv1.ApiRequestFailed {
    const fold = createFold();
    let last;
    for (const message of messages) last = fold.onSdkMessage(message, foldContext());
    const result = last?.turnEnded?.frame?.result;
    if (result?.case !== "failure") throw new Error("the api terminal must be a failure");
    const failure = result.value.failure;
    if (failure.case !== "apiRequestFailed") throw new Error("the api terminal must be api_request_failed");
    return failure.value;
  }

  it("reads the class off an api_retry record", () => {
    // Arrange + Act
    const failed = apiKindOf([apiRetry("max_output_tokens", 549), apiResult(null)]);

    // Assert
    expect(failed.kind.case).toBe("maxOutputTokens");
  });

  it("reads the wait off an api_retry record", () => {
    // Arrange + Act
    const failed = apiKindOf([apiRetry("rate_limit", 549), apiResult(429)]);

    // Assert
    expect(failed.kind.value).toMatchObject({ retryAfterMs: 549n });
  });

  it("reads the class off a failed assistant message", () => {
    // Arrange + Act
    const failed = apiKindOf([
      assistant("msg-api", [{ type: "text", text: "billing" }], { error: "billing_error" }),
      apiResult(402),
    ]);

    // Assert
    expect(failed.kind.case).toBe("billingError");
  });

  it("ignores a SUBAGENT's failed assistant message, whose request is not the turn's", () => {
    // Arrange
    const withoutSubagent = apiKindOf([apiRetry("rate_limit", 549), apiResult(null)]);

    // Act
    const failed = apiKindOf([
      apiRetry("rate_limit", 549),
      assistant("msg-sub-api", [{ type: "text", text: "billing" }], {
        error: "billing_error",
        parent_tool_use_id: "toolu_spawn",
      }),
      apiResult(null),
    ]);

    // Assert
    expect(failed.kind.case).toBe(withoutSubagent.kind.case);
  });

  it("carries the vendor's own sentence when the result states no errors", () => {
    // Arrange
    const sentence =
      "API Error: 400 Claude Code 2.1.220 does not support this model; version 2.1.251 or newer is required.";
    const noErrors = { ...(apiResult(400) as unknown as Record<string, unknown>), errors: [] } as unknown as SdkMessage;

    // Act
    const failed = apiKindOf([
      assistant("msg-api", [{ type: "text", text: sentence }], { error: "invalid_request" }),
      noErrors,
    ]);

    // Assert
    expect(failed.message).toBe(sentence);
  });

  it("prefers the result's own errors over the notice's sentence", () => {
    // Arrange + Act
    const failed = apiKindOf([
      assistant("msg-api", [{ type: "text", text: "the notice" }], { error: "invalid_request" }),
      apiResult(400),
    ]);

    // Assert
    expect(failed.message).toBe("the vendor said so");
  });

  it("keeps the LAST class stated when a run failed several times", () => {
    // Arrange + Act
    const failed = apiKindOf([
      apiRetry("overloaded", 549),
      apiRetry("oauth_org_not_allowed", 1_144),
      apiResult(403),
    ]);

    // Assert
    expect(failed.kind.case).toBe("oauthOrgNotAllowed");
  });

  it("does not let one turn's class reach the next turn's terminal", () => {
    // Arrange
    const fold = createFold();
    for (const message of [apiRetry("oauth_org_not_allowed", 549), apiResult(403)]) {
      fold.onSdkMessage(message, foldContext());
    }

    // Act
    const second = fold.onSdkMessage(apiResult(403), foldContext());

    // Assert
    const result = second.turnEnded?.frame?.result;
    const failure = result?.case === "failure" ? result.value.failure : undefined;
    const value = failure?.case === "apiRequestFailed" ? failure.value : undefined;
    expect(value?.kind.case).toBe("permissionDenied");
  });
});

/**
 * What the fold does with a record it is not there to convert.
 */
describe("a message carrying no conversation fact", () => {
  it("ignores a keep_alive outright rather than landing it as residue", () => {
    const output = createFold().onSdkMessage(
      { type: "keep_alive", uuid: "uuid-ka", session_id: "session-1" } as unknown as SdkMessage,
      foldContext(),
    );

    expect(output).toBe(EMPTY_FOLD_OUTPUT);
  });
});

describe("the place a record states", () => {
  it("places every row a timestamped record produced by that record, as the file plane does", () => {
    const at = "2026-09-30T12:00:00.000Z";
    const message = {
      ...(assistant("msg-placed", [{ type: "text", text: "hello" }]) as unknown as Record<string, unknown>),
      timestamp: at,
    } as unknown as SdkMessage;

    const output = createFold().onSdkMessage(message, foldContext());

    expect(output.entries.length).toBeGreaterThan(0);
    expect(output.entries.every((entry) => entry.recordPlace?.atMs === Date.parse(at))).toBe(true);
  });

  it("leaves a record with no timestamp to the writer's clock", () => {
    const output = createFold().onSdkMessage(assistant("msg-unplaced", [{ type: "text", text: "hello" }]), foldContext());

    expect(output.entries.some((entry) => entry.recordPlace !== undefined)).toBe(false);
  });
});

describe("an SDK message type no converter owns", () => {
  it("lands the record as residue named by its own type", () => {
    const output = createFold().onSdkMessage(
      { type: "telemetry_beacon", uuid: "uuid-tb", session_id: "session-1" } as unknown as SdkMessage,
      foldContext(),
    );

    expect(output.entries[0]?.source.discriminator).toBe("unknown.telemetry_beacon");
  });

  it("keeps the whole record, so a later converter loses nothing", () => {
    const output = createFold().onSdkMessage(
      { type: "telemetry_beacon", uuid: "uuid-tb", beat: 3 } as unknown as SdkMessage,
      foldContext(),
    );

    expect(residueOf(output.entries[0])?.unservedItem.case).toBe("unknown");
  });
});

/**
 * The class and the wait are remembered SEPARATELY: a retry that restates only
 * one of them must not erase the other, because the terminal reads both.
 */
describe("two api_retry records that each state only half the account", () => {
  function retry(fields: Record<string, unknown>): SdkMessage {
    return {
      type: "system",
      subtype: "api_retry",
      attempt: 1,
      max_retries: 10,
      error_status: null,
      uuid: "uuid-retry-partial",
      session_id: "session-1",
      ...fields,
    } as unknown as SdkMessage;
  }

  function apiTerminal(messages: readonly SdkMessage[]): conversationv1.ApiRequestFailed {
    const fold = createFold();
    let last;
    for (const message of messages) last = fold.onSdkMessage(message, foldContext());
    const result = last?.turnEnded?.frame?.result;
    if (result?.case !== "failure") throw new Error("the api terminal must be a failure");
    const failure = result.value.failure;
    if (failure.case !== "apiRequestFailed") throw new Error("expected api_request_failed");
    return failure.value;
  }

  const terminal = {
    type: "result",
    subtype: "error_during_execution",
    is_error: true,
    terminal_reason: "api_error",
    errors: ["the vendor said so"],
    api_error_status: null,
    uuid: "uuid-partial-result",
    session_id: "session-1",
  } as unknown as SdkMessage;

  it("keeps the earlier CLASS when the later retry stated only a wait", () => {
    // Arrange + Act
    const failed = apiTerminal([
      retry({ error: "rate_limit" }),
      retry({ retry_delay_ms: 900 }),
      terminal,
    ]);

    // Assert
    expect(failed.kind.case).toBe("rateLimited");
  });

  it("keeps the earlier WAIT when the later retry stated only a class", () => {
    // Arrange + Act
    const failed = apiTerminal([
      retry({ retry_delay_ms: 900 }),
      retry({ error: "rate_limit" }),
      terminal,
    ]);

    // Assert
    expect(failed.kind.value).toMatchObject({ retryAfterMs: 900n });
  });
});

/**
 * The fold's EARLY-WARNING visibility for a vendor API failure CLASS.
 *
 * The terminal owns the full diagnostic record; this is the moment the vendor
 * first NAMES the class, surfaced at INFO for the two classes that matter and
 * kept at the low-visibility verbose line for every other so ordinary
 * rate-limit retries do not flood the log.
 */
describe("remembering the vendor's API failure class", () => {
  it("surfaces an authentication_failed class at INFO under shim.vendor.auth_rejected", () => {
    // Arrange.
    const fold = createFold();
    mockedWriteSync.mockClear();

    // Act.
    fold.onSdkMessage(
      assistant("msg-auth", [{ type: "text", text: "API Error" }], {
        error: "authentication_failed",
      }),
      foldContext(),
    );

    // Assert.
    const record = logRecordsSince(0).find(
      (candidate) => candidate.operation === "shim.vendor.auth_rejected",
    );
    expect(record?.context.vendor_error).toBe("authentication_failed");
    expect(record?.verbosity).toBe("normal");
  });

  it("keeps an ordinary rate_limit class at the verbose line, not at INFO", () => {
    // Arrange.
    const fold = createFold();
    mockedWriteSync.mockClear();

    // Act.
    fold.onSdkMessage(
      assistant("msg-rate", [{ type: "text", text: "API Error" }], { error: "rate_limit" }),
      foldContext(),
    );

    // Assert.
    const records = logRecordsSince(0);
    expect(records.some((candidate) => (candidate.operation as string).startsWith("shim.vendor."))).toBe(false);
    const held = records.find(
      (candidate) =>
        candidate.message ===
          "an API attempt failed; the vendor may still retry past it, and the turn's terminal records the outcome",
    );
    expect(held?.verbosity).toBe("verbose");
    expect(held?.context.vendor_error).toBe("rate_limit");
  });
});

// ---------------------------------------------------------------------------
// Where the fold ends each stream's block state.
// ---------------------------------------------------------------------------

describe("where the fold ends a stream", () => {
  const SPAWN = "toolu_spawn";
  const UNSETTLED =
    "invariant violated: a streamed unit was started and never settled; its row stays unsettled";

  /** A stream event on the stream `parent` names. */
  function streamOn(parent: string | null, event: Record<string, unknown>): SdkMessage {
    return {
      type: "stream_event",
      uuid: `uuid-${String(event.type)}`,
      session_id: "session-1",
      parent_tool_use_id: parent,
      event,
    } as unknown as SdkMessage;
  }

  /** A response on the stream `parent` names that opens a text block and never settles it. */
  function strandedBlock(parent: string | null, messageId: string): SdkMessage[] {
    return [
      streamOn(parent, { type: "message_start", message: { id: messageId } }),
      streamOn(parent, { type: "content_block_start", index: 0, content_block: { type: "text" } }),
    ];
  }

  /** The `detected_at` of every unsettled-unit report folding `messages` wrote. */
  function unsettledDetectedAt(messages: readonly SdkMessage[]): unknown[] {
    const fold = createFold();
    mockedWriteSync.mockClear();
    for (const message of messages) fold.onSdkMessage(message, foldContext());
    return logRecordsSince(0)
      .filter((record) => record.message === UNSETTLED)
      .map((record) => record.context.detected_at);
  }

  it("ends a subagent's stream at the spawn's CONCLUDING tool_result", () => {
    // Arrange, Act.
    const detected = unsettledDetectedAt([
      ...strandedBlock(SPAWN, "msg_sub"),
      toolResult(SPAWN, { status: "completed", content: [] }),
    ]);

    // Assert.
    expect(detected).toEqual(["agent_end"]);
  });

  it("does NOT end a subagent's stream at a LAUNCH RECEIPT", () => {
    // Arrange, Act.
    const detected = unsettledDetectedAt([
      ...strandedBlock(SPAWN, "msg_sub"),
      toolResult(SPAWN, { status: "async_launched", isAsync: true }),
    ]);

    // Assert.
    expect(detected).toEqual([]);
  });

  it("ends a backgrounded subagent's stream at the task_notification naming its call", () => {
    // Arrange, Act.
    const detected = unsettledDetectedAt([
      ...strandedBlock(SPAWN, "msg_sub"),
      {
        type: "system",
        subtype: "task_notification",
        task_id: "a0000000000000001",
        tool_use_id: SPAWN,
        status: "completed",
        output_file: "/tmp/out",
        summary: "done",
        uuid: "uuid-notification",
        session_id: "session-1",
      } as unknown as SdkMessage,
    ]);

    // Assert.
    expect(detected).toEqual(["agent_end"]);
  });

  it("ends the main stream at the turn's result", () => {
    // Arrange, Act.
    const detected = unsettledDetectedAt([
      ...strandedBlock(null, "msg_main"),
      {
        type: "result",
        subtype: "success",
        is_error: false,
        result: "done",
        uuid: "uuid-result",
        session_id: "session-1",
      } as unknown as SdkMessage,
    ]);

    // Assert.
    expect(detected).toEqual(["turn_end"]);
  });

  it("does NOT end a subagent's stream at the turn's result", () => {
    // Arrange, Act.
    const detected = unsettledDetectedAt([
      ...strandedBlock(SPAWN, "msg_sub"),
      {
        type: "result",
        subtype: "success",
        is_error: false,
        result: "done",
        uuid: "uuid-result",
        session_id: "session-1",
      } as unknown as SdkMessage,
    ]);

    // Assert.
    expect(detected).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// the compaction's held cut
// ---------------------------------------------------------------------------

/** A `compact_boundary` as the stream states it, naming its summary by anchor unless told otherwise. */
function compactBoundary(uuid: string, anchor: string | null = `${uuid}-summary`): SdkMessage {
  return {
    type: "system",
    subtype: "compact_boundary",
    compact_metadata: {
      trigger: "manual",
      pre_tokens: 48_374,
      post_tokens: 3_759,
      duration_ms: 45_767,
      ...(anchor === null ? {} : { preserved_messages: { anchor_uuid: anchor, uuids: [] } }),
    },
    uuid,
    session_id: "session-1",
  } as unknown as SdkMessage;
}

/** The synthetic main-stream user record the vendor states a compaction's summary on. */
function compactSummaryRecord(
  uuid: string,
  content: unknown,
  extra: Record<string, unknown> = {},
): SdkMessage {
  return {
    type: "user",
    message: { role: "user", content },
    parent_tool_use_id: null,
    session_id: "session-1",
    uuid,
    isSynthetic: true,
    ...extra,
  } as unknown as SdkMessage;
}

/** The compaction a row carries, or undefined for any other row. */
function compactedOf(entry: PersistEntry | undefined): conversationv1.ContextCompacted | undefined {
  if (entry?.item.kind !== "frame") return undefined;
  const result = entry.item.frame.result;
  const update = result.case === "update" ? result.value.update : undefined;
  if (update?.case !== "contextCut") return undefined;
  return update.value.cut.case === "compacted" ? update.value.cut.value : undefined;
}

describe("a compaction's summary record", () => {
  it("releases the held cut carrying the summary the record states", () => {
    // Arrange.
    const fold = createFold();
    const held = fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-boundary-summary", "This session is being continued. Summary: the fold"),
      foldContext(),
    );

    // Assert.
    expect({
      held: held.entries.length,
      key: output.entries[0]?.upsertKey,
      summary: compactedOf(output.entries[0])?.summary?.markdown,
      tokensBefore: compactedOf(output.entries[0])?.tokens?.tokensBefore,
    }).toEqual({
      held: 0,
      key: "session:context_cut:uuid-boundary",
      summary: "This session is being continued. Summary: the fold",
      tokensBefore: 48_374n,
    });
  });

  it("joins a summary's text blocks with newlines, the way the file plane reads the same summary", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-boundary-summary", [
        { type: "text", text: "first" },
        { type: "image" },
        { type: "text", text: "second" },
      ]),
      foldContext(),
    );

    // Assert.
    expect(compactedOf(output.entries[0])?.summary?.markdown).toBe("first\nsecond");
  });

  it("is not the assistant prose that follows the boundary", () => {
    // Arrange: a /compact turn states no prose at all, and the next turn's
    // reply is the model talking, not the summary.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      assistant("msg-after", [{ type: "text", text: "the next reply" }]),
      foldContext(),
    );

    // Assert.
    expect(output.entries.map((entry) => entry.upsertKey)).toEqual(["activity:msg-after:0"]);
  });

  it("is not a synthetic record carrying a uuid other than the one the boundary's anchor names", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-stop-hook-feedback", "Stop hook feedback: keep going"),
      foldContext(),
    );

    // Assert.
    expect(output.entries).toEqual([]);
  });

  it("is not a subagent's synthetic record", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-boundary-summary", "a subagent's own text", {
        parent_tool_use_id: "toolu_spawn",
      }),
      foldContext(),
    );

    // Assert.
    expect(output.entries).toEqual([]);
  });

  it("is not a user record the vendor did not synthesize", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-boundary-summary", "a prompt", { isSynthetic: false }),
      foldContext(),
    );

    // Assert.
    expect(output.entries).toEqual([]);
  });

  it("is the first main-stream synthetic record when the boundary names no anchor", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary", null), foldContext());

    // Act.
    const output = fold.onSdkMessage(compactSummaryRecord("uuid-any", "the whole history, summarized"), foldContext());

    // Assert.
    expect(compactedOf(output.entries[0])?.summary?.markdown).toBe("the whole history, summarized");
  });

  it("releases the cut without a summary, and logs the gap at error, when it states no text", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext({ nowMs: 1_000 }));
    mockedWriteSync.mockClear();

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-boundary-summary", []),
      foldContext({ nowMs: 1_250 }),
    );

    // Assert.
    const error = logRecordsSince(0).find(
      (record) => record.message === "the compaction's summary record carried no text; recording the cut without a summary",
    );
    expect({
      summary: compactedOf(output.entries[0])?.summary,
      key: output.entries[0]?.upsertKey,
      level: (error as { level?: string } | undefined)?.level,
      stream: error?.context.stream,
      uuid: error?.context.uuid,
      age: error?.context.age_ms,
    }).toEqual({
      summary: undefined,
      key: "session:context_cut:uuid-boundary",
      level: "error",
      stream: "main",
      uuid: "uuid-boundary",
      age: 250,
    });
  });

  it("releases the cut without a summary when its content is neither text nor blocks", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext());

    // Act.
    const output = fold.onSdkMessage(compactSummaryRecord("uuid-boundary-summary", 42), foldContext());

    // Assert.
    expect({ key: output.entries[0]?.upsertKey, summary: compactedOf(output.entries[0])?.summary }).toEqual({
      key: "session:context_cut:uuid-boundary",
      summary: undefined,
    });
  });

  it("releases nothing on a restarted shim's fold, which holds no boundary", () => {
    // Arrange: the fold is per process, so a shim that restarted between the
    // boundary and its summary holds nothing. The file plane records that cut
    // from the transcript under the same key; this plane must not invent one.
    const restarted = createFold();

    // Act.
    const output = restarted.onSdkMessage(
      compactSummaryRecord("uuid-boundary-summary", "a summary for a boundary this process never saw"),
      foldContext(),
    );

    // Assert.
    expect(output.entries).toEqual([]);
  });
});

describe("a compaction cut still held when its turn ends", () => {
  /** A turn after a boundary whose summary never came: only tool calls, then the result. */
  function toolOnlyTurnAfterBoundary(): { fold: ReturnType<typeof createFold>; terminal: ReturnType<ReturnType<typeof createFold>["onSdkMessage"]> } {
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-boundary"), foldContext({ nowMs: 10_000 }));
    fold.onSdkMessage(
      assistant("msg-tool", [{ type: "tool_use", id: "toolu_read", name: "Read", input: { file_path: "/a" } }]),
      foldContext({ nowMs: 10_100 }),
    );
    fold.onSdkMessage(
      toolResult("toolu_read", { type: "text", file: { filePath: "/a", content: "x" } }),
      foldContext({ nowMs: 10_200 }),
    );
    mockedWriteSync.mockClear();
    const terminal = fold.onSdkMessage(streamMessage("result_success"), foldContext({ nowMs: 13_000 }));
    return { fold, terminal };
  }

  it("is released at the terminal, ahead of the terminal's own row", () => {
    // Arrange, Act.
    const { terminal } = toolOnlyTurnAfterBoundary();

    // Assert.
    expect({
      first: terminal.entries[0]?.upsertKey,
      summary: compactedOf(terminal.entries[0])?.summary,
      tokensAfter: compactedOf(terminal.entries[0])?.tokens?.tokensAfter,
      ended: terminal.turnEnded !== undefined,
      lastIsTerminal: terminal.entries.at(-1)?.upsertKey !== "session:context_cut:uuid-boundary",
    }).toEqual({
      first: "session:context_cut:uuid-boundary",
      summary: undefined,
      tokensAfter: 3_759n,
      ended: true,
      lastIsTerminal: true,
    });
  });

  it("records the missing summary at error, naming the stream, the boundary and its age", () => {
    // Arrange, Act.
    toolOnlyTurnAfterBoundary();

    // Assert.
    const error = logRecordsSince(0).find(
      (record) => record.message === "a compaction's summary never arrived; recording the held cut without a summary",
    );
    expect({
      level: (error as { level?: string } | undefined)?.level,
      stream: error?.context.stream,
      uuid: error?.context.uuid,
      age: error?.context.age_ms,
    }).toEqual({ level: "error", stream: "main", uuid: "uuid-boundary", age: 3_000 });
  });

  it("is not released twice: the next turn's terminal records no second cut", () => {
    // Arrange.
    const { fold } = toolOnlyTurnAfterBoundary();

    // Act.
    const next = fold.onSdkMessage(streamMessage("result_success"), foldContext());

    // Assert.
    expect(next.entries.filter((entry) => entry.upsertKey.startsWith("session:context_cut:"))).toEqual([]);
  });
});

describe("a second compaction boundary before the first one's summary", () => {
  it("records both cuts, in the order the vendor stated them", () => {
    // Arrange.
    const fold = createFold();
    const keys: string[] = [];
    const collect = (output: { entries: readonly { upsertKey: string }[] }): void => {
      keys.push(...output.entries.map((entry) => entry.upsertKey));
    };

    // Act.
    collect(fold.onSdkMessage(compactBoundary("uuid-first"), foldContext()));
    collect(fold.onSdkMessage(compactBoundary("uuid-second"), foldContext()));
    collect(fold.onSdkMessage(compactSummaryRecord("uuid-second-summary", "the second summary"), foldContext()));

    // Assert.
    expect(keys).toEqual(["session:context_cut:uuid-first", "session:context_cut:uuid-second"]);
  });

  it("releases the first at the second boundary without a summary, at error", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-first"), foldContext({ nowMs: 1_000 }));
    mockedWriteSync.mockClear();

    // Act.
    const output = fold.onSdkMessage(compactBoundary("uuid-second"), foldContext({ nowMs: 1_500 }));

    // Assert.
    const error = logRecordsSince(0).find(
      (record) => record.message === "a compaction's summary never arrived; recording the held cut without a summary",
    );
    expect({
      summary: compactedOf(output.entries[0])?.summary,
      uuid: error?.context.uuid,
      age: error?.context.age_ms,
    }).toEqual({ summary: undefined, uuid: "uuid-first", age: 500 });
  });

  it("gives the second cut the summary its own record states", () => {
    // Arrange.
    const fold = createFold();
    fold.onSdkMessage(compactBoundary("uuid-first"), foldContext());
    fold.onSdkMessage(compactBoundary("uuid-second"), foldContext());

    // Act.
    const output = fold.onSdkMessage(
      compactSummaryRecord("uuid-second-summary", "the second summary"),
      foldContext(),
    );

    // Assert.
    expect(compactedOf(output.entries[0])?.summary?.markdown).toBe("the second summary");
  });
});

// ---------------------------------------------------------------------------
// the calls in flight: held only while this stream can settle them
// ---------------------------------------------------------------------------

/** An assistant message on a SUBAGENT's stream: the spawning call it names. */
function assistantOn(spawningCall: string, messageId: string, content: unknown[]): SdkMessage {
  return {
    ...(assistant(messageId, content) as unknown as Record<string, unknown>),
    parent_tool_use_id: spawningCall,
  } as unknown as SdkMessage;
}

/** A subagent's tool result: on its stream, and with no typed output, as the SDK forwards it. */
function toolResultOn(spawningCall: string, toolUseId: string): SdkMessage {
  return {
    ...(toolResult(toolUseId, undefined) as unknown as Record<string, unknown>),
    parent_tool_use_id: spawningCall,
    tool_use_result: undefined,
  } as unknown as SdkMessage;
}

/** One `tool_use` content block. */
function toolUse(id: string, name: string, input: Record<string, unknown>): Record<string, unknown> {
  return { type: "tool_use", id, name, input };
}

/** The turn's terminal: `stopped` is a user stop, anything else ended on its own. */
function turnEnd(stopped: boolean): SdkMessage {
  const result = streamMessage("result_success") as unknown as Record<string, unknown>;
  return (stopped ? { ...result, terminal_reason: "aborted_streaming" } : result) as unknown as SdkMessage;
}

/** An async spawn's launch receipt. */
const LAUNCH_RECEIPT = { isAsync: true, status: "async_launched", agentId: "a-bg", description: "sweep" };

/** A patch moving the agent spawned by `toolu_spawn` (task `a-moved`) to the background. */
const MOVE_PATCH = {
  type: "system",
  subtype: "task_updated",
  uuid: "uuid-patch",
  session_id: "session-1",
  task_id: "a-moved",
  tool_use_id: "toolu_spawn",
  patch: { is_backgrounded: true },
} as unknown as SdkMessage;

/** A fold that saw `toolu_spawn` spawn agent task `a-moved` in the foreground. */
function spawnedInForeground(): ReturnType<typeof createFold> {
  return foldEach([
    assistant("msg-spawn", [toolUse("toolu_spawn", "Agent", { prompt: "count" })]),
    {
      type: "system",
      subtype: "task_started",
      uuid: "uuid-start",
      session_id: "session-1",
      task_id: "a-moved",
      tool_use_id: "toolu_spawn",
      task_type: "local_agent",
      description: "count",
      is_backgrounded: false,
    } as unknown as SdkMessage,
  ]);
}

/** A backgrounded shell's receipt. */
const SHELL_RECEIPT = { stdout: "", stderr: "", interrupted: false, backgroundTaskId: "b-bg" };

/** Fold every message, in order, through one fold. */
function foldEach(messages: readonly SdkMessage[]): ReturnType<typeof createFold> {
  const fold = createFold();
  for (const message of messages) fold.onSdkMessage(message, foldContext());
  return fold;
}

/** The ids the fold still holds. */
function heldIds(fold: ReturnType<typeof createFold>): string[] {
  return fold.inFlightCalls().map((call) => call.toolUseId);
}

describe("the calls in flight", () => {
  it("releases an awaited subagent's call when its untyped result arrives", () => {
    // Arrange, Act: a subagent's result reaches this stream with no
    // `tool_use_result`, so no terminal is written — and before the fix the
    // call was re-remembered forever.
    const fold = foldEach([
      assistant("msg-spawn", [toolUse("toolu_spawn", "Agent", { prompt: "read" })]),
      assistantOn("toolu_spawn", "msg-sub", [toolUse("toolu_read", "Read", { file_path: "/a" })]),
      toolResultOn("toolu_spawn", "toolu_read"),
    ]);

    // Assert
    expect(heldIds(fold)).toEqual(["toolu_spawn"]);
  });

  it("never holds a backgrounded agent's call, whose result never reaches this stream", () => {
    // Arrange, Act
    const fold = foldEach([
      assistant("msg-spawn", [toolUse("toolu_spawn", "Agent", { prompt: "sweep", run_in_background: true })]),
      toolResult("toolu_spawn", LAUNCH_RECEIPT),
      assistantOn("toolu_spawn", "msg-sub", [toolUse("toolu_sub_bash", "Bash", { command: "ls" })]),
    ]);

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });

  it("still announces a backgrounded agent's call it does not hold", () => {
    // Arrange
    const fold = foldEach([
      assistant("msg-spawn", [toolUse("toolu_spawn", "Agent", { prompt: "sweep", run_in_background: true })]),
      toolResult("toolu_spawn", LAUNCH_RECEIPT),
    ]);

    // Act
    const output = fold.onSdkMessage(
      assistantOn("toolu_spawn", "msg-sub", [toolUse("toolu_sub_bash", "Bash", { command: "ls" })]),
      foldContext(),
    );

    // Assert
    expect(output.entries.map((entry) => entry.upsertKey)).toContain("activity:toolu_sub_bash");
  });

  it("releases a backgrounded shell's call at its receipt", () => {
    // Arrange, Act
    const fold = foldEach([
      assistant("msg-bash", [toolUse("toolu_bg", "Bash", { command: "sleep 600", run_in_background: true })]),
      toolResult("toolu_bg", SHELL_RECEIPT),
    ]);

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });

  it("releases a monitor's call at its arming receipt", () => {
    // Arrange, Act
    const fold = foldEach([
      assistant("msg-mon", [toolUse("toolu_mon", "Monitor", { command: "tail -f log", description: "watch" })]),
      toolResult("toolu_mon", { taskId: "m1", timeoutMs: 600000, persistent: false }),
    ]);

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });

  it("releases a hand-backgrounded agent's calls at the patch that moves it", () => {
    // Arrange
    const fold = foldEach([
      assistant("msg-spawn", [toolUse("toolu_spawn", "Agent", { prompt: "count" })]),
      assistantOn("toolu_spawn", "msg-sub", [toolUse("toolu_sub_bash", "Bash", { command: "sleep 90" })]),
    ]);

    // Act
    fold.onSdkMessage(
      {
        type: "system",
        subtype: "task_updated",
        uuid: "uuid-patch",
        session_id: "session-1",
        task_id: "a-hand",
        tool_use_id: "toolu_spawn",
        patch: { is_backgrounded: true },
      } as unknown as SdkMessage,
      foldContext(),
    );

    // Assert
    expect(heldIds(fold)).toEqual(["toolu_spawn"]);
  });

  it("keeps a patch-moved agent `vendor_moved` through its launch receipt, which states no cause", () => {
    // Arrange
    const fold = spawnedInForeground();
    const messages = [MOVE_PATCH, toolResult("toolu_spawn", { ...LAUNCH_RECEIPT, agentId: "a-moved" })];

    // Act
    const discriminators = messages.flatMap((message) =>
      fold.onSdkMessage(message, foldContext()).entries.map((entry) => entry.source.discriminator),
    );

    // Assert
    expect(discriminators.filter((d) => d.startsWith("agent_frame.detached_work.detached"))).toEqual([
      "agent_frame.detached_work.detached.vendor_moved",
    ]);
  });

  it("announces a move the engine noted as the user's `by_user`", () => {
    // Arrange
    const fold = spawnedInForeground();
    fold.noteUserDetach("toolu_spawn");

    // Act
    const output = fold.onSdkMessage(MOVE_PATCH, foldContext());

    // Assert
    expect(output.entries.map((entry) => entry.source.discriminator)).toContain(
      "agent_frame.detached_work.detached.by_user",
    );
  });

  it("a request the engine retired leaves the move `vendor_moved`", () => {
    // Arrange
    const fold = spawnedInForeground();
    fold.noteUserDetach("toolu_spawn");
    fold.retireUserDetach("toolu_spawn", "the vendor moved nothing for the request");

    // Act
    const output = fold.onSdkMessage(MOVE_PATCH, foldContext());

    // Assert
    expect(output.entries.map((entry) => entry.source.discriminator)).toContain(
      "agent_frame.detached_work.detached.vendor_moved",
    );
  });

  it("the query's end retires every request", () => {
    // Arrange
    const fold = spawnedInForeground();
    fold.noteUserDetach("toolu_spawn");
    fold.endQuery("the query was replaced");

    // Act
    const output = fold.onSdkMessage(MOVE_PATCH, foldContext());

    // Assert
    expect(output.entries.map((entry) => entry.source.discriminator)).toContain(
      "agent_frame.detached_work.detached.vendor_moved",
    );
  });

  it("holds nothing after a turn that ended on its own", () => {
    // Arrange: a skill whose document never reaches this stream.
    const fold = foldEach([
      assistant("msg-skill", [toolUse("toolu_skill", "Skill", { skill: "debug-logs" })]),
      toolResult("toolu_skill", { success: true, commandName: "debug-logs" }),
    ]);

    // Act
    fold.onSdkMessage(turnEnd(false), foldContext());

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });

  it("returns to zero after a mixed workload of every leak path", () => {
    // Arrange, Act: interleaved main and subagent traffic, a sync and an
    // async spawn, a backgrounded shell, a monitor and a skill, in one turn.
    const fold = foldEach([
      assistant("msg-1", [toolUse("toolu_sync", "Agent", { prompt: "read" })]),
      assistant("msg-2", [toolUse("toolu_async", "Agent", { prompt: "sweep", run_in_background: true })]),
      assistantOn("toolu_sync", "msg-s1", [toolUse("toolu_s_read", "Read", { file_path: "/a" })]),
      toolResult("toolu_async", LAUNCH_RECEIPT),
      assistantOn("toolu_async", "msg-a1", [toolUse("toolu_a_bash", "Bash", { command: "ls" })]),
      toolResultOn("toolu_sync", "toolu_s_read"),
      assistant("msg-3", [toolUse("toolu_bg", "Bash", { command: "sleep 600", run_in_background: true })]),
      toolResult("toolu_bg", SHELL_RECEIPT),
      assistant("msg-4", [toolUse("toolu_mon", "Monitor", { command: "tail -f log", description: "watch" })]),
      toolResult("toolu_mon", { taskId: "m1", timeoutMs: 600000, persistent: false }),
      assistant("msg-5", [toolUse("toolu_skill", "Skill", { skill: "debug-logs" })]),
      toolResult("toolu_skill", { success: true, commandName: "debug-logs" }),
      assistantOn("toolu_async", "msg-a2", [toolUse("toolu_a_read", "Read", { file_path: "/b" })]),
      toolResult("toolu_sync", { status: "completed", prompt: "read", content: [], totalDurationMs: 1 }),
      turnEnd(false),
    ]);

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });

  it("cuts only the genuinely open call when a stop lands after a mixed workload", () => {
    // Arrange: every leak path first, then one foreground shell and one read
    // the stop lands inside.
    const fold = foldEach([
      assistant("msg-1", [toolUse("toolu_async", "Agent", { prompt: "sweep", run_in_background: true })]),
      toolResult("toolu_async", LAUNCH_RECEIPT),
      assistantOn("toolu_async", "msg-a1", [toolUse("toolu_a_bash", "Bash", { command: "ls" })]),
      assistant("msg-2", [toolUse("toolu_bg", "Bash", { command: "sleep 600", run_in_background: true })]),
      toolResult("toolu_bg", SHELL_RECEIPT),
      assistant("msg-3", [toolUse("toolu_sync", "Agent", { prompt: "read" })]),
      assistantOn("toolu_sync", "msg-s1", [toolUse("toolu_s_bash", "Bash", { command: "ls" })]),
      toolResultOn("toolu_sync", "toolu_s_bash"),
      toolResult("toolu_sync", { status: "completed", prompt: "read", content: [], totalDurationMs: 1 }),
      assistant("msg-4", [toolUse("toolu_fg", "Bash", { command: "sleep 600" })]),
      assistant("msg-5", [toolUse("toolu_open_read", "Read", { file_path: "/c" })]),
    ]);

    // Act
    const output = fold.onSdkMessage(turnEnd(true), foldContext());

    // Assert
    const cut = output.entries.filter((entry) => entry.source.discriminator.endsWith(".interrupted"));
    expect(cut.map((entry) => entry.upsertKey)).toEqual(["activity:toolu_fg"]);
  });

  it("holds nothing after a stop", () => {
    // Arrange
    const fold = foldEach([
      assistant("msg-4", [toolUse("toolu_fg", "Bash", { command: "sleep 600" })]),
      assistant("msg-5", [toolUse("toolu_open_read", "Read", { file_path: "/c" })]),
    ]);

    // Act
    fold.onSdkMessage(turnEnd(true), foldContext());

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });

  it("releases every call when the query ends", () => {
    // Arrange
    const fold = foldEach([assistant("msg-4", [toolUse("toolu_fg", "Bash", { command: "sleep 600" })])]);

    // Act
    fold.endQuery("the vendor query died");

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });
});

describe("the calls in flight across every capture", () => {
  it.each(captureNames())("holds nothing at any turn terminal of %s", (scenario) => {
    // Arrange
    const fold = createFold();
    const context = goldenContext(scenario);
    const heldAtTerminals: string[] = [];

    // Act
    for (const message of sdkMessages(scenario)) {
      fold.onSdkMessage(message, context);
      if (message.type === "result") heldAtTerminals.push(...heldIds(fold));
    }

    // Assert
    expect(heldAtTerminals).toEqual([]);
  });
});

describe("the calls in flight under the mocked vendor", () => {
  it("returns to zero after a mixed workload driven through the fake SDK", async () => {
    // Arrange: sync and async spawns, interleaved subagent streams, background
    // shells, a monitor and a skill, each a turn of its own.
    const driven = expectDroveCleanly(
      await driveScenario([
        "!subagent",
        "!subagent-detached",
        "!subagent-interleaved",
        "!subagent-failed",
        "!bash-detach",
        "!bash-timeout",
        "!monitor-persistent",
        "!skill",
        "!send-message",
        "!bash",
      ]),
    );

    // Act
    const fold = createFold();
    for (const message of driven.messages) fold.onSdkMessage(message, foldContext());

    // Assert
    expect(heldIds(fold)).toEqual([]);
  });
});
