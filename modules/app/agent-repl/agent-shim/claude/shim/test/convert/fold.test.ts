/**
 * The FOLD, driven with the real corpus.
 *
 * One test per behavior the contract names, and every input is either a real
 * anonymized capture from `testdata/corpus` or a sequence assembled from those
 * captures the way the SDK emits one. A converter that agrees only with our own
 * idea of the vendor's shapes fails here.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import { EMPTY_FOLD_OUTPUT, createFold } from "../../src/convert/fold.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { activityOf, foldContext, residueOf, streamMessage } from "./fold-harness.js";

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

  it("makes an unknown tool an unmodeled unit, and only an unknown one", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-mcp", [
        { type: "tool_use", id: "toolu_m", name: "mcp__Slack__send", input: { text: "hi" } },
      ]),
      foldContext(),
    );

    expect(activityOf(output.entries[0])?.item.case).toBe("unmodeled");
  });

  it("resolves an MCP server by LOOKUP against the names the session knows", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-mcp", [{ type: "tool_use", id: "toolu_m", name: "mcp__Slack__send", input: {} }]),
      foldContext({ mcpServerNames: ["Slack"] }),
    );

    const unmodeled = activityOf(output.entries[0])?.item.value as conversationv1.AgentUnmodeled;
    const start = unmodeled.result.value as conversationv1.AgentUnmodeledStart;
    expect(start.mcpServer).toBe("Slack");
  });

  it("leaves the MCP server unset when no known name matches, rather than guessing", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      assistant("msg-mcp", [{ type: "tool_use", id: "toolu_m", name: "mcp__Slack__send", input: {} }]),
      foldContext({ mcpServerNames: ["Gmail"] }),
    );

    const unmodeled = activityOf(output.entries[0])?.item.value as conversationv1.AgentUnmodeled;
    const start = unmodeled.result.value as conversationv1.AgentUnmodeledStart;
    expect(start.mcpServer).toBeUndefined();
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

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    expect(frame?.result.case).toBe("detachedWork");
    expect(output.entries).toHaveLength(1);
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

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const detached = frame?.result.value as conversationv1.AgentDetachedWork;
    const origin = detached.origin.value as conversationv1.DetachedWorkDetached;
    expect(origin.cause.case).toBe("timedOut");
  });
});

describe("detached work", () => {
  it("announces work that left the turn", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(streamMessage("task_started"), foldContext());

    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    expect(frame?.result.case).toBe("detachedWork");
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

  it("holds a compaction boundary until its summary arrives, then records the cut", () => {
    const fold = createFold();
    const held = fold.onSdkMessage(
      {
        type: "system",
        subtype: "compact_boundary",
        compact_metadata: {
          trigger: "auto",
          pre_tokens: 180_000,
          post_tokens: 12_000,
          duration_ms: 4_000,
        },
        uuid: "uuid-boundary",
        session_id: "session-1",
      } as unknown as SdkMessage,
      foldContext(),
    );

    const output = fold.onSdkMessage(
      assistant("msg-summary", [{ type: "text", text: "we discussed the fold" }]),
      foldContext(),
    );

    expect(held.entries).toHaveLength(0);
    const frame =
      output.entries[0]?.item.kind === "frame" ? output.entries[0].item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const cut = update.value as conversationv1.ContextCut;
    const compacted = cut.cut.value as conversationv1.ContextCompacted;
    expect(compacted.summary?.markdown).toBe("we discussed the fold");
    expect(compacted.tokens?.tokensBefore).toBe(180_000n);
    expect(compacted.trigger.case).toBe("automatic");
  });

  it("records a conversation reset as an identity rotation AND a cleared cut", () => {
    const fold = createFold();

    const output = fold.onSdkMessage(
      {
        type: "conversation_reset",
        new_conversation_id: "session-2",
        uuid: "uuid-reset",
        session_id: "session-1",
      } as unknown as SdkMessage,
      foldContext(),
    );

    const rotated =
      output.entries[0]?.item.kind === "session_update" ? output.entries[0].item.update : undefined;
    expect(rotated?.update.case).toBe("identityRotated");
    const frame =
      output.entries[1]?.item.kind === "frame" ? output.entries[1].item.frame : undefined;
    const cut = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.ContextCut;
    expect(cut.cut.case).toBe("cleared");
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

  it("settles NOTHING from a denied call's tool_result", () => {
    // A denied tool never ran. The vendor still emits a tool_result for it --
    // the deny message IS the result the model sees -- and folding that into an
    // activity would put a settled unit in the feed for work that never
    // happened, when the permission frame has already given the whole account.
    const fold = createFold();
    const denied = foldContext({ deniedCall: (toolUseId) => toolUseId === "toolu_d" });
    fold.onSdkMessage(
      assistant("msg-denied", [
        { type: "tool_use", id: "toolu_d", name: "Bash", input: { command: "rm -rf /" } },
      ]),
      denied,
    );

    const output = fold.onSdkMessage(toolResult("toolu_d", { stdout: "" }, true), denied);

    expect(output.entries).toHaveLength(0);
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

    expect(output.entries[0]?.agentId.value).toBe("agent-7");
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
    expect(output.entries[0]?.agentId.value).toBe("toolu_spawn");
  });
});
