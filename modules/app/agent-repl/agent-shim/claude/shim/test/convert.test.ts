import { beforeEach, describe, expect, it } from "vitest";
import { readFileSync, readdirSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { toJson } from "@bufbuild/protobuf";
import type { JsonObject, JsonValue } from "@bufbuild/protobuf";
import { anyUnpack, ListValueSchema, type ListValue } from "@bufbuild/protobuf/wkt";
import { SessionStartGate, convert, convertToolUseResult, promptPreview } from "../src/proto/convert.js";
import { __resetExtrasSeen } from "../src/proto/extras.js";
import { EventClass, Plane, SessionSource } from "../src/uds/proto.js";
import {
  ApiKeySource,
  ClaudeStreamMessageSchema,
  type ClaudeStreamMessage,
} from "../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";
import { DiscriminatorField } from "../../../../proto/gen/ts/agentshim/data/v1/unknown_pb.js";

const streamDir = fileURLToPath(new URL("../../../../testdata/corpus/stream/", import.meta.url));
const toolResultsDir = fileURLToPath(new URL("../../../../testdata/corpus/tool-results/", import.meta.url));

function loadStream(name: string): Record<string, unknown> {
  const line = readFileSync(new URL(`../../../../testdata/corpus/stream/${name}.jsonl`, import.meta.url), "utf8").split("\n")[0]!;
  return JSON.parse(line) as Record<string, unknown>;
}

function loadToolResult(name: string): unknown {
  const line = readFileSync(new URL(`../../../../testdata/corpus/tool-results/${name}.jsonl`, import.meta.url), "utf8").split("\n")[0]!;
  return (JSON.parse(line) as Record<string, unknown>)["toolUseResult"];
}

/** Unpack the vendor Any of a convert() result into a ClaudeStreamMessage. */
function vendor(result: ReturnType<typeof convert>): ClaudeStreamMessage {
  expect(result.vendor.payload.case).toBe("vendor");
  const any = result.vendor.payload.value;
  const csm = anyUnpack(any as Parameters<typeof anyUnpack>[0], ClaudeStreamMessageSchema);
  expect(csm).toBeDefined();
  return csm!;
}

beforeEach(() => __resetExtrasSeen());

// ---------------------------------------------------------------------------
// GOLDEN CONTRACT: every corpus/stream fixture decodes cleanly.
// ---------------------------------------------------------------------------

const EXPECTED_CASE: Record<string, ClaudeStreamMessage["msg"]["case"]> = {
  assistant: "assistant",
  background_tasks_changed: "backgroundTasksChanged",
  hook_response: "hookResponse",
  hook_started: "hookStarted",
  notification: "notification",
  rate_limit_event: "rateLimitEvent",
  result_success: "result",
  status: "status",
  "stream_event-content_block_delta-signature": "streamEvent",
  "stream_event-content_block_delta-text": "streamEvent",
  "stream_event-content_block_delta-thinking": "streamEvent",
  "stream_event-content_block_start": "streamEvent",
  "stream_event-content_block_stop": "streamEvent",
  "stream_event-message_delta": "streamEvent",
  "stream_event-message_start": "streamEvent",
  "stream_event-message_stop": "streamEvent",
  system_init: "systemInit",
  task_notification: "taskNotification",
  task_started: "taskStarted",
  task_updated: "taskUpdated",
  thinking_tokens: "thinkingTokens",
  user: "user",
};

describe("golden corpus: convert() over every stream fixture", () => {
  const files = readdirSync(streamDir).filter((f) => f.endsWith(".jsonl")).map((f) => f.replace(/\.jsonl$/, ""));

  it("covers all 22 stream fixture types with an expected case", () => {
    expect(files.sort()).toEqual(Object.keys(EXPECTED_CASE).sort());
  });

  for (const name of Object.keys(EXPECTED_CASE)) {
    describe(name, () => {
      it("produces a vendor Event (never UnparsedEvent)", () => {
        const result = convert(loadStream(name));
        expect(result.vendor.payload.case).toBe("vendor");
      });

      it("logs ZERO unknown fields", () => {
        const result = convert(loadStream(name));
        expect(result.loggedExtras).toEqual([]);
      });

      it("wraps the expected ClaudeStreamMessage case", () => {
        expect(vendor(convert(loadStream(name))).msg.case).toBe(EXPECTED_CASE[name]);
      });

      it("stamps plane=STREAM and seq=0", () => {
        const result = convert(loadStream(name));
        expect(result.vendor.plane).toBe(Plane.STREAM);
        expect(result.vendor.seq).toBe(0n);
      });
    });
  }
});

// ---------------------------------------------------------------------------
// EventClass: EPHEMERAL for stream_event, PERSISTENT otherwise.
// ---------------------------------------------------------------------------

describe("EventClass classification", () => {
  it("marks stream_event EPHEMERAL", () => {
    expect(convert(loadStream("stream_event-message_start")).vendor.class).toBe(EventClass.EPHEMERAL);
  });

  it("marks assistant PERSISTENT", () => {
    expect(convert(loadStream("assistant")).vendor.class).toBe(EventClass.PERSISTENT);
  });
});

// ---------------------------------------------------------------------------
// Per-type field assertions.
// ---------------------------------------------------------------------------

describe("assistant", () => {
  const csm = () => vendor(convert(loadStream("assistant")));

  it("maps the text content block", () => {
    const m = csm();
    if (m.msg.case !== "assistant") throw new Error("case");
    const block = m.msg.value.message!.content[0]!;
    expect(block.block.case).toBe("text");
    if (block.block.case !== "text") throw new Error("block");
    expect(block.block.value.text).toBe("hi");
  });

  it("maps the model and usage input tokens", () => {
    const m = csm();
    if (m.msg.case !== "assistant") throw new Error("case");
    expect(m.msg.value.message!.model).toBe("claude-haiku-4-5-20251001");
    expect(m.msg.value.message!.usage!.inputTokens).toBe(10n);
  });

  it("carries the top-level request_id onto the Event envelope", () => {
    expect(convert(loadStream("assistant")).vendor.requestId).toBe("req_011CdKQJZD99Dyyq53xXHGai");
  });

  it("parses the top-level timestamp into produced_at_ms", () => {
    const ms = convert(loadStream("assistant")).vendor.producedAtMs;
    expect(ms).toBe(BigInt(Date.parse("2026-07-23T17:42:47.752Z")));
  });

  it("has_error false with no error sibling", () => {
    const m = csm();
    if (m.msg.case !== "assistant") throw new Error("case");
    expect(m.msg.value.hasError).toBe(false);
  });
});

describe("result_success", () => {
  const csm = () => {
    const m = vendor(convert(loadStream("result_success")));
    if (m.msg.case !== "result") throw new Error("case");
    return m.msg.value;
  };

  it("maps subtype SUCCESS", () => expect(csm().subtype).toBe(1 /* RESULT_SUBTYPE_SUCCESS */));
  it("maps duration_ms as bigint", () => expect(csm().durationMs).toBe(1236n));
  it("maps result text", () => expect(csm().result).toBe("hi"));
  it("maps total_cost_usd as double", () => expect(csm().totalCostUsd).toBeCloseTo(0.0088135));
  it("maps stop_reason", () => expect(csm().stopReason).toBe("end_turn"));
  it("maps camelCase modelUsage into the map", () => {
    expect(csm().modelUsage["claude-haiku-4-5-20251001"]!.contextWindow).toBe(200000n);
  });
  it("api_error_status null → apiErrorStatusSet false", () => expect(csm().apiErrorStatusSet).toBe(false));
});

describe("system_init", () => {
  const csm = () => {
    const m = vendor(convert(loadStream("system_init")));
    if (m.msg.case !== "systemInit") throw new Error("case");
    return m.msg.value;
  };
  it("maps cwd", () => expect(csm().cwd).toBe("/private/tmp/sdk-probe"));
  it("maps camelCase permissionMode", () => expect(csm().permissionMode).toBe("default"));
  it("maps apiKeySource 'none' → NONE", () => expect(csm().apiKeySource).toBe(ApiKeySource.NONE));
  it("maps tools list", () => expect(csm().tools).toContain("Bash"));
  it("maps capabilities as a typed list (not extras)", () => {
    const result = convert(loadStream("system_init"));
    expect(csm().capabilities).toContain("interrupt_receipt_v1");
    expect(result.vendor.extras?.["capabilities"]).toBeUndefined();
    expect(result.loggedExtras).toEqual([]);
  });
  it("maps analytics_disabled/product_feedback_disabled as typed bools", () => {
    expect(csm().analyticsDisabled).toBe(false);
    expect(csm().productFeedbackDisabled).toBe(false);
  });
  it("maps memory_paths as a typed string map (not extras)", () => {
    const result = convert(loadStream("system_init"));
    expect(csm().memoryPaths["auto"]).toContain("/memory/");
    expect(result.vendor.extras?.["memory_paths"]).toBeUndefined();
  });
  it("maps plugin source/version as typed PluginRef fields", () => {
    const p = csm().plugins[0]!;
    expect(p.source).toBe("gns-cowork@chesscom-gns");
    expect(p.version).toBe("9.8.1");
  });
});

describe("rate_limit_event (camelCase nested fields)", () => {
  const info = () => {
    const m = vendor(convert(loadStream("rate_limit_event")));
    if (m.msg.case !== "rateLimitEvent") throw new Error("case");
    return m.msg.value.rateLimitInfo!;
  };
  it("normalizes resetsAt → resets_at", () => expect(info().resetsAt).toBe(1785542400n));
  it("normalizes rateLimitType", () => expect(info().rateLimitType).toBe("overage"));
  it("normalizes overageInUse", () => expect(info().overageInUse).toBe(true));
});

describe("hook_response", () => {
  const csm = () => {
    const m = vendor(convert(loadStream("hook_response")));
    if (m.msg.case !== "hookResponse") throw new Error("case");
    return m.msg.value;
  };
  it("maps outcome", () => expect(csm().outcome).toBe("success"));
  it("exit_code 0 present → exitCodeSet true", () => expect(csm().exitCodeSet).toBe(true));
});

describe("user", () => {
  const csm = () => {
    const m = vendor(convert(loadStream("user")));
    if (m.msg.case !== "user") throw new Error("case");
    return m.msg.value;
  };
  it("maps content string-or-blocks (blocks arm)", () => {
    expect(csm().message!.content.case).toBe("contentBlocks");
  });
  it("maps subagent_type as a typed UserMessage field (not extras)", () => {
    const result = convert(loadStream("user"));
    expect(csm().subagentType).toBe("claude");
    expect(result.vendor.extras?.["subagent_type"]).toBeUndefined();
  });
  it("maps task_description as a typed UserMessage field", () => {
    expect(csm().taskDescription).toBe("sync echo probe");
  });
  it("still logs ZERO unknown fields with subagent context now typed", () => {
    expect(convert(loadStream("user")).loggedExtras).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// Lifecycle twins.
// ---------------------------------------------------------------------------

describe("promptPreview", () => {
  it("keeps only the first line", () => {
    expect(promptPreview("first line\nsecond line")).toBe("first line");
  });

  it("caps the preview at 200 characters", () => {
    expect(promptPreview("x".repeat(500))).toHaveLength(200);
  });

  it("passes a short single-line prompt through unchanged", () => {
    expect(promptPreview("hello there")).toBe("hello there");
  });
});

describe("lifecycle twins", () => {
  it("system:init emits SessionStarted", () => {
    const { lifecycle } = convert(loadStream("system_init"));
    expect(lifecycle).toHaveLength(1);
    expect(lifecycle[0]!.payload.case).toBe("sessionStarted");
    if (lifecycle[0]!.payload.case !== "sessionStarted") throw new Error("case");
    expect(lifecycle[0]!.payload.value.model).toBe("claude-haiku-4-5-20251001");
    expect(lifecycle[0]!.payload.value.vendorSessionId).toBe("f7b59684-e29e-469c-a7e7-47bbc1828fb6");
  });

  it("SessionStarted.source defaults to FRESH when no sessionSource option is given", () => {
    // Arrange / Act: the pre-seam call site (no opts) is unchanged.
    const { lifecycle } = convert(loadStream("system_init"));
    // Assert
    if (lifecycle[0]!.payload.case !== "sessionStarted") throw new Error("case");
    expect(lifecycle[0]!.payload.value.source).toBe(SessionSource.FRESH);
  });

  it("SessionStarted.source is RESUME when the resume seam is passed", () => {
    // Arrange / Act: the stitch phase passes this when spawned with --resume.
    const { lifecycle } = convert(loadStream("system_init"), { sessionSource: SessionSource.RESUME });
    // Assert
    if (lifecycle[0]!.payload.case !== "sessionStarted") throw new Error("case");
    expect(lifecycle[0]!.payload.value.source).toBe(SessionSource.RESUME);
  });

  it("a second system:init under one gate emits NO SessionStarted twin", () => {
    // Arrange: the SDK re-inits per submit, which used to knock the SSM back
    // to IDLE at exactly the moment a submit put it into THINKING.
    const sessionGate = new SessionStartGate();
    convert(loadStream("system_init"), { sessionGate });
    // Act
    const { lifecycle } = convert(loadStream("system_init"), { sessionGate });
    // Assert
    expect(lifecycle).toHaveLength(0);
  });

  it("the FIRST system:init under a gate still emits SessionStarted", () => {
    // Arrange
    const sessionGate = new SessionStartGate();
    // Act
    const { lifecycle } = convert(loadStream("system_init"), { sessionGate });
    // Assert
    expect(lifecycle[0]!.payload.case).toBe("sessionStarted");
  });

  it("a system:init announcing a NEW vendor session id re-emits SessionStarted", () => {
    // Arrange: a conversation_reset / compact-continue genuinely starts a new
    // session, so the gate must re-admit it.
    const sessionGate = new SessionStartGate();
    convert(loadStream("system_init"), { sessionGate });
    const reinit = { ...(loadStream("system_init") as Record<string, unknown>), session_id: "a-different-uuid" };
    // Act
    const { lifecycle } = convert(reinit, { sessionGate });
    // Assert
    expect(lifecycle[0]!.payload.case).toBe("sessionStarted");
  });

  it("system:init emits SessionStarted when no gate is injected", () => {
    // Arrange / Act: the single-message decode path (probes, unit tests) has
    // no shim lifetime to be the second init of.
    const { lifecycle } = convert(loadStream("system_init"));
    // Assert
    expect(lifecycle).toHaveLength(1);
  });

  it("SessionStarted.source honors an explicit FRESH option", () => {
    // Arrange / Act
    const { lifecycle } = convert(loadStream("system_init"), { sessionSource: SessionSource.FRESH });
    // Assert
    if (lifecycle[0]!.payload.case !== "sessionStarted") throw new Error("case");
    expect(lifecycle[0]!.payload.value.source).toBe(SessionSource.FRESH);
  });

  it("result emits TurnEnded with stop_reason/duration/is_error", () => {
    const { lifecycle } = convert(loadStream("result_success"));
    expect(lifecycle).toHaveLength(1);
    if (lifecycle[0]!.payload.case !== "turnEnded") throw new Error("case");
    expect(lifecycle[0]!.payload.value.stopReason).toBe("end_turn");
    expect(lifecycle[0]!.payload.value.durationMs).toBe(1236n);
    expect(lifecycle[0]!.payload.value.isError).toBe(false);
  });

  it("a plain user message emits NO TurnStarted (turn start is shim-authoritative)", () => {
    // A `user` message is a replayed echo, not a turn start: deriving one from
    // it never fired for a live submit and double-counted turns on replay.
    expect(convert(loadStream("user")).lifecycle).toHaveLength(0);
  });

  it("a tool-result user message emits NO TurnStarted", () => {
    const toolResultUser = {
      type: "user",
      message: { role: "user", content: [{ type: "tool_result", tool_use_id: "toolu_x", content: "ok" }] },
      session_id: "s1",
      uuid: "u1",
    };
    expect(convert(toolResultUser).lifecycle).toHaveLength(0);
  });

  it("a REPLAYED user message emits NO TurnStarted", () => {
    // Arrange: the resume/rehydration echo, the one shape that DID reach the
    // old derivation and so double-counted the turn it was replaying.
    const replayed = { ...(loadStream("user") as Record<string, unknown>), is_replay: true };
    // Act
    const { lifecycle } = convert(replayed);
    // Assert
    expect(lifecycle).toHaveLength(0);
  });

  it("task_started emits TaskStarted with SHELL kind for local_bash", () => {
    const { lifecycle } = convert(loadStream("task_started"));
    expect(lifecycle).toHaveLength(1);
    if (lifecycle[0]!.payload.case !== "taskStarted") throw new Error("case");
    expect(lifecycle[0]!.payload.value.kind).toBe(2 /* TASK_KIND_SHELL */);
    expect(lifecycle[0]!.payload.value.taskId).toBe("b86pl7ir1");
  });

  it("task_notification 'completed' emits TaskEnded DONE", () => {
    const { lifecycle } = convert(loadStream("task_notification"));
    expect(lifecycle).toHaveLength(1);
    if (lifecycle[0]!.payload.case !== "taskEnded") throw new Error("case");
    expect(lifecycle[0]!.payload.value.status).toBe(1 /* TERMINAL_STATUS_DONE */);
    expect(lifecycle[0]!.payload.value.outputPath).toContain("a0cbd94e5da2d662d.output");
  });

  it("task_updated 'failed' emits TaskEnded ERROR", () => {
    const { lifecycle } = convert(loadStream("task_updated"));
    if (lifecycle[0]!.payload.case !== "taskEnded") throw new Error("case");
    expect(lifecycle[0]!.payload.value.status).toBe(2 /* TERMINAL_STATUS_ERROR */);
  });

  it("'stopped' status maps to STOPPED (per RawTaskStatus vocabulary)", () => {
    const stopped = { type: "system", subtype: "task_notification", task_id: "t", status: "stopped", session_id: "s", uuid: "u" };
    const { lifecycle } = convert(stopped);
    if (lifecycle[0]!.payload.case !== "taskEnded") throw new Error("case");
    expect(lifecycle[0]!.payload.value.status).toBe(4 /* TERMINAL_STATUS_STOPPED */);
  });

  it("background_tasks_changed is vendor-only (no twin)", () => {
    expect(convert(loadStream("background_tasks_changed")).lifecycle).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// StreamEvent ContentBlockDelta arms (total-fidelity typed model).
// ---------------------------------------------------------------------------

describe("StreamEvent content_block_delta arms", () => {
  function deltaArm(name: string) {
    const m = vendor(convert(loadStream(name)));
    if (m.msg.case !== "streamEvent") throw new Error("case");
    const ev = m.msg.value.event!.event;
    if (ev.case !== "contentBlockDelta") throw new Error("not a delta");
    return ev.value.delta;
  }

  it("text_delta arm", () => {
    const d = deltaArm("stream_event-content_block_delta-text");
    expect(d.case).toBe("textDelta");
    if (d.case !== "textDelta") throw new Error("arm");
    expect(d.value.text).toBe("hi");
  });

  it("thinking_delta arm with null estimated_tokens → set=false", () => {
    const d = deltaArm("stream_event-content_block_delta-thinking");
    if (d.case !== "thinkingDelta") throw new Error("arm");
    expect(d.value.thinking).toBe("The user is");
    expect(d.value.estimatedTokensSet).toBe(false);
  });

  it("signature_delta arm", () => {
    const d = deltaArm("stream_event-content_block_delta-signature");
    expect(d.case).toBe("signatureDelta");
  });

  it("input_json_delta arm (synthetic)", () => {
    const msg = { type: "stream_event", event: { type: "content_block_delta", index: 0, delta: { type: "input_json_delta", partial_json: "{\"a\":" } }, session_id: "s", uuid: "u" };
    const m = vendor(convert(msg));
    if (m.msg.case !== "streamEvent") throw new Error("case");
    const ev = m.msg.value.event!.event;
    if (ev.case !== "contentBlockDelta" || ev.value.delta.case !== "inputJsonDelta") throw new Error("arm");
    expect(ev.value.delta.value.partialJson).toBe("{\"a\":");
  });

  it("message_start maps ttft_ms as a typed StreamEvent field (not extras)", () => {
    const result = convert(loadStream("stream_event-message_start"));
    const m = vendor(result);
    if (m.msg.case !== "streamEvent") throw new Error("case");
    expect(m.msg.value.ttftMs).toBe(865n);
    expect(result.vendor.extras?.["ttft_ms"]).toBeUndefined();
    expect(result.loggedExtras).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// toolUseResult string-or-object union.
// ---------------------------------------------------------------------------

describe("convertToolUseResult union", () => {
  it("error string routes to the raw_string arm", () => {
    const r = convertToolUseResult(loadToolResult("raw_string"));
    expect(r.result.case).toBe("rawString");
    if (r.result.case !== "rawString") throw new Error("arm");
    expect(r.result.value).toBe("Error: denied from webapp");
  });

  it("plain string (rejection) routes to raw_string", () => {
    const r = convertToolUseResult("User rejected tool use");
    expect(r.result.case).toBe("rawString");
  });

  it("skill object classifies to the skill arm", () => {
    const r = convertToolUseResult(loadToolResult("skill"));
    expect(r.result.case).toBe("skill");
    if (r.result.case !== "skill") throw new Error("arm");
    expect(r.result.value.commandName).toBe("debug-logs");
    expect(r.result.value.success).toBe(true);
  });

  it("tool_search object classifies to the tool_search arm", () => {
    const r = convertToolUseResult(loadToolResult("tool_search"));
    expect(r.result.case).toBe("toolSearch");
    if (r.result.case !== "toolSearch") throw new Error("arm");
    expect(r.result.value.totalDeferredTools).toBe(43n);
  });

  it("an unidentifiable object routes to unclassified (capture, never guess)", () => {
    const r = convertToolUseResult(loadToolResult("unclassified-path_title_url"));
    expect(r.result.case).toBe("unclassified");
    if (r.result.case !== "unclassified") throw new Error("arm");
    expect(r.result.value["path"]).toBe("/tmp/coach_asset_mmap_plan.html");
  });
});

// ---------------------------------------------------------------------------
// Corpus-proven type corrections: the arms that could not classify while the
// proto modeled these fields as the wrong type (Struct-for-array, string-for-
// object, string-for-int, Struct-for-bool). Each asserts ONE corrected shape.
// ---------------------------------------------------------------------------

/** Read a ListValue field back as plain JSON for shape assertions. */
function listJson(lv: ListValue | undefined): JsonValue {
  expect(lv).toBeDefined();
  return toJson(ListValueSchema, lv!);
}

describe("corrected tool-result shapes", () => {
  it("task_list preserves the array verbatim (a Struct init silently emptied it)", () => {
    const r = convertToolUseResult({ tasks: [{ id: "1", status: "pending" }] });
    expect(r.result.case).toBe("taskList");
    if (r.result.case !== "taskList") throw new Error("arm");
    expect(listJson(r.result.value.tasks)).toEqual([{ id: "1", status: "pending" }]);
  });

  it("edit classifies and keeps structuredPatch hunks as a list", () => {
    const r = convertToolUseResult(loadToolResult("edit"));
    expect(r.result.case).toBe("edit");
    if (r.result.case !== "edit") throw new Error("arm");
    const hunks = listJson(r.result.value.structuredPatch) as JsonObject[];
    expect(hunks).toHaveLength(1);
    expect(hunks[0]!["oldStart"]).toBe(411);
  });

  it("write classifies with an EMPTY structuredPatch list still present", () => {
    const r = convertToolUseResult(loadToolResult("write"));
    expect(r.result.case).toBe("write");
    if (r.result.case !== "write") throw new Error("arm");
    expect(listJson(r.result.value.structuredPatch)).toEqual([]);
  });

  it("ask_user_question classifies its questions as typed Question elements", () => {
    const r = convertToolUseResult(loadToolResult("ask_user_question"));
    expect(r.result.case).toBe("askUserQuestion");
    if (r.result.case !== "askUserQuestion") throw new Error("arm");
    expect(r.result.value.questions).toHaveLength(1);
    expect(r.result.value.questions[0]!.header).toBe("Setup");
  });

  it("ask_user_question keeps the nested QuestionOption labels", () => {
    const r = convertToolUseResult(loadToolResult("ask_user_question"));
    if (r.result.case !== "askUserQuestion") throw new Error("arm");
    const opts = r.result.value.questions[0]!.options;
    expect(opts.map((o) => o.label)).toEqual([
      "New worktree off master (Recommended)",
      "Switch master checkout",
    ]);
  });

  it("web_search keeps its HETEROGENEOUS results list (objects and strings)", () => {
    const r = convertToolUseResult(loadToolResult("web_search"));
    expect(r.result.case).toBe("webSearch");
    if (r.result.case !== "webSearch") throw new Error("arm");
    const results = listJson(r.result.value.results) as JsonValue[];
    expect(typeof results[0]).toBe("object");
    expect(typeof results[1]).toBe("string");
  });

  it("task_update reads its statusChange as a typed TaskStatusChange", () => {
    const r = convertToolUseResult(loadToolResult("task_update"));
    expect(r.result.case).toBe("taskUpdate");
    if (r.result.case !== "taskUpdate") throw new Error("arm");
    expect(r.result.value.statusChange?.from).toBe("pending");
    expect(r.result.value.statusChange?.to).toBe("in_progress");
  });

  it("task_update leaves statusChange absent when the raw value is not an object", () => {
    const r = convertToolUseResult({ taskId: "1", statusChange: "pending->done" });
    expect(r.result.case).toBe("taskUpdate");
    if (r.result.case !== "taskUpdate") throw new Error("arm");
    expect(r.result.value.statusChange).toBeUndefined();
  });

  it("task_update reads updatedFields as a repeated string", () => {
    const r = convertToolUseResult(loadToolResult("task_update"));
    if (r.result.case !== "taskUpdate") throw new Error("arm");
    expect(r.result.value.updatedFields).toEqual(["status"]);
  });

  it("send_message reads its pin as a typed MessagePin", () => {
    const r = convertToolUseResult(loadToolResult("send_message"));
    expect(r.result.case).toBe("sendMessage");
    if (r.result.case !== "sendMessage") throw new Error("arm");
    expect(r.result.value.pin?.id).toBe("acd910f5fefb75908");
    expect(r.result.value.pin?.name).toBe("acd910f5fefb75908");
    expect(r.result.value.pin?.ref).toBe("2175c2");
  });

  it("send_message leaves pin absent when the raw value is not an object", () => {
    const r = convertToolUseResult({ pin: "2175c2", message: "hi" });
    expect(r.result.case).toBe("sendMessage");
    if (r.result.case !== "sendMessage") throw new Error("arm");
    expect(r.result.value.pin).toBeUndefined();
  });

  it("schedule_wakeup reads scheduledFor as an epoch-millis integer", () => {
    const r = convertToolUseResult(loadToolResult("schedule_wakeup"));
    expect(r.result.case).toBe("scheduleWakeup");
    if (r.result.case !== "scheduleWakeup") throw new Error("arm");
    expect(r.result.value.scheduledFor).toBe(1784408640000n);
  });

  it("a {message,success} object with no pin STAYS unclassified (send_message guard)", () => {
    const r = convertToolUseResult(loadToolResult("unclassified-message_success"));
    expect(r.result.case).toBe("unclassified");
  });
});

// ---------------------------------------------------------------------------
// Missing / unknown discriminators → UnparsedEvent (never a zero value).
// ---------------------------------------------------------------------------

describe("Anthropic API content blocks", () => {
  /** Convert one assistant content block and return its ContentBlock arm. */
  const blockOf = (block: Record<string, unknown>) => {
    const csm = vendor(convert({ type: "assistant", session_id: "s", message: { id: "m", model: "x", content: [block] } }));
    if (csm.msg.case !== "assistant") throw new Error("case");
    return csm.msg.value.message!.content[0]!.block;
  };

  it("redacted_thinking decodes its opaque blob to bytes", () => {
    // The blob must survive byte-for-byte to be replayable to the API.
    const arm = blockOf({ type: "redacted_thinking", data: Buffer.from("secret").toString("base64") });
    if (arm.case !== "redactedThinking") throw new Error(`case ${arm.case}`);
    expect(Buffer.from(arm.value.data).toString()).toBe("secret");
  });

  it("server_tool_use keeps its input struct", () => {
    const arm = blockOf({ type: "server_tool_use", id: "s1", name: "web_search", input: { query: "x" } });
    if (arm.case !== "serverToolUse") throw new Error(`case ${arm.case}`);
    expect(arm.value.input).toEqual({ query: "x" });
  });

  it("mcp_tool_use carries the server name that distinguishes it from tool_use", () => {
    const arm = blockOf({ type: "mcp_tool_use", id: "m1", name: "search", server_name: "srv" });
    if (arm.case !== "mcpToolUse") throw new Error(`case ${arm.case}`);
    expect(arm.value.serverName).toBe("srv");
  });

  it("mcp_tool_result preserves a heterogeneous content array", () => {
    const arm = blockOf({ type: "mcp_tool_result", tool_use_id: "t1", content: [{ type: "text", text: "a" }, "bare"] });
    if (arm.case !== "mcpToolResult") throw new Error(`case ${arm.case}`);
    expect(toJson(ListValueSchema, arm.value.content!)).toEqual([{ type: "text", text: "a" }, "bare"]);
  });

  it("web_search_tool_result routes an array to the results arm", () => {
    const arm = blockOf({ type: "web_search_tool_result", tool_use_id: "t1", content: [{ url: "u" }] });
    if (arm.case !== "webSearchToolResult") throw new Error(`case ${arm.case}`);
    expect(arm.value.content.case).toBe("results");
  });

  it("web_search_tool_result routes an error object to the error arm", () => {
    // A failed search must stay distinguishable from one that found nothing.
    const arm = blockOf({ type: "web_search_tool_result", tool_use_id: "t1", content: { error_code: "max_uses_exceeded" } });
    if (arm.case !== "webSearchToolResult") throw new Error(`case ${arm.case}`);
    if (arm.value.content.case !== "error") throw new Error(`content ${arm.value.content.case}`);
    expect(arm.value.content.value.errorCode).toBe("max_uses_exceeded");
  });

  it("web_fetch_tool_result shares the result-or-error union", () => {
    const arm = blockOf({ type: "web_fetch_tool_result", tool_use_id: "t1", content: { error_code: "unavailable" } });
    if (arm.case !== "webFetchToolResult") throw new Error(`case ${arm.case}`);
    expect(arm.value.content.case).toBe("error");
  });

  it("code_execution_tool_result shares the result-or-error union", () => {
    const arm = blockOf({ type: "code_execution_tool_result", tool_use_id: "t1", content: [{ stdout: "hi" }] });
    if (arm.case !== "codeExecutionToolResult") throw new Error(`case ${arm.case}`);
    expect(arm.value.content.case).toBe("results");
  });

  it("search_result carries its source", () => {
    const arm = blockOf({ type: "search_result", source: "docs", title: "t", content: [] });
    if (arm.case !== "searchResult") throw new Error(`case ${arm.case}`);
    expect(arm.value.source).toBe("docs");
  });

  it("container_upload carries the container's file id", () => {
    const arm = blockOf({ type: "container_upload", file_id: "f1" });
    if (arm.case !== "containerUpload") throw new Error(`case ${arm.case}`);
    expect(arm.value.fileId).toBe("f1");
  });
});

describe("SDK 0.3.220 stream families", () => {
  const armOf = (msg: Record<string, unknown>) => vendor(convert({ session_id: "s", ...msg })).msg;

  it("system/api_retry maps its retry counters", () => {
    const arm = armOf({ type: "system", subtype: "api_retry", attempt: 2, max_retries: 5, retry_delay_ms: 400, error_status: 529, error: "server_error" });
    if (arm.case !== "apiRetry") throw new Error(`case ${arm.case}`);
    expect(arm.value.attempt).toBe(2);
    expect(arm.value.retryDelayMs).toBe(400n);
  });

  it("system/api_retry distinguishes a null error_status from a zero one", () => {
    // null means "connection error, no HTTP response" — a different fact
    // from an HTTP 0, so the *_set companion must carry it.
    const arm = armOf({ type: "system", subtype: "api_retry", attempt: 1, error_status: null });
    if (arm.case !== "apiRetry") throw new Error(`case ${arm.case}`);
    expect(arm.value.errorStatusSet).toBe(false);
  });

  it("system/api_retry marks a present error_status as set", () => {
    const arm = armOf({ type: "system", subtype: "api_retry", attempt: 1, error_status: 500 });
    if (arm.case !== "apiRetry") throw new Error(`case ${arm.case}`);
    expect(arm.value.errorStatusSet).toBe(true);
  });

  it("system/control_request_progress leaves absent retry fields unset", () => {
    const arm = armOf({ type: "system", subtype: "control_request_progress", request_id: "r1", status: "started" });
    if (arm.case !== "controlRequestProgress") throw new Error(`case ${arm.case}`);
    expect(arm.value.attemptSet).toBe(false);
  });

  it("system/task_progress maps the nested usage block", () => {
    const arm = armOf({ type: "system", subtype: "task_progress", task_id: "a1", description: "d", usage: { total_tokens: 120, tool_uses: 3, duration_ms: 900 } });
    if (arm.case !== "taskProgress") throw new Error(`case ${arm.case}`);
    expect(arm.value.usage?.totalTokens).toBe(120n);
  });

  it("system/session_state_changed carries the requires_action state", () => {
    const arm = armOf({ type: "system", subtype: "session_state_changed", state: "requires_action" });
    if (arm.case !== "sessionStateChanged") throw new Error(`case ${arm.case}`);
    expect(arm.value.state).toBe("requires_action");
  });

  it("system/commands_changed maps each command including its aliases", () => {
    const arm = armOf({ type: "system", subtype: "commands_changed", commands: [{ name: "usage", description: "d", argumentHint: "", aliases: ["cost", "stats"] }] });
    if (arm.case !== "commandsChanged") throw new Error(`case ${arm.case}`);
    expect(arm.value.commands[0]?.aliases).toEqual(["cost", "stats"]);
  });

  it("system/files_persisted keeps successes and failures apart", () => {
    const arm = armOf({ type: "system", subtype: "files_persisted", files: [{ filename: "a", file_id: "f1" }], failed: [{ filename: "b", error: "nope" }], processed_at: "2026-07-25T00:00:00Z" });
    if (arm.case !== "filesPersisted") throw new Error(`case ${arm.case}`);
    expect(arm.value.files).toHaveLength(1);
    expect(arm.value.failed[0]?.error).toBe("nope");
  });

  it("system/memory_recall marks a lazy-loaded memory's content unset", () => {
    // A file-backed `select` entry omits content; the renderer reads `path`.
    const arm = armOf({ type: "system", subtype: "memory_recall", mode: "select", memories: [{ path: "/m.md", scope: "personal" }] });
    if (arm.case !== "memoryRecall") throw new Error(`case ${arm.case}`);
    expect(arm.value.memories[0]?.contentSet).toBe(false);
  });

  it("system/memory_recall marks an inlined memory's content set", () => {
    const arm = armOf({ type: "system", subtype: "memory_recall", mode: "synthesize", memories: [{ path: "<synthesis:/d>", scope: "team", content: "body" }] });
    if (arm.case !== "memoryRecall") throw new Error(`case ${arm.case}`);
    expect(arm.value.memories[0]?.contentSet).toBe(true);
  });

  it("system/permission_denied carries the decision reason the result tally lacks", () => {
    const arm = armOf({ type: "system", subtype: "permission_denied", tool_name: "Bash", tool_use_id: "t1", decision_reason: "deny rule", message: "blocked" });
    if (arm.case !== "permissionDenied") throw new Error(`case ${arm.case}`);
    expect(arm.value.decisionReason).toBe("deny rule");
  });

  it("system/mirror_error maps its camelCase nested key", () => {
    const arm = armOf({ type: "system", subtype: "mirror_error", error: "eperm", key: { projectKey: "p", sessionId: "s2", subpath: "x" } });
    if (arm.case !== "mirrorError") throw new Error(`case ${arm.case}`);
    expect(arm.value.key?.projectKey).toBe("p");
  });

  it("system/informational carries prevent_continuation", () => {
    const arm = armOf({ type: "system", subtype: "informational", content: "c", level: "warning", prevent_continuation: true });
    if (arm.case !== "informational") throw new Error(`case ${arm.case}`);
    expect(arm.value.preventContinuation).toBe(true);
  });

  it("system/hook_progress is distinct from the finished hook_response", () => {
    const arm = armOf({ type: "system", subtype: "hook_progress", hook_id: "h1", hook_name: "n", hook_event: "PreToolUse", stdout: "o", stderr: "", output: "" });
    if (arm.case !== "hookProgress") throw new Error(`case ${arm.case}`);
    expect(arm.value.hookId).toBe("h1");
  });

  it("system/plugin_install carries its lifecycle status", () => {
    const arm = armOf({ type: "system", subtype: "plugin_install", status: "failed", name: "p", error: "boom" });
    if (arm.case !== "pluginInstall") throw new Error(`case ${arm.case}`);
    expect(arm.value.status).toBe("failed");
  });

  it("system/worker_shutting_down carries its reason", () => {
    const arm = armOf({ type: "system", subtype: "worker_shutting_down", reason: "host_exit" });
    if (arm.case !== "workerShuttingDown") throw new Error(`case ${arm.case}`);
    expect(arm.value.reason).toBe("host_exit");
  });

  it("system/local_command_output carries its content", () => {
    const arm = armOf({ type: "system", subtype: "local_command_output", content: "out" });
    if (arm.case !== "localCommandOutput") throw new Error(`case ${arm.case}`);
    expect(arm.value.content).toBe("out");
  });

  it("system/elicitation_complete names the server and elicitation", () => {
    const arm = armOf({ type: "system", subtype: "elicitation_complete", mcp_server_name: "srv", elicitation_id: "e1" });
    if (arm.case !== "elicitationComplete") throw new Error(`case ${arm.case}`);
    expect(arm.value.elicitationId).toBe("e1");
  });

  it("system/model_refusal_fallback records the retracted message uuids", () => {
    const arm = armOf({ type: "system", subtype: "model_refusal_fallback", trigger: "refusal", direction: "retry", original_model: "a", fallback_model: "b", request_id: "r", content: "c", retracted_message_uuids: ["u1"] });
    if (arm.case !== "modelRefusalFallback") throw new Error(`case ${arm.case}`);
    expect(arm.value.retractedMessageUuids).toEqual(["u1"]);
  });

  it("system/model_refusal_no_fallback maps a null request_id to empty", () => {
    const arm = armOf({ type: "system", subtype: "model_refusal_no_fallback", original_model: "a", request_id: null, content: "c" });
    if (arm.case !== "modelRefusalNoFallback") throw new Error(`case ${arm.case}`);
    expect(arm.value.requestId).toBe("");
  });

  it("tool_use_summary names the tool uses it covers", () => {
    const arm = armOf({ type: "tool_use_summary", summary: "read some files", preceding_tool_use_ids: ["t1", "t2"] });
    if (arm.case !== "toolUseSummary") throw new Error(`case ${arm.case}`);
    expect(arm.value.precedingToolUseIds).toEqual(["t1", "t2"]);
  });

  it("prompt_suggestion carries the suggestion", () => {
    const arm = armOf({ type: "prompt_suggestion", suggestion: "try /help" });
    if (arm.case !== "promptSuggestion") throw new Error(`case ${arm.case}`);
    expect(arm.value.suggestion).toBe("try /help");
  });

  it("conversation_reset carries the new conversation id", () => {
    const arm = armOf({ type: "conversation_reset", new_conversation_id: "c2" });
    if (arm.case !== "conversationReset") throw new Error(`case ${arm.case}`);
    expect(arm.value.newConversationId).toBe("c2");
  });

  it("active_goal maps a set goal", () => {
    const arm = armOf({ type: "active_goal", value: { condition: "tests pass", iterations: 3, set_at: 10, tokens_at_start: 99 } });
    if (arm.case !== "activeGoal") throw new Error(`case ${arm.case}`);
    expect(arm.value.value?.condition).toBe("tests pass");
  });

  it("active_goal distinguishes a cleared goal from a zero-iteration one", () => {
    // `value: null` IS the clear signal, so value_set must carry it.
    const arm = armOf({ type: "active_goal", value: null });
    if (arm.case !== "activeGoal") throw new Error(`case ${arm.case}`);
    expect(arm.value.valueSet).toBe(false);
  });

  it("a new family is PERSISTENT, not ephemeral", () => {
    // None of these has a store-delivered final form to be replaced by, so
    // EPHEMERAL would mean LOST on reconnect rather than merely transient.
    const result = convert({ type: "system", subtype: "task_progress", session_id: "s", task_id: "a1", description: "d" });
    expect(result.vendor.class).toBe(EventClass.PERSISTENT);
  });
});

/**
 * The two halves of the §5.1 contract, which are NOT alternatives:
 * UnparsedEvent is for a KNOWN shape that failed conversion; the
 * UnknownRecord passthrough is for an unrecognized DISCRIMINATOR.
 */
describe("UnparsedEvent hard-error path (known shape, failed conversion)", () => {
  it("a message with no type is unparsed", () => {
    const result = convert({ foo: "bar" });
    expect(result.vendor.payload.case).toBe("unparsed");
  });

  it("a user message missing `message` is unparsed with the producer stamped", () => {
    // `user` IS a modeled type, so its shape is an expectation the record
    // violated — a hard error, never the passthrough arm.
    const result = convert({ type: "user", session_id: "s" });
    expect(result.vendor.payload.case).toBe("unparsed");
    if (result.vendor.payload.case !== "unparsed") throw new Error("case");
    expect(result.vendor.payload.value.producer).toBe("claude-shim");
  });

  it("a stream_event with an unusable delta arm is unparsed", () => {
    const result = convert({
      type: "stream_event",
      session_id: "s",
      event: { type: "content_block_delta", index: 0, delta: { type: "no_such_delta" } },
    });
    expect(result.vendor.payload.case).toBe("unparsed");
  });
});

describe("UnknownRecord passthrough (unrecognized discriminator)", () => {
  const unknownOf = (msg: unknown) => {
    const csm = vendor(convert(msg));
    if (csm.msg.case !== "unknown") throw new Error(`expected unknown arm, got ${csm.msg.case}`);
    return csm.msg.value;
  };

  it("an unknown top-level type lands on the passthrough arm", () => {
    const rec = unknownOf({ type: "totally_new_message", session_id: "s" });
    expect(rec.discriminator).toBe("totally_new_message");
  });

  it("an unknown top-level type is tagged as a TYPE discriminator", () => {
    const rec = unknownOf({ type: "totally_new_message", session_id: "s" });
    expect(rec.discriminatorField).toBe(DiscriminatorField.TYPE);
  });

  it("an unknown top-level type preserves the whole record verbatim", () => {
    const rec = unknownOf({ type: "totally_new_message", session_id: "s", payload: { a: 1 } });
    expect(rec.raw).toEqual({ type: "totally_new_message", session_id: "s", payload: { a: 1 } });
  });

  it("an unknown system subtype lands on the passthrough arm", () => {
    const rec = unknownOf({ type: "system", subtype: "brand_new_subtype", session_id: "s" });
    expect(rec.discriminator).toBe("brand_new_subtype");
  });

  it("an unknown system subtype is tagged as a SUBTYPE discriminator", () => {
    const rec = unknownOf({ type: "system", subtype: "brand_new_subtype", session_id: "s" });
    expect(rec.discriminatorField).toBe(DiscriminatorField.SUBTYPE);
  });

  it("an unknown system subtype records `system` as its parent type", () => {
    const rec = unknownOf({ type: "system", subtype: "brand_new_subtype", session_id: "s" });
    expect(rec.parentType).toBe("system");
  });

  it("a passthrough record does not also populate Event.extras", () => {
    // UnknownRecord.raw already holds every field; duplicating them into
    // extras would make the unknown-FIELD log lie about what is new.
    const result = convert({ type: "totally_new_message", session_id: "s", novel: 1 });
    expect(result.vendor.extras).toBeUndefined();
    expect(result.loggedExtras).toEqual([]);
  });

  it("an unknown content block is preserved instead of dropped", () => {
    const csm = vendor(convert({
      type: "assistant",
      session_id: "s",
      message: { id: "m", model: "x", content: [{ type: "no_such_block", data: "keep me" }] },
    }));
    if (csm.msg.case !== "assistant") throw new Error("case");
    const [block] = csm.msg.value.message!.content;
    if (block?.block.case !== "unknown") throw new Error(`expected unknown block, got ${block?.block.case}`);
    expect(block.block.value.raw).toEqual({ type: "no_such_block", data: "keep me" });
  });
});

// Silence unused-import lint for the dir constants documenting fixture roots.
void toolResultsDir;
