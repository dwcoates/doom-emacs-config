import { beforeEach, describe, expect, it } from "vitest";
import { readFileSync, readdirSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { toJson } from "@bufbuild/protobuf";
import type { JsonObject, JsonValue } from "@bufbuild/protobuf";
import { anyUnpack, ListValueSchema, type ListValue } from "@bufbuild/protobuf/wkt";
import { convert, convertToolUseResult } from "../src/proto/convert.js";
import { __resetExtrasSeen } from "../src/proto/extras.js";
import { EventClass, Plane, SessionSource } from "../src/uds/proto.js";
import {
  ApiKeySource,
  ClaudeStreamMessageSchema,
  type ClaudeStreamMessage,
} from "../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";

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

  it("non-tool-result user emits TurnStarted with a bounded preview", () => {
    const { lifecycle } = convert(loadStream("user"));
    expect(lifecycle).toHaveLength(1);
    if (lifecycle[0]!.payload.case !== "turnStarted") throw new Error("case");
    expect(lifecycle[0]!.payload.value.promptPreview).toBe("Reply with the single word ok and stop.");
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

  it("send_message classifies its pin as an object", () => {
    const r = convertToolUseResult(loadToolResult("send_message"));
    expect(r.result.case).toBe("sendMessage");
    if (r.result.case !== "sendMessage") throw new Error("arm");
    expect(r.result.value.pin).toEqual({ id: "acd910f5fefb75908", name: "acd910f5fefb75908", ref: "2175c2" });
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

describe("UnparsedEvent hard-error path", () => {
  it("a message with no type is unparsed", () => {
    const result = convert({ foo: "bar" });
    expect(result.vendor.payload.case).toBe("unparsed");
  });

  it("an unknown type is unparsed with the producer stamped", () => {
    const result = convert({ type: "totally_new_message", session_id: "s" });
    expect(result.vendor.payload.case).toBe("unparsed");
    if (result.vendor.payload.case !== "unparsed") throw new Error("case");
    expect(result.vendor.payload.value.producer).toBe("claude-shim");
  });

  it("an unknown system subtype is unparsed", () => {
    const result = convert({ type: "system", subtype: "brand_new_subtype", session_id: "s" });
    expect(result.vendor.payload.case).toBe("unparsed");
  });
});

// Silence unused-import lint for the dir constants documenting fixture roots.
void toolResultsDir;
