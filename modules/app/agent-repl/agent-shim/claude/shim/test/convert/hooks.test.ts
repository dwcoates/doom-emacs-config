/**
 * A HOOK FIRING, on the stream.
 *
 * Two facts carry the weight here. The vendor's hook EVENT has to resolve to
 * this contract's enum — a lookup that silently missed made every firing
 * UNSPECIFIED, which the proto declares malformed. And a hook that BLOCKED a
 * gated call has to be told apart from one that merely failed: only the first
 * produces text the model reads, and stderr is written by both.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import {
  convertHookProgress,
  convertHookResponse,
  convertHookStarted,
  createHookRegistry,
  hookBlockingText,
} from "../../src/convert/hooks.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { logRecordsDuring } from "../log-records.js";
import { activityOf, foldContext } from "./fold-harness.js";

/** A `hook_started` for one event. */
function started(hookEvent: string, hookId = "hook-1"): Extract<SdkMessage, { type: "system"; subtype: "hook_started" }> {
  return {
    type: "system",
    subtype: "hook_started",
    hook_id: hookId,
    hook_name: `${hookEvent}:one`,
    hook_event: hookEvent,
    uuid: "00000000-0000-0000-0000-000000000001",
    session_id: "session-1",
  };
}

/** A `hook_response` as the vendor spells one. */
function response(
  fields: Partial<Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>>,
): Extract<SdkMessage, { type: "system"; subtype: "hook_response" }> {
  return {
    type: "system",
    subtype: "hook_response",
    hook_id: "hook-1",
    hook_name: "PreToolUse:one",
    hook_event: "PreToolUse",
    output: "",
    stdout: "",
    stderr: "",
    exit_code: 0,
    outcome: "success",
    uuid: "00000000-0000-0000-0000-000000000002",
    session_id: "session-1",
    ...fields,
  };
}

/** A `hook_response` that FAILED without blocking: the outcome that is drawn. */
function failed(hookId = "hook-1"): Extract<SdkMessage, { type: "system"; subtype: "hook_response" }> {
  return response({ hook_id: hookId, outcome: "error", output: "", stderr: "boom", exit_code: 1 });
}

/** A `hook_progress` as the vendor spells one. */
function progress(): Extract<SdkMessage, { type: "system"; subtype: "hook_progress" }> {
  return {
    type: "system",
    subtype: "hook_progress",
    hook_id: "hook-1",
    hook_name: "PreToolUse:one",
    hook_event: "PreToolUse",
    stdout: "working",
    stderr: "",
    output: "",
    uuid: "00000000-0000-0000-0000-000000000003",
    session_id: "session-1",
  };
}

/** The hook arms one converted response carries, in order. */
function armsOf(message: Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>): (string | undefined)[] {
  const registry = createHookRegistry();
  const entries = convertHookResponse(message, foldContext(), registry);
  return entries.map((entry) => (activityOf(entry)?.item.value as conversationv1.AgentHook).result.case);
}

/** The event the start frame of one FAILED firing of `hookEvent` carries. */
function eventOf(hookEvent: string): conversationv1.AgentHookEvent | undefined {
  const registry = createHookRegistry();
  convertHookStarted(started(hookEvent), foldContext(), registry);
  const entries = convertHookResponse(failed(), foldContext(), registry);
  const hook = activityOf(entries[0])?.item.value as conversationv1.AgentHook;
  return (hook.result.value as conversationv1.AgentHookStart).event;
}

describe("the row a drawn hook firing owns", () => {
  // THE STREAM PLANE OWNS THE SERVED HOOK ROW (ruling 2026-09-04). The vendor
  // hands the two planes disjoint identity material — a `hook_id` here, a
  // `toolUseID` in the transcript attachment the sidecar reads, and differing
  // record uuids — so nothing can join them and only one plane may serve the
  // row. This key IS that decision, so it is asserted as a literal.
  it("is keyed by the vendor's own hook_id", () => {
    const registry = createHookRegistry();
    convertHookStarted(started("PreToolUse", "hook-abc"), foldContext(), registry);
    const entries = convertHookResponse(failed("hook-abc"), foldContext(), registry);
    expect(entries[1]?.upsertKey).toBe("activity:hook-abc");
  });

  it("files the start under the SAME key as the outcome, so a firing is one row and not two", () => {
    const registry = createHookRegistry();
    convertHookStarted(started("PreToolUse", "hook-abc"), foldContext(), registry);
    const entries = convertHookResponse(failed("hook-abc"), foldContext(), registry);
    expect(entries[0]?.upsertKey).toBe(entries[1]?.upsertKey);
  });

  it("keeps two firings of one hook on two rows", () => {
    const registry = createHookRegistry();
    convertHookStarted(started("PreToolUse", "hook-1"), foldContext(), registry);
    convertHookStarted(started("PreToolUse", "hook-2"), foldContext(), registry);
    const first = convertHookResponse(failed("hook-1"), foldContext(), registry);
    const second = convertHookResponse(failed("hook-2"), foldContext(), registry);
    expect(first[1]?.upsertKey).not.toBe(second[1]?.upsertKey);
  });
});

describe("the vendor's hook event, in this contract's enum", () => {
  // Every event the enum declares, spelled as the vendor spells it. A miss on
  // any of these used to answer UNSPECIFIED, which the proto calls malformed.
  const cases: readonly [string, conversationv1.AgentHookEvent][] = [
    ["PreToolUse", conversationv1.AgentHookEvent.PRE_TOOL_USE],
    ["PostToolUse", conversationv1.AgentHookEvent.POST_TOOL_USE],
    ["PostToolUseFailure", conversationv1.AgentHookEvent.POST_TOOL_USE_FAILURE],
    ["PostToolBatch", conversationv1.AgentHookEvent.POST_TOOL_BATCH],
    ["Notification", conversationv1.AgentHookEvent.NOTIFICATION],
    ["UserPromptSubmit", conversationv1.AgentHookEvent.USER_PROMPT_SUBMIT],
    ["UserPromptExpansion", conversationv1.AgentHookEvent.USER_PROMPT_EXPANSION],
    ["SessionStart", conversationv1.AgentHookEvent.SESSION_START],
    ["SessionEnd", conversationv1.AgentHookEvent.SESSION_END],
    ["Stop", conversationv1.AgentHookEvent.STOP],
    ["StopFailure", conversationv1.AgentHookEvent.STOP_FAILURE],
    ["SubagentStart", conversationv1.AgentHookEvent.SUBAGENT_START],
    ["SubagentStop", conversationv1.AgentHookEvent.SUBAGENT_STOP],
    ["PreCompact", conversationv1.AgentHookEvent.PRE_COMPACT],
    ["PostCompact", conversationv1.AgentHookEvent.POST_COMPACT],
    ["PermissionRequest", conversationv1.AgentHookEvent.PERMISSION_REQUEST],
    ["PermissionDenied", conversationv1.AgentHookEvent.PERMISSION_DENIED],
    ["Setup", conversationv1.AgentHookEvent.SETUP],
    ["TeammateIdle", conversationv1.AgentHookEvent.TEAMMATE_IDLE],
    ["TaskCreated", conversationv1.AgentHookEvent.TASK_CREATED],
    ["TaskCompleted", conversationv1.AgentHookEvent.TASK_COMPLETED],
    ["Elicitation", conversationv1.AgentHookEvent.ELICITATION],
    ["ElicitationResult", conversationv1.AgentHookEvent.ELICITATION_RESULT],
    ["ConfigChange", conversationv1.AgentHookEvent.CONFIG_CHANGE],
  ];

  for (const [literal, expected] of cases) {
    it(`resolves ${literal} to its own enum value`, () => {
      expect(eventOf(literal)).toBe(expected);
    });
  }

  it("answers UNSPECIFIED for an event this contract does not spell", () => {
    expect(eventOf("SomethingTheVendorAddedLater")).toBe(conversationv1.AgentHookEvent.UNSPECIFIED);
  });
});

describe("how a hook firing went", () => {
  it("is BLOCKING when the hook produced text the gated call is answered with", () => {
    expect(
      armsOf(
        response({
          outcome: "error",
          output: "the suite failed after the edit",
          stderr: "the suite failed after the edit",
          exit_code: 2,
        }),
      ),
    ).toEqual(["blockingError"]);
  });

  it("is NON-BLOCKING when the hook only failed, whatever it wrote on stderr", () => {
    // A SessionStart hook gates nothing; its stderr is a crash report, not a
    // refusal the model ever reads.
    expect(
      armsOf(
        response({
          hook_event: "SessionStart",
          outcome: "error",
          output: "",
          stderr: "Failed to run: no interpreter on PATH.",
          exit_code: 1,
        }),
      ),
    ).toEqual(["nonBlockingError"]);
  });
});

/**
 * NO HOOK RECORD THAT DRAWS NOTHING IS STORED (owner ruling 2026-10-06): a
 * start, a success, a cancellation and a progress report produce no entry, and
 * a drawn outcome brings its start with it.
 */
describe("the hook records that are not stored", () => {
  it("stores nothing for a hook's start", () => {
    const entries = convertHookStarted(started("SessionStart"), foldContext(), createHookRegistry());

    expect(entries).toEqual([]);
  });

  it("stores nothing for a hook that succeeded", () => {
    expect(armsOf(response({ outcome: "success", stdout: "{}\n" }))).toEqual([]);
  });

  it("stores nothing for a hook that was cancelled", () => {
    expect(armsOf(response({ outcome: "cancelled" }))).toEqual([]);
  });

  it("stores nothing for a hook's progress report", () => {
    expect(convertHookProgress(progress(), createHookRegistry())).toEqual([]);
  });

  it("records each dropped record at DEBUG with the query's running count", () => {
    // Arrange
    const registry = createHookRegistry();
    convertHookStarted(started("SessionStart"), foldContext(), registry);

    // Act
    const records = logRecordsDuring(() => convertHookResponse(response({ outcome: "success" }), foldContext(), registry));

    // Assert
    const dropped = records.find((record) => record.message === "a hook record that draws nothing is not stored");
    expect([dropped?.level, dropped?.context.hook_record, dropped?.context.dropped_total]).toEqual(["debug", "succeeded", 2]);
  });

  it("writes the start of a FAILED firing beside its outcome, start first", () => {
    const registry = createHookRegistry();
    convertHookStarted(started("SessionStart"), foldContext(), registry);

    const entries = convertHookResponse(failed(), foldContext(), registry);

    expect(entries.map((entry) => (activityOf(entry)?.item.value as conversationv1.AgentHook).result.case)).toEqual([
      "start",
      "nonBlockingError",
    ]);
  });

  it("names the started hook on the start it writes beside a failure", () => {
    const registry = createHookRegistry();
    convertHookStarted(started("SessionStart"), foldContext(), registry);

    const entries = convertHookResponse(failed(), foldContext(), registry);

    const hook = activityOf(entries[0])?.item.value as conversationv1.AgentHook;
    expect((hook.result.value as conversationv1.AgentHookStart).hookName).toBe("SessionStart:one");
  });

  it("refuses a start with no firing id as a converter defect", () => {
    expect(() => convertHookStarted(started("PreToolUse", ""), foldContext(), createHookRegistry())).toThrow();
  });

  it("refuses a response with no firing id as a converter defect", () => {
    expect(() => convertHookResponse(response({ hook_id: "" }), foldContext(), createHookRegistry())).toThrow();
  });

  it("writes only the outcome of a failed firing whose start this shim never saw", () => {
    expect(armsOf(failed())).toEqual(["nonBlockingError"]);
  });
});

describe("the per-query summary of dropped hook records", () => {
  it("is one INFO record counting each kind", () => {
    // Arrange
    const registry = createHookRegistry();
    convertHookStarted(started("SessionStart"), foldContext(), registry);
    convertHookResponse(response({ outcome: "success" }), foldContext(), registry);
    convertHookProgress(progress(), registry);

    // Act
    const records = logRecordsDuring(() => registry.reportDropped("the query was replaced"));

    // Assert
    expect(records.map((record) => [record.level, record.context.dropped_total, record.context.dropped_by_kind])).toEqual([
      ["info", 3, { start: 1, succeeded: 1, progress: 1 }],
    ]);
  });

  it("writes nothing when the query dropped nothing", () => {
    const records = logRecordsDuring(() => createHookRegistry().reportDropped("the query was replaced"));

    expect(records).toEqual([]);
  });

  it("starts counting afresh after a summary", () => {
    // Arrange
    const registry = createHookRegistry();
    convertHookStarted(started("SessionStart"), foldContext(), registry);
    registry.reportDropped("the query was replaced");

    // Act
    const total = registry.noteDropped("succeeded");

    // Assert
    expect(total).toBe(1);
  });
});

/**
 * The in-flight hook table is BOUNDED. A vendor that fires a hook and never
 * answers it must not grow this process's memory without limit, so the oldest
 * unanswered firing is forgotten rather than the table growing.
 */
describe("the registry of hook firings in flight", () => {
  /** One remembered firing, distinguished only by its id. */
  function pending(hookId: string) {
    return {
      hookId,
      activityId: create(conversationv1.AgentActivityIdSchema, { value: hookId }),
      startUuid: "00000000-0000-0000-0000-000000000001",
      hookName: "PreToolUse:one",
      event: conversationv1.AgentHookEvent.PRE_TOOL_USE,
      startedAtMs: 1,
    };
  }

  it("forgets the OLDEST unanswered firing once the table is full", () => {
    // Arrange: 128 is the capacity, so 129 firings evict exactly the first.
    const registry = createHookRegistry();

    // Act
    for (let index = 0; index <= 128; index += 1) registry.remember(pending(`hook-${index}`));

    // Assert
    expect(registry.take("hook-0")).toBeUndefined();
  });

  it("still answers the newest firing after an eviction", () => {
    // Arrange
    const registry = createHookRegistry();

    // Act
    for (let index = 0; index <= 128; index += 1) registry.remember(pending(`hook-${index}`));

    // Assert
    expect(registry.take("hook-128")?.hookId).toBe("hook-128");
  });
});

describe("the single reading of whether a hook blocked", () => {
  // THE CONVERTER AND THE ENGINE'S START GATE ASK THE SAME QUESTION. Drawing a
  // hook and refusing a start on one are two readings of one fact, so the fact
  // has one home; these pin what that home answers.

  it("names the blocking text of a hook that answered the gated action", () => {
    const message = response({ outcome: "error", output: "no.", stderr: "boom" });

    expect(hookBlockingText(message)).toBe("no.");
  });

  it("blocks nothing when a failing hook answered with no text at all", () => {
    const message = response({ outcome: "error", output: "", stderr: "command not found" });

    expect(hookBlockingText(message)).toBeUndefined();
  });

  it("blocks nothing when a SUCCEEDING hook printed text", () => {
    const message = response({ outcome: "success", output: "extra context", stdout: "hi" });

    expect(hookBlockingText(message)).toBeUndefined();
  });
});
