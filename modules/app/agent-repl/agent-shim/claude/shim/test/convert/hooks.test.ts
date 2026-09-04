/**
 * A HOOK FIRING, on the stream.
 *
 * Two facts carry the weight here. The vendor's hook EVENT has to resolve to
 * this contract's enum — a lookup that silently missed made every firing
 * UNSPECIFIED, which the proto declares malformed. And a hook that BLOCKED a
 * gated call has to be told apart from one that merely failed: only the first
 * produces text the model reads, and stderr is written by both.
 */
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import { convertHookResponse, convertHookStarted, createHookRegistry } from "../../src/convert/hooks.js";
import type { SdkMessage } from "../../src/sdk/types.js";
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
  } as Extract<SdkMessage, { type: "system"; subtype: "hook_started" }>;
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
  } as Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>;
}

/** The hook arm one converted response carries. */
function armOf(message: Extract<SdkMessage, { type: "system"; subtype: "hook_response" }>): string | undefined {
  const registry = createHookRegistry();
  const entries = convertHookResponse(message, foldContext(), registry);
  const item = activityOf(entries[0])?.item;
  expect(item?.case).toBe("hook");
  return (item?.value as conversationv1.AgentHook).result.case;
}

/** The event one converted `hook_started` carries. */
function eventOf(hookEvent: string): conversationv1.AgentHookEvent | undefined {
  const registry = createHookRegistry();
  const entries = convertHookStarted(started(hookEvent), foldContext(), registry);
  const item = activityOf(entries[0])?.item;
  const hook = item?.value as conversationv1.AgentHook;
  return (hook.result.value as conversationv1.AgentHookStart).event;
}

describe("the row a hook firing owns", () => {
  // THE STREAM PLANE OWNS THE SERVED HOOK ROW (ruling 2026-09-04). The vendor
  // hands the two planes disjoint identity material — a `hook_id` here, a
  // `toolUseID` in the transcript attachment the sidecar reads, and differing
  // record uuids — so nothing can join them and only one plane may serve the
  // row. This key IS that decision, so it is asserted as a literal.
  it("is keyed by the vendor's own hook_id, on the START", () => {
    const registry = createHookRegistry();
    const entries = convertHookStarted(started("PreToolUse", "hook-abc"), foldContext(), registry);
    expect(entries[0]?.upsertKey).toBe("activity:hook-abc");
  });

  it("is the SAME key on the response, so a firing is one row and not two", () => {
    const registry = createHookRegistry();
    const start = convertHookStarted(started("PreToolUse", "hook-abc"), foldContext(), registry);
    const end = convertHookResponse(response({ hook_id: "hook-abc" }), foldContext(), registry);
    expect(end[0]?.upsertKey).toBe("activity:hook-abc");
    expect(end[0]?.upsertKey).toBe(start[0]?.upsertKey);
  });

  it("keeps two firings of one hook on two rows", () => {
    const registry = createHookRegistry();
    const first = convertHookStarted(started("PreToolUse", "hook-1"), foldContext(), registry);
    const second = convertHookStarted(started("PreToolUse", "hook-2"), foldContext(), registry);
    expect(first[0]?.upsertKey).not.toBe(second[0]?.upsertKey);
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
  it("is SUCCEEDED when the vendor reports success", () => {
    expect(armOf(response({ outcome: "success", stdout: "{}\n" }))).toBe("succeeded");
  });

  it("is BLOCKING when the hook produced text the gated call is answered with", () => {
    expect(
      armOf(
        response({
          outcome: "error",
          output: "the suite failed after the edit",
          stderr: "the suite failed after the edit",
          exit_code: 2,
        }),
      ),
    ).toBe("blockingError");
  });

  it("is NON-BLOCKING when the hook only failed, whatever it wrote on stderr", () => {
    // A SessionStart hook gates nothing; its stderr is a crash report, not a
    // refusal the model ever reads.
    expect(
      armOf(
        response({
          hook_event: "SessionStart",
          outcome: "error",
          output: "",
          stderr: "Failed to run: no interpreter on PATH.",
          exit_code: 1,
        }),
      ),
    ).toBe("nonBlockingError");
  });

  it("is CANCELLED when the firing never finished", () => {
    expect(armOf(response({ outcome: "cancelled" }))).toBe("cancelled");
  });
});
