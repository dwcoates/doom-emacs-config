/**
 * THE ONE PERMISSION SHAPE THE FOLD PRODUCES.
 *
 * The engine owns every ask that was actually PUT to someone — it holds the
 * `canUseTool` callback — so what is left here is the vendor's own
 * `permission_denied`: a call refused without asking anyone. These tests pin the
 * two things a reader depends on. The DIVISION (the fold produces nothing when
 * the engine's gate holds the ask, so one unit never has two producers), and the
 * ARM (a classifier that could not decide is `undecidable`, which retrying may
 * resolve; a rule or a mode is a judgement and stays `policy`).
 */
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { convertPermissionDenied } from "../../src/convert/permission.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import { foldContext, type ContextOverrides } from "./fold-harness.js";

type DeniedMessage = Extract<SdkMessage, { type: "system"; subtype: "permission_denied" }>;

/** The vendor's `permission_denied` record. */
function denied(fields: Partial<DeniedMessage> = {}): DeniedMessage {
  return {
    type: "system",
    subtype: "permission_denied",
    tool_name: "Bash",
    tool_use_id: "toolu_1",
    message: "this workspace forbids it",
    uuid: "uuid-denied",
    session_id: "session-1",
    ...fields,
  } as DeniedMessage;
}

/** What one denial converts to. */
function convert(
  fields: Partial<DeniedMessage> = {},
  overrides: ContextOverrides = {},
): readonly PersistEntry[] {
  return convertPermissionDenied(denied(fields), foldContext(overrides));
}

/** The permission the row's update carries. */
function permissionOf(entry: PersistEntry | undefined): conversationv1.AgentPermission {
  const frame = entry?.item.kind === "frame" ? entry.item.frame : undefined;
  const update = (frame?.result.value as conversationv1.AgentUpdate).update;
  return update.value as conversationv1.AgentPermission;
}

/** The denial arm the permission settled on. */
function denialOf(entry: PersistEntry | undefined): conversationv1.AgentPermissionDenied {
  const success = permissionOf(entry).result.value as conversationv1.AgentPermissionSuccess;
  return success.decision.value as conversationv1.AgentPermissionDenied;
}

describe("a denial that names no gated call", () => {
  it("identifies no unit and produces nothing, rather than a row keyed on nothing", () => {
    expect(convert({ tool_use_id: "" })).toEqual([]);
  });
});

describe("the division with the engine's gate", () => {
  it("produces nothing while the engine holds an open PERMISSION ask for the call", () => {
    // Two producers for one unit would disagree whenever they disagreed.
    expect(convert({}, { pendingAsk: () => ({ kind: "permission" }) })).toEqual([]);
  });

  it("produces nothing while the engine holds an open QUESTION for the call", () => {
    expect(convert({}, { pendingAsk: () => ({ kind: "question" }) })).toEqual([]);
  });

  it("produces the denial when the engine holds no ask at all", () => {
    expect(convert()).toHaveLength(1);
  });
});

describe("a denial IS AN ANSWER, not a failure", () => {
  it("settles the gate's success arm", () => {
    expect(permissionOf(convert()[0]).result.case).toBe("success");
  });

  it("joins the consent to the call it gated, so a consumer can place it", () => {
    expect(permissionOf(convert()[0]).gatedCall?.value).toBe("toolu_1");
  });
});

describe("which denial arm the vendor's decider selects", () => {
  it("draws a RULE denial as policy, which is a judgement and not retryable", () => {
    const by = denialOf(convert({ decision_reason_type: "rule" })[0]).by;

    expect(by.case).toBe("policy");
  });

  it("draws a MODE denial as policy too", () => {
    expect(denialOf(convert({ decision_reason_type: "mode" })[0]).by.case).toBe("policy");
  });

  it("draws a denial with no stated decider as policy", () => {
    expect(denialOf(convert()[0]).by.case).toBe("policy");
  });

  it("draws the CLASSIFIER as undecidable: nobody refused is not policy refused", () => {
    const by = denialOf(convert({ decision_reason_type: "classifier" })[0]).by;

    expect(by.case).toBe("undecidable");
  });

  it("carries the vendor's sentence as the undecidable arm's detail", () => {
    const by = denialOf(
      convert({ decision_reason_type: "classifier", decision_reason: "could not classify" })[0],
    ).by;

    expect((by.value as conversationv1.AgentPermissionDeniedForWantOfDecider).detail).toBe(
      "could not classify",
    );
  });

  it("carries no detail on the undecidable arm when the vendor stated no reason", () => {
    const by = denialOf(convert({ decision_reason_type: "classifier" })[0]).by;

    expect((by.value as conversationv1.AgentPermissionDeniedForWantOfDecider).detail).toBeUndefined();
  });

  it("carries the rejection message the model was shown on the policy arm", () => {
    const by = denialOf(convert({ decision_reason_type: "rule" })[0]).by;

    expect((by.value as conversationv1.AgentPermissionDeniedByPolicy).message).toBe(
      "this workspace forbids it",
    );
  });
});

describe("whose book the denial lands in", () => {
  it("books a main-agent denial against the main agent", () => {
    expect(convert()[0]?.agentId?.value).toBe("main-agent");
  });

  it("books a denial inside a SUBAGENT against the subagent the record names", () => {
    // The vendor's own record is the one place the stream plane states an agent id.
    expect(convert({ agent_id: "sub-7" })[0]?.agentId?.value).toBe("sub-7");
  });

  it("falls back to the main agent when the record names an EMPTY agent id", () => {
    expect(convert({ agent_id: "" })[0]?.agentId?.value).toBe("main-agent");
  });
});
