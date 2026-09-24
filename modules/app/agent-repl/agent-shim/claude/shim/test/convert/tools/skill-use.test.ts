/**
 * The skill invocation. Its own return is a bare acknowledgement, so the
 * assertion that matters most is the NEGATIVE one: a successful result settles
 * nothing, because `AgentSkillUseSuccess.document` is not optional and the
 * acknowledgement carries no document.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { skillDocumentSettle, skillUseConverter } from "../../../src/convert/tools/skill-use.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { toolUseResult } from "./corpus.js";

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_01TQ2pMtt7kYZtX8cvdeNDED",
    toolName: "Skill",
    input,
    startedAtMs: 2_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "caller" }),
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 7_000 };
}

function armOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentSkillUse["result"] {
  expect(item?.case).toBe("skillUse");
  return (item?.value as conversationv1.AgentSkillUse).result;
}

describe("skillUseConverter.start", () => {
  it("names the skill from the argument the corpus actually spells", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.start(call({ skill: "check-cicd", args: "--branch x" })));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseStart).skill?.name).toBe("check-cicd");
  });

  it("carries the invocation's arguments", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.start(call({ skill: "check-cicd", args: "--branch x" })));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseStart).args).toBe("--branch x");
  });

  it("leaves args UNSET for a bare invocation, so an empty argument stays distinguishable", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.start(call({ skill: "check-cicd" })));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseStart).args).toBeUndefined();
  });

  it("keeps an empty argument as the empty string it was", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.start(call({ skill: "check-cicd", args: "" })));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseStart).args).toBe("");
  });

  it("stamps the invocation instant", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.start(call({ skill: "s" })));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseStart).startedAt?.atMs).toBe(2_000n);
  });

  it("produces NO frame when the invocation names no skill, since the unit IS the skill", () => {
    // Arrange, Act.
    const item = skillUseConverter.start(call({ args: "--branch x" }));

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("skillUseConverter.settle", () => {
  it("does NOT settle on the corpus acknowledgement, which carries no document", () => {
    // Arrange, Act, Assert.
    expect(
      skillUseConverter.settle(call({ skill: "debug-logs" }), outcome(toolUseResult("skill"))),
    ).toBeUndefined();
  });

  it("settles a vendor-stated error as the failure arm, since nothing further will arrive", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.settle(call({ skill: "nope" }), outcome(undefined, true)));

    // Assert.
    expect(arm.case).toBe("failure");
  });

  it("restates the skill on the failure arm, so the settled frame stands alone", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.settle(call({ skill: "nope" }), outcome(undefined, true)));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseFailure).skill?.name).toBe("nope");
  });

  it("produces NO failure frame for an invocation that named no skill, which had no start either", () => {
    // Arrange, Act, Assert.
    expect(skillUseConverter.settle(call({}), outcome(undefined, true))).toBeUndefined();
  });
});

describe("skillDocumentSettle", () => {
  it("settles the invocation on the document that landed", () => {
    // Arrange, Act.
    const arm = armOf(skillDocumentSettle(call({ skill: "debug-logs" }), "# Debug logs", undefined, 7_000));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseSuccess).document?.markdown).toBe("# Debug logs");
  });

  it("restates the skill on the settled frame, so it describes itself", () => {
    // Arrange, Act.
    const arm = armOf(skillDocumentSettle(call({ skill: "debug-logs" }), "doc", undefined, 7_000));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseSuccess).skill?.name).toBe("debug-logs");
  });

  it("leaves allowed_tools UNSET when the skill declared no allowances", () => {
    // Arrange, Act.
    const arm = armOf(skillDocumentSettle(call({ skill: "s" }), "doc", undefined, 7_000));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseSuccess).allowedTools).toBeUndefined();
  });

  it("carries a declared-but-empty allowance set, which is not the same as none", () => {
    // Arrange, Act.
    const arm = armOf(skillDocumentSettle(call({ skill: "s" }), "doc", [], 7_000));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseSuccess).allowedTools?.toolNames).toEqual([]);
  });

  it("carries the tools the skill permits, by bare name", () => {
    // Arrange, Act.
    const arm = armOf(skillDocumentSettle(call({ skill: "s" }), "doc", ["Bash", "Read"], 7_000));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseSuccess).allowedTools?.toolNames).toEqual([
      "Bash",
      "Read",
    ]);
  });

  it("stamps the settle instant", () => {
    // Arrange, Act.
    const arm = armOf(skillDocumentSettle(call({ skill: "s" }), "doc", undefined, 7_000));

    // Assert.
    expect((arm.value as conversationv1.AgentSkillUseSuccess).settledAt?.atMs).toBe(7_000n);
  });

  it("produces NO frame when the call named no skill", () => {
    // Arrange, Act.
    const item = skillDocumentSettle(call({}), "doc", undefined, 7_000);

    // Assert.
    expect(item?.case).toBeUndefined();
  });
});

describe("skillUseConverter.retain", () => {
  it("carries the allowances the acknowledgement declared onto the re-remembered call", () => {
    // Arrange.
    const pending = call({ skill: "s" });

    // Act.
    const retained = skillUseConverter.retain!(
      pending,
      outcome({ success: true, commandName: "s", allowedTools: ["Bash(x:*)", "Read(/tmp/y)"] }),
    );

    // Assert.
    expect(retained.retainedAllowedTools).toEqual(["Bash(x:*)", "Read(/tmp/y)"]);
  });

  it("carries a declared-but-EMPTY allowance set, which is not the same as none", () => {
    // Arrange, Act.
    const retained = skillUseConverter.retain!(call({ skill: "s" }), outcome({ allowedTools: [] }));

    // Assert.
    expect(retained.retainedAllowedTools).toEqual([]);
  });

  it("leaves the allowances UNSET when the acknowledgement declared none", () => {
    // Arrange, Act.
    const retained = skillUseConverter.retain!(call({ skill: "s" }), outcome({ success: true }));

    // Assert.
    expect(retained.retainedAllowedTools).toBeUndefined();
  });

  it("reads no declared set from a NON-ARRAY allowedTools", () => {
    // Arrange, Act.
    const retained = skillUseConverter.retain!(call({ skill: "s" }), outcome({ allowedTools: "Bash" }));

    // Assert.
    expect(retained.retainedAllowedTools).toBeUndefined();
  });

  it("keeps only the allowances that are tool NAMES", () => {
    // Arrange, Act.
    const retained = skillUseConverter.retain!(call({ skill: "s" }), outcome({ allowedTools: ["Bash", 7] }));

    // Assert.
    expect(retained.retainedAllowedTools).toEqual(["Bash"]);
  });
});

describe("skillUseConverter.progress", () => {
  it("relays the vendor's liveness beat as the unit's progress arm", () => {
    // Arrange, Act.
    const arm = armOf(skillUseConverter.progress!(toolProgress(5_000)));

    // Assert.
    expect(arm).toEqual({ case: "progress", value: toolProgress(5_000) });
  });

  it("declares a progress arm, because AgentSkillUse carries the vendor's beat", () => {
    // Arrange, Act, Assert.
    expect(skillUseConverter.carriesProgress).toBe(true);
  });
});
