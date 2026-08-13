/**
 * `frontend.v1.AgentToolOutcome` — a tool call's TYPED OUTCOME, decoded.
 *
 * The message used to carry `data.v1.ToolUseResult`, a vendor union of five
 * shapes the frontend destructured into two facts. conversation.v1 models
 * those two facts directly, so what is decoded here is the DETACHMENT'S own
 * vocabulary: which call it belongs to, and whether work started or ended.
 *
 * Everything is validated STRICTLY, as a frontend.v1-owned message must be:
 * an unknown field, an empty or multiple oneof, and a missing load-bearing
 * field all throw.
 */
import { describe, expect, it } from "vitest";

import { decodeToolOutcome, unwrapAgentEmission } from "../src/agent-emission.js";

describe("decodeToolOutcome — the correlation identity", () => {
  it("carries the tool_use id the outcome states", () => {
    // Arrange / Act
    const got = decodeToolOutcome({ toolUseId: "t1" }, "o");
    // Assert
    expect(got.toolUseId).toBe("t1");
  });

  it("rejects an outcome that names no call", () => {
    // Arrange / Act / Assert — the outcome has no correlation id of its own,
    // so one without this attaches to nothing.
    expect(() => decodeToolOutcome({}, "o")).toThrow(/missing required `toolUseId`/);
  });

  it("rejects an unrecognized field loudly", () => {
    // Arrange / Act / Assert — `structured`, the retired vendor union, included.
    expect(() => decodeToolOutcome({ toolUseId: "t1", structured: {} }, "o")).toThrow(
      /unrecognized field/,
    );
  });
});

describe("decodeToolOutcome — the detachment oneof", () => {
  it("reads an ABSENT oneof as a call that detached nothing", () => {
    // Arrange / Act
    const got = decodeToolOutcome({ toolUseId: "t1" }, "o");
    // Assert
    expect(got.detachment).toBeUndefined();
  });

  it("rejects both arms at once", () => {
    // Arrange / Act / Assert — a launch and an ending are different facts.
    expect(() =>
      decodeToolOutcome(
        { toolUseId: "t1", started: { kind: { agent: {} } }, ended: { cancelled: {} } },
        "o",
      ),
    ).toThrow(/sets both `started` and `ended`/);
  });

  it("carries the started detachment's origin call and label", () => {
    // Arrange / Act
    const got = decodeToolOutcome(
      {
        toolUseId: "t1",
        started: { originToolCallId: "t1", label: "sweep", kind: { shell: {} } },
      },
      "o",
    );
    // Assert
    expect(got.detachment).toEqual({
      case: "started",
      value: { originToolCallId: "t1", label: "sweep", kind: { case: "shell" } },
    });
  });

  it("rejects a started detachment that states no kind", () => {
    // Arrange / Act / Assert — the kind decides how the output is read.
    expect(() =>
      decodeToolOutcome({ toolUseId: "t1", started: { label: "x" } }, "o"),
    ).toThrow(/requires `kind`/);
  });
});

describe("decodeToolOutcome — the six kind arms", () => {
  const cases: ReadonlyArray<{ name: string; wire: unknown; want: unknown }> = [
    { name: "a subagent", wire: { agent: {} }, want: { case: "agent" } },
    { name: "a background shell", wire: { shell: {} }, want: { case: "shell" } },
    { name: "a workflow", wire: { workflow: {} }, want: { case: "workflow" } },
    { name: "a merge run", wire: { merge: {} }, want: { case: "merge" } },
    {
      name: "a skill, with its name and args",
      wire: { skill: { skillName: "standup", args: "--channel x" } },
      want: { case: "skill", skillName: "standup", args: "--channel x" },
    },
    {
      name: "work the daemon could not classify, named by its tool",
      wire: { unclassified: { toolName: "Weird" } },
      want: { case: "unclassified", toolName: "Weird" },
    },
  ];

  for (const testCase of cases) {
    it(`reads ${testCase.name}`, () => {
      // Arrange / Act
      const got = decodeToolOutcome(
        { toolUseId: "t1", started: { kind: testCase.wire } },
        "o",
      );
      // Assert
      const started = got.detachment;
      expect(started?.case === "started" && started.value.kind).toEqual(testCase.want);
    });
  }

  it("rejects a kind with no arm set", () => {
    // Arrange / Act / Assert
    expect(() => decodeToolOutcome({ toolUseId: "t1", started: { kind: {} } }, "o")).toThrow(
      /carries no arm/,
    );
  });

  it("rejects a kind that sets two arms", () => {
    // Arrange / Act / Assert
    expect(() =>
      decodeToolOutcome({ toolUseId: "t1", started: { kind: { agent: {}, shell: {} } } }, "o"),
    ).toThrow(/sets multiple arms/);
  });

  it("rejects an unrecognized kind arm rather than reading it as unclassified", () => {
    // Arrange / Act / Assert — `unclassified` is the daemon STATING it could
    // not tell, which is a different assertion from this end not recognizing
    // an arm the daemon does know.
    expect(() =>
      decodeToolOutcome({ toolUseId: "t1", started: { kind: { quantum: {} } } }, "o"),
    ).toThrow(/unrecognized field/);
  });
});

describe("decodeToolOutcome — the four ending arms", () => {
  const cases: ReadonlyArray<{ name: string; wire: unknown; want: unknown }> = [
    {
      name: "succeeded, with what it reported on the way out",
      wire: { succeeded: { summary: "3 files" } },
      want: { case: "succeeded", summary: "3 files" },
    },
    {
      name: "failed, with what went wrong",
      wire: { failed: { summary: "exit 1" } },
      want: { case: "failed", summary: "exit 1" },
    },
    { name: "cancelled, which carries nothing", wire: { cancelled: {} }, want: { case: "cancelled" } },
    {
      name: "lost, with how we concluded it",
      wire: { lost: { inference: "pid vanished" } },
      want: { case: "lost", inference: "pid vanished" },
    },
  ];

  for (const testCase of cases) {
    it(`reads ${testCase.name}`, () => {
      // Arrange / Act
      const got = decodeToolOutcome({ toolUseId: "t1", ended: testCase.wire }, "o");
      // Assert
      const ended = got.detachment;
      expect(ended?.case === "ended" && ended.value).toEqual(testCase.want);
    });
  }

  it("rejects an ending with no arm set", () => {
    // Arrange / Act / Assert
    expect(() => decodeToolOutcome({ toolUseId: "t1", ended: {} }, "o")).toThrow(/carries no arm/);
  });

  it("rejects an unrecognized ending arm", () => {
    // Arrange / Act / Assert
    expect(() => decodeToolOutcome({ toolUseId: "t1", ended: { exploded: {} } }, "o")).toThrow(
      /unrecognized field/,
    );
  });
});

describe("unwrapAgentEmission — the toolOutcome arm", () => {
  it("decodes the outcome onto the unwrapped emission", () => {
    // Arrange / Act
    const got = unwrapAgentEmission(
      { toolOutcome: { toolUseId: "t1", ended: { cancelled: {} } } },
      "Message.agent",
    );
    // Assert
    expect(got.toolOutcome?.detachment?.case).toBe("ended");
  });

  it("carries the detachment VERDICT through beside it", () => {
    // Arrange / Act — the verdict rides the envelope, one level above the
    // outcome, and names the message the work IS.
    const got = unwrapAgentEmission(
      { toolOutcome: { toolUseId: "t1", spawnedMessageId: "m9" } },
      "Message.agent",
    );
    // Assert
    expect(got.spawnedMessageId).toBe("m9");
  });

  it("rejects the retired `structured` vendor union rather than adopting it", () => {
    // Arrange / Act / Assert — no dual-decode: the old shape does not exist.
    expect(() =>
      unwrapAgentEmission(
        { toolOutcome: { toolUseId: "t1", structured: { isAsync: true } } },
        "Message.agent",
      ),
    ).toThrow(/unrecognized field/);
  });
});
