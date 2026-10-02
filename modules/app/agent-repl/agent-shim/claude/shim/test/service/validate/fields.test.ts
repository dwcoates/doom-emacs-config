/**
 * THE VALIDATION INVARIANT, field by field: an unset non-optional field is
 * illegal, an unset oneof is illegal, and an identity is never the empty
 * string. Each rule gets its own case, because each is a distinct way a caller
 * can hand the session something it would otherwise have to guess about.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import * as fields from "../../../src/service/validate/fields.js";

function codeOf(act: () => void): Code | undefined {
  try {
    act();
    return undefined;
  } catch (err) {
    return ConnectError.from(err).code;
  }
}

describe("validateAgentId", () => {
  it("accepts a named agent", () => {
    // Arrange.
    const id = create(conversationv1.AgentIdSchema, { value: "a-1" });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentId(id, "p"))).toBeUndefined();
  });

  it("refuses an unset id rather than defaulting to the main agent", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateAgentId(undefined, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses an EMPTY id, which is a sentinel and not an absence", () => {
    // Arrange.
    const id = create(conversationv1.AgentIdSchema, { value: "" });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentId(id, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateTurnId", () => {
  it("refuses an unset turn id", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateTurnId(undefined, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses an empty turn id", () => {
    // Arrange.
    const id = create(conversationv1.TurnIdSchema, { value: "" });

    // Act, Assert.
    expect(codeOf(() => fields.validateTurnId(id, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateAgentActivityId", () => {
  it("refuses an unset activity id", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateAgentActivityId(undefined, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses an empty activity id", () => {
    // Arrange.
    const id = create(conversationv1.AgentActivityIdSchema, { value: "" });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentActivityId(id, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateDetachedWorkId", () => {
  it("refuses an unset work id", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateDetachedWorkId(undefined, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses an empty work id", () => {
    // Arrange.
    const id = create(conversationv1.DetachedWorkIdSchema, { value: "" });

    // Act, Assert.
    expect(codeOf(() => fields.validateDetachedWorkId(id, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateHistoryPointer", () => {
  it("accepts any non-empty pointer WITHOUT parsing it; it is the store's shape", () => {
    // Arrange.
    const pointer = create(conversationv1.HistoryPointerSchema, { value: "{opaque}" });

    // Act, Assert.
    expect(codeOf(() => fields.validateHistoryPointer(pointer, "p"))).toBeUndefined();
  });

  it("refuses an empty pointer", () => {
    // Arrange.
    const pointer = create(conversationv1.HistoryPointerSchema, { value: "" });

    // Act, Assert.
    expect(codeOf(() => fields.validateHistoryPointer(pointer, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateConversationThrough", () => {
  it("accepts a positive instant", () => {
    // Arrange.
    const through = create(conversationv1.ConversationThroughSchema, { atMs: 1n });

    // Act, Assert.
    expect(codeOf(() => fields.validateConversationThrough(through, "p"))).toBeUndefined();
  });

  it("refuses an unset bound", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateConversationThrough(undefined, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses a bound that names no instant", () => {
    // Arrange.
    const through = create(conversationv1.ConversationThroughSchema, { atMs: 0n });

    // Act, Assert.
    expect(codeOf(() => fields.validateConversationThrough(through, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateAgentModel", () => {
  it("refuses an unset model", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateAgentModel(undefined, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses an unnamed model", () => {
    // Arrange.
    const model = create(conversationv1.AgentModelSchema, { name: "" });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentModel(model, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateAgentPermissionMode", () => {
  it("accepts a mode with its arm set", () => {
    // Arrange.
    const mode = create(conversationv1.AgentPermissionModeSchema, {
      mode: { case: "plan", value: create(conversationv1.AgentPermissionModePlanSchema, {}) },
    });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentPermissionMode(mode, "p"))).toBeUndefined();
  });

  it("refuses a mode whose oneof is unset rather than assuming default", () => {
    // Arrange.
    const mode = create(conversationv1.AgentPermissionModeSchema, {});

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentPermissionMode(mode, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validatePromptOrigin", () => {
  it("accepts a stated send site", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => fields.validatePromptOrigin(conversationv1.PromptOrigin.USER_SENT, "p")),
    ).toBeUndefined();
  });

  it("refuses UNSPECIFIED, because origin is persisted and replay reads it", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => fields.validatePromptOrigin(conversationv1.PromptOrigin.UNSPECIFIED, "p")),
    ).toBe(Code.InvalidArgument);
  });
});

describe("validateAgentEffortLevel", () => {
  it("accepts a named level", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => fields.validateAgentEffortLevel(conversationv1.AgentEffortLevel.HIGH, "p")),
    ).toBeUndefined();
  });

  it("refuses UNSPECIFIED, because a guessed level runs every later turn", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => fields.validateAgentEffortLevel(conversationv1.AgentEffortLevel.UNSPECIFIED, "p")),
    ).toBe(Code.InvalidArgument);
  });

  it("refuses a number that names no level", () => {
    // Arrange, Act, Assert.
    expect(
      codeOf(() => fields.validateAgentEffortLevel(99 as conversationv1.AgentEffortLevel, "p")),
    ).toBe(Code.InvalidArgument);
  });
});

describe("validateUserContent", () => {
  it("refuses an utterance with NO blocks, which is not a prompt", () => {
    // Arrange.
    const content = create(conversationv1.UserContentSchema, { blocks: [] });

    // Act, Assert.
    expect(codeOf(() => fields.validateUserContent(content, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses a block whose own oneof is unset", () => {
    // Arrange.
    const content = create(conversationv1.UserContentSchema, {
      blocks: [create(conversationv1.UserContentBlockSchema, {})],
    });

    // Act, Assert.
    expect(codeOf(() => fields.validateUserContent(content, "p"))).toBe(Code.InvalidArgument);
  });

  it("names the offending block's index", () => {
    // Arrange.
    const content = create(conversationv1.UserContentSchema, {
      blocks: [
        create(conversationv1.UserContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text: "ok" }) },
        }),
        create(conversationv1.UserContentBlockSchema, {}),
      ],
    });

    // Act.
    let message = "";
    try {
      fields.validateUserContent(content, "said.content");
    } catch (err) {
      message = ConnectError.from(err).message;
    }

    // Assert.
    expect(message).toContain("said.content.blocks[1]");
  });
});

describe("validateUserSaid", () => {
  it("refuses an unset utterance", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateUserSaid(undefined, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateAgentInput", () => {
  it("accepts a stop, which is legally empty", () => {
    // Arrange.
    const input = create(conversationv1.AgentInputSchema, {
      input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
    });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentInput(input, "p"))).toBeUndefined();
  });

  it("refuses an unset input oneof", () => {
    // Arrange.
    const input = create(conversationv1.AgentInputSchema, {});

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentInput(input, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses an answer whose own arm is unset", () => {
    // Arrange.
    const input = create(conversationv1.AgentInputSchema, {
      input: { case: "answer", value: create(conversationv1.AgentAnswerSchema, {}) },
    });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentInput(input, "p"))).toBe(Code.InvalidArgument);
  });

  it("refuses a prompt arm carrying an empty utterance", () => {
    // Arrange.
    const input = create(conversationv1.AgentInputSchema, {
      input: {
        case: "prompt",
        value: create(conversationv1.UserSaidSchema, {
          content: create(conversationv1.UserContentSchema, { blocks: [] }),
        }),
      },
    });

    // Act, Assert.
    expect(codeOf(() => fields.validateAgentInput(input, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateSessionColdRemediation", () => {
  it("refuses an unset remediation arm", () => {
    // Arrange.
    const remediation = create(conversationv1.SessionColdRemediationSchema, {});

    // Act, Assert.
    expect(codeOf(() => fields.validateSessionColdRemediation(remediation, "p"))).toBe(
      Code.InvalidArgument,
    );
  });

  it("refuses a compact remediation naming no model to compact with", () => {
    // Arrange.
    const remediation = create(conversationv1.SessionColdRemediationSchema, {
      remediation: {
        case: "compact",
        value: create(conversationv1.SessionColdCompactSchema, {}),
      },
    });

    // Act, Assert.
    expect(codeOf(() => fields.validateSessionColdRemediation(remediation, "p"))).toBe(
      Code.InvalidArgument,
    );
  });
});

describe("validatePageSize", () => {
  it("accepts a real budget", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validatePageSize(1, "p"))).toBeUndefined();
  });

  it("refuses a budget of nothing rather than substituting a default", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validatePageSize(0, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateHistoryPointer", () => {
  it("refuses an UNSET pointer rather than reading from an unstated place", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateHistoryPointer(undefined, "p"))).toBe(Code.InvalidArgument);
  });
});

describe("validateUserContent", () => {
  it("refuses UNSET content, which is not the same as an empty utterance", () => {
    // Arrange, Act, Assert.
    expect(codeOf(() => fields.validateUserContent(undefined, "p"))).toBe(Code.InvalidArgument);
  });
});
