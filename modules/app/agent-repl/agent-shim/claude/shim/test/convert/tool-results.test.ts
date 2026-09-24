/**
 * THE VENDOR'S USER-ROLE RECORDS, WHICH ARE MOSTLY NOT A USER.
 *
 * A tool result is not something a person said, so it settles the unit its
 * `tool_use_id` names and never becomes a prompt; the vendor's echo of the real
 * prompt is the same fact a second time and is dropped (R15). What these tests
 * pin is the handling of the records that state LESS than that: a block naming
 * no call, a record with no uuid to key a write by, and the skill DOCUMENT
 * record, which is the only thing that settles a skill unit.
 */
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { convertUserRecord, convertToolProgressMessage } from "../../src/convert/tool-results.js";
import { createTaskKindRegistry } from "../../src/convert/detached.js";
import {
  createCallRegistry,
  type CallRegistry,
  type PendingCall,
} from "../../src/convert/tool-calls.js";
import { TOOL_CONVERTERS } from "../../src/convert/tools/registry.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import { activityOf, foldContext, MAIN_AGENT, type ContextOverrides } from "./fold-harness.js";

type UserRecord = Extract<SdkMessage, { type: "user" }>;

/** One `type: "user"` record, as the vendor spells one. */
function userRecord(fields: Record<string, unknown>): UserRecord {
  return {
    type: "user",
    uuid: "uuid-user",
    session_id: "session-1",
    parent_tool_use_id: null,
    ...fields,
  } as unknown as UserRecord;
}

/** The typed Output object a text Read returns. */
const READ_OUTPUT = {
  type: "text",
  file: { filePath: "/tmp/a", content: "hi", numLines: 1, totalLines: 1 },
};

/** A record carrying tool_result blocks. */
function resultRecord(blocks: unknown[], fields: Record<string, unknown> = {}): UserRecord {
  return userRecord({ message: { role: "user", content: blocks }, ...fields });
}

/** One call in flight. */
function call(toolName: string, input: Record<string, unknown> = {}): PendingCall {
  return { toolUseId: "toolu_1", toolName, input, startedAtMs: 5, agentId: MAIN_AGENT };
}

/** What one user record converts to. */
function convert(
  message: UserRecord,
  registry: CallRegistry = createCallRegistry(),
  overrides: ContextOverrides = {},
): readonly PersistEntry[] {
  return convertUserRecord(
    message,
    foldContext(overrides),
    registry,
    TOOL_CONVERTERS,
    createTaskKindRegistry(),
  );
}

describe("a user record carrying no tool result", () => {
  it("is the prompt the shim already wrote (R15) and is dropped", () => {
    expect(convert(userRecord({ message: { role: "user", content: "hello" } }))).toEqual([]);
  });

  it("is dropped when it is SYNTHETIC too, rather than landing as residue", () => {
    expect(
      convert(userRecord({ message: { role: "user", content: [] }, isSynthetic: true })),
    ).toEqual([]);
  });
});

describe("a tool_result block naming no call", () => {
  it("lands as residue, since nothing can be settled by it", () => {
    const entries = convert(resultRecord([{ type: "tool_result", content: "done" }]));

    expect(entries[0]?.source.discriminator).toBe("residue.unparsed");
  });

  it("lands as residue when the call id is the EMPTY string", () => {
    const entries = convert(
      resultRecord([{ type: "tool_result", tool_use_id: "", content: "done" }]),
    );

    expect(entries[0]?.source.discriminator).toBe("residue.unparsed");
  });

  it("still settles the OTHER blocks of the same record", () => {
    const registry = createCallRegistry();
    registry.remember(call("Read", { file_path: "/tmp/a" }));
    const entries = convert(
      resultRecord([
        { type: "tool_result", content: "orphan" },
        { type: "tool_result", tool_use_id: "toolu_1", content: "read it" },
      ], { tool_use_result: READ_OUTPUT }),
      registry,
    );

    expect(entries.map((entry) => entry.source.discriminator)).toContain("activity.read.success");
  });
});

describe("the coordinate a write takes when the SDK omitted the record's uuid", () => {
  it("names exactly this settle, so a replay still absorbs", () => {
    // The SDK declares a user record's uuid OPTIONAL, and a write id needs a
    // coordinate; the settling call's own id is the deterministic stand-in.
    const registry = createCallRegistry();
    registry.remember(call("Read", { file_path: "/tmp/a" }));
    const entries = convert(
      resultRecord([{ type: "tool_result", tool_use_id: "toolu_1", content: "read it" }], {
        uuid: undefined,
        tool_use_result: READ_OUTPUT,
      }),
      registry,
    );

    expect(entries[0]?.source.vendorUuid).toBe("tool_result:toolu_1");
  });
});

describe("a DENIED call retires its unit", () => {
  it("settles failure with NO content, since the producer observed no error content", () => {
    const registry = createCallRegistry();
    registry.remember(call("Read", { file_path: "/tmp/a" }));
    const entries = convert(
      resultRecord([
        { type: "tool_result", tool_use_id: "toolu_1", content: "Error: denied", is_error: true },
      ]),
      registry,
      { deniedCall: () => true },
    );

    const activity = activityOf(entries[0]);
    const read = activity?.item.value as conversationv1.AgentRead;
    const failure = read.result.value as conversationv1.AgentReadFailure;
    expect(failure.error?.content).toBeUndefined();
  });

  it("restates the call's path, since the start it upserts over is gone once it lands", () => {
    const registry = createCallRegistry();
    registry.remember(call("Read", { file_path: "/tmp/a" }));
    const entries = convert(
      resultRecord([
        { type: "tool_result", tool_use_id: "toolu_1", content: "Error: denied", is_error: true },
      ]),
      registry,
      { deniedCall: () => true },
    );

    const read = activityOf(entries[0])?.item.value as conversationv1.AgentRead;
    expect((read.result.value as conversationv1.AgentReadFailure).path?.path).toBe("/tmp/a");
  });
});

// A SETTLED SEND STANDS ALONE on every path a tool result reaches the fold by:
// the SDK's live stream and a transcript's records are the same user record,
// and the settle it folds into must restate the call's address and summary
// because the start it upserts over is gone from the store once it lands.
describe("a send's settle, folded from its tool result", () => {
  const SEND_INPUT = { to: "vetter", message: "go", summary: "Scroll fix landed; merge master in" };

  /** The send arm the one settled entry carries. */
  function sendOf(entries: readonly PersistEntry[]): conversationv1.AgentSendMessage["result"] {
    const activity = activityOf(entries[0]);
    expect(activity?.item.case).toBe("sendMessage");
    return (activity?.item.value as conversationv1.AgentSendMessage).result;
  }

  it("restates the address and summary on a delivered send", () => {
    // Arrange.
    const registry = createCallRegistry();
    registry.remember(call("SendMessage", SEND_INPUT));

    // Act.
    const result = sendOf(
      convert(
        resultRecord([{ type: "tool_result", tool_use_id: "toolu_1", content: "sent" }], {
          tool_use_result: { success: true, pin: { id: "b7c" } },
        }),
        registry,
      ),
    );

    // Assert.
    const success = result.value as conversationv1.AgentSendMessageSuccess;
    expect({ to: success.addressedTo, summary: success.summary?.text }).toEqual({
      to: "vetter",
      summary: "Scroll fix landed; merge master in",
    });
  });

  it("restates the address and summary on a refused send", () => {
    // Arrange.
    const registry = createCallRegistry();
    registry.remember(call("SendMessage", SEND_INPUT));

    // Act.
    const result = sendOf(
      convert(
        resultRecord([
          { type: "tool_result", tool_use_id: "toolu_1", content: "stopped", is_error: true },
        ]),
        registry,
      ),
    );

    // Assert.
    const failure = result.value as conversationv1.AgentSendMessageFailure;
    expect({ to: failure.addressedTo, summary: failure.summary?.text }).toEqual({
      to: "vetter",
      summary: "Scroll fix landed; merge master in",
    });
  });

  it("restates the address and summary on a DENIED send", () => {
    // Arrange.
    const registry = createCallRegistry();
    registry.remember(call("SendMessage", SEND_INPUT));

    // Act.
    const result = sendOf(
      convert(
        resultRecord([
          { type: "tool_result", tool_use_id: "toolu_1", content: "Error: denied", is_error: true },
        ]),
        registry,
        { deniedCall: () => true },
      ),
    );

    // Assert.
    const failure = result.value as conversationv1.AgentSendMessageFailure;
    expect({ to: failure.addressedTo, summary: failure.summary?.text }).toEqual({
      to: "vetter",
      summary: "Scroll fix landed; merge master in",
    });
  });
});

// ---------------------------------------------------------------------------
// The skill DOCUMENT, joined to its call by `sourceToolUseID`
// ---------------------------------------------------------------------------

/** A skill-document record, as the vendor injects one. */
function skillDocument(content: unknown, fields: Record<string, unknown> = {}): UserRecord {
  return userRecord({
    isMeta: true,
    sourceToolUseID: "toolu_1",
    message: { role: "user", content },
    ...fields,
  });
}

/** A registry holding one Skill call in flight. */
function skillRegistry(input: Record<string, unknown> = { skill: "graphify" }): CallRegistry {
  const registry = createCallRegistry();
  registry.remember(call("Skill", input));
  return registry;
}

describe("the skill unit settles on its DOCUMENT", () => {
  it("carries the markdown the skill put in front of the agent", () => {
    const entries = convert(skillDocument("# Graphify\nrun it"), skillRegistry());

    const activity = activityOf(entries[0]);
    const skill = activity?.item.value as conversationv1.AgentSkillUse;
    const success = skill.result.value as conversationv1.AgentSkillUseSuccess;
    expect(success.document?.markdown).toBe("# Graphify\nrun it");
  });

  it("joins the blocks of an array-shaped document into one markdown body", () => {
    const entries = convert(
      skillDocument([{ type: "text", text: "# Graphify\n" }, { type: "text", text: "run it" }]),
      skillRegistry(),
    );

    const activity = activityOf(entries[0]);
    const skill = activity?.item.value as conversationv1.AgentSkillUse;
    const success = skill.result.value as conversationv1.AgentSkillUseSuccess;
    expect(success.document?.markdown).toBe("# Graphify\nrun it");
  });

  it("ignores an array block that carries no text at all", () => {
    const entries = convert(
      skillDocument([{ type: "image" }, { type: "text", text: "run it" }]),
      skillRegistry(),
    );

    const activity = activityOf(entries[0]);
    const skill = activity?.item.value as conversationv1.AgentSkillUse;
    const success = skill.result.value as conversationv1.AgentSkillUseSuccess;
    expect(success.document?.markdown).toBe("run it");
  });

  it("takes the call out of the registry, so the unit cannot settle twice", () => {
    const registry = skillRegistry();
    convert(skillDocument("# Graphify"), registry);

    expect(registry.peek("toolu_1")).toBeUndefined();
  });

  it("keys the write by the call when the record carried no uuid of its own", () => {
    const entries = convert(skillDocument("# Graphify", { uuid: undefined }), skillRegistry());

    expect(entries[0]?.source.vendorUuid).toBe("skill_document:toolu_1");
  });

  it("carries the allowances RETAINED off the acknowledgement, the only record that stated them", () => {
    const registry = createCallRegistry();
    registry.remember({ ...call("Skill", { skill: "graphify" }), retainedAllowedTools: ["Read"] });
    const entries = convert(skillDocument("# Graphify"), registry);

    const activity = activityOf(entries[0]);
    const skill = activity?.item.value as conversationv1.AgentSkillUse;
    const success = skill.result.value as conversationv1.AgentSkillUseSuccess;
    expect(success.allowedTools?.toolNames).toEqual(["Read"]);
  });
});

describe("what is NOT a skill document", () => {
  it("a record with no sourceToolUseID leaves the skill unit open", () => {
    const registry = skillRegistry();
    convert(userRecord({ isMeta: true, message: { role: "user", content: "# Graphify" } }), registry);

    expect(registry.peek("toolu_1")).toBeDefined();
  });

  it("a record whose sourceToolUseID is EMPTY leaves the skill unit open", () => {
    const registry = skillRegistry();
    convert(skillDocument("# Graphify", { sourceToolUseID: "" }), registry);

    expect(registry.peek("toolu_1")).toBeDefined();
  });

  it("a record that is not META leaves the skill unit open", () => {
    // The link alone is not the document; the vendor marks the injection.
    const registry = skillRegistry();
    convert(skillDocument("# Graphify", { isMeta: false }), registry);

    expect(registry.peek("toolu_1")).toBeDefined();
  });

  it("a document whose linked call this shim never saw settles nothing", () => {
    expect(convert(skillDocument("# Graphify"), createCallRegistry())).toEqual([]);
  });

  it("a document linked to a call that is NOT a Skill settles nothing", () => {
    const registry = createCallRegistry();
    registry.remember(call("Read", { file_path: "/tmp/a" }));

    expect(convert(skillDocument("# Graphify"), registry)).toEqual([]);
  });
});

describe("a skill document that says nothing", () => {
  it("leaves the skill unit OPEN rather than settling it on an empty body", () => {
    expect(convert(skillDocument(""), skillRegistry())).toEqual([]);
  });

  it("leaves it open when the content is neither text nor blocks", () => {
    expect(convert(skillDocument({ unexpected: true }), skillRegistry())).toEqual([]);
  });

  it("keeps the call remembered, so a later document can still settle it", () => {
    const registry = skillRegistry();
    convert(skillDocument(""), registry);

    expect(registry.peek("toolu_1")).toBeDefined();
  });
});

describe("a skill document whose call named no skill", () => {
  it("skips the settle frame rather than writing one with no arm", () => {
    // The store refuses an activity that sets no item arm, and a refused batch
    // blocks the queue behind it.
    expect(convert(skillDocument("# Something"), skillRegistry({}))).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// The per-call heartbeat
// ---------------------------------------------------------------------------

describe("convertToolProgressMessage", () => {
  it("produces a progress frame for a kind that declares one", () => {
    const registry = createCallRegistry();
    registry.remember(call("Bash", { command: "sleep 5" }));

    const entries = convertToolProgressMessage(
      {
        type: "tool_progress",
        uuid: "uuid-beat",
        session_id: "session-1",
        tool_use_id: "toolu_1",
      } as unknown as Extract<SdkMessage, { type: "tool_progress" }>,
      foldContext(),
      registry,
      TOOL_CONVERTERS,
    );

    expect(entries).toHaveLength(1);
  });

  it("produces nothing for a beat naming a call this shim never saw announced", () => {
    const entries = convertToolProgressMessage(
      {
        type: "tool_progress",
        uuid: "uuid-beat",
        session_id: "session-1",
        tool_use_id: "toolu_nope",
      } as unknown as Extract<SdkMessage, { type: "tool_progress" }>,
      foldContext(),
      createCallRegistry(),
      TOOL_CONVERTERS,
    );

    expect(entries).toEqual([]);
  });
});

describe("a subagent's tool result", () => {
  it("settles the call its own stream announced", () => {
    // Arrange: the spawn and the subagent's read, each on the stream it rode.
    const registry = createCallRegistry();
    registry.remember({ ...call("Agent"), toolUseId: "toolu_spawn" });
    registry.remember({ ...call("Read", { file_path: "/tmp/a" }), spawningCall: "toolu_spawn" });

    // Act
    const entries = convert(
      resultRecord([{ type: "tool_result", tool_use_id: "toolu_1", content: "hi" }], {
        parent_tool_use_id: "toolu_spawn",
        tool_use_result: READ_OUTPUT,
      }),
      registry,
    );

    // Assert
    expect(entries.map((entry) => entry.source.discriminator)).toEqual(["activity.read.success"]);
  });

  it("releases the call even when it carries no typed output", () => {
    // Arrange: the SDK forwards a subagent's results with no `tool_use_result`.
    const registry = createCallRegistry();
    registry.remember({ ...call("Agent"), toolUseId: "toolu_spawn" });
    registry.remember({ ...call("Read", { file_path: "/tmp/a" }), spawningCall: "toolu_spawn" });

    // Act
    convert(
      resultRecord([{ type: "tool_result", tool_use_id: "toolu_1", content: "hi" }], {
        parent_tool_use_id: "toolu_spawn",
      }),
      registry,
    );

    // Assert
    expect(registry.peek("toolu_1")).toBeUndefined();
  });
});
