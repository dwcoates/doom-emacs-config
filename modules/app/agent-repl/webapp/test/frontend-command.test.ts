/**
 * frontend-command — encode agentshim.frontend.v1.FrontendCommand frames as
 * canonical protojson. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { ARM_KEY, PromptOrigin, encodeFrontendCommand, type FrontendCommand } from "../src/frontend-command.js";

/** Encode and parse back, so assertions read the wire object, not a string. */
function wire(cmd: FrontendCommand): Record<string, unknown> {
  return JSON.parse(encodeFrontendCommand(cmd)) as Record<string, unknown>;
}

describe("the command vocabulary is closed", () => {
  it("carries no paint acknowledgment", () => {
    // Arrange / Act — attestation is gone end to end: this end no longer tells
    // the daemon what it drew, because a workspace's color is connection truth
    // and a render pass says nothing about that.
    // Assert
    expect(Object.keys(ARM_KEY)).not.toContain("paintAck");
  });

  it("names every arm it can encode", () => {
    // Arrange / Act — a body case with no arm key would serialize to an
    // envelope with no command at all.
    // Assert
    expect(Object.values(ARM_KEY).every((v) => v.length > 0)).toBe(true);
  });
});

describe("encodeFrontendCommand — envelope", () => {
  it("carries requestId and workspace on every frame", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws-a",
      body: { case: "interrupt", confirmAgents: true },
    });
    expect(w.requestId).toBe("r1");
    expect(w.workspace).toBe("ws-a");
  });

  it("nests the command under its lowerCamelCase oneof arm key", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "submitPrompt", text: "hi", permissionMode: "", promptOrigin: PromptOrigin.WEBAPP_USER_SENT },
    });
    expect(Object.keys(w).sort()).toEqual(["requestId", "submitPrompt", "workspace"]);
  });
});

describe("encodeFrontendCommand — submitPrompt", () => {
  it("encodes text and permissionMode", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "submitPrompt", text: "run the tests", permissionMode: "plan", promptOrigin: PromptOrigin.WEBAPP_USER_SENT },
    });
    expect(w.submitPrompt).toEqual({ text: "run the tests", permissionMode: "plan", promptOrigin: "PROMPT_ORIGIN_WEBAPP_USER_SENT" });
  });
});

describe("encodeFrontendCommand — interrupt", () => {
  it("encodes the confirm-agents flag", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "interrupt", confirmAgents: false },
    });
    expect(w.interrupt).toEqual({ confirmAgents: false });
  });
});

describe("encodeFrontendCommand — permissionAnswer", () => {
  it("encodes an allow with no updatedInput (Struct omitted)", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "permissionAnswer", permissionRequestId: "pr1", allow: true, denyMessage: "" },
    });
    expect(w.permissionAnswer).toEqual({ permissionRequestId: "pr1", allow: true, denyMessage: "" });
  });

  it("encodes an allow-with-edits Struct as a plain JSON object", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: {
        case: "permissionAnswer",
        permissionRequestId: "pr1",
        allow: true,
        updatedInput: { command: "ls -la" },
        denyMessage: "",
      },
    });
    expect(w.permissionAnswer).toEqual({
      permissionRequestId: "pr1",
      allow: true,
      denyMessage: "",
      updatedInput: { command: "ls -la" },
    });
  });

  it("encodes a denial with its denyMessage", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "permissionAnswer", permissionRequestId: "pr1", allow: false, denyMessage: "no" },
    });
    expect(w.permissionAnswer).toEqual({ permissionRequestId: "pr1", allow: false, denyMessage: "no" });
  });
});

describe("encodeFrontendCommand — createSession", () => {
  it("encodes every session-creation field", () => {
    const w = wire({
      requestId: "r1",
      workspace: "",
      body: {
        case: "createSession",
        cwd: "/work/ws",
        permissionMode: "default",
        configDir: "/home/u/.claude",
        resumeMode: "RESUME_MODE_EXPLICIT",
        fake: true,
      },
    });
    expect(w.createSession).toEqual({
      cwd: "/work/ws",
      permissionMode: "default",
      configDir: "/home/u/.claude",
      resumeMode: "RESUME_MODE_EXPLICIT",
      fake: true,
    });
  });

  it("cannot make creation choose a model", () => {
    const w = wire({
      requestId: "r1",
      workspace: "",
      body: {
        case: "createSession",
        cwd: "/work/ws",
        permissionMode: "default",
        configDir: "",
        resumeMode: "RESUME_MODE_CONTINUE",
        fake: false,
      },
    });
    expect(w.createSession).not.toHaveProperty("model");
  });

  it("never encodes the ungated-session consent the daemon gates on", () => {
    // Arrange / Act — the daemon refuses a bypassPermissions create without
    // allow_ungated, and the webapp has no business consenting from a browser
    // tab, so the field is unrepresentable here rather than merely unset.
    const w = wire({
      requestId: "r1",
      workspace: "",
      body: {
        case: "createSession",
        cwd: "/work/ws",
        permissionMode: "bypassPermissions",
        configDir: "",
        resumeMode: "RESUME_MODE_CONTINUE",
        fake: false,
      },
    });
    // Assert
    expect(w.createSession).not.toHaveProperty("allowUngated");
  });
});

describe("encodeFrontendCommand — setModel", () => {
  it("encodes only an explicit model-update request", () => {
    const w = wire({ requestId: "r1", workspace: "/work/ws", body: { case: "setModel", model: "opus" } });
    expect(w.setModel).toEqual({ model: "opus" });
  });
});

describe("encodeFrontendCommand — deleteSession", () => {
  it("encodes the session id", () => {
    const w = wire({ requestId: "r1", workspace: "", body: { case: "deleteSession", sessionId: "s9" } });
    expect(w.deleteSession).toEqual({ sessionId: "s9" });
  });
});

describe("encodeFrontendCommand — resync", () => {
  it("renders the workspace snapshot fence and uint64 fromSeq", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "resync", fromSeq: 42, fence: "f7" },
    });
    expect(w.resync).toEqual({ fromSeq: "42", fence: "f7" });
  });

  it("emits no field the reserved identity pair used to occupy", () => {
    // `session_id`/`controller_generation_id` are RESERVED in ResyncCmd, and
    // canonical protojson decoding rejects the whole frame on an unknown field.
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "resync", fromSeq: 42, fence: "f7" },
    });
    expect(Object.keys(w.resync as Record<string, unknown>).sort()).toEqual(["fence", "fromSeq"]);
  });
});

describe("encodeFrontendCommand — clientLog (E4)", () => {
  it("renders the level as its canonical protojson enum name", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "warn", message: "seq gap" },
    });
    expect(w.clientLog).toEqual({ level: "CLIENT_LOG_LEVEL_WARN", message: "seq gap" });
  });

  it("renders the error level", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "error", message: "boom" },
    });
    expect((w.clientLog as Record<string, unknown>).level).toBe("CLIENT_LOG_LEVEL_ERROR");
  });

  it("carries a structured context as a plain JSON object", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "info", message: "m", context: { seq: 42 } },
    });
    expect((w.clientLog as Record<string, unknown>).context).toEqual({ seq: 42 });
  });

  it("omits an absent context rather than fabricating an empty Struct", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "info", message: "m" },
    });
    expect("context" in (w.clientLog as Record<string, unknown>)).toBe(false);
  });
});

describe("encodeFrontendCommand — queue controls (E4)", () => {
  it("encodes a force by entry id", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "queueForce", entryId: "q1" } });
    expect(w.queueForce).toEqual({ entryId: "q1" });
  });

  it("encodes an accept by entry id", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "queueAccept", entryId: "q1" } });
    expect(w.queueAccept).toEqual({ entryId: "q1" });
  });

  it("encodes a cancel by entry id", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "queueCancel", entryId: "q1" } });
    expect(w.queueCancel).toEqual({ entryId: "q1" });
  });
});

describe("the hibernation command arms", () => {
  it("encodes HibernateWorkspaceCmd as an empty message under its arm key", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "hibernateWorkspace" } });
    // Assert — the workspace rides the envelope; there is nothing else to say.
    expect(w.hibernateWorkspace).toEqual({});
  });

  it("encodes the whole-conversation compaction with its own scope", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision: "compactAll" } });
    // Assert — the arm says compact, the scope says how much.
    expect(w.reviveSession).toEqual({ compactFirst: { scope: "COMPACTION_SCOPE_ALL" } });
  });

  it("encodes the responses-only compaction with its own scope", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision: "compactResponses" } });
    // Assert
    expect(w.reviveSession).toEqual({ compactFirst: { scope: "COMPACTION_SCOPE_RESPONSES" } });
  });

  it("encodes the prompts-only compaction with its own scope", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision: "compactPrompts" } });
    // Assert
    expect(w.reviveSession).toEqual({ compactFirst: { scope: "COMPACTION_SCOPE_PROMPTS" } });
  });

  it("encodes the prompts-and-responses compaction with its own scope", () => {
    // Arrange / Act
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "reviveSession", decision: "compactPromptsAndResponses" },
    });
    // Assert
    expect(w.reviveSession).toEqual({
      compactFirst: { scope: "COMPACTION_SCOPE_PROMPTS_AND_RESPONSES" },
    });
  });

  it("never sends a compaction without a scope", () => {
    // Arrange — an unstated scope is nacked by the daemon rather than
    // defaulted, so a scopeless encode would take every compacting option on
    // the gate out of service at once.
    const decisions = ["compactAll", "compactResponses", "compactPrompts", "compactPromptsAndResponses"] as const;
    for (const decision of decisions) {
      // Act
      const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision } });
      const arm = (w.reviveSession as Record<string, Record<string, unknown>>).compactFirst;
      // Assert
      expect(arm.scope).toMatch(/^COMPACTION_SCOPE_[A-Z_]+$/);
      expect(arm.scope).not.toBe("COMPACTION_SCOPE_UNSPECIFIED");
    }
  });

  it("encodes the direct revival as its own oneof arm", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision: "direct" } });
    // Assert — the direct arm has nothing to scope, so it stays empty.
    expect(w.reviveSession).toEqual({ direct: {} });
  });

  it("encodes the clear revival as its own oneof arm", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision: "clear" } });
    // Assert — an empty message, like the direct arm: the arm IS the decision,
    // and a clear has nothing to scope because it keeps nothing.
    expect(w.reviveSession).toEqual({ clear: {} });
  });

  it("never lets a revive frame set both modes at once", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "reviveSession", decision: "direct" } });
    // Assert — "no decision" and "both decisions" are equally unrepresentable.
    expect(Object.keys(w.reviveSession as Record<string, unknown>)).toEqual(["direct"]);
  });
});

describe("the merge-queue runtime controls", () => {
  it("encodes a pause as an empty daemon-global arm", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "pauseMergeQueue" } });
    // Assert — the verb IS the arm; the queue is global, so there is nothing
    // for the body to name.
    expect(w.pauseMergeQueue).toEqual({});
  });

  it("encodes a resume as an empty daemon-global arm", () => {
    // Arrange / Act
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "resumeMergeQueue" } });
    // Assert
    expect(w.resumeMergeQueue).toEqual({});
  });

  it("names the run an eviction targets", () => {
    // Arrange / Act
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "evictMerge", runId: "run-7" },
    });
    // Assert — the run id is the only key the roster and every MergeStatus
    // share, so it is the whole address of the entry being evicted.
    expect(w.evictMerge).toEqual({ runId: "run-7" });
  });
});
