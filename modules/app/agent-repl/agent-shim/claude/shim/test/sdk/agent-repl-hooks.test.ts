/**
 * agent-repl's hooks on the vendor's agent-to-agent tools: a `ListAgents`
 * answer is told about the workspaces it leaves out, a `SendMessage` to a
 * workspace's `agent-repl:` address is refused with the `workspace prompt` call
 * that reaches it, and a failed `SendMessage` is told how a workspace is
 * reached. The hooks are plain callbacks, so the suite calls them with the
 * inputs the vendor hands them.
 */
import type { HookCallback, HookInput } from "@anthropic-ai/claude-agent-sdk";
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  LIST_AGENTS_TOOL,
  LIST_AGENTS_WORKSPACES_NOTE,
  SEND_MESSAGE_TOOL,
  agentReplHooks,
  listAgentsWorkspacesHook,
  sendMessageFailureHook,
  sendMessageFailureNote,
  sendMessageRecipient,
  sendMessageWorkspaceAddressHook,
  workspaceAddressRefusal,
} from "../../src/sdk/agent-repl-hooks.js";

const BASE = { session_id: "s-1", transcript_path: "/t.jsonl", cwd: "/ws" };

function preToolUse(toolInput: unknown): HookInput {
  return { ...BASE, hook_event_name: "PreToolUse", tool_name: SEND_MESSAGE_TOOL, tool_input: toolInput, tool_use_id: "tu-1" };
}

function postToolUse(toolName: string): HookInput {
  return { ...BASE, hook_event_name: "PostToolUse", tool_name: toolName, tool_input: {}, tool_response: "3 peer sessions", tool_use_id: "tu-2" };
}

function postToolUseFailure(toolInput: unknown): HookInput {
  return {
    ...BASE,
    hook_event_name: "PostToolUseFailure",
    tool_name: SEND_MESSAGE_TOOL,
    tool_input: toolInput,
    tool_use_id: "tu-3",
    error: "No peer session named 'torch-human-support-ceac' is reachable.",
  };
}

function call(hook: HookCallback, input: HookInput): ReturnType<HookCallback> {
  return hook(input, "tu", { signal: new AbortController().signal });
}

describe("agentReplHooks", () => {
  it("registers each hook on the one vendor tool and event it answers", () => {
    // Act.
    const hooks = agentReplHooks();

    // Assert.
    expect(hooks).toEqual({
      PreToolUse: [{ matcher: SEND_MESSAGE_TOOL, hooks: [sendMessageWorkspaceAddressHook] }],
      PostToolUse: [{ matcher: LIST_AGENTS_TOOL, hooks: [listAgentsWorkspacesHook] }],
      PostToolUseFailure: [{ matcher: SEND_MESSAGE_TOOL, hooks: [sendMessageFailureHook] }],
    });
  });

  it("names the vendor's own tools", () => {
    expect([LIST_AGENTS_TOOL, SEND_MESSAGE_TOOL]).toEqual(["ListAgents", "SendMessage"]);
  });
});

describe("listAgentsWorkspacesHook", () => {
  it("tells every ListAgents answer about the workspaces it leaves out", async () => {
    // Act.
    const output = await call(listAgentsWorkspacesHook, postToolUse(LIST_AGENTS_TOOL));

    // Assert.
    expect(output).toEqual({
      hookSpecificOutput: { hookEventName: "PostToolUse", additionalContext: LIST_AGENTS_WORKSPACES_NOTE },
    });
  });

  it("says idle and cold workspaces are missing and how to list and message them", () => {
    expect(LIST_AGENTS_WORKSPACES_NOTE).toBe(
      "agent-repl: this list never includes agent-repl workspaces. A workspace that is open but idle " +
        "or cold, in any repository, is missing from it, and a peer session listed here may itself be " +
        "a workspace's agent. Never message a workspace with SendMessage. List the open workspaces " +
        "with `~/.claude/skills/agent-repl-workspaces/agent-repld workspace list` (one with " +
        "`workspace status NAME`, closed ones with `workspace find`), and message one with " +
        "`~/.claude/skills/agent-repl-workspaces/agent-repld workspace prompt NAME TEXT` " +
        "(the /agent-repl-workspaces skill).",
    );
  });

  it("logs the answer it annotated", async () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    await call(listAgentsWorkspacesHook, postToolUse(LIST_AGENTS_TOOL));

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("ListAgents answer"));
    expect([record?.level, record?.context.tool_use_id]).toEqual(["debug", "tu-2"]);
  });

  it("throws when called for an event it is not registered for", () => {
    expect(() => call(listAgentsWorkspacesHook, preToolUse({ to: "x" }))).toThrow(
      "shim agent-repl hooks: a PostToolUse hook ran for PreToolUse",
    );
  });

  it("logs the misregistered event it refused", () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    expect(() => call(listAgentsWorkspacesHook, preToolUse({ to: "x" }))).toThrow();

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("not registered for"));
    expect([record?.level, record?.context.expected, record?.context.got]).toEqual(["error", "PostToolUse", "PreToolUse"]);
  });
});

describe("sendMessageWorkspaceAddressHook", () => {
  it("refuses a SendMessage to a workspace's agent-repl address, naming workspace prompt", async () => {
    // Act.
    const output = await call(sendMessageWorkspaceAddressHook, preToolUse({ to: "agent-repl:torch-human-support-ceac", message: "hi" }));

    // Assert.
    expect(output).toEqual({
      hookSpecificOutput: {
        hookEventName: "PreToolUse",
        permissionDecision: "deny",
        permissionDecisionReason: workspaceAddressRefusal("torch-human-support-ceac"),
      },
    });
  });

  it("says how the refused workspace is reached", () => {
    expect(workspaceAddressRefusal("torch-human-support-ceac")).toBe(
      "agent-repl: 'agent-repl:torch-human-support-ceac' is the agent-repl workspace " +
        "torch-human-support-ceac, which SendMessage cannot reach. Message it with " +
        "`~/.claude/skills/agent-repl-workspaces/agent-repld workspace prompt torch-human-support-ceac TEXT`, " +
        "which holds the prompt behind its running work, wakes it if it is cold, and shows the send " +
        "in both workspaces' feeds.",
    );
  });

  it("lets a SendMessage to any other address run untouched", async () => {
    // Act.
    const output = await call(sendMessageWorkspaceAddressHook, preToolUse({ to: "researcher", message: "hi" }));

    // Assert.
    expect(output).toEqual({});
  });

  it("logs the refusal with the workspace it named", async () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    await call(sendMessageWorkspaceAddressHook, preToolUse({ to: "agent-repl:parser-fix", message: "hi" }));

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("refused a SendMessage"));
    expect([record?.level, record?.context.recipient, record?.context.workspace_name]).toEqual([
      "info",
      "agent-repl:parser-fix",
      "parser-fix",
    ]);
  });

  it("throws when called for an event it is not registered for", () => {
    expect(() => call(sendMessageWorkspaceAddressHook, postToolUse(SEND_MESSAGE_TOOL))).toThrow(
      "shim agent-repl hooks: a PreToolUse hook ran for PostToolUse",
    );
  });
});

describe("sendMessageFailureHook", () => {
  it("tells a failed SendMessage how a workspace of that name is reached", async () => {
    // Act.
    const output = await call(sendMessageFailureHook, postToolUseFailure({ to: "torch-human-support-ceac", message: "hi" }));

    // Assert.
    expect(output).toEqual({
      hookSpecificOutput: {
        hookEventName: "PostToolUseFailure",
        additionalContext: sendMessageFailureNote("torch-human-support-ceac"),
      },
    });
  });

  it("names the status and prompt calls for the recipient", () => {
    expect(sendMessageFailureNote("torch-human-support-ceac")).toBe(
      "agent-repl: if 'torch-human-support-ceac' is an agent-repl workspace, SendMessage cannot reach it, " +
        "and an idle or cold workspace is never among the peer sessions. Look it up with " +
        "`~/.claude/skills/agent-repl-workspaces/agent-repld workspace status torch-human-support-ceac` " +
        "(or `workspace list`) and message it with " +
        "`~/.claude/skills/agent-repl-workspaces/agent-repld workspace prompt torch-human-support-ceac TEXT`.",
    );
  });

  it("logs the failure with its recipient and the vendor's error", async () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    await call(sendMessageFailureHook, postToolUseFailure({ to: "torch-human-support-ceac", message: "hi" }));

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("failed SendMessage"));
    expect([record?.level, record?.context.recipient, record?.context.error]).toEqual([
      "debug",
      "torch-human-support-ceac",
      "No peer session named 'torch-human-support-ceac' is reachable.",
    ]);
  });

  it("throws when called for an event it is not registered for", () => {
    expect(() => call(sendMessageFailureHook, preToolUse({ to: "x" }))).toThrow(
      "shim agent-repl hooks: a PostToolUseFailure hook ran for PreToolUse",
    );
  });
});

describe("sendMessageRecipient", () => {
  it("reads the call's recipient", () => {
    expect(sendMessageRecipient({ to: "researcher", message: "hi" })).toBe("researcher");
  });

  it.each([
    ["an input with no `to`", { message: "hi" }],
    ["a `to` that is not a string", { to: 7 }],
    ["an input that is not an object", "researcher"],
    ["a null input", null],
  ])("throws on %s", (_label, input) => {
    expect(() => sendMessageRecipient(input)).toThrow("shim agent-repl hooks: a SendMessage input carries no string `to`");
  });

  it("logs the recipient-less input it refused", () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    expect(() => sendMessageRecipient({ message: "hi" })).toThrow();

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("without a recipient"));
    expect([record?.level, record?.context.input_type]).toEqual(["error", "object"]);
  });
});
