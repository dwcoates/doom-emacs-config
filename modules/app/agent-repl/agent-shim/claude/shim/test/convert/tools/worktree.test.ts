/**
 * The worktree converter. Two vendor tool names, one unit kind, and the
 * separation that matters: what the exit ASKED for lives on the start, what the
 * vendor says HAPPENED lives on the success — a refused removal must not read
 * as a completed one.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { worktreeConverter } from "../../../src/convert/tools/worktree.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callNamed(toolName: string, input: Record<string, unknown> = {}): PendingCall {
  return {
    toolUseId: "toolu_worktree",
    toolName,
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("what the model was shown"),
    isError,
    structured,
    settledAtMs: 1_700_000_005_000,
  };
}

function worktreeOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentWorktree {
  expect(item?.case).toBe("worktree");
  return item?.value as conversationv1.AgentWorktree;
}

function startOf(call: PendingCall): conversationv1.AgentWorktreeStart {
  const worktree = worktreeOf(worktreeConverter.start(call));
  expect(worktree.state.case).toBe("start");
  return worktree.state.value as conversationv1.AgentWorktreeStart;
}

function successOf(toolName: string, structured: unknown): conversationv1.AgentWorktreeSuccess {
  const worktree = worktreeOf(worktreeConverter.settle(callNamed(toolName), outcomeWith(structured)));
  expect(worktree.state.case).toBe("success");
  return worktree.state.value as conversationv1.AgentWorktreeSuccess;
}

describe("worktreeConverter kind and arms", () => {
  it("declares the worktree kind", () => {
    // Arrange, Act, Assert.
    expect(worktreeConverter.kind).toBe("worktree");
  });

  it("carries NO progress, because AgentWorktree declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(worktreeConverter.carriesProgress).toBe(false);
  });
});

describe("worktreeConverter.start", () => {
  it("carries the name an enter asked for", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("EnterWorktree", { name: "fix/thing" })).act).toEqual({
      case: "enter",
      value: create(conversationv1.AgentWorktreeEnterSchema, { name: "fix/thing" }),
    });
  });

  it("carries the path an enter asked for instead of a name", () => {
    // Arrange, Act, Assert.
    expect(
      (startOf(callNamed("EnterWorktree", { path: "/tmp/tree" })).act
        .value as conversationv1.AgentWorktreeEnter).path,
    ).toBe("/tmp/tree");
  });

  it("leaves both enter fields UNSET when the tool was left to choose", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("EnterWorktree")).act.value).toEqual(
      create(conversationv1.AgentWorktreeEnterSchema, {}),
    );
  });

  it("states an exit that asked to keep the tree", () => {
    // Arrange, Act.
    const act = startOf(callNamed("ExitWorktree", { action: "keep" })).act;

    // Assert.
    expect((act.value as conversationv1.AgentWorktreeExit).action.case).toBe("keep");
  });

  it("states an exit that asked to remove the tree, with the discard flag", () => {
    // Arrange, Act.
    const act = startOf(callNamed("ExitWorktree", { action: "remove", discard_changes: true })).act;

    // Assert.
    expect((act.value as conversationv1.AgentWorktreeExit).action).toEqual({
      case: "remove",
      value: create(conversationv1.AgentWorktreeExitRemoveSchema, { discardChanges: true }),
    });
  });

  it("leaves what was asked unstated when the exit named no recognized action", () => {
    // Arrange, Act.
    const act = startOf(callNamed("ExitWorktree", { action: "obliterate" })).act;

    // Assert.
    expect((act.value as conversationv1.AgentWorktreeExit).action.case).toBeUndefined();
  });

  it("says a removal discards NOTHING when the caller set no discard flag", () => {
    // Arrange, Act.
    const act = startOf(callNamed("ExitWorktree", { action: "remove" })).act;

    // Assert.
    expect((act.value as conversationv1.AgentWorktreeExit).action).toEqual({
      case: "remove",
      value: create(conversationv1.AgentWorktreeExitRemoveSchema, { discardChanges: false }),
    });
  });

  it("leaves the act UNSET when the tool name is neither the enter nor the exit call", () => {
    // Arrange, Act.
    const act = startOf(callNamed("Worktree")).act;

    // Assert.
    expect(act.case).toBeUndefined();
  });

  it("stamps the instant the call was issued", () => {
    // Arrange, Act, Assert.
    expect(startOf(callNamed("EnterWorktree")).startedAt?.atMs).toBe(1_700_000_000_000n);
  });
});

describe("worktreeConverter.settle", () => {
  it("states where the session now is on the entered arm", () => {
    // Arrange, Act.
    const success = successOf("EnterWorktree", {
      worktreePath: "/tmp/tree",
      worktreeBranch: "fix/thing",
      message: "Entered worktree",
    });

    // Assert.
    expect(success.act).toEqual({
      case: "entered",
      value: create(conversationv1.AgentWorktreeEnteredSchema, {
        path: "/tmp/tree",
        branch: "fix/thing",
        message: "Entered worktree",
      }),
    });
  });

  it("leaves the branch UNSET when the vendor named none", () => {
    // Arrange, Act.
    const success = successOf("EnterWorktree", { worktreePath: "/tmp/tree", message: "in" });

    // Assert.
    expect((success.act.value as conversationv1.AgentWorktreeEntered).branch).toBeUndefined();
  });

  it("states a kept tree from the RESULT's own action, not the request's", () => {
    // Arrange, Act.
    const success = successOf("ExitWorktree", {
      action: "keep",
      originalCwd: "/repo",
      worktreePath: "/tmp/tree",
      message: "kept",
    });

    // Assert.
    expect((success.act.value as conversationv1.AgentWorktreeExited).outcome.case).toBe("kept");
  });

  it("counts what a removal discarded, as the vendor stated it", () => {
    // Arrange, Act.
    const success = successOf("ExitWorktree", {
      action: "remove",
      originalCwd: "/repo",
      worktreePath: "/tmp/tree",
      discardedFiles: 2,
      discardedCommits: 1,
      message: "removed",
    });

    // Assert.
    expect((success.act.value as conversationv1.AgentWorktreeExited).outcome).toEqual({
      case: "removed",
      value: create(conversationv1.AgentWorktreeRemovedSchema, {
        discardedFiles: 2,
        discardedCommits: 1,
      }),
    });
  });

  it("leaves the discarded counts UNSET when the vendor stated no figure", () => {
    // Arrange, Act.
    const success = successOf("ExitWorktree", {
      action: "remove",
      originalCwd: "/repo",
      worktreePath: "/tmp/tree",
      message: "removed",
    });

    // Assert: unset is not zero — nobody counted.
    expect(
      ((success.act.value as conversationv1.AgentWorktreeExited).outcome
        .value as conversationv1.AgentWorktreeRemoved).discardedFiles,
    ).toBeUndefined();
  });

  it("carries the directory the session returned to", () => {
    // Arrange, Act.
    const success = successOf("ExitWorktree", {
      action: "keep",
      originalCwd: "/repo",
      worktreePath: "/tmp/tree",
      message: "out",
    });

    // Assert.
    expect((success.act.value as conversationv1.AgentWorktreeExited).originalCwd).toBe("/repo");
  });

  it("carries the tmux session name when the vendor stated one", () => {
    // Arrange, Act.
    const success = successOf("ExitWorktree", {
      action: "keep",
      originalCwd: "/repo",
      worktreePath: "/tmp/tree",
      tmuxSessionName: "tree-1",
      message: "out",
    });

    // Assert.
    expect((success.act.value as conversationv1.AgentWorktreeExited).tmuxSessionName).toBe("tree-1");
  });

  it("leaves what became of the tree unstated when the result named no known action", () => {
    // Arrange, Act.
    const success = successOf("ExitWorktree", {
      action: "vanished",
      originalCwd: "/repo",
      worktreePath: "/tmp/tree",
      message: "?",
    });

    // Assert.
    expect((success.act.value as conversationv1.AgentWorktreeExited).outcome.case).toBeUndefined();
  });

  it("stamps the settle instant on the success arm", () => {
    // Arrange, Act.
    const success = successOf("EnterWorktree", { worktreePath: "/tmp/tree", message: "in" });

    // Assert.
    expect(success.settledAt?.atMs).toBe(1_700_000_005_000n);
  });

  it("produces NO frame when the settled call named no worktree path", () => {
    // Arrange, Act.
    const item = worktreeConverter.settle(callNamed("EnterWorktree"), outcomeWith({ message: "in" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when the settled call's name names no act", () => {
    // Arrange, Act.
    const item = worktreeConverter.settle(
      callNamed("SomethingElse"),
      outcomeWith({ worktreePath: "/tmp/tree" }),
    );

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored call as the failure arm", () => {
    // Arrange, Act.
    const worktree = worktreeOf(
      worktreeConverter.settle(callNamed("ExitWorktree"), outcomeWith(undefined, true)),
    );

    // Assert.
    const failure = worktree.state.value as conversationv1.AgentWorktreeFailure;
    expect(worktree.state.case).toBe("failure");
    expect(failure.error?.settledAt?.atMs).toBe(1_700_000_005_000n);
  });
});
