import { describe, expect, it } from "vitest";
import { MemberContext, livePollSourceIds, resolveMember } from "../src/stream-member.js";
import { ConversationItem, ToolItem } from "../src/store.js";
import { TaskTail } from "../src/watcher-poll.js";

function tool(opts: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "t1",
    toolName: "Bash",
    messageId: "m1",
    ts: "2026-07-19T12:00:00.000Z",
    inputJson: "{}",
    input: {},
    inputDone: true,
    ...opts,
  };
}

function ctx(opts: {
  children?: readonly ConversationItem[];
  tails?: Record<string, TaskTail>;
} = {}): MemberContext {
  return {
    children: () => opts.children ?? [],
    taskTail: (id) => opts.tails?.[id],
  };
}

/** A spawning result announcing task id BG1. */
const SPAWN_RESULT = {
  isError: false,
  content: "Command running in background with ID: bg1. Output is being written to: /tmp/claude-1/s/tasks/bg1.output",
};

function stopChild(taskId: string, ok: boolean): ToolItem {
  return tool({
    toolUseId: "stop1",
    toolName: "TaskStop",
    input: { task_id: taskId },
    result: ok ? { isError: false, content: "stopped" } : { isError: true, content: "no" },
  });
}

describe("resolveMember", () => {
  it("returns null for a call with nothing streaming about it", () => {
    expect(resolveMember(tool({ result: { isError: false, content: "ok" } }), ctx())).toBeNull();
  });

  it("reads a completed notification as done and settled", () => {
    const m = resolveMember(
      tool({ result: SPAWN_RESULT, notification: { status: "completed", text: "done" } }),
      ctx(),
    );
    expect(m?.status).toBe("done");
    expect(m?.settled).toBe(true);
  });

  it("surfaces an errored notification as error rather than done", () => {
    const m = resolveMember(
      tool({ result: SPAWN_RESULT, notification: { status: "failed", text: "boom" } }),
      ctx(),
    );
    expect(m?.status).toBe("error");
  });

  it("surfaces a killed notification as killed", () => {
    const m = resolveMember(
      tool({ result: SPAWN_RESULT, notification: { status: "killed", text: "killed" } }),
      ctx(),
    );
    expect(m?.status).toBe("killed");
  });

  it("reads a successful folded TaskStop as killed", () => {
    const m = resolveMember(
      tool({ result: SPAWN_RESULT }),
      ctx({ children: [stopChild("bg1", true)] }),
    );
    expect(m?.status).toBe("killed");
    expect(m?.settled).toBe(true);
  });

  it("ignores an errored folded TaskStop", () => {
    const m = resolveMember(
      tool({ result: SPAWN_RESULT }),
      ctx({ children: [stopChild("bg1", false)] }),
    );
    expect(m?.status).toBe("running");
  });

  it("adopts a terminal daemon-classified source status", () => {
    const m = resolveMember(
      tool({
        result: SPAWN_RESULT,
        asyncSource: { source_id: "bg1", kind: "shell", status: "error" },
      }),
      ctx(),
    );
    expect(m?.status).toBe("error");
  });

  it("settles on the poll's done flag before any notification lands", () => {
    const m = resolveMember(
      tool({
        result: SPAWN_RESULT,
        asyncSource: { source_id: "bg1", kind: "shell", status: "running" },
      }),
      ctx({ tails: { bg1: { text: "out", offset: 3, done: true, elapsedMs: 5 } } }),
    );
    expect(m?.status).toBe("done");
  });

  it("prefers the ws-streamed tail over the polled tail", () => {
    const m = resolveMember(
      tool({
        result: SPAWN_RESULT,
        taskOutput: "ws-tail",
        asyncSource: { source_id: "bg1", kind: "shell", status: "running" },
      }),
      ctx({ tails: { bg1: { text: "polled", offset: 6, done: false, elapsedMs: 5 } } }),
    );
    expect(m?.tail).toBe("ws-tail");
  });

  it("falls back to the polled tail when nothing ws-streamed", () => {
    const m = resolveMember(
      tool({
        result: SPAWN_RESULT,
        asyncSource: { source_id: "bg1", kind: "shell", status: "running" },
      }),
      ctx({ tails: { bg1: { text: "polled", offset: 6, done: false, elapsedMs: 5 } } }),
    );
    expect(m?.tail).toBe("polled");
  });

  it("probes announced task ids for a tail when the source id has none", () => {
    const m = resolveMember(
      tool({ result: SPAWN_RESULT }),
      ctx({ tails: { bg1: { text: "by-task-id", offset: 10, done: false, elapsedMs: 5 } } }),
    );
    expect(m?.tail).toBe("by-task-id");
  });

  it("stacks child feed above the stream body in Shape A order", () => {
    const child = tool({ toolUseId: "c1", toolName: "Read" });
    const m = resolveMember(
      tool({
        toolName: "Agent",
        result: { isError: false, content: "Async agent launched. agentId: ag1" },
        asyncSource: {
          source_id: "ag1",
          kind: "agent",
          status: "running",
          stream: { transport: "poll", format: "jsonl-transcript" },
        },
      }),
      ctx({ children: [child] }),
    );
    expect(m?.bodies.map((b) => b.kind)).toEqual(["child-feed", "transcript"]);
  });

  it("maps a journal-format source to a journal body", () => {
    const m = resolveMember(
      tool({
        toolName: "Workflow",
        result: {
          isError: false,
          content:
            "Workflow started. Task ID: wf1. Transcript dir: /cfg/projects/-s/subagents/workflows/wf1",
        },
        asyncSource: {
          source_id: "wf1",
          kind: "workflow",
          status: "running",
          stream: { transport: "poll", format: "jsonl-journal" },
        },
        taskOutput: "{}",
      }),
      ctx(),
    );
    expect(m?.bodies.map((b) => b.kind)).toEqual(["journal"]);
  });

  it("gives an announcement-less tail a raw body", () => {
    const m = resolveMember(
      tool({ result: { isError: false, content: "(streaming)" }, taskOutput: "line" }),
      ctx(),
    );
    expect(m?.bodies).toEqual([{ kind: "raw", text: "line" }]);
  });

  it("settles an announcement-less member on its own result", () => {
    const m = resolveMember(
      tool({ result: { isError: true, content: "boom" }, taskOutput: "line" }),
      ctx(),
    );
    expect(m?.status).toBe("error");
  });

  it("keeps a child-feed-only member on its call's own result state", () => {
    const child = tool({ toolUseId: "c1", toolName: "Read" });
    const m = resolveMember(tool({ toolName: "Agent" }), ctx({ children: [child] }));
    expect(m?.status).toBe("running");
    expect(m?.bodies.map((b) => b.kind)).toEqual(["child-feed"]);
  });

  it("synthesizes a transcript body for a spawn announcement without classification", () => {
    const m = resolveMember(
      tool({
        toolName: "Agent",
        result: {
          isError: false,
          content: "Async agent launched. agentId: ag9, output_file: /tmp/claude-1/s/tasks/ag9.output",
        },
      }),
      ctx({ tails: { ag9: { text: "{}", offset: 2, done: false, elapsedMs: 7 } } }),
    );
    expect(m?.source?.kind).toBe("agent");
    expect(m?.bodies.map((b) => b.kind)).toEqual(["transcript"]);
  });

  it("prefers the poll's live elapsed over the frozen heartbeat", () => {
    const m = resolveMember(
      tool({
        result: SPAWN_RESULT,
        progressElapsedS: 2,
        asyncSource: { source_id: "bg1", kind: "shell", status: "running" },
      }),
      ctx({ tails: { bg1: { text: "x", offset: 1, done: false, elapsedMs: 9000 } } }),
    );
    expect(m?.elapsedMs).toBe(9000);
  });

  it("falls back to the frozen heartbeat elapsed when never polled", () => {
    const m = resolveMember(tool({ result: SPAWN_RESULT, progressElapsedS: 2 }), ctx());
    expect(m?.elapsedMs).toBe(2000);
  });

  it("settles on the transcript's own terminal record when no notification landed", () => {
    // Arrange — the stream says it ended; the notification never arrived.
    const tail = `{"type":"result","subtype":"success","is_error":false}`;
    const m = resolveMember(
      pollAgent(),
      ctx({ tails: { ag1: { text: tail, offset: 1, done: false, elapsedMs: 5 } } }),
    );
    // Assert
    expect(m?.status).toBe("done");
    expect(m?.settled).toBe(true);
  });

  it("reads a failing terminal record as an errored member", () => {
    // Arrange
    const tail = `{"type":"result","subtype":"error_during_execution"}`;
    const m = resolveMember(
      pollAgent(),
      ctx({ tails: { ag1: { text: tail, offset: 1, done: false, elapsedMs: 5 } } }),
    );
    // Assert
    expect(m?.status).toBe("error");
  });

  it("carries the transcript's token figure onto the member", () => {
    // Arrange
    const tail = `{"type":"assistant","message":{"usage":{"output_tokens":42},"content":[]}}`;
    const m = resolveMember(
      pollAgent(),
      ctx({ tails: { ag1: { text: tail, offset: 1, done: false, elapsedMs: 5 } } }),
    );
    // Assert
    expect(m?.outputTokens).toBe(42);
  });

  it("carries no token figure for a stream that is not a transcript", () => {
    // Arrange — a shell spool is bytes; tokens would be an invention.
    const m = resolveMember(
      tool({
        result: SPAWN_RESULT,
        taskOutput: "plain shell output",
        asyncSource: {
          source_id: "bg1",
          kind: "shell",
          status: "running",
          stream: { transport: "ws", format: "text" },
        },
      }),
      ctx(),
    );
    // Assert
    expect(m?.outputTokens).toBeUndefined();
  });
});

/** A detached agent spawn, poll-transport by classification. */
function pollAgent(opts: Partial<ToolItem> = {}): ToolItem {
  return tool({
    toolName: "Agent",
    result: { isError: false, content: "Async agent launched. agentId: ag1" },
    asyncSource: {
      source_id: "ag1",
      kind: "agent",
      status: "running",
      stream: { transport: "poll", format: "jsonl-transcript" },
    },
    ...opts,
  });
}

describe("livePollSourceIds", () => {
  it("includes a live poll-transport member's source id, fold state notwithstanding", () => {
    // Arrange
    const watchers = new Map([["b1", [pollAgent()]]]);
    // Act / Assert
    expect(livePollSourceIds(watchers, ctx())).toEqual(new Set(["ag1"]));
  });

  it("drops a member the moment it settles", () => {
    // Arrange — the completion notification landed; nothing left to poll for.
    const watchers = new Map([
      ["b1", [pollAgent({ notification: { status: "completed", text: "done" } })]],
    ]);
    // Act / Assert
    expect(livePollSourceIds(watchers, ctx()).size).toBe(0);
  });

  it("excludes a ws-transport member, whose tail already streams", () => {
    // Arrange
    const shell = tool({
      result: SPAWN_RESULT,
      asyncSource: {
        source_id: "bg1",
        kind: "shell",
        status: "running",
        stream: { transport: "ws", format: "text" },
      },
    });
    const watchers = new Map([["b1", [shell]]]);
    // Act / Assert
    expect(livePollSourceIds(watchers, ctx()).size).toBe(0);
  });

  it("dedups one member hosted on two bubbles to a single id", () => {
    // Arrange — the same spawn card can appear under two hosts (gns re-host).
    const watchers = new Map([
      ["b1", [pollAgent()]],
      ["b2", [pollAgent()]],
    ]);
    // Act / Assert
    expect(livePollSourceIds(watchers, ctx())).toEqual(new Set(["ag1"]));
  });
});

describe("a heartbeat alone (MEDIUM: the plain long-running tool)", () => {
  it("resolves a member for a tool with nothing detached but a heartbeat", () => {
    // Arrange — a slow Bash: no async source, no spawned task ids, no
    // children, no tail. Only the elapsed clock HeartbeatProgress feeds.
    const item = tool({ progressElapsedS: 7 });
    // Act
    const m = resolveMember(item, ctx());
    // Assert
    expect(m).not.toBeNull();
    expect(m?.elapsedMs).toBe(7000);
  });

  it("still resolves nothing for a tool with no heartbeat and nothing detached", () => {
    // Arrange / Act — the null case must stay null, or every plain tool call
    // grows a member it has nothing to put in.
    const m = resolveMember(tool(), ctx());
    // Assert
    expect(m).toBeNull();
  });

  it("keeps the polled elapsed ahead of the heartbeat's when both exist", () => {
    // Arrange — the poll is live, the heartbeat is a frozen fallback.
    const item = tool({
      result: SPAWN_RESULT,
      progressElapsedS: 2,
      asyncSource: { source_id: "bg1", kind: "shell", status: "running" },
    });
    // Act
    const m = resolveMember(
      item,
      ctx({ tails: { bg1: { text: "x", offset: 1, done: false, elapsedMs: 9000 } } }),
    );
    // Assert
    expect(m?.elapsedMs).toBe(9000);
  });
});
