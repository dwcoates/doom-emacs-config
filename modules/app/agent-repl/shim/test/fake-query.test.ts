import { describe, expect, it } from "vitest";
import { FAKE_MODELS, MARKDOWN_SHOWCASE, createFakeQuery } from "../src/fake-query.js";
import { AsyncQueue } from "../src/input-queue.js";
import {
  CanUseToolLike,
  PermissionResultLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../src/session.js";

function userMsg(text: string): SdkUserMessageLike {
  return {
    type: "user",
    message: { role: "user", content: [{ type: "text", text }] },
    parent_tool_use_id: null,
    session_id: "s",
  };
}

interface FakeHarness {
  input: AsyncQueue<SdkUserMessageLike>;
  collect: (n?: number) => Promise<SdkMessageLike[]>;
  query: ReturnType<typeof createFakeQuery>;
  permissionRequests: Array<{ toolName: string; input: Record<string, unknown> }>;
}

function makeFake(decision: PermissionResultLike = { behavior: "allow", updatedInput: {} }): FakeHarness {
  const input = new AsyncQueue<SdkUserMessageLike>();
  const permissionRequests: FakeHarness["permissionRequests"] = [];
  const canUseTool: CanUseToolLike = async (toolName, toolInput) => {
    permissionRequests.push({ toolName, input: toolInput });
    return decision;
  };
  let uuidCounter = 0;
  const query = createFakeQuery(input, canUseTool, {
    sessionId: "s",
    newUuid: () => `u${++uuidCounter}`,
  });
  const collect = async (): Promise<SdkMessageLike[]> => {
    const out: SdkMessageLike[] = [];
    for await (const m of query) out.push(m);
    return out;
  };
  return { input, collect, query, permissionRequests };
}

describe("createFakeQuery", () => {
  it("emits a system init message first", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.end();
    const msgs = await h.collect();
    // Assert
    expect(msgs[0]).toMatchObject({ type: "system", subtype: "init", model: "fake-model" });
  });

  it("reports the resumed session uuid in init when resuming", async () => {
    // Arrange — resume continuation mirrors verified real SDK behavior.
    const input = new AsyncQueue<SdkUserMessageLike>();
    const query = createFakeQuery(input, async () => ({ behavior: "allow", updatedInput: {} }), {
      sessionId: "s-new",
      newUuid: () => "u1",
      resume: "cli-uuid-resumed",
    });
    // Act
    input.end();
    const msgs: SdkMessageLike[] = [];
    for await (const m of query) msgs.push(m);
    // Assert — init carries the RESUMED uuid, not the shim-assigned id.
    expect(msgs[0]).toMatchObject({ type: "system", subtype: "init", session_id: "cli-uuid-resumed" });
  });

  it("streams an echoed text turn ending in a success result", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("hello"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const types = msgs.map((m) => m.type);
    expect(types).toContain("stream_event");
    expect(types).toContain("assistant");
    const result = msgs.find((m) => m.type === "result")!;
    expect(result).toMatchObject({ subtype: "success", result: expect.stringContaining("hello") });
  });

  it("asks canUseTool before running a !tool turn", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!tool ls -la"));
    h.input.end();
    await h.collect();
    // Assert
    expect(h.permissionRequests).toEqual([
      { toolName: "Bash", input: { command: "ls -la" } },
    ]);
  });

  it("emits a non-error tool_result user message when the tool is allowed", async () => {
    // Arrange
    const h = makeFake({ behavior: "allow", updatedInput: {} });
    // Act
    h.input.push(userMsg("!tool ls"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const toolResultMsg = msgs.find((m) => m.type === "user")!;
    const blocks = (toolResultMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(blocks[0]).toMatchObject({ type: "tool_result", is_error: false });
  });

  it("emits an error tool_result user message when the tool is denied", async () => {
    // Arrange
    const h = makeFake({ behavior: "deny", message: "not today" });
    // Act
    h.input.push(userMsg("!tool rm -rf /"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const toolResultMsg = msgs.find((m) => m.type === "user")!;
    const blocks = (toolResultMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(blocks[0]).toMatchObject({
      type: "tool_result",
      is_error: true,
      content: expect.stringContaining("not today"),
    });
  });

  it("replies with the markdown showcase on !md", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!md"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const result = msgs.find((m) => m.type === "result")!;
    expect(result.result).toBe(MARKDOWN_SHOWCASE);
  });

  it("reflects setPermissionMode in subsequent echoes", async () => {
    // Arrange
    const h = makeFake();
    // Act
    await h.query.setPermissionMode("plan");
    h.input.push(userMsg("check mode"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const result = msgs.find((m) => m.type === "result")!;
    expect(result.result).toContain("mode=plan");
  });

  it("ends the output stream when the input ends", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.end();
    const msgs = await h.collect();
    // Assert — collect() returning at all proves termination
    expect(msgs.length).toBeGreaterThan(0);
  });

  it("offers a supported-model menu", async () => {
    // Arrange
    const h = makeFake();
    // Act
    const models = await h.query.supportedModels();
    // Assert
    expect(models).toEqual(FAKE_MODELS);
  });

  it("reports the model set by setModel on subsequent assistant messages", async () => {
    // Arrange — this is the whole drift path in miniature: the model the
    // agent ANSWERS with is what the topbar mirrors.
    const h = makeFake();
    // Act
    await h.query.setModel("fake-model-fast");
    h.input.push(userMsg("hello"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const assistant = msgs.find((m) => m.type === "assistant")!;
    expect((assistant.message as { model: string }).model).toBe("fake-model-fast");
  });

  it("leaves the model at the default until setModel moves it", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("hello"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const assistant = msgs.find((m) => m.type === "assistant")!;
    expect((assistant.message as { model: string }).model).toBe("fake-model");
  });
});

describe("!bg background turns", () => {
  it("emits the tool_progress heartbeat for a backgrounded command", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!bg sleep 5"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const progress = msgs.find((m) => m.type === "tool_progress");
    expect(progress).toMatchObject({ tool_name: "Bash", elapsed_time_seconds: 1 });
  });

  it("announces the spawn with a task id and spool output path", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!bg sleep 5"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const resultMsg = msgs.find((m) => {
      if (m.type !== "user") return false;
      const blocks = (m.message as { content: Array<Record<string, unknown>> }).content;
      return blocks[0]?.type === "tool_result";
    })!;
    const blocks = (resultMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(String(blocks[0].content)).toMatch(/with ID: fakebg\d+\. Output is being written to: \/tmp\/claude-0\//);
  });

  it("completes the task with a harness-shaped notification text block", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!bg sleep 5"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const noteMsg = msgs.find((m) => {
      if (m.type !== "user") return false;
      const blocks = (m.message as { content: Array<Record<string, unknown>> }).content;
      return blocks[0]?.type === "text";
    })!;
    const blocks = (noteMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(String(blocks[0].text)).toContain("<task-notification>");
    expect(String(blocks[0].text)).toContain("<status>completed</status>");
  });
});
