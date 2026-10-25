// @vitest-environment jsdom
/** A verb whose answer is the next view (src/rpc/gesture.ts). */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { afterEach, describe, expect, it, vi } from "vitest";
import { filesUnder } from "../files-under.js";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { ShowChatNodeResponseSchema, type ShowChatNodeResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_chat_node_pb";
import { log } from "../../src/log.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { sendGesture } from "../../src/rpc/gesture.js";
import { NOW, SINK, WORKSPACE, fakeTicker } from "../sidebar/harness.js";
import { testAppContext } from "./app-context.js";

/** Send ShowChatNode to a daemon answering ANSWER. */
function send(answer: () => ShowChatNodeResponse): Promise<string | null> {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, { showChatNode: answer });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: fakeTicker(NOW),
    failures: SINK,
    composerEnabled: false,
  });
  return sendGesture(ctx, "ShowChatNode", "test.gesture-refused", (client) => client.showChatNode({ thread: WORKSPACE, node: { id: "n1" } }), ShowChatNodeResponseSchema);
}

describe("sendGesture", () => {
  afterEach(() => vi.restoreAllMocks());

  it("answers null for a success", async () => {
    await expect(send(() => create(ShowChatNodeResponseSchema, { result: { case: "success", value: {} } }))).resolves.toBeNull();
  });

  it("answers a refusal's arm, recorded at info under the site's operation", async () => {
    const infos: unknown[] = [];
    vi.spyOn(log, "info").mockImplementation((_message, fields) => void infos.push(fields?.operation));

    const arm = await send(() =>
      create(ShowChatNodeResponseSchema, { result: { case: "error", value: { cause: { case: "unknownNode", value: {} } } } }),
    );

    expect([arm, infos]).toEqual(["unknownNode", ["test.gesture-refused"]]);
  });

  it("answers null for a call that failed in transport", async () => {
    vi.spyOn(log, "warn").mockImplementation(() => undefined);
    vi.spyOn(log, "error").mockImplementation(() => undefined);
    await expect(
      send(() => {
        throw new ConnectError("down", Code.Unavailable);
      }),
    ).resolves.toBeNull();
  });
});

describe("the gesture sites", () => {
  it("note a refused arm only through sendGesture, but for the chat search that reads its success", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../../src");
    const noting = filesUnder(src, [".ts", ".tsx"])
      .filter((file) => /noteRefusedArm\(/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file))
      .sort();
    expect(noting).toEqual(["rpc/gesture.ts", "rpc/refusal.ts", "sidebar/chat-search.ts"]);
  });
});
