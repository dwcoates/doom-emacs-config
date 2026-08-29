/**
 * The fake daemon's OWN suite: the one file here that exercises the fake with
 * a plain generated client and no app at all.
 *
 * Everything else in this directory trusts the fake to be a faithful daemon.
 * That trust has to be earned somewhere, and this is where: every rpc answers,
 * streams push, tokens are minted and refused, unknown fields survive the
 * wire, scripted errors arrive as errors, and a stream can die without a
 * terminal frame. A failure here means the harness is lying, not that the app
 * is wrong — which is exactly why it is separated from the app-facing suites.
 */
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { createClient, type Client, ConnectError, Code } from "@connectrpc/connect";
import { createGrpcWebTransport } from "@connectrpc/connect-node";

import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { create } from "@bufbuild/protobuf";
import { SubmitPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { createFakeDaemon, ROOT_FEED, type FakeDaemon } from "./fake-daemon";
import {
  WEBAPP_ORIGIN,
  feedPageSuccess,
  feedId,
  footerView,
  holdTray,
  heldPromptItem,
  responseRow,
  roster,
  rosterRow,
  topbarView,
  userPromptRow,
  userSaid,
  workspaceRef,
  drainReason,
  WORKSPACE_ID,
} from "./fixtures";

let fake: FakeDaemon;
let client: Client<typeof AgentRepl>;

beforeEach(async () => {
  fake = createFakeDaemon();
  const { baseUrl } = await fake.start();
  client = createClient(
    AgentRepl,
    createGrpcWebTransport({ baseUrl, httpVersion: "1.1" }),
  );
});

afterEach(async () => {
  await fake.stop();
});

/** Read `count` messages off a server stream, then drop the client's leg. */
async function take<T>(stream: AsyncIterable<T>, count: number): Promise<T[]> {
  const got: T[] = [];
  for await (const message of stream) {
    got.push(message);
    if (got.length >= count) break;
  }
  return got;
}

describe("every rpc answers", () => {
  it("answers a unary rpc with its success arm", async () => {
    // Arrange / Act
    const response = await client.daemonHealth({});
    // Assert
    expect(response.result.case).toBe("success");
  });

  it("answers every unary rpc in the service with a success arm", async () => {
    // Arrange: each unary rpc with a minimally addressed request.
    const workspace = workspaceRef();
    const calls: Array<[string, Promise<{ result: { case?: string } }>]> = [
      ["submitPrompt", client.submitPrompt({ said: userSaid(), idempotencyKey: "k1", origin: WEBAPP_ORIGIN })],
      ["openFeed", client.openFeed({ workspace })],
      ["getFeedPage", client.getFeedPage({ workspace, page: { case: "first", value: {} } })],
      ["interrupt", client.interrupt({ workspace, target: { case: "turn", value: {} } })],
      ["answerPermission", client.answerPermission({ workspace, permission: feedId("p"), answer: { case: "allowOnce", value: {} } })],
      ["answerQuestion", client.answerQuestion({ workspace, question: feedId("q"), answers: [] })],
      ["answerColdGate", client.answerColdGate({ workspace, gate: feedId("g"), choice: { case: "pay", value: {} } })],
      ["createWorkspace", client.createWorkspace({ form: { case: "standard", value: {} } })],
      ["openWorkspace", client.openWorkspace({ workspace })],
      ["closeWorkspace", client.closeWorkspace({ workspace })],
      ["killWorkspace", client.killWorkspace({ workspace })],
      ["nukeWorkspace", client.nukeWorkspace({ workspace })],
      ["mergeWorkspace", client.mergeWorkspace({ workspace })],
      ["restartWorkspace", client.restartWorkspace({ workspace, force: false })],
      ["setWorkspacePriority", client.setWorkspacePriority({ workspace })],
      ["createTask", client.createTask({ title: "t" })],
      ["updateTask", client.updateTask({ task: { id: "t" }, change: { case: "setDone", value: {} } })],
      ["assignWorkspaceTask", client.assignWorkspaceTask({ workspace })],
      ["setModel", client.setModel({ workspace })],
      ["setPermissionMode", client.setPermissionMode({ workspace, mode: "default" })],
      ["updateHeldPrompt", client.updateHeldPrompt({ workspace, action: { case: "release", value: {} } })],
      ["answerHeldOffer", client.answerHeldOffer({ workspace })],
      ["updateShutdownSchedule", client.updateShutdownSchedule({ action: { case: "cancel", value: {} } })],
      ["updateMergeQueue", client.updateMergeQueue({ action: { case: "pause", value: {} } })],
      ["daemonHealth", client.daemonHealth({})],
      ["sessionHealth", client.sessionHealth({ workspace })],
      ["clientLog", client.clientLog({ workspace })],
      ["registerWorkspace", client.registerWorkspace({ dir: "/tmp/x" })],
      ["selectWorkspace", client.selectWorkspace({ workspace })],
      ["adoptHostWorkspace", client.adoptHostWorkspace({ workspace })],
      ["adoptWebWorkspace", client.adoptWebWorkspace({ workspace })],
      ["openLogin", client.openLogin({ workspace })],
      ["sendLoginInput", client.sendLoginInput({ workspace, input: { case: "resize", value: { rows: 24, cols: 80 } } })],
      ["closeLogin", client.closeLogin({ workspace })],
      ["openExternal", client.openExternal({ workspace, url: "https://example.test" })],
      ["openInEditor", client.openInEditor({ workspace, path: "/repo/a.ts" })],
      ["requestCommandSupport", client.requestCommandSupport({ workspace, command: "/agents" })],
    ];
    // Act
    const settled = await Promise.all(calls.map(async ([name, p]) => [name, (await p).result.case] as const));
    // Assert: every one carries `success`, none carries `error` or nothing.
    expect(Object.fromEntries(settled)).toEqual(
      Object.fromEntries(calls.map(([name]) => [name, "success"])),
    );
  });
});

describe("scripted answers", () => {
  it("serves the scripted response in place of the default", async () => {
    // Arrange
    fake.answer(
      "submitPrompt",
      create(SubmitPromptResponseSchema, {
        result: { case: "error", value: { reason: { case: "merging", value: {} } } },
      }),
    );
    // Act
    const response = await client.submitPrompt({ said: userSaid(), idempotencyKey: "k", origin: WEBAPP_ORIGIN });
    // Assert
    expect(response.result.case).toBe("error");
  });

  it("serves a scripted failure as a transport error for one call only", async () => {
    // Arrange
    fake.failNext("daemonHealth", "the daemon is down");
    // Act
    const failed = await client.daemonHealth({}).catch((e: unknown) => e);
    const recovered = await client.daemonHealth({});
    // Assert
    expect(ConnectError.from(failed).message).toContain("the daemon is down");
    expect(recovered.result.case).toBe("success");
  });

  it("refuses a submission whose origin is unspecified", async () => {
    // Arrange / Act: `origin` omitted entirely, which proto3 sends as 0.
    const failed = await client
      .submitPrompt({ said: userSaid(), idempotencyKey: "k" })
      .catch((e: unknown) => e);
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.InvalidArgument);
  });
});

describe("unknown-field injection", () => {
  it("serializes an injected unknown field on a unary response", async () => {
    // Arrange
    fake.injectUnknown("daemonHealth");
    // Act
    const response = await client.daemonHealth({});
    // Assert: protobuf-es round-trips it into the decoded message's $unknown.
    expect(response.$unknown).toEqual([
      expect.objectContaining({ no: 999, data: new Uint8Array([1]) }),
    ]);
  });

  it("serializes an injected unknown field on a stream push", async () => {
    // Arrange
    fake.injectUnknown("watchFooter");
    fake.setFooter(WORKSPACE_ID, footerView());
    // Act
    const [push] = await take(client.watchFooter({ workspace: workspaceRef() }), 1);
    // Assert
    expect(push.$unknown).toEqual([
      expect.objectContaining({ no: 999, data: new Uint8Array([1]) }),
    ]);
  });

  it("leaves the following response clean", async () => {
    // Arrange
    fake.injectUnknown("daemonHealth");
    // Act
    await client.daemonHealth({});
    const clean = await client.daemonHealth({});
    // Assert
    expect(clean.$unknown ?? []).toEqual([]);
  });
});

describe("view streams", () => {
  it("pushes the scripted footer on open", async () => {
    // Arrange
    fake.setFooter(WORKSPACE_ID, footerView({ status: "merging", substatus: "queued" }));
    // Act
    const [push] = await take(client.watchFooter({ workspace: workspaceRef() }), 1);
    // Assert
    expect(push.footer?.strip?.status?.status.case).toBe("merging");
  });

  it("pushes the scripted topbar on open", async () => {
    // Arrange
    fake.setTopbar(WORKSPACE_ID, topbarView({ title: "the port" }));
    // Act
    const [push] = await take(client.watchTopbar({ workspace: workspaceRef() }), 1);
    // Assert
    expect(push.topbar?.title?.text).toBe("the port");
  });

  it("pushes the scripted roster on the global stream", async () => {
    // Arrange
    fake.setRoster(roster({ rows: [rosterRow({ name: "ported" })] }));
    // Act
    const [push] = await take(client.watchWorkspaceRoster({}), 1);
    // Assert
    expect(push.roster?.repository?.sections[0]?.rows?.rows[0]?.name?.text).toBe("ported");
  });

  it("pushes the scripted tray", async () => {
    // Arrange
    fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem({ text: "held one" })] }));
    // Act
    const [push] = await take(client.watchDaemonHolds({ workspace: workspaceRef() }), 1);
    // Assert
    expect(push.tray?.items).toHaveLength(1);
  });

  it("delivers a later setter to an already-open stream", async () => {
    // Arrange
    const stream = client.watchFooter({ workspace: workspaceRef() });
    const reader = take(stream, 2);
    await fake.awaitStream("watchFooter");
    // Act
    fake.setFooter(WORKSPACE_ID, footerView({ status: "blocked", substatus: "auth" }));
    // Assert
    const pushes = await reader;
    expect(pushes[1].footer?.strip?.status?.status.case).toBe("blocked");
  });
});

describe("the feed universe", () => {
  it("answers OpenFeed with the scripted page and a minted token", async () => {
    // Arrange
    fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("hi")]));
    // Act
    const response = await client.openFeed({ workspace: workspaceRef() });
    // Assert
    const success = response.result.case === "success" ? response.result.value : undefined;
    expect(success?.watch?.value).toBe(fake.mintedTokens(WORKSPACE_ID, ROOT_FEED)[0]);
  });

  it("mints a distinct token per open", async () => {
    // Arrange / Act
    await client.openFeed({ workspace: workspaceRef() });
    await client.openFeed({ workspace: workspaceRef() });
    // Assert
    const [first, second] = fake.mintedTokens(WORKSPACE_ID, ROOT_FEED);
    expect(first).not.toBe(second);
  });

  it("refuses WatchFeed for a token it never minted", async () => {
    // Arrange / Act
    const failed = await take(client.watchFeed({ watch: { value: "forged" } }), 1).catch(
      (e: unknown) => e,
    );
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.NotFound);
  });

  it("delivers a pushed row on the tail of the feed its token names", async () => {
    // Arrange
    const opened = await client.openFeed({ workspace: workspaceRef() });
    const token = opened.result.case === "success" ? opened.result.value.watch : undefined;
    const reader = take(client.watchFeed({ watch: token }), 1);
    await fake.awaitStream("watchFeed");
    // Act
    fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "done"));
    // Assert
    const [push] = await reader;
    expect(push.row?.row.case).toBe("activity");
  });

  it("does not deliver a sub-feed's row to the root feed's tail", async () => {
    // Arrange: open the root, then push onto a different feed.
    const opened = await client.openFeed({ workspace: workspaceRef() });
    const token = opened.result.case === "success" ? opened.result.value.watch : undefined;
    const pushes: unknown[] = [];
    const stream = client.watchFeed({ watch: token });
    const reading = (async () => {
      for await (const p of stream) pushes.push(p);
    })();
    await fake.awaitStream("watchFeed");
    // Act
    fake.pushRow(WORKSPACE_ID, "bubble-1", responseRow("success"));
    fake.endStream("watchFeed");
    await reading;
    // Assert
    expect(pushes).toHaveLength(0);
  });

  it("answers GetFeedPage `next` with the next page when one is scripted", async () => {
    // Arrange
    fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("newest")]));
    fake.setNextPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("older")]));
    // Act
    const response = await client.getFeedPage({
      workspace: workspaceRef(),
      page: { case: "next", value: {} },
    });
    // Assert
    const page = response.result.case === "success" ? response.result.value : undefined;
    const rows = page?.result.case === "success" ? page.result.value.rows : [];
    const prompt = rows[0]?.row.case === "userPrompt" ? rows[0].row.value : undefined;
    const body = prompt?.result.case === "success" ? prompt.result.value.body : undefined;
    const block = body?.blocks[0]?.block;
    expect(block?.case === "text" ? block.value.text : undefined).toBe("older");
  });
});

describe("web-link and daemon pushes", () => {
  it("pushes a transfer with the new address", async () => {
    // Arrange
    const reader = take(client.watchWebWorkspace({ workspace: workspaceRef() }), 1);
    await fake.awaitStream("watchWebWorkspace");
    // Act
    fake.transfer(WORKSPACE_ID, "http://127.0.0.1:9999");
    // Assert
    const [push] = await reader;
    expect(push.push.case === "transferred" ? push.push.value.address : undefined).toBe(
      "http://127.0.0.1:9999",
    );
  });

  it("pushes a scheduled drain with its reason arm", async () => {
    // Arrange
    const reader = take(client.watchDaemon({}), 1);
    await fake.awaitStream("watchDaemon");
    // Act
    fake.scheduleDrain(60_000n, drainReason("maintenance"));
    // Assert
    const [push] = await reader;
    const scheduled = push.push.case === "drainScheduled" ? push.push.value : undefined;
    expect(scheduled?.reason?.kind.case).toBe("maintenance");
  });

  it("pushes a drain cancellation", async () => {
    // Arrange
    const reader = take(client.watchDaemon({}), 1);
    await fake.awaitStream("watchDaemon");
    // Act
    fake.cancelDrain();
    // Assert
    const [push] = await reader;
    expect(push.push.case).toBe("drainCancelled");
  });

  it("pushes a shutdown announcement carrying its outage window", async () => {
    // Arrange
    const reader = take(client.watchDaemon({}), 1);
    await fake.awaitStream("watchDaemon");
    // Act
    fake.announceShutdown({ expectedOutageMs: 4_000n, mintedAtMs: 1_000n });
    // Assert
    const [push] = await reader;
    const announced = push.push.case === "shutdownAnnounced" ? push.push.value : undefined;
    expect(announced?.expectedOutageMs).toBe(4_000n);
  });
});

describe("the login pty", () => {
  it("replays the scrollback before any live byte", async () => {
    // Arrange
    fake.setLoginScrollback(WORKSPACE_ID, [new Uint8Array([1, 2]), new Uint8Array([3])]);
    // Act
    const pushes = await take(client.watchLoginTerminal({ workspace: workspaceRef() }), 2);
    // Assert
    expect(pushes.map((p) => (p.output.case === "bytes" ? [...p.output.value.data] : []))).toEqual([
      [1, 2],
      [3],
    ]);
  });

  it("concludes the stream with the closed frame", async () => {
    // Arrange
    const reader = take(client.watchLoginTerminal({ workspace: workspaceRef() }), 1);
    await fake.awaitStream("watchLoginTerminal");
    // Act
    fake.closeLoginTerminal(WORKSPACE_ID);
    // Assert
    const [push] = await reader;
    expect(push.output.case).toBe("closed");
  });

  it("records a keystroke input as its own arm", async () => {
    // Arrange / Act
    await client.sendLoginInput({
      workspace: workspaceRef(),
      input: { case: "keystrokes", value: { data: new Uint8Array([13]) } },
    });
    // Assert
    const [request] = fake.calls<{ input: { case?: string } }>("sendLoginInput");
    expect(request.input.case).toBe("keystrokes");
  });
});

describe("bookkeeping", () => {
  it("counts a live stream and drops the count when the client cancels", async () => {
    // Arrange
    const controller = new AbortController();
    const stream = client.watchFooter({ workspace: workspaceRef() }, { signal: controller.signal });
    const reading = (async () => {
      try {
        for await (const _ of stream) break;
      } catch {
        // the cancellation surfaces here; the count is what this asserts
      }
    })();
    await fake.awaitStream("watchFooter");
    const whileOpen = fake.liveStreams("watchFooter", WORKSPACE_ID);
    // Act
    controller.abort();
    await reading;
    // Assert
    expect(whileOpen).toBe(1);
  });

  it("ends a stream without a terminal frame when the transport dies", async () => {
    // Arrange
    const pushes: unknown[] = [];
    const stream = client.watchFooter({ workspace: workspaceRef() });
    const reading = (async () => {
      for await (const p of stream) pushes.push(p);
    })();
    await fake.awaitStream("watchFooter");
    // Act
    fake.endStream("watchFooter");
    await reading;
    // Assert: only the open push arrived; nothing announced the end.
    expect(pushes).toHaveLength(1);
  });

  it("records each call against its rpc in arrival order", async () => {
    // Arrange / Act
    await client.selectWorkspace({ workspace: workspaceRef("ws-a") });
    await client.selectWorkspace({ workspace: workspaceRef("ws-b") });
    // Assert
    const seen = fake.calls<{ workspace?: { id: string } }>("selectWorkspace");
    expect(seen.map((r) => r.workspace?.id)).toEqual(["ws-a", "ws-b"]);
  });

  it("resolves nextCall with a request that has not arrived yet", async () => {
    // Arrange
    const pending = fake.nextCall<{ command: string }>("requestCommandSupport");
    // Act
    await client.requestCommandSupport({ workspace: workspaceRef(), command: "/agents" });
    // Assert
    expect((await pending).command).toBe("/agents");
  });

  it("hands successive nextCall waiters successive calls", async () => {
    // Arrange
    const first = fake.nextCall<{ command: string }>("requestCommandSupport");
    const second = fake.nextCall<{ command: string }>("requestCommandSupport");
    // Act
    await client.requestCommandSupport({ workspace: workspaceRef(), command: "/one" });
    await client.requestCommandSupport({ workspace: workspaceRef(), command: "/two" });
    // Assert
    expect([(await first).command, (await second).command]).toEqual(["/one", "/two"]);
  });
});
