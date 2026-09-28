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
import { createGrpcWebTransport, createConnectTransport } from "@connectrpc/connect-node";

import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { create } from "@bufbuild/protobuf";
import { SubmitPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { createFakeDaemon, ROOT_FEED, REFUSAL_FACTS, type FakeDaemon } from "./fake-daemon";
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
  daemonUnhealthy,
  sessionUnhealthy,
  WORKSPACE_ID,
} from "./fixtures";

let fake: FakeDaemon;
let client: Client<typeof AgentRepl>;

beforeEach(async () => {
  fake = createFakeDaemon();
  const { baseUrl, socketPath } = await fake.start();
  // `baseUrl` is the daemon's identity and resolves nowhere; `socketPath` is
  // where it actually accepts. connect-node hands `nodeOptions` straight to
  // http.request, so this is the same dial the harness makes through undici.
  client = createClient(
    AgentRepl,
    createGrpcWebTransport({ baseUrl, httpVersion: "1.1", nodeOptions: { socketPath } }),
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
      ["submitPrompt", client.submitPrompt({ workspace, said: userSaid(), idempotencyKey: "k1", origin: WEBAPP_ORIGIN })],
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
    const response = await client.submitPrompt({
      workspace: workspaceRef(),
      said: userSaid(),
      idempotencyKey: "k",
      origin: WEBAPP_ORIGIN,
    });
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
      .submitPrompt({ workspace: workspaceRef(), said: userSaid(), idempotencyKey: "k" })
      .catch((e: unknown) => e);
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.InvalidArgument);
  });

  it("refuses a submission carrying no workspace", async () => {
    // Arrange / Act: `workspace` omitted, which proto3 sends as absent.
    const failed = await client
      .submitPrompt({ said: userSaid(), idempotencyKey: "k", origin: WEBAPP_ORIGIN })
      .catch((e: unknown) => e);
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.InvalidArgument);
  });

  it("refuses a submission addressing another workspace's feed", async () => {
    // Arrange: the feed becomes known as ws-other's.
    fake.setPage("ws-other", "bubble-x", feedPageSuccess([]));
    // Act
    const failed = await client
      .submitPrompt({
        workspace: workspaceRef(WORKSPACE_ID),
        said: userSaid(),
        idempotencyKey: "k",
        origin: WEBAPP_ORIGIN,
        feed: feedId("bubble-x"),
      })
      .catch((e: unknown) => e);
    // Assert
    expect(ConnectError.from(failed).code).toBe(Code.InvalidArgument);
  });

  it("accepts a submission addressing its own workspace's feed", async () => {
    // Arrange
    fake.setPage(WORKSPACE_ID, "bubble-mine", feedPageSuccess([]));
    // Act
    const response = await client.submitPrompt({
      workspace: workspaceRef(WORKSPACE_ID),
      said: userSaid(),
      idempotencyKey: "k",
      origin: WEBAPP_ORIGIN,
      feed: feedId("bubble-mine"),
    });
    // Assert
    expect(response.result.case).toBe("success");
  });
});

describe("unset-field injection", () => {
  it("pushes a feed row with no row arm", async () => {
    // Arrange
    fake.injectUnsetField("watchFeed");
    const open = await client.openFeed({ workspace: workspaceRef() });
    const watch = open.result.case === "success" ? open.result.value.watch : undefined;
    // Act
    const reader = take(client.watchFeed({ watch }), 1);
    await fake.awaitStream("watchFeed");
    fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("poisoned"));
    const [push] = await reader;
    // Assert
    expect(push.row?.row.case).toBeUndefined();
  });

  it("pushes a footer strip with no status", async () => {
    // Arrange
    fake.injectUnsetField("watchFooter");
    fake.setFooter(WORKSPACE_ID, footerView());
    // Act
    const [push] = await take(client.watchFooter({ workspace: workspaceRef() }), 1);
    // Assert
    expect(push.footer?.strip?.status).toBeUndefined();
  });

  it("pushes a roster row with no status", async () => {
    // Arrange
    fake.injectUnsetField("watchWorkspaceRoster");
    fake.setRoster(roster({ rows: [rosterRow()] }));
    // Act
    const [push] = await take(client.watchWorkspaceRoster({}), 1);
    // Assert
    const row = (push.push.case === "roster" ? push.push.value : undefined)?.repository?.sections[0]?.rows?.rows[0];
    expect(row?.status.case).toBeUndefined();
  });

  it("leaves the stored view intact for the following push", async () => {
    // Arrange: the strip copies rather than mutates, so the SECOND push of the
    // same view is the healthy original.
    fake.injectUnsetField("watchFooter");
    fake.setFooter(WORKSPACE_ID, footerView());
    // Act
    const [push] = await take(client.watchFooter({ workspace: workspaceRef() }), 1);
    // Assert
    expect(push.footer?.strip?.status).toBeUndefined();
    const [again] = await take(client.watchFooter({ workspace: workspaceRef() }), 1);
    expect(again.footer?.strip?.status?.status.case).toBe("thinking");
  });

  it("names the path it strips", () => {
    // Arrange / Act / Assert
    expect(fake.unsetFieldPath("watchFeed")).toBe("FeedRow.row");
  });

  it("refuses an rpc it has no stripper for", () => {
    // Arrange / Act / Assert: a silent healthy frame would pass a test that
    // asserted the client refused a broken one.
    expect(() => fake.injectUnsetField("daemonHealth")).toThrow(/no stripper/);
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
    expect((push.push.case === "roster" ? push.push.value : undefined)?.repository?.sections[0]?.rows?.rows[0]?.name?.text).toBe("ported");
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

describe("headers flush on accept", () => {
  /**
   * A standing stream may push nothing for a long time, so "accepted" and "not
   * yet connected" must not look alike on the client. These run over BOTH
   * protocols because the app dials with the Connect binary transport while
   * this suite's own client uses grpc-web, and the fake writes the head itself
   * rather than waiting for the adapter's lazy one.
   */
  const PROTOCOLS = [
    { name: "connect binary", make: (baseUrl: string, socketPath: string) => createConnectTransport({ baseUrl, httpVersion: "1.1" as const, useBinaryFormat: true, nodeOptions: { socketPath } }) },
    { name: "grpc-web", make: (baseUrl: string, socketPath: string) => createGrpcWebTransport({ baseUrl, httpVersion: "1.1" as const, nodeOptions: { socketPath } }) },
  ];

  it.each(PROTOCOLS)("resolves the response head over $name with no pushes", async ({ make }) => {
    // Arrange: WatchDaemon pushes nothing on open, so only the head can arrive.
    const typed = createClient(AgentRepl, make(fake.baseUrl, fake.socketPath));
    let sawHeader = false;
    let resolveHeaderSeen: () => void;
    const headerSeen = new Promise<void>((resolve) => { resolveHeaderSeen = resolve; });
    const stream = typed.watchDaemon({}, { onHeader: () => { sawHeader = true; resolveHeaderSeen(); } });
    const reading = (async () => {
      try {
        for await (const _ of stream) break;
      } catch {
        // the stream is ended below; the head is what this asserts
      }
    })();
    // Act: wait on the header itself arriving, not on a fixed delay.
    await fake.awaitStream("watchDaemon");
    await headerSeen;
    // Assert
    expect(sawHeader).toBe(true);
    fake.endStream("watchDaemon");
    await reading;
  });

  it("registers the stream before any frame is pushed", async () => {
    // Arrange / Act
    const stream = client.watchDaemon({});
    const reading = (async () => {
      try {
        for await (const _ of stream) break;
      } catch {
        // ended below
      }
    })();
    await fake.awaitStream("watchDaemon");
    // Assert: acceptance is observable with nothing yet pushed.
    expect(fake.liveStreams("watchDaemon")).toBe(1);
    fake.endStream("watchDaemon");
    await reading;
  });

  it("still delivers the first frame after the early head", async () => {
    // Arrange
    const reader = take(client.watchDaemon({}), 1);
    await fake.awaitStream("watchDaemon");
    // Act
    fake.cancelDrain();
    // Assert: writing the head early must not swallow the frames after it.
    const [push] = await reader;
    expect(push.push.case).toBe("drainCancelled");
  });

  it("drops the stream when the client aborts its request", async () => {
    // Arrange
    const controller = new AbortController();
    const stream = client.watchDaemon({}, { signal: controller.signal });
    const reading = (async () => {
      try {
        for await (const _ of stream) break;
      } catch {
        // the abort surfaces here
      }
    })();
    await fake.awaitStream("watchDaemon");
    // Act: a client ends a watch ONLY by aborting; nothing terminal is sent.
    controller.abort();
    await reading;
    // Wait on the fake's own deregistration signal; the 500ms bound is a
    // failure backstop only, so a real regression fails fast with a clear
    // message instead of hanging on the suite's default test timeout.
    await Promise.race([
      fake.awaitStreamClosed("watchDaemon"),
      new Promise((_, reject) =>
        setTimeout(
          () => reject(new Error("watchDaemon stream did not drop within 500ms of the client's abort")),
          500,
        ),
      ),
    ]);
    // Assert
    expect(fake.liveStreams("watchDaemon")).toBe(0);
  });
});

describe("typed refusals", () => {
  /** Every per-workspace rpc declares the four cross-cutting arms. */
  const CROSS_CUTTING = ["unknownWorkspace", "workspaceRefMismatch", "transferringAway", "notYetAdopted"];

  it("reads an rpc's arms off its error descriptor", () => {
    // Assert
    expect(fake.refusalArms("setModel")).toEqual(expect.arrayContaining(CROSS_CUTTING));
  });

  it("reads the arm oneof even when it is not spelled `cause`", () => {
    // Assert: Interrupt spells it `kind`, SubmitPrompt spells it `reason`.
    expect(fake.refusalArms("interrupt")).toContain("confirmRequired");
    expect(fake.refusalArms("submitPrompt")).toContain("merging");
  });

  it("serves the scripted arm on the error result", async () => {
    // Arrange
    fake.refuse("setModel", "notInCatalog");
    // Act
    const response = await client.setModel({ workspace: workspaceRef() });
    // Assert
    const error = response.result.case === "error" ? response.result.value : undefined;
    expect(error?.cause.case).toBe("notInCatalog");
  });

  it("builds the ref-mismatch arm complete, carrying its registry dir", async () => {
    // Arrange
    fake.refuse("setModel", "workspaceRefMismatch");
    // Act
    const response = await client.setModel({ workspace: workspaceRef() });
    // Assert
    const error = response.result.case === "error" ? response.result.value : undefined;
    const arm = error?.cause.case === "workspaceRefMismatch" ? error.cause.value : undefined;
    expect(arm?.registryDir).toBe(REFUSAL_FACTS.registryDir);
  });

  it("builds the transferring-away arm complete, carrying its address", async () => {
    // Arrange
    fake.refuse("setModel", "transferringAway");
    // Act
    const response = await client.setModel({ workspace: workspaceRef() });
    // Assert
    const error = response.result.case === "error" ? response.result.value : undefined;
    const arm = error?.cause.case === "transferringAway" ? error.cause.value : undefined;
    expect(arm?.address).toBe(REFUSAL_FACTS.address);
  });

  it("serves a refusal on the rpc whose oneof is spelled `reason`", async () => {
    // Arrange
    fake.refuse("submitPrompt", "duplicateSubmission");
    // Act
    const response = await client.submitPrompt({
      workspace: workspaceRef(),
      said: userSaid(),
      idempotencyKey: "k",
      origin: WEBAPP_ORIGIN,
    });
    // Assert
    const error = response.result.case === "error" ? response.result.value : undefined;
    expect(error?.reason.case).toBe("duplicateSubmission");
  });

  it("refuses to script an arm the schema does not declare", () => {
    // Act / Assert: a typo must fail loudly rather than serve an empty error.
    expect(() => fake.refuse("setModel", "notAnArm")).toThrow(/has no arm/);
  });

  it("serves every declared arm of every rpc it is asked for", async () => {
    // Arrange: the whole cross-cutting set on a representative rpc.
    const served: string[] = [];
    for (const arm of CROSS_CUTTING) {
      fake.refuse("closeWorkspace", arm);
      const response = await client.closeWorkspace({ workspace: workspaceRef() });
      if (response.result.case === "error") served.push(response.result.value.cause.case ?? "");
    }
    // Assert
    expect(served).toEqual(CROSS_CUTTING);
  });
});

describe("typed faults", () => {
  it("answers unhealthy on the SUCCESS arm", async () => {
    // Arrange
    fake.answer("daemonHealth", daemonUnhealthy(["logSinkPoisoned"]));
    // Act
    const response = await client.daemonHealth({});
    // Assert: unhealthy is an answer, not an error.
    expect(response.result.case).toBe("success");
  });

  it("carries the fault's typed kind", async () => {
    // Arrange
    fake.answer("daemonHealth", daemonUnhealthy(["logSinkPoisoned"]));
    // Act
    const response = await client.daemonHealth({});
    // Assert
    const health = response.result.case === "success" ? response.result.value.health : undefined;
    const faults = health?.case === "unhealthy" ? health.value.faults : [];
    expect(faults[0]?.kind.case).toBe("logSinkPoisoned");
  });

  it("carries a session fault's typed kind", async () => {
    // Arrange
    fake.answer("sessionHealth", sessionUnhealthy(["shimDied"]));
    // Act
    const response = await client.sessionHealth({ workspace: workspaceRef() });
    // Assert
    const health = response.result.case === "success" ? response.result.value.health : undefined;
    const faults = health?.case === "unhealthy" ? health.value.faults : [];
    expect(faults[0]?.kind.case).toBe("shimDied");
  });
});

/**
 * THE LISTENER'S OWN CONTRACT — the two facts that make the boot-time
 * `AdoptWebWorkspace refused (transport): fetch failed` flake unrepresentable
 * rather than merely rare.
 *
 * The flake was a genuine `connect(2)` failure: every fake daemon used to bind
 * a fresh 127.0.0.1 port, ~1600 of them per run with several standing streams
 * each, and a few concurrent runs walked macOS's 49152-65535 ephemeral range
 * dry until `connect` answered EADDRNOTAVAIL. Undici reports that to the page
 * as a bare "fetch failed", the boot adoption is the first call to make, and it
 * fails there. Neither half of that mechanism can recur if the daemon consumes
 * no port and is accepting before `start()` resolves.
 */
describe("the listener's contract", () => {
  it("is already accepting when start() resolves", async () => {
    // Arrange: a daemon of its own, dialed on the very first tick after start.
    const fresh = createFakeDaemon();
    const { baseUrl, socketPath } = await fresh.start();
    const freshClient = createClient(
      AgentRepl,
      createGrpcWebTransport({ baseUrl, httpVersion: "1.1", nodeOptions: { socketPath } }),
    );
    try {
      // Act: no settle, no yield, no retry — the first dial must land.
      const response = await freshClient.daemonHealth({});
      // Assert
      expect(response.result.case).toBe("success");
    } finally {
      await fresh.stop();
    }
  });

  it("consumes no tcp port", async () => {
    // Arrange
    const fresh = createFakeDaemon();
    await fresh.start();
    try {
      // Act: what the listener actually bound.
      const bound = fresh.socketPath;
      // Assert: a filesystem path, and the identity url resolves nowhere.
      expect(bound.endsWith(".sock")).toBe(true);
      expect(new URL(fresh.baseUrl).hostname.endsWith(".invalid")).toBe(true);
    } finally {
      await fresh.stop();
    }
  });
});
