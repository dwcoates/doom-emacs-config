/**
 * The fake store is TEST INFRASTRUCTURE that other suites assert against, so
 * its own semantics are pinned here. A fake that upserts wrongly, or replays
 * its opening page down the tail, would make every suite built on it pass
 * while the real shim is broken.
 */
import { create } from "@bufbuild/protobuf";
import { containing } from "../expect-shapes.js";
import { nextPush } from "../next-push.js";
import { Code, ConnectError } from "@connectrpc/connect";
import { mkdtempSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import { conversationv1, storev1 } from "../../src/proto.js";

/** The stream plane every shim-written entry lands on. */
function streamPlane(): storev1.Plane {
  return create(storev1.PlaneSchema, {
    plane: { case: "stream", value: create(storev1.PlaneStreamSchema, {}) },
  });
}
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { startFakeStore, type FakeStore } from "./store-server.js";

const running: FakeStore[] = [];

afterEach(async () => {
  for (const store of running.splice(0)) await store.close();
});

async function store(): Promise<{ store: FakeStore; client: StoreClient }> {
  const sock = path.join(mkdtempSync(path.join(os.tmpdir(), "fake-store-")), "store.sock");
  const started = await startFakeStore(sock);
  running.push(started);
  return { store: started, client: createStoreClient(sock) };
}

const agentId = (value: string): conversationv1.AgentId =>
  create(conversationv1.AgentIdSchema, { value });

/** One page line for `book`, carrying an activity update, under `upsertKey`. */
function pageLineEntry(book: string, upsertKey: string, text: string): storev1.StoreEntry {
  return create(storev1.StoreEntrySchema, {
    plane: streamPlane(),
    writeId: `w-${upsertKey}-${text}`,
    upsertKey,
    entry: {
      case: "agentUpdate",
      value: create(storev1.StoreAgentUpdateSchema, {
        agentInfo: {
          case: "serveableFrame",
          value: create(storev1.StorePageLineSchema, {
            pageAgentId: agentId(book),
            agentItem: create(storev1.StoreAgentItemSchema, {
              item: {
                case: "agentPrompt",
                value: create(conversationv1.AgentPromptSchema, {
                  id: create(conversationv1.TurnIdSchema, { value: text }),
                  agent: agentId(book),
                }),
              },
            }),
          }),
        },
      }),
    },
  });
}

/** A spawning agent's frame whose subagent start CREATES `created`. */
function spawnEntry(book: string, upsertKey: string, created: string): storev1.StoreEntry {
  return create(storev1.StoreEntrySchema, {
    plane: streamPlane(),
    writeId: `w-${upsertKey}`,
    upsertKey,
    entry: {
      case: "agentUpdate",
      value: create(storev1.StoreAgentUpdateSchema, {
        agentInfo: {
          case: "serveableFrame",
          value: create(storev1.StorePageLineSchema, {
            pageAgentId: agentId(book),
            agentItem: create(storev1.StoreAgentItemSchema, {
              item: {
                case: "agentFrame",
                value: create(conversationv1.AgentFrameSchema, {
                  agentId: agentId(book),
                  result: {
                    case: "update",
                    value: create(conversationv1.AgentUpdateSchema, {
                      update: {
                        case: "activity",
                        value: create(conversationv1.AgentActivitySchema, {
                          activityId: create(conversationv1.AgentActivityIdSchema, {
                            value: upsertKey,
                          }),
                          item: {
                            case: "subagent",
                            value: create(conversationv1.AgentSubagentSchema, {
                              result: {
                                case: "start",
                                value: create(conversationv1.AgentSubagentStartSchema, {
                                  createdAgentId: agentId(created),
                                }),
                              },
                            }),
                          },
                        }),
                      },
                    }),
                  },
                }),
              },
            }),
          }),
        },
      }),
    },
  });
}

/** A frame carrying one of the terminal arms, which concludes an agent. */
function terminalEntry(book: string, upsertKey: string): storev1.StoreEntry {
  return create(storev1.StoreEntrySchema, {
    plane: streamPlane(),
    writeId: `w-${upsertKey}`,
    upsertKey,
    entry: {
      case: "agentUpdate",
      value: create(storev1.StoreAgentUpdateSchema, {
        agentInfo: {
          case: "serveableFrame",
          value: create(storev1.StorePageLineSchema, {
            pageAgentId: agentId(book),
            agentItem: create(storev1.StoreAgentItemSchema, {
              item: {
                case: "agentFrame",
                value: create(conversationv1.AgentFrameSchema, {
                  agentId: agentId(book),
                  result: {
                    case: "success",
                    value: create(conversationv1.AgentSuccessSchema, {
                      outcome: {
                        case: "completed",
                        value: create(conversationv1.AgentCompletedSchema, {}),
                      },
                    }),
                  },
                }),
              },
            }),
          }),
        },
      }),
    },
  });
}

/** A frame announcing detached work, optionally naming the run it left. */
function detachedEntry(book: string, upsertKey: string, workId: string, runId: string): storev1.StoreEntry {
  return create(storev1.StoreEntrySchema, {
    plane: streamPlane(),
    writeId: `w-${upsertKey}`,
    upsertKey,
    entry: {
      case: "agentUpdate",
      value: create(storev1.StoreAgentUpdateSchema, {
        agentInfo: {
          case: "serveableFrame",
          value: create(storev1.StorePageLineSchema, {
            pageAgentId: agentId(book),
            agentItem: create(storev1.StoreAgentItemSchema, {
              item: {
                case: "agentFrame",
                value: create(conversationv1.AgentFrameSchema, {
                  agentId: agentId(book),
                  result: {
                    case: "detachedWork",
                    value: create(conversationv1.AgentDetachedWorkSchema, {
                      work: create(conversationv1.DetachedWorkIdSchema, { value: workId }),
                      origin: {
                        case: "detached",
                        value: create(conversationv1.DetachedWorkDetachedSchema, {
                          detachedFromId: create(conversationv1.AgentActivityIdSchema, { value: runId }),
                          cause: {
                            case: "requested",
                            value: create(conversationv1.DetachedCauseRequestedSchema, {}),
                          },
                        }),
                      },
                    }),
                  },
                }),
              },
            }),
          }),
        },
      }),
    },
  });
}

/** A bash lifecycle row terminating one run. */
function bashTerminalEntry(runId: string): storev1.StoreEntry {
  return create(storev1.StoreEntrySchema, {
    plane: streamPlane(),
    writeId: `w-bash-${runId}`,
    upsertKey: `bash:${runId}`,
    entry: {
      case: "agentUpdate",
      value: create(storev1.StoreAgentUpdateSchema, {
        agentInfo: {
          case: "bash",
          value: create(storev1.StoreAgentBashSchema, {
            run: create(conversationv1.AgentActivityIdSchema, { value: runId }),
            frame: create(conversationv1.AgentBashSchema, {
              result: {
                case: "success",
                value: create(conversationv1.AgentBashSuccessSchema, {}),
              },
            }),
          }),
        },
      }),
    },
  });
}

async function write(client: StoreClient, ...entries: storev1.StoreEntry[]): Promise<void> {
  const response = await client.writeBatch(
    create(storev1.WriteBatchRequestSchema, {
      writeClass: create(storev1.WriteClassSchema, {
        writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
      }),
      producer: "claude-shim:test",
      batch: create(storev1.EntryBatchSchema, { entries }),
    }),
  );
  if (response.result.case !== "success") {
    throw new Error(`fake store refused a write: ${JSON.stringify(response.result)}`);
  }
}

async function open(
  client: StoreClient,
  book: string,
  pageSize: number,
  knownThrough?: storev1.StoreItemPointer,
): Promise<storev1.OpenAgentSessionSuccess> {
  const response = await client.openAgentSession(
    create(storev1.OpenAgentSessionRequestSchema, {
      agent: agentId(book),
      pageSize,
      ...(knownThrough === undefined ? {} : { knownThrough }),
    }),
  );
  if (response.result.case !== "success") throw new Error("fake store refused an open");
  return response.result.value;
}

describe("WriteBatch", () => {
  it("acks a batch and lands its entries", async () => {
    // Arrange.
    const { store: fake, client } = await store();

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "t1"));

    // Assert.
    expect(fake.book("a")).toHaveLength(1);
  });

  it("ABSORBS a write id that already landed, leaving the newer write standing", async () => {
    // Arrange: w1 lands, then w2 supersedes the same row.
    const { store: fake, client } = await store();
    const first = pageLineEntry("a", "prompt:t1", "first");
    await write(client, first);
    await write(client, pageLineEntry("a", "prompt:t1", "second"));

    // Act: w1 is re-sent.
    await write(client, first);

    // Assert.
    const line = fake.book("a")[0]?.line?.agentItem?.item;
    expect(line?.case === "agentPrompt" ? line.value.id?.value : undefined).toBe("second");
  });

  it("UPSERTS by key: a re-sent row replaces rather than appends", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "second"));

    // Assert.
    expect(fake.book("a")).toHaveLength(1);
  });

  it("keeps an upserted row's original POSITION", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "one-revised"));

    // Assert.
    expect(fake.book("a").map((line) => line.at?.value)).toEqual(["1", "2"]);
  });

  it("fails every write while the failure switch is set", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failWrites("store is down");

    // Act.
    const response = await client.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        writeClass: create(storev1.WriteClassSchema, {
          writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
        }),
        producer: "claude-shim:test",
        batch: create(storev1.EntryBatchSchema, {
          entries: [pageLineEntry("a", "prompt:t1", "t1")],
        }),
      }),
    );

    // Assert.
    expect(response.result.case).toBe("failure");
  });

  it("lands NOTHING from a failed batch, so a whole-batch retry cannot duplicate", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failWrites("store is down");

    // Act.
    await client.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        writeClass: create(storev1.WriteClassSchema, {
          writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
        }),
        producer: "claude-shim:test",
        batch: create(storev1.EntryBatchSchema, {
          entries: [pageLineEntry("a", "prompt:t1", "t1")],
        }),
      }),
    );

    // Assert.
    expect(fake.book("a")).toEqual([]);
  });

  it("accepts writes again once the switch is cleared", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failWrites("store is down");
    fake.failWrites(null);

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "t1"));

    // Assert.
    expect(fake.book("a")).toHaveLength(1);
  });

  it("records session updates separately from page lines", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    const entry = create(storev1.StoreEntrySchema, {
      plane: streamPlane(),
      writeId: "w-1",
      upsertKey: "session:model_changed:u1",
      entry: {
        case: "sessionUpdate",
        value: create(conversationv1.SessionUpdateSchema, {
          update: {
            case: "modelChanged",
            value: create(conversationv1.SessionModelChangedSchema, {}),
          },
        }),
      },
    });

    // Act.
    await write(client, entry);

    // Assert.
    expect(fake.sessionUpdates()).toHaveLength(1);
  });

  it("keeps unserved items out of the served book", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    const entry = create(storev1.StoreEntrySchema, {
      plane: streamPlane(),
      writeId: "w-2",
      upsertKey: "residue:hook_result",
      entry: {
        case: "agentUpdate",
        value: create(storev1.StoreAgentUpdateSchema, {
          agentInfo: {
            case: "unservedItem",
            value: create(storev1.StoreUnservedItemSchema, {
              unservedItem: {
                case: "vendorSpecific",
                value: create(storev1.StoreVendorSpecificSchema, { kind: "hook_result", raw: {} }),
              },
            }),
          },
        }),
      },
    });

    // Act.
    await write(client, entry);

    // Assert.
    expect({ book: fake.book("a"), unserved: fake.unserved().length }).toEqual({
      book: [],
      unserved: 1,
    });
  });
});

describe("OpenAgentSession", () => {
  it("serves the opening page NEWEST FIRST", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));

    // Act.
    const success = await open(client, "a", 10);

    // Assert.
    expect(success.page?.lines.map((line) => line.at?.value)).toEqual(["2", "1"]);
  });

  it("caps the page at the caller's budget", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));
    await write(client, pageLineEntry("a", "prompt:t3", "three"));

    // Act.
    const success = await open(client, "a", 2);

    // Assert.
    expect(success.page?.lines).toHaveLength(2);
  });

  it("points `more` at the page's OLDEST line, which the next read echoes", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));
    await write(client, pageLineEntry("a", "prompt:t3", "three"));

    // Act.
    const success = await open(client, "a", 2);

    // Assert.
    expect(success.page?.boundary).toEqual({
      case: "more",
      value: containing({
        lastItem: containing({ value: "2" }),
      }),
    });
  });

  it("reports the floor when the page reached the oldest line", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));

    // Act.
    const success = await open(client, "a", 10);

    // Assert.
    expect(success.page?.boundary.case).toBe("floor");
  });

  it("honors known_through by serving ONLY items newer than it", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));
    const knownThrough = create(storev1.StoreItemPointerSchema, { value: "1" });

    // Act.
    const success = await open(client, "a", 10, knownThrough);

    // Assert.
    expect(success.page?.lines.map((line) => line.at?.value)).toEqual(["2"]);
  });

  it("mints a watch token with the page", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "t1"));

    // Act.
    const success = await open(client, "a", 10);

    // Assert.
    expect(success.watch?.value).not.toBe("");
  });
});

describe("WatchAgentSession", () => {
  it("tails lines written AFTER the open", async () => {
    // Arrange.
    // The book is EMPTY at the open — the agent is known only because a spawn
    // in another book created it — so everything the tail carries is news.
    const { client } = await store();
    await write(client, spawnEntry("parent", "k-spawn", "a"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "after-open"));
    const first = await nextPush(tail);

    // Assert.
    const item = first.line?.line?.agentItem?.item;
    expect(item?.case === "agentPrompt" ? item.value.id?.value : undefined).toBe("after-open");
  });

  it("is a PURE tail: it never replays what the opening page carried", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "before-open"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();

    // Act.
    await write(client, pageLineEntry("a", "prompt:t2", "after-open"));
    const first = await nextPush(tail);

    // Assert.
    expect(first.line?.at?.value).toBe("2");
  });

  it("delivers an UPSERT OF A ROW THE OPENING PAGE ALREADY CARRIED", async () => {
    // A unit that started before the reader subscribed and settles afterwards
    // is a real change: the write supersedes the row whole, so the tail must
    // serve it even though the row predates the watch. Pinning the tail
    // against the row's original pointer silently dropped exactly these.
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "started"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "settled"));
    const first = await nextPush(tail);

    // Assert.
    expect(first.line?.at?.value).toBe("1");
  });

  it("serves an upserted row at its ORIGINAL pointer, not a fresh one", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "started"));
    await write(client, pageLineEntry("a", "prompt:t2", "other"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();

    // Act.
    await write(client, pageLineEntry("a", "prompt:t1", "settled"));
    const first = await nextPush(tail);

    // Assert.
    expect(first.line?.at?.value).toBe("1");
    // Narrowed on the arm, not read through it: only an `agentPrompt` carries an id,
    // and reading one off an unchecked value is what the `any` here used to allow.
    const settled = first.line?.line?.agentItem?.item;
    expect(settled?.case === "agentPrompt" ? settled.value.id?.value : undefined).toBe("settled");
  });

  it("delivers a NEW row written after the open down the tail", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));
    const success = await open(client, "a", 0);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();

    // Act.
    await write(client, pageLineEntry("a", "prompt:t2", "second"));
    const first = await nextPush(tail);

    // Assert.
    expect(first.line?.at?.value).toBe("2");
  });

  it("refuses an unknown token with NotFound, since a stream cannot say it otherwise", async () => {
    // Arrange.
    const { client } = await store();
    const request = create(storev1.WatchAgentSessionRequestSchema, {
      watch: create(storev1.AgentSessionTokenSchema, { value: "never-minted" }),
    });

    // Act.
    const rejection = await (async (): Promise<ConnectError | null> => {
      try {
        for await (const _line of client.watchAgentSession(request)) return null;
        return null;
      } catch (err) {
        return ConnectError.from(err);
      }
    })();

    // Assert.
    expect(rejection?.code).toBe(Code.NotFound);
  });
});

describe("ReadAgentPage", () => {
  it("walks strictly OLDER than the pointer it was given", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));
    await write(client, pageLineEntry("a", "prompt:t3", "three"));

    // Act.
    const response = await client.readAgentPage(
      create(storev1.ReadAgentPageRequestSchema, {
        book: agentId("a"),
        pageSize: 10,
        after: create(storev1.StoreItemPointerSchema, { value: "3" }),
      }),
    );

    // Assert.
    expect(response.result.case === "success" ? response.result.value.lines : []).toHaveLength(2);
  });

  it("reports the floor once the walk reached the oldest line", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));

    // Act.
    const response = await client.readAgentPage(
      create(storev1.ReadAgentPageRequestSchema, {
        book: agentId("a"),
        pageSize: 10,
        after: create(storev1.StoreItemPointerSchema, { value: "2" }),
      }),
    );

    // Assert.
    expect(response.result.case === "success" ? response.result.value.boundary.case : "").toBe(
      "floor",
    );
  });

  it("reports `more` when the budget cut the walk short", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "one"));
    await write(client, pageLineEntry("a", "prompt:t2", "two"));
    await write(client, pageLineEntry("a", "prompt:t3", "three"));

    // Act.
    const response = await client.readAgentPage(
      create(storev1.ReadAgentPageRequestSchema, {
        book: agentId("a"),
        pageSize: 1,
        after: create(storev1.StoreItemPointerSchema, { value: "3" }),
      }),
    );

    // Assert.
    expect(response.result.case === "success" ? response.result.value.boundary.case : "").toBe(
      "more",
    );
  });
});

/** A GetLiveWork request scoped to `session`, the only form the store answers. */
function liveWorkFor(session: string): storev1.GetLiveWorkRequest {
  return create(storev1.GetLiveWorkRequestSchema, { session: agentId(session) });
}

describe("GetLiveWork", () => {
  it("reports a spawned agent that never concluded", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, spawnEntry("main", "activity:spawn-1", "sub-1"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success"
        ? response.result.value.liveAgents.map((id) => id.value)
        : [],
    ).toEqual(["sub-1"]);
  });

  it("never reports the session's own main agent", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, detachedEntry("main", "activity:run1", "task-1", "run1"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.liveAgents : [],
    ).toEqual([]);
  });

  it("reports a nested subagent of the session", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, spawnEntry("main", "activity:spawn-1", "sub-1"));
    await write(client, spawnEntry("sub-1", "activity:spawn-2", "sub-2"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success"
        ? response.result.value.liveAgents.map((id) => id.value)
        : [],
    ).toEqual(["sub-1", "sub-2"]);
  });

  it("never reports another session's spawned agent", async () => {
    // Arrange: ONE store serves every session on the host.
    const { client } = await store();
    await write(client, spawnEntry("main-a", "activity:spawn-a", "sub-a"));
    await write(client, spawnEntry("main-b", "activity:spawn-b", "sub-b"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main-b"));

    // Assert.
    expect(
      response.result.case === "success"
        ? response.result.value.liveAgents.map((id) => id.value)
        : [],
    ).toEqual(["sub-b"]);
  });

  it("stops reporting an agent once a terminal frame lands", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, spawnEntry("main", "activity:spawn-1", "sub-1"));
    await write(client, terminalEntry("sub-1", "terminal:sub-1:u1"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.liveAgents : [],
    ).toEqual([]);
  });

  it("reports announced detached work with no terminal bash row", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, detachedEntry("main", "activity:run1", "task-1", "run1"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success"
        ? response.result.value.liveDetached.map((id) => id.value)
        : [],
    ).toEqual(["task-1"]);
  });

  it("never reports detached work another session announced", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, detachedEntry("main-a", "activity:run-a", "task-a", "run-a"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main-b"));

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.liveDetached : [],
    ).toEqual([]);
  });

  it("stops reporting detached work once its run terminates", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, detachedEntry("main", "activity:run1", "task-1", "run1"));
    await write(client, bashTerminalEntry("run1"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.liveDetached : [],
    ).toEqual([]);
  });

  it("keeps detached work ended when a row of its run lands AFTER the terminal", async () => {
    // Arrange: the run ends, then a late sidecar tail lands on it.
    const { client } = await store();
    await write(client, detachedEntry("main", "activity:run1", "task-1", "run1"));
    await write(client, bashTerminalEntry("run1"));
    await write(
      client,
      create(storev1.StoreEntrySchema, {
        plane: streamPlane(),
        writeId: "w-bash-run1-late-tail",
        upsertKey: "bash:run1:tail",
        entry: {
          case: "agentUpdate",
          value: create(storev1.StoreAgentUpdateSchema, {
            agentInfo: {
              case: "bash",
              value: create(storev1.StoreAgentBashSchema, {
                run: create(conversationv1.AgentActivityIdSchema, { value: "run1" }),
                frame: create(conversationv1.AgentBashSchema, {
                  result: { case: "tail", value: create(conversationv1.AgentBashTailSchema, { text: "late" }) },
                }),
              }),
            },
          }),
        },
      }),
    );

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.liveDetached : [],
    ).toEqual([]);
  });

  it("reports no live workflows, because nothing writes one this wave", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, detachedEntry("main", "activity:run1", "task-1", "run1"));

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.liveWorkflows : [],
    ).toEqual([]);
  });

  it("refuses a request naming no session with invalid_request on `session`", async () => {
    // Arrange.
    const { client } = await store();

    // Act.
    const response = await client.getLiveWork(create(storev1.GetLiveWorkRequestSchema, {}));

    // Assert.
    expect(
      response.result.case === "failure" && response.result.value.kind.case === "invalidRequest"
        ? response.result.value.kind.value.field
        : undefined,
    ).toBe("session");
  });
});

describe("GetWorkflow", () => {
  it("answers Unimplemented, the same way the shim's own verb does", async () => {
    // Arrange.
    const { client } = await store();

    // Act.
    const rejection = await client
      .getWorkflow(
        create(storev1.GetWorkflowRequestSchema, {
          work: create(conversationv1.DetachedWorkIdSchema, { value: "w1" }),
        }),
      )
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });
});

describe("GetSidecarCursors", () => {
  it("answers an empty SUCCESS: nothing-yet is an answer, not an error", async () => {
    // Arrange.
    const { client } = await store();

    // Act.
    const response = await client.getSidecarCursors(
      create(storev1.GetSidecarCursorsRequestSchema, {}),
    );

    // Assert.
    expect(
      response.result.case === "success" ? response.result.value.cursors : null,
    ).toEqual([]);
  });
});

describe("the read ledger", () => {
  it("records nothing before a read is served", async () => {
    // Arrange + Act.
    const { store: fake } = await store();

    // Assert.
    expect(fake.reads()).toEqual([]);
  });

  it("records a GetLiveWork", async () => {
    // Arrange.
    const { store: fake, client } = await store();

    // Act.
    await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(fake.reads().map((read) => read.rpc)).toEqual(["GetLiveWork"]);
  });

  it("records a GetSidecarCursors", async () => {
    // Arrange.
    const { store: fake, client } = await store();

    // Act.
    await client.getSidecarCursors(create(storev1.GetSidecarCursorsRequestSchema, {}));

    // Assert.
    expect(fake.reads().map((read) => read.rpc)).toEqual(["GetSidecarCursors"]);
  });

  it("records an OpenAgentSession with the request it was asked", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "t1"));

    // Act.
    await open(client, "a", 5);

    // Assert.
    const read = fake.reads().filter((entry) => entry.rpc === "OpenAgentSession")[0];
    expect({
      rpc: read?.rpc,
      agent: (read?.request as storev1.OpenAgentSessionRequest | undefined)?.agent?.value,
    }).toEqual({ rpc: "OpenAgentSession", agent: "a" });
  });

  it("records a ReadAgentPage", async () => {
    // Arrange.
    const { store: fake, client } = await store();

    // Act.
    await client.readAgentPage(
      create(storev1.ReadAgentPageRequestSchema, { book: agentId("a"), pageSize: 5 }),
    );

    // Assert.
    expect(fake.reads().map((read) => read.rpc)).toEqual(["ReadAgentPage"]);
  });

  it("keeps every read in the order it served them", async () => {
    // Arrange.
    const { store: fake, client } = await store();

    // Act.
    await client.getLiveWork(liveWorkFor("main"));
    await client.getSidecarCursors(create(storev1.GetSidecarCursorsRequestSchema, {}));
    await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(fake.reads().map((read) => read.rpc)).toEqual([
      "GetLiveWork",
      "GetSidecarCursors",
      "GetLiveWork",
    ]);
  });
});

describe("typed read refusals", () => {
  // THE FAKE MUST BE ABLE TO REFUSE. Every typed arm the store declares is a
  // branch of the shim's reader, and a fake that only ever succeeds leaves
  // those branches asserted nowhere.

  it("refuses OpenAgentSession under the stale_pointer arm", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failReads("OpenAgentSession", "stale_pointer", "no such line in this book");

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("stalePointer");
  });

  it("refuses OpenAgentSession under the invalid_request arm", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failReads("OpenAgentSession", "invalid_request", "no book by that name");

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("invalidRequest");
  });

  it("carries the caller's detail through on a refusal", async () => {
    // The detail is deliberately the caller's to choose, so a test can pair an
    // arm with contradicting prose and catch a consumer classifying by string.
    // Arrange.
    const { store: fake, client } = await store();
    fake.failReads("OpenAgentSession", "storage_failure", "that pointer is unknown");

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(response.result.case === "failure" ? response.result.value.detail : "").toBe(
      "that pointer is unknown",
    );
  });

  it("refuses ReadAgentPage under the named arm", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failReads("ReadAgentPage", "storage_failure", "the disk is full");

    // Act.
    const response = await client.readAgentPage(
      create(storev1.ReadAgentPageRequestSchema, {
        book: agentId("a"),
        pageSize: 10,
        after: create(storev1.StoreItemPointerSchema, { value: "9" }),
      }),
    );

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("storageFailure");
  });

  it("refuses GetLiveWork under storage_failure", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failReads("GetLiveWork", "storage_failure", "sqlite: no such table");

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("storageFailure");
  });

  it("refuses OpenAgentSession with unknown_agent before any write names the agent", async () => {
    // `db.ensureAgent` creates the agent row on the first write, so a book
    // opened before then does not exist (landing 7).
    // Arrange.
    const { client } = await store();

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("unknownAgent");
  });

  it("serves OpenAgentSession once a write has named the agent", async () => {
    // Arrange.
    const { client } = await store();
    await write(client, pageLineEntry("a", "k1", "one"));

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(response.result.case).toBe("success");
  });

  it("registers the agent a spawn frame CREATED, before that agent writes anything", async () => {
    // `db.createSpawnedAgent`: the created agent's book is addressable from the
    // spawn frame on.
    // Arrange.
    const { client } = await store();
    await write(client, spawnEntry("parent", "k-spawn", "child"));

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("child"), pageSize: 10 }),
    );

    // Assert.
    expect(response.result.case).toBe("success");
  });

  it("REFUSES to serve GetLiveWork an arm the proto does not declare", async () => {
    // Fabricating one would put a shape on the wire the proto forbids and test
    // a consumer against a store that cannot exist.
    // Arrange.
    const { store: fake } = await store();

    // Act, Assert.
    expect(() => fake.failReads("GetLiveWork", "stale_pointer")).toThrow(
      /declares invalid_request and storage_failure/,
    );
  });

  it("refuses GetLiveWork under invalid_request", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failReads("GetLiveWork", "invalid_request", "session: refused");

    // Act.
    const response = await client.getLiveWork(liveWorkFor("main"));

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("invalidRequest");
  });

  it("serves the verb again once its arm is cleared", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "t1"));
    fake.failReads("OpenAgentSession", "storage_failure");

    // Act.
    fake.failReads("OpenAgentSession", null);
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(response.result.case).toBe("success");
  });

  it("refuses ONLY the verb that was armed", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "t1"));
    fake.failReads("ReadAgentPage", "storage_failure");

    // Act.
    const response = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, { agent: agentId("a"), pageSize: 10 }),
    );

    // Assert.
    expect(response.result.case).toBe("success");
  });
});

describe("typed write refusals", () => {
  it("gives failWrites the RETRYABLE storage_failure arm", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failWrites("the store is down");

    // Act.
    const response = await client.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        writeClass: create(storev1.WriteClassSchema, {
          writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
        }),
        producer: "claude-shim:test",
        batch: create(storev1.EntryBatchSchema, { entries: [] }),
      }),
    );

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("storageFailure");
  });

  it("refuses a write that states no class, as the real store does", async () => {
    // Arrange.
    const { store: fake, client } = await store();

    // Act.
    const response = await client.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        producer: "claude-shim:test",
        batch: create(storev1.EntryBatchSchema, { entries: [pageLineEntry("a", "prompt:t1", "t1")] }),
      }),
    );

    // Assert.
    expect(
      response.result.case === "failure" && response.result.value.kind.case === "invalidRequest"
        ? response.result.value.kind.value.field
        : undefined,
    ).toBe("write_class");
    expect(fake.book("a")).toHaveLength(0);
  });

  it("names invalid_request when the bytes can never be accepted", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    fake.failWritesWith("invalid_request", "entry 0 sets no arm");

    // Act.
    const response = await client.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        writeClass: create(storev1.WriteClassSchema, {
          writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
        }),
        producer: "claude-shim:test",
        batch: create(storev1.EntryBatchSchema, { entries: [] }),
      }),
    );

    // Assert.
    expect(
      response.result.case === "failure" ? response.result.value.kind.case : undefined,
    ).toBe("invalidRequest");
  });
});

describe("the open-tail ledger", () => {
  it("lists no tail before anything watches", async () => {
    // Arrange, Act.
    const { store: fake } = await store();

    // Assert.
    expect(fake.openTails()).toEqual([]);
  });

  it("lists a tail while its stream is being read", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();

    // Act. Pull one line, so the generator is certainly running.
    await write(client, pageLineEntry("a", "prompt:t2", "second"));
    await tail.next();

    // Assert.
    expect(fake.openTails()).toEqual([success.watch?.value]);
  });

  it("tailOpened resolves on a tail that opens AFTER the wait began", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));
    const success = await open(client, "a", 10);
    const opened = fake.tailOpened();

    // Act.
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();
    await write(client, pageLineEntry("a", "prompt:t2", "second"));
    await tail.next();

    // Assert.
    expect(await opened).toBe(success.watch?.value);
  });

  it("tailOpened resolves immediately on a tail that is ALREADY open", async () => {
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();
    await write(client, pageLineEntry("a", "prompt:t2", "second"));
    await tail.next();

    // Act.
    const token = await fake.tailOpened();

    // Assert.
    expect(token).toBe(success.watch?.value);
  });

  it("DROPS the entry once the client ABORTS the call", async () => {
    // A client that stops reading must CANCEL THE CALL, not merely stop pulling
    // its iterator: Connect's stream close drains the body, which on a standing
    // tail never completes, so only an abort ends the subscription. A store
    // holding a stream open for a reader that never returns leaks one
    // subscription per closed watch — this is the ledger that proves it does
    // not, and the reader (src/store/reader.ts) aborts for exactly this reason.
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));
    const success = await open(client, "a", 10);
    const abort = new AbortController();
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
      abort.signal,
    )[Symbol.asyncIterator]();
    await write(client, pageLineEntry("a", "prompt:t2", "second"));
    await tail.next();

    // Act.
    abort.abort();
    await fake.tailClosed(success.watch?.value ?? "");

    // Assert.
    expect(fake.openTails()).toEqual([]);
  });

  it("KEEPS the entry while a client merely stops pulling without aborting", async () => {
    // The negative that gives the assertion above its meaning: if walking away
    // were enough, the ledger would prove nothing about cancellation.
    // Arrange.
    const { store: fake, client } = await store();
    await write(client, pageLineEntry("a", "prompt:t1", "first"));
    const success = await open(client, "a", 10);
    const tail = client.watchAgentSession(
      create(storev1.WatchAgentSessionRequestSchema, { watch: success.watch }),
    )[Symbol.asyncIterator]();
    await write(client, pageLineEntry("a", "prompt:t2", "second"));
    await tail.next();

    // Act. Stop pulling, and let every already-scheduled task run.
    await new Promise((resolve) => setImmediate(resolve));

    // Assert.
    expect(fake.openTails()).toEqual([success.watch?.value]);
  });
});
