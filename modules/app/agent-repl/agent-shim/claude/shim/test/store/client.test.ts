/**
 * The store client's own contract: that it builds, that it refuses a missing
 * socket, and that its narrowed surface really does reach a store.v1 server
 * over a unix socket.
 */
import { create } from "@bufbuild/protobuf";
import { mkdtempSync, writeSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it, vi } from "vitest";
import { conversationv1, storev1 } from "../../src/proto.js";

/** The stream plane every shim-written entry lands on. */
function streamPlane(): storev1.Plane {
  return create(storev1.PlaneSchema, {
    plane: { case: "stream", value: create(storev1.PlaneStreamSchema, {}) },
  });
}
import { STORE_BASE_URL, createStoreClient } from "../../src/store/client.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";

const running: FakeStore[] = [];
const mockedWriteSync = vi.mocked(writeSync);

function logRecordsSince(before: number): Array<Record<string, unknown>> {
  const calls = mockedWriteSync.mock.calls.slice(before) as unknown as Array<[number, Buffer, number, number]>;
  return calls.map(([, bytes, offset, length]) =>
    JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<string, unknown>,
  );
}

afterEach(async () => {
  for (const store of running.splice(0)) await store.close();
});

async function fakeStore(): Promise<string> {
  const sock = path.join(mkdtempSync(path.join(os.tmpdir(), "store-client-")), "store.sock");
  running.push(await startFakeStore(sock));
  return sock;
}

describe("createStoreClient", () => {
  it("refuses to build without a socket path rather than dialing nowhere", () => {
    // Arrange, Act, Assert.
    expect(() => createStoreClient("")).toThrow(/store socket path is required/);
  });

  it("exposes exactly the seven store.v1 verbs", async () => {
    // Arrange.
    const client = createStoreClient(await fakeStore());

    // Act.
    const verbs = Object.keys(client).sort();

    // Assert.
    expect(verbs).toEqual([
      "getLiveWork",
      "getSidecarCursors",
      "getWorkflow",
      "openAgentSession",
      "readAgentPage",
      "watchAgentSession",
      "watchBashRun",
      "writeBatch",
    ]);
  });

  it("reaches a real store.v1 server over the unix socket", async () => {
    // Arrange.
    const client = createStoreClient(await fakeStore());

    // Act.
    const response = await client.getLiveWork(
      create(storev1.GetLiveWorkRequestSchema, {
        session: create(conversationv1.AgentIdSchema, { value: "main-1" }),
      }),
    );

    // Assert.
    expect(response.result.case).toBe("success");
  });

  it("records both boundaries of a successful unary store round-trip at debug", async () => {
    // Arrange.
    const client = createStoreClient(await fakeStore());
    const before = mockedWriteSync.mock.calls.length;

    // Act.
    await client.getLiveWork(
      create(storev1.GetLiveWorkRequestSchema, {
        session: create(conversationv1.AgentIdSchema, { value: "main-1" }),
      }),
    );

    // Assert.
    expect(
      logRecordsSince(before)
        .filter((record) => record.context !== undefined && (record.context as Record<string, unknown>).rpc === "GetLiveWork")
        .map((record) => ({ level: record.level, boundary: (record.context as Record<string, unknown>).boundary })),
    ).toEqual([
      { level: "debug", boundary: "entered" },
      { level: "debug", boundary: "completed" },
    ]);
  });

  it("carries a written entry through to the store's own tables", async () => {
    // Arrange.
    const sock = await fakeStore();
    const client = createStoreClient(sock);
    const entry = create(storev1.StoreEntrySchema, {
      plane: streamPlane(),
      writeId: "w-1",
      upsertKey: "prompt:t1",
      entry: {
        case: "agentUpdate",
        value: create(storev1.StoreAgentUpdateSchema, {
          agentInfo: {
            case: "serveableFrame",
            value: create(storev1.StorePageLineSchema, {
              pageAgentId: create(conversationv1.AgentIdSchema, { value: "a" }),
              // The prompt is what makes the store record FIRST SIGHT of the
              // agent (`db.ensureAgent`); a line naming no agent item creates
              // no agent row, and the book would not be openable.
              agentItem: create(storev1.StoreAgentItemSchema, {
                item: {
                  case: "agentPrompt",
                  value: create(conversationv1.AgentPromptSchema, {
                    id: create(conversationv1.TurnIdSchema, { value: "t1" }),
                    agent: create(conversationv1.AgentIdSchema, { value: "a" }),
                  }),
                },
              }),
            }),
          },
        }),
      },
    });

    // Act.
    await client.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        writeClass: create(storev1.WriteClassSchema, {
          writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
        }),
        producer: "claude-shim:test",
        batch: create(storev1.EntryBatchSchema, { entries: [entry] }),
      }),
    );
    const opened = await client.openAgentSession(
      create(storev1.OpenAgentSessionRequestSchema, {
        agent: create(conversationv1.AgentIdSchema, { value: "a" }),
        pageSize: 10,
      }),
    );

    // Assert.
    expect(
      opened.result.case === "success" ? opened.result.value.page?.lines.length : 0,
    ).toBe(1);
  });
});

describe("STORE_BASE_URL", () => {
  it("is a syntactically valid placeholder authority, never actually resolved", () => {
    // Arrange, Act, Assert.
    expect(() => new URL(STORE_BASE_URL)).not.toThrow();
  });
});
