/**
 * test/fakes/store-server.ts — an IN-PROCESS store.v1 server.
 *
 * # What it is for
 *
 * The shim's store client, writer, reader and reconciler all need a store that
 * ANSWERS, and the real one is a Go process the shim's suite may not require
 * (an integration test involving another real system is an e2e test, which this
 * is not). This is the store reduced to its observable contract: in-memory
 * tables, the same seven rpcs, the same refusal shapes.
 *
 * It is deliberately shared by the store-client agent and the integration-tests
 * agent, so both are working against ONE notion of how the store behaves and a
 * disagreement between them is a disagreement about this file rather than a
 * mystery at merge time.
 *
 * # The semantics it reproduces, and why each matters
 *
 *   - UPSERT BY KEY, FIRST-INSERT ORDER. A row re-sent under the same
 *     `upsert_key` REPLACES the row in place and keeps its original position
 *     and pointer. That is what makes a unit that starts, updates and ends
 *     appear ONCE in a book, and it is the single behavior most shim code
 *     depends on.
 *   - PAGES ARE NEWEST FIRST, and `more.last_item` points at the page's OLDEST
 *     line — the pointer the next `ReadAgentPage` echoes to walk older.
 *   - `known_through` IS THE CALLER'S HIGH-WATER MARK. The store remembers
 *     nothing about what it served; a set pointer means "only items strictly
 *     newer than this".
 *   - A WATCH TOKEN IS PINNED AFTER THE NEWEST ITEM at open. The tail is PURE:
 *     it never replays what the opening page already carried, which is what
 *     makes open-then-watch race-free.
 *   - AN UNKNOWN WATCH TOKEN IS A CONNECT `NotFound`. A refused stream open has
 *     no failure message to live in — the response type is the frame it
 *     streams — so the refusal closes the stream at the transport.
 *   - `WatchBashRun` REPLAYS EVERY STORED ROW of a run in write order, then
 *     follows, and ENDS after the terminal row. A run with no stored row is a
 *     refused open — closed at the transport, like every other watch here.
 *   - WRITES CAN BE MADE TO FAIL ON DEMAND ({@link FakeStore.failWrites}), which
 *     is how the writer's retry buffer is testable at all.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, type ConnectRouter } from "@connectrpc/connect";
import { connectNodeAdapter } from "@connectrpc/connect-node";
import { unlinkSync } from "node:fs";
import http from "node:http";
import { conversationv1, storev1 } from "../../src/proto.js";

/** One stored line, with the identity and position the store gave it. */
interface StoredRow {
  /** The store-minted pointer. Stable across upserts of the same key. */
  readonly pointer: string;
  /** The key the row upserts on. */
  readonly upsertKey: string;
  /** The line itself, replaced whole on every upsert. */
  line: storev1.StorePageLine;
}

/** An opened reading session: which book, and where its tail begins. */
interface WatchSessionState {
  readonly book: string;
  /** Rows at or below this pointer were already served by the opening page. */
  readonly pinnedAfter: number;
  /** Lines waiting to be pulled by the tail, oldest first. */
  readonly pending: storev1.StoreLineAt[];
  /** Resolvers of pulls that arrived before any line did. */
  readonly waiters: Array<(line: storev1.StoreLineAt) => void>;
  /** Set when the store is closing, so a parked tail ends instead of hanging. */
  closed: boolean;
}

/** The running fake, and the levers a test pulls on it. */
export interface FakeStore {
  /** The unix socket it is listening on. */
  readonly socketPath: string;
  /** Stop serving, end every open tail, and remove the socket file. */
  close(): Promise<void>;
  /**
   * Make every subsequent WriteBatch answer a `failure` with this detail, or
   * pass `null` to accept writes again. The retry buffer has no other way to be
   * exercised: a real store fails when it is down, and a test cannot take one
   * down that it did not start.
   */
  failWrites(detail: string | null): void;
  /** Every WriteBatch request the store received, in order, failures included. */
  writes(): storev1.WriteBatchRequest[];
  /** One book's rows, oldest first — the store's own order, for assertions. */
  book(agentId: string): storev1.StoreLineAt[];
  /** Every session-update row written, in order. */
  sessionUpdates(): conversationv1.SessionUpdate[];
  /** Every unserved item written, in order. */
  unserved(): storev1.StoreUnservedItem[];
  /**
   * Every READ verb the store served, in order.
   *
   * `writes()` made the write plane observable and the read plane had no
   * counterpart, so "GetLiveWork is called exactly once, at session start"
   * could only be declared and never asserted. This is that lever.
   */
  reads(): FakeStoreRead[];
}

/** One read the fake served: which verb, and what was asked. */
export interface FakeStoreRead {
  readonly rpc:
    | "GetLiveWork"
    | "OpenAgentSession"
    | "ReadAgentPage"
    | "WatchBashRun"
    | "GetSidecarCursors";
  readonly request: unknown;
}

/** Start the fake store on `socketPath`. Resolves once it is accepting. */
export async function startFakeStore(socketPath: string): Promise<FakeStore> {
  const books = new Map<string, StoredRow[]>();
  const rowsByKey = new Map<string, StoredRow>();
  const watches = new Map<string, WatchSessionState>();
  const receivedWrites: storev1.WriteBatchRequest[] = [];
  const sessionUpdateRows: conversationv1.SessionUpdate[] = [];
  const unservedRows: storev1.StoreUnservedItem[] = [];
  /** Every read verb served, in order — see {@link FakeStore.reads}. */
  const servedReads: FakeStoreRead[] = [];
  const noteRead = (rpc: FakeStoreRead["rpc"], request: unknown): void => {
    servedReads.push({ rpc, request });
  };
  /** Detached work ids that were announced, mapped to the run they detached from. */
  const detachedAnnounced = new Map<string, string | undefined>();
  /**
   * Every bash lifecycle row per run, in write order.
   *
   * A LIST, NOT THE NEWEST ROW: `WatchBashRun` replays the run's whole history
   * before it follows, because a detached shell's output arrives as DELTAS and a
   * watcher handed only the newest one would have a hole where the output was.
   */
  const bashRowsByRun = new Map<string, storev1.StoreAgentBash[]>();
  /** Tails following one run's rows. */
  const bashWatchers = new Set<{
    readonly run: string;
    readonly pending: storev1.StoreAgentBash[];
    readonly waiters: Array<(row: storev1.StoreAgentBash | null) => void>;
    closed: boolean;
  }>();
  let nextPointer = 1;
  let nextToken = 1;
  let writeFailure: string | null = null;

  const pointerOf = (row: StoredRow): storev1.StoreItemPointer =>
    create(storev1.StoreItemPointerSchema, { value: row.pointer });

  const lineAt = (row: StoredRow): storev1.StoreLineAt =>
    create(storev1.StoreLineAtSchema, { at: pointerOf(row), line: row.line });

  /** Deliver a line to every tail watching its book, if the tail is past its pin. */
  const fanOut = (bookId: string, row: StoredRow): void => {
    for (const state of watches.values()) {
      if (state.book !== bookId) continue;
      if (Number(row.pointer) <= state.pinnedAfter) continue;
      const waiter = state.waiters.shift();
      if (waiter !== undefined) waiter(lineAt(row));
      else state.pending.push(lineAt(row));
    }
  };

  /** Land one page line, upserting by key and keeping first-insert position. */
  const upsertPageLine = (upsertKey: string, line: storev1.StorePageLine): void => {
    const bookId = line.pageAgentId?.value ?? "";
    const existing = rowsByKey.get(upsertKey);
    if (existing !== undefined) {
      existing.line = line;
      fanOut(bookId, existing);
      return;
    }
    const row: StoredRow = { pointer: String(nextPointer++), upsertKey, line };
    rowsByKey.set(upsertKey, row);
    const book = books.get(bookId) ?? [];
    book.push(row);
    books.set(bookId, book);
    fanOut(bookId, row);
  };

  /** Remember what an entry says about live work, for GetLiveWork. */
  const recordLiveness = (line: storev1.StorePageLine): void => {
    const item = line.agentItem?.item;
    if (item?.case !== "agentFrame") return;
    const frame = item.value;
    if (frame.result.case !== "detachedWork") return;
    const detached = frame.result.value;
    const workId = detached.work?.value;
    if (workId === undefined || workId === "") return;
    const runId =
      detached.origin.case === "detached" ? detached.origin.value.detachedFromId?.value : undefined;
    detachedAnnounced.set(workId, runId);
  };

  const landEntry = (entry: storev1.StoreEntry): void => {
    const update = entry.entry;
    if (update.case === "sessionUpdate") {
      sessionUpdateRows.push(update.value);
      return;
    }
    if (update.case !== "agentUpdate") return;
    const info = update.value.agentInfo;
    switch (info.case) {
      case "serveableFrame":
        recordLiveness(info.value);
        upsertPageLine(entry.upsertKey, info.value);
        return;
      case "unservedItem":
        unservedRows.push(info.value);
        return;
      case "bash": {
        const run = info.value.run?.value;
        if (run === undefined || run === "" || info.value.frame === undefined) return;
        const rows = bashRowsByRun.get(run) ?? [];
        rows.push(info.value);
        bashRowsByRun.set(run, rows);
        for (const watcher of bashWatchers) {
          if (watcher.run !== run || watcher.closed) continue;
          const waiter = watcher.waiters.shift();
          if (waiter !== undefined) waiter(info.value);
          else watcher.pending.push(info.value);
        }
        return;
      }
      default:
        // `workflow` is never written this wave; an unset arm is the producer's bug.
        return;
    }
  };

  /** A book's rows, oldest first. */
  const rowsOf = (agentId: string): StoredRow[] => books.get(agentId) ?? [];

  const routes = (router: ConnectRouter): void => {
    router.service(storev1.ShimStore, {
      async openAgentSession(request) {
        noteRead("OpenAgentSession", request);
        const bookId = request.agent?.value ?? "";
        const all = rowsOf(bookId);
        const floorPointer =
          request.knownThrough === undefined ? -1 : Number(request.knownThrough.value);
        const eligible = all.filter((row) => Number(row.pointer) > floorPointer);
        const budget = request.pageSize;
        // Newest first: take from the end, then reverse.
        const window = eligible.slice(Math.max(0, eligible.length - budget));
        const lines = [...window].reverse().map(lineAt);
        const olderExist = eligible.length > window.length;
        const oldestInPage = window[0];
        const newestOverall = all[all.length - 1];
        const token = `watch-${nextToken++}`;
        watches.set(token, {
          book: bookId,
          pinnedAfter: newestOverall === undefined ? 0 : Number(newestOverall.pointer),
          pending: [],
          waiters: [],
          closed: false,
        });
        return create(storev1.OpenAgentSessionResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.OpenAgentSessionSuccessSchema, {
              page: create(storev1.AgentSessionPageSchema, {
                lines,
                boundary:
                  olderExist && oldestInPage !== undefined
                    ? {
                        case: "more",
                        value: create(storev1.ReadAgentPageMoreSchema, {
                          lastItem: pointerOf(oldestInPage),
                        }),
                      }
                    : { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
              }),
              watch: create(storev1.AgentSessionTokenSchema, { value: token }),
            }),
          },
        });
      },

      async *watchAgentSession(request) {
        const token = request.watch?.value ?? "";
        const state = watches.get(token);
        if (state === undefined) {
          // A refused stream OPEN closes at the transport: the response type is
          // the frame, so there is nowhere in the message to say "no".
          throw new ConnectError(
            `store: no reading session for watch token ${JSON.stringify(token)}`,
            Code.NotFound,
          );
        }
        while (!state.closed) {
          const next = state.pending.shift();
          if (next !== undefined) {
            yield create(storev1.WatchAgentSessionResponseSchema, { line: next });
            continue;
          }
          const awaited = await new Promise<storev1.StoreLineAt | null>((resolve) => {
            state.waiters.push(resolve as (line: storev1.StoreLineAt) => void);
            if (state.closed) resolve(null);
          });
          if (awaited === null) return;
          yield create(storev1.WatchAgentSessionResponseSchema, { line: awaited });
        }
      },

      async *watchBashRun(request) {
        noteRead("WatchBashRun", request);
        const run = request.run?.value ?? "";
        const stored = bashRowsByRun.get(run);
        if (stored === undefined || stored.length === 0) {
          // A REFUSED OPEN closes at the transport: the response type is the
          // row, so there is nowhere in the message to say "no such run".
          throw new ConnectError(
            `store: no stored rows for bash run ${JSON.stringify(run)}`,
            Code.NotFound,
          );
        }
        const watcher = {
          run,
          // REPLAY FIRST, in write order: a snapshot taken now, so a row written
          // while the replay is being consumed lands in `pending` behind it.
          pending: [...stored],
          waiters: [] as Array<(row: storev1.StoreAgentBash | null) => void>,
          closed: false,
        };
        bashWatchers.add(watcher);
        try {
          for (;;) {
            const next = watcher.pending.shift();
            if (next !== undefined) {
              yield create(storev1.WatchBashRunResponseSchema, { row: next });
              const arm = next.frame?.result.case;
              // ENDS AFTER THE TERMINAL: the run's stream is bounded.
              if (arm === "success" || arm === "failure") return;
              continue;
            }
            if (watcher.closed) return;
            const awaited = await new Promise<storev1.StoreAgentBash | null>((resolve) => {
              watcher.waiters.push(resolve);
              if (watcher.closed) resolve(null);
            });
            if (awaited === null) return;
            yield create(storev1.WatchBashRunResponseSchema, { row: awaited });
            const arm = awaited.frame?.result.case;
            if (arm === "success" || arm === "failure") return;
          }
        } finally {
          watcher.closed = true;
          bashWatchers.delete(watcher);
        }
      },

      async readAgentPage(request) {
        noteRead("ReadAgentPage", request);
        const bookId = request.book?.value ?? "";
        const after = Number(request.after?.value ?? "0");
        // Strictly OLDER than `after`, newest first.
        const older = rowsOf(bookId).filter((row) => Number(row.pointer) < after);
        const window = older.slice(Math.max(0, older.length - request.pageSize));
        // EVERY LINE CARRIES ITS OWN POINTER (landing 3): a continuation page
        // is a reconnect mark like any other.
        const lines = [...window].reverse().map(lineAt);
        const oldestInPage = window[0];
        const olderExist = older.length > window.length;
        return create(storev1.ReadAgentPageResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.ReadAgentPageSuccessSchema, {
              lines,
              boundary:
                olderExist && oldestInPage !== undefined
                  ? {
                      case: "more",
                      value: create(storev1.ReadAgentPageMoreSchema, {
                        lastItem: pointerOf(oldestInPage),
                      }),
                    }
                  : { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
            }),
          },
        });
      },

      getWorkflow() {
        // WORKFLOW IS KICKED. The store's own verb answers the same way the
        // shim's does, so nothing in the stack can half-support it by accident.
        throw new ConnectError(
          "store.v1.GetWorkflow is not implemented in this wave (workflow is kicked)",
          Code.Unimplemented,
        );
      },

      async getSidecarCursors(request) {
        noteRead("GetSidecarCursors", request);
        // No sidecar is running behind this fake, so it has read no files and
        // holds no cursors. An empty SUCCESS, never a failure: "nothing yet" is
        // an answer, not an error.
        return create(storev1.GetSidecarCursorsResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.GetSidecarCursorsSuccessSchema, { cursors: [] }),
          },
        });
      },

      async getLiveWork(request) {
        noteRead("GetLiveWork", request);
        const liveAgents: conversationv1.AgentId[] = [];
        for (const [bookId, rows] of books) {
          if (bookId === "") continue;
          let started = false;
          let concluded = false;
          for (const row of rows) {
            const item = row.line.agentItem?.item;
            if (item?.case !== "agentFrame") continue;
            const arm = item.value.result.case;
            if (arm === "update" || arm === "detachedWork") started = true;
            if (arm === "success" || arm === "failure") concluded = true;
          }
          if (started && !concluded) {
            liveAgents.push(create(conversationv1.AgentIdSchema, { value: bookId }));
          }
        }
        const liveDetached: conversationv1.DetachedWorkId[] = [];
        for (const [workId, runId] of detachedAnnounced) {
          const rows = runId === undefined ? undefined : bashRowsByRun.get(runId);
          const newest = rows?.[rows.length - 1]?.frame;
          const ended = newest?.result.case === "success" || newest?.result.case === "failure";
          if (!ended) {
            liveDetached.push(create(conversationv1.DetachedWorkIdSchema, { value: workId }));
          }
        }
        return create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.GetLiveWorkSuccessSchema, {
              liveAgents,
              // Workflow is kicked, so nothing ever writes a workflow row and
              // the live set is empty by construction rather than by omission.
              liveWorkflows: [],
              liveDetached,
            }),
          },
        });
      },

      async writeBatch(request) {
        receivedWrites.push(request);
        if (writeFailure !== null) {
          // DURABLE OR NOTHING: a failed batch lands no entry at all, which is
          // what makes a whole-batch retry correct rather than duplicating.
          return create(storev1.WriteBatchResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.WriteBatchFailureSchema, { detail: writeFailure }),
            },
          });
        }
        for (const entry of request.batch?.entries ?? []) landEntry(entry);
        return create(storev1.WriteBatchResponseSchema, {
          result: { case: "success", value: create(storev1.WriteBatchSuccessSchema, {}) },
        });
      },
    });
  };

  const server = http.createServer(connectNodeAdapter({ routes }));
  const open = new Set<import("node:net").Socket>();
  server.on("connection", (socket) => {
    open.add(socket);
    socket.once("close", () => open.delete(socket));
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen({ path: socketPath }, () => resolve());
  });

  return {
    socketPath,
    failWrites: (detail) => {
      writeFailure = detail;
    },
    writes: () => [...receivedWrites],
    book: (agentId) => rowsOf(agentId).map(lineAt),
    sessionUpdates: () => [...sessionUpdateRows],
    unserved: () => [...unservedRows],
    reads: () => [...servedReads],
    close: async () => {
      // End every parked tail first: a watcher blocked on its promise would
      // otherwise keep the server's close callback from ever firing.
      for (const state of watches.values()) {
        state.closed = true;
        for (const waiter of state.waiters.splice(0)) {
          (waiter as unknown as (line: storev1.StoreLineAt | null) => void)(null);
        }
      }
      for (const watcher of bashWatchers) {
        watcher.closed = true;
        for (const waiter of watcher.waiters.splice(0)) waiter(null);
      }
      await new Promise<void>((resolve) => {
        server.close(() => resolve());
        for (const socket of open) socket.destroy();
        open.clear();
      });
      try {
        unlinkSync(socketPath);
      } catch {
        // Already gone; nothing to clean up.
      }
    },
  };
}
