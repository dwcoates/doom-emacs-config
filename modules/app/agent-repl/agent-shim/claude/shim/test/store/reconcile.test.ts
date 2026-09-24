/**
 * The OPEN OBLIGATIONS, and the honest closing terminals.
 *
 * The invariant under test is "every started thing eventually gets a terminal
 * row, by observation or by reconciliation" — so the assertions are about which
 * ARM a reconciled ending takes, because the arm is what a reader is told
 * happened.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { writeSync } from "node:fs";
import { create } from "@bufbuild/protobuf";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { producerId } from "../../src/store/keys.js";
import { PersistenceError } from "../../src/store/persistence.js";
import {
  closingMonitorTerminal,
  findMonitorCall,
  announceLiveWork,
  createReconciler,
  findBashStart,
  findUnit,
  reconciledCoordinate,
  stoppedBashTerminal,
} from "../../src/store/reconcile.js";
import { createPersistence } from "../../src/store/writer.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import { agent, readEntry, socketPathForTest, spawnEntry, unit } from "./persistence-fixtures.js";

const PRODUCER = producerId("vendor-session-1");
const BOOK = agent("book-1");
/** This session's main agent: the root every live-work read is scoped to. */
const MAIN = agent("main-1");
const RUN = unit("run-1");

/**
 * The backoff, taken instantly.
 *
 * A read now replays on the store's retry schedule; the schedule itself is
 * asserted in `test/store/retry.test.ts`, and waiting it out here would cost
 * seconds per refusal.
 */
const instantly = (): Promise<void> => Promise.resolve();

let store: FakeStore | undefined;

afterEach(async () => {
  await store?.close();
  store = undefined;
});

async function reconciler(name: string) {
  const started = await startFakeStore(socketPathForTest(name));
  store = started;
  const client = createStoreClient(started.socketPath);
  return { started, client, reconciler: createReconciler({ client, sleep: instantly }) };
}

/** A store client whose every verb but the ones given throws if called. */
function stubClient(overrides: Partial<StoreClient>): StoreClient {
  const refuse = (): never => {
    throw new Error("stub store client: this suite did not expect that call");
  };
  return {
    openAgentSession: refuse,
    watchAgentSession: refuse,
    watchBashRun: refuse,
    readAgentPage: refuse,
    getWorkflow: refuse,
    getSidecarCursors: refuse,
    getLiveWork: refuse,
    writeBatch: refuse,
    ...overrides,
  };
}

/** Every structured record the logger wrote since `before`. */
function recordsSince(before: number): Record<string, unknown>[] {
  const calls = vi.mocked(writeSync).mock.calls as unknown as [number, Buffer, number, number][];
  return calls.slice(before).map(([, bytes, offset, length]) => {
    return JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<
      string,
      unknown
    >;
  });
}

/** One recorded bash start, as it would come back from the agent's own book. */
function recordedBashStart(line: string): conversationv1.HistoryEntryAt {
  return create(conversationv1.HistoryEntryAtSchema, {
    at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
    entry: create(conversationv1.HistoryEntrySchema, {
      entry: {
        case: "agentFrame",
        value: create(conversationv1.AgentFrameSchema, {
          agentId: BOOK,
          result: {
            case: "update",
            value: create(conversationv1.AgentUpdateSchema, {
              update: {
                case: "activity",
                value: create(conversationv1.AgentActivitySchema, {
                  activityId: RUN,
                  item: {
                    case: "bash",
                    value: create(conversationv1.AgentBashSchema, {
                      result: {
                        case: "start",
                        value: create(conversationv1.AgentBashStartSchema, {
                          command: create(conversationv1.AgentBashCommandSchema, { line }),
                          startedAt: create(conversationv1.AgentActivityStartedAtSchema, {
                            atMs: 5_000n,
                          }),
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
  });
}

describe("liveWork", () => {
  it("answers the session's open obligations", async () => {
    const { started, client, reconciler: plane } = await reconciler("live-empty");
    const writer = createPersistence({
      client,
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    writer.write([spawnEntry(MAIN, "book-1"), readEntry(BOOK, "unit-1", "/tmp/a")]);
    await writer.flush();
    void started;

    const live = await plane.liveWork(MAIN);

    expect(live.liveAgents.map((id) => id.value)).toContain("book-1");
  });

  it("never answers another session's open obligations", async () => {
    // Arrange: ONE store serves every session on the host.
    const { client, reconciler: plane } = await reconciler("live-other-session");
    const writer = createPersistence({
      client,
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    writer.write([spawnEntry(agent("main-other"), "other-sub")]);
    await writer.flush();

    // Act.
    const live = await plane.liveWork(MAIN);

    // Assert.
    expect(live.liveAgents).toEqual([]);
  });

  it("asks the store for exactly the session it was given", async () => {
    // Arrange.
    const asked: storev1.GetLiveWorkRequest[] = [];
    const client = stubClient({
      getLiveWork: async (request) => {
        asked.push(request);
        return create(storev1.GetLiveWorkResponseSchema, {
          result: { case: "success", value: create(storev1.GetLiveWorkSuccessSchema, {}) },
        });
      },
    });

    // Act.
    await createReconciler({ client, sleep: instantly }).liveWork(MAIN);

    // Assert.
    expect(asked.map((request) => request.session?.value)).toEqual(["main-1"]);
  });

  it("refuses an empty session as invalid_request before the store is asked", async () => {
    // Arrange.
    let asked = 0;
    const client = stubClient({
      getLiveWork: async () => {
        asked += 1;
        return create(storev1.GetLiveWorkResponseSchema, {});
      },
    });

    // Act.
    const read = createReconciler({ client, sleep: instantly }).liveWork(agent(""));

    // Assert.
    await expect(read).rejects.toMatchObject({ kind: "invalid_request" });
    expect(asked).toBe(0);
  });

  it("logs the refusal of an empty session at error", async () => {
    // Arrange.
    const client = stubClient({});
    const before = vi.mocked(writeSync).mock.calls.length;

    // Act.
    await createReconciler({ client, sleep: instantly })
      .liveWork(agent(""))
      .catch(() => undefined);

    // Assert.
    expect(recordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "error",
        message: "refusing an open-obligation read that names no session",
      }),
    );
  });

  it("surfaces the store's invalid_request as invalid_request, never as a missing book", async () => {
    // Arrange: `unknown_agent` is read upstream as "no book yet" and served as
    // an EMPTY set, which would silently leave every obligation unresolved.
    const client = stubClient({
      getLiveWork: async () =>
        create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.GetLiveWorkFailureSchema, {
              detail: "session: GetLiveWork names no session",
              kind: {
                case: "invalidRequest",
                value: create(storev1.GetLiveWorkInvalidRequestSchema, { field: "session" }),
              },
            }),
          },
        }),
    });

    // Act.
    const read = createReconciler({ client, sleep: instantly }).liveWork(MAIN);

    // Assert.
    await expect(read).rejects.toMatchObject({ kind: "invalid_request" });
  });

  it("does not replay a request the store refused as malformed", async () => {
    // Arrange: the same bytes are refused again, so the retry schedule would
    // only delay a defect.
    let asked = 0;
    const client = stubClient({
      getLiveWork: async () => {
        asked += 1;
        return create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.GetLiveWorkFailureSchema, {
              detail: "session: GetLiveWork names no session",
              kind: {
                case: "invalidRequest",
                value: create(storev1.GetLiveWorkInvalidRequestSchema, { field: "session" }),
              },
            }),
          },
        });
      },
    });

    // Act.
    await createReconciler({ client, sleep: instantly })
      .liveWork(MAIN)
      .catch(() => undefined);

    // Assert.
    expect(asked).toBe(1);
  });

  it("logs the store's invalid_request at error, naming the field", async () => {
    // Arrange.
    const client = stubClient({
      getLiveWork: async () =>
        create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.GetLiveWorkFailureSchema, {
              detail: "session: GetLiveWork names no session",
              kind: {
                case: "invalidRequest",
                value: create(storev1.GetLiveWorkInvalidRequestSchema, { field: "session" }),
              },
            }),
          },
        }),
    });
    const before = vi.mocked(writeSync).mock.calls.length;

    // Act.
    await createReconciler({ client, sleep: instantly })
      .liveWork(MAIN)
      .catch(() => undefined);

    // Assert.
    expect(recordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "error",
        message: "the store refused the open-obligation read as malformed",
      }),
    );
  });

  it("raises a PersistenceError when the store refuses", async () => {
    const refusing: StoreClient = {
      openAgentSession: async () => {
        throw new Error("unused");
      },
      watchAgentSession: () => {
        throw new Error("unused");
      },
      watchBashRun: () => {
        throw new Error("unused");
      },
      readAgentPage: async () => {
        throw new Error("unused");
      },
      getWorkflow: async () => {
        throw new Error("unused");
      },
      getSidecarCursors: async () => {
        throw new Error("unused");
      },
      getLiveWork: async () =>
        create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.GetLiveWorkFailureSchema, { detail: "the disk is full" }),
          },
        }),
      writeBatch: async () => {
        throw new Error("unused");
      },
    };

    await expect(createReconciler({ client: refusing, sleep: instantly }).liveWork(MAIN)).rejects.toBeInstanceOf(
      PersistenceError,
    );
  });

  // THE READ THAT COST A DAY OF BRING-UPS. One `SQLITE_BUSY` on `begin read
  // transaction` used to be a permanent session fault; it is one busy moment.
  it("answers once a momentarily busy database lets go", async () => {
    // Arrange: the store is busy for its first answer only.
    let asked = 0;
    const busyOnce: StoreClient = {
      openAgentSession: async () => {
        throw new Error("unused");
      },
      watchAgentSession: () => {
        throw new Error("unused");
      },
      watchBashRun: () => {
        throw new Error("unused");
      },
      readAgentPage: async () => {
        throw new Error("unused");
      },
      getWorkflow: async () => {
        throw new Error("unused");
      },
      getSidecarCursors: async () => {
        throw new Error("unused");
      },
      getLiveWork: async () => {
        asked += 1;
        if (asked > 1) {
          return create(storev1.GetLiveWorkResponseSchema, {
            result: { case: "success", value: create(storev1.GetLiveWorkSuccessSchema, {}) },
          });
        }
        return create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.GetLiveWorkFailureSchema, {
              detail: "storage failure: begin read transaction: database is locked (5) (SQLITE_BUSY)",
              kind: {
                case: "storageFailure",
                value: create(storev1.GetLiveWorkStorageFailureSchema, {}),
              },
            }),
          },
        });
      },
      writeBatch: async () => {
        throw new Error("unused");
      },
    };

    // Act.
    const answer = await createReconciler({ client: busyOnce, sleep: instantly }).liveWork(MAIN);

    // Assert.
    expect(asked).toBe(2);
    expect(answer.liveDetached).toEqual([]);
  });

  it("raises loudly when the store answers with no result arm", async () => {
    const empty: StoreClient = {
      openAgentSession: async () => {
        throw new Error("unused");
      },
      watchAgentSession: () => {
        throw new Error("unused");
      },
      watchBashRun: () => {
        throw new Error("unused");
      },
      readAgentPage: async () => {
        throw new Error("unused");
      },
      getWorkflow: async () => {
        throw new Error("unused");
      },
      getSidecarCursors: async () => {
        throw new Error("unused");
      },
      getLiveWork: async () => create(storev1.GetLiveWorkResponseSchema, {}),
      writeBatch: async () => {
        throw new Error("unused");
      },
    };

    await expect(createReconciler({ client: empty, sleep: instantly }).liveWork(MAIN)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });
});

describe("closingAgentTerminal", () => {
  it("closes an agent the record holds no terminal for as swept up by the boot sweep", async () => {
    const { reconciler: plane } = await reconciler("close-agent");

    const entry = plane.closingAgentTerminal(BOOK);

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const failure = frame?.result.value as conversationv1.AgentFailure;
    expect(failure.failure.case).toBe("lost");
    const lost = failure.failure.value as conversationv1.DetachedLost;
    expect(lost.how.case).toBe("sweptUp");
  });

  it("accuses nobody: never a user stop, never an execution error", async () => {
    const { reconciler: plane } = await reconciler("close-agent-not-user");

    const entry = plane.closingAgentTerminal(BOOK);

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    expect(frame?.result.case).toBe("failure");
    const failure = frame?.result.value as conversationv1.AgentFailure;
    expect(failure.failure.case).not.toBe("executionError");
    expect(failure.errors).toEqual([]);
  });

  it("closes the SPAWN unit too, since the spawn bubble is a second row", async () => {
    const { reconciler: plane } = await reconciler("close-spawn");

    const entry = plane.closingSubagentTerminal(BOOK, RUN);

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const activity = update.value as conversationv1.AgentActivity;
    const subagent = activity.item.value as conversationv1.AgentSubagent;
    const failed = subagent.result.value as conversationv1.AgentSubagentFailure;
    expect(failed.cause.case).toBe("lost");
  });

  it("keys the row deterministically, so a second reconciliation upserts one ending", async () => {
    const { reconciler: plane } = await reconciler("close-agent-key");

    const first = plane.closingAgentTerminal(BOOK);
    const second = plane.closingAgentTerminal(BOOK);

    expect(second.upsertKey).toBe(first.upsertKey);
    expect(first.source.vendorUuid).toBe(reconciledCoordinate("book-1"));
  });
});

/** A monitor unit recorded in BOOK under RUN, in the given result arm. */
function recordedMonitor(result: conversationv1.AgentMonitor["result"]): conversationv1.HistoryEntryAt {
  return create(conversationv1.HistoryEntryAtSchema, {
    at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
    entry: create(conversationv1.HistoryEntrySchema, {
      entry: {
        case: "agentFrame",
        value: create(conversationv1.AgentFrameSchema, {
          agentId: BOOK,
          result: {
            case: "update",
            value: create(conversationv1.AgentUpdateSchema, {
              update: {
                case: "activity",
                value: create(conversationv1.AgentActivitySchema, {
                  activityId: RUN,
                  item: { case: "monitor", value: create(conversationv1.AgentMonitorSchema, { result }) },
                }),
              },
            }),
          },
        }),
      },
    }),
  });
}

const ARMED = create(conversationv1.AgentMonitorStartSchema, {
  description: "watch the log",
  source: { case: "command", value: create(conversationv1.AgentMonitorCommandSchema, { command: "tail -f log" }) },
});

describe("findMonitorCall", () => {
  it("answers the recorded start", () => {
    expect(findMonitorCall([recordedMonitor({ case: "start", value: ARMED })], RUN)).toEqual(ARMED);
  });

  it("answers the call a settled arm restated", () => {
    const ended = create(conversationv1.AgentMonitorEndedSchema, { call: ARMED });
    expect(findMonitorCall([recordedMonitor({ case: "ended", value: ended })], RUN)).toEqual(ARMED);
  });

  it("answers nothing for a unit that is not a monitor", () => {
    expect(findMonitorCall([recordedBashStart("sleep 1")], RUN)).toBeUndefined();
  });
});

describe("closingMonitorTerminal", () => {
  it("restates the call it was handed", () => {
    const entry = closingMonitorTerminal(BOOK, RUN, ARMED);

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const monitor = (update.value as conversationv1.AgentActivity).item.value as conversationv1.AgentMonitor;
    expect((monitor.result.value as conversationv1.AgentMonitorEnded).call).toEqual(ARMED);
  });

  it("ends the watch with the monitor's own ended arm", () => {
    const entry = closingMonitorTerminal(BOOK, RUN, undefined);

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    const activity = update.value as conversationv1.AgentActivity;
    expect(activity.item.case).toBe("monitor");
    expect((activity.item.value as conversationv1.AgentMonitor).result.case).toBe("ended");
  });

  it("keys the row by the monitor's own unit, so the unit concludes in place", () => {
    const entry = closingMonitorTerminal(BOOK, RUN, undefined);

    expect(entry.upsertKey).toBe(`activity:${RUN.value}`);
  });
});

describe("closingBashTerminal", () => {
  it("restates the command recovered from the record", async () => {
    const { reconciler: plane } = await reconciler("close-bash");
    const start = findBashStart([recordedBashStart("sleep 100")], RUN);

    const entry = plane.closingBashTerminal(BOOK, RUN, start as conversationv1.AgentBashStart);

    const frame = entry.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    expect(success.command?.line).toBe("sleep 100");
  });

  it("settles the run as interrupted because we lost sight of it, not because it failed", async () => {
    const { reconciler: plane } = await reconciler("close-bash-cause");
    const start = findBashStart([recordedBashStart("sleep 100")], RUN);

    const entry = plane.closingBashTerminal(BOOK, RUN, start as conversationv1.AgentBashStart);

    const frame = entry.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(success.outcome.case).toBe("interrupted");
    expect(interrupted.cause.case).toBe("lost");
    const lost = interrupted.cause.value as conversationv1.DetachedLost;
    expect(lost.how.case).toBe("sweptUp");
  });

  it("states not_observed for output the reconciliation never saw", async () => {
    // LANDING 5: the producer says it does not know, rather than claiming an
    // omission of zero bytes — which read as "we saw all none of it".
    const { reconciler: plane } = await reconciler("close-bash-output");
    const start = findBashStart([recordedBashStart("sleep 100")], RUN);

    const entry = plane.closingBashTerminal(BOOK, RUN, start as conversationv1.AgentBashStart);

    const frame = entry.item.kind === "bash_run" ? entry.item.frame : undefined;
    const success = frame?.result.value as conversationv1.AgentBashSuccess;
    const interrupted = success.outcome.value as conversationv1.AgentBashInterrupted;
    expect(interrupted.output?.form.case).toBe("notObserved");
  });

  it("refuses to invent a command when the record holds no start", async () => {
    const { reconciler: plane } = await reconciler("close-bash-no-start");

    expect(() =>
      plane.closingBashTerminal(BOOK, RUN, create(conversationv1.AgentBashStartSchema, {})),
    ).toThrow(PersistenceError);
  });
});

describe("findBashStart", () => {
  it("finds the run's own start among a book's entries", () => {
    const start = findBashStart([recordedBashStart("sleep 100")], RUN);

    expect(start?.command?.line).toBe("sleep 100");
  });

  it("answers nothing for a run the book does not hold", () => {
    const start = findBashStart([recordedBashStart("sleep 100")], unit("run-other"));

    expect(start).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// What a restarted consumer is told about work that is still running
// ---------------------------------------------------------------------------

describe("announceLiveWork", () => {
  /** One recorded shell run in a book, as the store holds it. */
  function recordedRun(unit: string): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            agentId: BOOK,
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: unit }),
                    item: {
                      case: "bash",
                      value: create(conversationv1.AgentBashSchema, {
                        result: {
                          case: "start",
                          value: create(conversationv1.AgentBashStartSchema, {
                            command: create(conversationv1.AgentBashCommandSchema, {
                              line: "sleep 100",
                            }),
                            startedAt: create(conversationv1.AgentActivityStartedAtSchema, {
                              atMs: 5n,
                            }),
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
    });
  }

  const HANDLE = create(conversationv1.DetachedWorkIdSchema, { value: "run-1" });

  it("uses the CREATED arm: a restarted daemon has no element to continue", () => {
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE], BOOK);

    // `detached` means "continue what you are drawing"; telling a fresh
    // consumer that leaves the work undrawn and unreachable.
    expect(announced[0]?.origin.case).toBe("created");
  });

  it("describes the work FROM THE STORE, by the unit the handle names", () => {
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE], BOOK);

    const created = announced[0]?.origin.value as conversationv1.DetachedWorkCreated;
    expect(created.workCreated?.work.case).toBe("bash");
  });

  it("carries the recorded start, so the description cannot disagree with the record", () => {
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE], BOOK);

    const created = announced[0]?.origin.value as conversationv1.DetachedWorkCreated;
    const bash = created.workCreated?.work.value as conversationv1.AgentBash;
    const start = bash.result.value as conversationv1.AgentBashStart;
    expect(start.command?.line).toBe("sleep 100");
  });

  it("names the book's agent as the work's OWNER, so it is drawn in that agent's feed", () => {
    // Arrange: the start was found in BOOK's own record.
    const records = [recordedRun("run-1")];

    // Act.
    const announced = announceLiveWork(records, [HANDLE], BOOK);

    // Assert.
    expect(announced[0]?.owner?.value).toBe(BOOK.value);
  });

  it("omits work the record cannot describe rather than announcing a bare handle", () => {
    expect(announceLiveWork([], [HANDLE], BOOK)).toEqual([]);
  });

  // WHO OWNS THE RECORD is whoever can tell the two cases apart, and this
  // function cannot: the obligation set spans the session's whole lineage, so
  // an undescribable handle is either a lost start or a subagent's work.
  it("hands an undescribable handle to a caller that asked for it", () => {
    // Arrange.
    const undescribed: conversationv1.DetachedWorkId[] = [];

    // Act.
    const announced = announceLiveWork([], [HANDLE], BOOK, (handle) => {
      undescribed.push(handle);
    });

    // Assert.
    expect(announced).toEqual([]);
    expect(undescribed.map((handle) => handle.value)).toEqual(["run-1"]);
  });

  it("does not hand over a handle it could describe", () => {
    // Arrange.
    const undescribed: conversationv1.DetachedWorkId[] = [];

    // Act.
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE], BOOK, (handle) => {
      undescribed.push(handle);
    });

    // Assert.
    expect(announced).toHaveLength(1);
    expect(undescribed).toEqual([]);
  });

  it("omits a unit whose kind cannot detach at all", () => {
    const read = create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            agentId: BOOK,
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: "run-1" }),
                    item: {
                      case: "read",
                      value: create(conversationv1.AgentReadSchema, {}),
                    },
                  }),
                },
              }),
            },
          }),
        },
      }),
    });

    // A kind absent from DetachableWork cannot claim to be detached.
    expect(announceLiveWork([read], [HANDLE], BOOK)).toEqual([]);
  });
});

describe("stoppedBashTerminal", () => {
  it("closes the run as interrupted by the user, with the output unobserved", () => {
    const command = create(conversationv1.AgentBashCommandSchema, { line: "sleep 100" });
    const run = create(conversationv1.AgentActivityIdSchema, { value: "run-1" });

    const entry = stoppedBashTerminal(agent("book-1"), run, command);

    expect(entry.item.kind).toBe("bash_run");
    const frame = entry.item.kind === "bash_run" ? entry.item.frame : undefined;
    expect(frame?.result.case).toBe("success");
    const success = frame?.result.value as conversationv1.AgentBashSuccess | undefined;
    expect(success?.outcome.case).toBe("interrupted");
    const interrupted = success?.outcome.value as conversationv1.AgentBashInterrupted | undefined;
    expect(interrupted?.cause.case).toBe("byUser");
    expect(interrupted?.output?.form.case).toBe("notObserved");
  });
});

// ---------------------------------------------------------------------------
// findUnit: the plain search over the pages the caller already has
// ---------------------------------------------------------------------------

describe("findUnit", () => {
  /** One history entry carrying whatever `entry` arm is given. */
  function entryAt(entry: conversationv1.HistoryEntry["entry"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      entry: create(conversationv1.HistoryEntrySchema, { entry }),
    });
  }

  /** One agent frame carrying whatever `result` arm is given. */
  function frameAt(result: conversationv1.AgentFrame["result"]): conversationv1.HistoryEntryAt {
    return entryAt({
      case: "agentFrame",
      value: create(conversationv1.AgentFrameSchema, { agentId: BOOK, result }),
    });
  }

  const cases: readonly {
    readonly name: string;
    readonly entry: conversationv1.HistoryEntryAt;
  }[] = [
    {
      name: "an entry that is not an agent frame at all",
      entry: entryAt({
        case: "userPrompt",
        value: create(conversationv1.AgentPromptSchema, { agent: BOOK }),
      }),
    },
    {
      name: "a frame that is not an update at all",
      entry: frameAt({
        case: "success",
        value: create(conversationv1.AgentSuccessSchema, {}),
      }),
    },
    {
      name: "an update that is not an activity",
      entry: frameAt({
        case: "update",
        value: create(conversationv1.AgentUpdateSchema, {
          update: {
            case: "contextCut",
            value: create(conversationv1.ContextCutSchema, {}),
          },
        }),
      }),
    },
  ];

  for (const testCase of cases) {
    it(`walks past ${testCase.name}`, () => {
      // Arrange, Act.
      const found = findUnit([testCase.entry], RUN);

      // Assert.
      expect(found).toBeUndefined();
    });
  }
});

describe("findBashStart on a unit that is not at its start", () => {
  it("answers nothing for a shell run the record already holds settled", () => {
    // A terminal restates the command, but it is not the START arm -- and the
    // reconciler asks only for the start it must restate.
    // Arrange.
    const settled = create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            agentId: BOOK,
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: RUN,
                    item: {
                      case: "bash",
                      value: create(conversationv1.AgentBashSchema, {
                        result: {
                          case: "tail",
                          value: create(conversationv1.AgentBashTailSchema, {
                            text: "working\n",
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
    });

    // Act, Assert.
    expect(findBashStart([settled], RUN)).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// The other two kinds of work that can detach
// ---------------------------------------------------------------------------

describe("announceLiveWork for the non-shell kinds", () => {
  /** One recorded unit of `item`'s kind, keyed as `run-1`. */
  function recorded(
    item: conversationv1.AgentActivity["item"],
  ): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            agentId: BOOK,
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, { activityId: RUN, item }),
                },
              }),
            },
          }),
        },
      }),
    });
  }

  const HANDLE = create(conversationv1.DetachedWorkIdSchema, { value: "run-1" });

  it("describes a started SUBAGENT as the subagent arm of DetachableWork", () => {
    // Arrange.
    const entry = recorded({
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentSubagentStartSchema, {}),
        },
      }),
    });

    // Act.
    const announced = announceLiveWork([entry], [HANDLE], BOOK);

    // Assert.
    const created = announced[0]?.origin.value as conversationv1.DetachedWorkCreated;
    expect(created.workCreated?.work.case).toBe("subagent");
  });

  it("describes a started MONITOR as the monitor arm of DetachableWork", () => {
    // Arrange.
    const entry = recorded({
      case: "monitor",
      value: create(conversationv1.AgentMonitorSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentMonitorStartSchema, {}),
        },
      }),
    });

    // Act.
    const announced = announceLiveWork([entry], [HANDLE], BOOK);

    // Assert.
    const created = announced[0]?.origin.value as conversationv1.DetachedWorkCreated;
    expect(created.workCreated?.work.case).toBe("monitor");
  });

  it("omits a subagent the record holds no start for, rather than inventing one", () => {
    // An announcement with an invented description is worse than a missing one.
    // Arrange.
    const entry = recorded({
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {}),
    });

    // Act, Assert.
    expect(announceLiveWork([entry], [HANDLE], BOOK)).toEqual([]);
  });
});

describe("liveWork against a store that cannot be reached", () => {
  it("reports the transport failure as store_unavailable", async () => {
    // Arrange.
    const refuse = (): never => {
      throw new Error("stub store client: this suite did not expect that call");
    };
    const client: StoreClient = {
      openAgentSession: refuse,
      watchAgentSession: refuse,
      watchBashRun: refuse,
      readAgentPage: refuse,
      getWorkflow: refuse,
      getSidecarCursors: refuse,
      getLiveWork: () => Promise.reject(new Error("connect ECONNREFUSED")),
      writeBatch: refuse,
    };

    // Act, Assert.
    await expect(createReconciler({ client, sleep: instantly }).liveWork(MAIN)).rejects.toMatchObject({
      kind: "store_unavailable",
      message: "connect ECONNREFUSED",
    });
  });
});
