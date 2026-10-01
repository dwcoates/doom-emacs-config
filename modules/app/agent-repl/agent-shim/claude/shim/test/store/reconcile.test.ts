/**
 * The OPEN OBLIGATIONS, and the honest closing terminals.
 *
 * The invariant under test is "every started thing eventually gets a terminal
 * row, by observation or by reconciliation" — so the assertions are about which
 * ARM a reconciled ending takes, because the arm is what a reader is told
 * happened.
 */
import { afterEach, describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import { create } from "@bufbuild/protobuf";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { producerId } from "../../src/store/keys.js";
import { PersistenceError, type PersistEntry } from "../../src/store/persistence.js";
import {
  closingMonitorTerminal,
  findMonitorCall,
  announceLiveWork,
  bashUnitsWithoutCommand,
  createReconciler,
  findAnnouncedKind,
  revivalFate,
  findUnit,
  reconciledCoordinate,
  resumedAgentAnnouncement,
  resumedRecipient,
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
    getAgentByVendorTask: refuse,
    writeBatch: refuse,
    ...overrides,
  };
}

/** One recorded bash start, as it would come back from the agent's own book. */
/** A spawn unit as the book records its start. */
function recordedSpawnStart(): conversationv1.AgentSubagent {
  return create(conversationv1.AgentSubagentSchema, {
    result: {
      case: "start",
      value: create(conversationv1.AgentSubagentStartSchema, {
        createdAgentId: agent("agent-created"),
        prompt: create(conversationv1.AgentSubagentPromptSchema, {
          description: "tidy the docs",
          text: "Tidy every doc.",
        }),
      }),
    },
  });
}

/** A spawn unit as the book records a running beat that upserted its start. */
function recordedSpawnBeat(): conversationv1.AgentSubagent {
  return create(conversationv1.AgentSubagentSchema, {
    result: {
      case: "update",
      value: create(conversationv1.AgentSubagentUpdateSchema, {
        prompt: create(conversationv1.AgentSubagentPromptSchema, { text: "keep going" }),
      }),
    },
  });
}

/** The failure a spawn closing settles with. */
function closedSpawn(entry: PersistEntry): conversationv1.AgentSubagentFailure {
  const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
  const activity = (frame?.result.value as conversationv1.AgentUpdate).update
    .value as conversationv1.AgentActivity;
  return (activity.item.value as conversationv1.AgentSubagent).result
    .value as conversationv1.AgentSubagentFailure;
}

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
    const before = logSinkMark();

    // Act.
    await createReconciler({ client, sleep: instantly })
      .liveWork(agent(""))
      .catch(() => undefined);

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
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
    const before = logSinkMark();

    // Act.
    await createReconciler({ client, sleep: instantly })
      .liveWork(MAIN)
      .catch(() => undefined);

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
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
      getAgentByVendorTask: () => Promise.reject(new Error("unused")),
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
      getAgentByVendorTask: () => Promise.reject(new Error("unused")),
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
      getAgentByVendorTask: () => Promise.reject(new Error("unused")),
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

    const entry = plane.closingSubagentTerminal(BOOK, RUN, recordedSpawnStart());

    expect(closedSpawn(entry).cause.case).toBe("lost");
  });

  it("restates the prompt the record holds for the spawn", async () => {
    const { reconciler: plane } = await reconciler("close-spawn-prompt");

    const entry = plane.closingSubagentTerminal(BOOK, RUN, recordedSpawnStart());

    expect(closedSpawn(entry).prompt?.description).toBe("tidy the docs");
  });

  it("restates the created agent the recorded start names", async () => {
    const { reconciler: plane } = await reconciler("close-spawn-created");

    const entry = plane.closingSubagentTerminal(BOOK, RUN, recordedSpawnStart());

    expect(closedSpawn(entry).createdAgentId?.value).toBe("agent-created");
  });

  it("restates the minting rule's created agent when the record holds only a running beat", async () => {
    const { reconciler: plane } = await reconciler("close-spawn-beat");

    const entry = plane.closingSubagentTerminal(BOOK, RUN, recordedSpawnBeat());

    expect(closedSpawn(entry).createdAgentId?.value).toBe(RUN.value);
  });

  it("restates the beat's prompt when the record holds only a running beat", async () => {
    const { reconciler: plane } = await reconciler("close-spawn-beat-prompt");

    const entry = plane.closingSubagentTerminal(BOOK, RUN, recordedSpawnBeat());

    expect(closedSpawn(entry).prompt?.text).toBe("keep going");
  });

  it("stamps the closed spawn unit with the stands-alone contract", async () => {
    const { reconciler: plane } = await reconciler("close-spawn-contract");

    const entry = plane.closingSubagentTerminal(BOOK, RUN, recordedSpawnStart());

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const activity = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.AgentActivity;
    expect(activity.contract).toBe(conversationv1.AgentActivityContract.SETTLES_STAND_ALONE);
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

  it("stamps the closed monitor unit with the stands-alone contract", () => {
    const entry = closingMonitorTerminal(BOOK, RUN, undefined);

    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const activity = (frame?.result.value as conversationv1.AgentUpdate).update
      .value as conversationv1.AgentActivity;
    expect(activity.contract).toBe(conversationv1.AgentActivityContract.SETTLES_STAND_ALONE);
  });

  it("keys the row by the monitor's own unit, so the unit concludes in place", () => {
    const entry = closingMonitorTerminal(BOOK, RUN, undefined);

    expect(entry.upsertKey).toBe(`activity:${RUN.value}`);
  });
});

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

  it("states the bash kind for a recorded shell run", () => {
    // Arrange, Act.
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE], BOOK);

    // Assert.
    expect(announced[0]?.kind?.kind.case).toBe("bash");
  });

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
          value: create(conversationv1.AgentSubagentStartSchema, {
            createdAgentId: create(conversationv1.AgentIdSchema, { value: HANDLE.value }),
          }),
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

  it("states the subagent kind, running the agent the recorded start created", () => {
    // Arrange.
    const entry = recorded({
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentSubagentStartSchema, {
            createdAgentId: create(conversationv1.AgentIdSchema, { value: "toolu_spawn" }),
          }),
        },
      }),
    });

    // Act.
    const kind = announceLiveWork([entry], [HANDLE], BOOK)[0]?.kind?.kind;

    // Assert.
    expect(kind?.case === "subagent" ? kind.value.agentId?.value : "").toBe("toolu_spawn");
  });

  it("states the monitor kind for a recorded monitor", () => {
    // Arrange.
    const entry = recorded({
      case: "monitor",
      value: create(conversationv1.AgentMonitorSchema, {
        result: { case: "start", value: create(conversationv1.AgentMonitorStartSchema, {}) },
      }),
    });

    // Act, Assert.
    expect(announceLiveWork([entry], [HANDLE], BOOK)[0]?.kind?.kind.case).toBe("monitor");
  });

  it("refuses a recorded spawn start that names no created agent, at ERROR", () => {
    // Arrange.
    const entry = recorded({
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: { case: "start", value: create(conversationv1.AgentSubagentStartSchema, {}) },
      }),
    });
    const before = logSinkMark();

    // Act.
    const announced = announceLiveWork([entry], [HANDLE], BOOK);

    // Assert.
    expect(announced).toEqual([]);
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "error",
        message: "the recorded start of this live work states no announceable kind; it is not announced",
      }),
    );
  });

  it("announces a spawn whose row states no prompt by its handle and kind, at ERROR", () => {
    // Arrange.
    const entry = recorded({
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {}),
    });
    const mark = logSinkMark();

    // Act.
    const announced = announceLiveWork([entry], [HANDLE], BOOK);

    // Assert.
    const record = logRecordsSince(mark).find((r) => r.context.detail === "the spawn unit's row states no prompt");
    expect([announced.map((a) => a.kind?.kind.case), record?.level]).toEqual([["subagent"], "error"]);
  });
});

/**
 * A LIVE UNIT'S ROW HAS USUALLY MOVED PAST ITS START (2026-09-30: two live
 * background subagents vanished from an adopting daemon's footer). The start is
 * rebuilt from whatever arm the row holds.
 */
describe("announceLiveWork for a unit whose row moved past its start", () => {
  const PROMPT = create(conversationv1.AgentSubagentPromptSchema, { text: "sweep the logs" });
  const HANDLE = create(conversationv1.DetachedWorkIdSchema, { value: "run-1" });

  /** One recorded unit keyed `run-1`, first placed at `atMs`. */
  function recordedAt(item: conversationv1.AgentActivity["item"], atMs?: bigint): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
      ...(atMs === undefined
        ? {}
        : { place: { case: "recordedPlace", value: create(conversationv1.ConversationPlaceSchema, { atMs }) } }),
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

  const RUNNING_SPAWN: conversationv1.AgentActivity["item"] = {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: { case: "update", value: create(conversationv1.AgentSubagentUpdateSchema, { prompt: PROMPT }) },
    }),
  };

  const PROGRESSING_SHELL: conversationv1.AgentActivity["item"] = {
    case: "bash",
    value: create(conversationv1.AgentBashSchema, {
      result: { case: "progress", value: create(conversationv1.AgentToolCallProgressSchema, { lastProgressAtMs: 9n }) },
    }),
  };

  /** The start arm the one announcement describes. */
  function startOf(announced: conversationv1.AgentDetachedWork[]): conversationv1.DetachableWork["work"] | undefined {
    const origin = announced[0]?.origin;
    return origin?.case === "created" ? origin.value.workCreated?.work : undefined;
  }

  it("describes a running subagent from its beat, naming the agent by the minting rule", () => {
    // Arrange, Act.
    const work = startOf(announceLiveWork([recordedAt(RUNNING_SPAWN)], [HANDLE], BOOK));

    // Assert.
    const start = work?.case === "subagent" && work.value.result.case === "start" ? work.value.result.value : undefined;
    expect([start?.createdAgentId?.value, start?.prompt?.text]).toEqual(["run-1", "sweep the logs"]);
  });

  it("states the running subagent's subagent kind", () => {
    // Arrange, Act.
    const announced = announceLiveWork([recordedAt(RUNNING_SPAWN)], [HANDLE], BOOK);

    // Assert.
    expect(announced[0]?.kind?.kind.case).toBe("subagent");
  });

  it("dates the rebuilt start from the row's first place", () => {
    // Arrange, Act.
    const work = startOf(announceLiveWork([recordedAt(RUNNING_SPAWN, 1234n)], [HANDLE], BOOK));

    // Assert.
    const start = work?.case === "subagent" && work.value.result.case === "start" ? work.value.result.value : undefined;
    expect(start?.startedAt?.atMs).toBe(1234n);
  });

  it("states no start instant when the store stated no place", () => {
    // Arrange, Act.
    const work = startOf(announceLiveWork([recordedAt(RUNNING_SPAWN)], [HANDLE], BOOK));

    // Assert.
    const start = work?.case === "subagent" && work.value.result.case === "start" ? work.value.result.value : undefined;
    expect(start?.startedAt).toBeUndefined();
  });

  it("takes the created agent a settle arm states over the minting rule", () => {
    // Arrange.
    const settled: conversationv1.AgentActivity["item"] = {
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentSubagentSuccessSchema, { prompt: PROMPT, createdAgentId: agent("stated") }),
        },
      }),
    };

    // Act.
    const work = startOf(announceLiveWork([recordedAt(settled)], [HANDLE], BOOK));

    // Assert.
    const start = work?.case === "subagent" && work.value.result.case === "start" ? work.value.result.value : undefined;
    expect(start?.createdAgentId?.value).toBe("stated");
  });

  it("describes a shell from the command its settle arm restates", () => {
    // Arrange.
    const settled: conversationv1.AgentActivity["item"] = {
      case: "bash",
      value: create(conversationv1.AgentBashSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentBashSuccessSchema, {
            command: create(conversationv1.AgentBashCommandSchema, { line: "make test" }),
          }),
        },
      }),
    };

    // Act.
    const work = startOf(announceLiveWork([recordedAt(settled)], [HANDLE], BOOK));

    // Assert.
    expect(work?.case === "bash" && work.value.result.case === "start" ? work.value.result.value.command?.line : undefined).toBe(
      "make test",
    );
  });

  it("describes a progressing shell from the run's own start row", () => {
    // Arrange.
    const runStart = create(conversationv1.AgentBashStartSchema, {
      command: create(conversationv1.AgentBashCommandSchema, { line: "sleep 600" }),
      startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: 77n }),
    });

    // Act.
    const work = startOf(
      announceLiveWork([recordedAt(PROGRESSING_SHELL)], [HANDLE], BOOK, undefined, new Map([["run-1", runStart]])),
    );

    // Assert.
    const start = work?.case === "bash" && work.value.result.case === "start" ? work.value.result.value : undefined;
    expect([start?.command?.line, start?.startedAt?.atMs]).toEqual(["sleep 600", 77n]);
  });

  it("announces a progressing shell with no start row anywhere by handle and kind, with an empty command", () => {
    // Arrange, Act.
    const announced = announceLiveWork([recordedAt(PROGRESSING_SHELL)], [HANDLE], BOOK);

    // Assert.
    const work = startOf(announced);
    expect([
      announced[0]?.kind?.kind.case,
      work?.case === "bash" && work.value.result.case === "start" ? work.value.result.value.command?.line : undefined,
    ]).toEqual(["bash", ""]);
  });

  it("records the shell's missing command at ERROR", () => {
    // Arrange.
    const mark = logSinkMark();

    // Act.
    announceLiveWork([recordedAt(PROGRESSING_SHELL)], [HANDLE], BOOK);

    // Assert.
    const record = logRecordsSince(mark).find((r) => r.context.kind === "bash" && r.context.detail !== undefined);
    expect([record?.level, record?.context.work]).toEqual(["error", "run-1"]);
  });

  it("describes a monitor from the call its ended arm restates", () => {
    // Arrange.
    const ended: conversationv1.AgentActivity["item"] = {
      case: "monitor",
      value: create(conversationv1.AgentMonitorSchema, {
        result: {
          case: "ended",
          value: create(conversationv1.AgentMonitorEndedSchema, {
            call: create(conversationv1.AgentMonitorStartSchema, { description: "watch the build" }),
          }),
        },
      }),
    };

    // Act.
    const work = startOf(announceLiveWork([recordedAt(ended)], [HANDLE], BOOK));

    // Assert.
    expect(work?.case === "monitor" && work.value.result.case === "start" ? work.value.result.value.description : undefined).toBe(
      "watch the build",
    );
  });

  it("names a progressing shell as needing its run's start row", () => {
    // Arrange, Act, Assert.
    expect(bashUnitsWithoutCommand([recordedAt(PROGRESSING_SHELL)], [HANDLE]).map((h) => h.value)).toEqual(["run-1"]);
  });

  it("names a shell whose row holds no arm as needing its run's start row", () => {
    // Arrange.
    const bare = recordedAt({ case: "bash", value: create(conversationv1.AgentBashSchema, {}) });

    // Act, Assert.
    expect(bashUnitsWithoutCommand([bare], [HANDLE]).map((h) => h.value)).toEqual(["run-1"]);
  });

  it("does not name a running subagent as needing a shell start row", () => {
    // Arrange, Act, Assert.
    expect(bashUnitsWithoutCommand([recordedAt(RUNNING_SPAWN)], [HANDLE])).toEqual([]);
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
      getAgentByVendorTask: () => Promise.reject(new Error("unused")),
      writeBatch: refuse,
    };

    // Act, Assert.
    await expect(createReconciler({ client, sleep: instantly }).liveWork(MAIN)).rejects.toMatchObject({
      kind: "store_unavailable",
      message: "connect ECONNREFUSED",
    });
  });
});

describe("the restore of a subagent resumed by a send", () => {
  const HANDLE = create(conversationv1.DetachedWorkIdSchema, { value: "toolu_send" });
  const SPAWN = create(conversationv1.AgentIdSchema, { value: "toolu_spawn" });
  const OWNER = create(conversationv1.AgentIdSchema, { value: "main" });

  function unit(at: string, id: string, item: conversationv1.AgentActivity["item"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: at }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: {
                  case: "activity",
                  value: create(conversationv1.AgentActivitySchema, {
                    activityId: create(conversationv1.AgentActivityIdSchema, { value: id }),
                    item,
                  }),
                },
              }),
            },
          }),
        },
      }),
    });
  }

  function send(result: conversationv1.AgentSendMessage["result"]): conversationv1.HistoryEntryAt {
    return unit("2", "toolu_send", {
      case: "sendMessage",
      value: create(conversationv1.AgentSendMessageSchema, { result }),
    });
  }

  function spawn(created: string): conversationv1.HistoryEntryAt {
    return unit("1", "toolu_spawn", {
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentSubagentStartSchema, {
            createdAgentId: create(conversationv1.AgentIdSchema, { value: created }),
          }),
        },
      }),
    });
  }

  const reached = (locator: string): conversationv1.AgentSendMessage["result"] => ({
    case: "success",
    value: create(conversationv1.AgentSendMessageSuccessSchema, {
      recipientAgentId: create(conversationv1.AgentIdSchema, { value: locator }),
    }),
  });

  it("reads the locator a settled send reached", () => {
    expect(resumedRecipient([send(reached("a5583"))], HANDLE)).toEqual({ kind: "locator", vendorTaskId: "a5583" });
  });

  it("says a send that has not settled names no recipient", () => {
    expect(
      resumedRecipient([send({ case: "start", value: create(conversationv1.AgentSendMessageStartSchema, {}) })], HANDLE),
    ).toEqual({ kind: "no_recipient" });
  });

  it("says a settled send with an empty recipient names none", () => {
    expect(resumedRecipient([send(reached(""))], HANDLE)).toEqual({ kind: "no_recipient" });
  });

  it("hands back a handle whose unit is not a send", () => {
    expect(resumedRecipient([spawn("toolu_spawn")], create(conversationv1.DetachedWorkIdSchema, { value: "toolu_spawn" }))).toEqual({
      kind: "not_a_send",
    });
  });

  it("announces the resumed agent created, described by its spawn, under the send's handle", () => {
    // Act.
    const announcement = resumedAgentAnnouncement([send(reached("a5583")), spawn("toolu_spawn")], HANDLE, OWNER, SPAWN);

    // Assert.
    expect([
      announcement?.work?.value,
      announcement?.owner?.value,
      announcement?.kind?.kind.case === "subagent" ? announcement.kind.kind.value.agentId?.value : "",
      announcement?.origin.case,
    ]).toEqual(["toolu_send", "main", "toolu_spawn", "created"]);
  });

  it("announces a resumed agent whose spawn row already settled, described from its settle", () => {
    // Arrange: a resumed agent's spawn concluded before the send woke it.
    const settledSpawn = unit("1", "toolu_spawn", {
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentSubagentSuccessSchema, {
            prompt: create(conversationv1.AgentSubagentPromptSchema, { text: "count" }),
            createdAgentId: SPAWN,
          }),
        },
      }),
    });

    // Act.
    const announcement = resumedAgentAnnouncement([send(reached("a5583")), settledSpawn], HANDLE, OWNER, SPAWN);

    // Assert.
    expect(announcement?.kind?.kind.case === "subagent" ? announcement.kind.kind.value.agentId?.value : "").toBe("toolu_spawn");
  });

  it("states the resumed agent's commission as its spawn row describes it", () => {
    // Arrange.
    const settledSpawn = unit("1", "toolu_spawn", {
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentSubagentSuccessSchema, {
            prompt: create(conversationv1.AgentSubagentPromptSchema, { text: "count", description: "count the rows" }),
            createdAgentId: SPAWN,
          }),
        },
      }),
    });

    // Act.
    const announcement = resumedAgentAnnouncement([send(reached("a5583")), settledSpawn], HANDLE, OWNER, SPAWN);

    // Assert.
    const kind = announcement?.kind?.kind;
    expect(kind?.case === "subagent" ? kind.value.commission?.description : "").toBe("count the rows");
  });

  it("records at ERROR a resumed agent whose spawn row states no prompt, and still announces it", () => {
    // Arrange.
    const bareSpawn = unit("1", "toolu_spawn", {
      case: "subagent",
      value: create(conversationv1.AgentSubagentSchema, {
        result: { case: "update", value: create(conversationv1.AgentSubagentUpdateSchema, {}) },
      }),
    });
    const mark = logSinkMark();

    // Act.
    const announcement = resumedAgentAnnouncement([send(reached("a5583")), bareSpawn], HANDLE, OWNER, SPAWN);

    // Assert.
    const record = logRecordsSince(mark).find((r) => r.context.agent === "toolu_spawn" && r.context.detail !== undefined);
    expect([announcement?.origin.case, record?.level]).toEqual(["created", "error"]);
  });

  it("announces nothing when the book holds no spawn of the agent", () => {
    expect(resumedAgentAnnouncement([send(reached("a5583"))], HANDLE, OWNER, SPAWN)).toBeUndefined();
  });

  it("announces nothing when the spawn created a different agent", () => {
    expect(resumedAgentAnnouncement([spawn("toolu_other")], HANDLE, OWNER, SPAWN)).toBeUndefined();
  });
});

describe("revivalFate", () => {
  const bash: conversationv1.AgentActivity["item"] = { case: "bash", value: create(conversationv1.AgentBashSchema, {}) };
  const subagent: conversationv1.AgentActivity["item"] = {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {}),
  };
  const monitor: conversationv1.AgentActivity["item"] = {
    case: "monitor",
    value: create(conversationv1.AgentMonitorSchema, {}),
  };
  const send: conversationv1.AgentActivity["item"] = {
    case: "sendMessage",
    value: create(conversationv1.AgentSendMessageSchema, {}),
  };

  it.each([
    ["a subagent", subagent, "in_process"],
    ["a send-resumed subagent", send, "in_process"],
    ["a monitor", monitor, "in_process"],
    ["a shell", bash, "spool"],
  ] as const)("judges %s by its unit", (_name, item, fate) => {
    // Arrange, Act, Assert.
    expect(revivalFate(item, undefined).kind).toBe(fate);
  });

  it("takes the announced kind when the book holds no unit", () => {
    // Arrange, Act, Assert.
    expect(revivalFate(undefined, "bash")).toEqual({ kind: "spool", unit: "bash" });
  });

  it("prefers the unit the book holds over the announced kind", () => {
    // Arrange, Act, Assert.
    expect(revivalFate(subagent, "bash").kind).toBe("in_process");
  });

  it("cannot judge an item nothing states a kind for", () => {
    // Arrange, Act, Assert.
    expect(revivalFate(undefined, undefined)).toEqual({ kind: "unknown" });
  });

  it("cannot judge a unit of a kind that never detaches", () => {
    // Arrange.
    const read: conversationv1.AgentActivity["item"] = { case: "read", value: create(conversationv1.AgentReadSchema, {}) };

    // Act, Assert.
    expect(revivalFate(read, undefined)).toEqual({ kind: "unknown" });
  });
});

describe("findAnnouncedKind", () => {
  /** The main book's announcement of `work`, stating `kind` when given. */
  function announcement(work: string, kind?: conversationv1.DetachedWorkKind["kind"]): conversationv1.HistoryEntryAt {
    return create(conversationv1.HistoryEntryAtSchema, {
      at: create(conversationv1.HistoryPointerSchema, { value: work }),
      entry: create(conversationv1.HistoryEntrySchema, {
        entry: {
          case: "agentFrame",
          value: create(conversationv1.AgentFrameSchema, {
            result: {
              case: "detachedWork",
              value: create(conversationv1.AgentDetachedWorkSchema, {
                work: create(conversationv1.DetachedWorkIdSchema, { value: work }),
                ...(kind === undefined ? {} : { kind: create(conversationv1.DetachedWorkKindSchema, { kind }) }),
              }),
            },
          }),
        },
      }),
    });
  }

  const handle = (work: string): conversationv1.DetachedWorkId => create(conversationv1.DetachedWorkIdSchema, { value: work });

  it("answers the kind the handle's announcement states", () => {
    // Arrange.
    const book = [announcement("b1", { case: "bash", value: create(conversationv1.DetachedWorkKindBashSchema, {}) })];

    // Act, Assert.
    expect(findAnnouncedKind(book, handle("b1"))).toBe("bash");
  });

  it("answers nothing for an announcement that states no kind", () => {
    // Arrange, Act, Assert.
    expect(findAnnouncedKind([announcement("b1")], handle("b1"))).toBeUndefined();
  });

  it("answers nothing for another handle's announcement", () => {
    // Arrange.
    const book = [announcement("b2", { case: "bash", value: create(conversationv1.DetachedWorkKindBashSchema, {}) })];

    // Act, Assert.
    expect(findAnnouncedKind(book, handle("b1"))).toBeUndefined();
  });
});
