/**
 * The OPEN OBLIGATIONS, and the honest closing terminals.
 *
 * The invariant under test is "every started thing eventually gets a terminal
 * row, by observation or by reconciliation" — so the assertions are about which
 * ARM a reconciled ending takes, because the arm is what a reader is told
 * happened.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { producerId } from "../../src/store/keys.js";
import { PersistenceError } from "../../src/store/persistence.js";
import {
  announceLiveWork,
  createReconciler,
  findBashStart,
  reconciledCoordinate,
  stoppedBashTerminal,
} from "../../src/store/reconcile.js";
import { createPersistence } from "../../src/store/writer.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import { agent, readEntry, socketPathForTest, unit } from "./persistence-fixtures.js";

const PRODUCER = producerId("vendor-session-1");
const BOOK = agent("book-1");
const RUN = unit("run-1");

let store: FakeStore | undefined;

afterEach(async () => {
  await store?.close();
  store = undefined;
});

async function reconciler(name: string) {
  const started = await startFakeStore(socketPathForTest(name));
  store = started;
  const client = createStoreClient(started.socketPath);
  return { started, client, reconciler: createReconciler({ client }) };
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
  it("answers the record's open obligations", async () => {
    const { started, client, reconciler: plane } = await reconciler("live-empty");
    const writer = createPersistence({
      client,
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    writer.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await writer.flush();
    void started;

    const live = await plane.liveWork();

    expect(live.liveAgents.map((id) => id.value)).toContain("book-1");
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

    await expect(createReconciler({ client: refusing }).liveWork()).rejects.toBeInstanceOf(
      PersistenceError,
    );
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

    await expect(createReconciler({ client: empty }).liveWork()).rejects.toMatchObject({
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
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE]);

    // `detached` means "continue what you are drawing"; telling a fresh
    // consumer that leaves the work undrawn and unreachable.
    expect(announced[0]?.origin.case).toBe("created");
  });

  it("describes the work FROM THE STORE, by the unit the handle names", () => {
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE]);

    const created = announced[0]?.origin.value as conversationv1.DetachedWorkCreated;
    expect(created.workCreated?.work.case).toBe("bash");
  });

  it("carries the recorded start, so the description cannot disagree with the record", () => {
    const announced = announceLiveWork([recordedRun("run-1")], [HANDLE]);

    const created = announced[0]?.origin.value as conversationv1.DetachedWorkCreated;
    const bash = created.workCreated?.work.value as conversationv1.AgentBash;
    const start = bash.result.value as conversationv1.AgentBashStart;
    expect(start.command?.line).toBe("sleep 100");
  });

  it("omits work the record cannot describe rather than announcing a bare handle", () => {
    expect(announceLiveWork([], [HANDLE])).toEqual([]);
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
    expect(announceLiveWork([read], [HANDLE])).toEqual([]);
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
