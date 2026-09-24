/**
 * test/store/persistence-fixtures.ts — the rows the record-plane suites write.
 *
 * Shared so the writer, reader and reconciler suites agree on what a row looks
 * like: a disagreement about a fixture would otherwise read as a disagreement
 * about the store.
 */
import { create } from "@bufbuild/protobuf";
import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { conversationv1 } from "../../src/proto.js";
import { activityUpsertKey, detachedWorkUpsertKey, promptUpsertKey } from "../../src/store/keys.js";
import type { PersistEntry } from "../../src/store/persistence.js";

/** A private unix socket path for one test's fake store. */
export function socketPathForTest(name: string): string {
  return join(mkdtempSync(join(tmpdir(), `shim-store-${name}-`)), "store.sock");
}

/** An agent identity. */
export function agent(value: string): conversationv1.AgentId {
  return create(conversationv1.AgentIdSchema, { value });
}

/** A unit identity. */
export function unit(value: string): conversationv1.AgentActivityId {
  return create(conversationv1.AgentActivityIdSchema, { value });
}

/** A turn's prompt row. */
export function promptEntry(
  book: conversationv1.AgentId,
  turnValue: string,
  text: string,
): PersistEntry {
  const turn = create(conversationv1.TurnIdSchema, { value: turnValue });
  return {
    agentId: book,
    upsertKey: promptUpsertKey(turn),
    source: { vendorUuid: `uuid-${turnValue}`, discriminator: "agent_prompt" },
    keepalive: false,
    turn,
    item: {
      kind: "prompt",
      prompt: create(conversationv1.AgentPromptSchema, {
        id: turn,
        agent: book,
        said: create(conversationv1.UserSaidSchema, {
          content: create(conversationv1.UserContentSchema, {
            blocks: [
              create(conversationv1.UserContentBlockSchema, {
                block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
              }),
            ],
          }),
        }),
        origin: conversationv1.PromptOrigin.USER_SENT,
      }),
    },
  };
}

/** One read unit's frame, at whatever state `discriminator` names. */
export function readEntry(
  book: conversationv1.AgentId,
  unitValue: string,
  path: string,
  options: { keepalive?: boolean; vendorUuid?: string } = {},
): PersistEntry {
  const activityId = unit(unitValue);
  return {
    agentId: book,
    upsertKey: activityUpsertKey(activityId),
    source: {
      vendorUuid: options.vendorUuid ?? `uuid-${unitValue}`,
      discriminator: "activity.read.start",
    },
    keepalive: options.keepalive ?? false,
    turn: undefined,
    item: {
      kind: "frame",
      frame: create(conversationv1.AgentFrameSchema, {
        agentId: book,
        result: {
          case: "update",
          value: create(conversationv1.AgentUpdateSchema, {
            update: {
              case: "activity",
              value: create(conversationv1.AgentActivitySchema, {
                activityId,
                item: {
                  case: "read",
                  value: create(conversationv1.AgentReadSchema, {
                    result: {
                      case: "start",
                      value: create(conversationv1.AgentReadStartSchema, {
                        path: create(conversationv1.ReadPathSchema, { path }),
                        startedAt: create(conversationv1.AgentActivityStartedAtSchema, {
                          atMs: 1_000n,
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
  };
}

/**
 * A subagent spawn in `spawner`'s book whose start CREATES `created` — the row
 * the store records `agent.spawned_by_agent` from, and so what puts `created`
 * in `spawner`'s session lineage.
 */
export function spawnEntry(spawner: conversationv1.AgentId, created: string): PersistEntry {
  const activityId = unit(`spawn-${created}`);
  return {
    agentId: spawner,
    upsertKey: activityUpsertKey(activityId),
    source: { vendorUuid: `uuid-spawn-${created}`, discriminator: "activity.subagent.start" },
    keepalive: false,
    turn: undefined,
    item: {
      kind: "frame",
      frame: create(conversationv1.AgentFrameSchema, {
        agentId: spawner,
        result: {
          case: "update",
          value: create(conversationv1.AgentUpdateSchema, {
            update: {
              case: "activity",
              value: create(conversationv1.AgentActivitySchema, {
                activityId,
                item: {
                  case: "subagent",
                  value: create(conversationv1.AgentSubagentSchema, {
                    result: {
                      case: "start",
                      value: create(conversationv1.AgentSubagentStartSchema, {
                        createdAgentId: agent(created),
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
  };
}

/** One detached shell run's lifecycle frame. */
export function bashRunEntry(
  book: conversationv1.AgentId,
  runValue: string,
  line: string,
): PersistEntry {
  const run = unit(runValue);
  return {
    agentId: book,
    upsertKey: `bash:${runValue}`,
    source: { vendorUuid: `uuid-${runValue}`, discriminator: "agent_bash.start" },
    keepalive: false,
    turn: undefined,
    item: {
      kind: "bash_run",
      run,
      frame: create(conversationv1.AgentBashSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentBashStartSchema, {
            command: create(conversationv1.AgentBashCommandSchema, { line }),
            startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: 5_000n }),
          }),
        },
      }),
    },
  };
}

// ---------------------------------------------------------------------------
// A detached shell run: its announcement, and its lifecycle rows
// ---------------------------------------------------------------------------

/** The run every bash fixture below is about. */
export const RUN_VALUE = "run-1";
/** The handle the run is addressed by. */
export const WORK_VALUE = "work-1";

/**
 * The announcement that carries the handle→run join.
 *
 * The reader learns which run a `DetachedWorkId` names from exactly this row, so
 * a bash test that skips it is testing a watch nobody could have opened.
 */
export function bashAnnouncementEntry(book = agent("book-1")): PersistEntry {
  const work = create(conversationv1.DetachedWorkIdSchema, { value: WORK_VALUE });
  return {
    agentId: book,
    upsertKey: detachedWorkUpsertKey(work),
    source: { vendorUuid: `uuid-${WORK_VALUE}`, discriminator: "agent_frame.detached_work" },
    keepalive: false,
    turn: undefined,
    item: {
      kind: "frame",
      frame: create(conversationv1.AgentFrameSchema, {
        agentId: book,
        result: {
          case: "detachedWork",
          value: create(conversationv1.AgentDetachedWorkSchema, {
            work,
            origin: {
              case: "detached",
              value: create(conversationv1.DetachedWorkDetachedSchema, {
                detachedFromId: unit(RUN_VALUE),
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
  };
}

/** One lifecycle row of the run, keyed as every row of it is. */
function bashRow(
  frame: conversationv1.AgentBash,
  discriminator: string,
  book = agent("book-1"),
): PersistEntry {
  return {
    agentId: book,
    upsertKey: `bash:${RUN_VALUE}:${discriminator}`,
    source: { vendorUuid: `uuid-${RUN_VALUE}-${discriminator}`, discriminator },
    keepalive: false,
    turn: undefined,
    item: { kind: "bash_run", run: unit(RUN_VALUE), frame },
  };
}

/** The run's announced start. */
export function bashStartEntry(book = agent("book-1")): PersistEntry {
  return bashRow(
    create(conversationv1.AgentBashSchema, {
      result: {
        case: "start",
        value: create(conversationv1.AgentBashStartSchema, {
          command: create(conversationv1.AgentBashCommandSchema, { line: "sleep 100" }),
          startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: 5_000n }),
        }),
      },
    }),
    "agent_bash.start",
    book,
  );
}

/** The run's rendered tail — the row only the sidecar can write. */
export function bashTailEntry(book = agent("book-1")): PersistEntry {
  return bashRow(
    create(conversationv1.AgentBashSchema, {
      result: {
        case: "tail",
        value: create(conversationv1.AgentBashTailSchema, { text: "working\n" }),
      },
    }),
    "agent_bash.tail",
    book,
  );
}

/** The run's terminal row. */
export function bashTerminalEntry(book = agent("book-1")): PersistEntry {
  return bashRow(
    create(conversationv1.AgentBashSchema, {
      result: {
        case: "success",
        value: create(conversationv1.AgentBashSuccessSchema, {
          command: create(conversationv1.AgentBashCommandSchema, { line: "sleep 100" }),
          outcome: {
            case: "completed",
            value: create(conversationv1.AgentBashCompletedSchema, {
              output: create(conversationv1.AgentBashOutputSchema, {
                form: {
                  case: "text",
                  value: create(conversationv1.AgentBashOutputTextSchema, {
                    stdout: "working\n",
                    stderr: "",
                    extent: {
                      case: "whole",
                      value: create(conversationv1.AgentBashOutputWholeSchema, {}),
                    },
                  }),
                },
              }),
            }),
          },
        }),
      },
    }),
    "agent_bash.success",
    book,
  );
}

/**
 * An agent terminal: the `success` frame a turn ends on in `book`. A TURN EDGE
 * for the writer's priority rule when `book` is the main agent's.
 */
export function terminalEntry(book: conversationv1.AgentId, turnValue: string): PersistEntry {
  return {
    agentId: book,
    upsertKey: `terminal:${turnValue}`,
    source: { vendorUuid: `uuid-result-${turnValue}`, discriminator: "agent_frame.success.completed" },
    keepalive: false,
    turn: undefined,
    item: {
      kind: "frame",
      frame: create(conversationv1.AgentFrameSchema, {
        agentId: book,
        result: {
          case: "success",
          value: create(conversationv1.AgentSuccessSchema, {
            outcome: { case: "completed", value: create(conversationv1.AgentCompletedSchema, {}) },
          }),
        },
      }),
    },
  };
}
