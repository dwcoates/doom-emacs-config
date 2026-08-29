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
import { activityUpsertKey, promptUpsertKey } from "../../src/store/keys.js";
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
