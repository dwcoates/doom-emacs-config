/**
 * test/integration-support/store.ts — write into the fake store as the SIDECAR
 * would, and read what the shim wrote.
 *
 * # Why the suite writes store rows at all
 *
 * Two obligations of the shim are only reachable through rows it did not write
 * itself:
 *
 *   - DETACHED SHELL OUTPUT. Every byte of it comes from the sidecar tailing
 *     the vendor's spool (verified: foreground shell output is observable
 *     nowhere while running). The sidecar is another real system, so it is not
 *     in this suite — its ROWS are, seeded here through the store's own
 *     `WriteBatch` and served back to the shim by `WatchBashRun`.
 *   - RECONCILIATION. `GetLiveWork` answers from THE RECORD, so a started item
 *     with no terminal has to exist in the record before the shim starts, which
 *     means seeding it before the spawn.
 *
 * Writing through the real `store.v1` client rather than reaching into the fake
 * store's internals keeps the seeding honest: a row this helper can write is a
 * row the sidecar could write.
 */
import { create } from "@bufbuild/protobuf";
import { createClient, type Client } from "@connectrpc/connect";
import { createConnectTransport } from "@connectrpc/connect-node";
import { conversationv1, storev1 } from "../../src/proto.js";

/** A store.v1 client, dialed the way the shim dials it. */
export type StoreClient = Client<typeof storev1.ShimStore>;

/** Dial the fake store over its unix socket. */
export function createStoreClient(socketPath: string): StoreClient {
  return createClient(
    storev1.ShimStore,
    createConnectTransport({
      httpVersion: "1.1",
      baseUrl: "http://store",
      nodeOptions: { socketPath },
    }),
  );
}

/** `store.v1.Plane{file}` — the sidecar's plane, since it reads files. */
export function filePlane(): storev1.Plane {
  return create(storev1.PlaneSchema, {
    plane: { case: "file", value: create(storev1.PlaneFileSchema, {}) },
  });
}

/** The producer name a seeded row carries. */
export function sidecarProducer(originalVendorSessionId: string): string {
  return `claude-sidecar:${originalVendorSessionId}`;
}

/** Send one batch of already-built entries, asserting the store took them. */
export async function writeEntries(
  client: StoreClient,
  producer: string,
  entries: readonly storev1.StoreEntry[],
): Promise<void> {
  const response = await client.writeBatch(
    create(storev1.WriteBatchRequestSchema, {
      writeClass: create(storev1.WriteClassSchema, {
        writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
      }),
      producer,
      batch: create(storev1.EntryBatchSchema, { entries: [...entries] }),
    }),
  );
  if (response.result.case !== "success") {
    throw new Error(
      `seeding the store failed: ${
        response.result.case === "failure" ? response.result.value.detail : "unset result oneof"
      }`,
    );
  }
}

/** One `StoreAgentUpdate.bash` row, keyed as the run's lifecycle key. */
export function bashRowEntry(init: {
  readonly run: string;
  readonly frame: conversationv1.AgentBash;
  readonly writeId: string;
  /** Defaults to the contract's `bash:<run activity id>`. */
  readonly upsertKey?: string;
  readonly topLevel?: string;
}): storev1.StoreEntry {
  return create(storev1.StoreEntrySchema, {
    plane: filePlane(),
    writeId: init.writeId,
    upsertKey: init.upsertKey ?? `bash:${init.run}`,
    entry: {
      case: "agentUpdate",
      value: create(storev1.StoreAgentUpdateSchema, {
        ...(init.topLevel === undefined
          ? {}
          : { topLevel: create(conversationv1.AgentIdSchema, { value: init.topLevel }) }),
        agentInfo: {
          case: "bash",
          value: create(storev1.StoreAgentBashSchema, {
            run: create(conversationv1.AgentActivityIdSchema, { value: init.run }),
            frame: init.frame,
          }),
        },
      }),
    },
  });
}

/** `AgentBash{start}` carrying the run's ORIGINAL instant. */
export function bashStart(line: string, startedAtMs: number): conversationv1.AgentBash {
  return create(conversationv1.AgentBashSchema, {
    result: {
      case: "start",
      value: create(conversationv1.AgentBashStartSchema, {
        command: bashCommand(line),
        startedAt: create(conversationv1.AgentActivityStartedAtSchema, {
          atMs: BigInt(startedAtMs),
        }),
      }),
    },
  });
}

/** `AgentBashCommand` with the sandbox arm the vendor reported unset. */
export function bashCommand(line: string): conversationv1.AgentBashCommand {
  return create(conversationv1.AgentBashCommandSchema, { line });
}

/** `AgentBash{tail}` — the run's whole output so far, inside the cap. */
export function bashTail(text: string): conversationv1.AgentBash {
  return create(conversationv1.AgentBashSchema, {
    result: {
      case: "tail",
      value: create(conversationv1.AgentBashTailSchema, { text }),
    },
  });
}

/** `AgentBash{success{completed}}` — the spool's `EXIT=<code>` line, modeled. */
export function bashCompleted(
  line: string,
  exitCode: number,
  stdout: string,
): conversationv1.AgentBash {
  return create(conversationv1.AgentBashSchema, {
    result: {
      case: "success",
      value: create(conversationv1.AgentBashSuccessSchema, {
        command: bashCommand(line),
        outcome: {
          case: "completed",
          value: create(conversationv1.AgentBashCompletedSchema, {
            output: bashOutput(stdout),
            termination: create(conversationv1.AgentBashTerminationSchema, {
              how: {
                case: "exited",
                value: create(conversationv1.AgentBashExitedSchema, { code: exitCode }),
              },
            }),
          }),
        },
      }),
    },
  });
}

/** `AgentBashOutput` carrying whole text on stdout. */
export function bashOutput(stdout: string): conversationv1.AgentBashOutput {
  return create(conversationv1.AgentBashOutputSchema, {
    form: {
      case: "text",
      value: create(conversationv1.AgentBashOutputTextSchema, {
        stdout,
        stderr: "",
        extent: {
          case: "whole",
          value: create(conversationv1.AgentBashOutputWholeSchema, {}),
        },
      }),
    },
  });
}

/** One detached shell run's whole lifecycle, as the sidecar would write it. */
export interface BashLifecycleSeed {
  /** The run's `AgentActivityId` — the bash tool call's own id. */
  readonly run: string;
  /** The `DetachedWorkId` the daemon watches by. */
  readonly work: string;
  /** The command, echoed on start and on the terminal. */
  readonly command: string;
  /** The ORIGINAL instant — the one a re-announcement must repeat. */
  readonly startedAtMs: number;
  /** Output chunks, appended in order; offsets are derived from the lengths. */
  readonly chunks: readonly string[];
  /** The exit code, or `null` for a run that never ended (no terminal row). */
  readonly exitCode: number | null;
  /** The main agent, stamped as the row's top level. */
  readonly topLevel?: string;
}

/**
 * Seed a run's `start`, one `tail` write per chunk (each superseding the
 * run's one tail row with the whole output so far, as the sidecar writes it)
 * and (unless it never ends) its terminal, each under the producers' own key.
 */
export async function seedBashLifecycle(
  client: StoreClient,
  producer: string,
  seed: BashLifecycleSeed,
): Promise<void> {
  const entries: storev1.StoreEntry[] = [
    bashRowEntry({
      run: seed.run,
      frame: bashStart(seed.command, seed.startedAtMs),
      writeId: `seed-${seed.run}-start`,
      ...(seed.topLevel === undefined ? {} : { topLevel: seed.topLevel }),
    }),
  ];
  let whole = "";
  for (const [index, chunk] of seed.chunks.entries()) {
    whole += chunk;
    entries.push(
      bashRowEntry({
        run: seed.run,
        frame: bashTail(whole),
        writeId: `seed-${seed.run}-tail-${String(index)}`,
        upsertKey: `bash:${seed.run}:tail`,
        ...(seed.topLevel === undefined ? {} : { topLevel: seed.topLevel }),
      }),
    );
  }
  if (seed.exitCode !== null) {
    entries.push(
      bashRowEntry({
        run: seed.run,
        frame: bashCompleted(seed.command, seed.exitCode, whole),
        writeId: `seed-${seed.run}-terminal`,
        upsertKey: `bash:${seed.run}:terminal`,
        ...(seed.topLevel === undefined ? {} : { topLevel: seed.topLevel }),
      }),
    );
  }
  await writeEntries(client, producer, entries);
}

/**
 * Seed the ANNOUNCEMENT of a detached item as a page line.
 *
 * `GetLiveWork` answers from the record, and this row is what makes an item
 * "started with no terminal" — the shape a revived shim must reconcile, either
 * by re-adopting the item or by writing its closing terminal.
 */
export async function seedDetachedAnnouncement(
  client: StoreClient,
  producer: string,
  init: {
    readonly work: string;
    readonly agent: string;
    readonly detachedFromId: string;
    readonly outputPath?: string;
  },
): Promise<void> {
  const detached = create(conversationv1.AgentDetachedWorkSchema, {
    work: create(conversationv1.DetachedWorkIdSchema, { value: init.work }),
    ...(init.outputPath === undefined
      ? {}
      : {
          output: create(conversationv1.DetachedWorkOutputSchema, {
            path: init.outputPath,
            readability: {
              case: "readable",
              value: create(conversationv1.DetachedWorkOutputReadableSchema, {}),
            },
          }),
        }),
    origin: {
      case: "detached",
      value: create(conversationv1.DetachedWorkDetachedSchema, {
        detachedFromId: create(conversationv1.AgentActivityIdSchema, {
          value: init.detachedFromId,
        }),
        cause: {
          case: "requested",
          value: create(conversationv1.DetachedCauseRequestedSchema, {}),
        },
      }),
    },
  });
  await writeEntries(client, producer, [
    create(storev1.StoreEntrySchema, {
      plane: filePlane(),
      writeId: `seed-${init.work}-announcement`,
      // The cross-plane key for an announcement (landing 3): its own row, not
      // the spawning call's, so it cannot overwrite the call.
      upsertKey: `detached:${init.work}`,
      entry: {
        case: "agentUpdate",
        value: create(storev1.StoreAgentUpdateSchema, {
          topLevel: create(conversationv1.AgentIdSchema, { value: init.agent }),
          agentInfo: {
            case: "serveableFrame",
            value: create(storev1.StorePageLineSchema, {
              pageAgentId: create(conversationv1.AgentIdSchema, { value: init.agent }),
              agentItem: create(storev1.StoreAgentItemSchema, {
                item: {
                  case: "agentFrame",
                  value: create(conversationv1.AgentFrameSchema, {
                    agentId: create(conversationv1.AgentIdSchema, { value: init.agent }),
                    result: { case: "detachedWork", value: detached },
                  }),
                },
              }),
            }),
          },
        }),
      },
    }),
  ]);
}

/**
 * Seed a SUBAGENT SPAWN as a page line of the spawner's book.
 *
 * The spawn frame's start names the created agent, which is what the store
 * records `agent.spawned_by_agent` from — so the created agent is an open
 * obligation IN THE SPAWNER'S SESSION LINEAGE, and in no other session's.
 */
export async function seedSubagentSpawn(
  client: StoreClient,
  producer: string,
  init: { readonly spawner: string; readonly created: string },
): Promise<void> {
  const spawner = create(conversationv1.AgentIdSchema, { value: init.spawner });
  await writeEntries(client, producer, [
    create(storev1.StoreEntrySchema, {
      plane: filePlane(),
      writeId: `seed-${init.created}-spawn`,
      upsertKey: `activity:spawn-${init.created}`,
      entry: {
        case: "agentUpdate",
        value: create(storev1.StoreAgentUpdateSchema, {
          topLevel: spawner,
          agentInfo: {
            case: "serveableFrame",
            value: create(storev1.StorePageLineSchema, {
              pageAgentId: spawner,
              agentItem: create(storev1.StoreAgentItemSchema, {
                item: {
                  case: "agentFrame",
                  value: create(conversationv1.AgentFrameSchema, {
                    agentId: spawner,
                    result: {
                      case: "update",
                      value: create(conversationv1.AgentUpdateSchema, {
                        update: {
                          case: "activity",
                          value: create(conversationv1.AgentActivitySchema, {
                            activityId: create(conversationv1.AgentActivityIdSchema, {
                              value: `spawn-${init.created}`,
                            }),
                            item: {
                              case: "subagent",
                              value: create(conversationv1.AgentSubagentSchema, {
                                result: {
                                  case: "start",
                                  value: create(conversationv1.AgentSubagentStartSchema, {
                                    createdAgentId: create(conversationv1.AgentIdSchema, {
                                      value: init.created,
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
            }),
          },
        }),
      },
    }),
  ]);
}

// ---------------------------------------------------------------------------
// reading what the SHIM wrote
// ---------------------------------------------------------------------------

/** Every entry the shim sent, flattened out of its batches, in order. */
export function writtenEntries(
  writes: readonly storev1.WriteBatchRequest[],
): storev1.StoreEntry[] {
  return writes.flatMap((request) => request.batch?.entries ?? []);
}

/** Every entry whose `upsert_key` starts with `prefix`, in write order. */
export function entriesKeyed(
  writes: readonly storev1.WriteBatchRequest[],
  prefix: string,
): storev1.StoreEntry[] {
  return writtenEntries(writes).filter((entry) => entry.upsertKey.startsWith(prefix));
}

/** The `upsert_key` of every entry the shim wrote, in order. */
export function writtenKeys(writes: readonly storev1.WriteBatchRequest[]): string[] {
  return writtenEntries(writes).map((entry) => entry.upsertKey);
}

/** The page line an entry carries, when it carries one. */
export function pageLineOf(entry: storev1.StoreEntry): storev1.StorePageLine | null {
  if (entry.entry.case !== "agentUpdate") return null;
  const info = entry.entry.value.agentInfo;
  return info.case === "serveableFrame" ? info.value : null;
}

/** The producer every batch named; a suite asserts there is exactly one. */
export function producers(writes: readonly storev1.WriteBatchRequest[]): string[] {
  return [...new Set(writes.map((request) => request.producer))];
}
