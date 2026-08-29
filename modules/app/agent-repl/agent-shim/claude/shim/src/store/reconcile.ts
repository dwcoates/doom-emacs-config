/**
 * store/reconcile.ts — the OPEN OBLIGATIONS, and how each one is closed.
 *
 * # What "live" means here
 *
 * `GetLiveWork` is not a claim about the world. It answers "what did the record
 * see START and never see END", which is timeless and cannot go stale. The SHIM
 * calls it once at session start and resolves every item: re-adopt what the
 * revived vendor process actually has, and WRITE the closing terminal for what
 * did not survive. Either way the set shrinks to empty, which is what makes
 * "every started thing eventually gets a terminal row" hold across any gap in
 * observation.
 *
 * # The honest closing arm: `lost.swept_up`
 *
 * A thing the record holds no terminal for after a restart is NOT known to have
 * failed and NOT known to have finished — we simply stopped being able to see
 * it. `DetachedLost.swept_up` is that fact exactly: "a boot sweep found the run
 * open with no living producer" (landing 3). Every other arm would be a claim
 * nobody can support: `execution_error` says something broke, `interrupted.
 * by_user` accuses the user of a stop they did not command, and
 * `interrupted.host_shutdown` asserts a cause the record cannot distinguish
 * from a shim that was simply not watching.
 *
 * # Why the original start has to be read back
 *
 * A shell run's terminal restates the command that ran (`AgentBashSuccess.
 * command` is not optional, because a settled frame describes itself). After a
 * bounce the shim remembers nothing, so the start is recovered FROM THE STORE by
 * walking the agent's own book for the unit — the one place the fact still is.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import { activityUpsertKey, bashUpsertKey, terminalUpsertKey } from "./keys.js";
import { PersistenceError, type PersistEntry } from "./persistence.js";
import { readFailure, transportFailure } from "./reader.js";

const LOGGER = bindLog({ component: "shim-store-reconcile", operation: "shim.store.reconcile" });

/**
 * The vendor-record coordinate a RECONCILED terminal is keyed by.
 *
 * Deterministic and synthetic: no vendor record states this terminal, so the
 * coordinate names the reconciliation itself. Deterministic so a second
 * reconciliation of the same agent upserts the same row rather than adding a
 * second stop notice to the feed.
 */
export function reconciledCoordinate(subject: string): string {
  return `reconcile:${subject}`;
}

/** The reconciler, as the engine drives it. */
export interface Reconciler {
  /** Everything the record holds a start for and no terminal. */
  liveWork(): Promise<storev1.GetLiveWorkSuccess>;
  /** The row that closes an agent that did not survive the shim's restart. */
  closingAgentTerminal(agent: conversationv1.AgentId): PersistEntry;
  /**
   * The row that closes a shell run that did not survive, with its command and
   * its ORIGINAL start instant recovered from the record.
   *
   * `originalStart` is the start frame read back from the agent's book — see
   * {@link findBashStart}. Rejects with `unknown_work` when the record holds no
   * start for the run, because a terminal restating a command nobody observed
   * would be an invention.
   */
  closingBashTerminal(
    agent: conversationv1.AgentId,
    run: conversationv1.AgentActivityId,
    originalStart: conversationv1.AgentBashStart,
  ): PersistEntry;
  /**
   * The row that closes a SPAWN unit the record holds no terminal for.
   *
   * Distinct from {@link Reconciler.closingAgentTerminal}: that closes the
   * detached AGENT's own book, and this closes the calling agent's unit that
   * spawned it. Both are owed — a reader looking at the spawn bubble and a
   * reader looking at the subagent's container are looking at two rows.
   */
  closingSubagentTerminal(
    agent: conversationv1.AgentId,
    spawn: conversationv1.AgentActivityId,
  ): PersistEntry;
}

/**
 * The one arm a RECONCILIATION may ever state.
 *
 * `swept_up` is the reconciliation's own word for what it did: it found the run
 * open with no living producer. `file_vanished` belongs to the SIDECAR, which is
 * the only thing that reads files, and `went_silent` belongs to whoever holds a
 * silence ruling — neither is knowable from a store row.
 */
export function sweptUp(): conversationv1.DetachedLost {
  return create(conversationv1.DetachedLostSchema, {
    how: { case: "sweptUp", value: create(conversationv1.DetachedLostSweptUpSchema, {}) },
  });
}

/** What a reconciler needs to exist. */
export interface ReconcilerOptions {
  readonly client: StoreClient;
}

/**
 * The shell run's own `start` frame, found in one agent's book.
 *
 * A PLAIN SEARCH OVER PAGES the caller already has, deliberately: the reconciler
 * does not open a reading session of its own, because the engine is already
 * paging the book it is reconciling and a second walk would double the reads.
 */
export function findBashStart(
  entries: readonly conversationv1.HistoryEntryAt[],
  run: conversationv1.AgentActivityId,
): conversationv1.AgentBashStart | undefined {
  for (const at of entries) {
    const entry = at.entry?.entry;
    if (entry?.case !== "agentFrame") continue;
    const result = entry.value.result;
    if (result.case !== "update") continue;
    const update = result.value.update;
    if (update.case !== "activity") continue;
    if (update.value.activityId?.value !== run.value) continue;
    const item = update.value.item;
    if (item.case !== "bash") continue;
    if (item.value.result.case !== "start") continue;
    return item.value.result.value;
  }
  return undefined;
}

export function createReconciler(options: ReconcilerOptions): Reconciler {
  return {
    async liveWork(): Promise<storev1.GetLiveWorkSuccess> {
      let response: storev1.GetLiveWorkResponse;
      try {
        response = await options.client.getLiveWork(create(storev1.GetLiveWorkRequestSchema, {}));
      } catch (error) {
        LOGGER.log(
          { level: "error", detail: String(error) },
          "the store could not be reached for the open obligations",
        );
        throw transportFailure(error);
      }
      const result = response.result;
      if (result.case === "failure") {
        LOGGER.log(
          { level: "warn", detail: result.value.detail },
          "the store refused to state the open obligations",
        );
        throw readFailure(result.value.detail);
      }
      if (result.case !== "success") {
        throw new PersistenceError(
          "store_unavailable",
          "the store answered GetLiveWork with no result arm set",
        );
      }
      LOGGER.log(
        {
          live_agents: result.value.liveAgents.length,
          live_detached: result.value.liveDetached.length,
          live_workflows: result.value.liveWorkflows.length,
        },
        "read the record's open obligations",
      );
      return result.value;
    },

    closingAgentTerminal(agent: conversationv1.AgentId): PersistEntry {
      LOGGER.log(
        { agent: agent.value },
        "closing an agent the record holds no terminal for: the boot sweep found it open with no producer",
      );
      const frame = create(conversationv1.AgentFrameSchema, {
        agentId: agent,
        result: {
          case: "failure",
          value: create(conversationv1.AgentFailureSchema, {
            // NO ERROR STRINGS: the run accumulated none that anyone observed,
            // and inventing one ("did not survive the shim restart") would put
            // a sentence in the record that no producer ever said.
            failure: { case: "lost", value: sweptUp() },
          }),
        },
      });
      const coordinate = reconciledCoordinate(agent.value);
      return {
        agentId: agent,
        upsertKey: terminalUpsertKey(agent, coordinate),
        source: { vendorUuid: coordinate, discriminator: "agent_frame.failure.lost.swept_up" },
        keepalive: false,
        item: { kind: "frame", frame },
      };
    },

    closingSubagentTerminal(
      agent: conversationv1.AgentId,
      spawn: conversationv1.AgentActivityId,
    ): PersistEntry {
      LOGGER.log(
        { agent: agent.value, spawn: spawn.value },
        "closing a spawn unit the record holds no terminal for as lost",
      );
      const activity = create(conversationv1.AgentActivitySchema, {
        activityId: spawn,
        item: {
          case: "subagent",
          value: create(conversationv1.AgentSubagentSchema, {
            result: {
              case: "failure",
              value: create(conversationv1.AgentSubagentFailureSchema, {
                cause: { case: "lost", value: sweptUp() },
              }),
            },
          }),
        },
      });
      return {
        agentId: agent,
        upsertKey: activityUpsertKey(spawn),
        source: {
          vendorUuid: reconciledCoordinate(spawn.value),
          discriminator: "activity.subagent.failure.lost.swept_up",
        },
        keepalive: false,
        item: {
          kind: "frame",
          frame: create(conversationv1.AgentFrameSchema, {
            agentId: agent,
            result: {
              case: "update",
              value: create(conversationv1.AgentUpdateSchema, {
                update: { case: "activity", value: activity },
              }),
            },
          }),
        },
      };
    },

    closingBashTerminal(
      agent: conversationv1.AgentId,
      run: conversationv1.AgentActivityId,
      originalStart: conversationv1.AgentBashStart,
    ): PersistEntry {
      if (originalStart.command === undefined) {
        throw new PersistenceError(
          "unknown_work",
          `the recorded start for shell run ${JSON.stringify(run.value)} states no command`,
        );
      }
      LOGGER.log(
        { agent: agent.value, run: run.value },
        "closing a shell run that did not survive the shim's restart as interrupted",
      );
      const frame = create(conversationv1.AgentBashSchema, {
        result: {
          case: "success",
          value: create(conversationv1.AgentBashSuccessSchema, {
            command: originalStart.command,
            outcome: {
              case: "interrupted",
              value: create(conversationv1.AgentBashInterruptedSchema, {
                // NOT OURS TO STATE: the reconciliation observed no output at
                // all, so the extent is `partial` with nothing omitted that we
                // can count and no spill we can point at. The contract has no
                // "not observed" arm; recorded as a gap in the record-plane
                // report rather than answered with a `whole` that would claim
                // the command said nothing.
                output: create(conversationv1.AgentBashOutputSchema, {
                  form: {
                    case: "text",
                    value: create(conversationv1.AgentBashOutputTextSchema, {
                      stdout: "",
                      stderr: "",
                      extent: {
                        case: "partial",
                        value: create(conversationv1.AgentBashOutputPartialSchema, {
                          bytesOmitted: 0n,
                        }),
                      },
                    }),
                  },
                }),
                // THE CAUSE IS NOW STATEABLE (landing 3): we stopped being
                // able to see the run, which is what `lost.swept_up` says.
                cause: { case: "lost", value: sweptUp() },
              }),
            },
          }),
        },
      });
      return {
        agentId: agent,
        upsertKey: bashUpsertKey(run),
        source: {
          vendorUuid: reconciledCoordinate(run.value),
          discriminator: "agent_bash.success.interrupted.lost.swept_up",
        },
        keepalive: false,
        item: { kind: "bash_run", run, frame },
      };
    },
  };
}
