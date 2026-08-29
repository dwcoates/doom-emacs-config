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
 * # The honest closing arm
 *
 * A thing that did not survive a shim restart was not an execution error and
 * nobody stopped it: the process hosting it went down. So an agent closes as
 * `AgentSuccess.interrupted{host_shutdown}` — the arm the contract declares for
 * exactly this — and never as `AgentFailure.execution_error` with an invented
 * error string. Drawing a host shutdown as a user stop tells the user they
 * stopped something they did not; drawing it as a failure tells them something
 * broke that did not.
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
import { bashUpsertKey, terminalUpsertKey } from "./keys.js";
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
        "closing an agent that did not survive the shim's restart as interrupted by host shutdown",
      );
      const frame = create(conversationv1.AgentFrameSchema, {
        agentId: agent,
        result: {
          case: "success",
          value: create(conversationv1.AgentSuccessSchema, {
            outcome: {
              case: "interrupted",
              value: create(conversationv1.AgentInterruptedSchema, {
                cause: {
                  case: "hostShutdown",
                  value: create(conversationv1.AgentInterruptedByHostShutdownSchema, {}),
                },
              }),
            },
          }),
        },
      });
      const coordinate = reconciledCoordinate(agent.value);
      return {
        agentId: agent,
        upsertKey: terminalUpsertKey(agent, coordinate),
        source: {
          vendorUuid: coordinate,
          discriminator: "agent_frame.success.interrupted.host_shutdown",
        },
        keepalive: false,
        item: { kind: "frame", frame },
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
                // The cause arm stays UNSET: nobody stopped it and it did not
                // time out — the host went down, which the arms do not spell.
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
          discriminator: "agent_bash.success.interrupted",
        },
        keepalive: false,
        item: { kind: "bash_run", run, frame },
      };
    },
  };
}
