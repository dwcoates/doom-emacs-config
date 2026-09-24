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
 * # Always scoped to this session
 *
 * ONE STORE SERVES EVERY SESSION ON THE HOST. The read names this
 * conversation's main agent and the store answers only that agent's lineage,
 * because everything answered here that this vendor does not hold gets a
 * closing terminal. Unscoped, a session start closed OTHER sessions' running
 * work (2026-09-23: opening one workspace reaped five running subagents of
 * another). The reconciler has no unscoped form, and an empty session is
 * refused here before the store is asked.
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
import { agentActivity } from "../convert/entries.js";
import { subagentId } from "../convert/ids.js";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import { activityUpsertKey, bashTerminalUpsertKey, terminalUpsertKey } from "./keys.js";
import { PersistenceError, type PersistEntry } from "./persistence.js";
import { readFailure, transportFailure } from "./reader.js";
import { readWithRetry, type ReadRetryOptions } from "./retry.js";

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
interface Reconciler {
  /** Everything the record holds a start for and no terminal, in `session`'s lineage. */
  liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess>;
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
   *
   * `recorded` is the spawn unit as the agent's book holds it (see
   * {@link findUnit}): the closing is a settled frame, so it restates what the
   * spawn was asked and which agent it created, and the record is the one
   * place those facts still are after a bounce.
   */
  closingSubagentTerminal(
    agent: conversationv1.AgentId,
    spawn: conversationv1.AgentActivityId,
    recorded: conversationv1.AgentSubagent,
  ): PersistEntry;
}

// ---------------------------------------------------------------------------
// The closing terminals — pure, because they need no store at all
// ---------------------------------------------------------------------------

export function closingAgentTerminal(agent: conversationv1.AgentId): PersistEntry {
  LOGGER.debug(
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
}
  });
  const coordinate = reconciledCoordinate(agent.value);
  return {
    agentId: agent,
    upsertKey: terminalUpsertKey(agent, coordinate),
    source: { vendorUuid: coordinate, discriminator: "agent_frame.failure.lost.swept_up" },
    keepalive: false,
    // A BOOT SWEEP RUNS OUTSIDE ANY TURN; the store keeps a swept row's stamp.
    turn: undefined,
    item: { kind: "frame", frame },
  };
}

/**
 * What a recorded spawn unit says it was asked, off whichever arm the record
 * holds: the start, a running beat, or a settle. UNDEFINED when the arm states
 * none.
 */
function recordedPrompt(
  recorded: conversationv1.AgentSubagent,
): conversationv1.AgentSubagentPrompt | undefined {
  switch (recorded.result.case) {
    case "start":
    case "update":
    case "success":
    case "failure":
      return recorded.result.value.prompt;
    default:
      return undefined;
  }
}

/**
 * Which agent a recorded spawn unit says it created. A running beat names
 * none, and then the minting rule is the answer: a subagent's identity IS its
 * spawning call's id, which is this unit's own.
 */
function recordedCreatedAgent(
  recorded: conversationv1.AgentSubagent,
  spawn: conversationv1.AgentActivityId,
): conversationv1.AgentId {
  switch (recorded.result.case) {
    case "start":
    case "success":
    case "failure": {
      const created = recorded.result.value.createdAgentId;
      if (created !== undefined && created.value !== "") return created;
      break;
    }
    default:
      break;
  }
  return subagentId(spawn.value);
}

export function closingSubagentTerminal(
  agent: conversationv1.AgentId,
  spawn: conversationv1.AgentActivityId,
  recorded: conversationv1.AgentSubagent,
): PersistEntry {
  LOGGER.debug(
    { agent: agent.value, spawn: spawn.value, recorded: recorded.result.case ?? "" },
    "closing a spawn unit the record holds no terminal for as lost",
  );
  const prompt = recordedPrompt(recorded);
  if (prompt === undefined) {
    LOGGER.debug(
      { agent: agent.value, spawn: spawn.value },
      "the recorded spawn unit states no prompt; the closing restates an empty one",
    );
  }
  const activity = agentActivity(spawn, {
    case: "subagent",
    value: create(conversationv1.AgentSubagentSchema, {
      result: {
        case: "failure",
        value: create(conversationv1.AgentSubagentFailureSchema, {
          cause: { case: "lost", value: sweptUp() },
          // RESTATED FROM THE RECORD: a replay serves this closing with no
          // start beside it, and it must still draw the spawn's label and
          // address its sub-feed.
          prompt: prompt ?? create(conversationv1.AgentSubagentPromptSchema, { text: "" }),
          createdAgentId: recordedCreatedAgent(recorded, spawn),
        }),
      },
    }),
  });
  return {
    agentId: agent,
    upsertKey: activityUpsertKey(spawn),
    source: {
      vendorUuid: reconciledCoordinate(spawn.value),
      discriminator: "activity.subagent.failure.lost.swept_up",
    },
    keepalive: false,
    // A BOOT SWEEP RUNS OUTSIDE ANY TURN; the store keeps a swept row's stamp.
    turn: undefined,
    item: {
      kind: "frame",
      frame: create(conversationv1.AgentFrameSchema, {
        agentId: agent,
        result: {
          case: "update",
          value: create(conversationv1.AgentUpdateSchema, {
            update: { case: "activity", value: activity },
          }),
}
      }),
}
  };
}

/**
 * The row that closes a MONITOR unit the record holds no terminal for.
 *
 * `AgentMonitor.ended` is the monitor's own word for "the watch left the live
 * set", and it claims no cause, which is exactly what a sweep knows. Closing a
 * monitor with a SHELL terminal instead relabelled the watch as a shell run in
 * the store (three monitors on 2026-09-23).
 */
export function closingMonitorTerminal(
  agent: conversationv1.AgentId,
  monitor: conversationv1.AgentActivityId,
  call: conversationv1.AgentMonitorStart | undefined,
): PersistEntry {
  LOGGER.debug(
    { agent: agent.value, monitor: monitor.value, restated: call !== undefined },
    "closing a monitor the record holds no terminal for as ended",
  );
  const activity = agentActivity(monitor, {
    case: "monitor",
    value: create(conversationv1.AgentMonitorSchema, {
      // THE CALL IS RESTATED from the record's own start: this row replaces
      // it, and the daemon draws the monitor's card from it on a replay.
      result: { case: "ended", value: create(conversationv1.AgentMonitorEndedSchema, { call }) },
    }),
  });
  return {
    agentId: agent,
    upsertKey: activityUpsertKey(monitor),
    source: {
      vendorUuid: reconciledCoordinate(monitor.value),
      discriminator: "activity.monitor.ended.swept_up",
    },
    keepalive: false,
    // A BOOT SWEEP RUNS OUTSIDE ANY TURN; the store keeps a swept row's stamp.
    turn: undefined,
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
}

export function closingBashTerminal(
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
  LOGGER.debug(
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
            // NOT OURS TO STATE, AND NOW SAYABLE (landing 5): the
            // reconciliation observed no output at all, so the form is
            // `not_observed` — the producer stating that it does not know,
            // rather than a `partial` omission of zero bytes claiming we saw
            // all none of what it printed.
            output: create(conversationv1.AgentBashOutputSchema, {
              form: {
                case: "notObserved",
                value: create(conversationv1.AgentBashOutputNotObservedSchema, {}),
              },
            }),
            // THE CAUSE IS NOW STATEABLE (landing 3): we stopped being
            // able to see the run, which is what `lost.swept_up` says.
            cause: { case: "lost", value: sweptUp() },
          }),
}
      }),
}
  });
  return {
    agentId: agent,
    upsertKey: bashTerminalUpsertKey(run),
    source: {
      vendorUuid: reconciledCoordinate(run.value),
      discriminator: "agent_bash.success.interrupted.lost.swept_up",
    },
    keepalive: false,
    // A BOOT SWEEP RUNS OUTSIDE ANY TURN; the store keeps a swept row's stamp.
    turn: undefined,
    item: { kind: "bash_run", run, frame },
  };
}


/**
 * The terminal for a run THIS SHIM ENDED, by a stop it issued itself.
 *
 * # Why the shim writes it, when the sidecar writes every other bash row
 *
 * A detached shell's output rows are the sidecar's, read off the spool. Its
 * TERMINAL normally is too — the spool's `EXIT=` line. But a run the shim
 * stops during a teardown may have no sidecar left to read that line, and the
 * contract is unconditional: every stream concludes with a terminal frame, and
 * every started thing eventually gets a terminal row. The act was ours, so the
 * record of the act is ours.
 *
 * `by_user` and not `lost`: a stop the shim issued on a caller's behalf is a
 * cause we OBSERVED, not a run we merely lost sight of. Re-writing the same key
 * is absorbed by the store, so a sidecar that does later see the `EXIT=` line
 * cannot produce a second, conflicting terminal.
 */
export function stoppedBashTerminal(
  agent: conversationv1.AgentId,
  run: conversationv1.AgentActivityId,
  command: conversationv1.AgentBashCommand,
): PersistEntry {
  LOGGER.debug(
    { agent: agent.value, run: run.value },
    "closing a shell run the shim stopped, as interrupted by the user",
  );
  const frame = create(conversationv1.AgentBashSchema, {
    result: {
      case: "success",
      value: create(conversationv1.AgentBashSuccessSchema, {
        command,
        outcome: {
          case: "interrupted",
          value: create(conversationv1.AgentBashInterruptedSchema, {
            // WHAT WE SAW OF THE OUTPUT IS NOTHING: every byte of a detached
            // run is read from the spool by the sidecar, and this shim read
            // none of it. `not_observed` is the producer saying so, rather
            // than an empty `partial` claiming the run printed nothing.
            output: create(conversationv1.AgentBashOutputSchema, {
              form: {
                case: "notObserved",
                value: create(conversationv1.AgentBashOutputNotObservedSchema, {}),
              },
            }),
            cause: {
              case: "byUser",
              value: create(conversationv1.AgentBashInterruptedByUserSchema, {}),
            },
          }),
        },
      }),
    },
  });
  return {
    agentId: agent,
    upsertKey: bashTerminalUpsertKey(run),
    source: {
      vendorUuid: reconciledCoordinate(run.value),
      discriminator: "agent_bash.success.interrupted.by_user",
    },
    keepalive: false,
    // A BOOT SWEEP RUNS OUTSIDE ANY TURN; the store keeps a swept row's stamp.
    turn: undefined,
    item: { kind: "bash_run", run, frame },
  };
}

/**
 * The one arm a RECONCILIATION may ever state.
 *
 * `swept_up` is the reconciliation's own word for what it did: it found the run
 * open with no living producer. `file_vanished` belongs to the SIDECAR, which is
 * the only thing that reads files, and `went_silent` belongs to whoever holds a
 * silence ruling — neither is knowable from a store row.
 */
function sweptUp(): conversationv1.DetachedLost {
  return create(conversationv1.DetachedLostSchema, {
    how: { case: "sweptUp", value: create(conversationv1.DetachedLostSweptUpSchema, {}) },
  });
}

/** What a reconciler needs to exist. */
interface ReconcilerOptions extends ReadRetryOptions {
  readonly client: StoreClient;
}

/**
 * One unit's own frames, found in an agent's book.
 *
 * A PLAIN SEARCH OVER PAGES the caller already has, deliberately: the reconciler
 * does not open a reading session of its own, because the engine is already
 * paging the book it is reconciling and a second walk would double the reads.
 */
export function findUnit(
  entries: readonly conversationv1.HistoryEntryAt[],
  unit: conversationv1.AgentActivityId,
): conversationv1.AgentActivity["item"] | undefined {
  for (const at of entries) {
    const entry = at.entry?.entry;
    if (entry?.case !== "agentFrame") continue;
    const result = entry.value.result;
    if (result.case !== "update") continue;
    const update = result.value.update;
    if (update.case !== "activity") continue;
    if (update.value.activityId?.value !== unit.value) continue;
    return update.value.item;
  }
  return undefined;
}

/**
 * The start a monitor was armed with, found in one agent's book: its `start`
 * frame, or the call a settled arm restated. Undefined when the book holds no
 * monitor unit for it, or one that restated nothing.
 */
export function findMonitorCall(
  entries: readonly conversationv1.HistoryEntryAt[],
  monitor: conversationv1.AgentActivityId,
): conversationv1.AgentMonitorStart | undefined {
  const item = findUnit(entries, monitor);
  if (item?.case !== "monitor") return undefined;
  const result = item.value.result;
  switch (result.case) {
    case "start":
      return result.value;
    case "ended":
    case "failure":
      return result.value.call;
    case undefined:
      return undefined;
  }
}

/** The shell run's own `start` frame, found in one agent's book. */
export function findBashStart(
  entries: readonly conversationv1.HistoryEntryAt[],
  run: conversationv1.AgentActivityId,
): conversationv1.AgentBashStart | undefined {
  const item = findUnit(entries, run);
  if (item?.case !== "bash") return undefined;
  return item.value.result.case === "start" ? item.value.result.value : undefined;
}

/**
 * WHAT THE WORK IS, for a consumer that has never seen it.
 *
 * `DetachableWork` names the UNIT TYPES themselves, each carrying the `start`
 * arm that describes the work — so a stored unit is exactly what an
 * announcement needs, with no separate description type and no second copy able
 * to disagree with the first.
 *
 * Answers nothing for a unit whose kind cannot detach, or for one the record
 * holds no `start` for: a kind absent from `DetachableWork` cannot claim to be
 * detached, and an announcement with an invented description is worse than one
 * that is missing.
 */
function describeDetachable(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.DetachableWork | undefined {
  if (item === undefined) return undefined;
  if (item.case === "bash" && item.value.result.case === "start") {
    return create(conversationv1.DetachableWorkSchema, { work: { case: "bash", value: item.value } });
  }
  if (item.case === "subagent" && item.value.result.case === "start") {
    return create(conversationv1.DetachableWorkSchema, {
      work: { case: "subagent", value: item.value },
    });
  }
  if (item.case === "monitor" && item.value.result.case === "start") {
    return create(conversationv1.DetachableWorkSchema, {
      work: { case: "monitor", value: item.value },
    });
  }
  return undefined;
}

/**
 * The announcements `SessionStarted.live_work` carries.
 *
 * THE `created` ARM, ALWAYS. A daemon that restarted was NOT THERE for the
 * original announcement, so it has no element to continue — and `detached`
 * means exactly "continue the element you are already drawing". Telling a fresh
 * consumer to continue something it never saw leaves the work undrawn and
 * unreachable, which is the whole reason the two arms are different situations.
 *
 * `owner` is the agent whose book `entries` is. A unit found there is that
 * agent's own call, so the announcement names it as the owner.
 *
 * The description comes FROM THE STORE, by unit id: the shim remembers nothing
 * across a bounce, and the record is the only place the work's own start
 * survives. Work the record cannot describe is OMITTED and logged rather than
 * announced as a handle with nothing behind it.
 */
export function announceLiveWork(
  entries: readonly conversationv1.HistoryEntryAt[],
  work: readonly conversationv1.DetachedWorkId[],
  owner: conversationv1.AgentId,
  onUndescribed?: (handle: conversationv1.DetachedWorkId) => void,
): conversationv1.AgentDetachedWork[] {
  const announcements: conversationv1.AgentDetachedWork[] = [];
  for (const handle of work) {
    // THE HANDLE IS THE UNIT (ruling, landing 3), so the lookup is equality.
    const unit = create(conversationv1.AgentActivityIdSchema, { value: handle.value });
    const described = describeDetachable(findUnit(entries, unit));
    if (described === undefined) {
      // WHO OWNS THE RECORD DEPENDS ON WHO CAN TELL THE TWO CASES APART, and
      // this function cannot. `GetLiveWork` is scoped to this session's
      // lineage, so a handle with no start in THIS book is either work whose own
      // start the record lost -- a defect -- or work a SUBAGENT of this session
      // announced, whose start lives in that subagent's book. Only the caller
      // can ask the vendor which, so a caller that passes this hands over the
      // record and gets the handle instead.
      if (onUndescribed !== undefined) {
        onUndescribed(handle);
        continue;
      }
      LOGGER.debug(
        { work: handle.value },
        "the record holds no describable start for this live work; it is not announced",
      );
      continue;
    }
    announcements.push(
      create(conversationv1.AgentDetachedWorkSchema, {
        work: handle,
        // THE BOOK THE START WAS FOUND IN IS THE OWNER'S: `entries` is one
        // agent's book, and a unit described from it is that agent's call.
        owner,
        origin: {
          case: "created",
          value: create(conversationv1.DetachedWorkCreatedSchema, { workCreated: described }),
        },
      }),
    );
  }
  LOGGER.debug(
    { announced: announcements.length, live: work.length },
    "described the live work a restarted consumer has never seen",
  );
  return announcements;
}

export function createReconciler(options: ReconcilerOptions): Reconciler {
  /** One session's open obligations, as the store answered them once. */
  const liveWorkOnce = async (
    session: conversationv1.AgentId,
  ): Promise<storev1.GetLiveWorkSuccess> => {
    let response: storev1.GetLiveWorkResponse;
    try {
      response = await options.client.getLiveWork(
        create(storev1.GetLiveWorkRequestSchema, { session }),
      );
    } catch (error) {
      LOGGER.error(
        { agent: session.value, detail: String(error) },
        "the store could not be reached for the open obligations",
      );
      throw transportFailure(error);
    }
    const result = response.result;
    if (result.case === "failure") {
      if (result.value.kind.case === "invalidRequest") {
        // THE STORE REFUSED A REQUEST THIS PROCESS BUILT: a shim defect, and
        // never an answer. It is not waited out (the same bytes are refused
        // again) and it is never read as "no book yet", which would serve an
        // empty obligation set and leave every open item unresolved in silence.
        LOGGER.error(
          { agent: session.value, field: result.value.kind.value.field, detail: result.value.detail },
          "the store refused the open-obligation read as malformed",
        );
        throw new PersistenceError("invalid_request", result.value.detail);
      }
      // warn: a defect because the store refused the obligation read needed for reconciliation.
      LOGGER.warn(
        { agent: session.value, detail: result.value.detail },
        "the store refused to state the open obligations",
      );
      // `storage_failure` and an unset arm both land here, so a store that
      // answers with no reason is unavailable rather than silently benign.
      throw readFailure(result.value);
    }
    if (result.case !== "success") {
      throw new PersistenceError(
        "store_unavailable",
        "the store answered GetLiveWork with no result arm set",
      );
    }
    LOGGER.debug(
      {
        agent: session.value,
        live_agents: result.value.liveAgents.length,
        live_detached: result.value.liveDetached.length,
        live_workflows: result.value.liveWorkflows.length,
      },
      "read the record's open obligations",
    );
    return result.value;
  };

  return {
    // ON THE RETRY SCHEDULE. This read is what StartSession reconciles from and
    // what every joining WatchSession re-announces from, and it was the read a
    // single `SQLITE_BUSY` used to turn into a session fault that never lifted.
    //
    // THE ONLY FORM IS SCOPED. An empty session is refused HERE, before the
    // store is asked: the store would refuse it too, but a request this process
    // could never have built correctly is this process's defect to name.
    liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess> {
      if (session.value === "") {
        const detail = "GetLiveWork names no session; the store is shared and is never read unscoped";
        LOGGER.error({ detail }, "refusing an open-obligation read that names no session");
        return Promise.reject(new PersistenceError("invalid_request", detail));
      }
      return readWithRetry("GetLiveWork", () => liveWorkOnce(session), options);
    },
    closingAgentTerminal,
    closingSubagentTerminal,
    closingBashTerminal,
  };
}
