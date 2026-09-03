/**
 * engine/turn.ts — the turn: delivery, refusal, and what a kill reaches; and
 * the agent-, detached- and history-addressed verbs that hang off it.
 *
 * ONE TURN IN FLIGHT. The DAEMON is the only queue, so a second `StartTurn`
 * while one is open is the daemon's bug and is refused with
 * `StartTurnTurnAlreadyOpen` rather than queued. Queuing it here would create a
 * second queue nobody can see, and the daemon's model of what is pending would
 * silently stop being true.
 *
 * R15: THE PROMPT ROW IS WRITTEN AND ACKED FIRST. `StartTurn` writes its
 * `AgentPrompt` row and has the store's DURABLE ACK before the prompt is
 * submitted — otherwise a crash in between leaves activity hanging under a
 * prompt that was never recorded.
 *
 * HISTORY IS SERVED FROM THE STORE, NEVER FROM MEMORY. `WatchAgent` opens with
 * a page and then tails; `ReadHistory` reads one page. The engine keeps no
 * conversation of its own to serve from, which is why a shim bounce loses
 * nothing a consumer can see.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";
import { promptUpsertKey } from "../store/keys.js";
import {
  PersistenceError,
  type AgentPageSession,
  type PersistEntry,
  type Persistence,
} from "../store/persistence.js";
import { storeItemPointerValue } from "../convert/ids.js";
import {
  detachForegroundDetached,
  detachForegroundRefused,
  killTurnKilled,
  killTurnRefused,
  notFound,
  readHistoryPage,
  readHistoryRefused,
  emptyOpeningPage,
  startTurnAccepted,
  startTurnRefused,
  stopBashRefused,
  stopBashStopped,
  updateAgentDelivered,
  updateAgentRefused,
} from "../service/failures.js";
import type { PermissionGate } from "./permission-gate.js";
import type { LiveWorkEntry, LiveWorkTable } from "./detached.js";
import type { ForegroundUnitTable } from "./foreground.js";
import type { SessionIdentity } from "./identity.js";
import type { QueryLike } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-turn", operation: "shim.engine.turn" });

/** The one turn that may be open. */
export interface OpenTurn {
  readonly id: conversationv1.TurnId;
  /** True when the shim opened this turn for its own keep-alive. */
  readonly keepalive: boolean;
  readonly startedAtMs: number;
}

/** What the turn verbs need from the session that owns them. */
export interface SessionContext {
  readonly persistence: Persistence;
  readonly gate: PermissionGate;
  readonly live: LiveWorkTable;
  /** The tool calls in flight, which is what tells DetachForeground's arms apart. */
  readonly foreground: ForegroundUnitTable;
  /** The session's identity, or absence before StartSession. */
  identity(): SessionIdentity | undefined;
  /** The one live query, or absence when it is dead or not yet started. */
  query(): QueryLike | undefined;
  nowMs(): number;
  openTurn(): OpenTurn | undefined;
  /**
   * Deliver a prompt to the vendor.
   *
   * The session owns this because the yield obligation may have to REPLACE the
   * query (a rewind is a fresh `resume` + `resumeSessionAt`), and the query is
   * the session's.
   */
  submit(said: conversationv1.UserSaid, keepalive: boolean): Promise<void>;
  /** Adopt (or clear) the open turn. */
  setOpenTurn(turn: OpenTurn | undefined): void;
  /**
   * Register an open `WatchAgent` tail so the teardown can conclude it.
   *
   * A standing tail must not be CUT at the exit: the consumer is waiting on it
   * for the interrupted terminal the teardown is about to write, and a cut
   * stream reaches it as a transport failure instead. The teardown concludes
   * every registered tail through the book's head and waits for it to end.
   *
   * Returns the callback the handler runs when its stream is finished, however
   * it finished.
   */
  watcherOpened(agent: conversationv1.AgentId, page: AgentPageSession): () => void;
  /**
   * Register one open `WatchBash` stream, so the teardown can wait for it.
   *
   * A stopped run's interrupted terminal is written by the teardown itself, and
   * a consumer must RECEIVE it before the process goes — a stream cut where a
   * terminal was owed reads as a transport failure. Returns the callback that
   * retires the registration, however the stream ended.
   */
  bashWatcherOpened(work: conversationv1.DetachedWorkId): () => void;
  /**
   * Write the `interrupted.by_user` terminal for every SHELL run in `entries`.
   *
   * THE ACT WAS OURS, SO THE RECORD IS OURS. A detached shell's rows are the
   * sidecar's, but the spool's `EXIT=` line read alone says only what code it
   * exited with — that a user asked for the stop is known here and nowhere
   * else. Subagents are untouched: their terminals come from the vendor's own
   * `task_notification`, which does state `stopped`.
   */
  concludeStoppedRuns(entries: readonly LiveWorkEntry[]): void;
  /**
   * Whether this shim has ever ANNOUNCED the named agent.
   *
   * The main agent, anything the live table holds or watched retire, and the
   * subagents the record named at reconciliation. IT IS THE PRODUCER'S SIDE OF
   * THE UNKNOWN-TARGET QUESTION, and since landing 7 it answers in two places:
   *   - The store DOES refuse a book it holds no rows for
   *     (`OpenAgentSessionFailure.unknown_agent`), and the main agent's row is
   *     created only by its first write — so this predicate is what says
   *     whether that refusal names a fresh agent whose first row is still
   *     coming (wait it out, serve an empty page) or an id nobody minted
   *     (refuse).
   *   - A store that serves an EMPTY book instead of refusing leaves the same
   *     ambiguity, and this answers it there too.
   */
  knowsAgent(agent: conversationv1.AgentId): boolean;
  /**
   * Report that the record plane could not be reached.
   *
   * A REFUSAL IS NOT A REPORT. The caller of the verb learns its own call was
   * refused; every OTHER consumer -- the daemon watching this session's health,
   * deciding whether to trust what it is painting -- learns nothing from that.
   * The fault is what tells them, and without it an unreachable store looked
   * like a session in perfect health serving one odd refusal.
   */
  reportStoreUnreachable(detail: string): void;
}

/**
 * What the user said, as one prompt string.
 *
 * TEXT blocks are the text. An IMAGE block contributes its PATH or URL as its
 * own line: the vendor's own prompt convention resolves a bare absolute path or
 * url, and re-encoding the bytes here would make the shim a second image
 * pipeline. An `UnsupportedBlock` RAISES — the contract says it is "not a
 * fallback", so one arriving in a prompt is a producer defect and delivering
 * the prompt without it would silently drop what the user sent.
 */
export function saidText(said: conversationv1.UserSaid): string {
  const lines: string[] = [];
  for (const block of said.content?.blocks ?? []) {
    switch (block.block.case) {
      case "text":
        lines.push(block.block.value.text);
        break;
      case "image": {
        const location = block.block.value.location;
        lines.push(
          location.case === "path"
            ? location.value.path
            : location.case === "url"
              ? location.value.url
              : "",
        );
        break;
      }
      case "unsupported":
        throw new Error(
          `shim turn: a prompt carries an UnsupportedBlock (${block.block.value.kind}); ` +
            "UnsupportedBlock is not a fallback and the shim will not deliver a prompt with one silently dropped",
        );
      default:
        throw new Error("shim turn: a prompt content block carries no arm");
    }
  }
  return lines.join("\n");
}

/** A `UserSaid` carrying one text block — how the shim spells its own prompts. */
export function textSaid(text: string): conversationv1.UserSaid {
  return create(conversationv1.UserSaidSchema, {
    content: create(conversationv1.UserContentSchema, {
      blocks: [
        create(conversationv1.UserContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
        }),
      ],
    }),
  });
}

/** The prompt as delivered: the daemon's id adopted, the recipient resolved. */
export function buildPrompt(
  turn: conversationv1.TurnId,
  agent: conversationv1.AgentId,
  said: conversationv1.UserSaid,
  origin: conversationv1.PromptOrigin,
): conversationv1.AgentPrompt {
  return create(conversationv1.AgentPromptSchema, { id: turn, agent, said, origin });
}

/** The one served prompt row (R15). */
export function promptEntry(
  prompt: conversationv1.AgentPrompt,
  agentId: conversationv1.AgentId,
  keepalive: boolean,
): PersistEntry {
  const turn = prompt.id;
  if (turn === undefined) throw new Error("shim turn: a prompt with no turn id cannot be recorded");
  return {
    agentId,
    upsertKey: promptUpsertKey(turn),
    // A prompt has no vendor record yet — the shim writes it BEFORE the vendor
    // sees it, which is the whole point of R15 — so the turn id is its
    // coordinate, and it is unique by construction.
    source: { vendorUuid: turn.value, discriminator: "agent_prompt" },
    keepalive,
    item: { kind: "prompt", prompt },
  };
}

/** The turn verbs, over one session. */
export class TurnEngine {
  constructor(private readonly session: SessionContext) {}

  // -- StartTurn ------------------------------------------------------------

  async startTurn(request: shimv1.StartTurnRequest): Promise<shimv1.StartTurnResponse> {
    const identity = this.session.identity();
    if (identity === undefined) {
      return startTurnRefused({ kind: "noSession" }, "no session has been started on this shim");
    }
    const open = this.session.openTurn();
    if (open !== undefined) {
      LOGGER.log(
        { level: "warn", open_turn: open.id.value, requested_turn: request.turn?.value ?? "" },
        "REFUSED a second StartTurn: one turn is in flight and the daemon is the only queue",
      );
      return startTurnRefused(
        { kind: "turnAlreadyOpen" },
        `turn ${open.id.value} is already in flight; the daemon holds the queue and the shim never does`,
      );
    }
    if (this.session.query() === undefined) {
      return startTurnRefused({ kind: "queryDead" }, "the vendor query is dead; nothing can accept a prompt");
    }
    const turn = request.turn;
    const said = request.said;
    if (turn === undefined || said === undefined) {
      // validate/ refuses this at the wire; the guard keeps the invariant local.
      throw new Error("shim turn: StartTurn reached the engine without a turn id or a prompt");
    }
    const prompt = buildPrompt(turn, identity.agentId, said, request.origin);
    const promptRow = promptEntry(prompt, identity.agentId, false);
    // R15: the prompt row is DURABLE before anything the turn does is written.
    try {
      await this.session.persistence.writeDurable([promptRow]);
    } catch (err) {
      if (!(err instanceof PersistenceError)) throw err;
      // A STORE OUTAGE IS NOT A REFUSED TURN. The daemon asked for work the
      // vendor can do, and answering Code.Internal would leave it unable to
      // tell an unreachable store from a shim defect; refusing the turn would
      // make the record plane's availability the session's. So the row goes
      // back through the ORDERED retry buffer, which owns the outage: it
      // replays transiently, opens a degraded window, raises store_unreachable,
      // and -- if it never lands -- drops loudly naming the key. The write id
      // is deterministic, so a partially-landed batch absorbs the replay.
      LOGGER.log(
        { level: "error", turn_id: turn.value, upsert_key: promptRow.upsertKey, cause: err.message },
        "the prompt row could not be made durable before the turn; re-queued it behind the retry buffer",
      );
      this.session.persistence.write([promptRow]);
    }
    // ONE CALL SUBMITS AND PAINTS — and the page is read BEFORE the prompt is
    // delivered, not after. R15 (RULED) says a fresh session's opening page
    // holds EXACTLY the prompt row, and the prompt row is already durable here,
    // so reading now is what makes that deterministic. Reading after the submit
    // instead made the page's contents a RACE against however fast the vendor
    // answered: a turn whose first API response opened a reasoning block before
    // the read returned painted two entries, and one whose vendor was a
    // millisecond slower painted one. A consumer's first paint still carries
    // the turn it just opened, and everything the turn goes on to produce
    // reaches it on its own WatchAgent.
    const page = await this.openingPage(identity.agentId, request.pageSize, request.knownThrough);
    this.session.setOpenTurn({ id: turn, keepalive: false, startedAtMs: this.session.nowMs() });
    try {
      await this.session.submit(said, false);
    } catch (err) {
      this.session.setOpenTurn(undefined);
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.log({ level: "error", turn_id: turn.value, cause: detail }, "the vendor refused the prompt");
      return startTurnRefused({ kind: "vendorRefused" }, detail);
    }
    LOGGER.log(
      { turn_id: turn.value, origin: request.origin, page_entries: page.entries.length },
      "opened a turn, delivered its prompt, and painted the opening page",
    );
    return startTurnAccepted(prompt, page);
  }

  /**
   * The opening page StartTurn answers with.
   *
   * The page session's tail is closed IMMEDIATELY: StartTurn paints once and the
   * consumer follows with its own WatchAgent, so a tail left open here would be
   * a second reader of the same book that nobody drains.
   *
   * A store that cannot be read does NOT fail the call: the prompt is already
   * durable and already delivered, so a failure would tell the daemon a turn did
   * not start that is running. The empty page says "nothing to paint from here".
   */
  private async openingPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage> {
    let opened: AgentPageSession | undefined;
    try {
      opened = await this.session.persistence.openAgentPage(agent, pageSize, knownThrough);
      return opened.page;
    } catch (err) {
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.log(
        { level: "warn", agent_id: agent.value, cause: detail },
        "the opening page could not be read; answering an empty page rather than failing a turn that is running",
      );
      return emptyOpeningPage();
    } finally {
      opened?.close();
    }
  }

  // -- UpdateAgent ----------------------------------------------------------

  async updateAgent(request: shimv1.UpdateAgentRequest): Promise<shimv1.UpdateAgentResponse> {
    const identity = this.session.identity();
    if (identity === undefined) {
      return updateAgentRefused({ kind: "noSession" }, "no session has been started on this shim");
    }
    const input = request.input;
    if (input === undefined) {
      throw new Error("shim turn: UpdateAgent reached the engine with no input");
    }
    const target = request.target ?? identity.agentId;
    const isMain = target.value === identity.agentId.value;
    switch (input.input.case) {
      case "stop":
        return this.stopAgent(target, isMain);
      case "answer":
        return this.answer(input.input.value);
      case "prompt":
        return this.promptAgent(target, isMain);
      default:
        throw new Error("shim turn: UpdateAgent reached the engine with no input arm");
    }
  }

  private async stopAgent(
    target: conversationv1.AgentId,
    isMain: boolean,
  ): Promise<shimv1.UpdateAgentResponse> {
    const query = this.session.query();
    if (query === undefined) {
      return updateAgentRefused({ kind: "nothingRunning" }, "the vendor query is dead; nothing is running");
    }
    if (isMain) {
      if (this.session.openTurn() === undefined) {
        return updateAgentRefused({ kind: "nothingRunning" }, "the main agent has no turn in flight");
      }
      // Callback liveness FIRST: an unresolved canUseTool promise survives an
      // interrupt and wedges the vendor.
      this.session.gate.standDown("the main agent was stopped");
      await query.interrupt();
      LOGGER.log({ agent_id: target.value }, "interrupted the main agent");
      return updateAgentDelivered();
    }
    // ONE HANDLE, NO VENDOR IDS. A subagent's `AgentId` on the wire is the
    // SPAWNING CALL's tool_use_id — the vendor's own task id never crosses the
    // boundary — so a target is resolved through the spawn map as well as by
    // the task id. Looking only at the task id refused every stop a consumer
    // addressed by the id it was actually given.
    const entry =
      this.session.live.all().find((item) => item.taskId === target.value) ??
      this.session.live.byToolUseId(target.value);
    if (entry === undefined) {
      return updateAgentRefused(
        { kind: "unknownAgent" },
        `no live work is addressed by ${JSON.stringify(target.value)}`,
      );
    }
    await query.stopTask(entry.taskId);
    LOGGER.log({ agent_id: target.value, task_id: entry.taskId }, "stopped a subagent by its task id");
    return updateAgentDelivered();
  }

  private answer(answer: conversationv1.AgentAnswer): Promise<shimv1.UpdateAgentResponse> {
    switch (answer.answer.case) {
      case "questionAnswer": {
        const value = answer.answer.value;
        const ask = value.ask;
        const answers = value.answers;
        if (ask === undefined || answers === undefined) {
          throw new Error("shim turn: a question answer carries no ask or no answers");
        }
        const outcome = this.session.gate.answerQuestion(ask, answers);
        return Promise.resolve(this.answerOutcome(outcome, "question", ask.value));
      }
      case "permissionDecision": {
        const outcome = this.session.gate.decidePermission(answer.answer.value);
        return Promise.resolve(
          this.answerOutcome(outcome, "permission", answer.answer.value.ask?.value ?? ""),
        );
      }
      default:
        throw new Error("shim turn: an answer carries no arm");
    }
  }

  private answerOutcome(
    outcome: "delivered" | "no_open_ask" | "answer_mismatch",
    what: string,
    id: string,
  ): shimv1.UpdateAgentResponse {
    if (outcome === "delivered") return updateAgentDelivered();
    if (outcome === "no_open_ask") {
      return updateAgentRefused({ kind: "noOpenAsk" }, `no ${what} is open under ${JSON.stringify(id)}`);
    }
    return updateAgentRefused(
      { kind: "answerMismatch" },
      `the answer echoed for ${JSON.stringify(id)} does not match the ${what} the shim is holding`,
    );
  }

  /**
   * Prompt an EXISTING agent.
   *
   * REFUSED, AND THE GAP IS REPORTED RATHER THAN IMPROVISED. The pinned SDK
   * declares no route that delivers a prompt to a named subagent: `streamInput`
   * takes an `SDKUserMessage` whose `parent_tool_use_id` is documented as which
   * tool use a message BELONGS TO (the tool-result path) and whose
   * `subagent_type`/`task_description` are documented as which subagent
   * PRODUCED a message — neither is an address. Guessing that one of them
   * doubles as a delivery address would, if wrong, inject the user's words into
   * the main thread as if the user had said them there.
   *
   * The main agent is refused separately and for a different reason: prompting
   * it is `StartTurn`, and accepting it here would create a second submitter.
   */
  private promptAgent(
    target: conversationv1.AgentId,
    isMain: boolean,
  ): Promise<shimv1.UpdateAgentResponse> {
    if (isMain) {
      return Promise.resolve(
        updateAgentRefused(
          { kind: "unknownAgent" },
          "UpdateAgent.prompt never targets the session's own turn; StartTurn is that verb",
        ),
      );
    }
    // THE ADDRESSEE'S STATE IS ANSWERED FIRST. A subagent whose own turn is
    // still running is BUSY, and the route question never arises: the daemon
    // relays this as SubmitPromptError.bubble_refused{agent_busy}, and this
    // refusal is that relay's one producer (landing 7). The live table holds
    // only running work -- an entry retires the moment its task settles -- so
    // an entry addressed by the target IS the running turn.
    //
    // `local_agent` (or an unstated kind, which the vendor leaves unset for a
    // spawned agent) is the AGENT kind; a `local_bash` shell under the same
    // handle is not an agent and is left to the route refusal below.
    const busy = this.session.live
      .all()
      .find((item) => item.taskId === target.value || item.toolUseId === target.value);
    if (
      busy !== undefined &&
      (busy.taskType === undefined || busy.taskType === "" || busy.taskType === "local_agent")
    ) {
      LOGGER.log(
        { level: "warn", agent_id: target.value, task_id: busy.taskId },
        "REFUSED UpdateAgent.prompt to a subagent whose own turn is already running",
      );
      return Promise.resolve(
        updateAgentRefused(
          { kind: "agentBusy" },
          `agent ${JSON.stringify(target.value)} has a turn of its own already running; ` +
            "a prompt to a busy subagent is refused rather than queued",
        ),
      );
    }
    LOGGER.log(
      { level: "warn", agent_id: target.value, gap: "no_declared_subagent_prompt_route" },
      "REFUSED UpdateAgent.prompt to a subagent: the pinned SDK declares no route that delivers a prompt to a named agent",
    );
    // NOT `nothingRunning`: that is a claim about the AGENT'S STATE, and it
    // would send a caller looking for a live agent that was live all along. The
    // gap is the SDK's — no route reaches a named agent — and the same input to
    // the main agent would deliver.
    return Promise.resolve(
      updateAgentRefused(
        { kind: "notDeliverable" },
        `the pinned agent SDK declares no route that delivers a prompt to agent ${JSON.stringify(target.value)}; ` +
          "the shim refuses rather than guessing an address (reported as a contract gap)",
      ),
    );
  }

  // -- KillTurn -------------------------------------------------------------

  async killTurn(request: shimv1.KillTurnRequest): Promise<shimv1.KillTurnResponse> {
    if (this.session.identity() === undefined) {
      return killTurnRefused({ kind: "noSession" }, "no session has been started on this shim");
    }
    const open = this.session.openTurn();
    const requested = request.turn?.value ?? "";
    // A TURN THAT CLOSED CAN STILL OWN LIVE WORK. Detached work outlives the
    // turn that spawned it by design, and KillTurn is the verb that ends a
    // turn AND EVERYTHING IT SPAWNED, transitively -- so answering `no turn is
    // open` while that work is still running would leave the daemon with a
    // running shell it has no verb to stop under the turn it belongs to.
    // `no_turn_open` stays the answer only when there is genuinely nothing of
    // this turn's left.
    if (open === undefined) {
      const orphaned = this.session.live.spawnedBy(requested);
      if (orphaned.length === 0) {
        return killTurnRefused({ kind: "noTurnOpen" }, "no turn is open");
      }
      return this.killSpawnedWork(requested, orphaned, request.force);
    }
    if (requested !== open.id.value) {
      return killTurnRefused(
        { kind: "notTheOpenTurn" },
        `turn ${JSON.stringify(requested)} is not the open turn (${open.id.value})`,
      );
    }
    const spawned = this.session.live.spawnedBy(open.id.value);
    const announceable = spawned.filter((entry) => !entry.skipTranscript);
    if (!request.force && announceable.length > 0) {
      LOGGER.log(
        { level: "warn", turn_id: open.id.value, live: announceable.length },
        "REFUSED KillTurn: the turn still has live work and force was not set",
      );
      return killTurnRefused(
        { kind: "live", live: create(conversationv1.TurnLiveSchema, { liveWork: this.session.live.workIds(announceable) }) },
        `turn ${open.id.value} spawned ${announceable.length} live item(s); pass force to end them`,
      );
    }
    const query = this.session.query();
    // Callback liveness before the interrupt, always.
    this.session.gate.standDown(`turn ${open.id.value} was killed`);
    if (query !== undefined) {
      await query.interrupt();
      for (const entry of spawned) await query.stopTask(entry.taskId);
    }
    // A SUBAGENT'S TERMINAL ARRIVES ON ITS OWN, in the `task_notification` the
    // stop provokes; A SHELL'S DOES NOT, because no vendor message states that
    // a shell run was stopped. Every item this kill named concludes, or the
    // consumer is left watching work that will never end.
    this.session.concludeStoppedRuns(spawned);
    this.session.setOpenTurn(undefined);
    const killed = create(conversationv1.TurnKilledSchema, {
      how:
        spawned.length === 0
          ? { case: "agentOnly", value: create(conversationv1.TurnKilledAgentOnlySchema, {}) }
          : {
              case: "forced",
              value: create(conversationv1.TurnKilledForcedSchema, {
                stoppedWork: this.session.live.workIds(spawned),
              }),
            },
    });
    LOGGER.log({ turn_id: open.id.value, stopped: spawned.length }, "killed a turn and everything it spawned");
    return killTurnKilled(killed);
  }

  /**
   * End what a CLOSED turn left running.
   *
   * The same refusal and the same forced outcome as the open-turn path: the
   * consumer's question ("end this turn and its work") does not change because
   * the agent already stopped talking.
   */
  private async killSpawnedWork(
    turnId: string,
    spawned: readonly LiveWorkEntry[],
    force: boolean,
  ): Promise<shimv1.KillTurnResponse> {
    const announceable = spawned.filter((entry) => !entry.skipTranscript);
    if (!force && announceable.length > 0) {
      LOGGER.log(
        { level: "warn", turn_id: turnId, live: announceable.length },
        "REFUSED KillTurn: the turn has closed but still has live work and force was not set",
      );
      return killTurnRefused(
        {
          kind: "live",
          live: create(conversationv1.TurnLiveSchema, {
            liveWork: this.session.live.workIds(announceable),
          }),
        },
        `turn ${turnId} spawned ${announceable.length} live item(s); pass force to end them`,
      );
    }
    const query = this.session.query();
    if (query !== undefined) {
      for (const entry of spawned) await query.stopTask(entry.taskId);
    }
    this.session.concludeStoppedRuns(spawned);
    LOGGER.log(
      { turn_id: turnId, stopped: spawned.length },
      "killed the live work a closed turn left running",
    );
    return killTurnKilled(
      create(conversationv1.TurnKilledSchema, {
        how: {
          case: "forced",
          value: create(conversationv1.TurnKilledForcedSchema, {
            stoppedWork: this.session.live.workIds(spawned),
          }),
        },
      }),
    );
  }

  // -- DetachForeground -----------------------------------------------------

  /**
   * Ctrl-B.
   *
   * The pinned SDK's only declared handle on this is
   * `query.backgroundTasks(toolUseId)`, which ANSWERS whether the call has live
   * background work — it does not cause detachment. So this verb reports what
   * the vendor already did and refuses what it cannot do, rather than
   * pretending: a unit the vendor has backgrounded is detached (the
   * announcement rides its own task stream), and one it has not is
   * `NotDetachable` with the reason stated. The missing route is a reported
   * contract gap, not something to improvise around.
   */
  async detachForeground(
    request: shimv1.DetachForegroundRequest,
  ): Promise<shimv1.DetachForegroundResponse> {
    if (this.session.identity() === undefined) {
      return detachForegroundRefused({ kind: "noSession" }, "no session has been started on this shim");
    }
    const unit = request.unit?.value ?? "";
    const query = this.session.query();
    if (query === undefined) {
      return detachForegroundRefused({ kind: "alreadyConcluded" }, "the vendor query is dead");
    }
    const known = this.session.live.byToolUseId(unit);
    const live = await query.backgroundTasks(unit);
    // THE FOREGROUND TABLE IS WHAT KEEPS THE FOUR REFUSALS APART. Without it
    // the engine can only tell "the vendor holds background work for this id"
    // from "it does not", and three of the four answers collapse onto
    // `unknown_unit` -- which tells a consumer to stop offering an affordance
    // for work that backgrounds itself routinely.
    const verdict = this.session.foreground.verdict(unit);
    LOGGER.logVerbose(
      { unit, vendor_holds_background: live, known_detached: known !== undefined, foreground: verdict.kind },
      "judging a DetachForeground against the foreground table",
    );
    // KIND BEFORE STATE: "this kind cannot be detached at all" stays true
    // whether or not the unit finished, and saying `already_concluded` for it
    // would invite the consumer to try again on the next one of its kind.
    if (verdict.kind === "not_detachable") {
      return detachForegroundRefused(
        { kind: "notDetachable" },
        `unit ${JSON.stringify(unit)} is of a kind that cannot be detached; ` +
          "the detachable kinds are subagent, bash, workflow and monitor",
      );
    }
    if (!live) {
      if (known !== undefined || verdict.kind === "settled") {
        return detachForegroundRefused(
          { kind: "alreadyConcluded" },
          `unit ${JSON.stringify(unit)} has no live background work`,
        );
      }
      if (verdict.kind === "unknown") {
        return detachForegroundRefused(
          { kind: "unknownUnit" },
          `no live unit is addressed by ${JSON.stringify(unit)}`,
        );
      }
    }
    // THE VENDOR MADE THIS DETACHMENT, NOT US. `backgroundTasks(unit)` is an
    // OBSERVATION: it says the vendor already holds live background work for
    // the unit, which is a detachment the shim can confirm. The pinned SDK
    // offers no verb to INITIATE one, so a unit that is detachable in kind and
    // still in flight in the FOREGROUND is refused `unsupported` -- never
    // `notDetachable`, which would say its kind cannot detach at all and tell a
    // consumer to stop offering an affordance for work that backgrounds itself
    // routinely.
    // `backgroundTasks(unit) === true` IS the confirmation: the vendor holding
    // live background work for the unit is the detachment, observed. The live
    // table's own `backgrounded` flag is a laggier restatement of the same fact
    // (it arrives on a later `background_tasks_changed`), so requiring it too
    // refused detachments the vendor had already made.
    if (!live && verdict.kind === "live_detachable") {
      LOGGER.log(
        { level: "warn", unit, gap: "no_declared_detach_verb" },
        "REFUSED DetachForeground: the unit is detachable in kind and live, but the pinned SDK offers no verb to initiate a detachment",
      );
      return detachForegroundRefused(
        { kind: "unsupported" },
        `unit ${JSON.stringify(unit)} is detachable in kind and still in flight, but the pinned agent SDK ` +
          "offers no verb to initiate a detachment; the shim can only observe detachments the vendor made " +
          "(reported as a contract gap)",
      );
    }
    LOGGER.log({ unit }, "reported a foreground unit as detached: the vendor holds live background work for it");
    return detachForegroundDetached();
  }

  // -- WatchAgent / ReadHistory --------------------------------------------

  /**
   * One agent's page then its tail, served FROM THE STORE.
   *
   * An unknown target closes the stream at the transport: a stream's response
   * type is the frame it carries, so it has no arm to say "refused".
   *
   * A FRESH AGENT IS NOT AN UNKNOWN ONE. The endpoint contract has the daemon
   * open the main agent's watch before any turn, and the store's row for that
   * agent is created by its FIRST WRITE — so the open races the write and loses
   * on every fresh bring-up. There the producer is the arbiter: `knowsAgent`
   * rides down as the record plane's licence to wait that race out, serving the
   * empty opening page the contract promises and standing the tail on the first
   * row. An id this shim never announced still gets the store's refusal, and
   * every other refusal (an unreachable store) is untouched.
   */
  async *watchAgent(request: shimv1.WatchAgentRequest): AsyncIterable<shimv1.WatchAgentResponse> {
    const identity = this.session.identity();
    if (identity === undefined) throw notFound("no session has been started on this shim");
    const target = request.target ?? identity.agentId;
    let opened;
    try {
      opened = await this.session.persistence.openAgentPage(
        target,
        request.pageSize,
        request.knownThrough,
        () => this.session.knowsAgent(target),
      );
    } catch (err) {
      throw err instanceof PersistenceError
        ? notFound(`WatchAgent(${target.value}): ${err.message}`)
        : err;
    }
    // A STORE THAT REFUSES NOTHING still leaves the empty book ambiguous: it is
    // either an agent nothing has been written for yet or an id nobody ever
    // minted, and only the producer can tell those apart. An id this shim never
    // announced names no agent, and standing a tail on it would leave a
    // consumer watching forever for frames that can never come.
    //
    // A stream has no arm to say "refused" — its response type is the frame it
    // carries — so the refusal closes the stream at the transport.
    if (opened.page.entries.length === 0 && !this.session.knowsAgent(target)) {
      opened.close();
      LOGGER.log(
        { level: "warn", agent_id: target.value },
        "REFUSED WatchAgent: the record holds no rows for this target and this shim never announced it",
      );
      throw notFound(
        `WatchAgent(${target.value}): no agent by that id has been announced by this session, and the ` +
          "record holds no rows under it",
      );
    }
    const watcherEnded = this.session.watcherOpened(target, opened);
    try {
      yield create(shimv1.WatchAgentResponseSchema, {
        frame: { case: "page", value: opened.page },
      });
      for await (const entry of opened.tail) {
        yield create(shimv1.WatchAgentResponseSchema, { frame: { case: "entry", value: entry } });
      }
    } finally {
      watcherEnded();
      opened.close();
    }
  }

  /** One page of one agent's durable past, newest first. */
  async readHistory(request: shimv1.ReadHistoryRequest): Promise<shimv1.ReadHistoryResponse> {
    const identity = this.session.identity();
    if (identity === undefined) {
      return readHistoryRefused({ kind: "unknownAgent" }, "no session has been started on this shim");
    }
    const target = request.target ?? identity.agentId;
    try {
      if (request.position.case === "after") {
        const after = request.position.value;
        // Validated at the wire; read back through ids.ts so an empty pointer
        // cannot reach the store as a legal-looking cursor.
        storeItemPointerValue(after);
        return readHistoryPage(
          await this.session.persistence.readAgentPage(target, request.pageSize, after),
        );
      }
      // THE SAME PRODUCER VERDICT `WatchAgent` GIVES. An agent this shim
      // announced whose first row has not landed has an EMPTY past, not an
      // unknown one — a keep-alive-only session writes nothing to any book, and
      // refusing there told a consumer its own agent did not exist.
      const opened = await this.session.persistence.openAgentPage(
        target,
        request.pageSize,
        undefined,
        () => this.session.knowsAgent(target),
      );
      // ReadHistory is one page and no tail; the reading session opened to get
      // the page is closed at once rather than leaked for a tail nobody reads.
      opened.close();
      return readHistoryPage(opened.page);
    } catch (err) {
      if (err instanceof PersistenceError) {
        const kind =
          err.kind === "unknown_agent"
            ? ({ kind: "unknownAgent" } as const)
            : err.kind === "stale_pointer"
              ? ({ kind: "stalePointer" } as const)
              : ({ kind: "storeUnavailable" } as const);
        LOGGER.log({ level: "warn", agent_id: target.value, kind: err.kind }, "ReadHistory refused");
        if (kind.kind === "storeUnavailable") {
          this.session.reportStoreUnreachable(`ReadHistory(${target.value}): ${err.message}`);
        }
        return readHistoryRefused(kind, err.message);
      }
      throw err;
    }
  }

  // -- WatchBash / StopBash -------------------------------------------------

  /** Follow one backgrounded shell: its start, the sidecar-fed deltas, its terminal. */
  async *watchBash(request: shimv1.WatchBashRequest): AsyncIterable<shimv1.WatchBashResponse> {
    const work = request.work;
    if (work === undefined) throw notFound("WatchBash reached the engine with no work id");
    // THE LIVE TABLE IS THE SHIM'S OWN ANSWER to "does this run exist". The
    // store refuses a run before its first row lands, and the daemon opens its
    // watch on the announcement, so the refusal is waited out while the shim
    // still holds the run and only then reported.
    const stillLive = (): boolean => this.session.live.byToolUseId(work.value) !== undefined;
    // A REFUSAL CAN SURFACE FROM THE ITERATION, not only from the open: the
    // store's own stream is what refuses, and its first frame is pulled when
    // the tail is read. Both are mapped, or the transport answers a bare
    // `internal error` for a refusal the shim understood perfectly well.
    const watcherEnded = this.session.bashWatcherOpened(work);
    try {
      const run = await this.session.persistence.openBashRun(work, stillLive);
      for await (const frame of run) {
        yield create(shimv1.WatchBashResponseSchema, { bash: frame });
      }
    } catch (err) {
      if (err instanceof PersistenceError) {
        LOGGER.log(
          { level: "warn", work_id: work.value, kind: err.kind },
          "WatchBash refused",
        );
        throw notFound(`WatchBash(${work.value}): ${err.message}`);
      }
      throw err;
    } finally {
      watcherEnded();
    }
  }

  /** Kill a backgrounded shell — the only input a process takes. */
  async stopBash(request: shimv1.StopBashRequest): Promise<shimv1.StopBashResponse> {
    const work = request.work;
    if (work === undefined) throw new Error("shim turn: StopBash reached the engine with no work id");
    // A HANDLE NAMES THE SPAWNING CALL (ruling, landing 3), not the vendor's
    // task id — so the resolution is by tool_use_id, and the task id it finds is
    // the internal address `stopTask` wants.
    const entry = this.session.live.byToolUseId(work.value);
    if (entry === undefined) {
      // TWO REASONS FOR ONE MISS, and the arms mean different things. A handle
      // the table watched RETIRE names a shell that ended, which is what
      // `already_ended` says; a handle it never knew names nothing at all.
      if (this.session.live.retired(work.value)) {
        LOGGER.log({ level: "warn", work_id: work.value }, "StopBash refused: the shell already ended");
        return stopBashRefused(
          { kind: "alreadyEnded" },
          `the shell addressed by ${JSON.stringify(work.value)} has already ended`,
        );
      }
      LOGGER.log({ level: "warn", work_id: work.value }, "StopBash refused: no live shell carries this handle");
      return stopBashRefused(
        { kind: "unknownWork" },
        `no live detached work is addressed by ${JSON.stringify(work.value)}`,
      );
    }
    const query = this.session.query();
    if (query === undefined) {
      return stopBashRefused({ kind: "alreadyEnded" }, "the vendor query is dead; the run cannot still be live");
    }
    await query.stopTask(entry.taskId);
    // THE STOP WAS OURS, SO ITS TERMINAL IS OURS. The spool's `EXIT=143` line
    // is all the sidecar can see, and read alone it says "exited 143" — only
    // this shim knows a user asked for the kill, which is what
    // `interrupted.by_user` states. The row shares the run's terminal upsert
    // key, so a sidecar row for the same run supersedes rather than duplicates.
    this.session.concludeStoppedRuns([entry]);
    LOGGER.log({ work_id: work.value }, "stopped a detached shell run");
    return stopBashStopped();
  }
}
