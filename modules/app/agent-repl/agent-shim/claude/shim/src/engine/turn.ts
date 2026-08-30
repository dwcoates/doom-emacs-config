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
import { detachedWorkId, storeItemPointerValue } from "../convert/ids.js";
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
import type { LiveWorkTable } from "./detached.js";
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
    // R15: the prompt row is DURABLE before anything the turn does is written.
    await this.session.persistence.writeDurable([promptEntry(prompt, identity.agentId, false)]);
    this.session.setOpenTurn({ id: turn, keepalive: false, startedAtMs: this.session.nowMs() });
    try {
      await this.session.submit(said, false);
    } catch (err) {
      this.session.setOpenTurn(undefined);
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.log({ level: "error", turn_id: turn.value, cause: detail }, "the vendor refused the prompt");
      return startTurnRefused({ kind: "vendorRefused" }, detail);
    }
    // ONE CALL SUBMITS AND PAINTS. The page is read AFTER the prompt is
    // delivered, so it already contains the prompt row and a consumer's first
    // paint is never missing the turn it just opened.
    const page = await this.openingPage(identity.agentId, request.pageSize, request.knownThrough);
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
    const entry = this.session.live.all().find((item) => item.taskId === target.value);
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
    if (open === undefined) {
      return killTurnRefused({ kind: "noTurnOpen" }, "no turn is open");
    }
    const requested = request.turn?.value ?? "";
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
    if (!live && known === undefined) {
      return detachForegroundRefused(
        { kind: "unknownUnit" },
        `no live unit is addressed by ${JSON.stringify(unit)}`,
      );
    }
    if (!live) {
      return detachForegroundRefused(
        { kind: "alreadyConcluded" },
        `unit ${JSON.stringify(unit)} has no live background work`,
      );
    }
    // THE VENDOR MADE THIS DETACHMENT, NOT US. `backgroundTasks(unit)` is an
    // OBSERVATION: it says the vendor already holds live background work for
    // the unit, which is a detachment the shim can confirm. The pinned SDK
    // offers no verb to INITIATE one, so a unit that is detachable in kind and
    // still in flight in the FOREGROUND is refused `unsupported` — never
    // `notDetachable`, which would say its kind cannot detach at all and tell a
    // consumer to stop offering an affordance for work that backgrounds itself
    // routinely.
    if (known !== undefined && known.backgrounded !== true) {
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
      );
    } catch (err) {
      throw err instanceof PersistenceError
        ? notFound(`WatchAgent(${target.value}): ${err.message}`)
        : err;
    }
    try {
      yield create(shimv1.WatchAgentResponseSchema, {
        frame: { case: "page", value: opened.page },
      });
      for await (const entry of opened.tail) {
        yield create(shimv1.WatchAgentResponseSchema, { frame: { case: "entry", value: entry } });
      }
    } finally {
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
      const opened = await this.session.persistence.openAgentPage(target, request.pageSize);
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
    let run;
    try {
      run = await this.session.persistence.openBashRun(work);
    } catch (err) {
      throw err instanceof PersistenceError
        ? notFound(`WatchBash(${work.value}): ${err.message}`)
        : err;
    }
    for await (const frame of run) {
      yield create(shimv1.WatchBashResponseSchema, { bash: frame });
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
    LOGGER.log({ work_id: work.value }, "stopped a detached shell run");
    return stopBashStopped();
  }

  /** The live set as `TurnLive` names it — the refusal's own vocabulary. */
  turnLive(): conversationv1.TurnLive {
    return create(conversationv1.TurnLiveSchema, {
      liveWork: this.session.live.workIds(this.session.live.announceable()),
    });
  }

  /**
   * A work id, for callers that hold the SPAWNING CALL's id.
   *
   * The wire handle is the call's `tool_use_id` (ruling, landing 3); a caller
   * holding a vendor task id resolves it through the live set first.
   */
  static workId(spawningToolUseId: string): conversationv1.DetachedWorkId {
    return detachedWorkId(spawningToolUseId);
  }
}
