/**
 * engine/keepalive.ts — the keep-alive cadence, and the yield it owes a real
 * prompt.
 *
 * RESPONSIBILITY. The vendor's prompt cache lapses on a timer, and a lapsed
 * cache makes the next real turn cost full price. The shim submits its own
 * minimal turns to keep it warm — entirely shim-internal work that no consumer
 * asked for and none should see.
 *
 * THE MARKER IS THE CONTRACT (ruled 2026-08-29). Every keep-alive prompt BEGINS
 * with the literal `<!--agent-repl:keepalive-->`, mirroring the existing
 * `<!--agent-repl:meta-->` marker. NOTHING OF A KEEP-ALIVE IS STORED (2026-09-23):
 * this plane drops every entry its {@link KeepaliveScope} tags at the writer's
 * door (`store/writer.ts`), and the sidecar skips the turn's transcript records
 * by the marker plus the transcript's own promptId and parentUuid links. None
 * of the keep-alive's machinery needs a row: the send, the answer and the
 * rewind anchor below are this process's memory, and the rewind itself reads
 * the vendor's own transcript.
 *
 * THE INTERVAL. Fifty-two minutes (ruled 2026-09-17). The ~5-minute ephemeral
 * cache-invalidation window (`cache_creation.ephemeral_5m_input_tokens`,
 * engine/cold.ts) is an API-BILLING window, not a subscription-billing one:
 * subscription billing's own window is ~1 hour, so the keep-alive fires every
 * 52 minutes to stay inside THAT window, with an eight-minute margin against a
 * slow turn.
 *
 * THE YIELD OBLIGATION. A real prompt must never build on keep-alive context,
 * so before one is delivered the vendor context is ROLLED BACK to just after
 * the last real record. The mechanism is the SDK's own declared one: close the
 * query and open a new one with `resume: <vendor session id>` and
 * `resumeSessionAt: <uuid of the last real record>` — "when resuming, only
 * resume messages up to and including the message with this UUID". It is the
 * only declared surface that truncates a conversation without rewriting the
 * vendor's file, and rewriting the file is the thing the transcript backup
 * exists because we cannot safely do. The rewind is an explicit, tested step:
 * {@link KeepaliveRewind.obligation} answers whether one is owed and names the
 * uuid to resume at.
 *
 * THE SAME ROLLBACK RUNS BETWEEN KEEP-ALIVES (ruled 2026-09-17). The anchor is
 * always the last REAL record — keep-alive turns never advance it — so the same
 * obligation the next real prompt owes is owed by the next keep-alive BEAT too.
 * The cadence discharges it before each beat, so the transcript never holds
 * more than the one keep-alive currently in flight: a degenerate keep-alive
 * (see {@link keepalivePromptText}) is rewound out before the next one is sent,
 * and context can never accumulate a pile of them across an idle night. Because
 * every beat but the first {@link KeepaliveRewind.settled}s the debt, the count
 * an eventual real prompt discards is at most one. The rollback is byte-for-byte
 * the real-prompt rollback — same declared `resumeSessionAt` surface, same
 * uuid, no file rewrite — so nothing about the transcript's safety changes.
 *
 * THE ANCHOR IS AN ASSISTANT RECORD OF A REAL TURN (ruled 2026-09-14). The SDK
 * states the type of the uuid it will accept: "The message ID should be from
 * `SDKAssistantMessage.uuid`". Every OTHER message the vendor emits carries a
 * `uuid` too — `system:init` and `result` both declare one as a REQUIRED field —
 * and those uuids name no transcript record, so a query opened at one is
 * refused by the vendor with `No message found with message.uuid of: <uuid>`
 * and the session dies with the prompt it was carrying. It killed two of the
 * owner's real sessions on 2026-09-14. So {@link KeepaliveRewind.noteRecord}
 * takes the MESSAGE, not a uuid, and refuses everything that is not an
 * `assistant` message of a REAL (non-keep-alive) open turn — the filter cannot
 * be got wrong by a caller, because the caller never extracts the uuid.
 *
 * AND THE ANCHOR DOES NOT CROSS A BOUNDARY. A uuid from before a compaction, a
 * conversation reset, or a fresh query binding may no longer be resumable, so
 * every such event {@link KeepaliveRewind.clearAnchor}s it and the next real
 * prompt after keep-alives simply carries them instead of rewinding.
 */
import { bindLog } from "../log.js";
import type { SdkMessage } from "../sdk/types.js";
import type { SendVerdict } from "./sends.js";

const LOGGER = bindLog({ component: "shim-engine-keepalive", operation: "shim.engine.keepalive" });

/** Every keep-alive prompt BEGINS with this literal. */
export const KEEPALIVE_PROMPT_MARKER = "<!--agent-repl:keepalive-->";

/**
 * Fifty-two minutes: inside subscription billing's ~1-hour cache window, with
 * an eight-minute margin. (The ~5-minute ephemeral window is API billing's,
 * not subscription billing's.)
 */
export const KEEPALIVE_INTERVAL_MS = 52 * 60 * 1000;

/**
 * The prompt itself.
 *
 * Minimal on purpose: it exists to make an API call that reads the cache back,
 * not to get an answer, so anything the model would have to think about is
 * wasted money.
 *
 * NUMBERED, AND WHY (ruled 2026-09-17). Successive keep-alives used to be
 * byte-for-byte identical. On 2026-09-17 a keep-alive turn on claude-opus-5
 * degenerated: instead of answering ".", the model echoed the prompt back into
 * a ~64,000-token block and hit max_tokens, and it did so on turn after turn.
 * A repeated-identical prompt is the pattern that reinforces such an echo loop,
 * so every keep-alive now carries an incrementing counter and no two are the
 * same. The counter trails the instruction, so the marker the store and
 * sidecar classify on is untouched and {@link isKeepalivePrompt} still matches.
 */
export function keepalivePromptText(counter: number): string {
  return `${KEEPALIVE_PROMPT_MARKER}\nRespond with the single character "." and nothing else. (${counter})`;
}

/** True when a prompt the shim is about to submit is one of its own keep-alives. */
export function isKeepalivePrompt(text: string): boolean {
  return text.startsWith(KEEPALIVE_PROMPT_MARKER);
}

/** What a rewind needs: the record to resume at, and why. */
export interface RewindObligation {
  /** The vendor record uuid the next query resumes THROUGH, inclusive. */
  readonly resumeSessionAt: string;
  /** The turn whose assistant record that uuid is — the anchor's provenance. */
  readonly anchorTurnId: string;
  /** How many keep-alive turns are being discarded. */
  readonly discardedKeepaliveTurns: number;
}

/** The turn a record arrived under, as the rewind needs to see it. */
export interface RecordTurn {
  /** The turn's id, kept so the anchor can say which turn it came from. */
  readonly turnId: string;
  /** True when the shim opened this turn for its own keep-alive. */
  readonly keepalive: boolean;
}

/**
 * The yield obligation's bookkeeping: one remembered uuid and one counter.
 *
 * Deliberately not a history. The rewind needs exactly one anchor — the last
 * record written while a REAL turn was in flight — and the count of keep-alive
 * turns since, which is what makes the obligation reportable.
 */
export class KeepaliveRewind {
  private anchor: { readonly uuid: string; readonly turnId: string } | undefined;
  private keepaliveTurns = 0;

  /**
   * A message arrived under `turn`; it becomes the anchor only if it qualifies.
   *
   * IT TAKES THE MESSAGE, NOT A UUID, on purpose: the one rule that matters —
   * only an `assistant` message of a real, open turn may anchor a rewind — is
   * then enforced here rather than trusted to every call site.
   */
  noteRecord(message: SdkMessage, turn: RecordTurn | undefined): void {
    // Only an assistant record. `system:init`, `result`, the user echo, stream
    // events, hooks and control messages all carry uuids the vendor will not
    // resume at.
    if (message.type !== "assistant") return;
    // Only a REAL turn, and only while one is open. A keep-alive's own answer
    // is exactly the material the rewind exists to discard, and a record with
    // no open turn belongs to no turn this shim asked for.
    if (turn === undefined || turn.keepalive) return;
    const uuid = (message as { uuid?: unknown }).uuid;
    if (typeof uuid !== "string" || uuid === "") return;
    this.anchor = { uuid, turnId: turn.turnId };
  }

  /**
   * A boundary the anchor cannot be trusted across: forget it.
   *
   * INFO, not debug: the next real prompt after keep-alives will then carry
   * them rather than rewind, and an operator reading the feed has to be able to
   * see why. Silent when there was no anchor — nothing happened.
   */
  clearAnchor(reason: string): void {
    const held = this.anchor;
    if (held === undefined) return;
    this.anchor = undefined;
    LOGGER.info(
      { reason, anchor_uuid: held.uuid, anchor_turn_id: held.turnId },
      "the keep-alive rewind anchor is CLEARED: a uuid from before this boundary may not be resumable",
    );
  }

  /** The anchor as it stands, for a caller that needs to name it. */
  anchorUuid(): string | undefined {
    return this.anchor?.uuid;
  }

  /** A keep-alive turn ended; the context now carries material a real prompt must not see. */
  noteKeepaliveTurn(): void {
    this.keepaliveTurns++;
  }

  /**
   * What the next REAL prompt owes, or absence when it owes nothing.
   *
   * Absence covers both ordinary cases: no keep-alive has run since the last
   * real record, or no real record exists yet (a first prompt on a fresh
   * session has nothing to roll back TO, and truncating to nothing would
   * discard the session's own opening).
   */
  obligation(): RewindObligation | undefined {
    if (this.keepaliveTurns === 0) return undefined;
    if (this.anchor === undefined) {
      // INFO: keep-alive material is about to be CARRIED into a real turn,
      // which is the thing the rewind exists to prevent. It is the right
      // answer -- there is nothing safe to resume at -- but it is never
      // invisible.
      LOGGER.info(
        { keepalive_turns: this.keepaliveTurns },
        "no rewind anchor exists (none taken yet, or cleared at a boundary): the next real prompt proceeds WITHOUT a rewind and carries the keep-alive turns",
      );
      return undefined;
    }
    return {
      resumeSessionAt: this.anchor.uuid,
      anchorTurnId: this.anchor.turnId,
      discardedKeepaliveTurns: this.keepaliveTurns,
    };
  }

  /** The rewind happened (or was found unnecessary); the debt is cleared. */
  settled(): void {
    this.keepaliveTurns = 0;
  }
}

/**
 * What one vendor message is, relative to the keep-alive turn scope.
 *
 * `keepalive` is THE TAG: true when the keep-alive turn produced the message,
 * so every row and every push it gives rise to must stay off every served
 * plane. `endsKeepalive` is true on exactly one message per keep-alive: the
 * `result` that answers the keep-alive's own send.
 */
export interface KeepaliveAttribution {
  readonly keepalive: boolean;
  readonly endsKeepalive: boolean;
}

/** The one keep-alive send that may be outstanding. */
interface PendingKeepalive {
  readonly uuid: string;
  readonly turnId: string;
}

/** The attribution of every message the keep-alive did not produce. */
const NOT_KEEPALIVE: KeepaliveAttribution = { keepalive: false, endsKeepalive: false };

/**
 * THE KEEP-ALIVE TURN SCOPE: which vendor messages the keep-alive produced.
 *
 * WHY THE SHIM'S OWN "OPEN TURN" IS NOT THE ANSWER (the leak of 2026-09-23).
 * The shim used to tag every message that arrived while its keep-alive turn
 * was open, and to close that turn on the next `result`. But the vendor runs
 * turns no send asked for — a background task's notification starts one — and
 * those end in a `result` exactly as a send's turn does. On the owner's
 * workspace the keep-alive was pushed, the vendor first ran a
 * task-notification turn, and ITS result closed the keep-alive: that turn's
 * rows were tagged keep-alive (the store refused them as an identity change),
 * and the keep-alive's real answer — `.` — arrived after the close, untagged,
 * and was drawn as a green final answer.
 *
 * SO THE VENDOR ATTRIBUTES, NOT THE ARRIVAL ORDER. Every keep-alive send
 * carries a client uuid the shim mints ({@link begin}), registered in the
 * session's one send ledger like every other send (engine/sends.ts, ruled
 * 2026-09-28), and the vendor echoes it on the replies to that send. A vendor
 * turn is the keep-alive's from its first frame naming the keep-alive's uuid
 * until its `result`; a frame naming some other send, or a turn whose first
 * reply names none, is not the keep-alive's. Frames that carry no stamp — a
 * turn's later blocks, its tool traffic, its subagents — belong to whichever
 * vendor turn is running. The LEDGER reads the echo and says which turn that
 * is; this scope only asks whether the answer is the keep-alive
 * ({@link attribute} takes the ledger's verdict, never the stamps).
 *
 * NEVER FROM THE TEXT. A reply is never classified by what it says — the
 * model's "." is evidence of nothing — only by the send it answers.
 *
 * WORK IS THE KEEP-ALIVE'S ONLY WHEN THE KEEP-ALIVE STARTED IT. A frame that
 * names the work it belongs to — a subagent's frame (`parent_tool_use_id`), a
 * tool's progress, a task's lifecycle — belongs to whoever opened that work,
 * never to the vendor turn running when it arrives. A BACKGROUNDED subagent
 * keeps streaming across the turns after the one that launched it, and its
 * frames used to inherit the running turn: on 2026-09-15 and 2026-09-23 three
 * of them landed during a keep-alive turn and were stored as keep-alive rows
 * under real subagent upsert_keys, and the sidecar's page line for each was
 * later refused as an identity change, parking the subagent's whole
 * transcript. So the scope remembers the tool_use ids the keep-alive's own
 * frames opened ({@link spawns}), and only work under one of those is tagged.
 *
 * WHAT IT DOES NOT CLAIM. The frames a vendor turn emits BEFORE its first
 * reply — its `init`, a `UserPromptSubmit` hook, a status line — carry no
 * stamp, and there a task-notification turn's preamble is indistinguishable
 * from the keep-alive's. They stay untagged: `init` folds only into session
 * facts the engine states itself, and a succeeded hook draws nothing. Tagging
 * every unstamped message while a keep-alive is pending is exactly the defect
 * above, turned around.
 *
 * ONE SEND AT A TIME, CONSTANT SIZE. The keep-alive is only ever submitted into
 * an idle session, and a real prompt that arrives while it is pending waits
 * inside the shim for it to leave the turn slot (engine/turn.ts,
 * `waitOutKeepalive`), so at most one keep-alive uuid is ever outstanding and
 * no real send is ever pushed while one is.
 */
export class KeepaliveScope {
  private send: PendingKeepalive | undefined;
  /** Whether the vendor turn now running is the pending keep-alive's own, as the ledger stated it. */
  private running = false;
  /**
   * The tool_use ids the keep-alive's own frames opened, nested subagents'
   * included: the only work whose frames are the keep-alive's. Emptied when the
   * scope closes, so it is bounded by one keep-alive turn's calls.
   */
  private readonly spawns = new Set<string>();
  /** The vendor task ids a `task_started` bound to one of {@link spawns}. */
  private readonly spawnedTasks = new Set<string>();

  /** A keep-alive send is about to be pushed under `uuid`. */
  begin(uuid: string, turnId: string): void {
    if (this.send !== undefined) {
      // The cadence never beats into an open turn, so a second keep-alive
      // while one is pending means the one-submitter bookkeeping broke.
      throw new Error(
        `keep-alive scope: keep-alive ${turnId} began while ${this.send.turnId} was still unanswered`,
      );
    }
    this.send = { uuid, turnId };
    LOGGER.debug({ turn: turnId, client_uuid: uuid }, "a keep-alive send opened its turn scope");
  }

  /** The pending keep-alive's client uuid, so a re-delivery carries the same one. */
  pendingUuid(): string | undefined {
    return this.send?.uuid;
  }

  /**
   * The keep-alive will never be answered (its submission failed, or the query
   * it was pushed onto is gone): close the scope without an answer.
   */
  abandon(reason: string): void {
    const held = this.send;
    this.send = undefined;
    this.running = false;
    this.forgetSpawns();
    if (held === undefined) return;
    LOGGER.info(
      { turn: held.turnId, client_uuid: held.uuid, reason },
      "a keep-alive's turn scope closed WITHOUT its answer",
    );
  }

  /**
   * A new query is bound: it is running no vendor turn yet. A pending
   * keep-alive stays pending — the rewind's recovery re-delivers the same send
   * onto the new query, uuid and all.
   */
  queryBound(): void {
    this.running = false;
  }

  /** Whether the vendor turn now running is the keep-alive's. For callbacks between messages. */
  producing(): boolean {
    return this.running;
  }

  /**
   * Whether the work opened by `toolUseId` is the keep-alive's: the subagent
   * form of {@link producing}, for a callback raised inside that work.
   */
  spawned(toolUseId: string): boolean {
    return this.spawns.has(toolUseId);
  }

  /**
   * Tag one vendor message, under the send ledger's verdict for it. Called
   * ONCE per message, before anything else reads it, so every consumer sees
   * the same answer.
   */
  attribute(message: SdkMessage, verdict: SendVerdict): KeepaliveAttribution {
    const held = this.send;
    const turn = verdict.turn;
    const ownTurn = held !== undefined && turn.kind === "send" && turn.send.uuid === held.uuid;
    if (ownTurn !== this.running && !verdict.ended) {
      LOGGER.debug(
        { keepalive_pending: held !== undefined, attributed: ownTurn ? "keepalive" : "other" },
        "the running vendor turn's keep-alive attribution changed",
      );
    }
    if (message.type !== "result") {
      this.running = ownTurn;
      const work = workOf(message);
      const keepalive = work === undefined ? ownTurn : this.ownsWork(work);
      if (keepalive) this.noteSpawns(message);
      return keepalive ? { keepalive: true, endsKeepalive: false } : NOT_KEEPALIVE;
    }
    // A RESULT ENDS THE VENDOR TURN, whoever's it was.
    this.running = false;
    if (!ownTurn) {
      if (held !== undefined) {
        LOGGER.info(
          { keepalive_turn: held.turnId },
          "a vendor turn the keep-alive did not start ended; the keep-alive's own turn stays open",
        );
      }
      return NOT_KEEPALIVE;
    }
    this.send = undefined;
    this.forgetSpawns();
    LOGGER.debug(
      { turn: held.turnId, client_uuid: held.uuid },
      "the keep-alive's own result closed its turn scope",
    );
    return { keepalive: true, endsKeepalive: true };
  }

  /** Whether the named work was opened by the keep-alive. */
  private ownsWork(work: WorkRef): boolean {
    return work.kind === "tool_use" ? this.spawns.has(work.id) : this.spawnedTasks.has(work.id);
  }

  /** Remember the work a keep-alive-tagged message opens. */
  private noteSpawns(message: SdkMessage): void {
    if (message.type === "assistant") {
      const content = (message.message as { content?: unknown }).content;
      if (!Array.isArray(content)) return;
      for (const block of content as readonly { type?: unknown; id?: unknown }[]) {
        if (block.type === "tool_use" && typeof block.id === "string" && block.id !== "") this.spawns.add(block.id);
      }
      return;
    }
    if (message.type === "system" && message.subtype === "task_started") this.spawnedTasks.add(message.task_id);
  }

  /** The scope closed: nothing it opened can still be running as its own. */
  private forgetSpawns(): void {
    this.spawns.clear();
    this.spawnedTasks.clear();
  }
}

/** The work a frame names itself part of: the call that opened it, or the vendor task. */
type WorkRef = { readonly kind: "tool_use"; readonly id: string } | { readonly kind: "task"; readonly id: string };

/**
 * The work one message names as its own, or absence for a frame that names
 * none (it then belongs to the vendor turn running).
 *
 * Read off the SDK's DECLARED fields: `parent_tool_use_id` on a subagent's
 * assistant, stream, user and tool-progress frames; `tool_use_id` on a task's
 * start, progress and notification; the bare `task_id` on a task update, which
 * names no call.
 */
function workOf(message: SdkMessage): WorkRef | undefined {
  switch (message.type) {
    case "assistant":
    case "stream_event":
    case "user":
    case "tool_progress":
      return message.parent_tool_use_id === null ? undefined : { kind: "tool_use", id: message.parent_tool_use_id };
    case "system":
      switch (message.subtype) {
        case "task_started":
        case "task_progress":
        case "task_notification":
          return message.tool_use_id === undefined || message.tool_use_id === ""
            ? { kind: "task", id: message.task_id }
            : { kind: "tool_use", id: message.tool_use_id };
        case "task_updated":
          return { kind: "task", id: message.task_id };
        default:
          return undefined;
      }
    default:
      return undefined;
  }
}

/** How the cadence schedules itself; injected so a suite never waits on a clock. */
export interface KeepaliveScheduler {
  setInterval(handler: () => void, ms: number): unknown;
  clearInterval(handle: unknown): void;
}

/** The real one. */
export const REAL_SCHEDULER: KeepaliveScheduler = {
  setInterval: (handler, ms) => {
    const handle = setInterval(handler, ms);
    // A keep-alive must never be the reason a process stays alive: the listener
    // holds the shim open, and an unref'd timer keeps `--version`-style short
    // lives short.
    if (typeof (handle as { unref?: () => void }).unref === "function") {
      (handle as { unref: () => void }).unref();
    }
    return handle;
  },
  clearInterval: (handle) => clearInterval(handle as ReturnType<typeof setInterval>),
};

/**
 * The cadence.
 *
 * STARTED BEFORE `StartSession` RETURNS SUCCESS, so a session that is never
 * prompted still keeps its cache warm. PAUSED while a turn is in flight,
 * because a keep-alive submitted into an open turn would be a second submitter
 * — the one thing the one-submitter invariant forbids. STOPPED at kill.
 */
export class KeepaliveCadence {
  private handle: unknown;
  private paused = false;

  constructor(
    private readonly beat: () => void,
    private readonly intervalMs: number = KEEPALIVE_INTERVAL_MS,
    private readonly scheduler: KeepaliveScheduler = REAL_SCHEDULER,
  ) {}

  /** Begin beating. Idempotent: a second start does not double the cadence. */
  start(): void {
    if (this.handle !== undefined) return;
    this.handle = this.scheduler.setInterval(() => {
      if (this.paused) {
        LOGGER.logVerbose({ outcome: "skipped_turn_in_flight" }, "keep-alive beat skipped: a turn is in flight");
        return;
      }
      this.beat();
    }, this.intervalMs);
    LOGGER.debug({ interval_ms: this.intervalMs }, "keep-alive cadence started");
  }

  /** A turn is in flight; hold the beat. */
  pause(): void {
    this.paused = true;
  }

  /** The turn ended; beat again. */
  resume(): void {
    this.paused = false;
  }

  /** The session is ending. */
  stop(): void {
    if (this.handle === undefined) return;
    this.scheduler.clearInterval(this.handle);
    this.handle = undefined;
    LOGGER.debug({}, "keep-alive cadence stopped");
  }
}
