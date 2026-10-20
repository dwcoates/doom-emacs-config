/**
 * engine/sends.ts — which of the shim's sends each vendor turn answers.
 *
 * RULED 2026-09-28: A REPLY IS MATCHED TO THE SEND THAT CAUSED IT BY ID, NEVER
 * BY ARRIVAL ORDER. Every prompt the shim pushes into the vendor — a daemon's
 * StartTurn, the network-resume prompt, a re-delivery after a refused rewind,
 * the shim's own keep-alive — carries a client `uuid` the shim mints and
 * registers here BEFORE the push ({@link SendLedger.sent}). The vendor echoes
 * it (sdk.d.ts `user_message_uuid` / `user_message_uuids`) on the first reply
 * frames and the `result` of the turn that answers that send, and this ledger
 * attributes each vendor turn to the send whose uuid the echo names.
 *
 * WHY ARRIVAL ORDER WAS WRONG. The vendor runs turns no send asked for — a
 * background subagent's hand-back, a task notification — and it can start one
 * the instant before a send of the shim's lands in its queue. Matching by
 * arrival then charged the vendor's own turn to the send, closed the send's
 * turn on the vendor turn's result, and adopted the send's real answer as a
 * turn nobody started. The keep-alive paid for the same mistake first
 * (2026-09-23), and its stamp-and-echo was the fix this generalizes.
 *
 * THE RULES, one per shape of frame:
 *   - A frame that NAMES a send of ours (open, or retired within
 *     {@link RETIRED_SENDS_REMEMBERED}) makes the running vendor turn that
 *     send's. `user_message_uuid` is the send the frame answers; the list is
 *     the complete set the turn consumed, and the SDK's own binding rule is
 *     that a sender finds its uuid ANYWHERE in it — so when the single field
 *     is not ours, the last of ours in the list is the send answered. Any
 *     OTHER open send of ours the list names was consumed by this turn too,
 *     and is reported ABSORBED.
 *   - A uuid we never sent is an INVARIANT VIOLATION: it is recorded at ERROR
 *     every time a frame names it, and a frame naming nothing but such uuids
 *     is attributed to nothing. The ledger never guesses which send an
 *     unknown uuid "must" have meant.
 *   - An UNSTAMPED first reply (or a result with no reply ahead of it) opens a
 *     VENDOR-STARTED turn: the SDK stamps the first reply of every turn a
 *     stamped send started, so a turn whose first reply names nothing answers
 *     no send of ours. The session adopts it (engine/session.ts).
 *   - Unstamped frames AFTER a turn's first reply — later blocks, tool
 *     traffic, subagents — belong to the vendor turn already running: the SDK
 *     stamps once per frame kind per send.
 *   - A vendor-started turn that later NAMES a send of ours FOLDED that send
 *     in (sdk.d.ts: "the fold takes the echo over"). From that frame on the
 *     turn answers the send, and the vendor-started turn is reported ABSORBED
 *     so the session can conclude it.
 *   - A `result` ends the running vendor turn, whoever's it was.
 *
 * WHAT IT DOES NOT CLAIM. A turn's PREAMBLE — `init`, a `UserPromptSubmit`
 * hook, a status line — and detached work arriving between turns carry no
 * stamp and precede any reply, so no id speaks to them. The verdict for such a
 * frame is `unstated`, and the session charges it by its own slot rather than
 * by any send: that is not a reply's attribution.
 *
 * A PROMPT BATCH RUNS UNDER ITS FIRST MEMBER'S TURN. Prompts sent to join a
 * running turn that ended before folding them are still queued in the vendor,
 * and the vendor runs every queued prompt as ONE turn whose echo answers the
 * batch's LAST member (sdk.d.ts `user_message_uuids`). The session opens the
 * FIRST of them as the turn that runs and states the rest as its batch
 * ({@link SendLedger.batched}); a frame answering a member is then attributed
 * to the batch's first send while that send is named beside it, and every
 * other member the frame names is ABSORBED, as a fold is.
 *
 * A PEER SEND IS MATCHED BY ITS LIFECYCLE, NOT BY AN ECHO. An agent's prompt
 * delivered as another session's message (engine/peer-send.ts) is stamped
 * like every send, but the vendor echoes no `user_message_uuid` on the turn a
 * peer message drives. What names it is the vendor's `command_lifecycle`
 * frame, whose `command_uuid` is the uuid the shim stamped: `started` is the
 * moment the message drains into a turn. So for a send marked `peer`, and
 * only for one, that frame is a second matching path beside the echo:
 *   - between turns, `started` makes the running vendor turn that send's, and
 *     the unstamped replies that follow belong to it rather than opening a
 *     vendor-started turn;
 *   - while a vendor-started turn runs, `started` is a fold: the turn answers
 *     the peer send from there, and the vendor turn is ABSORBED, as an echo's
 *     fold is;
 *   - while another send's turn runs, `started` is the peer send folded INTO
 *     it: the peer send is ABSORBED, and the running turn goes on;
 *   - `started` for a peer send already RETIRED (one whose StartTurn was
 *     refused, which the vendor ran anyway) between turns opens a
 *     vendor-started turn, recorded at ERROR, since nothing of ours stands
 *     behind it any more.
 * Every other lifecycle state, and a lifecycle frame about any other send,
 * attributes nothing: an ordinary send is still matched by its echo alone.
 *
 * CONSTANT SIZE. The open sends are bounded by the session's own turn slots
 * (one daemon or network-resume turn, or one keep-alive) and the prompts
 * waiting to join it, enforced here as an invariant
 * ({@link OPEN_SENDS_BOUND}); the retired ring is bounded by
 * {@link RETIRED_SENDS_REMEMBERED}.
 */
import { bindLog } from "../log.js";
import type { SdkMessage } from "../sdk/types.js";
import { commandLifecycleOf, type CommandLifecycle } from "./peer-send.js";

const LOGGER = bindLog({ component: "shim-engine-sends", operation: "shim.engine.sends" });

/**
 * How many sends may be open at once before the ledger refuses another.
 *
 * The session's slots allow one open send (a daemon turn, a network-resume
 * turn or a keep-alive, each waiting for the last to be answered) and the
 * prompts sent to join it, which the daemon sends all at once when it flushes
 * its held queue. The bound is the vendor's own: an echo lists at most 64
 * sends a turn consumed (sdk.d.ts `user_message_uuids`), so a send past it
 * could never be attributed. It is a throw rather than an unbounded map.
 */
export const OPEN_SENDS_BOUND = 64;

/**
 * How many answered or forgotten sends are still recognized by their echo.
 *
 * A killed turn's stop result, or a re-delivered send's late echo, can name a
 * send after the session stopped holding it open. Those are still OUR sends,
 * and recognizing them keeps them from reading as an unknown uuid.
 */
export const RETIRED_SENDS_REMEMBERED = 16;

/** One send the shim pushed into the vendor, under the uuid the vendor will echo. */
export interface Send {
  /** The client uuid the send carried. */
  readonly uuid: string;
  /** The shim-side turn the send opened (a daemon turn id, a network-resume or keep-alive id). */
  readonly turnId: string;
  /** True for the shim's own keep-alive. */
  readonly keepalive: boolean;
  /**
   * True for a send delivered as another session's message: its turn is
   * matched by the vendor's `command_lifecycle`, which names it, since the
   * vendor echoes nothing for it (see the file comment).
   */
  readonly peer?: true;
}

/** The vendor turn a message belongs to, as the echoes have stated it. */
export type VendorTurn =
  /** No reply has said which turn is running: a preamble, or a frame between turns. */
  | { readonly kind: "unstated" }
  /** The turn answers this send of ours (possibly one already retired). */
  | { readonly kind: "send"; readonly send: Send; readonly retired: boolean }
  /** The vendor started this turn on its own: its first reply named no send. */
  | { readonly kind: "vendor" }
  /** The turn names only uuids the shim never sent. Attributed to nothing. */
  | { readonly kind: "unknown"; readonly uuids: readonly string[] };

/** A turn a message's echo took the running vendor turn away from. */
export type AbsorbedTurn =
  | { readonly kind: "vendor" }
  | { readonly kind: "send"; readonly send: Send };

/** What one vendor message is, relative to the shim's sends. */
export interface SendVerdict {
  /** The vendor turn this message belongs to. For a `result`, the turn it ends. */
  readonly turn: VendorTurn;
  /** This message opened a vendor-started turn. */
  readonly openedVendorTurn: boolean;
  /**
   * The turns this message's echo moved the running vendor turn away from,
   * or folded into it: each is concluded, because its own result will never
   * come.
   */
  readonly absorbed: readonly AbsorbedTurn[];
  /** True exactly when the message is a `result`: the vendor turn in {@link turn} ended. */
  readonly ended: boolean;
}

const UNSTATED: VendorTurn = { kind: "unstated" };

/**
 * The client uuids a vendor frame names, the one it ANSWERS first; absence when
 * it names none.
 *
 * THE SDK'S OWN ATTRIBUTION (sdk.d.ts): `user_message_uuid` is the send the
 * frame answers, stamped on its turn's first top-level assistant message, its
 * first non-ping stream event, every `thinking_tokens` frame and its `result`,
 * and moved onto a folded-in send when a fold takes the echo over.
 * `user_message_uuids` is the complete list the turn consumed; the single
 * field is always in it. An older producer states only the list's absence, so
 * a list with no single field is read as answering its LAST member, which is
 * what the SDK says the single field names.
 *
 * Read off the SDK's DECLARED fields, never an index signature, so an SDK that
 * stops declaring them is a type error here rather than an attribution that
 * silently stops working.
 */
export function stampedSends(message: SdkMessage): { readonly answers: string; readonly all: readonly string[] } | undefined {
  let stamps: { user_message_uuid?: string; user_message_uuids?: string[] } | undefined;
  if (message.type === "assistant" || message.type === "stream_event") {
    // Subagent frames are never stamped; they belong to whatever turn is running.
    if (message.parent_tool_use_id !== null) return undefined;
    stamps = message;
  } else if (message.type === "result") {
    stamps = message;
  } else if (message.type === "system" && message.subtype === "thinking_tokens") {
    stamps = message;
  }
  if (stamps === undefined) return undefined;
  const list = Array.isArray(stamps.user_message_uuids)
    ? stamps.user_message_uuids.filter((uuid) => typeof uuid === "string" && uuid !== "")
    : [];
  const single = typeof stamps.user_message_uuid === "string" && stamps.user_message_uuid !== ""
    ? stamps.user_message_uuid
    : undefined;
  const answers = single ?? list.at(-1);
  if (answers === undefined) return undefined;
  return { answers, all: list.includes(answers) ? list : [...list, answers] };
}

/** A frame that is a turn's REPLY rather than its preamble: the vendor stamps the first of these. */
export function isTopLevelReply(message: SdkMessage): boolean {
  if (message.type === "assistant") return message.parent_tool_use_id === null;
  // Every declared stream event is a reply frame: the SDK's event union has no
  // `ping` arm, which is the one kind the vendor leaves unstamped.
  if (message.type === "stream_event") return message.parent_tool_use_id === null;
  return message.type === "system" && message.subtype === "thinking_tokens";
}

/**
 * THE LEDGER: the shim's open sends, and the vendor turn now running.
 *
 * ONE CALL PER MESSAGE ({@link attribute}), in the order the vendor emits them,
 * before anything else reads the message, so every consumer sees one answer.
 */
/**
 * Told when the vendor's first top-level reply to one of the shim's prompts
 * arrives: the send, and the seconds since it was registered on the ledger's
 * monotonic clock. The shim's keep-alive sends are never reported.
 */
export type FirstReply = (send: Send, seconds: number) => void;

export class SendLedger {
  private readonly open = new Map<string, Send>();
  /** When each prompt still awaiting its first reply was registered, monotonic ms. */
  private readonly awaitingFirstReply = new Map<string, number>();
  /** Answered or forgotten sends, oldest first, bounded by {@link RETIRED_SENDS_REMEMBERED}. */
  private readonly retired = new Map<string, Send>();
  /** A batch member's uuid -> the uuid of the batch's first send ({@link batched}). */
  private readonly batchedUnder = new Map<string, string>();
  private running: VendorTurn = UNSTATED;

  /**
   * `firstReply` hears each prompt's first reply (engine/measurements.ts);
   * `monoMs` is the monotonic clock it is timed on.
   */
  constructor(
    private readonly firstReply: FirstReply = () => undefined,
    private readonly monoMs: () => number = () => performance.now(),
  ) {}

  /**
   * A send is about to be pushed. Registered BEFORE the push, so the vendor
   * cannot answer a send the ledger does not yet know.
   *
   * A uuid registered twice, or more open sends than {@link OPEN_SENDS_BOUND},
   * is a defect in the caller's bookkeeping, and throws.
   */
  sent(send: Send): void {
    if (this.open.has(send.uuid)) {
      throw new Error(`send ledger: send ${send.uuid} (turn ${send.turnId}) was registered twice`);
    }
    if (this.open.size >= OPEN_SENDS_BOUND) {
      throw new Error(
        `send ledger: ${this.open.size} sends are already open; turn ${send.turnId} cannot open another ` +
          `(open: ${[...this.open.values()].map((open) => open.turnId).join(", ")})`,
      );
    }
    this.retired.delete(send.uuid);
    this.open.set(send.uuid, send);
    if (!send.keepalive) this.awaitingFirstReply.set(send.uuid, this.monoMs());
    LOGGER.debug(
      { turn_id: send.turnId, client_uuid: send.uuid, keepalive: send.keepalive, open_sends: this.open.size },
      "a send was registered under the client uuid the vendor will echo",
    );
  }

  /**
   * The session stopped holding the send's turn open for a reason other than
   * its own result — a kill, a refused push, the query's death, a teardown.
   * The send is RETIRED, not dropped: an echo that still names it (a stop
   * result) is recognized as ours.
   */
  forget(turnId: string, reason: string): void {
    for (const send of [...this.open.values()]) {
      if (send.turnId !== turnId) continue;
      this.open.delete(send.uuid);
      this.retire(send);
      LOGGER.debug(
        { turn_id: send.turnId, client_uuid: send.uuid, reason },
        "a send was retired without its answer; a late echo of it is still recognized",
      );
    }
  }

  /**
   * The open sends of `members` are queued in the vendor behind the open send
   * of `firstTurnId`, and the vendor runs them all as one prompt batch: a
   * frame answering a member is the first send's turn while the first send is
   * named beside it (see the file comment). A member re-batched under a later
   * first send is moved there.
   *
   * A turn with no open send is a defect in the caller's bookkeeping, and
   * throws.
   */
  batched(firstTurnId: string, members: readonly string[]): void {
    const first = this.openSendOf(firstTurnId);
    if (first === undefined) {
      throw new Error(`send ledger: turn ${firstTurnId} heads a prompt batch with no open send`);
    }
    for (const member of members) {
      const send = this.openSendOf(member);
      if (send === undefined) {
        throw new Error(`send ledger: turn ${member} is batched behind ${firstTurnId} with no open send`);
      }
      this.batchedUnder.set(send.uuid, first.uuid);
    }
    LOGGER.debug(
      { first_turn_id: firstTurnId, members: members.join(","), client_uuid: first.uuid },
      "prompts queued behind a send run as one prompt batch under that send's turn",
    );
  }

  /** The open send of `turnId`, if the ledger holds one. */
  openSendOf(turnId: string): Send | undefined {
    for (const send of this.open.values()) if (send.turnId === turnId) return send;
    return undefined;
  }

  /** A new query is bound: it is running no vendor turn yet. Open sends stay open. */
  queryBound(): void {
    this.running = UNSTATED;
  }

  /** The vendor turn now running, for a caller between messages. */
  current(): VendorTurn {
    return this.running;
  }

  /** Attribute one vendor message. Called ONCE per message, in arrival order. */
  attribute(message: SdkMessage): SendVerdict {
    const lifecycle = commandLifecycleOf(message);
    if (lifecycle !== undefined) return this.lifecycle(lifecycle);
    const stamps = stampedSends(message);
    const absorbed: AbsorbedTurn[] = [];
    let openedVendorTurn = false;
    if (stamps !== undefined) {
      const next = this.stated(stamps.answers, stamps.all);
      const answered = next.kind === "send" ? next.send.uuid : undefined;
      if (next.kind === "send" && this.running.kind === "vendor") {
        // THE FOLD. A turn the vendor started on its own took one of our sends
        // in, and its echo moved onto that send: from here the turn answers it.
        absorbed.push({ kind: "vendor" });
        LOGGER.info(
          { turn_id: next.send.turnId, client_uuid: next.send.uuid },
          "a vendor-started turn folded one of the shim's sends in; the turn answers that send from here",
        );
      }
      if (
        next.kind === "send" &&
        this.running.kind === "send" &&
        this.running.send.uuid !== next.send.uuid &&
        !this.running.retired &&
        this.open.has(this.running.send.uuid)
      ) {
        absorbed.push({ kind: "send", send: this.running.send });
        LOGGER.info(
          { turn_id: next.send.turnId, absorbed_turn_id: this.running.send.turnId },
          "the vendor turn's echo moved from one of the shim's sends onto another; the first is concluded",
        );
      }
      // EVERY OTHER OPEN SEND THE TURN NAMES WAS CONSUMED BY IT (a batch the
      // vendor merged, a send folded in between tool rounds): its own turn
      // will never come, so it is concluded with this one.
      for (const uuid of stamps.all) {
        if (uuid === answered) continue;
        const consumed = this.open.get(uuid);
        if (consumed === undefined) continue;
        if (absorbed.some((turn) => turn.kind === "send" && turn.send.uuid === uuid)) continue;
        absorbed.push({ kind: "send", send: consumed });
      }
      for (const turn of absorbed) {
        if (turn.kind !== "send") continue;
        this.open.delete(turn.send.uuid);
        this.retire(turn.send);
      }
      if (!sameTurn(next, this.running)) {
        LOGGER.debug(
          { answers: stamps.answers, all: stamps.all.join(","), attributed: describe(next) },
          "a vendor frame named the send it answers; the running vendor turn is attributed",
        );
      }
      this.running = next;
      if (next.kind === "send" && isTopLevelReply(message)) this.repliedTo(next.send);
    } else if (this.running.kind === "send" && this.running.send.peer === true && isTopLevelReply(message)) {
      // A PEER SEND'S REPLIES ARE NEVER STAMPED; its first is timed here.
      this.repliedTo(this.running.send);
    } else if (this.running.kind === "unstated" && (isTopLevelReply(message) || message.type === "result")) {
      // THE FIRST REPLY OF A TURN A STAMPED SEND STARTED IS ALWAYS STAMPED
      // (sdk.d.ts). One that names nothing opens a turn no send of ours
      // started — and so does a result with no reply ahead of it.
      this.running = { kind: "vendor" };
      openedVendorTurn = true;
      LOGGER.debug(
        { first_message: message.type, open_sends: this.open.size },
        "a vendor turn's first reply named no send; the vendor started it on its own",
      );
    }
    const turn = this.running;
    if (message.type !== "result") return { turn, openedVendorTurn, absorbed, ended: false };
    // A RESULT ENDS THE VENDOR TURN, whoever's it was.
    this.running = UNSTATED;
    if (turn.kind === "send" && this.open.delete(turn.send.uuid)) this.retire(turn.send);
    return { turn, openedVendorTurn, absorbed, ended: true };
  }

  /**
   * A `command_lifecycle` frame: the second matching path, for peer sends
   * only (see the file comment).
   */
  private lifecycle(lifecycle: CommandLifecycle): SendVerdict {
    const unchanged: SendVerdict = { turn: this.running, openedVendorTurn: false, absorbed: [], ended: false };
    if (lifecycle.state !== "started") return unchanged;
    const open = this.open.get(lifecycle.commandUuid);
    if (open === undefined) {
      const retired = this.retired.get(lifecycle.commandUuid);
      if (retired?.peer !== true) return unchanged;
      return this.retiredPeerStarted(retired);
    }
    if (open.peer !== true) return unchanged;
    switch (this.running.kind) {
      case "send": {
        if (this.running.send.uuid === open.uuid) return unchanged;
        // FOLDED INTO THE RUNNING TURN: the peer send is consumed by it, and
        // its own turn will never come.
        this.open.delete(open.uuid);
        this.retire(open);
        LOGGER.info(
          { turn_id: open.turnId, running_turn_id: this.running.send.turnId, client_uuid: open.uuid },
          "the vendor folded a peer send into the running turn; the peer send is concluded",
        );
        return { ...unchanged, absorbed: [{ kind: "send", send: open }] };
      }
      case "vendor": {
        this.running = { kind: "send", send: open, retired: false };
        LOGGER.info(
          { turn_id: open.turnId, client_uuid: open.uuid },
          "a vendor-started turn folded a peer send in; the turn answers that send from here",
        );
        return { turn: this.running, openedVendorTurn: false, absorbed: [{ kind: "vendor" }], ended: false };
      }
      case "unstated":
      case "unknown":
        this.running = { kind: "send", send: open, retired: false };
        LOGGER.debug(
          { turn_id: open.turnId, client_uuid: open.uuid },
          "the vendor started a peer send's turn; the running vendor turn is attributed by its lifecycle",
        );
        return { ...unchanged, turn: this.running };
    }
  }

  /**
   * The vendor started a peer send the shim already retired: its StartTurn
   * was refused (a hold, a lost admission), and the vendor ran it anyway.
   * Between turns it opens a turn no send of ours stands behind; while a turn
   * runs it is that turn's, which goes on.
   */
  private retiredPeerStarted(retired: Send): SendVerdict {
    LOGGER.error(
      {
        turn_id: retired.turnId,
        client_uuid: retired.uuid,
        running: describe(this.running),
        detail: "a refused peer send never reaches the agent; the vendor ran one anyway",
      },
      "the vendor started a peer send whose StartTurn was refused",
    );
    if (this.running.kind !== "unstated") {
      return { turn: this.running, openedVendorTurn: false, absorbed: [], ended: false };
    }
    this.running = { kind: "vendor" };
    return { turn: this.running, openedVendorTurn: true, absorbed: [], ended: false };
  }

  /**
   * The vendor turn an echo states, recording an unknown uuid at ERROR: the
   * single field when it is ours, else the last of ours in the list.
   */
  private stated(answers: string, all: readonly string[]): VendorTurn {
    const unknown = all.filter((uuid) => !this.open.has(uuid) && !this.retired.has(uuid));
    if (unknown.length > 0) {
      // AN INVARIANT VIOLATION, NEVER A GUESS: the vendor named a send this
      // process never registered. Nothing is attributed to any send of ours on
      // its word.
      LOGGER.error(
        {
          unknown_uuids: unknown.join(","),
          answers,
          open_sends: [...this.open.values()].map((send) => `${send.turnId}:${send.uuid}`).join(","),
          detail: "every send the shim pushes is registered before the push; an echo naming another uuid answers nothing the shim sent",
        },
        "a vendor frame echoed a client uuid the shim never sent; it is attributed to no send",
      );
    }
    const ours = all.filter((uuid) => this.open.has(uuid) || this.retired.has(uuid));
    const stated = ours.includes(answers) ? answers : ours.at(-1);
    if (stated === undefined) return { kind: "unknown", uuids: unknown };
    // A BATCH MEMBER ANSWERED BESIDE ITS FIRST SEND IS THE FIRST SEND'S TURN.
    const first = this.batchedUnder.get(stated);
    const chosen = first !== undefined && ours.includes(first) ? first : stated;
    const open = this.open.get(chosen);
    if (open !== undefined) return { kind: "send", send: open, retired: false };
    const retired = this.retired.get(chosen) as Send;
    return { kind: "send", send: retired, retired: true };
  }

  /** The first top-level reply to `send` arrived: it is timed, once. */
  private repliedTo(send: Send): void {
    const sentAt = this.awaitingFirstReply.get(send.uuid);
    if (sentAt === undefined) return;
    this.awaitingFirstReply.delete(send.uuid);
    this.firstReply(send, (this.monoMs() - sentAt) / 1000);
  }

  private retire(send: Send): void {
    // A send that ended without a reply is never timed.
    this.awaitingFirstReply.delete(send.uuid);
    this.retired.delete(send.uuid);
    this.retired.set(send.uuid, send);
    while (this.retired.size > RETIRED_SENDS_REMEMBERED) {
      const oldest = this.retired.keys().next();
      if (oldest.done === true) break;
      this.retired.delete(oldest.value);
      this.forgetBatch(oldest.value);
    }
  }

  /** A send no echo can name any more leaves every batch it was in. */
  private forgetBatch(uuid: string): void {
    this.batchedUnder.delete(uuid);
    for (const [member, first] of this.batchedUnder) if (first === uuid) this.batchedUnder.delete(member);
  }
}

function sameTurn(a: VendorTurn, b: VendorTurn): boolean {
  if (a.kind !== b.kind) return false;
  if (a.kind === "send" && b.kind === "send") return a.send.uuid === b.send.uuid;
  return true;
}

function describe(turn: VendorTurn): string {
  switch (turn.kind) {
    case "send":
      return `${turn.send.keepalive ? "keepalive" : "send"}:${turn.send.turnId}`;
    case "unknown":
      return "unknown";
    case "vendor":
      return "vendor";
    case "unstated":
      return "unstated";
  }
}
