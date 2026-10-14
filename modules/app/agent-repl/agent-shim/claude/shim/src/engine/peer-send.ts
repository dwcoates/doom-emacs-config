/**
 * engine/peer-send.ts — AN AGENT'S PROMPT, SENT AS ANOTHER SESSION'S MESSAGE.
 *
 * A prompt another workspace's agent sent (`StartTurnRequest.sent_by`, origin
 * PROMPT_ORIGIN_AGENT_SENT) is not words the user typed, and the vendor has
 * its own way to say so: a user message wrapped in its cross-session envelope
 * and stamped `origin.kind: "peer"`. The vendor then frames it for the model
 * as another session's message rather than the user's. This module is
 * everything the shim needs to WRITE such a message, which until now it only
 * ever read (convert/peer.ts, the sidecar's convert/user.go):
 *
 *   - the declaration: the address `agent-repl:<sender>`, the sender's name,
 *     and the SENDER's own permission class, honestly ({@link peerDeclarationOf});
 *   - the envelope the vendor recognizes ({@link peerEnvelope});
 *   - the vendor's two undeclared frames about such a message: its
 *     `command_lifecycle` (which names the message's own uuid, since a peer
 *     message's turn carries no `user_message_uuid` echo) and its
 *     `peer_message_hold`;
 *   - the admission wait ({@link PeerAdmissions}): a peer send is accepted
 *     only once the vendor has QUEUED it. A message the vendor HOLDS instead
 *     (its sender's class is not this session's, or this session's settings
 *     hold cross-session messages) would sit parked until the vendor drops it
 *     unread, so the StartTurn is refused loudly rather than left to stall.
 *
 * NEVER A LIE TO GET PAST THE HOLD. The shim never states this session's own
 * class in place of the sender's, and never sets `crossSessionInbound`: the
 * hold is the vendor's policy, and the honest answer to it is a refusal.
 *
 * The two frames are not in the SDK's published message union (the vendor's
 * `init` advertises `msg_lifecycle_v1` for the first), so they are read
 * loosely here, by their observed shape, and nowhere else.
 */
import type { SaidContent } from "./said-content.js";
import { conversationv1, type shimv1 } from "../proto.js";
import { bindLog } from "../log.js";
import { AGENT_REPL_PEER_PREFIX } from "../convert/peer.js";
import type { SdkMessage } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-peer-send", operation: "shim.engine.peer_send" });

/** The vendor's permission classes, as a host declares a sender's (sdk.d.ts `fromMode`). */
export type PeerPermissionClass = "bypass" | "prompting";

/** What the shim declares to the vendor about the session a peer message is from. */
export interface PeerDeclaration {
  /** The envelope's and the origin's `from`: `agent-repl:<name>`, or `agent-repl:<id>` unnamed. */
  readonly from: string;
  /** The sender's display name (`from-name`), absent when the daemon could not name it. */
  readonly fromName?: string;
  /** The SENDER's permission class, absent when the daemon holds none for it. */
  readonly fromMode?: PeerPermissionClass;
}

/**
 * The vendor's class of a permission mode: `bypass` is the class of a session
 * that runs tools without asking, and every other mode is `prompting`. The
 * mode-to-class rule is the vendor's (only `bypassPermissions` bypasses), so
 * it is stated here, in the vendor adapter, and the daemon sends the mode.
 */
export function permissionClassOf(mode: conversationv1.AgentPermissionMode): PeerPermissionClass {
  switch (mode.mode.case) {
    case "bypass":
      return "bypass";
    case "default":
    case "acceptEdits":
    case "plan":
    case "dontAsk":
    case "auto":
      return "prompting";
    case undefined:
      // validate/ refuses a stated mode with no arm at the wire.
      throw new Error("shim peer send: a sender's permission mode names no mode");
  }
}

/**
 * The address characters the vendor keeps in a `from` (an address-shaped
 * token). Every other character is percent-encoded, which the vendor's own
 * address alphabet admits, so a workspace name never loses a character to the
 * vendor's scrubbing.
 */
const ADDRESS_CHAR = /[A-Za-z0-9:_/.-]/;

/** `label`, percent-encoded into the vendor's address alphabet. */
export function peerAddressLabel(label: string): string {
  let out = "";
  for (const char of label) {
    if (ADDRESS_CHAR.test(char)) {
      out += char;
      continue;
    }
    for (const byte of new TextEncoder().encode(char)) out += `%${byte.toString(16).toUpperCase().padStart(2, "0")}`;
  }
  return out;
}

/** What the shim declares for SENTBY: the address, the name, the sender's own class. */
export function peerDeclarationOf(sentBy: shimv1.StartTurnSentBy): PeerDeclaration {
  const name = sentBy.workspaceName;
  const label = name !== undefined && name !== "" ? name : sentBy.workspaceId;
  return {
    from: `${AGENT_REPL_PEER_PREFIX}${peerAddressLabel(label)}`,
    ...(name !== undefined && name !== "" ? { fromName: name } : {}),
    ...(sentBy.permissionMode === undefined ? {} : { fromMode: permissionClassOf(sentBy.permissionMode) }),
  };
}

/** VALUE escaped for a double-quoted envelope attribute, which may hold no `"`, `<`, `>` or line break. */
function attributeValue(value: string): string {
  return value
    .replaceAll("&", "&amp;")
    .replaceAll('"', "&quot;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll(/[\r\n\u0085\u2028\u2029]/g, " ");
}

/**
 * TEXT in the vendor's cross-session envelope. The vendor treats a stdin
 * message as another session's only when it is exactly one such envelope, so
 * everything the agent is sent for the prompt (a vendor note included) is
 * inside it.
 */
export function peerEnvelope(text: string, peer: PeerDeclaration): string {
  const name = peer.fromName === undefined ? "" : ` from-name="${attributeValue(peer.fromName)}"`;
  return `<cross-session-message from="${attributeValue(peer.from)}"${name}>\n${text}\n</cross-session-message>`;
}

/**
 * A peer send's whole content, in the cross-session envelope.
 *
 * Words alone are {@link peerEnvelope}'s string. A prompt that carries images
 * is a block list (engine/said-content.ts), so the envelope's opening and
 * closing tags become its first and last text blocks and every block between
 * keeps its place -- an image an agent sends stays an image.
 */
export function peerContent(content: SaidContent, peer: PeerDeclaration): SaidContent {
  if (typeof content === "string") return peerEnvelope(content, peer);
  const [open, close] = peerEnvelope("\u0000", peer).split("\u0000");
  if (open === undefined || close === undefined) {
    throw new Error("shim peer send: the cross-session envelope did not split around its body");
  }
  return [{ type: "text", text: open.trimEnd() }, ...content, { type: "text", text: close.trimStart() }];
}

/** The fate of a queued command, as the vendor's `command_lifecycle` frame states it. */
export type CommandLifecycleState = "queued" | "started" | "completed" | "cancelled" | "discarded" | "refused";

const LIFECYCLE_STATES: ReadonlySet<string> = new Set<CommandLifecycleState>([
  "queued",
  "started",
  "completed",
  "cancelled",
  "discarded",
  "refused",
]);

/** A `command_lifecycle` frame's facts. */
export interface CommandLifecycle {
  /** The client uuid the command carried on its inbound message. */
  readonly commandUuid: string;
  readonly state: CommandLifecycleState;
}

/**
 * The facts of a `command_lifecycle` frame (`{type: "command_lifecycle",
 * command_uuid, state}`), or absence for any other message. A frame of that
 * type with no uuid or an unknown state is absence too, recorded at ERROR: it
 * attributes nothing, and nothing guesses what it meant.
 */
export function commandLifecycleOf(message: SdkMessage): CommandLifecycle | undefined {
  const raw = message as unknown as { type?: unknown; command_uuid?: unknown; state?: unknown };
  if (raw.type !== "command_lifecycle") return undefined;
  if (typeof raw.command_uuid !== "string" || raw.command_uuid === "" || typeof raw.state !== "string" || !LIFECYCLE_STATES.has(raw.state)) {
    LOGGER.error(
      {
        command_uuid: JSON.stringify(raw.command_uuid ?? null),
        state: JSON.stringify(raw.state ?? null),
        detail: "a lifecycle frame names the command it is about and one of the vendor's states",
      },
      "a command_lifecycle frame named no command or no known state; it attributes nothing",
    );
    return undefined;
  }
  return { commandUuid: raw.command_uuid, state: raw.state as CommandLifecycleState };
}

/** A `peer_message_hold` frame's facts. */
export interface PeerHold {
  readonly state: "held" | "released" | "dropped";
  /** The held message's uuid; for the stdin lane, the one the shim stamped. */
  readonly messageUuid: string | undefined;
  /** Why the vendor held it (`held` only). */
  readonly cause: string | undefined;
  /** How a drop ended (`dropped` only). */
  readonly outcome: string | undefined;
  readonly lane: string;
  readonly from: string;
}

/**
 * The facts of a `peer_message_hold` system frame, or absence for any other
 * message. Read loosely: the frame is not in the SDK's published union.
 */
export function peerHoldOf(message: SdkMessage): PeerHold | undefined {
  const raw = message as unknown as Record<string, unknown>;
  if (raw.type !== "system" || raw.subtype !== "peer_message_hold") return undefined;
  const state = raw.state === "held" || raw.state === "released" || raw.state === "dropped" ? raw.state : undefined;
  if (state === undefined) {
    LOGGER.error(
      { state: JSON.stringify(raw.state ?? null), detail: "a hold frame states held, released or dropped" },
      "a peer_message_hold frame named no known state; it is read as a hold",
    );
  }
  const text = (value: unknown): string | undefined => (typeof value === "string" && value !== "" ? value : undefined);
  return {
    state: state ?? "held",
    messageUuid: text(raw.message_uuid),
    cause: text(raw.cause),
    outcome: text(raw.outcome),
    lane: text(raw.lane) ?? "",
    from: text(raw.from) ?? "",
  };
}

/**
 * The vendor's account of a hold cause, for the refusal's detail. A cause the
 * vendor adds later is named as it spells it.
 */
function holdCauseSentence(cause: string | undefined): string {
  switch (cause) {
    case "mode-mismatch":
      return "the sending workspace's permission class is not this session's";
    case "no-mode-asserted":
      return "the sending workspace declared no permission class and this session bypasses permissions";
    case "mode-unknown":
      return "the vendor does not recognize this session's permission mode";
    case "explicit-setting":
    case "managed-setting":
    case "repo-setting":
    case "invalid-setting":
    case "bypass-default":
      return `this session's crossSessionInbound settings hold messages from other sessions (${cause})`;
    default:
      return `the vendor held it (${cause ?? "no cause stated"})`;
  }
}

/** A peer send the vendor did not queue: the StartTurn's refusal, with why. */
export class PeerNotAdmittedError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PeerNotAdmittedError";
  }
}

/**
 * How long a peer send waits for the vendor to say it queued or held it.
 *
 * A LAST RESORT, NOT THE MECHANISM: the wait ends on the vendor's own frame,
 * which it writes the moment it reads the message off its input. UNMEASURED
 * against the real vendor as of 2026-10-20: the admission latency is recorded
 * on every admitted send (`admission_ms`, DEBUG), so this bound can be sized
 * at just over twice the measured healthy latency. Until then it is ten
 * seconds, far past any input read, and well inside the daemon's own wait.
 */
export const PEER_ADMISSION_BUDGET_MS = 10_000;

interface Waiter {
  readonly turnId: string;
  readonly from: string;
  readonly sentAtMs: number;
  readonly resolve: () => void;
  readonly reject: (err: Error) => void;
  readonly timer: ReturnType<typeof setTimeout>;
}

/**
 * THE PEER SENDS AWAITING ADMISSION, by client uuid. Constant-size: one per
 * open peer send, which the send ledger bounds.
 */
export class PeerAdmissions {
  private readonly waiting = new Map<string, Waiter>();
  /** Peer sends the vendor answered, so a late or repeated frame about one is named, not ignored. */
  private readonly settled = new Map<string, { turnId: string; admitted: boolean }>();

  constructor(
    private readonly budgetMs: number = PEER_ADMISSION_BUDGET_MS,
    private readonly nowMs: () => number = () => performance.now(),
  ) {}

  /**
   * Wait for the vendor's word on the send `uuid` of `turnId`, registered
   * BEFORE its push so no frame about it can arrive unheard. Resolves once the
   * vendor queued it; rejects with a {@link PeerNotAdmittedError} when it held,
   * cancelled, discarded or refused it, when the budget ran out, or when the
   * query died first.
   */
  expect(uuid: string, turnId: string, from: string): Promise<void> {
    if (this.waiting.has(uuid)) {
      return Promise.reject(new Error(`shim peer send: send ${uuid} (turn ${turnId}) is already awaiting admission`));
    }
    return new Promise<void>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.fail(
          uuid,
          `the vendor said neither that it queued nor that it held turn ${turnId}'s message from ${from} within ${this.budgetMs}ms; nothing reached the agent`,
          "the vendor did not answer a peer send within its admission budget",
        );
      }, this.budgetMs);
      this.waiting.set(uuid, { turnId, from, sentAtMs: this.nowMs(), resolve, reject, timer });
    });
  }

  /** The push of `uuid` was refused: nothing is awaited for it any more. */
  withdraw(uuid: string): void {
    const waiter = this.waiting.get(uuid);
    if (waiter === undefined) return;
    clearTimeout(waiter.timer);
    this.waiting.delete(uuid);
  }

  /** The query died: every waiting send is refused, since no frame about it can come. */
  abandonAll(detail: string): void {
    for (const uuid of [...this.waiting.keys()]) {
      this.fail(uuid, `the vendor query ended before it queued the message: ${detail}`, "a peer send's query ended before the vendor queued it");
    }
  }

  /** Read one vendor message for what it says about a peer send. Called once per message. */
  observe(message: SdkMessage): void {
    const lifecycle = commandLifecycleOf(message);
    if (lifecycle !== undefined) {
      this.observeLifecycle(lifecycle);
      return;
    }
    const hold = peerHoldOf(message);
    if (hold !== undefined) this.observeHold(hold);
  }

  private observeLifecycle(lifecycle: CommandLifecycle): void {
    const waiter = this.waiting.get(lifecycle.commandUuid);
    if (waiter === undefined) return;
    switch (lifecycle.state) {
      case "queued":
      case "started":
      case "completed":
        this.admit(lifecycle.commandUuid, waiter, lifecycle.state);
        return;
      case "cancelled":
      case "discarded":
      case "refused":
        this.fail(
          lifecycle.commandUuid,
          `the vendor ${lifecycle.state} turn ${waiter.turnId}'s message from ${waiter.from} instead of queuing it; nothing reached the agent`,
          "the vendor did not queue a peer send",
        );
        return;
    }
  }

  private observeHold(hold: PeerHold): void {
    const uuid = hold.messageUuid;
    if (uuid === undefined || hold.lane !== "stdin") {
      // ANOTHER SESSION'S MESSAGE (the socket lane) or one the vendor could
      // not name: it is not a send of this shim's, and the uuid on the socket
      // lane is the SENDER's choice, never matched against ours.
      LOGGER.info(
        { state: hold.state, lane: hold.lane, from: hold.from, cause: hold.cause ?? "", outcome: hold.outcome ?? "" },
        "the vendor reported a peer message hold for a message this shim did not send",
      );
      return;
    }
    const waiter = this.waiting.get(uuid);
    if (waiter !== undefined && hold.state === "held") {
      this.fail(
        uuid,
        `the vendor HELD turn ${waiter.turnId}'s message from ${waiter.from} instead of running it: ${holdCauseSentence(hold.cause)}; ` +
          "a held message is dropped unread, so the prompt was not delivered",
        "the vendor held a peer send instead of queuing it; the StartTurn is refused",
        hold.cause,
      );
      return;
    }
    // A HOLD ABOUT A SEND ALREADY ANSWERED: a refused send the vendor later
    // released or dropped, or a re-delivered one it held. Nothing waits on it,
    // and a release runs a message the daemon was told failed, so it is said
    // at ERROR; the drop that ends a refused hold is the expected end of it.
    const settled = this.settled.get(uuid);
    const refusedThenDropped = settled !== undefined && !settled.admitted && hold.state === "dropped";
    const fields = {
      client_uuid: uuid,
      turn_id: settled?.turnId ?? waiter?.turnId ?? "",
      state: hold.state,
      cause: hold.cause ?? "",
      outcome: hold.outcome ?? "",
      admitted: settled?.admitted ?? true,
    };
    if (refusedThenDropped) {
      LOGGER.info(fields, "the vendor dropped a peer send it had held; its StartTurn was already refused");
      return;
    }
    LOGGER.error(
      { ...fields, detail: "a peer send's admission was already answered; the vendor changed its fate afterwards" },
      "the vendor reported a hold for a peer send no StartTurn is waiting on",
    );
  }

  private admit(uuid: string, waiter: Waiter, state: CommandLifecycleState): void {
    clearTimeout(waiter.timer);
    this.waiting.delete(uuid);
    this.remember(uuid, waiter.turnId, true);
    LOGGER.debug(
      { client_uuid: uuid, turn_id: waiter.turnId, from: waiter.from, state, admission_ms: Math.round(this.nowMs() - waiter.sentAtMs) },
      "the vendor queued a peer send; its StartTurn is accepted",
    );
    waiter.resolve();
  }

  private fail(uuid: string, detail: string, message: string, cause?: string): void {
    const waiter = this.waiting.get(uuid);
    if (waiter === undefined) return;
    clearTimeout(waiter.timer);
    this.waiting.delete(uuid);
    this.remember(uuid, waiter.turnId, false);
    // INFO, NOT ERROR: the refusal this rejection becomes is recorded at
    // ERROR once, where the StartTurn is refused (engine/turn.ts).
    LOGGER.info({ client_uuid: uuid, turn_id: waiter.turnId, from: waiter.from, cause: cause ?? "", detail }, message);
    waiter.reject(new PeerNotAdmittedError(detail));
  }

  /** Bounded like the ledger's retired ring: a late frame names a recent send. */
  private remember(uuid: string, turnId: string, admitted: boolean): void {
    this.settled.delete(uuid);
    this.settled.set(uuid, { turnId, admitted });
    while (this.settled.size > SETTLED_PEER_SENDS_REMEMBERED) {
      const oldest = this.settled.keys().next();
      if (oldest.done === true) break;
      this.settled.delete(oldest.value);
    }
  }
}

/** How many answered peer sends a late hold frame is still matched to. */
const SETTLED_PEER_SENDS_REMEMBERED = 16;
