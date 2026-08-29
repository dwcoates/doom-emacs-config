/**
 * store/persistence.ts — THE seam between the control plane and the record
 * plane.
 *
 * The engine never speaks to the store directly. It hands this interface
 * self-describing ENTRIES — "this prompt, on this agent's book, from this
 * vendor record" — and the record plane decides how they become store rows:
 * which upsert key, which write id, which routing arm. That split is what lets
 * the two planes be built and tested independently, and it is why the engine
 * carries no knowledge of `store.v1` beyond this file.
 *
 * TWO WRITE PATHS, DELIBERATELY. `writeDurable` resolves on the store's own
 * durable ack and is used exactly where the record must exist before the next
 * act (R15: the turn's `AgentPrompt` row, before the first activity frame).
 * `write` is enqueued and retried from a bounded buffer, because a turn cannot
 * stall on a store blip and a shim that awaited every frame would make the
 * store's latency the vendor's.
 *
 * OWNERSHIP. The record-plane agent implements this. The engine agent declares
 * it and codes against it; the declarations here are the contract both sides
 * agreed, and a change to them is a change to that contract.
 */
import type { conversationv1, storev1 } from "../proto.js";

/** WHERE an entry came from in the vendor's own record — the write id's material. */
export interface SourceCoordinates {
  /** The SDK message's uuid. */
  vendorUuid: string;
  /** The 0-based content-block index, for a block-derived entry. */
  blockIndex?: number;
  /** The entry's arm path, which separates two entries one record produced. */
  discriminator: string;
}

/**
 * One thing to record.
 *
 * `agentId` names THE BOOK the row belongs to — the main agent or a subagent;
 * for a session update it is the main agent, because a session fact belongs to
 * the conversation and the conversation is the main agent's book.
 */
export interface PersistEntry {
  agentId: conversationv1.AgentId;
  /** From `store/keys.ts` — the row this entry supersedes. */
  upsertKey: string;
  source: SourceCoordinates;
  /** True for a keep-alive turn's material: indexed, never served. */
  keepalive: boolean;
  item:
    | { kind: "prompt"; prompt: conversationv1.AgentPrompt }
    | { kind: "frame"; frame: conversationv1.AgentFrame }
    | { kind: "session_update"; update: conversationv1.SessionUpdate }
    | { kind: "bash_run"; run: conversationv1.AgentActivityId; frame: conversationv1.AgentBash }
    | { kind: "residue"; residue: storev1.StoreUnservedItem };
}

/** An opened reading session: the first page, then the pure tail. */
export interface AgentPageSession {
  page: conversationv1.HistoryPage;
  tail: AsyncIterable<conversationv1.HistoryEntryAt>;
  close(): void;
}

/** Why a persistence call could not be served. */
export type PersistenceFailureKind =
  | "store_unavailable"
  | "unknown_agent"
  | "stale_pointer"
  | "unknown_work";

/**
 * A persistence refusal, typed so the engine can map it onto the wire's own
 * failure arms without parsing a message string.
 */
export class PersistenceError extends Error {
  constructor(
    public readonly kind: PersistenceFailureKind,
    message: string,
  ) {
    super(message);
    this.name = "PersistenceError";
  }
}

/** The record plane, as the engine drives it. */
export interface Persistence {
  /** Write and await the store's DURABLE ack. R15's path. */
  writeDurable(entries: PersistEntry[]): Promise<void>;
  /** Enqueue; retried from the bounded buffer. `flush()` awaits the queue. */
  write(entries: PersistEntry[]): void;
  /** Every buffered write acked, or loudly dropped. */
  flush(): Promise<void>;
  /** One agent's opening page plus its tail. */
  openAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
  ): Promise<AgentPageSession>;
  /** An OLDER page of one agent's book. */
  readAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    after: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage>;
  /** The open obligations, per the record. */
  liveWork(): Promise<storev1.GetLiveWorkSuccess>;
  /** A detached shell run's frames, as the sidecar feeds them. */
  openBashRun(
    work: conversationv1.DetachedWorkId,
  ): Promise<AsyncIterable<conversationv1.AgentBash>>;
  /** Faults the record plane observed about itself. Returns an unsubscribe. */
  onFault(listener: (fault: conversationv1.SessionFault) => void): () => void;
  /** Degraded windows the record plane opened and closed. Returns an unsubscribe. */
  onDegradedWindow(listener: (w: conversationv1.SessionDegradedWindow) => void): () => void;
}

/**
 * The persistence before there is a record plane.
 *
 * SCAFFOLD, owned jointly and replaced by the record-plane agent's real
 * factory. It REFUSES rather than pretending: a placeholder that answered with
 * an empty page would make a history read look like an empty conversation, and
 * one that swallowed writes would make a lost record look like a written one.
 * Every verb answers `store_unavailable`, which the engine already maps onto a
 * loud `SessionFault` and a `ReadHistoryStoreUnavailable`.
 */
export function unavailablePersistence(): Persistence {
  const refuse = (verb: string): PersistenceError =>
    new PersistenceError(
      "store_unavailable",
      `shim persistence: ${verb} is not implemented in this build; store/persistence.ts is a scaffold placeholder owned by the record-plane agent`,
    );
  return {
    writeDurable: () => Promise.reject(refuse("writeDurable")),
    write: () => {
      throw refuse("write");
    },
    flush: () => Promise.resolve(),
    openAgentPage: () => Promise.reject(refuse("openAgentPage")),
    readAgentPage: () => Promise.reject(refuse("readAgentPage")),
    liveWork: () => Promise.reject(refuse("liveWork")),
    openBashRun: () => Promise.reject(refuse("openBashRun")),
    onFault: () => () => undefined,
    onDegradedWindow: () => () => undefined,
  };
}
