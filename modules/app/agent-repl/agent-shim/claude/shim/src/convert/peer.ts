/**
 * convert/peer.ts — A MESSAGE FROM ANOTHER CLAUDE, on the live stream plane.
 *
 * The vendor injects inter-session peer messages and subagent hand-backs as
 * user-role records whose `origin.kind` is `"peer"` (a hand-back also sets
 * `origin.handback`). They are NOT prompts: before this converter they were
 * dropped by convertUserRecord's R15 branch (a user record with no tool result
 * is "the prompt the shim already wrote") and so vanished from a resumed
 * session's feed. This is where the stream plane recognizes one and emits the
 * ONE peer-message row instead.
 *
 * THE DISCRIMINATOR IS SHARED WITH THE SIDECAR (file plane) by contract, not by
 * code: `origin.kind == "peer"` on both planes, keyed on the vendor record uuid
 * on both planes (see store/keys.ts peerUpsertKey), so the two planes' rows for
 * one record collapse into one exactly as a prompt's do.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { peerUpsertKey } from "../store/keys.js";
import type { FoldContext } from "./fold-context.js";
import { bookFor } from "./fold-context.js";

const LOGGER = bindLog({ component: "shim-convert-peer", operation: "shim.convert.peer" });

/** The vendor's message origin, read loosely: an observed shape, not a declared type. */
interface RawPeerOrigin {
  readonly kind?: string;
  readonly from?: string;
  readonly name?: string;
  readonly senderTaskId?: string;
  readonly body?: string;
  readonly handback?: boolean;
}

/** The sender label and body a peer origin resolves to. */
export interface PeerFacts {
  readonly sender: string;
  readonly body: string;
}

/**
 * The peer facts of a user record, or undefined when it is not a peer message.
 *
 * A record is a peer message exactly when `origin.kind == "peer"` — which
 * covers both an inter-session peer and a subagent hand-back (`handback` set).
 * The sender is `origin.from`, or `origin.senderTaskId` when `from` is absent;
 * the body is `origin.body` when the vendor states it, else the record's own
 * text content (envelope and all), so nothing the sender said is lost.
 */
export function peerFacts(record: Record<string, unknown>, fallbackBody: string): PeerFacts | undefined {
  const origin = record.origin;
  if (typeof origin !== "object" || origin === null) return undefined;
  const raw = origin as RawPeerOrigin;
  if (raw.kind !== "peer") return undefined;
  const sender = (raw.from ?? "").trim() !== "" ? (raw.from as string) : (raw.senderTaskId ?? "");
  const body = (raw.body ?? "").trim() !== "" ? (raw.body as string) : fallbackBody;
  return { sender, body };
}

/** The first text of a user message's content, whether a string or a block list. */
export function userRecordText(message: { content?: unknown } | undefined): string {
  const content = message?.content;
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  const parts: string[] = [];
  for (const raw of content) {
    if (typeof raw === "object" && raw !== null) {
      const block = raw as { type?: string; text?: string };
      if (block.type === "text" && typeof block.text === "string") parts.push(block.text);
    }
  }
  return parts.join("\n");
}

/**
 * One peer-message PersistEntry, or undefined when this user record is not a
 * peer message and the ordinary user-record path should handle it.
 *
 * The book is the recipient's — the main agent for a top-level message, the
 * spawning call's subagent book for one addressed under a subagent — resolved
 * exactly as every other stream frame's book is.
 */
export function convertPeerMessage(
  message: Extract<SdkMessage, { type: "user" }>,
  context: FoldContext,
): PersistEntry | undefined {
  const record = message as unknown as Record<string, unknown>;
  const facts = peerFacts(record, userRecordText(message.message));
  if (facts === undefined) return undefined;

  // The SDK types a user record's uuid as always-present, but the audit records
  // it as OPTIONAL on the real surface, so it is read loosely and guarded.
  const uuid = (record.uuid ?? "") as string;
  if (uuid === "") {
    // A peer record with no uuid cannot be keyed cross-plane, so the stream and
    // file rows could never collapse. It is a producer defect, not something to
    // key on a stand-in; let the ordinary path residue it.
    // warn: a defect because a peer record with no uuid cannot be keyed cross-plane
    LOGGER.warn({}, "a peer user record carried no uuid; it cannot be keyed and is left to the ordinary path");
    return undefined;
  }

  const book = bookFor(context, record.parent_tool_use_id);
  const peer = create(conversationv1.PeerMessageSchema, {
    agent: book,
    sender: facts.sender,
    body: facts.body,
    id: uuid,
  });
  LOGGER.logVerbose(
    { uuid, sender: facts.sender, agent: book.value },
    "a peer message (origin.kind=peer) emitted as its own row rather than a prompt",
  );
  return {
    agentId: book,
    upsertKey: peerUpsertKey(uuid),
    source: { vendorUuid: uuid, discriminator: "peer_message" },
    keepalive: false,
    item: { kind: "peer", peer },
  };
}
