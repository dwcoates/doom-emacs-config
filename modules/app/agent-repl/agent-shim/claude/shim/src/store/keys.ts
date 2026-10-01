/**
 * store/keys.ts — THE one place `upsert_key` and `write_id` are minted.
 *
 * # What each key is FOR
 *
 * `upsert_key` names the THING a row is about. Every frame of one unit carries
 * the same upsert key, so a re-sent or updated frame REPLACES the row rather
 * than appending a second one — which is what makes a unit that starts, streams
 * progress, and terminates appear once in the record instead of three times.
 * The keys are a CROSS-PLANE contract agreed with the store lead: the sidecar
 * mints the same key for the same thing from the file plane, so the shim's
 * stream-plane row and the sidecar's file-plane row for one unit COLLIDE ON
 * PURPOSE and settle as one.
 *
 * `write_id` names the WRITE. It is deterministic — `sha256(producer | source
 * coordinates | discriminator)` — so re-sending a frame after a store outage
 * mints the SAME id and the store absorbs the duplicate instead of doubling the
 * row. That is the whole retry story: the writer can resend freely because
 * identity comes from the content's provenance, not from when it was sent.
 *
 * # Why every function takes a typed id message
 *
 * THE FOUR IDENTIFIER SPACES ARE NEVER INTERCHANGEABLE (standing convention):
 * the vendor's agent id names WHICH AGENT, its tool-use id names WHICH CALL,
 * our activity id names WHICH UNIT, our TurnId names WHICH TURN. A join on the
 * wrong one produces plausible, silently wrong attribution. Taking `AgentId`
 * rather than `string` makes that mistake a type error instead of a support
 * ticket, which is worth the wrapping at every call site.
 */
import { createHash } from "node:crypto";
import { bindLog } from "../log.js";
import type { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-store-keys", operation: "shim.store.keys" });

/** Refuse an identity that is present but empty; a key built on one collides with every other. */
function requireValue(value: string, what: string): string {
  if (value === "") {
    const message = `shim store keys: ${what} is empty; a key built on an empty identity would collide with every other`;
    LOGGER.error({ what, detail: message }, message);
    throw new Error(message);
  }
  return value;
}

// ---------------------------------------------------------------------------
// producer
// ---------------------------------------------------------------------------

/**
 * This writer's name, as every row records it.
 *
 * Keyed by the ORIGINAL vendor session id — not the current one. A vendor
 * session id can rotate mid-conversation (`/clear` mints a new one), and a
 * producer name that rotated with it would make the same writer look like two,
 * splitting the deterministic write ids of one conversation across two
 * namespaces.
 */
export function producerId(originalVendorSessionId: string): string {
  return `claude-shim:${requireValue(originalVendorSessionId, "the original vendor session id")}`;
}

// ---------------------------------------------------------------------------
// upsert_key — one function per identity that owns rows
// ---------------------------------------------------------------------------

/**
 * A subagent RESUMED BY A SEND: every frame of the run the send woke, from its
 * first running beat to its terminal.
 *
 * NOT THE SEND'S OWN KEY. The run is addressed by the send's call id (its
 * detached-work handle and its unit are those bytes), but the send's card is a
 * different fact, keyed `activity:<send>`; under one key the run's frames
 * replaced the send in the record. Two facts never share one upsert key.
 */
export function resumedRunUpsertKey(send: conversationv1.AgentActivityId): string {
  return `resumed-run:${requireValue(send.value, "the resuming send's activity id")}`;
}

/**
 * A unit of work: every frame of it, from its first to its last.
 *
 * The activity id is the vendor `tool_use_id` for a tool call, and
 * `<message.id>:<block_index>` (0-based) for a text or thinking block.
 */
export function activityUpsertKey(unit: conversationv1.AgentActivityId): string {
  return `activity:${requireValue(unit.value, "the activity id")}`;
}

/**
 * The turn's prompt row.
 *
 * R15: this is the ONE served prompt row. The sidecar classifies the vendor's
 * own transcript user records as unserved precisely so the two do not both
 * appear in the feed.
 */
export function promptUpsertKey(turn: conversationv1.TurnId): string {
  return `prompt:${requireValue(turn.value, "the turn id")}`;
}

/**
 * A PEER MESSAGE — a message another Claude session sent into this
 * conversation (an inter-session peer, or a returning subagent's hand-back).
 *
 * Keyed by the vendor RECORD UUID, and that is a CROSS-PLANE contract: the
 * stream plane (this shim) and the file plane (the sidecar) both convert the
 * SAME vendor `user` record — the SDK streams it and writes it to the
 * transcript under one uuid — so both mint `peer:<uuid>` and their two rows
 * collapse into one exactly as a prompt's two planes do. The same uuid is spelled
 * into PeerMessage.id, so the daemon draws ONE feed row from whichever plane
 * arrives first.
 */
export function peerUpsertKey(vendorRecordUuid: string): string {
  return `peer:${requireValue(vendorRecordUuid, "the peer record uuid")}`;
}

/**
 * HOOK ROWS: this key is the ONE served hook row (ruling 2026-09-04).
 *
 * Same shape of ruling as the prompt row above, for the same reason. A hook is
 * recorded on both planes, but the vendor hands them DISJOINT identity
 * material — a `hook_id` here, a `toolUseID` in the transcript attachment, and
 * differing record uuids — so nothing downstream can join the two. The stream
 * owns the row (it is the plane that sees the firing's START, so it is the only
 * one that carries the hook's name and event and a turn), and the sidecar
 * classifies its transcript hook attachments as unserved items.
 */

/** An open question to the user (the AskUserQuestion call's own tool_use_id). */
export function questionUpsertKey(question: conversationv1.AgentQuestionId): string {
  return `question:${requireValue(question.value, "the question id")}`;
}

/**
 * A permission gate (the GATED call's tool_use_id).
 *
 * Consent joins to the work it gates, which is why the id is the gated call's
 * and not one minted for the prompt. It never collides with a question key
 * because a question is never a permission's gated call.
 */
export function permissionUpsertKey(permission: conversationv1.AgentPermissionId): string {
  return `permission:${requireValue(permission.value, "the permission id")}`;
}

/**
 * An agent's terminal frame (its success or failure).
 *
 * Keyed by agent AND by the vendor record's uuid, because one agent can
 * terminate more than once over a conversation — every turn of the main agent
 * ends — and a key of `terminal:<agent>` alone would have each ending overwrite
 * the last, leaving a conversation with exactly one visible terminal.
 */
export function terminalUpsertKey(
  agent: conversationv1.AgentId,
  vendorRecordUuid: string,
): string {
  return `terminal:${requireValue(agent.value, "the agent id")}:${requireValue(
    vendorRecordUuid,
    "the vendor record uuid",
  )}`;
}

/** The prefix every row of one detached shell run is keyed under. */
function bashRunPrefix(run: conversationv1.AgentActivityId): string {
  return `bash:${requireValue(run.value, "the bash run's activity id")}`;
}

/**
 * A detached shell run's START row, keyed by the RUN's activity id.
 *
 * The run is the bash tool call itself, so its lifecycle rows and its page line
 * are about the same unit — different key prefixes, one identity.
 *
 * ONE KEY PER KIND OF FACT (project lead amendment, binding). A shell run's rows
 * are the start, its one rendered tail, and the terminal. They shared one key
 * once, and under that spelling the terminal erased the output entirely — so
 * `WatchBashRun`, which replays a run's rows in first-insert order, had nothing
 * to replay.
 *
 * `bash:<run>:start`, THE SIDECAR'S SPELLING (`BashStartKey`, shim-sidecar
 * internal/convert/keys.go). The shim is the start's one producer — it writes
 * the row at the run's announcement — and a key that disagreed with the other
 * plane's for the same fact would let the two write two starts for one run.
 */
export function bashStartUpsertKey(run: conversationv1.AgentActivityId): string {
  return `${bashRunPrefix(run)}:start`;
}

/**
 * A detached shell run's RENDERED TAIL row: one row, superseded whole by every
 * write.
 *
 * Owner ruling 2026-09-23: output beyond what is rendered is not stored. The
 * run's output is therefore ONE snapshot — the most recent bytes, bounded by
 * `AgentBashTailCap`, and what was omitted before them — never one row per
 * delta. The retired `bash:<run>:<from_offset>` rows are left in the store as
 * outmoded and nothing mints that spelling any more.
 */
export function bashTailUpsertKey(run: conversationv1.AgentActivityId): string {
  return `${bashRunPrefix(run)}:tail`;
}

/**
 * A detached shell run's TERMINAL row.
 *
 * Its own key, so settling the run adds the stop notice rather than replacing
 * the output the run produced on its way there.
 */
export function bashTerminalUpsertKey(run: conversationv1.AgentActivityId): string {
  return `${bashRunPrefix(run)}:terminal`;
}

/**
 * A detached-work ANNOUNCEMENT, keyed by the handle the work is addressed by.
 *
 * ITS OWN KEY RATHER THAN THE UNIT'S: the announcement and the announced unit
 * are two rows about two facts — "this work left the turn" and "this is what
 * the work is" — and keying the announcement as the unit would have the
 * announcement overwrite the call that spawned it.
 */
export function detachedWorkUpsertKey(work: conversationv1.DetachedWorkId): string {
  return `detached:${requireValue(work.value, "the detached work id")}`;
}

/**
 * A session-level fact, keyed by WHICH KIND of fact and which vendor record
 * stated it.
 *
 * `arm` is the `SessionUpdate` oneof field name, so two different kinds of fact
 * arriving from one vendor record stay two rows. Shim-SYNTHESIZED facts
 * (`diagnostics`, `context_usage`) are never written at all — they are the
 * shim's own report about itself, not vendor conversation — so they never reach
 * this function.
 */
export function sessionUpsertKey(arm: string, vendorRecordUuid: string): string {
  return `session:${requireValue(arm, "the session update arm")}:${requireValue(
    vendorRecordUuid,
    "the vendor record uuid",
  )}`;
}

/**
 * The vendor's context-budget warning, keyed by the record that stated it.
 *
 * A PAGE LINE of the agent's book (landing 4), so it needs a key of its own: it
 * is not an activity, so `activity:` would be a lie, and a key naming only the
 * warning would have each new one overwrite the last — leaving a conversation
 * with exactly one visible warning however often the window filled.
 *
 * IT IS A `session:<arm>:<uuid>` KEY, NOT A `budget:` ONE (ruling, landing 5).
 * BOTH PLANES produce this fact from ONE transcript line — the sidecar reads
 * the file, the shim reads the stream — and write_id dedup collapses them into
 * one row only if the key BYTES match. The sidecar mints `session:<arm>:<uuid>`
 * for every arm it serves, so this plane spells it the same way.
 */
export function contextBudgetWarningUpsertKey(vendorRecordUuid: string): string {
  return sessionUpsertKey("context_budget_warning", vendorRecordUuid);
}

/**
 * A CONTEXT CUT — a `/clear` or a compaction — keyed by WHAT THE CUT IS, in the
 * one spelling BOTH PLANES can reach.
 *
 * THE SAME RULING AS THE BUDGET WARNING ABOVE, and for the same reason. write_id
 * dedup collapses two writes into one row only if the key BYTES match, and the
 * sidecar mints `session:context_cut:<id>` (its `SessionKey`, pinned by
 * `internal/convert/entry_test.go` and its own AGENTS.md). Where the keys did
 * not match, the store held TWO entries at TWO positions for ONE cut, and the
 * daemon — which keys the separation divider on the store position precisely so
 * a second delivery upserts the first's row — drew the divider TWICE, the second
 * copy from whichever plane's frame was less complete.
 *
 * WHAT THE IDENTITY IS DEPENDS ON WHAT THE VENDOR GIVES THE TWO PLANES:
 *
 *   - a COMPACTION is ONE vendor record on both planes — the stream's
 *     `compact_boundary` and the transcript's carry the SAME uuid
 *     (`testdata/captures/compaction-directed`, `b14c2f08-…`) — so the identity
 *     is that uuid;
 *   - a CLEAR is TWO DISJOINT records. `testdata/captures/identity-rotation-clear`
 *     has the stream's `conversation_reset` at `cc07c2a0-…` and the file plane's
 *     only evidence, the expanded `/clear` command envelope, at `04f97c00-…` in
 *     a transcript the reset never names; `new_conversation_id` is a third uuid
 *     nothing ever uses. The one fact both planes hold is THE SESSION THE CLEAR
 *     ROTATED TO — the sidecar's transcript file is named for it, and the
 *     `system:init` that follows the reset states it — so THAT is the identity.
 *     `PendingClear` in `convert/session-updates.ts` is why the row waits for it.
 */
export function contextCutUpsertKey(cutIdentity: string): string {
  return sessionUpsertKey("context_cut", cutIdentity);
}

/**
 * A residue row, keyed by THE RECORD IT WAS and nothing else.
 *
 * Residue has no identity of its own — that is what makes it residue — so the
 * key is its provenance. Keyed by the vendor record's own uuid so a redelivered
 * record settles as one row rather than accumulating copies of the same
 * unconverted line.
 *
 * NO KIND SEGMENT (ruling, landing 5). The kind lives INSIDE the row, and both
 * planes' conversions of one record must collapse to one row — which they
 * cannot if one plane's key carries a segment the other's does not.
 */
export function residueUpsertKey(vendorRecordUuid: string): string {
  return `residue:${requireValue(vendorRecordUuid, "the vendor record uuid")}`;
}

/**
 * The residue key for a stream record the vendor gave NO uuid.
 *
 * A PER-PROCESS MONOTONIC SEQUENCE, so it can never collide with the sidecar's
 * own `residue:file:<path>:<offset>`: the two planes name their unidentified
 * residue in disjoint spaces, because there is nothing about a record with no
 * identity for the two to agree on.
 */
export function streamResidueUpsertKey(sequence: number): string {
  return `residue:stream:${String(sequence)}`;
}

// ---------------------------------------------------------------------------
// write_id
// ---------------------------------------------------------------------------

/**
 * WHERE a frame came from in the vendor's own record, plus WHICH ARM it is.
 *
 * THE ONE DECLARATION. `store/persistence.ts` re-exports this type rather than
 * restating it: the same three fields were declared twice, once here for the
 * hash and once there for the envelope, differing only in the uuid field's
 * name — so `writer.ts` had to translate between two shapes of one fact, which
 * is exactly the drift a shared declaration makes impossible.
 *
 * `blockIndex` is present exactly for a frame derived from one content block of
 * a message: without it, every block of one assistant message would share the
 * message's uuid and therefore mint one write id, and the store would absorb
 * all but the first as duplicates.
 *
 * `discriminator` is the frame's ARM PATH, and it is what keeps two frames
 * derived from ONE vendor record (a tool call's start and the session fact the
 * same record implied) from hashing identically.
 */
export interface SourceCoordinates {
  /** The SDK message's `uuid` — the vendor's own name for the record. */
  readonly vendorUuid: string;
  /** The 0-based content-block index, for a block-derived frame. */
  readonly blockIndex?: number;
  /** The frame's arm path, e.g. `agent_frame.update.activity.read.start`. */
  readonly discriminator: string;
}

/** The coordinates as they appear inside the hashed string. */
export function formatSourceCoordinates(coordinates: SourceCoordinates): string {
  const uuid = requireValue(coordinates.vendorUuid, "the vendor record uuid");
  if (coordinates.blockIndex === undefined) return uuid;
  if (!Number.isInteger(coordinates.blockIndex) || coordinates.blockIndex < 0) {
    throw new Error(
      `shim store keys: block index ${String(coordinates.blockIndex)} is not a 0-based integer`,
    );
  }
  return `${uuid}:${coordinates.blockIndex}`;
}

/**
 * The deterministic identity of one write.
 *
 * `discriminator` is the frame's ARM PATH (e.g. `agent_frame.update.activity`),
 * which is what separates two frames the same vendor record produced — a tool
 * call's start arm and the session update that record also implied would
 * otherwise hash identically and one would be silently absorbed.
 */
export function writeId(producer: string, coordinates: SourceCoordinates): string {
  const material = `${requireValue(producer, "the producer")}|${formatSourceCoordinates(
    coordinates,
  )}|${requireValue(coordinates.discriminator, "the frame's arm path")}`;
  return createHash("sha256").update(material, "utf8").digest("hex");
}
