/**
 * proto-names — the ONE place the webapp spells a protocol name that the
 * hand-written protojson layer has to type out as a string literal.
 *
 * WHY THIS EXISTS. `frontend-proto.ts` and `frontend-command.ts` are hand-typed
 * against the wire (see their headers for why), which means every oneof arm
 * key, command arm key and enum name they speak is a string literal a human
 * wrote. The hibernation surface made the cost of that concrete: the SAME
 * vocabulary — the three cause arms, the two revival modes, the keep-alive hold
 * key, `PROMPT_ORIGIN_CACHE_KEEP_ALIVE` — was re-declared in the decoder, in
 * the encoder, and in the Emacs frontend, aligned only by review. A daemon-side
 * rename would have left each copy independently, silently wrong.
 *
 * WHAT IT GUARANTEES. Every constant here is CHECKED AT BUILD TIME against the
 * committed `proto/gen/ts` stubs:
 *
 * - oneof arm keys are typed as the generated oneof's own `case` union, so a
 *   renamed or removed arm is a type error HERE rather than a decoder that
 *   silently stops recognizing a frame;
 * - command arm keys are typed as the generated `FrontendCommand.command` case
 *   union, so an arm the daemon does not have cannot be encoded;
 * - message field keys are typed as the generated message's own field names;
 * - enum NAMES — which no TypeScript type can spell, because protobuf-es strips
 *   the enum's prefix from its members — are built from the generated member
 *   key through {@link promptOriginName}, so the prefix is written once and the
 *   member is checked.
 *
 * `test/proto-names.test.ts` closes the last gap by asserting the table against
 * the generated DESCRIPTORS at runtime (json names, oneof membership, enum
 * member names), which catches the case the type system cannot see: a proto
 * that renames a field while keeping a compatible TypeScript shape.
 *
 * WHAT IT IS NOT. This is the fallback form of the remediation the review
 * asked for. The structural fix is a GENERATED constants module (one build step
 * emitting these names for both frontends, elisp included), which makes drift
 * impossible rather than merely build-failing; it was deferred as a
 * cross-branch build-surface change. A hand-written table checked against the
 * generated stubs makes drift UNLIKELY — it fails this build — not impossible:
 * a name nothing here references can still drift, and the Emacs frontend is
 * outside this module's reach entirely.
 */

import type {
  FrontendCommand as GeneratedFrontendCommand,
  HibernationDetail as GeneratedHibernationDetail,
  QueueEntry as GeneratedQueueEntry,
  QueueEntryKeepAliveHold as GeneratedQueueEntryKeepAliveHold,
  QueueEntryRevivalHold as GeneratedQueueEntryRevivalHold,
  ReviveSessionCmd as GeneratedReviveSessionCmd,
} from "../../proto/gen/ts/agentshim/frontend/v1/frontend_pb";
import { PromptOrigin as GeneratedPromptOrigin } from "../../proto/gen/ts/agentshim/core/v1/core_pb";

/** A generated oneof's arm keys, with protobuf-es's "nothing set" arm dropped. */
type ArmKeys<Oneof extends { case: string | undefined }> = Exclude<Oneof["case"], undefined>;

/**
 * A generated message's own field names — protobuf-es's runtime bookkeeping
 * keys (`$typeName`, `$unknown`) are not wire fields and are never spelled.
 */
type FieldKeys<Msg> = Exclude<Extract<keyof Msg, string>, `$${string}`>;

/**
 * `HibernationDetail.cause` arm keys.
 *
 * The arm IS the news (hibernation.ts renders a different sentence per arm), so
 * a mis-spelled key here would not degrade the gate — it would make the decoder
 * refuse a frame the daemon considers well-formed.
 */
export const HIBERNATION_CAUSE = {
  idleCutoff: "idleCutoff",
  forced: "forced",
  cacheExpired: "cacheExpired",
} as const satisfies Record<
  ArmKeys<GeneratedHibernationDetail["cause"]>,
  ArmKeys<GeneratedHibernationDetail["cause"]>
>;

/** `ReviveSessionCmd.mode` arm keys — the whole content of the command. */
export const REVIVE_MODE = {
  compactFirst: "compactFirst",
  direct: "direct",
} as const satisfies Record<
  ArmKeys<GeneratedReviveSessionCmd["mode"]>,
  ArmKeys<GeneratedReviveSessionCmd["mode"]>
>;

/**
 * The `FrontendCommand.command` arm keys the WEBAPP sends.
 *
 * Deliberately a SUBSET: the workspace-lifecycle commands are the Emacs
 * frontend's and `shutdown` is the daemon's, so they are absent here for the
 * same reason `frontend-command.ts` cannot encode them. The value type is the
 * generated union, so every key present is one the daemon really has.
 */
export const COMMAND_ARM = {
  submitPrompt: "submitPrompt",
  interrupt: "interrupt",
  permissionAnswer: "permissionAnswer",
  createSession: "createSession",
  setModel: "setModel",
  deleteSession: "deleteSession",
  resync: "resync",
  clientLog: "clientLog",
  queueForce: "queueForce",
  queueAccept: "queueAccept",
  queueCancel: "queueCancel",
  hibernateWorkspace: "hibernateWorkspace",
  reviveSession: "reviveSession",
} as const satisfies Record<string, ArmKeys<GeneratedFrontendCommand["command"]>>;

/** The `QueueEntry` field naming the keep-alive turn that holds an entry. */
export const QUEUE_ENTRY_KEEP_ALIVE_HOLD: FieldKeys<GeneratedQueueEntry> = "keepAliveHold";

/** The keep-alive hold's only field: the ping turn whose end releases it. */
export const KEEP_ALIVE_HOLD_TURN_ID: FieldKeys<GeneratedQueueEntryKeepAliveHold> = "turnId";

/** The `QueueEntry` field naming the pending revival that holds an entry. */
export const QUEUE_ENTRY_REVIVAL_HOLD: FieldKeys<GeneratedQueueEntry> = "revivalHold";

/** The revival hold's only field: the session whose compaction releases it. */
export const REVIVAL_HOLD_SESSION_ID: FieldKeys<GeneratedQueueEntryRevivalHold> = "sessionId";

/**
 * The `core.v1.PromptOrigin` prefix protobuf-es strips from its enum members.
 *
 * Canonical protojson carries the FULL member name, so the wire spelling is
 * this prefix plus the generated member key — the one place the prefix is
 * written, and the only part of an origin name a type cannot check.
 */
export const PROMPT_ORIGIN_PREFIX = "PROMPT_ORIGIN_";

/**
 * The canonical protojson name of one generated `PromptOrigin` member.
 *
 * The ARGUMENT is checked (it must be a generated member key) and the return
 * type is the exact literal, so a renamed arm fails to compile at every call
 * site instead of producing a name the daemon rejects at run time.
 */
export function promptOriginName<K extends keyof typeof GeneratedPromptOrigin>(
  member: K,
): `${typeof PROMPT_ORIGIN_PREFIX}${K}` {
  return `${PROMPT_ORIGIN_PREFIX}${member}`;
}

/** The unattributed origin — a value the daemon refuses, so both ends name it. */
export const PROMPT_ORIGIN_UNSPECIFIED = promptOriginName("UNSPECIFIED");

/** The daemon's cache keep-alive ping: the one origin the webapp distinguishes. */
export const PROMPT_ORIGIN_CACHE_KEEP_ALIVE = promptOriginName("CACHE_KEEP_ALIVE");

/** A prompt the user sent from the composer. */
export const PROMPT_ORIGIN_WEBAPP_USER_SENT = promptOriginName("WEBAPP_USER_SENT");

/** A prompt a card's own control sent on the user's behalf. */
export const PROMPT_ORIGIN_WEBAPP_CARD_ACTION = promptOriginName("WEBAPP_CARD_ACTION");
