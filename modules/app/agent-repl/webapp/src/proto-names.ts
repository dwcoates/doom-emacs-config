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

import type { FrontendCommand as GeneratedFrontendCommand } from "../../proto/gen/ts/agentshim/frontend/v1/frame_pb";
import type {
  HibernationDetail as GeneratedHibernationDetail,
  ReviveSessionCmd as GeneratedReviveSessionCmd,
} from "../../proto/gen/ts/agentshim/frontend/v1/gate-revival_pb";
import type {
  QueueEntry as GeneratedQueueEntry,
  QueueEntryKeepAliveHold as GeneratedQueueEntryKeepAliveHold,
  QueueEntryRevivalHold as GeneratedQueueEntryRevivalHold,
} from "../../proto/gen/ts/agentshim/frontend/v1/prompt-queue_pb";
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

/**
 * The `QueueEntry.classification` arm keys — the verdict IS the arm.
 *
 * These were one `QueueClassification` enum before the figma-idl reshape. As
 * arms they carry their own payloads (a rationale, an acceptance flag, a
 * failure detail), so a verdict can no longer be spelled without the evidence
 * that belongs to it.
 */
export const QUEUE_CLASSIFICATION_ARM = {
  pending: "pending",
  interject: "interject",
  holdForTurnEnd: "holdForTurnEnd",
  error: "error",
} as const satisfies Record<string, ArmKeys<GeneratedQueueEntry["classification"]>>;

/**
 * The `QueueEntry.hold` arm keys — WHAT is holding an entry, when something
 * other than a running turn is.
 *
 * These were three independent optional fields before the reshape. As one
 * oneof, two holds at once is unrepresentable rather than merely unexpected.
 */
export const QUEUE_HOLD_ARM = {
  shutdown: "shutdown",
  keepAlive: "keepAlive",
  revival: "revival",
} as const satisfies Record<string, ArmKeys<GeneratedQueueEntry["hold"]>>;

/** The keep-alive hold's only field: the ping turn whose end releases it. */
export const KEEP_ALIVE_HOLD_TURN_ID: FieldKeys<GeneratedQueueEntryKeepAliveHold> = "turnId";

/**
 * The revival hold carries NOTHING: it is a bare marker arm.
 *
 * It named the session being revived before the reshape. That field is gone —
 * the queue push is fenced and the workspace it belongs to has exactly one
 * session, so a second copy of the session id here could only disagree with
 * `WorkspaceState`. Spelled as an empty tuple so that a field ADDED to the
 * message fails this build rather than being silently ignored.
 */
export const REVIVAL_HOLD_FIELDS: readonly FieldKeys<GeneratedQueueEntryRevivalHold>[] = [];

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
