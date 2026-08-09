/**
 * proto-names — the ONE place the webapp spells a protocol name that the
 * hand-written protojson layer has to type out as a string literal.
 *
 * WHY THIS EXISTS. `frontend-proto.ts` and `frontend-command.ts` are hand-typed
 * against the wire (see their headers for why), which means every oneof arm
 * key, command arm key and enum name they speak is a string literal a human
 * wrote. The hibernation surface made the cost of that concrete: the SAME
 * vocabulary — the three cause arms, the revival modes and their compaction
 * scopes, the keep-alive hold
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
  ResyncCmd as GeneratedResyncCmd,
} from "../../proto/gen/ts/agentshim/frontend/v1/frame_pb";
import type {
  HibernationDetail as GeneratedHibernationDetail,
  ReviveCompactFirst as GeneratedReviveCompactFirst,
  ReviveSessionCmd as GeneratedReviveSessionCmd,
  WorkspaceGateView as GeneratedWorkspaceGateView,
} from "../../proto/gen/ts/agentshim/frontend/v1/gate-revival_pb";
import { CompactionScope as GeneratedCompactionScope } from "../../proto/gen/ts/agentshim/frontend/v1/gate-revival_pb";
import type { FailureKind as GeneratedFailureKind } from "../../proto/gen/ts/agentshim/frontend/v1/errors_pb";
import type { FailureCardView as GeneratedFailureCardView } from "../../proto/gen/ts/agentshim/frontend/v1/failure-card_pb";
import type {
  QueueEntry as GeneratedQueueEntry,
  QueueEntryKeepAliveHold as GeneratedQueueEntryKeepAliveHold,
  QueueEntryRevivalHold as GeneratedQueueEntryRevivalHold,
  QueueEntryBuildRefreshHold as GeneratedQueueEntryBuildRefreshHold,
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
  pauseMergeQueue: "pauseMergeQueue",
  resumeMergeQueue: "resumeMergeQueue",
  evictMerge: "evictMerge",
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
  buildRefresh: "buildRefresh",
} as const satisfies Record<string, ArmKeys<GeneratedQueueEntry["hold"]>>;

/**
 * `ResyncCmd`'s FIELD names — the replay watermark and the fence echo.
 *
 * This table exists because the encoder drifted from the message once already:
 * `ResyncCmd` carried `session_id` + `controller_generation_id`, those two were
 * reserved in favour of a single `fence`, and the hand-written encoder kept
 * emitting the old pair. Canonical protojson decoding is STRICT about unknown
 * fields, so the daemon rejected every resync outright and the client's replay
 * request silently never happened. Binding the spellings to the generated
 * message makes the same drift a build failure here instead.
 */
export const RESYNC_FIELD = {
  fromSeq: "fromSeq",
  fence: "fence",
} as const satisfies Record<FieldKeys<GeneratedResyncCmd>, FieldKeys<GeneratedResyncCmd>>;

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
 * The build-refresh hold carries NOTHING either: the arm being set is the whole
 * claim — the entry waits for its session's shim to restart onto the current
 * build, and the workspace it rides already names the session. Spelled as an
 * empty tuple for the same reason as above: a field ADDED to the message fails
 * this build rather than being silently ignored.
 */
export const BUILD_REFRESH_HOLD_FIELDS: readonly FieldKeys<GeneratedQueueEntryBuildRefreshHold>[] =
  [];

/** The compact-first arm's only field: how much the compaction may swallow. */
export const REVIVE_COMPACT_SCOPE: FieldKeys<GeneratedReviveCompactFirst> = "scope";

/**
 * The `frontend.v1.CompactionScope` prefix protobuf-es strips from its members.
 *
 * Same discipline as {@link PROMPT_ORIGIN_PREFIX}, and the stakes are higher: a
 * scope name the daemon does not recognize is NACKED rather than defaulted, so
 * a drifted spelling here takes the revival gate's every compacting option out
 * of service at once.
 */
export const COMPACTION_SCOPE_PREFIX = "COMPACTION_SCOPE_";

/**
 * The canonical protojson name of one generated `CompactionScope` member. The
 * argument is checked against the generated enum, so a renamed member fails to
 * compile here rather than producing a scope the daemon refuses.
 */
export function compactionScopeName<K extends keyof typeof GeneratedCompactionScope>(
  member: K,
): `${typeof COMPACTION_SCOPE_PREFIX}${K}` {
  return `${COMPACTION_SCOPE_PREFIX}${member}`;
}

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

/**
 * `WorkspaceGateView.gate` arm keys — WHETHER prompts may be sent right now.
 *
 * The arm IS the gate: there is no boolean beside it that could disagree, and a
 * view with neither arm is a malformed frame rather than an implied "open".
 */
export const WORKSPACE_GATE_ARM = {
  open: "open",
  hibernated: "hibernated",
} as const satisfies Record<
  ArmKeys<GeneratedWorkspaceGateView["gate"]>,
  ArmKeys<GeneratedWorkspaceGateView["gate"]>
>;

/**
 * `FailureCardView.lifecycle` arm keys — HOW a failure card ends.
 *
 * A window-shaped failure re-arrives under the SAME `ConversationItem.uuid`
 * with a different arm here, and the feed reconciles it in place, so an alarm
 * and its own all-clear can never stand beside each other.
 */
export const FAILURE_CARD_LIFECYCLE_ARM = {
  open: "open",
  resolved: "resolved",
  terminal: "terminal",
} as const satisfies Record<
  ArmKeys<GeneratedFailureCardView["lifecycle"]>,
  ArmKeys<GeneratedFailureCardView["lifecycle"]>
>;

/**
 * Which SIDE of the failure vocabulary a `FailureKind` arm belongs to.
 *
 * `errors.proto` states the rule in prose — every arm belongs to exactly one
 * side, machinery or vendor, and each arm's comment names the color that side
 * resolves the workspace to. This is that rule as data, and it is the ONLY
 * place the webapp decides a failure's side.
 *
 * It is keyed by the generated `FailureKind.kind` case union, so the record is
 * EXHAUSTIVE by construction: a new arm on the wire fails `npm run typecheck`
 * here until somebody states which side it is on, rather than falling through
 * to a neutral tone that would paint a vendor block in the machinery's color.
 *
 * The CLIENT-LOCAL arms (100+) are machinery too: a frontend can only ever
 * observe its OWN plumbing failing.
 */
export type FailureSide = "machinery" | "vendor";

export const FAILURE_KIND_SIDE = {
  // ---- MACHINERY (blue): agent-repl's own plumbing did not work. ----
  shimNotConnected: "machinery",
  shimRejected: "machinery",
  shimAckTimeout: "machinery",
  shimVersionMismatch: "machinery",
  shimSeqRegression: "machinery",
  shimDegraded: "machinery",
  shimStoreWriteRejected: "machinery",
  queryTermination: "machinery",
  shimNotSpawned: "machinery",
  shimHandshakeIncomplete: "machinery",
  shimUnhealthy: "machinery",
  sessionNotEstablished: "machinery",
  workspaceNotLive: "machinery",
  sessionDeleted: "machinery",
  sessionSuperseded: "machinery",
  reconnectSuperseded: "machinery",
  sessionShimDied: "machinery",
  sessionStartFailed: "machinery",
  sessionResumeFailed: "machinery",
  conversationUnresumable: "machinery",
  resumeModeRetired: "machinery",
  sessionEndedUnclassified: "machinery",
  historyRepullInFlight: "machinery",
  historyReplayTruncated: "machinery",
  interruptUndelivered: "machinery",
  queueEntryUnwired: "machinery",
  queueEntryKeepAliveHeld: "machinery",
  sessionHibernated: "machinery",
  keepAliveWindowUnclosed: "machinery",
  keepAliveWindowInverted: "machinery",
  compactionColdRead: "machinery",
  clientLogIdentityStale: "machinery",
  promptRefusedByMergeState: "machinery",
  internalUnclassified: "machinery",

  // ---- VENDOR (purple): the SDK or the vendor refused or concluded it. ----
  apiAuthenticationFailed: "vendor",
  apiBillingError: "vendor",
  apiRateLimit: "vendor",
  apiInvalidRequest: "vendor",
  apiServerError: "vendor",
  apiOverloaded: "vendor",
  apiOauthOrgNotAllowed: "vendor",
  apiModelNotFound: "vendor",
  apiNetworkDown: "vendor",
  apiRequestFailed: "vendor",
  apiUnknown: "vendor",
  apiMaxOutputTokens: "vendor",
  apiMaxTurns: "vendor",
  apiMaxBudget: "vendor",
  apiExecutionError: "vendor",
  apiRefusal: "vendor",
  apiTurnFailed: "vendor",

  // ---- CLIENT-LOCAL (blue): the frontend's own machinery. ----
  daemonUnreachable: "machinery",
  workspaceGone: "machinery",
  bootFailed: "machinery",
  controlPlaneFailed: "machinery",
  frameUndecodable: "machinery",
  staleBundle: "machinery",
  commandUnsent: "machinery",
  commandRejectionUnclassified: "machinery",
} as const satisfies Record<ArmKeys<GeneratedFailureKind["kind"]>, FailureSide>;

/** Every `FailureKind` arm key the daemon or this frontend may set. */
export const FAILURE_KIND_ARMS: readonly ArmKeys<GeneratedFailureKind["kind"]>[] = Object.keys(
  FAILURE_KIND_SIDE,
) as ArmKeys<GeneratedFailureKind["kind"]>[];
