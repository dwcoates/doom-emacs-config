/**
 * agentshim.frontend.v1.FrontendCommand — the ONLY frontend→daemon inbound
 * frame, hand-encoded as canonical proto3 JSON (protojson).
 *
 * This REPLACES the legacy JSON `ClientCommand` vocabulary (protocol.ts) AND
 * the `POST /sessions` HTTP path: after the S8/S9 cutover every command the
 * composer, permission controls, session lifecycle, and reconnect resync send
 * is one `FrontendCommand` frame over the WebSocket. The daemon decodes these
 * with a strict protojson reader (the same wire the S7 Emacs frontend already
 * speaks over its UDS), so the two frontends can never diverge on the command
 * plane either.
 *
 * WIRE FORMAT (mirrors the decode side in frontend-proto.ts):
 * - Field names are canonical protojson lowerCamelCase.
 * - int64/uint64 scalars are rendered as JSON strings.
 * - A `google.protobuf.Struct` (permission_answer.updated_input) is a plain
 *   JSON object.
 * - The `command` oneof is a single top-level key named for the selected arm
 *   (`submitPrompt`, `interrupt`, …) whose value is the nested command message
 *   — exactly how the inbound `FrontendFrame.frame` oneof is shaped.
 *
 * SCOPE: only the commands the WEBAPP originates are modeled here —
 * submit_prompt, interrupt, permission_answer, create_session, delete_session,
 * resync, client_log, and the three queue controls. The workspace-lifecycle
 * commands (merge/close/open) are the Emacs frontend's, and `shutdown` is a
 * daemon-lifecycle command the webapp never sends; none are encodable here so
 * a stray call is a compile error, not a silently malformed frame.
 */
import {
  COMMAND_ARM,
  MERGE_DEQUEUE_ANSWER,
  PROMPT_ORIGIN_WEBAPP_CARD_ACTION,
  PROMPT_ORIGIN_WEBAPP_USER_SENT,
  PAGE_ANCHOR_ARM,
  PAGE_BEFORE_FIELD,
  PAGE_CMD_FIELD,
  PAGE_TAIL_FIELD,
  RESYNC_FIELD,
  REVIVE_COMPACT_SCOPE,
  REVIVE_MODE,
  compactionScopeName,
} from "./proto-names.js";

// The webapp hand-types protojson for the same module-resolution reason as
// `frontend-proto.ts`. Values are the canonical core.v1 enum names, so the Go
// protojson decoder validates them against the generated descriptor.
// The origin NAMES come from the build-checked spelling table (proto-names.ts)
// rather than being typed out here: the Go protojson decoder validates them
// against the generated descriptor, so a drifted name is a refused command.
export const PromptOrigin = {
  WEBAPP_USER_SENT: PROMPT_ORIGIN_WEBAPP_USER_SENT,
  WEBAPP_CARD_ACTION: PROMPT_ORIGIN_WEBAPP_CARD_ACTION,
} as const;
export type PromptOrigin = (typeof PromptOrigin)[keyof typeof PromptOrigin];

/** A protojson `google.protobuf.Struct` value (a free-form JSON object). */
export type CommandStruct = Record<string, unknown>;

/** SubmitPromptCmd — send a user prompt into the session's turn. */
export interface SubmitPromptBody {
  case: "submitPrompt";
  text: string;
  /** Optional per-prompt permission-mode override; "" = no override. */
  permissionMode: string;
  promptOrigin: PromptOrigin;
}

/** InterruptCmd — stop the running turn. `confirmAgents` answers the daemon's
 * interrupt_confirm_required challenge: false asks, true confirms stopping
 * live subagents when no turn is in flight. */
export interface InterruptBody {
  case: "interrupt";
  confirmAgents: boolean;
}

/** PermissionAnswerCmd — resolve a pending canUseTool request. */
export interface PermissionAnswerBody {
  case: "permissionAnswer";
  permissionRequestId: string;
  allow: boolean;
  /** Optional allow-with-edits Struct; omitted from the frame when undefined. */
  updatedInput?: CommandStruct;
  /** Denial reason; "" on an allow. */
  denyMessage: string;
}

/**
 * CreateSessionCmd — spawn a session; the new id arrives via a pushed SessionView.
 *
 * `allow_ungated` is DELIBERATELY absent. The daemon refuses to create a
 * session in a mode that leaves it with no permission gate
 * (`bypassPermissions`, under which the SDK auto-approves every tool before
 * `canUseTool` is consulted) unless the create carries that consent, and the
 * webapp has no business consenting on the user's behalf from a browser tab.
 * Omitting the field here makes a webapp-originated ungated session
 * unrepresentable rather than merely discouraged.
 */
export interface CreateSessionBody {
  case: "createSession";
  cwd: string;
  permissionMode: string;
  configDir: string;
  /**
    * Which conversation to land on, as an INTENT rather than a pointer:
    * "RESUME_MODE_CONTINUE" (the daemon resolves this cwd's conversation) or
    * "RESUME_MODE_EXPLICIT". There is no fresh intent — the enum's tag 2 was
    * retired because a workspace's conversation is not caller-replaceable,
    * and the daemon refuses a create that still names it.
    *
    * The browser has no business naming a conversation — it used to send a
    * uuid it had put in localStorage, which made it a second authority on
    * which conversation a workspace owns. See ResumeMode in frontend.proto.
    */
   resumeMode: string;
  /** Test-harness sessions against the offline fake SDK. */
  fake: boolean;
}

/** A deliberate model-update request. Bootstrap and rebind cannot encode it. */
export interface SetModelBody {
  case: "setModel";
  model: string;
}

/** DeleteSessionCmd — tear down a session. */
export interface DeleteSessionBody {
  case: "deleteSession";
  sessionId: string;
}

/**
 * ResyncCmd — request a replay from a store seq watermark (uint64).
 *
 * The FENCE is copied from the revisioned WorkspaceState that authorized this
 * request.  It binds an old webview to the exact controller generation it
 * observed, so the daemon can refuse the request before replaying a newer or
 * retired controller's history.
 *
 * It is one token, not the `session_id` + `controller_generation_id` pair this
 * carried before: those two are reserved in the message, and canonical
 * protojson decoding rejects an unknown field, so emitting them costs the whole
 * command.  The spellings are bound to the generated message via
 * {@link RESYNC_FIELD}.
 */
export interface ResyncBody {
  case: "resync";
  fromSeq: number;
  fence: string;
}

/**
 * ConversationPageCmd — ask for ONE page of history.
 *
 * It does not replace {@link ResyncBody}, which stays the LIVE-PAGE path: an
 * incremental `from_seq` resync is how a client that already holds history
 * catches up. This is the COLD-OPEN path, and the load-more that walks
 * backwards from it.
 *
 * `cursor` empty means the TAIL anchor (the newest end of the conversation);
 * a non-empty cursor is the BEFORE anchor and is the daemon's own opaque
 * token, copied back byte-for-byte. The two are a oneof on the wire, and this
 * shape is what proves exactly one arm is ever encoded.
 *
 * The FENCE is captured at DECISION time, exactly as a resync's is, and for
 * exactly the same reason: a delayed request must not rebind itself to a newer
 * controller generation and page a conversation nobody asked about.
 */
export interface ConversationPageBody {
  case: "conversationPage";
  /** "" = tail; otherwise the opaque continuation token from a prior page. */
  cursor: string;
  /** 0 = the daemon's default; the daemon clamps its own ceiling. */
  limit: number;
  fence: string;
}

/** The `ClientLogLevel` enum values, as their canonical protojson names. */
const CLIENT_LOG_LEVEL_NAME = {
  info: "CLIENT_LOG_LEVEL_INFO",
  warn: "CLIENT_LOG_LEVEL_WARN",
  error: "CLIENT_LOG_LEVEL_ERROR",
} as const;

export type ClientLogBodyLevel = keyof typeof CLIENT_LOG_LEVEL_NAME;

/**
 * ClientLogCmd (E4) — mirror one webapp diagnostic line into the daemon's log.
 *
 * The webview's JS console is invisible and unpersisted, so this is how a
 * delivery-path failure in here leaves evidence anywhere. It is EVIDENCE, not
 * a control signal: the daemon records it and does nothing else.
 */
export interface ClientLogBody {
  case: "clientLog";
  level: ClientLogBodyLevel;
  message: string;
  /** Optional structured payload (ids, counters, timings); omitted when absent. */
  context?: CommandStruct;
}

/**
 * The three held-prompt queue controls (E4). All carry only an entry id: the
 * daemon owns what happens next, and the webapp names WHICH entry, never what
 * to do with it beyond the verb the arm already is.
 */
export interface QueueForceBody {
  case: "queueForce";
  entryId: string;
}
export interface QueueAcceptBody {
  case: "queueAccept";
  entryId: string;
}
export interface QueueCancelBody {
  case: "queueCancel";
  entryId: string;
}

/**
 * HibernateWorkspaceCmd — put this workspace's settled session to sleep now.
 *
 * Deliberately empty: the workspace is the envelope's, and there is nothing
 * else to say. The daemon REFUSES it while a turn is live or the merge lease
 * is held, and that refusal arrives as an ordinary rejected `CommandAck` —
 * the webapp never pre-judges settledness, which it cannot resolve.
 */
export interface HibernateWorkspaceBody {
  case: "hibernateWorkspace";
}

/**
 * ReviveSessionCmd — the user's revival decision for a hibernated workspace.
 *
 * The mode is a ONEOF of empty messages on the wire, so "no decision" is
 * unrepresentable; it is modeled here as a closed union for the same reason,
 * and a caller cannot send a revive without choosing.
 */
export interface ReviveSessionBody {
  case: "reviveSession";
  /**
   * The user's whole answer to the gate: resume as-is, clear, or compact first
   * at a stated scope. One value, because the halves are not independent — a
   * scope means nothing to a direct resume and nothing to a clear.
   */
  decision: ReviveDecision;
}

/**
 * The revival modes, as the arm keys the wire's oneof spells them with. Taken
 * from the build-checked spelling table so the key this end sends and the arm
 * the daemon reads cannot become two different strings.
 */
export type ReviveMode = (typeof REVIVE_MODE)[keyof typeof REVIVE_MODE];

/**
 * The COMPACTING decisions, each mapped to the protojson `CompactionScope` name
 * its command carries.
 *
 * THE TABLE IS THE ONLY MAPPING. A decision the gate can offer but this table
 * cannot spell is a compile error at the union below, so an option added to the
 * card can never reach the socket as a scope the daemon refuses.
 */
export const COMPACT_DECISION_SCOPE = {
  compactAll: compactionScopeName("ALL"),
  compactResponses: compactionScopeName("RESPONSES"),
  compactPrompts: compactionScopeName("PROMPTS"),
  compactPromptsAndResponses: compactionScopeName("PROMPTS_AND_RESPONSES"),
} as const;

/** A decision that compacts, named by what the compaction is allowed to eat. */
export type CompactDecision = keyof typeof COMPACT_DECISION_SCOPE;

/**
 * The whole revival vocabulary: the four compactions, the direct resume, and
 * the clear.
 *
 * MODELED AS ONE CLOSED UNION rather than a mode beside an optional scope,
 * because "resume as-is, summarizing the prompts" has no meaning and should not
 * be a value anything has to rule out. `clear` is in the union for the same
 * reason it is not a fifth scope: a scope says what a summary KEEPS, and a
 * clear keeps nothing.
 */
export type ReviveDecision = CompactDecision | "direct" | "clear";

/**
 * The three merge-queue runtime controls.
 *
 * Pause and resume are DAEMON-GLOBAL and carry nothing: the envelope's
 * workspace is ignored, and both are idempotent. Evict names ONE waiting run
 * by the run id the roster and every `MergeStatus` already carry; the daemon
 * refuses it for the running head, because only that run's drain goroutine may
 * retire it.
 */
export interface PauseMergeQueueBody {
  case: "pauseMergeQueue";
}
export interface ResumeMergeQueueBody {
  case: "resumeMergeQueue";
}
export interface EvictMergeBody {
  case: "evictMerge";
  runId: string;
}

/**
 * The answer to a workspace's outstanding merge dequeue offer.
 *
 * It names the OFFER, never the run: a run id still resolves after its offer
 * was answered or superseded, so a click on a stale card would dequeue a merge
 * the user never saw the question for. The daemon refuses an id that is not
 * the outstanding one.
 *
 * `keep` is a real answer rather than the absence of one, and sending it is
 * what takes the card down — there is no local dismissal.
 */
export interface AnswerMergeDequeueBody {
  case: "answerMergeDequeue";
  offerId: string;
  answer: "dequeue" | "keep";
}

export type FrontendCommandBody =
  | SubmitPromptBody
  | InterruptBody
  | PermissionAnswerBody
  | CreateSessionBody
  | SetModelBody
  | DeleteSessionBody
  | ResyncBody
  | ConversationPageBody
  | ClientLogBody
  | QueueForceBody
  | QueueAcceptBody
  | QueueCancelBody
  | HibernateWorkspaceBody
  | ReviveSessionBody
  | PauseMergeQueueBody
  | ResumeMergeQueueBody
  | EvictMergeBody
  | AnswerMergeDequeueBody;

/** The command envelope: correlation id + workspace + exactly one command arm. */
export interface FrontendCommand {
  /** Correlates the CommandAck receipt back to this send. */
  requestId: string;
  /** Owning workspace; "" for session-lifecycle commands not scoped to one. */
  workspace: string;
  body: FrontendCommandBody;
}

/** The protojson arm key for each command body case.
 *
 * EXPORTED so a test can assert the vocabulary is CLOSED — specifically that
 * no paint acknowledgment can be sent. Viewer-based attestation is gone: a
 * workspace's color is connection truth, so nothing this end draws is a
 * statement the daemon wants back. */
export const ARM_KEY: Record<FrontendCommandBody["case"], string> = COMMAND_ARM;

/** Build the nested protojson command message for one body arm. */
function encodeBody(b: FrontendCommandBody): Record<string, unknown> {
  switch (b.case) {
    case "submitPrompt":
      return { text: b.text, permissionMode: b.permissionMode, promptOrigin: b.promptOrigin };
    case "interrupt":
      return { confirmAgents: b.confirmAgents };
    case "permissionAnswer": {
      const arm: Record<string, unknown> = {
        permissionRequestId: b.permissionRequestId,
        allow: b.allow,
        denyMessage: b.denyMessage,
      };
      // A `google.protobuf.Struct` is a plain JSON object; only present it when
      // the caller supplied an allow-with-edits payload (never fabricate {}).
      if (b.updatedInput !== undefined) arm.updatedInput = b.updatedInput;
      return arm;
    }
    case "createSession":
      return {
        cwd: b.cwd,
        permissionMode: b.permissionMode,
        configDir: b.configDir,
        resumeMode: b.resumeMode,
        fake: b.fake,
      };
    case "setModel":
      return { model: b.model };
    case "deleteSession":
      return { sessionId: b.sessionId };
    case "resync":
      // uint64 renders as a JSON string in protojson.
      return {
        [RESYNC_FIELD.fromSeq]: String(b.fromSeq),
        [RESYNC_FIELD.fence]: b.fence,
      };
    case "conversationPage": {
      // The anchor is a oneof, so exactly one arm is emitted — never both and
      // never neither. The daemon REFUSES an anchorless command rather than
      // reading it as a tail, so an encoder that emitted neither would fail
      // every page request rather than degrade quietly.
      //
      // uint32 renders as a JSON number in protojson, unlike the uint64 above.
      const anchor =
        b.cursor === ""
          ? { [PAGE_ANCHOR_ARM.tail]: { [PAGE_TAIL_FIELD.limit]: b.limit } }
          : {
              [PAGE_ANCHOR_ARM.before]: {
                [PAGE_BEFORE_FIELD.cursor]: b.cursor,
                [PAGE_BEFORE_FIELD.limit]: b.limit,
              },
            };
      return { ...anchor, [PAGE_CMD_FIELD.fence]: b.fence };
    }
    case "clientLog": {
      // An enum renders as its proto NAME in canonical protojson.
      const arm: Record<string, unknown> = {
        level: CLIENT_LOG_LEVEL_NAME[b.level],
        message: b.message,
      };
      // A `google.protobuf.Struct` is a plain JSON object; only present it when
      // the caller supplied one (never fabricate {}).
      if (b.context !== undefined) arm.context = b.context;
      return arm;
    }
    case "queueForce":
    case "queueAccept":
    case "queueCancel":
      return { entryId: b.entryId };
    case "hibernateWorkspace":
      // Empty message: the workspace rides the envelope, and there is no
      // second thing to say. `{}` is the canonical protojson for it.
      return {};
    case "reviveSession":
      // The DIRECT and CLEAR arms' values are empty — the arm key is the whole
      // decision — while a compaction additionally states its scope, which the
      // daemon nacks rather than defaults if it is missing.
      if (b.decision === "direct") return { [REVIVE_MODE.direct]: {} };
      if (b.decision === "clear") return { [REVIVE_MODE.clear]: {} };
      return {
        [REVIVE_MODE.compactFirst]: {
          [REVIVE_COMPACT_SCOPE]: COMPACT_DECISION_SCOPE[b.decision],
        },
      };
    case "pauseMergeQueue":
    case "resumeMergeQueue":
      // Empty messages: the verb IS the arm, and the queue is daemon-global,
      // so there is nothing for the body to name.
      return {};
    case "evictMerge":
      return { runId: b.runId };
    case "answerMergeDequeue":
      // The answer is a oneof of EMPTY messages, so the arm key IS the
      // decision and its value carries nothing. Encoding a bool instead would
      // make "no answer" representable, which is the state the schema refuses.
      return { offerId: b.offerId, [MERGE_DEQUEUE_ANSWER[b.answer]]: {} };
  }
}

/**
 * Serialize a `FrontendCommand` to its canonical protojson wire string.
 * The envelope carries `requestId` + `workspace` and exactly one command arm.
 */
export function encodeFrontendCommand(cmd: FrontendCommand): string {
  const frame: Record<string, unknown> = {
    requestId: cmd.requestId,
    workspace: cmd.workspace,
  };
  frame[ARM_KEY[cmd.body.case]] = encodeBody(cmd.body);
  return JSON.stringify(frame);
}
