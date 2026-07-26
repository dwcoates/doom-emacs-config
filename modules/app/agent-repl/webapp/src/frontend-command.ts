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

/** A protojson `google.protobuf.Struct` value (a free-form JSON object). */
export type CommandStruct = Record<string, unknown>;

/** SubmitPromptCmd — send a user prompt into the session's turn. */
export interface SubmitPromptBody {
  case: "submitPrompt";
  text: string;
  /** Optional per-prompt permission-mode override; "" = no override. */
  permissionMode: string;
}

/** InterruptCmd — stop the running turn (hard = SDK interrupt, soft = after msg). */
export interface InterruptBody {
  case: "interrupt";
  hard: boolean;
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
  model: string;
  permissionMode: string;
  configDir: string;
  /** "" = fresh; else resume this durable CLI conversation uuid. */
  resumeClaudeSessionId: string;
  /** Test-harness sessions against the offline fake SDK. */
  fake: boolean;
}

/** DeleteSessionCmd — tear down a session. */
export interface DeleteSessionBody {
  case: "deleteSession";
  sessionId: string;
}

/** ResyncCmd — request a replay from a store seq watermark (uint64). */
export interface ResyncBody {
  case: "resync";
  fromSeq: number;
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

export type FrontendCommandBody =
  | SubmitPromptBody
  | InterruptBody
  | PermissionAnswerBody
  | CreateSessionBody
  | DeleteSessionBody
  | ResyncBody
  | ClientLogBody
  | QueueForceBody
  | QueueAcceptBody
  | QueueCancelBody;

/** The command envelope: correlation id + workspace + exactly one command arm. */
export interface FrontendCommand {
  /** Correlates the CommandAck receipt back to this send. */
  requestId: string;
  /** Owning workspace; "" for session-lifecycle commands not scoped to one. */
  workspace: string;
  body: FrontendCommandBody;
}

/** The protojson arm key for each command body case. */
const ARM_KEY: Record<FrontendCommandBody["case"], string> = {
  submitPrompt: "submitPrompt",
  interrupt: "interrupt",
  permissionAnswer: "permissionAnswer",
  createSession: "createSession",
  deleteSession: "deleteSession",
  resync: "resync",
  clientLog: "clientLog",
  queueForce: "queueForce",
  queueAccept: "queueAccept",
  queueCancel: "queueCancel",
};

/** Build the nested protojson command message for one body arm. */
function encodeBody(b: FrontendCommandBody): Record<string, unknown> {
  switch (b.case) {
    case "submitPrompt":
      return { text: b.text, permissionMode: b.permissionMode };
    case "interrupt":
      return { hard: b.hard };
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
        model: b.model,
        permissionMode: b.permissionMode,
        configDir: b.configDir,
        resumeClaudeSessionId: b.resumeClaudeSessionId,
        fake: b.fake,
      };
    case "deleteSession":
      return { sessionId: b.sessionId };
    case "resync":
      // uint64 renders as a JSON string in protojson.
      return { fromSeq: String(b.fromSeq) };
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
