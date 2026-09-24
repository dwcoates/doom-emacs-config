/**
 * FIXTURES — one builder per drawn family, producing COMPLETE valid messages.
 *
 * "Complete" is the contract the client is entitled to: every non-optional
 * field set, every oneof set. A fixture that leaves one unset is a malformed
 * view by the client's own rules and would fail a test for the wrong reason,
 * so absence here is always deliberate (an `optional` field a test wants gone).
 *
 * Every builder takes an overrides object merged over its defaults, so a test
 * states only the field it is about.
 *
 * TYPING. Builders that compose a nested piece return that piece's INIT SHAPE
 * (`MessageInitShape<typeof XSchema>`), not its message type: a plain object
 * literal is an init, and `create()` at the outermost builder turns the whole
 * tree into messages once. Returning message types would force a cast at every
 * nested literal and lose the compiler's field checking, which is the one
 * thing keeping these fixtures honest against the generated code.
 *
 * ARM ENUMERATION. The arm tables below are keyed by the generated oneof CASE
 * names, and `assertCoversOneof` checks a table against the schema descriptor.
 * A new arm landed in the contract therefore fails the suite loudly instead of
 * going quietly untested.
 */
import { create, type DescMessage, type MessageInitShape } from "@bufbuild/protobuf";

import { WorkspaceRefSchema, RepositoryRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { TurnIdSchema } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import { AgentModelSchema, ModelOptionSchema } from "../../../proto/gen/ts/conversation/v1/api_pb";
import { UserSaidSchema } from "../../../proto/gen/ts/conversation/v1/user_pb";
import { SessionCompactScope } from "../../../proto/gen/ts/conversation/v1/session_pb";
import { SessionCommand } from "../../../proto/gen/ts/conversation/v1/slash_command_pb";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";
import {
  FeedIdSchema,
  FeedPageSchema,
  FeedRowSchema,
  FeedTurnActivitySchema,
  type FeedId,
  type FeedPage,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  FooterViewSchema,
  FooterStatusSchema,
  FooterExpandedSchema,
  FooterAllowanceSchema,
  type FooterView,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import {
  TopbarAccountOptionSchema,
  TopbarViewSchema,
  TopbarWarningSchema,
  type TopbarAccountOption,
  type TopbarView,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  WorkspaceRosterSchema,
  RosterRowSchema,
  type RosterRow,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  DaemonHoldTraySchema,
  DaemonHoldItemSchema,
  HeldPromptSchema,
  type DaemonHoldTray,
  type HeldPrompt,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { FailureKindSchema, type FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  WatchHostWorkspaceResponseSchema,
  type WatchHostWorkspaceResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_host_workspace_pb";
import {
  WatchDaemonResponseSchema,
  type WatchDaemonResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import { DrainReasonSchema, type DrainReason } from "../../../proto/gen/ts/agentrepl/v1/drain_reason_pb";
import {
  DaemonHealthResponseSchema,
  DaemonFaultSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_daemon_health_pb";
import {
  SessionHealthResponseSchema,
  SessionFaultSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_session_health_pb";
import {
  SubmitPromptCommandPanelSchema,
  type SubmitPromptCommandPanel,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";

// ---------------------------------------------------------------------------
// Arm enumeration
// ---------------------------------------------------------------------------

/** The generated CASE names of one oneof, read off the schema descriptor. */
export function armsOf(schema: DescMessage, oneofLocalName: string): string[] {
  const oneof = schema.oneofs.find((o) => o.localName === oneofLocalName);
  if (!oneof) {
    throw new Error(
      `${schema.typeName} has no oneof ${JSON.stringify(oneofLocalName)}; it has [${schema.oneofs
        .map((o) => o.localName)
        .join(", ")}]`,
    );
  }
  return oneof.fields.map((f) => f.localName);
}

/**
 * Fail loudly when a fixture table does not cover every arm the schema
 * declares — the mechanism that turns a newly landed arm into a red test.
 */
export function assertCoversOneof(
  schema: DescMessage,
  oneofLocalName: string,
  covered: readonly string[],
): void {
  const declared = armsOf(schema, oneofLocalName).sort();
  const seen = [...new Set(covered)].sort();
  const missing = declared.filter((a) => !seen.includes(a));
  const extra = seen.filter((a) => !declared.includes(a));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `${schema.typeName}.${oneofLocalName}: untested arms [${missing.join(", ")}]` +
        `; unknown arms in table [${extra.join(", ")}]`,
    );
  }
}

/** `create` with the overrides object merged over the defaults, once. */
function make<Desc extends DescMessage>(
  schema: Desc,
  defaults: MessageInitShape<Desc>,
  overrides?: Partial<MessageInitShape<Desc>>,
): ReturnType<typeof create<Desc>> {
  return create(schema, { ...defaults, ...(overrides ?? {}) });
}

// ---------------------------------------------------------------------------
// Init-shape aliases (see the TYPING note above)
// ---------------------------------------------------------------------------

export type RowInit = MessageInitShape<typeof FeedRowSchema>;
export type RowArm = NonNullable<RowInit["row"]>;
export type ActivityUnit = NonNullable<MessageInitShape<typeof FeedTurnActivitySchema>["unit"]>;
export type StatusArm = NonNullable<MessageInitShape<typeof FooterStatusSchema>["status"]>;
export type WarningInit = MessageInitShape<typeof TopbarWarningSchema>;
export type HoldItemInit = MessageInitShape<typeof DaemonHoldItemSchema>;

// ---------------------------------------------------------------------------
// Identity
// ---------------------------------------------------------------------------

export const WORKSPACE_ID = "ws-1";
export const WORKSPACE_DIR = "/tmp/ws-1";

export const workspaceRef = (id = WORKSPACE_ID) => create(WorkspaceRefSchema, { id, dir: `/tmp/${id}` });
export const repositoryRef = (id = "repo-1") => create(RepositoryRefSchema, { id, dir: `/src/${id}` });
export const feedId = (value: string): FeedId => create(FeedIdSchema, { value });
export const turnId = (value = "turn-1") => create(TurnIdSchema, { value });
export const agentModel = (name = "opus") => create(AgentModelSchema, { name });
export const modelOption = (name = "opus", displayName = "Opus", description = "the big one") =>
  create(ModelOptionSchema, { model: agentModel(name), displayName, description });
export const userSaid = (text = "hello") =>
  create(UserSaidSchema, { content: { blocks: [{ block: { case: "text", value: { text } } }] } });

/** The origin every webapp submission carries; re-exported so suites assert it. */
export const WEBAPP_ORIGIN = PromptOrigin.WEBAPP_USER_SENT;
export const CARD_ACTION_ORIGIN = PromptOrigin.WEBAPP_CARD_ACTION;

// ---------------------------------------------------------------------------
// Feed rows
// ---------------------------------------------------------------------------

/** A FeedRow wrapping `row`, with id and turn defaulted and overridable. */
export function feedRow(row: RowArm, overrides?: Partial<RowInit>): FeedRow {
  return make(FeedRowSchema, { id: feedId("row-1"), turn: turnId(), row }, overrides);
}

/** An activity row: the FeedTurnActivity wrapper around one unit. */
export function activityRow(unit: ActivityUnit, overrides?: Partial<RowInit>): FeedRow {
  return feedRow({ case: "activity", value: { unit } }, overrides);
}

export const userPromptRow = (text = "do the thing", overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    {
      case: "userPrompt",
      value: {
        author: { label: "you" },
        result: {
          case: "success",
          value: { body: { blocks: [{ block: { case: "text", value: { text } } }] } },
        },
      },
    },
    overrides,
  );

export const agentPromptRow = (text = "please review", overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    {
      case: "agentPrompt",
      value: {
        address: { text: "to reviewer" },
        body: { blocks: [{ block: { case: "text", value: { text } } }] },
      },
    },
    overrides,
  );

// ---- FeedResponse ---------------------------------------------------------

export const RESPONSE_STATES = ["update", "success", "error"] as const;
export type ResponseState = (typeof RESPONSE_STATES)[number];

/** The heading a vendor-synthesized notice carries; suites echo it verbatim. */
export const RESPONSE_NOTICE_HEADING = "the turn was interrupted";

export const responseUnit = (
  state: ResponseState,
  markdown = "the answer",
  usageText = "1.2k in / 340 out",
  /** SET = the prose is a vendor notice, drawn in the notice register. */
  notice?: string,
): ActivityUnit => ({
  case: "response",
  value: {
    usage: { text: usageText },
    result: { case: state, value: { prose: { markdown } } },
    notice: notice === undefined ? undefined : { heading: notice },
  },
});

/** A response bubble in the NOTICE register: vendor-synthesized, not the agent's. */
export const responseNoticeUnit = (
  state: ResponseState = "success",
  heading = RESPONSE_NOTICE_HEADING,
): ActivityUnit => responseUnit(state, "the vendor's remark", undefined, heading);

export const responseRow = (
  state: ResponseState,
  markdown?: string,
  overrides?: Partial<RowInit>,
): FeedRow => activityRow(responseUnit(state, markdown), overrides);

// ---- FeedSimpleToolCall ---------------------------------------------------

export const TOOL_OUTPUT_FORMS = ["text", "code", "diff", "lines", "links", "image", "none"] as const;
export type ToolOutputForm = (typeof TOOL_OUTPUT_FORMS)[number];

/** The output forms that carry an omission note; `text`/`diff`/`image`/`none` do not. */
export const TOOL_OUTPUT_FORMS_WITH_OMISSION = ["code", "lines", "links"] as const;

/**
 * THE DRAWN FORM of the input line. The daemon states it because only it knows
 * the tool; the client applies a treatment and still knows no tool. UNSET is a
 * legal fourth state meaning plain text, so it is exercised alongside the arms.
 */
export const TOOL_INPUT_FORMS = ["command", "path", "query"] as const;
export type ToolInputForm = (typeof TOOL_INPUT_FORMS)[number];

type ToolInputInit = NonNullable<Extract<ActivityUnit, { case: "simpleToolCall" }>["value"]["input"]>;

/**
 * The ONE input builder every tool-call fixture uses, so the four form states
 * are stated in a single place rather than inline at each call site.
 */
export function toolCallInput(init?: {
  text?: string;
  form?: ToolInputForm;
  link?: boolean;
}): ToolInputInit {
  return {
    text: init?.text ?? "npm test",
    link: init?.link ? { url: "https://example.test/run" } : undefined,
    form: init?.form ? { case: init.form, value: {} } : undefined,
  };
}

export const TOOL_VERDICTS = ["succeeded", "failed"] as const;
export const DIFF_LINE_KINDS = ["header", "added", "removed", "context"] as const;

/**
 * The code output's spans exercise every paint-class case at once: a syntax
 * name, an ansi name, the empty string (plain), and a name in neither
 * vocabulary list (unstyled, with a warning, never an error).
 */
export const CODE_SPANS = [
  { text: "const ", paintClass: "keyword" },
  { text: "red ", paintClass: "ansi-fg-red" },
  { text: "plain ", paintClass: "" },
  { text: "alien ", paintClass: "not-a-real-class" },
] as const;

type ToolCallArm = Extract<ActivityUnit, { case: "simpleToolCall" }>;
type ToolCallOutcome = NonNullable<ToolCallArm["value"]["outcome"]>;

type ToolOutputFormInit = NonNullable<
  Extract<ToolCallOutcome, { case: "returned" }>["value"]["form"]
>;

/** The src the image output fixture carries; suites assert it verbatim. */
export const TOOL_IMAGE_SRC = "data:image/png;base64,iVBORw0KGgo=";

/** The alt the image output fixture carries; suites assert it verbatim. */
export const TOOL_IMAGE_ALT = "screenshot.png";

const toolOutputForm = (form: ToolOutputForm): ToolOutputFormInit => {
  switch (form) {
    case "text":
      return { case: "text" as const, value: { text: "plain output" } };
    case "code":
      return {
        case: "code" as const,
        value: { spans: CODE_SPANS.map((s) => ({ ...s })), omitted: { text: "12 lines omitted" } },
      };
    case "diff":
      return {
        case: "diff" as const,
        value: {
          lines: [
            { kind: { case: "header" as const, value: {} }, text: "@@ -1,3 +1,4 @@" },
            { kind: { case: "added" as const, value: {} }, text: "+ added line" },
            { kind: { case: "removed" as const, value: {} }, text: "- removed line" },
            { kind: { case: "context" as const, value: {} }, text: "  context line" },
          ],
        },
      };
    case "lines":
      return {
        case: "lines" as const,
        value: { lines: ["first line", "second line"], omitted: { text: "3 more" } },
      };
    case "links":
      return {
        case: "links" as const,
        value: {
          links: [
            { text: "the docs", url: { url: "https://example.test/docs" } },
            { text: "no url here" },
          ],
          omitted: { text: "1 more link" },
        },
      };
    case "image":
      // A call that answered with a PICTURE rather than characters — a
      // screenshot, a rendered chart. The `src` is the daemon's resolution of
      // the record's reference, so the fixture states one a browser could
      // actually load rather than a placeholder path.
      return {
        case: "image" as const,
        value: { src: TOOL_IMAGE_SRC, alt: TOOL_IMAGE_ALT },
      };
    case "none":
      // A call that returned nothing to show. The card still draws its input
      // line, its verdict and its runtime; there is simply no output section.
      return { case: "none" as const, value: {} };
  }
};

export const toolCallRunningUnit = (
  lastProgressAtMs = 1_000n,
  form?: ToolInputForm,
): ActivityUnit => ({
  case: "simpleToolCall",
  value: {
    name: { text: "Bash" },
    input: toolCallInput({ form }),
    outcome: { case: "running", value: { lastProgress: { atMs: lastProgressAtMs } } },
  },
});

export const toolCallReturnedUnit = (
  form: ToolOutputForm,
  verdict: (typeof TOOL_VERDICTS)[number] = "succeeded",
  inputForm?: ToolInputForm,
): ActivityUnit => {
  const outcome: ToolCallOutcome = {
    case: "returned",
    value: {
      verdict: { case: verdict, value: {} },
      form: toolOutputForm(form),
      runtime: { text: "ran 4.2 s" },
      diagnostics: { lines: ["one warning"] },
    },
  };
  return {
    case: "simpleToolCall",
    value: {
      name: { text: "Bash" },
      input: toolCallInput({ form: inputForm, link: true }),
      outcome,
    },
  };
};

export const toolCallDeniedUnit = (): ActivityUnit => ({
  case: "simpleToolCall",
  value: {
    name: { text: "Bash" },
    input: toolCallInput({ text: "rm -rf /" }),
    outcome: { case: "denied", value: {} },
  },
});

// ---- FeedSkill ------------------------------------------------------------

export const SKILL_OUTCOMES = ["running", "loaded", "failed", "denied"] as const;
export type SkillOutcome = (typeof SKILL_OUTCOMES)[number];

export const skillUnit = (outcome: SkillOutcome): ActivityUnit => ({
  case: "skill",
  value: {
    invocation: { text: "/graphify" },
    outcome:
      outcome === "loaded"
        ? {
            case: "loaded",
            value: { document: { markdown: "# graphify" }, allowances: { text: "Bash, Read" } },
          }
        : outcome === "failed"
          ? { case: "failed", value: { text: "skill file unreadable" } }
          : { case: outcome, value: {} },
  },
});

// ---- FeedHook -------------------------------------------------------------

export const HOOK_OUTCOMES = ["blocked", "failed"] as const;
export type HookOutcome = (typeof HOOK_OUTCOMES)[number];

export const hookUnit = (outcome: HookOutcome): ActivityUnit => ({
  case: "hook",
  value: {
    headline: { text: "PreToolUse hook" },
    gatedCall: { row: feedId("tool-row") },
    outcome:
      outcome === "blocked"
        ? { case: "blocked", value: { reason: "writes outside the worktree are refused" } }
        : { case: "failed", value: { exitCode: 2, output: { text: "hook stderr" } } },
  },
});

// ---- FeedArtifact / FeedPlan / FeedFindings -------------------------------

export const ARTIFACT_STATES = ["publishing", "published", "failed"] as const;
export type ArtifactState = (typeof ARTIFACT_STATES)[number];

export const artifactUnit = (state: ArtifactState): ActivityUnit => ({
  case: "artifact",
  value: {
    heading: { text: "Release notes" },
    state:
      state === "published"
        ? { case: "published", value: { url: { url: "https://example.test/a" } } }
        : state === "failed"
          ? { case: "failed", value: { text: "publish refused" } }
          : { case: "publishing", value: {} },
  },
});

export const PLAN_STATES = ["planning", "planned", "failed"] as const;
export type PlanState = (typeof PLAN_STATES)[number];

/** The path the planned plan's edit link opens; the suites echo it verbatim. */
export const PLAN_EDIT_PATH = "/repo/PLAN.md";

export const planUnit = (state: PlanState): ActivityUnit => ({
  case: "plan",
  value: {
    state:
      state === "planned"
        ? { case: "planned", value: { prose: { markdown: "1. do it" }, edit: { path: PLAN_EDIT_PATH } } }
        : state === "failed"
          ? { case: "failed", value: { text: "planning refused" } }
          : { case: "planning", value: {} },
  },
});

export const FINDINGS_VERDICTS = ["confirmed", "plausible"] as const;
export const FINDINGS_OUTCOMES = ["fixed", "skipped", "noChange"] as const;

/** The first findings row's location: the one that carries a line number. */
export const FINDINGS_LOCATION = { text: "feed.ts:42", path: "/repo/src/feed.ts", line: 42 } as const;
/** The third row's location has NO line — the client must omit it from the rpc. */
export const FINDINGS_LOCATION_NO_LINE = { text: "row.ts", path: "/repo/src/row.ts" } as const;

export const findingsUnit = (): ActivityUnit => ({
  case: "findings",
  value: {
    heading: { text: "Review findings" },
    rows: [
      {
        verdict: { case: "confirmed", value: {} },
        category: { text: "correctness" },
        location: { ...FINDINGS_LOCATION },
        summary: { text: "the id is parsed" },
        scenario: { text: "when a sub-feed row arrives" },
        outcome: { case: "fixed", value: {} },
      },
      {
        verdict: { case: "plausible", value: {} },
        category: { text: "style" },
        location: { text: "log.ts:7", path: "/repo/src/log.ts", line: 7 },
        summary: { text: "the message is vague" },
        scenario: { text: "on a warn" },
        outcome: { case: "skipped", value: {} },
      },
      {
        verdict: { case: "confirmed", value: {} },
        category: { text: "perf" },
        location: { ...FINDINGS_LOCATION_NO_LINE },
        summary: { text: "already handled" },
        scenario: { text: "on a burst" },
        outcome: { case: "noChange", value: {} },
      },
    ],
  },
});

// ---- FeedSubagent / FeedShell ---------------------------------------------

export const SUBAGENT_OUTCOMES = ["succeeded", "failed", "cancelled", "lost"] as const;
export type SubagentOutcome = (typeof SUBAGENT_OUTCOMES)[number];

export const subagentUnit = (state: "live" | SubagentOutcome): ActivityUnit => ({
  case: "subagent",
  value: {
    label: { text: "reviewer" },
    description: { text: "review the diff" },
    tokens: { text: "12.4k" },
    runtime: { startedAtMs: 1_000n },
    state:
      state === "live"
        ? { case: "live", value: { lastProgress: { atMs: 2_000n } } }
        : { case: "settled", value: { endedAtMs: 9_000n, outcome: { case: state, value: {} } } },
  },
});

export const SHELL_OUTCOMES = ["completed", "cancelled", "lost"] as const;
export type ShellOutcome = (typeof SHELL_OUTCOMES)[number];

const shellInit = (state: "live" | ShellOutcome) => ({
  command: { text: "npm run watch" },
  runtime: { startedAtMs: 1_000n },
  spool: { text: "building...", omitted: { text: "40 lines omitted" } },
  state:
    state === "live"
      ? { case: "live" as const, value: { lastProgress: { atMs: 2_000n } } }
      : {
          case: "settled" as const,
          value: { endedAtMs: 9_000n, exit: { code: 1 }, outcome: { case: state, value: {} } },
        },
});

/**
 * A MESSAGE FROM ANOTHER CLAUDE — `FeedRow.peer_message`. Not a prompt: drawn
 * on the prompt's side of the feed but purple, and ABBREVIATED (the collapsed
 * form is the sender label and a chevron, with no body).
 */
export const peerMessageRow = (
  overrides?: Partial<RowInit>,
): FeedRow =>
  feedRow(
    { case: "peerMessage", value: { sender: "agent Explore", body: "the sweep found nothing" } },
    overrides,
  );

/**
 * A REMOVAL — `FeedRow.removed`, the DUAL of an upsert on the same tail: the
 * daemon retired the row this one keys, so the client drops it rather than
 * drawing anything. The arm carries no payload.
 */
export const removedRow = (overrides?: Partial<RowInit>): FeedRow =>
  feedRow({ case: "removed", value: {} }, overrides);

export const detachedSubagentRow = (
  state: "live" | SubagentOutcome = "live",
  overrides?: Partial<RowInit>,
): FeedRow => {
  const unit = subagentUnit(state) as Extract<ActivityUnit, { case: "subagent" }>;
  return feedRow({ case: "detachedSubagent", value: { subagent: unit.value } }, overrides);
};

/**
 * The shell bubble's BODY row — the spool alone, on the shell's own sub-feed.
 *
 * feed.proto: `FeedRow.detached_shell` is the spool BODY. The command, clock,
 * stop and outcome live on the HEAD (`shellHeadRow`), so only the spool is
 * drawn from this arm.
 */
export const detachedShellRow = (
  state: "live" | ShellOutcome = "live",
  overrides?: Partial<RowInit>,
): FeedRow => feedRow({ case: "detachedShell", value: { shell: shellInit(state) } }, overrides);

/**
 * The shell bubble's HEAD row — the command, the clock and, while live, the
 * stop, carried on the PARENT feed (`FeedRow.shell_head`).
 *
 * The SAME `FeedShell` message feeds both arms; which half is drawn is the
 * arm's business, not the message's. Every head fact — `data-state`, the
 * clock, the quiet-for figure, the exit chip, the stop control — is asserted
 * against THIS row, never against the spool-only body.
 */
export const shellHeadRow = (
  state: "live" | ShellOutcome = "live",
  overrides?: Partial<RowInit>,
): FeedRow => feedRow({ case: "shellHead", value: shellInit(state) }, overrides);

// ---- FeedTurnEnded --------------------------------------------------------

export const TURN_ERROR_ARMS = [
  "rateLimited",
  "overloaded",
  "authenticationFailed",
  "permissionDenied",
  "invalidRequest",
  "requestTooLarge",
  "notFound",
  "internal",
  "vendorUnmodeled",
  "maxTokens",
  "refusal",
  "queryDied",
  "billingError",
  "modelNotFound",
  "oauthOrgNotAllowed",
  "maxOutputTokens",
  "maxTurns",
  "maxBudget",
  "executionError",
  "turnFailed",
  "stopHookPrevented",
] as const;
export type TurnErrorArm = (typeof TURN_ERROR_ARMS)[number];

/** The two arms that ship a retry deadline the client counts down to. */
export const RETRYING_TURN_ERROR_ARMS = ["rateLimited", "overloaded"] as const;

/**
 * THE DAEMON'S OWN HEADLINE PER ARM.
 *
 * `headline` is required and the client holds NO per-arm sentence table: it
 * draws this verbatim. So the suites assert these strings, and the two
 * token-limit arms read differently here because the DAEMON composed them
 * differently — not because the renderer knows what the arms mean. Only the
 * retry countdown is client-ticked.
 */
export const TURN_ERROR_HEADLINES: Record<TurnErrorArm, string> = {
  rateLimited: "rate limited",
  overloaded: "the vendor is overloaded",
  authenticationFailed: "authentication failed",
  permissionDenied: "the vendor refused this account",
  invalidRequest: "the request was rejected",
  requestTooLarge: "the request was too large",
  notFound: "the vendor found nothing at that address",
  internal: "the vendor failed internally",
  vendorUnmodeled: "the vendor returned something unmodeled",
  maxTokens: "cut short mid-answer at the context ceiling",
  refusal: "the model declined to answer",
  queryDied: "the query died",
  billingError: "billing refused the request",
  modelNotFound: "that model does not exist",
  oauthOrgNotAllowed: "this organization is not allowed",
  maxOutputTokens: "refused outright at the output ceiling",
  maxTurns: "the run reached the turn ceiling",
  maxBudget: "the run reached its budget",
  executionError: "the run broke while executing",
  turnFailed: "the turn ended abnormally",
  stopHookPrevented: "a Stop hook forbade the stop",
};

/** The vendor conversation the run's own terminals name as their context. */
const VENDOR_FAILURE_CONTEXT = {
  vendorSessionId: "vendor-session-1",
  requestId: "req-1",
} as const;

type TurnEndedValue = Extract<RowArm, { case: "turnEnded" }>["value"];
type TurnEndedOutcome = NonNullable<TurnEndedValue["outcome"]>;

const turnErrorValue = (arm: TurnErrorArm, retryAfterMs?: bigint) => {
  if (arm === "rateLimited" || arm === "overloaded") {
    return { case: arm, value: { retryAfterMs: retryAfterMs ?? 30_000n } };
  }
  if (arm === "vendorUnmodeled") return { case: arm, value: { type: "vendor_teapot" } };
  // The run's own terminals import failure.proto's evidence messages, so they
  // carry the vendor context rather than being empty (Landing 8).
  if (arm === "turnFailed") {
    return { case: arm, value: { vendor: VENDOR_FAILURE_CONTEXT, stopReason: "structured_output_retry_exhausted" } };
  }
  if (arm === "maxTurns" || arm === "maxBudget" || arm === "executionError") {
    return { case: arm, value: { vendor: VENDOR_FAILURE_CONTEXT } };
  }
  return { case: arm, value: {} };
};

export const turnEndedConcludedRow = (answer: FeedId, overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    { case: "turnEnded", value: { endedAtMs: 9_000n, outcome: { case: "concluded", value: { answer } } } },
    overrides,
  );

export const turnEndedErroredRow = (
  arm: TurnErrorArm,
  init?: { retryAfterMs?: bigint; message?: string; headline?: string },
  overrides?: Partial<RowInit>,
): FeedRow => {
  const outcome = {
    case: "errored",
    value: {
      message: { text: init?.message ?? `the turn failed: ${arm}` },
      headline: { text: init?.headline ?? TURN_ERROR_HEADLINES[arm] },
      error: turnErrorValue(arm, init?.retryAfterMs),
    },
  } as TurnEndedOutcome;
  return feedRow({ case: "turnEnded", value: { endedAtMs: 9_000n, outcome } }, overrides);
};

export const turnEndedInterruptedRow = (overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    { case: "turnEnded", value: { endedAtMs: 9_000n, outcome: { case: "interrupted", value: {} } } },
    overrides,
  );

// ---- FeedPermission -------------------------------------------------------

export const PERMISSION_ANSWERS = [
  "allowedOnce",
  "allowedStanding",
  "deniedByUser",
  "deniedByPolicy",
  "deniedUndecidable",
] as const;
export type PermissionAnswer = (typeof PERMISSION_ANSWERS)[number];

type PermissionValue = Extract<RowArm, { case: "permission" }>["value"];
type PermissionState = NonNullable<PermissionValue["state"]>;

export function permissionRow(
  state: "open" | "abandoned" | PermissionAnswer,
  init?: { standingOffered?: boolean },
  overrides?: Partial<RowInit>,
): FeedRow {
  const answered = (PERMISSION_ANSWERS as readonly string[]).includes(state);
  const resolved: PermissionState = answered
    ? {
        case: "answered",
        value: {
          atMs: 5_000n,
          answer:
            state === "deniedByPolicy"
              ? { case: "deniedByPolicy", value: { text: "policy forbids it" } }
              : state === "deniedUndecidable"
                ? {
                    case: "deniedUndecidable",
                    value: { text: "denied for want of a decider" },
                  }
                : {
                    case: state as Exclude<
                      PermissionAnswer,
                      "deniedByPolicy" | "deniedUndecidable"
                    >,
                    value: {},
                  },
        },
      }
    : state === "abandoned"
      ? { case: "abandoned", value: { atMs: 5_000n } }
      : { case: "open", value: {} };
  return feedRow(
    {
      case: "permission",
      value: {
        headline: { text: "Bash wants to run" },
        subtitle: { text: "npm test" },
        trigger: { text: "requested by the reviewer subagent" },
        arguments: { lines: ["cwd=/repo", "timeout=120s"] },
        standingOffered: init?.standingOffered === false ? undefined : {},
        state: resolved,
      },
    },
    overrides,
  );
}

// ---- FeedQuestion ---------------------------------------------------------

export const QUESTION_STATES = ["open", "answered", "expired"] as const;
export type QuestionState = (typeof QUESTION_STATES)[number];

/** The two questions every question fixture poses; suites echo these verbatim. */
export const QUESTION_ONE = {
  header: "Scope",
  text: "How far should the port go?",
  options: ["whole app", "the feed only"],
} as const;
export const QUESTION_TWO = {
  header: "Suites",
  text: "Which suites run?",
  options: ["unit", "integration"],
} as const;

export function questionRow(state: QuestionState, overrides?: Partial<RowInit>): FeedRow {
  return feedRow(
    {
      case: "question",
      value: {
        questions: [
          {
            header: { text: QUESTION_ONE.header },
            text: { text: QUESTION_ONE.text },
            options: {
              case: "singleSelect",
              value: {
                options: [
                  { label: { text: QUESTION_ONE.options[0] }, description: { text: "every component" } },
                  { label: { text: QUESTION_ONE.options[1] }, description: { text: "one component" } },
                ],
              },
            },
          },
          {
            header: { text: QUESTION_TWO.header },
            text: { text: QUESTION_TWO.text },
            options: {
              case: "multiSelect",
              value: {
                options: [
                  { label: { text: QUESTION_TWO.options[0] }, description: { text: "vitest" } },
                  { label: { text: QUESTION_TWO.options[1] }, description: { text: "the fake daemon" } },
                ],
              },
            },
          },
        ],
        state:
          state === "answered"
            ? {
                case: "answered",
                value: {
                  atMs: 5_000n,
                  answers: [
                    {
                      header: { text: QUESTION_ONE.header },
                      chosen: [QUESTION_ONE.options[0]],
                      otherText: { text: "and the docs" },
                    },
                    { header: { text: QUESTION_TWO.header }, chosen: [...QUESTION_TWO.options] },
                  ],
                },
              }
            : state === "expired"
              ? { case: "expired", value: { atMs: 5_000n } }
              : { case: "open", value: {} },
      },
    },
    overrides,
  );
}

// ---- FeedSessionSeparation ------------------------------------------------

export const SEPARATION_ARMS = [
  "cleared",
  "compacted",
  "worktreeEntered",
  "worktreeLeft",
  "compactionFailed",
] as const;
export type SeparationArm = (typeof SEPARATION_ARMS)[number];

/** The producer's account of a compaction that did not happen, echoed verbatim. */
export const COMPACTION_FAILED_ERROR = "the summarizing request was refused";

/** The path the worktree divider's link opens; the suites echo it verbatim. */
export const WORKTREE_PATH = "/repo/wt";

type SeparationValue = Extract<RowArm, { case: "separation" }>["value"];
type SeparationKind = NonNullable<SeparationValue["kind"]>;

const separationKind = (arm: SeparationArm): SeparationKind => {
  switch (arm) {
    case "cleared":
      return { case: "cleared", value: {} };
    case "compacted":
      return {
        case: "compacted",
        value: {
          summary: { markdown: "the session so far" },
          fold: { folded: true },
          coldRead: { evidence: { uncachedInputTokens: 40_000n } },
        },
      };
    case "worktreeEntered":
      return {
        case: "worktreeEntered",
        value: { path: { text: WORKTREE_PATH }, branch: { text: "feature/x" } },
      };
    case "worktreeLeft":
      return {
        case: "worktreeLeft",
        value: { outcome: { case: "kept", value: { path: { text: WORKTREE_PATH } } } },
      };
    case "compactionFailed":
      return { case: "compactionFailed", value: { error: COMPACTION_FAILED_ERROR } };
  }
};

export const separationRow = (arm: SeparationArm, overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    {
      case: "separation",
      value: {
        label: { text: `separation: ${arm}` },
        kind: separationKind(arm),
        // A compaction that did not happen cut nothing: `tokens` is UNSET on
        // that arm, as the schema states.
        tokens: arm === "compactionFailed" ? undefined : { beforeText: "180k", afterText: "12k" },
      },
    },
    overrides,
  );

/** The worktree-left `removed` outcome, the one nested arm the table above skips. */
export const worktreeRemovedRow = (overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    {
      case: "separation",
      value: {
        label: { text: "worktree removed" },
        kind: {
          case: "worktreeLeft",
          value: { outcome: { case: "removed", value: { discarded: { text: "2 uncommitted files" } } } },
        },
        tokens: { beforeText: "180k", afterText: "12k" },
      },
    },
    overrides,
  );

// ---- FeedColdGate ---------------------------------------------------------

export const COLD_GATE_CHOICES = ["pay", "clear", "compact"] as const;
export type ColdGateChoice = (typeof COLD_GATE_CHOICES)[number];

/** The cold gate's served facts — the ONE card the client formats itself. */
export const COLD_GATE_TOKENS = 184_320n;
export const COLD_GATE_LAST_REQUEST_MS = 1_000n;
export const COLD_GATE_MODELS = ["opus", "haiku"] as const;
export const COLD_GATE_SCOPES = [
  SessionCompactScope.ALL,
  SessionCompactScope.PROMPTS,
  SessionCompactScope.RESPONSES,
] as const;

export const coldGateStandingRow = (
  init?: { contextTokens?: bigint; lastRequestAtMs?: bigint },
  overrides?: Partial<RowInit>,
): FeedRow =>
  feedRow(
    {
      case: "coldGate",
      value: {
        state: {
          case: "standing",
          value: {
            contextTokens: { tokens: init?.contextTokens ?? COLD_GATE_TOKENS },
            lastRequest: { atMs: init?.lastRequestAtMs ?? COLD_GATE_LAST_REQUEST_MS },
            model: { model: agentModel(COLD_GATE_MODELS[0]) },
            compact: {
              models: COLD_GATE_MODELS.map((m) => ({ model: agentModel(m) })),
              scopes: [...COLD_GATE_SCOPES],
            },
          },
        },
      },
    },
    overrides,
  );

export const coldGateResolvedRow = (
  choice: ColdGateChoice,
  init?: { scope?: SessionCompactScope },
  overrides?: Partial<RowInit>,
): FeedRow =>
  feedRow(
    {
      case: "coldGate",
      value: {
        state: {
          case: "resolved",
          value: {
            atMs: 5_000n,
            choice:
              choice === "compact"
                ? {
                    case: "compact",
                    value: {
                      model: { model: agentModel("haiku") },
                      scope: init?.scope ?? SessionCompactScope.ALL,
                    },
                  }
                : { case: choice, value: {} },
          },
        },
      },
    },
    overrides,
  );

// ---- FeedCommandPanel / FeedCommandRefused (synthesized, non-durable) -----

export const FEED_COMMAND_PANEL_ARMS = ["status", "todos", "mcp", "context"] as const;
export type FeedCommandPanelArm = (typeof FEED_COMMAND_PANEL_ARMS)[number];

type CommandPanelValue = Extract<RowArm, { case: "commandPanel" }>["value"];
type CommandPanelArmInit = NonNullable<CommandPanelValue["panel"]>;

/**
 * A command-panel row. These are SYNTHESIZED and NON-DURABLE: the daemon puts
 * them on the live tail only, so a page fixture must never carry one.
 */
export const commandPanelRow = (arm: FeedCommandPanelArm, overrides?: Partial<RowInit>): FeedRow =>
  feedRow({ case: "commandPanel", value: { panel: panelValue(arm) as CommandPanelArmInit } }, overrides);

/** The command text the refused card carries; RequestCommandSupport echoes it. */
export const REFUSED_COMMAND = "/agents";

export const commandRefusedRow = (
  init?: { command?: string; reason?: string; addSupport?: boolean },
  overrides?: Partial<RowInit>,
): FeedRow =>
  feedRow(
    {
      case: "commandRefused",
      value: {
        command: { text: init?.command ?? REFUSED_COMMAND },
        reason: { text: init?.reason ?? "recognized, but not supported this wave" },
        addSupport: init?.addSupport === false ? undefined : {},
      },
    },
    overrides,
  );

// ---- FeedMerge and FeedMergeTab -------------------------------------------

export const MERGE_TAB_KINDS = [
  "queue",
  "prePrompt",
  "merge",
  "conflicts",
  "tests",
  "fixes",
  "postPrompt",
] as const;
export type MergeTabKind = (typeof MERGE_TAB_KINDS)[number];

/** Which state arms each tab kind legally carries; `parked` only on two. */
export const MERGE_TAB_STATES: Record<MergeTabKind, readonly ("live" | "parked" | "settled")[]> = {
  queue: ["live", "settled"],
  prePrompt: ["live", "settled"],
  merge: ["live", "settled"],
  conflicts: ["live", "parked", "settled"],
  tests: ["live", "settled"],
  fixes: ["live", "parked", "settled"],
  postPrompt: ["live", "settled"],
};

const liveState = () => ({ case: "live" as const, value: {} });
const settledState = () => ({
  case: "settled" as const,
  value: { endedAtMs: 9_000n, outcome: { case: "succeeded" as const, value: {} } },
});
const parkedState = () => ({
  case: "parked" as const,
  value: { line: { text: "paused for your answer" } },
});

/** The state arms every tab carries; `parked` is legal on conflicts/fixes only. */
const plainState = (state: "live" | "parked" | "settled") =>
  state === "settled" ? settledState() : liveState();
const parkableState = (state: "live" | "parked" | "settled") =>
  state === "settled" ? settledState() : state === "parked" ? parkedState() : liveState();

/** The tests tab's suites: one passed, one failed, one running, painted spans. */
export const MERGE_TEST_SPANS = [
  { text: "PASS ", paintClass: "ansi-fg-green" },
  { text: "24 tests", paintClass: "" },
] as const;

type MergeTabValue = Extract<RowArm, { case: "mergeTab" }>["value"];
type MergeTabKindInit = NonNullable<MergeTabValue["kind"]>;

const mergeTabKindValue = (
  kind: MergeTabKind,
  state: "live" | "parked" | "settled",
): MergeTabKindInit => {
  switch (kind) {
    case "queue":
      return {
        case: "queue",
        value: {
          state: plainState(state),
          queue: {
            ahead: [
              {
                workspace: { ref: workspaceRef("ws-ahead") },
                label: { text: "ws-ahead" },
                status: { case: "waiting", value: {} },
              },
            ],
            current: {
              workspace: { ref: workspaceRef() },
              label: { text: "ws-1" },
              status: { case: "merging", value: { activeTab: { text: "tests", round: 2 } } },
            },
            behind: [
              {
                workspace: { ref: workspaceRef("ws-behind") },
                label: { text: "ws-behind" },
                status: { case: "waiting", value: {} },
              },
            ],
          },
        },
      };
    case "merge":
      return {
        case: "merge",
        value: {
          state: plainState(state),
          lines: [{ text: "cherry-picked 3 commits" }, { text: "no conflicts" }],
        },
      };
    case "tests":
      return {
        case: "tests",
        value: {
          state: plainState(state),
          suites: [
            {
              name: "vitest",
              state: { case: "passed", value: {} },
              output: MERGE_TEST_SPANS.map((s) => ({ ...s })),
            },
            {
              name: "ert",
              state: { case: "failed", value: {} },
              output: [{ text: "FAIL ", paintClass: "ansi-fg-red" }],
            },
            {
              name: "go",
              state: { case: "running", value: {} },
              output: [{ text: "running", paintClass: "ansi-dim" }],
            },
          ],
        },
      };
    case "conflicts":
      return { case: "conflicts", value: { state: parkableState(state) } };
    case "fixes":
      return { case: "fixes", value: { state: parkableState(state) } };
    case "prePrompt":
      return { case: "prePrompt", value: { state: plainState(state) } };
    case "postPrompt":
      return { case: "postPrompt", value: { state: plainState(state) } };
  }
};

export const mergeTabRow = (
  kind: MergeTabKind,
  state: "live" | "parked" | "settled" = "live",
  overrides?: Partial<RowInit>,
): FeedRow =>
  feedRow(
    {
      case: "mergeTab",
      value: {
        label: { text: kind, round: kind === "tests" ? 2 : 1 },
        kind: mergeTabKindValue(kind, state),
      },
    },
    overrides,
  );

export const MERGE_RESULTS = ["update", "success", "failed", "abandoned"] as const;
export type MergeResult = (typeof MERGE_RESULTS)[number];

export const mergeUnit = (result: MergeResult): ActivityUnit => ({
  case: "merge",
  value: {
    head: {
      glyph: { icon: "merge" },
      label: { text: "merging ws-1" },
      runtime: { startedAtMs: 1_000n },
      // R2: the INITIAL fold. A bubble arrives collapsed, like every other.
      fold: { folded: true },
    },
    result:
      result === "update"
        ? { case: "update", value: {} }
        : result === "success"
          ? { case: "success", value: { endedAtMs: 9_000n, commit: "abc1234" } }
          : {
              case: "error",
              value: {
                endedAtMs: 9_000n,
                reason:
                  result === "failed"
                    ? { case: "failed", value: { summary: "tests failed twice" } }
                    : { case: "abandoned", value: { summary: "taken off the queue" } },
              },
            },
  },
});

// ---- pages ----------------------------------------------------------------

export function feedPageSuccess(
  rows: FeedRow[],
  init?: { edge?: "hasMore" | "atStart"; breadcrumbs?: { label: string; target: string }[] },
): FeedPage {
  return create(FeedPageSchema, {
    result: {
      case: "success",
      value: {
        rows,
        edge: { case: init?.edge ?? "atStart", value: {} },
        // ALWAYS PRESENT, EVEN WHEN EMPTY. `breadcrumbs` is a non-optional
        // message on the wire, so a page without a trail carries an EMPTY
        // trail, not an absent one — the daemon sends it that way and the
        // strict check refuses the absent form as a malformed view.
        breadcrumbs: {
          crumbs: (init?.breadcrumbs ?? []).map((c) => ({ target: feedId(c.target), label: c.label })),
        },
      },
    },
  });
}

export const FEED_PAGE_ERROR_ARMS = ["historyReplayTruncated"] as const;

export function feedPageError(): FeedPage {
  return create(FeedPageSchema, {
    result: {
      case: "error",
      value: {
        // The tone vocabulary is render-colors.json#topbar_tones; "warn" was a
        // word from before the colors were the vocabulary.
        headline: { text: "history could not be replayed", tone: "yellow" },
        kind: {
          case: "historyReplayTruncated",
          value: { fromSeq: 10n, stopAtSeq: 90n, delivered: 40n, reason: "store gap" },
        },
      },
    },
  });
}

/** An empty page, used as the fake daemon's default answer. */
export const emptyFeedPage = (): FeedPage => feedPageSuccess([]);

// ---------------------------------------------------------------------------
// Footer
// ---------------------------------------------------------------------------

/** Every status arm, with every legal substatus arm under it. */
export const FOOTER_STATUS_SUBSTATUSES: Record<string, readonly string[]> = {
  idle: ["ready", "done"],
  thinking: ["submitting", "thinking", "clearing", "compacting"],
  waiting: ["wakeup", "permission", "question", "coldGate", "interrupting"],
  interrupted: ["byUser", "hostShutdown"],
  merging: [
    "enqueuing",
    "queued",
    "prePrompt",
    "merge",
    "testing",
    "parked",
    "postPrompt",
    "failed",
    "merged",
    "fixes",
    "conflicts",
  ],
  background: [],
  blocked: ["auth", "usageLimit", "vendorError", "billing", "queryDied"],
  disconnected: ["starting", "degraded", "severed", "dead", "startFailed"],
  closing: ["blocked"],
  loading: ["memory", "invoked", "discovered", "listing"],
};

export const FOOTER_STATUS_ARMS = Object.keys(FOOTER_STATUS_SUBSTATUSES);

/** The one status arm with no substatus of its own: it merges into the cell. */
export const FOOTER_STATUS_WITHOUT_SUBSTATUS = "background";

/** The substatus arms that carry payload; everything else is empty. */
const substatusValue = (substatus: string): object => {
  if (substatus === "queued") return { position: 2, depth: 5 };
  if (substatus === "parked") return { line: "waiting on your answer" };
  return {};
};

/** The allowance verdicts; the free-text status string was retired. */
export const FOOTER_ALLOWANCE_ARMS = ["allowed", "allowedWarning", "rejected"] as const;
export type FooterAllowanceArm = (typeof FOOTER_ALLOWANCE_ARMS)[number];

/**
 * One rate-limit allowance.
 *
 * The verdict is DELIBERATELY absent when `arm` is undefined: UNSET is legal
 * and means no rate-limit event has been observed for the window yet, so the
 * sampled figures draw with no verdict class and the arm joins later. That is
 * the one place in this file where leaving a oneof unset is correct rather
 * than a malformed view.
 */
export function allowance(
  arm?: FooterAllowanceArm,
  init?: { newsworthy?: boolean; resetsAtS?: bigint; utilization?: number },
): MessageInitShape<typeof FooterAllowanceSchema> {
  return {
    newsworthy: init?.newsworthy ?? true,
    resetsAtS: init?.resetsAtS ?? 1_700n,
    utilization: init?.utilization ?? 0.82,
    status: arm === undefined ? undefined : { case: arm, value: {} },
  };
}

/** A rate-limited activity whose two allowances carry the given verdicts. */
export const rateLimitedActivity = (
  session?: FooterAllowanceArm,
  weekly?: FooterAllowanceArm,
): object => ({ session: allowance(session), weekly: allowance(weekly ?? "allowed") });

/** Every activity kind arm, with a complete payload for each. */
export const FOOTER_ACTIVITY_KINDS: Record<string, object> = {
  notification: { text: "the agent addressed you" },
  contextBudget: { text: "84% of the window" },
  rateLimited: {
    session: allowance("allowedWarning", { newsworthy: true, resetsAtS: 1_700n, utilization: 0.82 }),
    weekly: allowance("allowed", { newsworthy: false, resetsAtS: 9_000n, utilization: 0.3 }),
  },
  hook: { name: "PreToolUse" },
  retrying: { attempt: 3, status: "overloaded" },
  contextInjected: { text: "CLAUDE.md loaded" },
  wakeup: { wakeAtMs: 60_000n, reason: { text: "the cron fires" } },
  gatedCall: { text: "Bash npm test" },
  questionLead: { text: "How far should the port go?" },
  blockedOnUser: { detail: "answer the permission card" },
  coldGateCost: { text: "184k tokens uncached" },
  compaction: { text: "compacting · summarizing 412 messages" },
  interrupting: { text: "stopping 3 agents" },
  mergingCommit: { sha: "abc1234", subject: "port the transport" },
  authenticating: { line: "opening the login terminal" },
  queryDied: { text: "the vendor query died" },
  closeBlocked: { text: "a turn is live" },
};

/** Which activity kinds each status arm legally carries. */
export const FOOTER_STATUS_ACTIVITIES: Record<string, readonly string[]> = {
  idle: ["notification", "contextBudget", "rateLimited"],
  thinking: [
    "hook",
    "retrying",
    "contextInjected",
    "compaction",
    "notification",
    "contextBudget",
    "rateLimited",
  ],
  waiting: [
    "wakeup",
    "gatedCall",
    "questionLead",
    "blockedOnUser",
    "notification",
    "coldGateCost",
    "rateLimited",
    "contextBudget",
    "interrupting",
  ],
  interrupted: ["notification", "rateLimited", "contextBudget"],
  merging: ["mergingCommit", "notification", "rateLimited", "contextBudget"],
  background: ["notification", "rateLimited", "contextBudget"],
  blocked: ["authenticating", "rateLimited", "notification", "queryDied", "contextBudget"],
  disconnected: ["notification", "rateLimited", "contextBudget"],
  closing: ["closeBlocked", "notification", "rateLimited", "contextBudget"],
  loading: ["contextInjected", "notification", "rateLimited", "contextBudget"],
};

/** The status arms whose `activity` is NOT optional on the wire. */
const STATUS_REQUIRING_ACTIVITY: readonly string[] = ["waiting", "loading"];

/**
 * The status arm, built from the string tables above.
 *
 * The tables are keyed by generated case names checked against the descriptors
 * by `assertCoversOneof`, so the ONE cast here is the seam between a
 * string-keyed table and the generated union — not a way around the contract.
 */
export function footerStatus(
  status: string,
  init?: {
    substatus?: string;
    activity?: string;
    activityAtMs?: bigint;
    /** Replace the activity kind's payload, for arm-by-arm tables. */
    activityOverride?: object;
  },
): StatusArm {
  const substatuses = FOOTER_STATUS_SUBSTATUSES[status];
  if (!substatuses) throw new Error(`no substatus table for footer status ${JSON.stringify(status)}`);
  const value: Record<string, unknown> = {};
  if (substatuses.length > 0) {
    const substatus = init?.substatus ?? substatuses[0];
    value.substatus = { case: substatus, value: substatusValue(substatus) };
  }
  // TWO STATUSES REQUIRE AN ACTIVITY (footer.proto: waiting, loading — every
  // such state has a composable line by construction). Elsewhere the field is
  // `optional` and absence is a legitimate state, so it is set only when a case
  // asks for one.
  const kind = init?.activity ?? (STATUS_REQUIRING_ACTIVITY.includes(status)
    ? FOOTER_STATUS_ACTIVITIES[status][0]
    : undefined);
  if (kind !== undefined) {
    value.activity = {
      at: { atMs: init?.activityAtMs ?? 3_000n },
      kind: { case: kind, value: init?.activityOverride ?? FOOTER_ACTIVITY_KINDS[kind] },
    };
  }
  return { case: status, value } as StatusArm;
}

export const FOOTER_CHIPS = ["agents", "tasks", "shells", "monitors", "crons"] as const;
export type FooterChip = (typeof FOOTER_CHIPS)[number];

export const FOOTER_PANELS = ["tokens", ...FOOTER_CHIPS] as const;
export const FOOTER_TOKENS_VERDICTS = ["complete", "incomplete", "invalid"] as const;

/** The FeedIds the expanded panels' jump rows target. */
export const FOOTER_AGENT_TARGET = "agent-row";
export const FOOTER_SHELL_TARGET = "shell-row";

type FooterInit = {
  status?: string;
  substatus?: string;
  activity?: string;
  activityAtMs?: bigint;
  activityOverride?: object;
  turnStartedAtMs?: bigint;
  tokensText?: string;
  alarm?: boolean;
  verdict?: (typeof FOOTER_TOKENS_VERDICTS)[number];
  chips?: Partial<Record<FooterChip, boolean>>;
  expanded?: boolean;
};

export function footerView(init?: FooterInit): FooterView {
  const chips = init?.chips ?? { agents: true, tasks: true, shells: true, monitors: true, crons: true };
  return create(FooterViewSchema, {
    strip: {
      status: { status: footerStatus(init?.status ?? "thinking", init) },
      // `turn_started_at_ms` is optional: an EXPLICIT undefined is the
      // no-turn-is-live case, which a `??` default would quietly overwrite.
      clock: {
        turnStartedAtMs:
          init !== undefined && "turnStartedAtMs" in init ? init.turnStartedAtMs : 1_000n,
      },
      tokens: {
        input: { text: init?.tokensText ?? "42.1k" },
        alarm: init?.alarm ? {} : undefined,
        verdict: init?.verdict ? { verdict: { case: init.verdict, value: {} } } : undefined,
      },
      liveWork: {
        agents: chips.agents ? { count: 3 } : undefined,
        tasks: chips.tasks ? { done: 2, total: 5 } : undefined,
        shells: chips.shells ? { count: 1 } : undefined,
        monitors: chips.monitors ? { count: 4 } : undefined,
        crons: chips.crons ? { count: 2 } : undefined,
      },
    },
    expanded: init?.expanded === false ? undefined : footerExpandedInit(),
  });
}

/** Every expanded panel, fully resolved, exactly as the daemon ships them. */
function footerExpandedInit(): MessageInitShape<typeof FooterExpandedSchema> {
  return {
    tokens: {
      contextGrowth: { value: "18.2k" },
      input: { value: "42.1k" },
      cacheRead: { value: "180k" },
      cacheWrite: { value: "3.2k" },
      output: { value: "1.1k" },
      thinking: { value: "800" },
      firstToken: { value: "1.2 s" },
      alarm: { text: "context is nearly full" },
      verdict: { verdict: { case: "incomplete" as const, value: { text: "usage still arriving" } } },
      agents: [
        {
          label: "main",
          input: { value: "40k" },
          cacheRead: { value: "170k" },
          cacheWrite: { value: "3k" },
          output: { value: "1k" },
        },
        {
          label: "reviewer · review the diff",
          input: { value: "2.1k" },
          cacheRead: { value: "10k" },
          cacheWrite: { value: "200" },
          output: { value: "100" },
        },
      ],
    },
    agents: {
      rows: [
        {
          work: { value: "work-agent" },
          jump: { target: { case: "entry" as const, value: feedId(FOOTER_AGENT_TARGET) } },
          label: { text: "reviewer" },
          description: { text: "review the diff" },
          tokens: { text: "12.4k" },
          runtime: { startedAtMs: 1_000n },
        },
      ],
    },
    tasks: {
      rows: [
        { status: { status: { case: "pending" as const, value: {} } }, subject: { text: "write fixtures" } },
        {
          status: {
            status: { case: "running" as const, value: { activeForm: { text: "writing the harness" } } },
          },
          subject: { text: "write the harness" },
        },
        {
          status: { status: { case: "completed" as const, value: {} } },
          subject: { text: "read the protos" },
        },
      ],
    },
    shells: {
      rows: [
        {
          work: { value: "work-shell" },
          jump: { target: { case: "entry" as const, value: feedId(FOOTER_SHELL_TARGET) } },
          command: { text: "npm run watch" },
          runtime: { startedAtMs: 1_000n },
        },
      ],
    },
    monitors: {
      rows: [
        {
          work: { value: "work-monitor" },
          jump: {
            target: {
              case: "unresolved" as const,
              value: { reason: { case: "noFeedEntry" as const, value: {} } },
            },
          },
          description: { text: "watch the daemon log" },
          runtime: { startedAtMs: 1_000n },
          persistent: {},
        },
      ],
    },
    crons: {
      rows: [
        {
          schedule: { text: "every 5 minutes" },
          prompt: { text: "check the queue" },
          nextFire: { fireAtMs: 60_000n },
          recurring: {},
          durable: {},
        },
      ],
    },
  };
}

/** The fake daemon's default footer: a complete, healthy, idle strip. */
export const emptyFooterView = (): FooterView => footerView({ status: "idle", substatus: "ready" });

// ---------------------------------------------------------------------------
// Topbar
// ---------------------------------------------------------------------------

export const TOPBAR_WARNING_ARMS = [
  "accounting",
  "unmodeledTool",
  "detachedUnmodeled",
  "sessionFault",
  "degradedWindow",
] as const;
export type TopbarWarningArm = (typeof TOPBAR_WARNING_ARMS)[number];

type WarningDetail = NonNullable<WarningInit["detail"]>;

const warningDetail = (arm: TopbarWarningArm): WarningDetail => {
  switch (arm) {
    case "accounting":
      return {
        case: "accounting",
        value: { lines: [{ text: "usage figures are estimates" }, { text: "cache reads uncounted" }] },
      };
    case "unmodeledTool":
      return {
        case: "unmodeledTool",
        value: {
          toolName: { text: "mcp__weather__forecast" },
          argumentLines: [{ text: "city=Berlin" }, { text: "days=3" }],
        },
      };
    case "detachedUnmodeled":
      return {
        case: "detachedUnmodeled",
        value: { toolName: { text: "mcp__weather__watch" }, startedAtMs: 1_000n },
      };
    case "sessionFault":
      return {
        case: "sessionFault",
        value: { component: { text: "shim" }, detail: { text: "store writes rejected" } },
      };
    case "degradedWindow":
      return {
        case: "degradedWindow",
        value: {
          component: { text: "store" },
          reason: { text: "disk pressure" },
          beganAtMs: 1_000n,
          extent: { case: "open", value: {} },
        },
      };
  }
};

export const topbarWarning = (arm: TopbarWarningArm): WarningInit => ({
  line: { text: `warning: ${arm}` },
  detail: warningDetail(arm),
});

/** The closed extent of the degraded-window detail, drawn without a tick. */
export const degradedWindowClosedWarning = (): WarningInit => ({
  line: { text: "warning: degradedWindow closed" },
  detail: {
    case: "degradedWindow",
    value: {
      component: { text: "store" },
      reason: { text: "disk pressure" },
      beganAtMs: 1_000n,
      extent: { case: "closed", value: { endedAtMs: 5_000n, droppedCount: 12n } },
    },
  },
});

export const TOPBAR_ACCOUNT_ARMS = ["loggedIn", "loggedOut"] as const;

/**
 * The permission modes the picker lists; SetPermissionMode echoes `mode`.
 *
 * `auto` LEADS AND `default` IS ABSENT, mirroring what the daemon's
 * `topbar.SwitchableModes` now serves (owner ruling 2026-09-14). A fixture
 * offering `default` would be a view no daemon can publish.
 */
export const PERMISSION_MODES = [
  { mode: "auto", displayName: "auto" },
  { mode: "acceptEdits", displayName: "accept edits" },
  { mode: "plan", displayName: "plan" },
] as const;

/**
 * The vendor's `default`, as a session started BEFORE the ruling still reports
 * it: a mode in force that the served set does not carry, so the picker draws
 * it as the current value and offers no row for it.
 */
export const LIVE_DEFAULT_MODE = { mode: "default", displayName: "default" } as const;

/**
 * The account root the fixture daemon serves, and the second one a switch
 * picks. `TopbarAccount.options` is ALWAYS THE WHOLE SET (topbar.proto): a
 * one-root machine still gets a one-row dropdown, so a fixture that served an
 * empty list would be a view no daemon can publish.
 */
export const ACCOUNT_CONFIG_DIR = "/tmp/config";
export const OTHER_ACCOUNT_CONFIG_DIR = "/tmp/config-other";

/** One offered root, as `TopbarAccount.options` takes it. */
export type AccountOptionInit = {
  configDir: string;
  /** The email this root holds a login for; omitted means a logged-out root. */
  email?: string;
  current?: boolean;
};

type TopbarInit = {
  title?: string;
  sessionLine?: string;
  account?: (typeof TOPBAR_ACCOUNT_ARMS)[number];
  /**
   * The roots the dropdown offers. Omitted, the fixture serves exactly one —
   * the root the cell itself describes, marked `current` — which is the
   * smallest set the contract permits.
   */
  accountOptions?: AccountOptionInit[];
  email?: string;
  tone?: string;
  glyph?: string;
  connectivityTitle?: string;
  models?: { name: string; displayName: string; description: string }[];
  selected?: string;
  /**
   * UNSET `TopbarModelSelector.selected`. The field is `optional` and absence
   * is the legitimate "no model is selected" state, which the chip draws as
   * its placeholder rather than guessing an option.
   */
  unselected?: boolean;
  contextText?: string;
  breakdown?: boolean;
  /** Omit the per-row share, which is `optional` and drawn only when set. */
  shares?: boolean;
  warnings?: WarningInit[];
  permissionMode?: string;
};

/** One offered root: the arm is the state, exactly as the cell's own is. */
function accountOption(init: AccountOptionInit): TopbarAccountOption {
  return create(TopbarAccountOptionSchema, {
    configDir: init.configDir,
    current: init.current ?? false,
    state:
      init.email === undefined
        ? { case: "loggedOut", value: {} }
        : { case: "loggedIn", value: { email: init.email } },
  });
}

export function topbarView(init?: TopbarInit): TopbarView {
  const models = init?.models ?? [
    { name: "opus", displayName: "Opus", description: "the big one" },
    { name: "sonnet", displayName: "Sonnet", description: "the fast one" },
  ];
  return create(TopbarViewSchema, {
    title: { text: init?.title ?? "port the webapp" },
    sessionLine: { text: init?.sessionLine ?? "session 3 of the overhaul" },
    modelSelector: {
      selected:
        init?.unselected === true
          ? undefined
          : modelOption(
              init?.selected ?? models[0].name,
              models[0].displayName,
              models[0].description,
            ),
      options: models.map((m) => modelOption(m.name, m.displayName, m.description)),
    },
    connectivity: {
      tone: init?.tone ?? "green",
      glyph: init?.glyph ?? "dot",
      title: init?.connectivityTitle ?? "connected to claude-repld",
    },
    warnings: { warnings: init?.warnings ?? [] },
    context: {
      text: init?.contextText ?? "184k",
      breakdown:
        init?.breakdown === false
          ? undefined
          : {
              sections: [
                {
                  heading: { text: "session" },
                  rows: [
                    {
                      label: "system prompt",
                      tokens: 12_000n,
                      sharePermille: init?.shares === false ? undefined : 65,
                      emphasized: true,
                      depth: 0,
                    },
                    { label: "memory files", tokens: 4_000n, emphasized: false, depth: 1 },
                  ],
                },
              ],
            },
    },
    account: {
      ...(init?.account === "loggedOut"
        ? { state: { case: "loggedOut" as const, value: {} } }
        : { state: { case: "loggedIn" as const, value: { email: init?.email ?? "dev@example.test" } } }),
      options: (
        init?.accountOptions ?? [
          init?.account === "loggedOut"
            ? { configDir: ACCOUNT_CONFIG_DIR, current: true }
            : { configDir: ACCOUNT_CONFIG_DIR, email: init?.email ?? "dev@example.test", current: true },
        ]
      ).map(accountOption),
    },
    permissionModePicker: {
      current:
        PERMISSION_MODES.find((m) => m.mode === (init?.permissionMode ?? "auto")) ??
        (init?.permissionMode === LIVE_DEFAULT_MODE.mode ? { ...LIVE_DEFAULT_MODE } : undefined),
      options: PERMISSION_MODES.map((m) => ({ ...m })),
    },
  });
}

export const emptyTopbarView = (): TopbarView => topbarView();

// ---------------------------------------------------------------------------
// Sidebar
// ---------------------------------------------------------------------------

export const ROSTER_STATUS_ARMS = [
  "submitting",
  "thinking",
  "clearing",
  "compacting",
  "permission",
  "done",
  "interrupted",
  "ready",
  "idleAsync",
  "vendorBlocked",
  "init",
  "severed",
  "startFailed",
  "degraded",
  "dead",
  "mergeEnqueuing",
  "merging",
  "mergeQueued",
  "mergeConflict",
  "mergeFailed",
  "merged",
  "none",
  "inactive",
] as const;
export type RosterStatusArm = (typeof ROSTER_STATUS_ARMS)[number];

/** The merge arms the vocabulary paints with glyphs instead of a color. */
export const ROSTER_MERGE_ARMS = [
  "mergeEnqueuing",
  "merging",
  "mergeQueued",
  "mergeConflict",
  "mergeFailed",
  "merged",
] as const;

type RosterRowInit = {
  id?: string;
  name?: string;
  status?: RosterStatusArm;
  attention?: boolean;
  priority?: string;
  current?: boolean;
  closed?: boolean;
  detail?: boolean;
  when?: "lastSelected" | "merged" | "active" | "created";
  whenAtMs?: bigint;
  children?: RosterRow[];
};

export function rosterRow(init?: RosterRowInit): RosterRow {
  return create(RosterRowSchema, {
    workspace: { workspace: workspaceRef(init?.id ?? WORKSPACE_ID) },
    attention: init?.attention ? {} : undefined,
    priority: init?.priority ? { label: init.priority } : undefined,
    name: { text: init?.name ?? "webapp-integration-suite" },
    status: { case: init?.status ?? "ready", value: {} } as RosterRow["status"],
    current: { current: init?.current ?? false },
    children: init?.children ?? [],
    when: { shown: { case: init?.when ?? "lastSelected", value: { atMs: init?.whenAtMs ?? 1_000n } } },
    detail:
      init?.detail === false
        ? undefined
        : {
            branch: { name: "overhaul/webapp-integration-suite" },
            parentBranch: { name: "master" },
            summary: { text: "the integration suite" },
          },
    closed: { closed: init?.closed ?? false },
  });
}

export function roster(init?: {
  rows?: RosterRow[];
  taskRows?: RosterRow[];
  merged?: RosterRow[];
  current?: string;
  /**
   * UNSET `WorkspaceRoster.current`. The field is `optional`, and absence is
   * the legitimate "no workspace is current" state — never a MalformedView,
   * and never a reason to highlight a row (the row's own flag decides that).
   */
  currentUnset?: boolean;
  /** The task section header's done check, as the daemon resolved it. */
  taskDone?: boolean;
  /** Replace the repository grouping's sections, IN WIRE ORDER. */
  repositorySections?: { repositoryId: string; label: string; rows: RosterRow[] }[];
}): WorkspaceRoster {
  const rows = init?.rows ?? [rosterRow()];
  return create(WorkspaceRosterSchema, {
    repository: {
      sections: init?.repositorySections?.map((section) => ({
        key: { repository: repositoryRef(section.repositoryId) },
        header: { label: { text: section.label } },
        rows: { rows: section.rows },
      })) ?? [
        { key: { repository: repositoryRef() }, header: { label: { text: "doom" } }, rows: { rows } },
      ],
    },
    task: {
      sections: [
        {
          key: { taskId: "task-1" },
          header: { label: { text: "the overhaul" }, done: { done: init?.taskDone ?? false } },
          rows: { rows: init?.taskRows ?? rows },
        },
      ],
    },
    recentlyMerged: {
      header: { label: { text: "recently merged" } },
      rows: { rows: init?.merged ?? [rosterRow({ id: "ws-merged", status: "merged", when: "merged" })] },
    },
    current:
      init?.currentUnset === true
        ? undefined
        : { workspace: workspaceRef(init?.current ?? WORKSPACE_ID) },
  });
}

export const emptyRoster = (): WorkspaceRoster => roster();

// ---------------------------------------------------------------------------
// Daemon hold tray
// ---------------------------------------------------------------------------

export const HOLD_CLASSIFICATION_ARMS = [
  "classifying",
  "interject",
  "holdForTurnEnd",
  "uninterruptibleTurn",
  "classificationError",
] as const;
export type HoldClassificationArm = (typeof HOLD_CLASSIFICATION_ARMS)[number];

/** The ONE classification that draws an [accept] button (ruled). */
export const HOLD_ACCEPTABLE_ARM = "holdForTurnEnd";

export const HOLD_ARMS = ["shutdown", "sessionStarting", "buildRefresh"] as const;
export type HoldArm = (typeof HOLD_ARMS)[number];

type HeldPromptInit = MessageInitShape<typeof HeldPromptSchema>;

const classificationValue = (
  arm: HoldClassificationArm,
  accepted?: boolean,
): NonNullable<HeldPromptInit["classification"]> => {
  switch (arm) {
    case "classifying":
      return { case: "classifying", value: {} };
    case "interject":
      return { case: "interject", value: { rationale: "it changes the current work" } };
    case "holdForTurnEnd":
      return {
        case: "holdForTurnEnd",
        value: { rationale: "it is a follow-up", accepted: { accepted: accepted ?? false } },
      };
    case "uninterruptibleTurn":
      return { case: "uninterruptibleTurn", value: { command: SessionCommand.COMPACT } };
    case "classificationError":
      return { case: "classificationError", value: { detail: "the classifier timed out" } };
  }
};

const holdValue = (arm: HoldArm): NonNullable<HeldPromptInit["hold"]> => {
  switch (arm) {
    case "shutdown":
      return { case: "shutdown", value: { scheduleId: "sched-1" } };
    case "sessionStarting":
      return { case: "sessionStarting", value: {} };
    case "buildRefresh":
      return { case: "buildRefresh", value: {} };
  }
};

export const HELD_TURN_ID = "turn-held";

/**
 * The badge the daemon composes for each status (daemon/internal/resolve/holds),
 * as a scripted daemon serves it. The webapp draws these words verbatim.
 */
export const HOLD_BADGES: Readonly<Record<string, { label: string; detail?: string }>> = {
  classifying: { label: "classifying", detail: "queued — classifying" },
  interject: { label: "interrupting", detail: "interjects" },
  holdForTurnEnd: { label: "after this turn" },
  uninterruptibleTurn: { label: "after /compact", detail: "waits for /compact to finish" },
  classificationError: { label: "unclassified" },
  accepted: { label: "confirmed" },
  shutdown: { label: "restart hold", detail: "held for the scheduled restart (sched-1)" },
  sessionStarting: { label: "starting up", detail: "held until the session is up" },
  buildRefresh: { label: "build refresh", detail: "held for the build refresh" },
};

export function heldPrompt(init?: {
  turn?: string;
  text?: string;
  classification?: HoldClassificationArm;
  hold?: HoldArm;
  accepted?: boolean;
}): HeldPrompt {
  const classification = init?.classification ?? "interject";
  const hold = init?.hold ?? "sessionStarting";
  const statuses: string[] = [classification];
  if (classification === "holdForTurnEnd" && init?.accepted === true) statuses.push("accepted");
  statuses.push(hold);
  return create(HeldPromptSchema, {
    turn: turnId(init?.turn ?? HELD_TURN_ID),
    said: userSaid(init?.text ?? "also fix the footer"),
    queuedAt: { atMs: 3_000n },
    classification: classificationValue(classification, init?.accepted),
    hold: holdValue(hold),
    badges: statuses.map((status) => HOLD_BADGES[status] ?? { label: status }),
  });
}

export const heldPromptItem = (init?: Parameters<typeof heldPrompt>[0]): HoldItemInit => ({
  item: { case: "prompt", value: heldPrompt(init) },
});

export const HELD_OFFER_ARMS = ["mergeDequeue"] as const;
export const HELD_OFFER_HEADLINE = "your merge is queued behind 2 others";

export const heldOfferItem = (headline = HELD_OFFER_HEADLINE): HoldItemInit => ({
  item: {
    case: "offer",
    value: { offer: { case: "mergeDequeue", value: { headline: { text: headline } } } },
  },
});

export function holdTray(init?: { items?: HoldItemInit[] }): DaemonHoldTray {
  return create(DaemonHoldTraySchema, {
    items: init?.items ?? [heldPromptItem()],
  });
}

/** The fake daemon's default tray: nothing held — so it draws nothing. */
export const emptyTray = (): DaemonHoldTray => holdTray({ items: [] });

// ---------------------------------------------------------------------------
// Failures (the six client-local arms the webapp's own overlay mints)
// ---------------------------------------------------------------------------

export const CLIENT_FAILURE_ARMS = [
  "daemonUnreachable",
  "workspaceGone",
  "bootFailed",
  "controlPlaneFailed",
  "frameUndecodable",
  "staleBundle",
] as const;
export type ClientFailureArm = (typeof CLIENT_FAILURE_ARMS)[number];

type FailureArm = NonNullable<MessageInitShape<typeof FailureKindSchema>["kind"]>;

const failureValue = (arm: ClientFailureArm): FailureArm => {
  switch (arm) {
    case "daemonUnreachable":
      return { case: "daemonUnreachable", value: { closeCode: 1006, closeReason: "abnormal" } };
    case "workspaceGone":
      return { case: "workspaceGone", value: {} };
    case "bootFailed":
      return { case: "bootFailed", value: { cause: "the page address had no workspace" } };
    case "controlPlaneFailed":
      return {
        case: "controlPlaneFailed",
        value: { what: "WatchFooter", cause: "the stream refused the token" },
      };
    case "frameUndecodable":
      return {
        case: "frameUndecodable",
        value: { cause: "unknown field 999", frameHead: "WatchFooterResponse" },
      };
    case "staleBundle":
      return { case: "staleBundle", value: { detail: "the daemon shipped a newer bundle" } };
  }
};

export const clientFailure = (arm: ClientFailureArm): FailureKind =>
  create(FailureKindSchema, { kind: failureValue(arm) });

// ---------------------------------------------------------------------------
// Daemon lifecycle pushes
// ---------------------------------------------------------------------------

export const DRAIN_REASON_ARMS = ["deploy", "maintenance", "operator"] as const;
export type DrainReasonArm = (typeof DRAIN_REASON_ARMS)[number];

export const drainReason = (arm: DrainReasonArm): DrainReason =>
  create(DrainReasonSchema, {
    kind:
      arm === "operator"
        ? { case: "operator", value: { note: "the operator asked" } }
        : { case: arm, value: {} },
  });

export const SHUTDOWN_CAUSE_ARMS = ["selfMergeRollout", "scheduledDrain", "immediate"] as const;
export type ShutdownCauseArm = (typeof SHUTDOWN_CAUSE_ARMS)[number];

export const WATCH_DAEMON_PUSHES = [
  "shutdownAnnounced",
  "drainScheduled",
  "drainCancelled",
  // A TOP-LEVEL arm this bundle has NO case for. It is covered by the skew
  // tests rather than by a drawing test, because not drawing it is the
  // contract: `unreachablePushArm` raises `UnknownPushArm`, and the stream
  // pipeline skips it quietly rather than filing a bad frame.
  "mutationProgress",
] as const;

export function shutdownAnnounced(init?: {
  address?: string;
  cause?: ShutdownCauseArm;
  reason?: DrainReasonArm;
  expectedOutageMs?: bigint;
  mintedAtMs?: bigint;
}): WatchDaemonResponse {
  const cause = init?.cause ?? "scheduledDrain";
  return create(WatchDaemonResponseSchema, {
    push: {
      case: "shutdownAnnounced",
      value: {
        address: init?.address,
        cause: {
          kind:
            cause === "selfMergeRollout"
              ? { case: "selfMergeRollout", value: {} }
              : { case: cause, value: { reason: drainReason(init?.reason ?? "deploy") } },
        },
        expectedOutageMs: init?.expectedOutageMs ?? 4_000n,
        mintedAtMs: init?.mintedAtMs ?? 1_000n,
      },
    },
  });
}

/** The default host-workspace push the fake serves; Emacs's stream, not the webapp's. */
export const hostWorkspacePush = (): WatchHostWorkspaceResponse =>
  create(WatchHostWorkspaceResponseSchema, {
    push: {
      case: "host",
      value: {
        session: {
          case: "existing",
          value: {
            id: { value: "session-1" },
            standing: {
              case: "live",
              value: {
                generation: { value: "gen-1" },
                shimAttached: true,
                vendorInfo: { case: "claude", value: { sessionId: "vendor-1", configDir: "/tmp/config" } },
                backfill: { state: { case: "done", value: {} } },
                composer: { case: "open", value: {} },
                faults: [],
              },
            },
          },
        },
        naming: { slug: "webapp-integration-suite", title: "the integration suite" },
      },
    },
  });

// ---------------------------------------------------------------------------
// Faults (DaemonHealth / SessionHealth answers, and the host stream's rows)
// ---------------------------------------------------------------------------

export const DAEMON_FAULT_ARMS = [
  "adoptionWindowExpired",
  "logSinkPoisoned",
  "deployScriptFailed",
  "successorSpawnFailed",
  "promptsDirMissing",
  "wsmReadOnly",
] as const;
export type DaemonFaultArm = (typeof DAEMON_FAULT_ARMS)[number];

export const SESSION_FAULT_ARMS = [
  "shimStartFailed",
  "shimDied",
  "linkSevered",
  "resumeFailed",
  "bounceDied",
  "bounceUnknown",
  "classifierFailed",
  "shimReported",
] as const;
export type SessionFaultArm = (typeof SESSION_FAULT_ARMS)[number];

type DaemonFaultInit = MessageInitShape<typeof DaemonFaultSchema>;
type SessionFaultInit = MessageInitShape<typeof SessionFaultSchema>;

/** An unhealthy answer is an ANSWER, not an error: it rides the success arm. */
export function daemonFault(arm: DaemonFaultArm, detail?: string): DaemonFaultInit {
  // ANNOTATED, because an inferred IIFE union merges the six arms' value
  // shapes into one optional-everything object that no single arm accepts.
  const kind = ((): DaemonFaultInit["kind"] => {
    switch (arm) {
      case "adoptionWindowExpired":
        return { case: "adoptionWindowExpired" as const, value: { workspace: workspaceRef() } };
      case "logSinkPoisoned":
        return { case: "logSinkPoisoned" as const, value: { sink: "the durable log" } };
      case "deployScriptFailed":
        return { case: "deployScriptFailed" as const, value: { detail: "the deploy script exited 1" } };
      case "successorSpawnFailed":
        return { case: "successorSpawnFailed" as const, value: { detail: "the successor never came up" } };
      case "promptsDirMissing":
        return { case: "promptsDirMissing" as const, value: { path: "/no/such/prompts" } };
      case "wsmReadOnly":
        return { case: "wsmReadOnly" as const, value: {} };
    }
  })();
  return { detail: detail ?? `daemon fault: ${arm}`, kind };
}

export function sessionFault(arm: SessionFaultArm, detail?: string): SessionFaultInit {
  return { detail: detail ?? `session fault: ${arm}`, kind: { case: arm, value: {} } };
}

export const daemonUnhealthy = (arms: readonly DaemonFaultArm[] = DAEMON_FAULT_ARMS) =>
  create(DaemonHealthResponseSchema, {
    result: {
      case: "success",
      value: { health: { case: "unhealthy", value: { faults: arms.map((a) => daemonFault(a)) } } },
    },
  });

export const sessionUnhealthy = (arms: readonly SessionFaultArm[] = SESSION_FAULT_ARMS) =>
  create(SessionHealthResponseSchema, {
    result: {
      case: "success",
      value: { health: { case: "unhealthy", value: { faults: arms.map((a) => sessionFault(a)) } } },
    },
  });

// ---------------------------------------------------------------------------
// Command panels (SubmitPrompt success arms and the feed's panel row)
// ---------------------------------------------------------------------------

export const COMMAND_PANEL_ARMS = ["status", "todos", "agents", "mcp", "context", "help"] as const;
export type CommandPanelArm = (typeof COMMAND_PANEL_ARMS)[number];

export const MCP_STATUS_ARMS = ["connected", "failed", "needsAuth", "pending", "disabled"] as const;
export const TODO_STATUS_ARMS = ["pending", "running", "completed"] as const;

type PanelArm = NonNullable<MessageInitShape<typeof SubmitPromptCommandPanelSchema>["panel"]>;

function panelValue(arm: CommandPanelArm | FeedCommandPanelArm): PanelArm {
  switch (arm) {
    case "status":
      return {
        case: "status",
        value: {
          rows: [
            { label: "version", value: "0.1.0" },
            { label: "account", value: "dev@example.test" },
            { label: "model", value: "opus" },
            { label: "mode", value: "default" },
          ],
        },
      };
    case "todos":
      return {
        case: "todos",
        value: {
          rows: [
            { status: { case: "pending", value: {} }, subject: "write fixtures" },
            { status: { case: "running", value: {} }, subject: "write the harness" },
            { status: { case: "completed", value: {} }, subject: "read the protos" },
          ],
        },
      };
    case "agents":
      return {
        case: "agents",
        value: {
          rows: [
            { name: "reviewer", description: { text: "review the diff" } },
            { name: "explorer", description: { text: "search the repo" } },
          ],
        },
      };
    case "mcp":
      return {
        case: "mcp",
        value: {
          rows: [
            { name: "weather", status: { case: "connected", value: {} } },
            { name: "gmail", status: { case: "failed", value: { detail: { text: "handshake refused" } } } },
            { name: "drive", status: { case: "needsAuth", value: {} } },
            { name: "slack", status: { case: "pending", value: {} } },
            { name: "figma", status: { case: "disabled", value: {} } },
          ],
        },
      };
    case "context":
      return {
        case: "context",
        value: {
          header: { used: "142.3k", total: "200k", percent: 71, model: "claude-opus-5" },
          sections: [
            {
              label: "System prompt",
              figure: "12.0k · 6%",
              items: [{ label: "identity", figure: "1.2k" }],
            },
            {
              label: "Messages",
              figure: "38.1k · 19%",
              items: [{ label: "assistant messages", figure: "8.0k" }],
              sections: [
                {
                  label: "tool calls",
                  items: [{ label: "Bash", figure: "call 3.4k · result 22.0k" }],
                },
                { label: "attachments", items: [{ label: "images", figure: "1.0k" }] },
              ],
            },
            { label: "Free space", figure: "57.7k · 29%" },
          ],
          autoCompactLine: "auto-compact at 90%",
        },
      };
    case "help":
      return {
        case: "help",
        value: {
          rows: [
            { command: "/clear", description: { text: "clear the context" } },
            { command: "/compact", description: { text: "compact the context" } },
          ],
        },
      };
  }
}

export const commandPanel = (arm: CommandPanelArm): SubmitPromptCommandPanel =>
  create(SubmitPromptCommandPanelSchema, { panel: panelValue(arm) });
