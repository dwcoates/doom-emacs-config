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
import {
  FeedIdSchema,
  FeedPageSchema,
  FeedRowSchema,
  type FeedId,
  type FeedPage,
  type FeedRow,
  type FeedTurnActivity,
  type FeedSubagent,
  type FeedShell,
  type FeedMergeTab,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  FooterViewSchema,
  type FooterView,
  type FooterStatus,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import {
  TopbarViewSchema,
  type TopbarView,
  type TopbarWarning,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import {
  WorkspaceRosterSchema,
  RosterRowSchema,
  type RosterRow,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  DaemonHoldTraySchema,
  HeldPromptSchema,
  type DaemonHoldTray,
  type HeldPrompt,
  type DaemonHoldItem,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { FailureKindSchema, type FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  WatchHostWorkspaceResponseSchema,
  type WatchHostWorkspaceResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_host_workspace_pb";
import { WatchDaemonResponseSchema, type WatchDaemonResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import { DrainReasonSchema, type DrainReason } from "../../../proto/gen/ts/agentrepl/v1/drain_reason_pb";
import { SubmitPromptCommandPanelSchema, type SubmitPromptCommandPanel } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";

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
  const seen = [...covered].sort();
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
  return create(schema, { ...defaults, ...(overrides ?? {}) } as MessageInitShape<Desc>);
}

// ---------------------------------------------------------------------------
// Identity
// ---------------------------------------------------------------------------

export const WORKSPACE_ID = "ws-1";

export const workspaceRef = (id = WORKSPACE_ID) => create(WorkspaceRefSchema, { id, dir: `/tmp/${id}` });
export const repositoryRef = (id = "repo-1") => create(RepositoryRefSchema, { id, dir: `/src/${id}` });
export const feedId = (value: string): FeedId => create(FeedIdSchema, { value });
export const turnId = (value = "turn-1") => create(TurnIdSchema, { value });
export const agentModel = (name = "opus") => create(AgentModelSchema, { name });
export const modelOption = (name = "opus", displayName = "Opus", description = "the big one") =>
  create(ModelOptionSchema, { model: agentModel(name), displayName, description });
export const userSaid = (text = "hello") =>
  create(UserSaidSchema, { content: { blocks: [{ block: { case: "text", value: { text } } }] } });

// ---------------------------------------------------------------------------
// Feed rows
// ---------------------------------------------------------------------------

type RowInit = MessageInitShape<typeof FeedRowSchema>;

/** A FeedRow wrapping `row`, with id/turn defaulted and overridable. */
export function feedRow(row: RowInit["row"], overrides?: Partial<RowInit>): FeedRow {
  return make(
    FeedRowSchema,
    { id: feedId("row-1"), turn: turnId(), row },
    overrides,
  );
}

/** An activity row: the FeedTurnActivity wrapper around one unit. */
export function activityRow(
  unit: MessageInitShape<typeof FeedRowSchema>["row"] extends infer _ ? FeedTurnActivity["unit"] : never,
  overrides?: Partial<RowInit>,
): FeedRow {
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

export const responseUnit = (
  state: ResponseState,
  markdown = "the answer",
  usageText = "1.2k in / 340 out",
): FeedTurnActivity["unit"] => ({
  case: "response",
  value: { usage: { text: usageText }, result: { case: state, value: { prose: { markdown } } } },
});

export const responseRow = (
  state: ResponseState,
  markdown?: string,
  overrides?: Partial<RowInit>,
): FeedRow => activityRow(responseUnit(state, markdown), overrides);

// ---- FeedSimpleToolCall ---------------------------------------------------

export const TOOL_OUTPUT_FORMS = ["text", "code", "diff", "lines", "links"] as const;
export type ToolOutputForm = (typeof TOOL_OUTPUT_FORMS)[number];

export const DIFF_LINE_KINDS = ["header", "added", "removed", "context"] as const;

const toolOutputForm = (form: ToolOutputForm) => {
  switch (form) {
    case "text":
      return { case: "text" as const, value: { text: "plain output" } };
    case "code":
      return {
        case: "code" as const,
        value: {
          spans: [
            { text: "PASS ", paintClass: "ok" },
            { text: "FAIL ", paintClass: "err" },
            { text: "raw ", paintClass: "" },
          ],
          omitted: { text: "12 lines omitted" },
        },
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
  }
};

export const toolCallRunningUnit = (lastProgressAtMs = 1_000n): FeedTurnActivity["unit"] => ({
  case: "simpleToolCall",
  value: {
    name: { text: "Bash" },
    input: { text: "npm test" },
    outcome: { case: "running", value: { lastProgress: { atMs: lastProgressAtMs } } },
  },
});

export const toolCallReturnedUnit = (
  form: ToolOutputForm,
  verdict: "succeeded" | "failed" = "succeeded",
): FeedTurnActivity["unit"] => ({
  case: "simpleToolCall",
  value: {
    name: { text: "Bash" },
    input: { text: "npm test", link: { url: "https://example.test/run" } },
    outcome: {
      case: "returned",
      value: {
        verdict: { case: verdict, value: {} },
        form: toolOutputForm(form),
        runtime: { text: "ran 4.2 s" },
        diagnostics: { lines: ["one warning"] },
      },
    },
  },
});

export const toolCallDeniedUnit = (): FeedTurnActivity["unit"] => ({
  case: "simpleToolCall",
  value: {
    name: { text: "Bash" },
    input: { text: "rm -rf /" },
    outcome: { case: "denied", value: {} },
  },
});

// ---- FeedSkill ------------------------------------------------------------

export const SKILL_OUTCOMES = ["running", "loaded", "failed", "denied"] as const;
export type SkillOutcome = (typeof SKILL_OUTCOMES)[number];

export const skillUnit = (outcome: SkillOutcome): FeedTurnActivity["unit"] => ({
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

export const hookUnit = (outcome: HookOutcome): FeedTurnActivity["unit"] => ({
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

// ---- FeedArtifact / FeedPlan / FeedFindings --------------------------------

export const ARTIFACT_STATES = ["publishing", "published", "failed"] as const;
export type ArtifactState = (typeof ARTIFACT_STATES)[number];

export const artifactUnit = (state: ArtifactState): FeedTurnActivity["unit"] => ({
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

export const planUnit = (state: PlanState): FeedTurnActivity["unit"] => ({
  case: "plan",
  value: {
    state:
      state === "planned"
        ? {
            case: "planned",
            value: { prose: { markdown: "1. do it" }, edit: { path: "/repo/PLAN.md" } },
          }
        : state === "failed"
          ? { case: "failed", value: { text: "planning refused" } }
          : { case: "planning", value: {} },
  },
});

export const FINDINGS_VERDICTS = ["confirmed", "plausible"] as const;
export const FINDINGS_OUTCOMES = ["fixed", "skipped", "noChange"] as const;

export const findingsUnit = (): FeedTurnActivity["unit"] => ({
  case: "findings",
  value: {
    heading: { text: "Review findings" },
    rows: [
      {
        verdict: { case: "confirmed", value: {} },
        category: { text: "correctness" },
        location: { text: "feed.ts:42", path: "/repo/src/feed.ts", line: 42 },
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
        location: { text: "row.ts:1", path: "/repo/src/row.ts", line: 1 },
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

export function subagent(
  state: "live" | SubagentOutcome,
  overrides?: Partial<MessageInitShape<typeof FeedRowSchema>>,
): FeedSubagent["state"] extends never ? never : FeedTurnActivity["unit"] {
  void overrides;
  return {
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
  };
}

export const SHELL_OUTCOMES = ["completed", "cancelled", "lost"] as const;
export type ShellOutcome = (typeof SHELL_OUTCOMES)[number];

export function shell(state: "live" | ShellOutcome): FeedShell["state"] extends never ? never : MessageInitShape<typeof FeedRowSchema>["row"] {
  return {
    case: "detachedShell",
    value: {
      shell: {
        command: { text: "npm run watch" },
        runtime: { startedAtMs: 1_000n },
        spool: { text: "building...", omitted: { text: "40 lines omitted" } },
        state:
          state === "live"
            ? { case: "live", value: { lastProgress: { atMs: 2_000n } } }
            : {
                case: "settled",
                value: { endedAtMs: 9_000n, exit: { code: 1 }, outcome: { case: state, value: {} } },
              },
      },
    },
  };
}

export const detachedSubagentRow = (
  state: "live" | SubagentOutcome = "live",
  overrides?: Partial<RowInit>,
): FeedRow => {
  const unit = subagent(state) as { case: "subagent"; value: unknown };
  return feedRow(
    { case: "detachedSubagent", value: { subagent: unit.value as never } },
    overrides,
  );
};

export const detachedShellRow = (state: "live" | ShellOutcome = "live", overrides?: Partial<RowInit>): FeedRow =>
  feedRow(shell(state) as RowInit["row"], overrides);

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
] as const;
export type TurnErrorArm = (typeof TURN_ERROR_ARMS)[number];

const turnErrorValue = (arm: TurnErrorArm, retryAfterMs?: bigint) => {
  if (arm === "rateLimited" || arm === "overloaded") {
    return { case: arm, value: { retryAfterMs: retryAfterMs ?? 30_000n } };
  }
  if (arm === "vendorUnmodeled") return { case: arm, value: { type: "vendor_teapot" } };
  return { case: arm, value: {} };
};

export const turnEndedConcludedRow = (answer: FeedId, overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    { case: "turnEnded", value: { endedAtMs: 9_000n, outcome: { case: "concluded", value: { answer } } } },
    overrides,
  );

export const turnEndedErroredRow = (
  arm: TurnErrorArm,
  init?: { retryAfterMs?: bigint; message?: string },
  overrides?: Partial<RowInit>,
): FeedRow =>
  feedRow(
    {
      case: "turnEnded",
      value: {
        endedAtMs: 9_000n,
        outcome: {
          case: "errored",
          value: {
            message: { text: init?.message ?? `the turn failed: ${arm}` },
            error: turnErrorValue(arm, init?.retryAfterMs) as never,
          },
        },
      },
    },
    overrides,
  );

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
] as const;
export type PermissionAnswer = (typeof PERMISSION_ANSWERS)[number];

export function permissionRow(
  state: "open" | "abandoned" | PermissionAnswer,
  init?: { standingOffered?: boolean },
  overrides?: Partial<RowInit>,
): FeedRow {
  const answered = (PERMISSION_ANSWERS as readonly string[]).includes(state);
  return feedRow(
    {
      case: "permission",
      value: {
        headline: { text: "Bash wants to run" },
        subtitle: { text: "npm test" },
        trigger: { text: "requested by the reviewer subagent" },
        arguments: { lines: ["cwd=/repo", "timeout=120s"] },
        standingOffered: init?.standingOffered === false ? undefined : {},
        state: answered
          ? {
              case: "answered",
              value: {
                atMs: 5_000n,
                answer:
                  state === "deniedByPolicy"
                    ? { case: "deniedByPolicy", value: { text: "policy forbids it" } }
                    : { case: state as Exclude<PermissionAnswer, "deniedByPolicy">, value: {} },
              },
            }
          : state === "abandoned"
            ? { case: "abandoned", value: { atMs: 5_000n } }
            : { case: "open", value: {} },
      },
    },
    overrides,
  );
}

// ---- FeedQuestion ---------------------------------------------------------

export function questionRow(
  state: "open" | "answered" | "expired",
  overrides?: Partial<RowInit>,
): FeedRow {
  return feedRow(
    {
      case: "question",
      value: {
        questions: [
          {
            header: { text: "Scope" },
            text: { text: "How far should the port go?" },
            options: {
              case: "singleSelect",
              value: {
                options: [
                  { label: { text: "whole app" }, description: { text: "every component" } },
                  { label: { text: "the feed only" }, description: { text: "one component" } },
                ],
              },
            },
          },
          {
            header: { text: "Suites" },
            text: { text: "Which suites run?" },
            options: {
              case: "multiSelect",
              value: {
                options: [
                  { label: { text: "unit" }, description: { text: "vitest" } },
                  { label: { text: "integration" }, description: { text: "the fake daemon" } },
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
                    { header: { text: "Scope" }, chosen: ["whole app"], otherText: { text: "and the docs" } },
                    { header: { text: "Suites" }, chosen: ["unit", "integration"] },
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

export const SEPARATION_ARMS = ["cleared", "compacted", "worktreeEntered", "worktreeLeft"] as const;
export type SeparationArm = (typeof SEPARATION_ARMS)[number];

const separationKind = (arm: SeparationArm) => {
  switch (arm) {
    case "cleared":
      return { case: "cleared" as const, value: {} };
    case "compacted":
      return {
        case: "compacted" as const,
        value: {
          summary: { markdown: "the session so far" },
          fold: { folded: true },
          coldRead: { evidence: { uncachedInputTokens: 40_000n } },
        },
      };
    case "worktreeEntered":
      return {
        case: "worktreeEntered" as const,
        value: { path: { text: "/repo/wt" }, branch: { text: "feature/x" } },
      };
    case "worktreeLeft":
      return {
        case: "worktreeLeft" as const,
        value: { outcome: { case: "kept" as const, value: { path: { text: "/repo/wt" } } } },
      };
  }
};

export const separationRow = (arm: SeparationArm, overrides?: Partial<RowInit>): FeedRow =>
  feedRow(
    {
      case: "separation",
      value: {
        label: { text: `separation: ${arm}` },
        kind: separationKind(arm) as never,
        tokens: { beforeText: "180k", afterText: "12k" },
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
            contextTokens: { tokens: init?.contextTokens ?? 184_320n },
            lastRequest: { atMs: init?.lastRequestAtMs ?? 1_000n },
            model: { model: agentModel("opus") },
            compact: {
              models: [{ model: agentModel("opus") }, { model: agentModel("haiku") }],
              scopes: [SessionCompactScope.ALL, SessionCompactScope.PROMPTS, SessionCompactScope.RESPONSES],
            },
          },
        },
      },
    },
    overrides,
  );

export const coldGateResolvedRow = (choice: ColdGateChoice, overrides?: Partial<RowInit>): FeedRow =>
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
                ? { case: "compact", value: { model: { model: agentModel("haiku") } } }
                : { case: choice, value: {} },
          },
        },
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

const mergeTabState = (state: "live" | "parked" | "settled") => {
  if (state === "live") return { case: "live" as const, value: {} };
  if (state === "parked") {
    return { case: "parked" as const, value: { line: { text: "paused for your answer" } } };
  }
  return {
    case: "settled" as const,
    value: { endedAtMs: 9_000n, outcome: { case: "succeeded" as const, value: {} } },
  };
};

const mergeTabKindValue = (kind: MergeTabKind, state: "live" | "parked" | "settled") => {
  const base = { state: mergeTabState(state) };
  switch (kind) {
    case "queue":
      return {
        case: "queue" as const,
        value: {
          ...base,
          queue: {
            ahead: [
              {
                workspace: { ref: workspaceRef("ws-ahead") },
                label: { text: "ws-ahead" },
                status: { case: "waiting" as const, value: {} },
              },
            ],
            current: {
              workspace: { ref: workspaceRef() },
              label: { text: "ws-1" },
              status: { case: "merging" as const, value: { activeTab: { text: "tests", round: 2 } } },
            },
            behind: [
              {
                workspace: { ref: workspaceRef("ws-behind") },
                label: { text: "ws-behind" },
                status: { case: "waiting" as const, value: {} },
              },
            ],
          },
        },
      };
    case "merge":
      return {
        case: "merge" as const,
        value: { ...base, lines: [{ text: "cherry-picked 3 commits" }, { text: "no conflicts" }] },
      };
    case "tests":
      return {
        case: "tests" as const,
        value: {
          ...base,
          suites: [
            {
              name: "vitest",
              state: { case: "passed" as const, value: {} },
              output: [
                { text: "PASS ", paintClass: "ok" },
                { text: "24 tests", paintClass: "" },
              ],
            },
            {
              name: "ert",
              state: { case: "failed" as const, value: {} },
              output: [{ text: "FAIL ", paintClass: "err" }],
            },
            {
              name: "go",
              state: { case: "running" as const, value: {} },
              output: [{ text: "running", paintClass: "dim" }],
            },
          ],
        },
      };
    default:
      return { case: kind, value: base } as never;
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
        kind: mergeTabKindValue(kind, state) as FeedMergeTab["kind"],
      },
    },
    overrides,
  );

export const MERGE_RESULTS = ["update", "success", "error"] as const;

export const mergeUnit = (
  result: "update" | "success" | "failed" | "abandoned",
): FeedTurnActivity["unit"] => ({
  case: "merge",
  value: {
    head: {
      glyph: { icon: "merge" },
      label: { text: "merging ws-1" },
      runtime: { startedAtMs: 1_000n },
      fold: { folded: false },
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
                    : { case: "abandoned", value: {} },
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
        breadcrumbs: init?.breadcrumbs
          ? { crumbs: init.breadcrumbs.map((c) => ({ target: feedId(c.target), label: c.label })) }
          : undefined,
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
        headline: { text: "history could not be replayed", tone: "warn" },
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

/** The substatus arms that carry payload; everything else is empty. */
const substatusValue = (substatus: string): object => {
  if (substatus === "queued") return { position: 2, depth: 5 };
  if (substatus === "parked") return { line: "waiting on your answer" };
  return {};
};

/** Every activity kind arm, with a complete payload for each. */
export const FOOTER_ACTIVITY_KINDS: Record<string, object> = {
  notification: { text: "the agent addressed you" },
  contextBudget: { text: "84% of the window" },
  rateLimited: {
    session: { newsworthy: true, resetsAtS: 1_700n, utilization: 0.82, status: "warn" },
    weekly: { newsworthy: false, resetsAtS: 9_000n, utilization: 0.3, status: "ok" },
  },
  hook: { name: "PreToolUse" },
  retrying: { attempt: 3, status: "overloaded" },
  contextInjected: { text: "CLAUDE.md loaded" },
  wakeup: { wakeAtMs: 60_000n, reason: { text: "the cron fires" } },
  gatedCall: { text: "Bash npm test" },
  questionLead: { text: "How far should the port go?" },
  blockedOnUser: { detail: "answer the permission card" },
  coldGateCost: { text: "184k tokens uncached" },
  interrupting: { text: "stopping 3 agents" },
  mergingCommit: { sha: "abc1234", subject: "port the transport" },
  authenticating: { line: "opening the login terminal" },
  queryDied: { text: "the vendor query died" },
  closeBlocked: { text: "a turn is live" },
};

/** Which activity kinds each status arm legally carries. */
export const FOOTER_STATUS_ACTIVITIES: Record<string, readonly string[]> = {
  idle: ["notification", "contextBudget", "rateLimited"],
  thinking: ["hook", "retrying", "contextInjected", "notification", "contextBudget", "rateLimited"],
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

export function footerStatus(
  status: string,
  init?: { substatus?: string; activity?: string; activityAtMs?: bigint },
): FooterStatus["status"] {
  const substatuses = FOOTER_STATUS_SUBSTATUSES[status];
  const substatus = init?.substatus ?? substatuses[0];
  const activityKind = init?.activity;
  const value: Record<string, unknown> = {};
  if (substatuses.length > 0) {
    value.substatus = { case: substatus, value: substatusValue(substatus) };
  }
  if (activityKind) {
    value.activity = {
      at: { atMs: init?.activityAtMs ?? 3_000n },
      kind: { case: activityKind, value: FOOTER_ACTIVITY_KINDS[activityKind] },
    };
  }
  return { case: status, value } as FooterStatus["status"];
}

export const FOOTER_CHIPS = ["agents", "tasks", "shells", "monitors", "crons"] as const;
export type FooterChip = (typeof FOOTER_CHIPS)[number];

export const FOOTER_TOKENS_VERDICTS = ["complete", "incomplete", "invalid"] as const;

type FooterInit = {
  status?: string;
  substatus?: string;
  activity?: string;
  activityAtMs?: bigint;
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
      clock: { turnStartedAtMs: init?.turnStartedAtMs ?? 1_000n },
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
function footerExpandedInit() {
  return {
    tokens: {
      input: { value: "42.1k" },
      cacheRead: { value: "180k" },
      cacheWrite: { value: "3.2k" },
      output: { value: "1.1k" },
      thinking: { value: "800" },
      firstToken: { value: "1.2 s" },
      alarm: { text: "context is nearly full" },
      verdict: { verdict: { case: "incomplete" as const, value: { text: "usage still arriving" } } },
    },
    agents: {
      rows: [
        {
          target: feedId("agent-row"),
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
        { status: { status: { case: "completed" as const, value: {} } }, subject: { text: "read the protos" } },
      ],
    },
    shells: {
      rows: [
        {
          target: feedId("shell-row"),
          command: { text: "npm run watch" },
          runtime: { startedAtMs: 1_000n },
        },
      ],
    },
    monitors: {
      rows: [
        {
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

const warningDetail = (arm: TopbarWarningArm) => {
  switch (arm) {
    case "accounting":
      return {
        case: "accounting" as const,
        value: { lines: [{ text: "usage figures are estimates" }, { text: "cache reads uncounted" }] },
      };
    case "unmodeledTool":
      return {
        case: "unmodeledTool" as const,
        value: {
          toolName: { text: "mcp__weather__forecast" },
          argumentLines: [{ text: "city=Berlin" }, { text: "days=3" }],
        },
      };
    case "detachedUnmodeled":
      return {
        case: "detachedUnmodeled" as const,
        value: { toolName: { text: "mcp__weather__watch" }, startedAtMs: 1_000n },
      };
    case "sessionFault":
      return {
        case: "sessionFault" as const,
        value: { component: { text: "shim" }, detail: { text: "store writes rejected" } },
      };
    case "degradedWindow":
      return {
        case: "degradedWindow" as const,
        value: {
          component: { text: "store" },
          reason: { text: "disk pressure" },
          beganAtMs: 1_000n,
          extent: { case: "open" as const, value: {} },
        },
      };
  }
};

export const topbarWarning = (arm: TopbarWarningArm): MessageInitShape<typeof TopbarViewSchema>["warnings"] extends infer _ ? TopbarWarning : never =>
  ({
    line: { text: `warning: ${arm}` },
    detail: warningDetail(arm),
  }) as unknown as TopbarWarning;

/** The closed extent of the degraded-window detail, drawn without a tick. */
export const degradedWindowClosedWarning = (): TopbarWarning =>
  ({
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
  }) as unknown as TopbarWarning;

type TopbarInit = {
  title?: string;
  sessionLine?: string;
  account?: "loggedIn" | "loggedOut";
  email?: string;
  tone?: string;
  glyph?: string;
  connectivityTitle?: string;
  models?: { name: string; displayName: string; description: string }[];
  selected?: string;
  contextText?: string;
  breakdown?: boolean;
  warnings?: TopbarWarning[];
};

export function topbarView(init?: TopbarInit): TopbarView {
  const models = init?.models ?? [
    { name: "opus", displayName: "Opus", description: "the big one" },
    { name: "sonnet", displayName: "Sonnet", description: "the fast one" },
  ];
  return create(TopbarViewSchema, {
    title: { text: init?.title ?? "port the webapp" },
    sessionLine: { text: init?.sessionLine ?? "session 3 of the overhaul" },
    modelSelector: {
      selected: modelOption(init?.selected ?? models[0].name, models[0].displayName, models[0].description),
      options: models.map((m) => modelOption(m.name, m.displayName, m.description)),
    },
    connectivity: {
      tone: init?.tone ?? "ok",
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
                    { label: "system prompt", tokens: 12_000n, sharePermille: 65, emphasized: true, depth: 0 },
                    { label: "memory files", tokens: 4_000n, emphasized: false, depth: 1 },
                  ],
                },
              ],
            },
    },
    account:
      init?.account === "loggedOut"
        ? { state: { case: "loggedOut", value: {} } }
        : { state: { case: "loggedIn", value: { email: init?.email ?? "dev@example.test" } } },
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

type RosterRowInit = {
  id?: string;
  name?: string;
  status?: RosterStatusArm;
  attention?: boolean;
  priority?: string;
  current?: boolean;
  closed?: boolean;
  detail?: boolean;
  when?: "lastSelected" | "merged";
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
    when: {
      shown: { case: init?.when ?? "lastSelected", value: { atMs: init?.whenAtMs ?? 1_000n } },
    },
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

export function roster(init?: { rows?: RosterRow[]; taskRows?: RosterRow[]; merged?: RosterRow[]; current?: string }): WorkspaceRoster {
  const rows = init?.rows ?? [rosterRow()];
  return create(WorkspaceRosterSchema, {
    repository: {
      sections: [
        {
          key: { repository: repositoryRef() },
          header: { label: { text: "doom" } },
          rows: { rows },
        },
      ],
    },
    task: {
      sections: [
        {
          key: { taskId: "task-1" },
          header: { label: { text: "the overhaul" }, done: { done: false } },
          rows: { rows: init?.taskRows ?? rows },
        },
      ],
    },
    recentlyMerged: {
      header: { label: { text: "recently merged" } },
      rows: { rows: init?.merged ?? [rosterRow({ id: "ws-merged", status: "merged", when: "merged" })] },
    },
    current: { workspace: workspaceRef(init?.current ?? WORKSPACE_ID) },
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

export const HOLD_ARMS = ["shutdown", "keepAlive", "sessionStarting", "buildRefresh"] as const;
export type HoldArm = (typeof HOLD_ARMS)[number];

const classificationValue = (arm: HoldClassificationArm, accepted?: boolean) => {
  switch (arm) {
    case "classifying":
      return { case: "classifying" as const, value: {} };
    case "interject":
      return { case: "interject" as const, value: { rationale: "it changes the current work" } };
    case "holdForTurnEnd":
      return {
        case: "holdForTurnEnd" as const,
        value: { rationale: "it is a follow-up", accepted: { accepted: accepted ?? false } },
      };
    case "uninterruptibleTurn":
      return { case: "uninterruptibleTurn" as const, value: { command: SessionCommand.COMPACT } };
    case "classificationError":
      return { case: "classificationError" as const, value: { detail: "the classifier timed out" } };
  }
};

const holdValue = (arm: HoldArm) => {
  switch (arm) {
    case "shutdown":
      return { case: "shutdown" as const, value: { scheduleId: "sched-1" } };
    case "keepAlive":
      return { case: "keepAlive" as const, value: { turn: turnId("turn-live") } };
    case "sessionStarting":
      return { case: "sessionStarting" as const, value: {} };
    case "buildRefresh":
      return { case: "buildRefresh" as const, value: {} };
  }
};

export function heldPrompt(init?: {
  turn?: string;
  text?: string;
  classification?: HoldClassificationArm;
  hold?: HoldArm;
  accepted?: boolean;
}): HeldPrompt {
  return create(HeldPromptSchema, {
    turn: turnId(init?.turn ?? "turn-held"),
    said: userSaid(init?.text ?? "also fix the footer"),
    queuedAt: { atMs: 3_000n },
    classification: classificationValue(init?.classification ?? "interject", init?.accepted) as never,
    hold: holdValue(init?.hold ?? "keepAlive") as never,
  });
}

export const heldOfferItem = (headline = "your merge is queued behind 2 others"): DaemonHoldItem =>
  ({
    item: { case: "offer", value: { offer: { case: "mergeDequeue", value: { headline: { text: headline } } } } },
  }) as unknown as DaemonHoldItem;

export function holdTray(init?: { heading?: string; items?: DaemonHoldItem[] }): DaemonHoldTray {
  return create(DaemonHoldTraySchema, {
    heading: { text: init?.heading ?? "held prompts" },
    items:
      init?.items ??
      ([{ item: { case: "prompt", value: heldPrompt() } }] as unknown as DaemonHoldItem[]),
  });
}

/** The fake daemon's default tray: heading present, nothing held. */
export const emptyTray = (): DaemonHoldTray => holdTray({ items: [] });

// ---------------------------------------------------------------------------
// Failures (client-local arms the webapp's own overlay mints)
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

const failureValue = (arm: ClientFailureArm) => {
  switch (arm) {
    case "daemonUnreachable":
      return { case: "daemonUnreachable" as const, value: { closeCode: 1006, closeReason: "abnormal" } };
    case "workspaceGone":
      return { case: "workspaceGone" as const, value: {} };
    case "bootFailed":
      return { case: "bootFailed" as const, value: { cause: "the page address had no workspace" } };
    case "controlPlaneFailed":
      return {
        case: "controlPlaneFailed" as const,
        value: { what: "WatchFooter", cause: "the stream refused the token" },
      };
    case "frameUndecodable":
      return {
        case: "frameUndecodable" as const,
        value: { cause: "unknown field 999", frameHead: "WatchFooterResponse" },
      };
    case "staleBundle":
      return { case: "staleBundle" as const, value: { detail: "the daemon shipped a newer bundle" } };
  }
};

export const clientFailure = (arm: ClientFailureArm): FailureKind =>
  create(FailureKindSchema, { kind: failureValue(arm) as never });

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
// Command panels (SubmitPrompt success arms)
// ---------------------------------------------------------------------------

export const COMMAND_PANEL_ARMS = ["status", "todos", "agents", "mcp", "context", "help"] as const;
export type CommandPanelArm = (typeof COMMAND_PANEL_ARMS)[number];

export const MCP_STATUS_ARMS = ["connected", "failed", "needsAuth", "pending", "disabled"] as const;
export const TODO_STATUS_ARMS = ["pending", "running", "completed"] as const;

const panelValue = (arm: CommandPanelArm) => {
  switch (arm) {
    case "status":
      return {
        case: "status" as const,
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
        case: "todos" as const,
        value: {
          rows: [
            { status: { case: "pending" as const, value: {} }, subject: "write fixtures" },
            { status: { case: "running" as const, value: {} }, subject: "write the harness" },
            { status: { case: "completed" as const, value: {} }, subject: "read the protos" },
          ],
        },
      };
    case "agents":
      return {
        case: "agents" as const,
        value: {
          rows: [
            { name: "reviewer", description: { text: "review the diff" } },
            { name: "explorer", description: { text: "search the repo" } },
          ],
        },
      };
    case "mcp":
      return {
        case: "mcp" as const,
        value: {
          rows: [
            { name: "weather", status: { case: "connected" as const, value: {} } },
            {
              name: "gmail",
              status: { case: "failed" as const, value: { detail: { text: "handshake refused" } } },
            },
            { name: "drive", status: { case: "needsAuth" as const, value: {} } },
            { name: "slack", status: { case: "pending" as const, value: {} } },
            { name: "figma", status: { case: "disabled" as const, value: {} } },
          ],
        },
      };
    case "context":
      return {
        case: "context" as const,
        value: {
          header: "context usage",
          categories: [{ label: "system prompt", figure: "12.0k", color: "blue" }],
          memoryFiles: [{ label: "CLAUDE.md", figure: "3.1k" }],
          mcpTools: [{ label: "weather", figure: "900" }],
          deferredBuiltinTools: [{ label: "WebFetch", figure: "120" }],
          systemTools: [{ label: "Bash", figure: "400" }],
          systemPromptSections: [{ label: "identity", figure: "1.2k" }],
          agents: [{ label: "reviewer", figure: "800" }],
          slashCommands: { line: "42 commands, 6.1k" },
          skills: { line: "8 skills, 2.2k", skills: [{ label: "graphify", figure: "700" }] },
          messageBreakdown: {
            planes: [{ label: "prompts", figure: "8.0k" }],
            toolCalls: [{ label: "Bash", figure: "22.0k" }],
            attachments: [{ label: "images", figure: "1.0k" }],
          },
          autoCompactLine: "auto-compact at 90%",
        },
      };
    case "help":
      return {
        case: "help" as const,
        value: {
          rows: [
            { command: "/clear", description: { text: "clear the context" } },
            { command: "/compact", description: { text: "compact the context" } },
          ],
        },
      };
  }
};

export const commandPanel = (arm: CommandPanelArm): SubmitPromptCommandPanel =>
  create(SubmitPromptCommandPanelSchema, { panel: panelValue(arm) as never });
