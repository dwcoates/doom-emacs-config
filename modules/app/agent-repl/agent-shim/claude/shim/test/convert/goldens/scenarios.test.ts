/**
 * WHAT EACH REAL CAPTURE FOLDS INTO — one row per scenario, one test per row.
 *
 * The tables below are the CONTRACT the goldens hold the converter to: the unit
 * kinds a scenario's stream produces, the session-update arms it implies, and
 * the terminal it ends on. Each was read off the fold's output and then checked
 * against the capture's own `tool_use` census, so a row states what the vendor
 * actually did rather than what the fold happens to do today. A row that changes
 * is either a converter regression or a new vendor shape, and both want a human.
 *
 * WHY SOME KINDS ARE ABSENT EVERYWHERE: the run's own model never called
 * `Glob`, `Grep`, `Artifact`, `ScheduleWakeup`, `EnterWorktree` or
 * `ExitWorktree` — it reached for `Bash` and `Skill` instead — so no capture
 * exercises those arms. That is an EVIDENCE GAP recorded here, never a licence
 * to assert the arm from an invented fixture.
 */
import { describe, expect, it } from "vitest";
import {
  foldScenario,
  scenarioNames,
  sessionUpdateArms,
  terminalArms,
  unitKinds,
} from "./harness.js";

/**
 * The unit kinds each capture produces, in first-appearance order.
 *
 * NO `hook` UNIT: every hook firing these captures record SUCCEEDED, and a hook
 * record that draws nothing is never stored (owner ruling 2026-10-06,
 * convert/hooks.ts).
 */
const UNIT_KINDS: readonly (readonly [string, readonly string[]])[] = [
  ["account-usage", ["thinking", "response"]],
  ["artifact-publish-and-list", ["thinking", "read", "bash", "response"]],
  ["auto-compaction", ["thinking", "read", "response"]],
  ["bash-detached", ["thinking", "bash", "response"]],
  ["bash-foreground-completed", ["thinking", "bash", "response"]],
  ["bash-image-output", ["thinking", "bash", "read", "response"]],
  ["bash-interrupted-by-timeout", ["thinking", "bash", "response"]],
  ["bash-nonzero-exit", ["thinking", "bash", "response"]],
  ["bash-partial-output-with-spill", ["thinking", "bash", "read", "response"]],
  // The 2026-09-03 re-capture's first 6 filler turns answer directly with no
  // `thinking` unit at all — only turn 7 ("Recap, at length...") produces
  // one, so `response` (turn 1's) is the SECOND kind seen, `thinking` the
  // THIRD.
  ["compaction-directed", ["response", "thinking"]],
  ["context-injected-memory", ["thinking", "response"]],
  ["context-injected-skills", ["thinking", "response"]],
  ["context-usage", ["thinking", "read", "response"]],
  ["cron-create-list-delete", ["thinking", "response", "cron"]],
  ["ctrl-b-detach-of-foreground-subagent", ["thinking", "subagent", "bash", "response"]],
  ["ctrl-b-detach-of-foreground-work", ["thinking", "bash", "response", "read"]],
  ["diagnostics", ["thinking", "response"]],
  ["edit", ["thinking", "read", "edit", "response"]],
  ["fan-wide-cancel", ["thinking", "bash", "response"]],
  ["fast-mode", ["thinking", "response"]],
  ["glob", ["thinking", "bash", "response"]],
  ["grep-content-files-count", ["thinking", "bash", "response"]],
  ["held-turn-gate", ["thinking", "bash", "response"]],
  ["hook-blocked", ["thinking", "bash", "response"]],
  ["hook-cancelled", ["thinking", "bash", "response"]],
  ["hook-failed", ["thinking", "bash", "response"]],
  ["hook-succeeded", ["thinking", "bash", "response"]],
  ["ide-diagnostics-after-edit", ["thinking", "response", "read", "edit", "bash"]],
  ["identity-rotation-clear", ["thinking", "response"]],
  ["interrupt", ["thinking", "response"]],
  ["max-tokens", ["response"]],
  ["mcp-server-healths", ["thinking", "bash", "response"]],
  ["mcp-unmodeled-tool", ["thinking", "response", "mcpToolCall"]],
  ["model-changed", ["thinking", "response"]],
  ["monitor-deadline", ["thinking", "bash", "monitor", "response"]],
  ["monitor-persistent", ["thinking", "monitor", "response"]],
  ["permission-allow-once", ["thinking", "bash", "response"]],
  ["permission-allow-standing", ["thinking", "response", "bash"]],
  ["permission-denied-by-policy", ["thinking", "bash", "response"]],
  ["permission-denied-by-user", ["thinking", "bash", "response"]],
  ["permission-mode-changed", ["thinking", "response"]],
  ["permission-undecidable-parked", ["thinking", "bash"]],
  ["plan-mode-enter-exit", ["thinking", "planMode", "subagent", "response", "bash"]],
  ["prose-streamed", ["thinking", "response"]],
  ["push-notification-not-sent", ["thinking", "pushNotification", "response"]],
  ["push-notification-sent", ["thinking", "pushNotification", "response"]],
  ["question-free-text", ["thinking", "response"]],
  ["question-multi-select", ["thinking", "response"]],
  ["question-multiple-in-one-batch", ["thinking", "response"]],
  ["question-single-select", ["thinking", "response"]],
  ["question-unanswered", ["thinking", "response"]],
  ["read-whole-head-range", ["thinking", "response", "read"]],
  ["report-findings", ["thinking", "read", "reportFindings", "response"]],
  ["schedule-wakeup-schedule-and-stop", ["thinking", "skillUse", "bash", "response"]],
  ["send-message-queued-and-resumed", ["thinking", "response", "subagent", "sendMessage", "bash"]],
  ["skill-invocation", ["thinking", "skillUse", "read", "response"]],
  ["subagent-detached", ["thinking", "subagent", "bash", "response"]],
  ["subagent-sync-nested-activity", ["thinking", "subagent", "bash", "read", "response"]],
  ["task-acts-create-change-reject", ["thinking", "taskAct", "response"]],
  ["turn-stop-error-during-execution", ["thinking", "response"]],
  ["turn-stop-hook-stop", ["thinking", "response"]],
  ["turn-stop-max-budget-usd", ["thinking", "response"]],
  ["turn-stop-max-structured-output-retries", ["thinking", "response", "unmodeled"]],
  ["turn-stop-max-turns", ["thinking", "bash"]],
  ["vendor-answered-slash-commands", ["response"]],
  ["web-fetch", ["thinking", "webFetch", "response"]],
  ["web-search", ["thinking", "webSearch", "response"]],
  ["worktree-enter-exit-kept-and-removed", ["thinking", "response", "skillUse", "read", "bash", "write"]],
  ["write-created-and-updated", ["thinking", "read", "write", "response"]],
];

/** The session-update arms each capture produces, in first-appearance order. */
const SESSION_ARMS: readonly (readonly [string, readonly string[]])[] = [
  ["account-usage", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["artifact-publish-and-list", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["auto-compaction", ["mcpServer", "fastMode", "rateLimitStatus", "compacting"]],
  ["bash-detached", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["bash-foreground-completed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["bash-image-output", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["bash-interrupted-by-timeout", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["bash-nonzero-exit", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["bash-partial-output-with-spill", ["mcpServer", "fastMode", "rateLimitStatus"]],
  // The real capture's OPENING `system:init` states `fast_mode_state` but an
  // EMPTY `mcp_servers` array (the vendor has not yet probed the MCP catalog
  // at session start), and a `rate_limit_event` follows before any LATER
  // `system:init` finally carries a populated `mcp_servers` array — so
  // `fastMode` and `rateLimitStatus` are seen before `mcpServer`, not after.
  ["compaction-directed", ["fastMode", "rateLimitStatus", "mcpServer", "compacting"]],
  ["context-injected-memory", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["context-injected-skills", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["context-usage", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["cron-create-list-delete", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["ctrl-b-detach-of-foreground-subagent", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["ctrl-b-detach-of-foreground-work", ["fastMode", "rateLimitStatus"]],
  ["diagnostics", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["edit", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["fan-wide-cancel", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["fast-mode", ["fastMode", "rateLimitStatus"]],
  ["glob", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["grep-content-files-count", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["held-turn-gate", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["hook-blocked", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["hook-cancelled", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["hook-failed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["hook-succeeded", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["ide-diagnostics-after-edit", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["identity-rotation-clear", ["mcpServer", "fastMode", "rateLimitStatus", "identityRotated"]],
  ["interrupt", ["mcpServer", "fastMode"]],
  ["max-tokens", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["mcp-server-healths", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["mcp-unmodeled-tool", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["model-changed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["monitor-deadline", ["fastMode", "rateLimitStatus"]],
  ["monitor-persistent", ["fastMode", "rateLimitStatus"]],
  ["permission-allow-once", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["permission-allow-standing", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["permission-denied-by-policy", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["permission-denied-by-user", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["permission-mode-changed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["permission-undecidable-parked", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["plan-mode-enter-exit", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["prose-streamed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["push-notification-not-sent", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["push-notification-sent", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["question-free-text", ["fastMode", "rateLimitStatus"]],
  ["question-multi-select", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["question-multiple-in-one-batch", ["fastMode", "rateLimitStatus"]],
  ["question-single-select", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["question-unanswered", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["read-whole-head-range", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["report-findings", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["schedule-wakeup-schedule-and-stop", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["send-message-queued-and-resumed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["skill-invocation", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["subagent-detached", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["subagent-sync-nested-activity", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["task-acts-create-change-reject", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["turn-stop-error-during-execution", ["mcpServer", "fastMode"]],
  ["turn-stop-hook-stop", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["turn-stop-max-budget-usd", ["mcpServer", "fastMode"]],
  ["turn-stop-max-structured-output-retries", ["fastMode", "rateLimitStatus"]],
  ["turn-stop-max-turns", ["fastMode", "rateLimitStatus"]],
  ["vendor-answered-slash-commands", ["mcpServer", "fastMode"]],
  ["web-fetch", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["web-search", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["worktree-enter-exit-kept-and-removed", ["mcpServer", "fastMode", "rateLimitStatus"]],
  ["write-created-and-updated", ["mcpServer", "fastMode", "rateLimitStatus"]],
];

/**
 * The turn terminal EVERY turn of each capture ends on, in order.
 *
 * One entry per turn: a single-turn capture states one arm, and a multi-turn
 * one states every arm it produced. Reading only the first left a capture whose
 * SECOND turn started failing passing on the strength of its first.
 */
const TERMINALS: readonly (readonly [string, readonly string[]])[] = [
  ["account-usage", ["success.completed"]],
  ["artifact-publish-and-list", ["success.completed"]],
  // 16 turns, one per paced 40000-byte read: the window climbs ~10k tokens a
  // turn until the vendor compacts on its own between turns. Every turn ends
  // `success.completed`; the auto-compaction is not a terminal of its own.
  [
    "auto-compaction",
    [
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
    ],
  ],
  ["bash-detached", ["success.completed"]],
  ["bash-foreground-completed", ["success.completed"]],
  ["bash-image-output", ["success.completed"]],
  ["bash-interrupted-by-timeout", ["success.completed"]],
  ["bash-nonzero-exit", ["success.completed"]],
  ["bash-partial-output-with-spill", ["success.completed"]],
  // 9 turns, not 3: the 2026-09-03 re-capture added 6 filler turns (so
  // `/compact` has enough transcript to actually cut) ahead of the original
  // 3 (`/compact` itself plus the follow-up question) — all 9 end
  // `success.completed`.
  [
    "compaction-directed",
    [
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
      "success.completed",
    ],
  ],
  ["context-injected-memory", ["success.completed"]],
  ["context-injected-skills", ["success.completed"]],
  ["context-usage", ["success.completed"]],
  ["cron-create-list-delete", ["success.completed"]],
  ["ctrl-b-detach-of-foreground-subagent", ["success.completed"]],
  ["ctrl-b-detach-of-foreground-work", ["success.completed"]],
  ["diagnostics", ["success.completed"]],
  ["edit", ["success.completed"]],
  ["fan-wide-cancel", ["success.completed"]],
  ["fast-mode", ["success.completed"]],
  ["glob", ["success.completed"]],
  ["grep-content-files-count", ["success.completed"]],
  ["held-turn-gate", ["success.completed"]],
  ["hook-blocked", ["success.completed"]],
  ["hook-cancelled", ["success.completed"]],
  ["hook-failed", ["success.completed"]],
  ["hook-succeeded", ["success.completed"]],
  ["ide-diagnostics-after-edit", ["success.completed"]],
  ["identity-rotation-clear", ["success.completed", "success.completed", "success.completed"]],
  ["interrupt", ["success.interrupted"]],
  ["max-tokens", ["success.completed"]],
  ["mcp-server-healths", ["success.completed"]],
  ["mcp-unmodeled-tool", ["success.completed"]],
  ["model-changed", ["success.completed"]],
  ["monitor-deadline", ["success.completed"]],
  ["monitor-persistent", ["success.completed"]],
  ["permission-allow-once", ["success.completed"]],
  ["permission-allow-standing", ["success.completed"]],
  ["permission-denied-by-policy", ["success.completed"]],
  ["permission-denied-by-user", ["success.completed"]],
  ["permission-mode-changed", ["success.completed"]],
  ["permission-undecidable-parked", ["success.interrupted"]],
  ["plan-mode-enter-exit", ["success.completed"]],
  ["prose-streamed", ["success.completed"]],
  ["push-notification-not-sent", ["success.completed"]],
  ["push-notification-sent", ["success.completed"]],
  ["question-free-text", ["success.completed"]],
  ["question-multi-select", ["success.completed"]],
  ["question-multiple-in-one-batch", ["success.completed"]],
  ["question-single-select", ["success.completed"]],
  ["question-unanswered", ["success.completed"]],
  ["read-whole-head-range", ["success.completed"]],
  ["report-findings", ["success.completed"]],
  ["schedule-wakeup-schedule-and-stop", ["success.completed"]],
  ["send-message-queued-and-resumed", ["success.completed"]],
  ["skill-invocation", ["success.completed"]],
  ["subagent-detached", ["success.completed"]],
  ["subagent-sync-nested-activity", ["success.completed"]],
  ["task-acts-create-change-reject", ["success.completed"]],
  ["turn-stop-error-during-execution", ["success.interrupted"]],
  ["turn-stop-hook-stop", ["success.completed"]],
  ["turn-stop-max-budget-usd", ["failure.budgetExhausted"]],
  ["turn-stop-max-structured-output-retries", ["success.completed"]],
  ["turn-stop-max-turns", ["failure.maxTurns"]],
  ["vendor-answered-slash-commands", ["success.completed"]],
  ["web-fetch", ["success.completed"]],
  ["web-search", ["success.completed"]],
  ["worktree-enter-exit-kept-and-removed", ["success.completed"]],
  ["write-created-and-updated", ["success.completed"]],
];

describe("the unit kinds a capture produces", () => {
  it("has a row for every committed scenario", () => {
    expect(UNIT_KINDS.map(([name]) => name)).toEqual(scenarioNames());
  });

  it.each(UNIT_KINDS)("%s", (scenario, expected) => {
    expect(unitKinds(foldScenario(scenario))).toEqual(expected);
  });
});

describe("the session-update arms a capture implies", () => {
  it("has a row for every committed scenario", () => {
    expect(SESSION_ARMS.map(([name]) => name)).toEqual(scenarioNames());
  });

  it.each(SESSION_ARMS)("%s", (scenario, expected) => {
    expect(sessionUpdateArms(foldScenario(scenario))).toEqual(expected);
  });
});

describe("the terminals a capture's turns end on", () => {
  it("has a row for every committed scenario", () => {
    expect(TERMINALS.map(([name]) => name)).toEqual(scenarioNames());
  });

  it.each(TERMINALS)("%s", (scenario, expected) => {
    expect(terminalArms(foldScenario(scenario))).toEqual([...expected]);
  });
});

