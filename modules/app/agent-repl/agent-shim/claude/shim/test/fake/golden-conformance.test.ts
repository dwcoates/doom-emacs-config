/**
 * test/fake/golden-conformance.test.ts — THE MOCK, HELD AGAINST THE REAL VENDOR.
 *
 * # Why this file exists
 *
 * A mocked vendor written from our own reading of `sdk.d.ts` agrees with us BY
 * CONSTRUCTION, and a suite built on it can only ever confirm what we already
 * believed. The 69 captures under `testdata/captures/` are recordings of the
 * ACTUAL agent binary, so this file drives the mock and the golden through the
 * SAME fold and compares what comes out. A mock that drifts from the vendor
 * fails here rather than in production.
 *
 * # What one row states
 *
 * `golden` is what the real capture folds into; `mock` is what the mock's
 * scenario folds into. Both are the `AgentActivity` unit kinds in
 * first-appearance order. A row with no `diverges` asserts they are EQUAL — the
 * mock reproduces the vendor's shape exactly. A row WITH one records a real,
 * named difference, and still pins both sides, so either changing is a failure
 * that wants a human.
 *
 * # The two legitimate reasons a row diverges
 *
 *   - MODEL CHOICE. The capture ran a real model against a real prompt, and it
 *     chose its own tools: it read a file before editing it, it globbed with
 *     `Bash` instead of `Glob`, it answered before its last call. None of that
 *     is vendor SHAPE, and transcribing it would make the mock a recording of
 *     one model's habits. Where the model reached for `Bash` in place of a
 *     typed arm, the arm is one of the MANIFEST's recorded evidence gaps.
 *   - DECLARED, NOT CAPTURE-GROUNDED. The mock emits an arm no capture
 *     exercises. Those stay — a declared type is still a contract — but they
 *     are marked here so nobody mistakes them for observed behavior.
 *
 * A row marked KNOWN MOCK GAP is neither: it is a difference the mock SHOULD
 * close and has not.
 *
 * # What is deliberately excluded from the comparison
 *
 *   - THE `hook` UNIT. Every capture opens with a `SessionStart:startup` hook
 *     pair, because the machine that recorded them had a hook configured. A
 *     hook fires only if an operator installed one, so requiring the mock to
 *     emit it would be transcribing the capture ENVIRONMENT rather than the
 *     vendor. Both sides are compared with `hook` removed.
 *   - PARKING SCENARIOS (`!ctrl-b`, `!interrupt`, `!cancel-all`,
 *     `!perm-undecidable`, `!ask-unanswered`). Each waits on a caller's verb
 *     that this harness does not issue, so driving one here would hang rather
 *     than compare. They are exercised in `test/integration/`.
 *   - CAPTURES WITH NO SINGLE COUNTERPART: `fan-wide-cancel`,
 *     `context-budget-warning`, `held-turn-gate`, `question-multiple-in-one-batch`
 *     and `ctrl-b-detach-of-foreground-{work,subagent}` are driven by caller
 *     verbs or span several mock scenarios.
 */
import { describe, expect, it } from "vitest";
import { createFold } from "../../src/convert/fold.js";
import { activityOf, foldContext } from "../convert/fold-harness.js";
import { foldScenario, unitKinds } from "../convert/goldens/harness.js";
import { driveScenario } from "./harness.js";

/** One capture, the mock scenario that stands for it, and what each folds into. */
interface ConformanceRow {
  /** The capture directory under `testdata/captures/`. */
  readonly capture: string;
  /** The mock prompt that reproduces it. */
  readonly prompt: string;
  /** The real capture's unit kinds, first-appearance order, `hook` removed. */
  readonly golden: readonly string[];
  /** The mock's unit kinds, same order, same exclusion. */
  readonly mock: readonly string[];
  /** Set only when the two differ; the category is stated above the row. */
  readonly diverges?: string;
}

const ROWS: readonly ConformanceRow[] = [
  { capture: "account-usage", prompt: "!usage-available", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "artifact-publish-and-list",
    prompt: "!artifact-publish",
    golden: ["thinking", "read", "bash", "response"],
    mock: ["thinking", "artifact", "response"],
    // MODEL CHOICE: the run reached for Read + Bash instead of the Artifact tool, so the artifact arm has no golden (MANIFEST evidence gap).
    diverges: "MODEL CHOICE",
  },
  { capture: "bash-detached", prompt: "!bash-detach", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  { capture: "bash-foreground-completed", prompt: "!bash", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  {
    capture: "bash-image-output",
    prompt: "!bash-image",
    golden: ["thinking", "bash", "read", "response"],
    mock: ["thinking", "bash", "response"],
    // MODEL CHOICE: the run Read the captured image back; the mock's Bash result carries the image inline, which is the shape under test.
    diverges: "MODEL CHOICE",
  },
  { capture: "bash-interrupted-by-timeout", prompt: "!bash-timeout", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  { capture: "bash-nonzero-exit", prompt: "!bash-fail", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  {
    capture: "bash-partial-output-with-spill",
    prompt: "!bash-spill",
    golden: ["thinking", "bash", "read", "response"],
    mock: ["thinking", "bash", "response"],
    // MODEL CHOICE: the run Read the spill file back; the spill itself is what the mock reproduces.
    diverges: "MODEL CHOICE",
  },
  { capture: "compaction-directed", prompt: "!compact", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "context-injected-memory",
    prompt: "!memory",
    golden: ["thinking", "response"],
    mock: ["contextInjected", "thinking", "response"],
    // DECLARED, NOT CAPTURE-GROUNDED: no capture produces a contextInjected record at all (MANIFEST evidence gap), so the mock's arm is the declared type and the golden has none.
    diverges: "DECLARED, NOT CAPTURE-GROUNDED",
  },
  {
    capture: "context-injected-skills",
    prompt: "!skills-injected",
    golden: ["thinking", "response"],
    mock: ["contextInjected", "thinking", "response"],
    // DECLARED, NOT CAPTURE-GROUNDED: as context-injected-memory.
    diverges: "DECLARED, NOT CAPTURE-GROUNDED",
  },
  {
    capture: "context-usage",
    prompt: "!context-usage-drift",
    golden: ["thinking", "read", "response"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the run Read a file while the sampled figures drifted; context_usage is a control answer with no unit of its own.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "cron-create-list-delete",
    prompt: "!cron",
    golden: ["thinking", "response", "cron"],
    mock: ["thinking", "cron", "response"],
    // MODEL CHOICE: the run answered before its last cron call, so response precedes cron in the golden.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "diagnostics",
    prompt: "!ide-diagnostics",
    golden: ["thinking", "response"],
    mock: ["thinking", "edit", "response"],
    // MODEL CHOICE: the run answered without editing, so the vendor's diagnostics record has no write/edit to attach to.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "edit",
    prompt: "!edit",
    golden: ["thinking", "read", "edit", "response"],
    mock: ["thinking", "edit", "response"],
    // MODEL CHOICE: the run Read the file before editing it.
    diverges: "MODEL CHOICE",
  },
  { capture: "fast-mode", prompt: "!fast-on", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "glob",
    prompt: "!glob",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "glob", "response"],
    // MODEL CHOICE: the run globbed with Bash, so the Glob arm has no golden (MANIFEST evidence gap).
    diverges: "MODEL CHOICE",
  },
  {
    capture: "grep-content-files-count",
    prompt: "!grep-content",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "grep", "response"],
    // MODEL CHOICE: the run grepped with Bash, so the Grep arm has no golden (MANIFEST evidence gap).
    diverges: "MODEL CHOICE",
  },
  {
    capture: "hook-blocked",
    prompt: "!hook-blocked",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "edit", "response"],
    // MODEL CHOICE: the run's blocked call was Bash, the mock's is Edit; the hook arms under test are the same either way.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "hook-cancelled",
    prompt: "!hook-cancelled",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "edit", "response"],
    // MODEL CHOICE: as hook-blocked.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "hook-failed",
    prompt: "!hook-failed",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the run's failing hook fired around a Bash call the mock does not make.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "hook-succeeded",
    prompt: "!hook-success",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "read", "response"],
    // MODEL CHOICE: the run's succeeding hook fired around Bash, the mock's around Read.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "ide-diagnostics-after-edit",
    prompt: "!ide-diagnostics",
    golden: ["thinking", "response", "read", "edit", "bash"],
    mock: ["thinking", "edit", "response"],
    // MODEL CHOICE: the run read, edited and then verified with Bash; the mock makes the one edit the diagnostics attach to.
    diverges: "MODEL CHOICE",
  },
  { capture: "identity-rotation-clear", prompt: "!rotate", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  { capture: "max-tokens", prompt: "!max-tokens", golden: ["response"], mock: ["response"] },
  {
    capture: "mcp-server-healths",
    prompt: "!mcp-all",
    golden: ["thinking", "bash", "response"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the run used Bash while the healths were sampled; mcp_server is a control answer with no unit of its own.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "mcp-unmodeled-tool",
    prompt: "!unmodeled",
    golden: ["thinking", "response", "unmodeled"],
    mock: ["thinking", "unmodeled", "response"],
    // MODEL CHOICE: the run answered before its unmodeled call.
    diverges: "MODEL CHOICE",
  },
  { capture: "model-changed", prompt: "!model-fallback", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "monitor-deadline",
    prompt: "!monitor-deadline",
    golden: ["thinking", "bash", "monitor", "response"],
    mock: ["thinking", "monitor", "response"],
    // MODEL CHOICE: the run started something with Bash before monitoring it.
    diverges: "MODEL CHOICE",
  },
  { capture: "monitor-persistent", prompt: "!monitor-persistent", golden: ["thinking", "monitor", "response"], mock: ["thinking", "monitor", "response"] },
  { capture: "permission-allow-once", prompt: "!perm-allow-once", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  {
    capture: "permission-allow-standing",
    prompt: "!perm-allow-standing",
    golden: ["thinking", "response", "bash"],
    mock: ["thinking", "bash", "response"],
    // MODEL CHOICE: the run answered before the gated call.
    diverges: "MODEL CHOICE",
  },
  { capture: "permission-denied-by-policy", prompt: "!perm-deny-policy", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  { capture: "permission-denied-by-user", prompt: "!perm-deny-user", golden: ["thinking", "bash", "response"], mock: ["thinking", "bash", "response"] },
  {
    capture: "plan-mode-enter-exit",
    prompt: "!plan",
    golden: ["thinking", "planMode", "subagent", "response", "bash"],
    mock: ["thinking", "planMode", "response"],
    // MODEL CHOICE: the run spawned an agent and ran Bash inside plan mode.
    diverges: "MODEL CHOICE",
  },
  { capture: "prose-streamed", prompt: "!md", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  { capture: "push-notification-not-sent", prompt: "!push-config-off", golden: ["thinking", "pushNotification", "response"], mock: ["thinking", "pushNotification", "response"] },
  { capture: "push-notification-sent", prompt: "!push-sent", golden: ["thinking", "pushNotification", "response"], mock: ["thinking", "pushNotification", "response"] },
  { capture: "question-free-text", prompt: "!ask-free", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  { capture: "question-multi-select", prompt: "!ask-multi", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  { capture: "question-single-select", prompt: "!ask-single", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "read-whole-head-range",
    prompt: "!read",
    golden: ["thinking", "response", "read"],
    mock: ["thinking", "read", "response"],
    // MODEL CHOICE: the run answered before its reads.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "report-findings",
    prompt: "!findings",
    golden: ["thinking", "read", "reportFindings", "response"],
    mock: ["thinking", "reportFindings", "response"],
    // MODEL CHOICE: the run Read before reporting.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "schedule-wakeup-schedule-and-stop",
    prompt: "!wakeup-schedule",
    golden: ["thinking", "skillUse", "bash", "response"],
    mock: ["thinking", "scheduleWakeup", "response"],
    // MODEL CHOICE: the run reached for Skill + Bash, so the ScheduleWakeup arm has no golden (MANIFEST evidence gap).
    diverges: "MODEL CHOICE",
  },
  {
    capture: "send-message-queued-and-resumed",
    prompt: "!send-message",
    golden: ["thinking", "response", "subagent", "sendMessage", "bash"],
    mock: ["thinking", "sendMessage", "response"],
    // MODEL CHOICE: the run spawned the recipient and ran Bash; the mock addresses an existing one.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "skill-invocation",
    prompt: "!skill",
    golden: ["thinking", "skillUse", "read", "response"],
    mock: ["thinking", "skillUse", "response"],
    // MODEL CHOICE: the skill's document told the run to Read.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "subagent-detached",
    prompt: "!subagent-detached",
    golden: ["thinking", "subagent", "bash", "response"],
    mock: ["thinking", "subagent", "response"],
    // MODEL CHOICE: the run also ran Bash beside the detached spawn.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "subagent-sync-nested-activity",
    prompt: "!subagent",
    golden: ["thinking", "subagent", "bash", "read", "response"],
    mock: ["thinking", "subagent", "response", "read"],
    // MODEL CHOICE: the run's subagent ran Bash as well as Read.
    diverges: "MODEL CHOICE",
  },
  { capture: "task-acts-create-change-reject", prompt: "!task-create", golden: ["thinking", "taskAct", "response"], mock: ["thinking", "taskAct", "response"] },
  { capture: "turn-stop-error-during-execution", prompt: "!fail-execution", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  { capture: "turn-stop-hook-stop", prompt: "!fail-stop-hook", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  { capture: "turn-stop-max-budget-usd", prompt: "!fail-budget", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "turn-stop-max-structured-output-retries",
    prompt: "!fail-structured-output",
    golden: ["thinking", "response", "unmodeled"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the run's retries were around an unmodeled MCP call.
    diverges: "MODEL CHOICE",
  },
  {
    capture: "turn-stop-max-turns",
    prompt: "!fail-max-turns",
    golden: ["thinking", "bash"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the run was cut off inside a Bash call; the mock is cut off after its answer.
    diverges: "MODEL CHOICE",
  },
  { capture: "vendor-answered-slash-commands", prompt: "!slash", golden: ["response"], mock: ["response"] },
  { capture: "web-fetch", prompt: "!web-fetch", golden: ["thinking", "webFetch", "response"], mock: ["thinking", "webFetch", "response"] },
  { capture: "web-search", prompt: "!web-search", golden: ["thinking", "webSearch", "response"], mock: ["thinking", "webSearch", "response"] },
  {
    capture: "worktree-enter-exit-kept-and-removed",
    prompt: "!worktree-keep",
    golden: ["thinking", "response", "skillUse", "read", "bash", "write"],
    mock: ["thinking", "worktree", "response"],
    // MODEL CHOICE: the run used Skill + Bash to make the tree, so the Worktree arm has no golden (MANIFEST evidence gap).
    diverges: "MODEL CHOICE",
  },
  {
    capture: "write-created-and-updated",
    prompt: "!write-create",
    golden: ["thinking", "read", "write", "response"],
    mock: ["thinking", "write", "response"],
    // MODEL CHOICE: the run Read the file before rewriting it.
    diverges: "MODEL CHOICE",
  },
];

/** The unit kinds the mock's scenario folds into, in first-appearance order. */
async function mockKinds(prompt: string): Promise<string[]> {
  const driven = await driveScenario([prompt]);
  const fold = createFold();
  const context = foldContext();
  const kinds: string[] = [];
  for (const message of driven.messages) {
    for (const entry of fold.onSdkMessage(message, context).entries) {
      const kind = activityOf(entry)?.item.case;
      if (kind !== undefined && kind !== "hook" && !kinds.includes(kind)) kinds.push(kind);
    }
  }
  return kinds;
}

describe("the mock, against the real captures", () => {
  it.each(ROWS.map((row) => [row.capture, row] as const))(
    "%s — the golden still folds into the recorded kinds",
    (_name, row) => {
      expect(unitKinds(foldScenario(row.capture)).filter((kind) => kind !== "hook")).toEqual(
        row.golden,
      );
    },
  );

  it.each(ROWS.map((row) => [row.prompt, row] as const))(
    "%s — the mock still folds into the recorded kinds",
    async (_name, row) => {
      expect(await mockKinds(row.prompt)).toEqual(row.mock);
    },
  );

  it.each(
    ROWS.filter((row) => row.diverges === undefined).map((row) => [row.capture, row] as const),
  )("%s — the mock reproduces the vendor's shape exactly", (_name, row) => {
    expect(row.mock).toEqual(row.golden);
  });

  it("states a reason for every row that diverges, and none for one that does not", () => {
    // A row cannot be marked divergent while agreeing, and cannot disagree
    // silently — which is the only thing that would let drift accumulate here.
    for (const row of ROWS) {
      const agrees = JSON.stringify(row.mock) === JSON.stringify(row.golden);
      expect({ capture: row.capture, agrees, marked: row.diverges !== undefined }).toEqual({
        capture: row.capture,
        agrees,
        marked: !agrees,
      });
    }
  });

  it("maps every capture at most once", () => {
    expect(new Set(ROWS.map((row) => row.capture)).size).toBe(ROWS.length);
  });
});
