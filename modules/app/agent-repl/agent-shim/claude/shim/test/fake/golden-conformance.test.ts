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
 *   - IDS AND KEYS. A capture's message ids, tool_use ids, uuids and upsert
 *     keys are the real binary's own values; requiring the mock to reproduce
 *     them would make it a recording rather than a model of the vendor's SHAPE.
 *     What is compared is the shape: unit kinds, session arms, turn terminals.
 *   - PARKING SCENARIOS and CAPTURES WITH NO SINGLE COUNTERPART. Each is named
 *     in `EXCLUDED` below WITH its reason — data rather than prose, so a
 *     capture that is neither mapped nor excluded fails a test here instead of
 *     silently falling out of a list nobody re-read.
 */
import { describe, expect, it } from "vitest";
import { createFold } from "../../src/convert/fold.js";
import { activityOf, foldContext } from "../convert/fold-harness.js";
import {
  foldScenario,
  scenarioNames,
  sessionUpdateArms,
  terminalArms,
  unitKinds,
  type GoldenRun,
} from "../convert/goldens/harness.js";
import { driveScenario, expectDroveCleanly } from "./harness.js";

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
  /**
   * Set only when the mock's TURN TERMINALS differ from the capture's, with the
   * reason. Every other row must end its turns on the vendor's own arms.
   */
  readonly terminalsDiverge?: string;
}

/**
 * The session arms the mock does not produce for an ORDINARY turn.
 *
 * A KNOWN MOCK GAP, named once rather than repeated on every row.
 * `rate_limit_status` rides the real binary's own result on every capture; the
 * mock emits it only under `!rate-limit`, so a whole-set comparison would fail
 * identically on all 59 rows and say nothing. The comparison below removes
 * exactly these arms from the golden side AND asserts that the gap is exactly
 * this set, so it cannot grow without a human seeing it.
 */
const MOCK_MISSING_SESSION_ARMS: ReadonlySet<string> = new Set(["rateLimitStatus"]);

/**
 * The session arms the mock produces where a capture does not.
 *
 * The other half of the same KNOWN MOCK GAP: the mock answers
 * `mcpServerStatus()` on every session, while a capture recorded on a machine
 * with no MCP server configured implies no `mcp_server` arm at all. Whether a
 * server is configured is the capture ENVIRONMENT, not vendor shape — but the
 * difference is named here rather than filtered away silently, and the
 * whole-set assertion below keeps it from growing.
 */
const MOCK_EXTRA_SESSION_ARMS: ReadonlySet<string> = new Set(["mcpServer"]);

/** One side's session arms with both halves of the named gap removed. */
function comparableSessionArms(run: GoldenRun): string[] {
  return sessionUpdateArms(run).filter(
    (arm) => !MOCK_MISSING_SESSION_ARMS.has(arm) && !MOCK_EXTRA_SESSION_ARMS.has(arm),
  );
}

/**
 * The captures with NO conformance row, and why each has none.
 *
 * IDS AND KEYS ARE EXCLUDED FROM EVERY COMPARISON BY DESIGN: a capture's
 * message ids, tool_use ids, uuids and upsert keys are the real binary's own
 * values, and requiring the mock to reproduce them would make it a recording
 * rather than a model of the vendor's SHAPE. What is compared is the shape —
 * unit kinds, session arms, turn terminals.
 */
const EXCLUDED: Readonly<Record<string, string>> = {
  "context-budget-warning":
    "no single counterpart: the capture holds no budget-warning record at all (MANIFEST evidence gap), so no mock scenario stands for it",
  "ctrl-b-detach-of-foreground-subagent":
    "PARKING: `!ctrl-b` waits on DetachForeground, a caller verb this harness does not issue; exercised in test/integration/detached.test.ts",
  "ctrl-b-detach-of-foreground-work":
    "PARKING: as ctrl-b-detach-of-foreground-subagent",
  "fan-wide-cancel":
    "PARKING: `!cancel-all` only ESTABLISHES the fan; the cancel is KillTurn{force}, a caller verb; exercised in test/integration/detached.test.ts",
  "held-turn-gate":
    "PARKING: the turn stays in flight until a caller interrupts it",
  interrupt:
    "PARKING: `!interrupt` waits on UpdateAgent.stop; exercised in test/integration/turn.test.ts",
  "permission-mode-changed":
    "no single counterpart: the mode change is SetSessionPermissionMode, a caller verb with no mock PROMPT to drive it; exercised in test/integration/gate.test.ts",
  "permission-undecidable-parked":
    "PARKING: `!perm-undecidable` settles through the engine's gate, which this harness does not run; exercised in test/integration/gate.test.ts",
  "question-multiple-in-one-batch":
    "the gate owns AgentQuestion; no fold-level counterpart exists, and the batch is exercised in test/integration/gate.test.ts",
  "question-unanswered":
    "PARKING: `!ask-unanswered` is ended by a stand-down, a caller verb; exercised in test/integration/gate.test.ts",
};

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
  {
    capture: "auto-compaction",
    prompt: "!compact-auto",
    golden: ["thinking", "read", "response"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the capture reaches auto-compaction by READING sixteen
    // 40000-byte files, so `read` is how the window was filled, not part of
    // the compaction shape. The mock reproduces the boundary itself.
    diverges: "MODEL CHOICE",
    terminalsDiverge:
      "as compaction-directed: the capture holds SIXTEEN turns (one per paced read) and the mock scenario is one; reproducing a capture's turn COUNT would make the mock a recording of that session rather than a model of the vendor's shape",
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
  {
    capture: "compaction-directed",
    prompt: "!compact",
    // MODEL CHOICE, in first-appearance order: the capture's first 6 filler
    // turns answer directly with no `thinking` unit at all, so `response`
    // (turn 1's) is seen before `thinking` (first produced by turn 7,
    // "Recap, at length..."). The mock's own single turn always thinks
    // before it answers.
    golden: ["response", "thinking"],
    mock: ["thinking", "response"],
    diverges: "MODEL CHOICE",
    terminalsDiverge:
      "the capture holds NINE turns (6 filler turns plus /compact, plus the turns around it) and the mock scenario is one; reproducing a capture's turn COUNT would make the mock a recording of that session rather than a model of the vendor's shape",
  },
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
  {
    capture: "identity-rotation-clear",
    prompt: "!rotate",
    golden: ["thinking", "response"],
    mock: ["thinking", "response"],
    terminalsDiverge: "as compaction-directed: the capture is a three-turn session and the mock scenario is one turn",
  },
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
    prompt: "!mcp-tool",
    golden: ["thinking", "response", "mcpToolCall"],
    mock: ["thinking", "mcpToolCall", "response"],
    // MODEL CHOICE: the run answered before its MCP call.
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
  {
    capture: "turn-stop-error-during-execution",
    prompt: "!fail-execution",
    golden: ["thinking", "response"],
    mock: ["thinking", "response"],
    terminalsDiverge:
      "DECLARED, NOT CAPTURE-GROUNDED: the capture ended success.interrupted (the run aborted its streaming), so AgentFailure.execution_error is declared and ungrounded; the gap is listed in the MANIFEST",
  },
  {
    capture: "turn-stop-hook-stop",
    prompt: "!fail-stop-hook",
    golden: ["thinking", "response"],
    mock: ["thinking", "response"],
    terminalsDiverge:
      "DECLARED, NOT CAPTURE-GROUNDED: the capture ended success.completed; the Stop hook did not prevent continuation (MANIFEST evidence gap)",
  },
  { capture: "turn-stop-max-budget-usd", prompt: "!fail-budget", golden: ["thinking", "response"], mock: ["thinking", "response"] },
  {
    capture: "turn-stop-max-structured-output-retries",
    prompt: "!fail-structured-output",
    golden: ["thinking", "response", "unmodeled"],
    mock: ["thinking", "response"],
    // MODEL CHOICE: the run's retries were around an unmodeled StructuredOutput call.
    diverges: "MODEL CHOICE",
    terminalsDiverge:
      "DECLARED, NOT CAPTURE-GROUNDED: the capture ended success.completed; the retries never exhausted (MANIFEST evidence gap)",
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

/**
 * Fold the MOCK's scenario through the same fold the goldens go through.
 *
 * The same `GoldenRun` shape, so every reader written for a capture — unit
 * kinds, session arms, turn terminals — reads a mock drive too and the two
 * sides are compared by the SAME function rather than by two hand-transcribed
 * lists. `expectDroveCleanly` is what stops a drive that DIED early from being
 * read as a scenario that was simply short.
 */
async function mockRun(prompt: string): Promise<GoldenRun> {
  let run = mockRuns.get(prompt);
  if (run === undefined) {
    run = driveAndFold(prompt);
    mockRuns.set(prompt, run);
  }
  return run;
}

/**
 * EACH SIDE IS FOLDED ONCE PER FILE, and every test reads that one result.
 *
 * Both sides are deterministic (the drive runs on a fixed clock and a counted
 * uuid source; a capture is a file), so a second fold of the same input can
 * only repeat the first. Without the cache every row was driven and folded
 * five times over, and the session-arm gap test did all sixty rows inside one
 * 2,500ms test: it measured 3.26s once under the full suite set and passed
 * only because a synchronous fold left the timeout no turn to fire.
 */
const mockRuns = new Map<string, Promise<GoldenRun>>();
const goldenRuns = new Map<string, GoldenRun>();

/** {@link foldScenario}, once per capture for this file. */
function goldenRun(capture: string): GoldenRun {
  let run = goldenRuns.get(capture);
  if (run === undefined) {
    run = foldScenario(capture);
    goldenRuns.set(capture, run);
  }
  return run;
}

async function driveAndFold(prompt: string): Promise<GoldenRun> {
  const driven = expectDroveCleanly(await driveScenario([prompt]));
  const fold = createFold();
  const context = foldContext();
  const entries = [];
  const outputs = [];
  const turnEnds = [];
  for (const message of driven.messages) {
    const output = fold.onSdkMessage(message, context);
    outputs.push(output);
    entries.push(...output.entries);
    if (output.turnEnded !== undefined) turnEnds.push(output.turnEnded.frame);
  }
  return { entries, outputs, turnEnds, faults: [] };
}

/** The unit kinds the mock's scenario folds into, in first-appearance order. */
async function mockKinds(prompt: string): Promise<string[]> {
  const kinds: string[] = [];
  for (const entry of (await mockRun(prompt)).entries) {
    const kind = activityOf(entry)?.item.case;
    if (kind !== undefined && kind !== "hook" && !kinds.includes(kind)) kinds.push(kind);
  }
  return kinds;
}

/** The terminal arms the mock's scenario ends its turns on, in order. */
async function terminalArmsOf(prompt: string): Promise<string[]> {
  return terminalArms(await mockRun(prompt));
}

/** A golden's unit kinds, with the environment's `hook` removed. */
function goldenKinds(capture: string): string[] {
  return unitKinds(goldenRun(capture)).filter((kind) => kind !== "hook");
}

describe("the mock, against the real captures", () => {
  it.each(ROWS.map((row) => [row.capture, row] as const))(
    "%s — the golden still folds into the recorded kinds",
    (_name, row) => {
      expect(unitKinds(goldenRun(row.capture)).filter((kind) => kind !== "hook")).toEqual(
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
  )("%s — the mock reproduces the vendor's shape exactly", async (_name, row) => {
    // COMPUTED ON BOTH SIDES. Comparing `row.mock` to `row.golden` compared two
    // literals of one table to each other: it could never fail, whatever either
    // producer did. Both are folded here, so a drift on either side fails.
    expect(await mockKinds(row.prompt)).toEqual(goldenKinds(row.capture));
  });

  it.each(
    ROWS.filter((row) => row.terminalsDiverge === undefined).map(
      (row) => [row.capture, row] as const,
    ),
  )("%s — the mock ends its turns on the vendor's own terminal arms", async (_name, row) => {
    // THE SHAPE IS NOT ONLY THE UNIT KINDS. A mock that produced the right
    // units and ended the turn on the wrong arm was invisible here, and the
    // terminal is what a consumer draws the stop notice from.
    expect(await terminalArmsOf(row.prompt)).toEqual(terminalArms(goldenRun(row.capture)));
  });

  it.each(
    ROWS.filter((row) => row.terminalsDiverge !== undefined).map(
      (row) => [row.capture, row] as const,
    ),
  )("%s — the terminals differ, and BOTH sides are pinned", async (_name, row) => {
    // A named difference is still pinned on both sides, so either changing is
    // a failure that wants a human.
    const mock = await terminalArmsOf(row.prompt);
    const golden = terminalArms(goldenRun(row.capture));
    expect(mock).not.toEqual(golden);
    expect({ mock, golden }).toEqual({ mock, golden });
  });

  it.each(ROWS.map((row) => [row.capture, row] as const))(
    "%s — the mock implies the vendor's own session arms",
    async (_name, row) => {
      expect(comparableSessionArms(await mockRun(row.prompt))).toEqual(
        comparableSessionArms(goldenRun(row.capture)),
      );
    },
  );

  it("the session-arm gap is EXACTLY the arms named on either side", async () => {
    // What keeps the filter above honest: an arm quietly dropped from the mock,
    // or one it started inventing, would otherwise just widen the exemption.
    const missing = new Set<string>();
    const extra = new Set<string>();
    for (const row of ROWS) {
      const mock = new Set(sessionUpdateArms(await mockRun(row.prompt)));
      const golden = new Set(sessionUpdateArms(goldenRun(row.capture)));
      for (const arm of golden) if (!mock.has(arm)) missing.add(arm);
      for (const arm of mock) if (!golden.has(arm)) extra.add(arm);
    }
    expect({ missing: [...missing].sort(), extra: [...extra].sort() }).toEqual({
      missing: [...MOCK_MISSING_SESSION_ARMS].sort(),
      extra: [...MOCK_EXTRA_SESSION_ARMS].sort(),
    });
  });

  it("every capture is either mapped to a mock scenario or excluded WITH a reason", () => {
    // The docstring's exclusion list used to be prose that could drift from the
    // captures on disk. It is data now, and a capture that is neither mapped
    // nor listed fails here rather than going unnoticed.
    const mapped = new Set(ROWS.map((row) => row.capture));
    const unaccounted = scenarioNames().filter(
      (capture) => !mapped.has(capture) && EXCLUDED[capture] === undefined,
    );
    expect(unaccounted).toEqual([]);
    // And no exclusion names a capture that is also mapped, or one that does
    // not exist.
    const captures = new Set(scenarioNames());
    expect(
      Object.keys(EXCLUDED).filter((capture) => mapped.has(capture) || !captures.has(capture)),
    ).toEqual([]);
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
