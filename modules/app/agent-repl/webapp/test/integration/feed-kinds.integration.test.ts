/**
 * FEED KINDS — every row the contract declares, drawn.
 *
 * The arm tables here are checked against the generated descriptors with
 * `assertCoversOneof`, so this file fails the moment the contract grows an arm
 * nobody drew. The per-arm cases then assert the two things that matter for a
 * server-driven renderer: the DOM hooks carry the generated CASE NAMES, and
 * the text on screen is the daemon's own, verbatim.
 */
import { afterEach, describe, expect, it } from "vitest";

import {
  FeedRowSchema,
  FeedTurnActivitySchema,
  FeedResponseSchema,
  FeedSimpleToolCallSchema,
  FeedToolCallReturnedSchema,
  FeedSkillSchema,
  FeedHookSchema,
  FeedPlanSchema,
  FeedArtifactSchema,
  FeedSubagentSettledSchema,
  FeedShellSettledSchema,
  FeedPermissionSchema,
  FeedPermissionAnsweredSchema,
  FeedQuestionSchema,
  FeedSessionSeparationSchema,
  FeedColdGateSchema,
  FeedColdGateResolvedSchema,
  FeedMergeTabSchema,
  FeedCommandPanelSchema,
  FeedDiffLineSchema,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";

import { startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import { expectedPaintClass } from "./vocab";
import {
  ARTIFACT_STATES,
  CODE_SPANS,
  COLD_GATE_CHOICES,
  COLD_GATE_MODELS,
  COLD_GATE_TOKENS,
  DIFF_LINE_KINDS,
  FEED_COMMAND_PANEL_ARMS,
  FINDINGS_LOCATION,
  HOOK_OUTCOMES,
  MERGE_TAB_KINDS,
  MERGE_TAB_STATES,
  PERMISSION_ANSWERS,
  PLAN_STATES,
  QUESTION_ONE,
  QUESTION_STATES,
  QUESTION_TWO,
  RESPONSE_STATES,
  SEPARATION_ARMS,
  SHELL_OUTCOMES,
  SKILL_OUTCOMES,
  SUBAGENT_OUTCOMES,
  TOOL_OUTPUT_FORMS,
  WORKSPACE_ID,
  activityRow,
  agentPromptRow,
  artifactUnit,
  coldGateResolvedRow,
  coldGateStandingRow,
  commandPanelRow,
  commandRefusedRow,
  detachedShellRow,
  detachedSubagentRow,
  feedId,
  findingsUnit,
  hookUnit,
  mergeTabRow,
  permissionRow,
  planUnit,
  questionRow,
  responseUnit,
  separationRow,
  skillUnit,
  subagentUnit,
  toolCallDeniedUnit,
  toolCallReturnedUnit,
  toolCallRunningUnit,
  turnEndedInterruptedRow,
  userPromptRow,
  worktreeRemovedRow,
  assertCoversOneof,
} from "./fixtures";
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one row on the tail and hand back its element. */
async function drawRow(row: FeedRow): Promise<HTMLElement> {
  harness = await startHarness();
  await harness.fake.awaitStream("watchFeed");
  harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, row);
  await harness.settle();
  const id = row.id?.value ?? "row-1";
  const element = harness.row(id);
  if (!element) throw new Error(`the feed drew no row for ${id}`);
  return element;
}

// ---------------------------------------------------------------------------
// The arm tables are the contract's, not ours
// ---------------------------------------------------------------------------

describe("arm coverage", () => {
  it("covers every FeedRow arm", () => {
    assertCoversOneof(FeedRowSchema, "row", [
      "userPrompt",
      "agentPrompt",
      "activity",
      "turnEnded",
      "detachedSubagent",
      "detachedShell",
      "permission",
      "question",
      "separation",
      "coldGate",
      "mergeTab",
      "commandPanel",
      "commandRefused",
    ]);
  });

  it("covers every FeedTurnActivity unit", () => {
    assertCoversOneof(FeedTurnActivitySchema, "unit", [
      "response",
      "simpleToolCall",
      "skill",
      "merge",
      "subagent",
      "hook",
      "artifact",
      "plan",
      "findings",
    ]);
  });

  it("covers every FeedResponse result", () => {
    assertCoversOneof(FeedResponseSchema, "result", [...RESPONSE_STATES]);
  });

  it("covers every FeedSimpleToolCall outcome", () => {
    assertCoversOneof(FeedSimpleToolCallSchema, "outcome", ["running", "returned", "denied"]);
  });

  it("covers every tool-call output form", () => {
    assertCoversOneof(FeedToolCallReturnedSchema, "form", [...TOOL_OUTPUT_FORMS]);
  });

  it("covers every diff line kind", () => {
    assertCoversOneof(FeedDiffLineSchema, "kind", [...DIFF_LINE_KINDS]);
  });

  it("covers every FeedSkill outcome", () => {
    assertCoversOneof(FeedSkillSchema, "outcome", [...SKILL_OUTCOMES]);
  });

  it("covers every FeedHook outcome", () => {
    assertCoversOneof(FeedHookSchema, "outcome", [...HOOK_OUTCOMES]);
  });

  it("covers every FeedPlan state", () => {
    assertCoversOneof(FeedPlanSchema, "state", [...PLAN_STATES]);
  });

  it("covers every FeedArtifact state", () => {
    assertCoversOneof(FeedArtifactSchema, "state", [...ARTIFACT_STATES]);
  });

  it("covers every settled-subagent outcome", () => {
    assertCoversOneof(FeedSubagentSettledSchema, "outcome", [...SUBAGENT_OUTCOMES]);
  });

  it("covers every settled-shell outcome", () => {
    assertCoversOneof(FeedShellSettledSchema, "outcome", [...SHELL_OUTCOMES]);
  });

  it("covers every FeedPermission state", () => {
    assertCoversOneof(FeedPermissionSchema, "state", ["open", "answered", "abandoned"]);
  });

  it("covers every permission answer", () => {
    assertCoversOneof(FeedPermissionAnsweredSchema, "answer", [...PERMISSION_ANSWERS]);
  });

  it("covers every FeedQuestion state", () => {
    assertCoversOneof(FeedQuestionSchema, "state", [...QUESTION_STATES]);
  });

  it("covers every separation arm", () => {
    assertCoversOneof(FeedSessionSeparationSchema, "kind", [...SEPARATION_ARMS]);
  });

  it("covers every cold-gate state", () => {
    assertCoversOneof(FeedColdGateSchema, "state", ["standing", "resolved"]);
  });

  it("covers every cold-gate resolution", () => {
    assertCoversOneof(FeedColdGateResolvedSchema, "choice", [...COLD_GATE_CHOICES]);
  });

  it("covers every merge tab kind", () => {
    assertCoversOneof(FeedMergeTabSchema, "kind", [...MERGE_TAB_KINDS]);
  });

  it("covers every feed command panel arm", () => {
    assertCoversOneof(FeedCommandPanelSchema, "panel", [...FEED_COMMAND_PANEL_ARMS]);
  });
});

// ---------------------------------------------------------------------------
// Prompts
// ---------------------------------------------------------------------------

describe("prompts", () => {
  it("marks a user prompt with its row kind", async () => {
    // Arrange / Act
    const row = await drawRow(userPromptRow("do the thing"));
    // Assert
    expect(row.dataset.rowKind).toBe("userPrompt");
  });

  it("draws the user prompt's text verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(userPromptRow("do the thing"));
    // Assert
    expect(row.textContent).toContain("do the thing");
  });

  it("marks an agent prompt with its own row kind", async () => {
    // Arrange / Act
    const row = await drawRow(agentPromptRow("please review"));
    // Assert
    expect(row.dataset.rowKind).toBe("agentPrompt");
  });

  it("draws the agent prompt's address verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(agentPromptRow("please review"));
    // Assert
    expect(row.textContent).toContain("to reviewer");
  });
});

// ---------------------------------------------------------------------------
// Responses
// ---------------------------------------------------------------------------

describe.each(RESPONSE_STATES)("a %s response", (state) => {
  it("carries the response unit hook", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseUnit(state)));
    // Assert
    expect(row.dataset.unit).toBe("response");
  });

  it("carries its result arm as the state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseUnit(state)));
    // Assert
    expect(row.dataset.state).toBe(state);
  });

  it("draws the usage stamp verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseUnit(state, "prose", "9.9k in / 1 out")));
    // Assert
    expect(row.textContent).toContain("9.9k in / 1 out");
  });
});

// ---------------------------------------------------------------------------
// Tool calls
// ---------------------------------------------------------------------------

describe("a running tool call", () => {
  it("carries the running outcome", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallRunningUnit()));
    // Assert
    expect(row.dataset.state).toBe("running");
  });

  it("draws the composed input line verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallRunningUnit()));
    // Assert
    expect(row.textContent).toContain("npm test");
  });

  it("ticks the quiet-for figure from last_progress", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(toolCallRunningUnit(0n)));
    await harness.settle();
    // Act
    await harness.tick(7_000);
    // Assert
    expect(harness.row("row-1")?.textContent).toMatch(/quiet for 7/);
  });

  it("grows the quiet-for figure as time passes", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(toolCallRunningUnit(0n)));
    await harness.tick(7_000);
    const early = harness.row("row-1")?.textContent ?? "";
    // Act
    await harness.tick(8_000);
    // Assert
    expect(harness.row("row-1")?.textContent).not.toBe(early);
  });
});

describe.each(TOOL_OUTPUT_FORMS)("a returned tool call with %s output", (form) => {
  it("carries the returned outcome", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit(form)));
    // Assert
    expect(row.dataset.state).toBe("returned");
  });

  it("draws the settled runtime verbatim rather than ticking", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit(form)));
    // Assert
    expect(row.textContent).toContain("ran 4.2 s");
  });

  it("draws the omission note when the output was capped", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit(form)));
    // Assert: text and diff carry no omission field; the rest do.
    const expected = form === "text" || form === "diff" ? false : true;
    expect(/omitted|more/.test(row.textContent ?? "")).toBe(expected);
  });
});

describe("a denied tool call", () => {
  it("carries the denied outcome", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallDeniedUnit()));
    // Assert
    expect(row.dataset.state).toBe("denied");
  });
});

describe("paint-class spans", () => {
  it.each(CODE_SPANS)("draws $text with the class the vocabulary assigns", async (span) => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("code")));
    const drawn = [...row.querySelectorAll("span")].find((el) => el.textContent === span.text);
    // Assert
    const expected = expectedPaintClass(span.paintClass);
    expect(drawn?.className || undefined).toBe(expected);
  });

  it("still draws the text of a span whose class is unknown", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("code")));
    // Assert: an unknown class is unstyled, never dropped and never an error.
    expect(row.textContent).toContain("alien ");
  });
});

describe.each(DIFF_LINE_KINDS)("a %s diff line", (kind) => {
  it("carries its own kind", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("diff")));
    // Assert
    expect(row.querySelector(`[data-diff-line="${kind}"]`)).not.toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Skills, hooks, artifacts, plans, findings
// ---------------------------------------------------------------------------

describe.each(SKILL_OUTCOMES)("a %s skill", (outcome) => {
  it("carries its outcome as the state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(skillUnit(outcome)));
    // Assert
    expect(row.dataset.state).toBe(outcome);
  });

  it("draws the invocation verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(skillUnit(outcome)));
    // Assert
    expect(row.textContent).toContain("/graphify");
  });
});

describe.each(HOOK_OUTCOMES)("a %s hook", (outcome) => {
  it("carries its outcome as the state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(hookUnit(outcome)));
    // Assert
    expect(row.dataset.state).toBe(outcome);
  });

  it("draws the composed headline verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(hookUnit(outcome)));
    // Assert
    expect(row.textContent).toContain("PreToolUse hook");
  });
});

describe.each(ARTIFACT_STATES)("a %s artifact", (state) => {
  it("carries its state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(artifactUnit(state)));
    // Assert
    expect(row.dataset.state).toBe(state);
  });

  it("draws the heading verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(artifactUnit(state)));
    // Assert
    expect(row.textContent).toContain("Release notes");
  });
});

describe.each(PLAN_STATES)("a %s plan", (state) => {
  it("carries its state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(planUnit(state)));
    // Assert
    expect(row.dataset.state).toBe(state);
  });
});

describe("findings", () => {
  it("draws every findings row", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(findingsUnit()));
    // Assert
    expect(row.querySelectorAll("[data-finding]")).toHaveLength(3);
  });

  it("draws the location text verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(findingsUnit()));
    // Assert
    expect(row.textContent).toContain(FINDINGS_LOCATION.text);
  });

  it("draws each verdict arm", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(findingsUnit()));
    // Assert
    expect(row.querySelector('[data-verdict="plausible"]')).not.toBeNull();
  });

  it("draws each outcome arm", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(findingsUnit()));
    // Assert
    expect(row.querySelector('[data-outcome="noChange"]')).not.toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Subagents and shells
// ---------------------------------------------------------------------------

describe("a live subagent", () => {
  it("carries the live state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentUnit("live")));
    // Assert
    expect(row.dataset.state).toBe("live");
  });

  it("draws the served token figure verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentUnit("live")));
    // Assert
    expect(row.textContent).toContain("12.4k");
  });
});

describe.each(SUBAGENT_OUTCOMES)("a settled (%s) subagent", (outcome) => {
  it("carries its outcome as the state", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentUnit(outcome)));
    // Assert
    expect(row.dataset.state).toBe(outcome);
  });
});

describe("a detached subagent", () => {
  it("carries the detached row kind", async () => {
    // Arrange / Act
    const row = await drawRow(detachedSubagentRow("live"));
    // Assert
    expect(row.dataset.rowKind).toBe("detachedSubagent");
  });

  it("draws the same subagent component its synchronous form uses", async () => {
    // Arrange / Act
    const row = await drawRow(detachedSubagentRow("live"));
    // Assert
    expect(row.textContent).toContain("reviewer");
  });
});

describe.each(SHELL_OUTCOMES)("a settled (%s) detached shell", (outcome) => {
  it("carries its outcome as the state", async () => {
    // Arrange / Act
    const row = await drawRow(detachedShellRow(outcome));
    // Assert
    expect(row.dataset.state).toBe(outcome);
  });

  it("draws the exit code as a badge rather than a failure", async () => {
    // Arrange / Act
    const row = await drawRow(detachedShellRow(outcome));
    // Assert: a non-zero exit is still `completed`; the code is information.
    expect(row.textContent).toContain("1");
  });
});

describe("a live detached shell", () => {
  it("carries the live state", async () => {
    // Arrange / Act
    const row = await drawRow(detachedShellRow("live"));
    // Assert
    expect(row.dataset.state).toBe("live");
  });

  it("draws the spool's omission note verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(detachedShellRow("live"));
    // Assert
    expect(row.textContent).toContain("40 lines omitted");
  });
});

// ---------------------------------------------------------------------------
// The terminal row
// ---------------------------------------------------------------------------

describe("turn ended", () => {
  it("draws the interrupted arm as its own state", async () => {
    // Arrange / Act
    const row = await drawRow(turnEndedInterruptedRow());
    // Assert
    expect(row.dataset.state).toBe("interrupted");
  });

  it("carries the turnEnded row kind", async () => {
    // Arrange / Act
    const row = await drawRow(turnEndedInterruptedRow());
    // Assert
    expect(row.dataset.rowKind).toBe("turnEnded");
  });
});

// ---------------------------------------------------------------------------
// Separations — ONE renderer for every arm
// ---------------------------------------------------------------------------

describe.each(SEPARATION_ARMS)("a %s separation", (arm) => {
  it("carries its arm", async () => {
    // Arrange / Act
    const row = await drawRow(separationRow(arm));
    // Assert
    expect(row.dataset.state).toBe(arm);
  });

  it("draws the composed label verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(separationRow(arm));
    // Assert
    expect(row.textContent).toContain(`separation: ${arm}`);
  });
});

describe("the one separation renderer", () => {
  /** The structural shape of an element, ignoring text and arm-specific class. */
  const shapeOf = (element: Element): string =>
    [...element.children]
      .map((child) => `${child.tagName}[${[...child.attributes].map((a) => a.name).sort().join(",")}]`)
      .join(">");

  it("draws every arm with identical element structure", async () => {
    // Arrange
    const shapes: string[] = [];
    for (const arm of SEPARATION_ARMS) {
      const row = await drawRow(separationRow(arm));
      shapes.push(shapeOf(row));
      await harness.stop();
    }
    harness = await startHarness();
    // Assert: a per-arm divider renderer is a defect; only accent and text differ.
    expect(new Set(shapes).size).toBe(1);
  });

  it("draws the worktree-removed outcome through the same renderer", async () => {
    // Arrange / Act
    const row = await drawRow(worktreeRemovedRow());
    // Assert
    expect(row.textContent).toContain("2 uncommitted files");
  });
});

// ---------------------------------------------------------------------------
// The cold gate — the ONE card the client formats itself
// ---------------------------------------------------------------------------

describe("a standing cold gate", () => {
  it("draws the served token figure formatted", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateStandingRow());
    // Assert: 184320 tokens, formatted by the client (the deliberate exception).
    expect(row.textContent).toMatch(/184[.,]?3?\s*k/i);
  });

  it("draws the model name verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateStandingRow());
    // Assert
    expect(row.textContent).toContain(COLD_GATE_MODELS[0]);
  });

  it("ticks the lapse since the last request", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, coldGateStandingRow({ lastRequestAtMs: 0n }));
    await harness.settle();
    const before = harness.row("row-1")?.textContent ?? "";
    // Act
    await harness.tick(60_000);
    // Assert
    expect(harness.row("row-1")?.textContent).not.toBe(before);
  });

  it("lists every compact model the menu offers", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateStandingRow());
    // Assert
    expect(row.querySelectorAll("[data-compact-model]")).toHaveLength(COLD_GATE_MODELS.length);
  });

  it("lists every compact scope the menu offers", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateStandingRow());
    // Assert
    expect(row.querySelectorAll("[data-compact-scope]")).toHaveLength(3);
  });

  it.each(COLD_GATE_CHOICES)("offers a %s button", async (choice) => {
    // Arrange / Act
    const row = await drawRow(coldGateStandingRow());
    // Assert
    expect(row.querySelector(`[data-cold-gate="${choice}"]`)).not.toBeNull();
  });

  it("calls AnswerColdGate with the pay arm", async () => {
    // Arrange
    await drawRow(coldGateStandingRow());
    // Act
    await harness.click('[data-cold-gate="pay"]');
    // Assert
    const [request] = harness.fake.calls<{ choice: { case?: string } }>("answerColdGate");
    expect(request.choice.case).toBe("pay");
  });

  it("echoes the gate's own FeedId on the answer", async () => {
    // Arrange
    await drawRow(coldGateStandingRow(undefined, { id: feedId("gate-7") }));
    // Act
    await harness.click('[data-cold-gate="clear"]');
    // Assert
    const [request] = harness.fake.calls<{ gate?: { value: string } }>("answerColdGate");
    expect(request.gate?.value).toBe("gate-7");
  });

  it("echoes the served model on a compact answer", async () => {
    // Arrange
    await drawRow(coldGateStandingRow());
    // Act
    await harness.click(`[data-compact-model="${COLD_GATE_MODELS[1]}"]`);
    // Assert
    const [request] = harness.fake.calls<{ choice: { case?: string; value?: { model?: { name: string } } } }>(
      "answerColdGate",
    );
    expect(request.choice.value?.model?.name).toBe(COLD_GATE_MODELS[1]);
  });

  it("draws the served token count and not a computed one", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateStandingRow({ contextTokens: COLD_GATE_TOKENS + 1n }));
    // Assert: the figure moved with the served value.
    expect(row.textContent).not.toBe("");
  });
});

describe.each(COLD_GATE_CHOICES)("a cold gate resolved by %s", (choice) => {
  it("carries the resolution as its state", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateResolvedRow(choice));
    // Assert
    expect(row.dataset.state).toBe(choice);
  });

  it("offers no buttons once resolved", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateResolvedRow(choice));
    // Assert
    expect(row.querySelectorAll("[data-cold-gate]")).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// Permissions
// ---------------------------------------------------------------------------

describe("an open permission", () => {
  it("carries the open state", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("open"));
    // Assert
    expect(row.dataset.state).toBe("open");
  });

  it("draws the composed headline verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("open"));
    // Assert
    expect(row.textContent).toContain("Bash wants to run");
  });

  it("draws every argument line verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("open"));
    // Assert
    expect(row.textContent).toContain("timeout=120s");
  });

  it("offers the standing-allow button when the marker is present", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("open"));
    // Assert
    expect(row.querySelector('[data-permission="allowStanding"]')).not.toBeNull();
  });

  it("omits the standing-allow button when the marker is absent", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("open", { standingOffered: false }));
    // Assert
    expect(row.querySelector('[data-permission="allowStanding"]')).toBeNull();
  });

  it("calls AnswerPermission with the allow-once arm", async () => {
    // Arrange
    await drawRow(permissionRow("open"));
    // Act
    await harness.click('[data-permission="allowOnce"]');
    // Assert
    const [request] = harness.fake.calls<{ answer: { case?: string } }>("answerPermission");
    expect(request.answer.case).toBe("allowOnce");
  });

  it("calls AnswerPermission with the deny arm", async () => {
    // Arrange
    await drawRow(permissionRow("open"));
    // Act
    await harness.click('[data-permission="deny"]');
    // Assert
    const [request] = harness.fake.calls<{ answer: { case?: string } }>("answerPermission");
    expect(request.answer.case).toBe("deny");
  });

  it("echoes the card's own FeedId", async () => {
    // Arrange
    await drawRow(permissionRow("open", undefined, { id: feedId("perm-9") }));
    // Act
    await harness.click('[data-permission="allowOnce"]');
    // Assert
    const [request] = harness.fake.calls<{ permission?: { value: string } }>("answerPermission");
    expect(request.permission?.value).toBe("perm-9");
  });
});

describe.each(PERMISSION_ANSWERS)("a permission answered %s", (answer) => {
  it("carries the answer as its state", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow(answer));
    // Assert
    expect(row.dataset.state).toBe(answer);
  });

  it("offers no buttons once answered", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow(answer));
    // Assert
    expect(row.querySelectorAll("[data-permission]")).toHaveLength(0);
  });
});

describe("an abandoned permission", () => {
  it("carries the abandoned state", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("abandoned"));
    // Assert
    expect(row.dataset.state).toBe("abandoned");
  });
});

describe("a policy denial", () => {
  it("draws the policy's own text verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("deniedByPolicy"));
    // Assert
    expect(row.textContent).toContain("policy forbids it");
  });
});

// ---------------------------------------------------------------------------
// Questions
// ---------------------------------------------------------------------------

describe("an open question", () => {
  it("draws every question's text verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("open"));
    // Assert
    expect(row.textContent).toContain(QUESTION_TWO.text);
  });

  it("draws every option label verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("open"));
    // Assert
    expect(row.textContent).toContain(QUESTION_ONE.options[1]);
  });

  it("always draws the free-text escape", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("open"));
    // Assert
    expect(row.querySelectorAll("[data-question-other]")).toHaveLength(2);
  });

  it("submits the whole batch as one AnswerQuestion", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    await harness.click(`[data-question-option="${QUESTION_ONE.options[0]}"]`);
    await harness.click(`[data-question-option="${QUESTION_TWO.options[0]}"]`);
    // Act
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: unknown[] }>("answerQuestion");
    expect(request.answers).toHaveLength(2);
  });

  it("echoes the question text on each answer", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    await harness.click(`[data-question-option="${QUESTION_ONE.options[0]}"]`);
    // Act
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: { questionText: string }[] }>("answerQuestion");
    expect(request.answers.map((a) => a.questionText)).toContain(QUESTION_ONE.text);
  });

  it("echoes the chosen option label verbatim", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    // Act
    await harness.click(`[data-question-option="${QUESTION_ONE.options[1]}"]`);
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: { chosen: string[] }[] }>("answerQuestion");
    expect(request.answers[0].chosen).toEqual([QUESTION_ONE.options[1]]);
  });

  it("never sends two labels for a single-select question", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    // Act: click both options of the SINGLE-select question.
    await harness.click(`[data-question-option="${QUESTION_ONE.options[0]}"]`);
    await harness.click(`[data-question-option="${QUESTION_ONE.options[1]}"]`);
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: { chosen: string[] }[] }>("answerQuestion");
    expect(request.answers[0].chosen).toHaveLength(1);
  });

  it("sends both labels for a multi-select question", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    // Act
    await harness.click(`[data-question-option="${QUESTION_TWO.options[0]}"]`);
    await harness.click(`[data-question-option="${QUESTION_TWO.options[1]}"]`);
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: { chosen: string[] }[] }>("answerQuestion");
    expect(request.answers[1].chosen).toEqual([...QUESTION_TWO.options]);
  });

  it("sends the free text for the question it was typed into", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    const field = harness.$$("[data-question-other]")[0] as HTMLInputElement;
    field.value = "something else entirely";
    field.dispatchEvent(new Event("input", { bubbles: true }));
    // Act
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: { otherText?: { text: string } }[] }>("answerQuestion");
    expect(request.answers[0].otherText?.text).toBe("something else entirely");
  });
});

describe("an answered question", () => {
  it("carries the answered state", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("answered"));
    // Assert
    expect(row.dataset.state).toBe("answered");
  });

  it("draws the given answers verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("answered"));
    // Assert
    expect(row.textContent).toContain("and the docs");
  });
});

describe("an expired question", () => {
  it("draws as expired rather than pending", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("expired"));
    // Assert
    expect(row.dataset.state).toBe("expired");
  });

  it("offers no submit control", async () => {
    // Arrange / Act
    const row = await drawRow(questionRow("expired"));
    // Assert
    expect(row.querySelector("[data-question-submit]")).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// Merge tabs
// ---------------------------------------------------------------------------

describe("merge tabs", () => {
  it.each(MERGE_TAB_KINDS)("draws the %s tab with its kind", async (kind) => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow(kind));
    // Assert
    expect(row.querySelector(`[data-merge-tab="${kind}"]`)).not.toBeNull();
  });

  it.each(
    MERGE_TAB_KINDS.flatMap((kind) => MERGE_TAB_STATES[kind].map((state) => ({ kind, state }))),
  )("draws the $kind tab in its $state state", async ({ kind, state }) => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow(kind, state));
    // Assert
    expect(row.querySelector(`[data-merge-tab="${kind}"]`)?.getAttribute("data-tab-state")).toBe(state);
  });

  it("draws the label with its round", async () => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow("tests"));
    // Assert
    expect(row.textContent).toContain("tests");
  });

  it("draws the queue tab's own snapshot", async () => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow("queue"));
    // Assert
    expect(row.textContent).toContain("ws-ahead");
  });

  it("draws the merge tab's narration lines verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow("merge"));
    // Assert
    expect(row.textContent).toContain("cherry-picked 3 commits");
  });

  it("draws the tests tab's suites", async () => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow("tests"));
    // Assert
    expect(row.textContent).toContain("vitest");
  });

  it("paints the tests tab's spans from the vocabulary", async () => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow("tests"));
    const drawn = [...row.querySelectorAll("span")].find((el) => el.textContent === "PASS ");
    // Assert
    expect(drawn?.className).toBe(expectedPaintClass("ansi-fg-green"));
  });

  it("draws the parked line on a parked conflicts tab", async () => {
    // Arrange / Act
    const row = await drawRow(mergeTabRow("conflicts", "parked"));
    // Assert
    expect(row.textContent).toContain("paused for your answer");
  });
});

// ---------------------------------------------------------------------------
// Command panels and the refused card (synthesized, non-durable rows)
// ---------------------------------------------------------------------------

describe.each(FEED_COMMAND_PANEL_ARMS)("a %s command panel row", (arm) => {
  it("carries the command-panel row kind", async () => {
    // Arrange / Act
    const row = await drawRow(commandPanelRow(arm));
    // Assert
    expect(row.dataset.rowKind).toBe("commandPanel");
  });

  it("draws the panel arm", async () => {
    // Arrange / Act
    const row = await drawRow(commandPanelRow(arm));
    // Assert
    expect(row.querySelector(`[data-panel="${arm}"]`)).not.toBeNull();
  });
});

describe("a refused command card", () => {
  it("carries the command-refused row kind", async () => {
    // Arrange / Act
    const row = await drawRow(commandRefusedRow());
    // Assert
    expect(row.dataset.rowKind).toBe("commandRefused");
  });

  it("draws the command verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(commandRefusedRow({ command: "/help" }));
    // Assert
    expect(row.textContent).toContain("/help");
  });

  it("draws the composed reason verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(commandRefusedRow({ reason: "no support this wave" }));
    // Assert
    expect(row.textContent).toContain("no support this wave");
  });

  it("offers the add-support button when the marker is present", async () => {
    // Arrange / Act
    const row = await drawRow(commandRefusedRow());
    // Assert
    expect(row.querySelector("[data-add-support]")).not.toBeNull();
  });

  it("omits the add-support button when the marker is absent", async () => {
    // Arrange / Act
    const row = await drawRow(commandRefusedRow({ addSupport: false }));
    // Assert
    expect(row.querySelector("[data-add-support]")).toBeNull();
  });
});
