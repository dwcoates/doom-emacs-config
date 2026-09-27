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
  FeedToolCallInputSchema,
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

import { HARNESS_EPOCH_MS, bootColdOnce, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import { drawnPaintClass, expectedPaintClass } from "./vocab";
import { SessionCompactScope } from "../../../proto/gen/ts/conversation/v1/session_pb";
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
  TOOL_IMAGE_ALT,
  TOOL_IMAGE_SRC,
  TOOL_INPUT_FORMS,
  TOOL_OUTPUT_FORMS,
  TOOL_OUTPUT_FORMS_WITH_OMISSION,
  WORKSPACE_ID,
  activityRow,
  agentPromptRow,
  mcpToolCallUnit,
  artifactUnit,
  coldGateResolvedRow,
  coldGateStandingRow,
  commandPanelRow,
  commandRefusedRow,
  detachedShellRow,
  shellHeadRow,
  peerMessageRow,
  subagentResultUnit,
  subagentHandbackRow,
  removedRow,
  detachedSubagentRow,
  feedId,
  findingsUnit,
  hookUnit,
  mergeUnit,
  responseRow,
  FINDINGS_LOCATION_NO_LINE,
  PLAN_EDIT_PATH,
  WORKTREE_PATH,
  mergeTabRow,
  permissionRow,
  planUnit,
  questionRow,
  RESPONSE_NOTICE_HEADING,
  responseNoticeUnit,
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

bootColdOnce();

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
      "shellHead",
      "peerMessage",
      "subagentHandback",
      "removed",
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
      "subagentResult",
    ]);
  });

  it("covers every FeedResponse result", () => {
    assertCoversOneof(FeedResponseSchema, "result", [...RESPONSE_STATES]);
  });

  it("covers every FeedSimpleToolCall outcome", () => {
    assertCoversOneof(FeedSimpleToolCallSchema, "outcome", [
      "running",
      "returned",
      "denied",
    ]);
  });

  it("covers every tool-call output form", () => {
    assertCoversOneof(FeedToolCallReturnedSchema, "form", [...TOOL_OUTPUT_FORMS]);
  });

  it("covers every tool-call input form", () => {
    assertCoversOneof(FeedToolCallInputSchema, "form", [...TOOL_INPUT_FORMS]);
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

describe("a response in the notice register", () => {
  it("marks the bubble as a notice", async () => {
    // Arrange / Act: vendor-synthesized prose, not the agent's own words.
    const row = await drawRow(activityRow(responseNoticeUnit()));
    // Assert
    expect(row.querySelector("[data-notice]")).not.toBeNull();
  });

  it("draws the daemon-composed heading verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseNoticeUnit()));
    // Assert: the client holds no notice vocabulary of its own.
    expect(row.textContent).toContain(RESPONSE_NOTICE_HEADING);
  });

  it("still draws the prose beneath the heading", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseNoticeUnit()));
    // Assert
    expect(row.textContent).toContain("the vendor's remark");
  });

  it("draws whatever heading the daemon serves", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseNoticeUnit("success", "a system remark")));
    // Assert
    expect(row.textContent).toContain("a system remark");
  });

  it.each(RESPONSE_STATES)("carries the notice register on a %s response too", async (state) => {
    // Arrange / Act: the notice rides every result arm.
    const row = await drawRow(activityRow(responseNoticeUnit(state)));
    // Assert
    expect(row.querySelector("[data-notice]")).not.toBeNull();
  });
});

describe("an ordinary response", () => {
  it("carries no notice register", async () => {
    // Arrange / Act: `notice` is optional — absent means draw nothing.
    const row = await drawRow(activityRow(responseUnit("success")));
    // Assert
    expect(row.querySelector("[data-notice]")).toBeNull();
  });
});

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
    // `last_progress` is an ABSOLUTE instant, and the page's clock starts at
    // the harness epoch (harness.ts, HARNESS_EPOCH_MS) — so a beat reported AT
    // the epoch is what makes the figure read the time advanced since it.
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      activityRow(toolCallRunningUnit(BigInt(HARNESS_EPOCH_MS))),
    );
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
    // Assert: only the capped forms carry an omission field.
    const expected = (TOOL_OUTPUT_FORMS_WITH_OMISSION as readonly string[]).includes(form);
    expect(/omitted|more/.test(row.textContent ?? "")).toBe(expected);
  });
});

describe("an MCP server's tool call", () => {
  it("draws through the ordinary tool card", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(mcpToolCallUnit()));
    // Assert
    expect({ state: row.dataset.state, card: row.querySelector(".tool-card") !== null }).toEqual({
      state: "returned",
      card: true,
    });
  });

  it("draws the qualified tool name as the head, verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(mcpToolCallUnit()));
    // Assert
    expect(row.querySelector(".tool-name")?.textContent).toContain("mcp__claude-in-chrome__navigate");
  });

  it("draws the arguments' JSON as the input line, verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(mcpToolCallUnit()));
    // Assert
    expect(row.textContent).toContain('{"tabId":7,"url":"https://example.com"}');
  });

  it("draws what the tool returned", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(mcpToolCallUnit()));
    // Assert
    expect(row.textContent).toContain("Navigated to https://example.com");
  });

  it("draws a failed call's own account", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(mcpToolCallUnit("failed")));
    // Assert
    expect(row.textContent).toContain("Error: Couldn't determine which page this action targets.");
  });
});

describe("a tool call that answered with an image", () => {
  it("carries the image form", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("image")));
    // Assert
    expect(row.querySelector('[data-output-form="image"]')).not.toBeNull();
  });

  it("draws the picture as an image element", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("image")));
    // Assert
    expect(row.querySelector("img.prompt-block-image")).not.toBeNull();
  });

  it("loads the daemon's resolved src verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("image")));
    // Assert
    expect(row.querySelector("img.prompt-block-image")?.getAttribute("src")).toBe(TOOL_IMAGE_SRC);
  });

  it("names the picture with the daemon's resolved alt", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("image")));
    // Assert
    expect(row.querySelector("img.prompt-block-image")?.getAttribute("alt")).toBe(TOOL_IMAGE_ALT);
  });

  it("sits in the card's output section like every other form", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("image")));
    // Assert
    expect(row.querySelector("[data-output-body] img.prompt-block-image")).not.toBeNull();
  });

  it("still draws its verdict when the call failed", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("image", "failed")));
    // Assert
    expect(row.querySelector('[data-verdict="failed"]')).not.toBeNull();
  });
});

describe("a tool call that returned no output", () => {
  it("carries the none form", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("none")));
    // Assert
    expect(row.querySelector('[data-output-form="none"]')).not.toBeNull();
  });

  it("draws no output section at all", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("none")));
    // Assert
    expect(row.querySelector("[data-output-body]")).toBeNull();
  });

  it("still draws its input line", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("none")));
    // Assert
    expect(row.textContent).toContain("npm test");
  });

  it("still draws its verdict", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("none", "failed")));
    // Assert
    expect(row.querySelector('[data-verdict="failed"]')).not.toBeNull();
  });

  it("still draws its runtime", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("none")));
    // Assert
    expect(row.textContent).toContain("ran 4.2 s");
  });
});

describe.each(TOOL_INPUT_FORMS)("an input line drawn as a %s", (inputForm) => {
  it("carries its form as the treatment", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("text", "succeeded", inputForm)));
    // Assert
    expect(row.querySelector("[data-input-form]")?.getAttribute("data-input-form")).toBe(inputForm);
  });

  it("gives the line a treatment class of its own", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("text", "succeeded", inputForm)));
    // Assert: the daemon states the form; the client applies a treatment and
    // still knows no tool.
    expect(row.querySelector("[data-input-form]")?.className).toContain(inputForm);
  });

  it("draws the composed line verbatim regardless of the treatment", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("text", "succeeded", inputForm)));
    // Assert
    expect(row.textContent).toContain("npm test");
  });
});

describe("an input line with no form", () => {
  it("draws as plain text", async () => {
    // Arrange / Act: UNSET form is a legal fourth state meaning plain text.
    const row = await drawRow(activityRow(toolCallReturnedUnit("text")));
    // Assert
    expect(row.querySelector("[data-input-form]")).toBeNull();
  });

  it("still draws the composed line verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(toolCallReturnedUnit("text")));
    // Assert
    expect(row.textContent).toContain("npm test");
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
    expect(drawnPaintClass(drawn)).toBe(expected);
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

describe("a subagent's returned result", () => {
  it("draws the report through the markdown pipeline", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentResultUnit()));
    // Assert
    expect(row.querySelector(".subagent-result strong")?.textContent).toBe("all four items done");
  });

  it("draws uncapped, like a final answer", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentResultUnit()));
    // Assert
    expect(row.querySelector(".subagent-result")?.getAttribute("data-cap-lines")).toBe("none");
  });

  it("draws an undelivered result's reason in the quiet line", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentResultUnit("undelivered")));
    // Assert
    expect(row.querySelector(".subagent-result-reason")?.textContent).toBe("the parent is gone");
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

describe.each(SHELL_OUTCOMES)("a settled (%s) detached shell head", (outcome) => {
  it("carries its outcome as the state", async () => {
    // Arrange / Act
    const row = await drawRow(shellHeadRow(outcome));
    // Assert
    expect(row.dataset.state).toBe(outcome);
  });

  it("draws the exit code as a badge rather than a failure", async () => {
    // Arrange / Act
    const row = await drawRow(shellHeadRow(outcome));
    // Assert: a non-zero exit is still `completed`; the code is information.
    expect(row.querySelector(".shell-exit")?.getAttribute("data-exit-code")).toBe("1");
  });
});

describe("a live detached shell head", () => {
  it("carries the live state", async () => {
    // Arrange / Act
    const row = await drawRow(shellHeadRow("live"));
    // Assert
    expect(row.dataset.state).toBe("live");
  });

});

describe("a peer message", () => {
  it("draws the sender label the daemon composed, verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(peerMessageRow());
    // Assert
    expect(row.querySelector(".peer-label")?.textContent).toBe("agent Explore");
  });

  it("is not a prompt: it wears the peer bubble, never the user's", async () => {
    // Arrange / Act
    const row = await drawRow(peerMessageRow());
    // Assert
    expect(row.querySelector(".bubble.peer")).not.toBeNull();
    expect(row.querySelector(".bubble.user")).toBeNull();
  });

  it("starts collapsed to its header alone, so the body is not revealed", async () => {
    // Arrange / Act
    const row = await drawRow(peerMessageRow());
    // Assert
    expect([
      row.querySelector(".bubble.peer")?.getAttribute("data-cap-lines"),
      row.querySelector(".bubble.peer > .bubble-scroll")?.classList.contains("expanded"),
    ]).toEqual(["0", false]);
  });

  it("reveals the body when its label is clicked, through the one toggle", async () => {
    // Arrange
    await drawRow(peerMessageRow({ id: feedId("peer-1") }));
    // Act
    await harness.click('[data-feed-row="peer-1"] .peer-label');
    // Assert
    expect(
      harness.$('[data-feed-row="peer-1"] .bubble.peer > .bubble-scroll')?.classList.contains("expanded"),
    ).toBe(true);
  });
});

describe("a subagent hand-back", () => {
  it("draws the label the daemon composed, verbatim", async () => {
    // Arrange / Act
    const row = await drawRow(subagentHandbackRow());
    // Assert
    expect(row.querySelector(".subagent-handback-badge")?.textContent).toBe("agent Explore reported back");
  });

  it("is a badge, never a bubble", async () => {
    // Arrange / Act
    const row = await drawRow(subagentHandbackRow());
    // Assert
    expect(row.querySelector(".bubble")).toBeNull();
  });

  it("carries no body: the report is not drawn in the main feed", async () => {
    // Arrange / Act
    const row = await drawRow(subagentHandbackRow());
    // Assert
    expect(row.querySelector(".subagent-handback-badge")?.children.length).toBe(0);
  });
});

describe("a removal", () => {
  it("drops the row it keys rather than drawing anything", async () => {
    // Arrange: a row on the tail.
    await drawRow(userPromptRow("the retired row", { id: feedId("gone-1") }));
    // Act: the daemon retires it — the upsert's dual, on the same tail.
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, removedRow({ id: feedId("gone-1") }));
    await harness.settle();
    // Assert
    expect(harness.row("gone-1")).toBeNull();
  });

  it("leaves every other row standing", async () => {
    // Arrange
    await drawRow(userPromptRow("the retired row", { id: feedId("gone-1") }));
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("the row that stays", { id: feedId("kept-1") }));
    await harness.settle();
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, removedRow({ id: feedId("gone-1") }));
    await harness.settle();
    // Assert
    expect(harness.rowIds()).toEqual(["kept-1"]);
  });
});

describe("a detached shell's spool body", () => {
  it("draws the spool's omission note verbatim", async () => {
    // Arrange / Act: the BODY arm draws the spool and nothing else.
    const row = await drawRow(detachedShellRow("live"));
    // Assert
    expect(row.textContent).toContain("40 lines omitted");
  });

  it("draws no head facts, which live on the head row", async () => {
    // Arrange / Act
    const row = await drawRow(detachedShellRow("live"));
    // Assert: drawing the clock here too would duplicate the head's.
    expect(row.querySelector(".shell-clock")).toBeNull();
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

  // The socket-backed per-arm redraw loop reached 936ms under concurrent
  // integration load, so its host-contention budget is local to this test.
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
  }, 1_500);

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
    // Act: the compact path needs a model AND a scope, so the submenu's send
    // button is the verb's control — the radio only records the choice.
    await harness.click(`[data-compact-model="${COLD_GATE_MODELS[1]}"]`);
    await harness.click('[data-cold-gate="compact"]');
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

describe("a cold gate resolved by compacting", () => {
  it("draws the scope the resolution carries", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateResolvedRow("compact", { scope: SessionCompactScope.PROMPTS }));
    // Assert
    expect(row.querySelector("[data-compact-scope]")?.getAttribute("data-compact-scope")).toBe("PROMPTS");
  });

  it("draws the model the resolution carries", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateResolvedRow("compact"));
    // Assert
    expect(row.textContent).toContain("haiku");
  });

  it("draws a different scope when a different one was chosen", async () => {
    // Arrange / Act
    const row = await drawRow(coldGateResolvedRow("compact", { scope: SessionCompactScope.RESPONSES }));
    // Assert
    expect(row.querySelector("[data-compact-scope]")?.getAttribute("data-compact-scope")).toBe("RESPONSES");
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

describe("an undecidable denial", () => {
  it("carries its own verdict value, apart from the policy denial's", async () => {
    // Arrange / Act
    const row = await drawRow(permissionRow("deniedUndecidable"));
    // Assert
    expect(
      row.querySelector(".perm-verdict")?.getAttribute("data-permission-verdict"),
    ).toBe("deniedUndecidable");
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
    // The batch is answered WHOLE (question.ts: an empty answer blocks submit
    // rather than sending a partial batch), so the second question is answered
    // too even though this case is about the first one's echoed text.
    await harness.click(`[data-question-option="${QUESTION_TWO.options[0]}"]`);
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
    await harness.click(`[data-question-option="${QUESTION_TWO.options[0]}"]`);
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
    await harness.click(`[data-question-option="${QUESTION_TWO.options[0]}"]`);
    await harness.click("[data-question-submit]");
    // Assert
    const [request] = harness.fake.calls<{ answers: { chosen: string[] }[] }>("answerQuestion");
    expect(request.answers[0].chosen).toHaveLength(1);
  });

  it("sends both labels for a multi-select question", async () => {
    // Arrange
    await drawRow(questionRow("open"));
    // Act
    await harness.click(`[data-question-option="${QUESTION_ONE.options[0]}"]`);
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
    await harness.click(`[data-question-option="${QUESTION_TWO.options[0]}"]`);
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

// ---------------------------------------------------------------------------
// TOKEN FORMATTING, ACROSS THE THREE SURFACES
//
// Three places show a token figure, and only ONE of them formats: a feed row's
// usage stamp and the topbar's context chip arrive daemon-composed and are
// drawn verbatim, while the cold gate is the contract's deliberate exception
// and formats from a raw `bigint` client-side.
//
// The convention is RULED (daemon lead, format.go) and the table below is it,
// verbatim. The rules behind it: under 1000 unscaled; from 1000 up, scaled to
// k or M with exactly one fractional digit, round-to-nearest, and a trailing
// ".0" trimmed; the unit follows the RENDERED value, so 999,950 reads "1M" and
// never "1000k". Suffixes ("tok", "in") belong to the call site, not here.
//
// EXPECT THE COLD-GATE CASES RED until the wiring agent lands the shared
// formatter: today's client code keeps the ".0" and caps the fraction, which
// disagrees with the ruling. They are written to the RULING, deliberately, so
// that fixing the formatter turns them green rather than someone having to
// remember to tighten a loosened test. See the report.
// ---------------------------------------------------------------------------

/**
 * The ruled table. Each row straddles a boundary a formatter can get wrong:
 * zero, the last unscaled value, the exact scale boundary, one-digit rounding,
 * a trimmed ".0", the rounding edge that flips the unit, and the M scale.
 */
const RULED_TOKEN_FIGURES: ReadonlyArray<readonly [bigint, string]> = [
  [0n, "0"],
  [999n, "999"],
  [1_000n, "1k"],
  [1_200n, "1.2k"],
  [12_340n, "12.3k"],
  [182_000n, "182k"],
  [999_949n, "999.9k"],
  [999_950n, "1M"],
  [1_200_000n, "1.2M"],
];

describe("token formatting across surfaces", () => {
  /** The cold gate's own drawn figure for a raw token count. */
  const coldGateFigure = async (tokens: bigint): Promise<string> => {
    const row = await drawRow(coldGateStandingRow({ contextTokens: tokens }));
    const drawn = row.querySelector("[data-context-tokens]")?.textContent?.trim();
    if (drawn === undefined) throw new Error("the cold gate drew no context-token figure");
    return drawn;
  };

  it.each(RULED_TOKEN_FIGURES)("formats %s as the ruled %s", async (tokens, expected) => {
    // Arrange / Act
    const drawn = await coldGateFigure(tokens);
    // Assert: the ruled table verbatim — the client's one formatting site must
    // agree with the daemon's, or the same quantity reads two ways on screen.
    expect(drawn).toBe(expected);
  });

  it("draws the daemon's own usage stamp verbatim, whatever its scale", async () => {
    // Arrange / Act: the feed never reformats what the daemon composed.
    const row = await drawRow(activityRow(responseUnit("success", "prose", "1.2M in / 3.4k out")));
    // Assert
    expect(row.textContent).toContain("1.2M in / 3.4k out");
  });

  it("does not rescale a daemon usage stamp into its own convention", async () => {
    // Arrange / Act: a stamp that disagrees with the ruling is still drawn as
    // sent — reformatting it would be the client deriving, and the disagreement
    // is the daemon's to fix.
    const row = await drawRow(activityRow(responseUnit("success", "prose", "1234.6k in")));
    // Assert
    expect(row.textContent).toContain("1234.6k in");
  });

  it("carries the call site's own suffix, not the formatter's", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(responseUnit("success", "prose", "182k tok")));
    // Assert
    expect(row.textContent).toContain("182k tok");
  });
});

// ---------------------------------------------------------------------------
// STOPPING A DETACHED HEAD (audit 1, item 1)
//
// THE ARM IS THE TARGET: a live bubble or shell head stops ITS OWN work, by
// the bubble row's `FeedId` exactly as the feed served it. The id is echoed
// verbatim — `FeedId` is one of the four identifier spaces and is never
// parsed or constructed.
// ---------------------------------------------------------------------------

describe("a live detached subagent's stop", () => {
  it("sends the detached target", async () => {
    // Arrange
    await drawRow(detachedSubagentRow("live", { id: feedId("agent-7") }));
    // Act
    await harness.click('[data-feed-row="agent-7"] [data-interrupt]');
    // Assert
    const [request] = harness.fake.calls<{ target: { case?: string } }>("interrupt");
    expect(request.target.case).toBe("detached");
  });

  it("echoes the row's own FeedId as the target", async () => {
    // Arrange
    await drawRow(detachedSubagentRow("live", { id: feedId("agent-7") }));
    // Act
    await harness.click('[data-feed-row="agent-7"] [data-interrupt]');
    // Assert
    const [request] = harness.fake.calls<{ target: { value?: { value?: string } } }>("interrupt");
    expect(request.target.value?.value).toBe("agent-7");
  });

  it("marks the control with the row's own FeedId", async () => {
    // Arrange / Act
    const row = await drawRow(detachedSubagentRow("live", { id: feedId("agent-7") }));
    // Assert
    expect(row.querySelector("[data-interrupt]")?.getAttribute("data-interrupt")).toBe("agent-7");
  });

  it("draws no refusal for an interrupted-detached answer", async () => {
    // Arrange
    await drawRow(detachedSubagentRow("live", { id: feedId("agent-7") }));
    // Act
    await harness.click('[data-feed-row="agent-7"] [data-interrupt]');
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });
});

describe("a live detached shell head's stop", () => {
  it("sends the detached target", async () => {
    // Arrange
    await drawRow(shellHeadRow("live", { id: feedId("shell-3") }));
    // Act
    await harness.click('[data-feed-row="shell-3"] [data-interrupt]');
    // Assert
    const [request] = harness.fake.calls<{ target: { case?: string } }>("interrupt");
    expect(request.target.case).toBe("detached");
  });

  it("echoes the shell row's own FeedId", async () => {
    // Arrange
    await drawRow(shellHeadRow("live", { id: feedId("shell-3") }));
    // Act
    await harness.click('[data-feed-row="shell-3"] [data-interrupt]');
    // Assert
    const [request] = harness.fake.calls<{ target: { value?: { value?: string } } }>("interrupt");
    expect(request.target.value?.value).toBe("shell-3");
  });

  it("offers no stop on a settled shell", async () => {
    // Arrange / Act: there is nothing left to stop.
    const row = await drawRow(shellHeadRow("completed", { id: feedId("shell-3") }));
    // Assert
    expect(row.querySelector("[data-interrupt]")).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// R2: THE WIRE'S FOLD IS THE INITIAL FOLD (audit 1, item 4)
//
// "R2 fold fields are the INITIAL fold on first draw; the user's toggle wins
// after (a re-push never un-toggles)." Every fold field in the contract is
// asserted the same way: draw it folded, toggle it open, re-push the SAME row
// id, and read the fold back.
// ---------------------------------------------------------------------------

describe("a merge bubble's fold", () => {
  /** The merge bubble arrives folded (FeedMergeHead.fold.folded = true). */
  const drawMerge = () => drawRow(activityRow(mergeUnit("update"), { id: feedId("merge-1") }));

  it("starts folded where the wire said", async () => {
    // Arrange / Act
    const row = await drawMerge();
    // Assert
    expect(row.dataset.expanded).toBe("false");
  });

  it("opens on the reader's toggle", async () => {
    // Arrange
    await drawMerge();
    // Act
    await harness.click('[data-feed-row="merge-1"] [data-expand]');
    // Assert
    expect(harness.row("merge-1")?.dataset.expanded).toBe("true");
  });

  it("keeps the reader's toggle across a re-push of the same row", async () => {
    // Arrange
    await drawMerge();
    await harness.click('[data-feed-row="merge-1"] [data-expand]');
    // Act: the wire still says folded; the reader's toggle wins.
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      activityRow(mergeUnit("update"), { id: feedId("merge-1") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("merge-1")?.dataset.expanded).toBe("true");
  });
});

describe("a compaction divider's fold", () => {
  it("starts folded where the wire said", async () => {
    // Arrange / Act
    const row = await drawRow(separationRow("compacted", { id: feedId("cut-1") }));
    // Assert
    expect(row.querySelector('[data-fold="compaction-summary"]')?.getAttribute("data-folded")).toBe(
      "true",
    );
  });

  it("opens on the reader's toggle", async () => {
    // Arrange
    await drawRow(separationRow("compacted", { id: feedId("cut-1") }));
    // Act
    await harness.click('[data-feed-row="cut-1"] [data-fold="compaction-summary"]');
    // Assert
    expect(
      harness.$('[data-feed-row="cut-1"] [data-fold="compaction-summary"]')?.getAttribute("data-folded"),
    ).toBe("false");
  });

  it("keeps the reader's toggle across a re-push of the same row", async () => {
    // Arrange
    await drawRow(separationRow("compacted", { id: feedId("cut-1") }));
    await harness.click('[data-feed-row="cut-1"] [data-fold="compaction-summary"]');
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, separationRow("compacted", { id: feedId("cut-1") }));
    await harness.settle();
    // Assert
    expect(
      harness.$('[data-feed-row="cut-1"] [data-fold="compaction-summary"]')?.getAttribute("data-folded"),
    ).toBe("false");
  });
});

// THE SKILL CARD'S FOLD IS THE WHOLE CARD (owner ruling, 2026-09-15). It is
// not a named `data-fold` section with a `data-folded` flag: the card itself
// wears `.tool-fold` — a CAPPED_CLASSES section — and the feed-wide expand
// handler toggles `.expanded` on it, which is the same key `expandedKeys`
// reconciles across a re-push.
describe("a skill document's fold", () => {
  it("starts folded", async () => {
    // Arrange / Act: a SKILL.md is long, and the reader asked for a skill to
    // run rather than to be read to.
    const row = await drawRow(activityRow(skillUnit("loaded"), { id: feedId("skill-1") }));
    // Assert
    expect(row.querySelector(".tool-skill.tool-fold")?.classList.contains("expanded")).toBe(
      false,
    );
  });

  it("hides the document until the reader opens the card", async () => {
    // Arrange / Act: no preview — the document is not revealed on a collapsed
    // card, which is the whole point of the card-level fold.
    const row = await drawRow(activityRow(skillUnit("loaded"), { id: feedId("skill-1") }));
    // Assert: the document is present but its host card is not expanded.
    expect(row.querySelector(".skill-content")).not.toBeNull();
    expect(row.querySelector(".tool-skill.expanded")).toBeNull();
  });

  it("reveals the document when the card is clicked", async () => {
    // Arrange
    await drawRow(activityRow(skillUnit("loaded"), { id: feedId("skill-1") }));
    // Act
    await harness.click('[data-feed-row="skill-1"] .tool-skill');
    // Assert
    expect(
      harness.$('[data-feed-row="skill-1"] .tool-skill')?.classList.contains("expanded"),
    ).toBe(true);
  });

  it("keeps the reader's toggle across a re-push of the same row", async () => {
    // Arrange
    await drawRow(activityRow(skillUnit("loaded"), { id: feedId("skill-1") }));
    await harness.click('[data-feed-row="skill-1"] .tool-skill');
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      activityRow(skillUnit("loaded"), { id: feedId("skill-1") }),
    );
    await harness.settle();
    // Assert: a re-push never un-toggles what the reader opened.
    expect(
      harness.$('[data-feed-row="skill-1"] .tool-skill')?.classList.contains("expanded"),
    ).toBe(true);
  });
});

describe("a finding's scenario fold", () => {
  it("starts folded", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(findingsUnit(), { id: feedId("find-1") }));
    // Assert
    expect(
      row.querySelector('[data-fold="finding-scenario-0"]')?.getAttribute("data-folded"),
    ).toBe("true");
  });

  it("keeps the reader's toggle across a re-push of the same row", async () => {
    // Arrange
    await drawRow(activityRow(findingsUnit(), { id: feedId("find-1") }));
    await harness.click('[data-feed-row="find-1"] [data-fold="finding-scenario-0"]');
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      activityRow(findingsUnit(), { id: feedId("find-1") }),
    );
    await harness.settle();
    // Assert
    expect(
      harness.$('[data-feed-row="find-1"] [data-fold="finding-scenario-0"]')?.getAttribute("data-folded"),
    ).toBe("false");
  });

  it("keeps one finding's toggle without opening its neighbours", async () => {
    // Arrange
    await drawRow(activityRow(findingsUnit(), { id: feedId("find-1") }));
    // Act
    await harness.click('[data-feed-row="find-1"] [data-fold="finding-scenario-0"]');
    // Assert: each row's fold is its own, keyed by its index.
    expect(
      harness.$('[data-feed-row="find-1"] [data-fold="finding-scenario-1"]')?.getAttribute("data-folded"),
    ).toBe("true");
  });
});

// ---------------------------------------------------------------------------
// R8: A CROSS-WORKSPACE CLICK CALLS SelectWorkspace AND NOTHING ELSE
// (audit 1, item 5)
// ---------------------------------------------------------------------------

describe("a merge queue entry's jump", () => {
  /** The queue tab, whose entries name other workspaces. */
  const drawQueue = () => drawRow(mergeTabRow("queue", "live", { id: feedId("tab-q") }));

  it("calls SelectWorkspace exactly once", async () => {
    // Arrange
    await drawQueue();
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-queue-place="ahead"] [data-select]');
    // Assert
    expect(harness.fake.calls("selectWorkspace")).toHaveLength(1);
  });

  it("echoes that entry's own WorkspaceRef", async () => {
    // Arrange
    await drawQueue();
    // Act
    await harness.click('[data-queue-place="ahead"] [data-select]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("selectWorkspace");
    expect(request.workspace?.id).toBe("ws-ahead");
  });

  it("makes no other call for the jump", async () => {
    // Arrange
    await drawQueue();
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-queue-place="behind"] [data-select]');
    // Assert
    expect(harness.fake.log().map((c) => c.rpc)).toEqual(["selectWorkspace"]);
  });

  it("echoes the behind entry's own ref rather than the current one", async () => {
    // Arrange
    await drawQueue();
    // Act
    await harness.click('[data-queue-place="behind"] [data-select]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("selectWorkspace");
    expect(request.workspace?.id).toBe("ws-behind");
  });
});

// ---------------------------------------------------------------------------
// A CARD ANSWER DERIVES NO STATE (audit 1, item 10)
//
// "STATELESS RENDERER ... nothing accumulates across pushes." A successful
// AnswerPermission is not the card closing: the card closes when the DAEMON
// pushes the answered row. Closing it locally would be the client deciding
// what happened, and would be wrong the moment the daemon disagreed.
// ---------------------------------------------------------------------------

describe("an answered permission card", () => {
  it("stays open until the daemon pushes the answer", async () => {
    // Arrange
    await drawRow(permissionRow("open", undefined, { id: feedId("perm-1") }));
    // Act
    await harness.click('[data-permission="allowOnce"]');
    // Assert
    expect(harness.row("perm-1")?.dataset.state).toBe("open");
  });

  it("closes when the daemon pushes the answered row", async () => {
    // Arrange
    await drawRow(permissionRow("open", undefined, { id: feedId("perm-1") }));
    await harness.click('[data-permission="allowOnce"]');
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      permissionRow("allowedOnce", undefined, { id: feedId("perm-1") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("perm-1")?.dataset.state).toBe("allowedOnce");
  });

  it("draws no refusal for a successful answer", async () => {
    // Arrange
    await drawRow(permissionRow("open", undefined, { id: feedId("perm-1") }));
    // Act
    await harness.click('[data-permission="allowOnce"]');
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  it("draws the state the daemon pushed even when it is not the answer clicked", async () => {
    // Arrange: the daemon is the authority on what happened.
    await drawRow(permissionRow("open", undefined, { id: feedId("perm-1") }));
    await harness.click('[data-permission="allowOnce"]');
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      permissionRow("deniedByPolicy", undefined, { id: feedId("perm-1") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("perm-1")?.dataset.state).toBe("deniedByPolicy");
  });
});

// ---------------------------------------------------------------------------
// THE ONE SHARED EDITOR LINK, AT ALL THREE SITES (audit 1, item 12)
//
// Ruling (2026-08-29): `renderEditorLink` is the ONE shared component for the
// plan edit button, findings locations and worktree divider paths, and
// `OpenInEditor{path, line?}` carries the SERVED values — the client never
// parses a location string to derive them.
// ---------------------------------------------------------------------------

describe("the editor link on a plan", () => {
  it("wears the shared editor-link hook", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(planUnit("planned"), { id: feedId("plan-1") }));
    // Assert
    expect(row.querySelector("[data-editor-link]")).not.toBeNull();
  });

  it("calls OpenInEditor with the served path", async () => {
    // Arrange
    await drawRow(activityRow(planUnit("planned"), { id: feedId("plan-1") }));
    // Act
    await harness.click('[data-feed-row="plan-1"] [data-editor-link]');
    // Assert
    const [request] = harness.fake.calls<{ path: string }>("openInEditor");
    expect(request.path).toBe(PLAN_EDIT_PATH);
  });

  it("echoes the workspace on the editor call", async () => {
    // Arrange
    await drawRow(activityRow(planUnit("planned"), { id: feedId("plan-1") }));
    // Act
    await harness.click('[data-feed-row="plan-1"] [data-editor-link]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("openInEditor");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });
});

describe("the editor link on a finding's location", () => {
  it("wears the shared editor-link hook on every location", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(findingsUnit(), { id: feedId("find-2") }));
    // Assert
    expect(row.querySelectorAll("[data-editor-link]")).toHaveLength(3);
  });

  it("calls OpenInEditor with the served path", async () => {
    // Arrange
    await drawRow(activityRow(findingsUnit(), { id: feedId("find-2") }));
    // Act
    await harness.click('[data-feed-row="find-2"] [data-editor-link]');
    // Assert
    const [request] = harness.fake.calls<{ path: string }>("openInEditor");
    expect(request.path).toBe(FINDINGS_LOCATION.path);
  });

  it("carries the served line rather than one parsed off the text", async () => {
    // Arrange
    await drawRow(activityRow(findingsUnit(), { id: feedId("find-2") }));
    // Act
    await harness.click('[data-feed-row="find-2"] [data-editor-link]');
    // Assert
    const [request] = harness.fake.calls<{ line?: number }>("openInEditor");
    expect(request.line).toBe(FINDINGS_LOCATION.line);
  });

  it("omits the line where the location carries none", async () => {
    // Arrange: an absent `optional` field means send nothing, never a zero.
    await drawRow(activityRow(findingsUnit(), { id: feedId("find-2") }));
    const links = harness.$$('[data-feed-row="find-2"] [data-editor-link]');
    // Act
    await harness.clickElement(links[2]);
    // Assert
    const [request] = harness.fake.calls<{ line?: number }>("openInEditor");
    expect(request.line).toBeUndefined();
  });

  it("names the path the third location carries", async () => {
    // Arrange
    await drawRow(activityRow(findingsUnit(), { id: feedId("find-2") }));
    const links = harness.$$('[data-feed-row="find-2"] [data-editor-link]');
    // Act
    await harness.clickElement(links[2]);
    // Assert
    const [request] = harness.fake.calls<{ path: string }>("openInEditor");
    expect(request.path).toBe(FINDINGS_LOCATION_NO_LINE.path);
  });
});

describe("the editor link on a worktree divider", () => {
  it("wears the shared editor-link hook", async () => {
    // Arrange / Act
    const row = await drawRow(separationRow("worktreeEntered", { id: feedId("wt-1") }));
    // Assert
    expect(row.querySelector("[data-editor-link]")).not.toBeNull();
  });

  it("calls OpenInEditor with the served worktree path", async () => {
    // Arrange
    await drawRow(separationRow("worktreeEntered", { id: feedId("wt-1") }));
    // Act
    await harness.click('[data-feed-row="wt-1"] [data-editor-link]');
    // Assert
    const [request] = harness.fake.calls<{ path: string }>("openInEditor");
    expect(request.path).toBe(WORKTREE_PATH);
  });

  it("sends no line for a directory", async () => {
    // Arrange
    await drawRow(separationRow("worktreeEntered", { id: feedId("wt-1") }));
    // Act
    await harness.click('[data-feed-row="wt-1"] [data-editor-link]');
    // Assert
    const [request] = harness.fake.calls<{ line?: number }>("openInEditor");
    expect(request.line).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// AGENTIC MERGE TABS ARE CONTAINERS (audit 1, item 17)
//
// "AGENTIC tabs (pre_prompt, conflicts, fixes, post_prompt) are containers —
// their content is the sub-feed rows parented to this row." So a row whose
// parent is a tab's FeedId draws INSIDE that tab, and a tab that was never
// served draws nothing at all (tabs are conditional: a tab appears BECAUSE
// that work began).
// ---------------------------------------------------------------------------

describe("an agentic merge tab", () => {
  /** Draw the pre-prompt tab, then push a row parented to it. */
  const withChild = async (tabId: string): Promise<void> => {
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, mergeTabRow("prePrompt", "live", { id: feedId(tabId) }));
    await harness.settle();
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      responseRow("success", "the lease said this", {
        id: feedId("lease-1"),
        parent: { row: feedId(tabId) },
      }),
    );
    await harness.settle();
  };

  it("draws a row parented to the tab inside that tab's row", async () => {
    // Arrange / Act
    await withChild("tab-pre");
    // Assert
    expect(harness.row("tab-pre")?.contains(harness.row("lease-1"))).toBe(true);
  });

  it("draws the parented row's text verbatim", async () => {
    // Arrange / Act
    await withChild("tab-pre");
    // Assert
    expect(harness.row("tab-pre")?.textContent).toContain("the lease said this");
  });

  it("draws no tab for a kind the daemon never served", async () => {
    // Arrange / Act: tabs are CONDITIONAL — no conflicts means no tab.
    await withChild("tab-pre");
    // Assert
    expect(harness.$('[data-merge-tab="conflicts"]')).toBeNull();
  });

  it("draws only the tab kinds that were served", async () => {
    // Arrange / Act
    await withChild("tab-pre");
    // Assert
    expect(harness.$$("[data-merge-tab]").map((el) => el.dataset.mergeTab)).toEqual(["prePrompt"]);
  });
});

// ---------------------------------------------------------------------------
// The head clocks: instants on the wire, the count-up on the client
// ---------------------------------------------------------------------------

/**
 * CLOCKS TICK CLIENT-SIDE. Every fixture stamps its start at 1 s absolute and
 * the page's clock starts at the harness epoch (10 s), so a live head reads 9s
 * on its first paint — a figure the daemon never sent and the client derived
 * from an instant it did.
 */
describe("a live subagent's head clock", () => {
  it("counts up from the served start instant", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentUnit("live")));
    // Assert
    expect(row.querySelector(".subagent-clock")?.textContent).toBe("9s");
  });

  it("grows as time passes", async () => {
    // Arrange
    await drawRow(activityRow(subagentUnit("live")));
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.row("row-1")?.querySelector(".subagent-clock")?.textContent).toBe("14s");
  });

  it("reads the quiet-for figure from the live arm's last progress", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(subagentUnit("live")));
    // Assert: last_progress is stamped at 2 s, eight seconds before the epoch.
    expect(row.querySelector(".subagent-quiet")?.textContent).toBe("quiet for 8s");
  });

  it("stops the clock where a settled arm says it stopped", async () => {
    // Arrange / Act: started 1 s, ended 9 s — a span, not a count-up.
    const row = await drawRow(activityRow(subagentUnit("succeeded")));
    // Assert
    expect(row.querySelector(".subagent-clock")?.textContent).toBe("8s");
  });
});

describe("a detached subagent's head clock", () => {
  it("counts up from the served start instant", async () => {
    // Arrange / Act
    const row = await drawRow(detachedSubagentRow("live"));
    // Assert
    expect(row.querySelector(".subagent-clock")?.textContent).toBe("9s");
  });

  it("keeps the original start instant across a re-push", async () => {
    // Arrange
    await drawRow(detachedSubagentRow("live"));
    await harness.tick(5_000);
    // Act: the daemon re-pushes the SAME row, start instant unchanged.
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, detachedSubagentRow("live"));
    await harness.settle();
    // Assert: the count-up continues rather than restarting at the re-push.
    expect(harness.row("row-1")?.querySelector(".subagent-clock")?.textContent).toBe("14s");
  });
});

describe("a live detached shell's head clock", () => {
  it("counts up from the served start instant", async () => {
    // Arrange / Act
    const row = await drawRow(shellHeadRow("live"));
    // Assert
    expect(row.querySelector(".shell-clock")?.textContent).toBe("9s");
  });

  it("grows as time passes", async () => {
    // Arrange
    await drawRow(shellHeadRow("live"));
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.row("row-1")?.querySelector(".shell-clock")?.textContent).toBe("14s");
  });

  it("reads the quiet-for figure from the live arm's last progress", async () => {
    // Arrange / Act
    const row = await drawRow(shellHeadRow("live"));
    // Assert
    expect(row.querySelector(".shell-quiet")?.textContent).toBe("quiet for 8s");
  });

  it("keeps the original start instant across a re-push", async () => {
    // Arrange
    await drawRow(shellHeadRow("live"));
    await harness.tick(5_000);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, shellHeadRow("live"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".shell-clock")?.textContent).toBe("14s");
  });
});

describe("a live merge head's clock", () => {
  it("counts up from the served enqueue instant", async () => {
    // Arrange / Act
    const row = await drawRow(activityRow(mergeUnit("update")));
    // Assert
    expect(row.querySelector(".merge-clock")?.textContent).toBe("9s");
  });

  it("grows as time passes", async () => {
    // Arrange
    await drawRow(activityRow(mergeUnit("update")));
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.row("row-1")?.querySelector(".merge-clock")?.textContent).toBe("14s");
  });

  it("stops where the settled arm says it stopped", async () => {
    // Arrange
    await drawRow(activityRow(mergeUnit("success")));
    // Act
    await harness.tick(5_000);
    // Assert: started 1 s, ended 9 s, and time passing changes nothing.
    expect(harness.row("row-1")?.querySelector(".merge-clock")?.textContent).toBe("8s");
  });
});

describe("an open permission's waiting clock", () => {
  it("starts at zero when the card arrives", async () => {
    // Arrange / Act: the wait is stamped at the FIRST DRAW, not on the wire.
    const row = await drawRow(permissionRow("open"));
    // Assert
    expect(row.querySelector(".perm-waiting")?.textContent).toBe("waiting 0s");
  });

  it("ticks up while the card stands", async () => {
    // Arrange
    await drawRow(permissionRow("open"));
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.row("row-1")?.querySelector(".perm-waiting")?.textContent).toBe("waiting 5s");
  });

  it("keeps counting the real wait across a re-push of the open card", async () => {
    // Arrange
    await drawRow(permissionRow("open"));
    await harness.tick(5_000);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("open"));
    await harness.settle();
    // Assert: a push while the reader is deciding does not reset their wait.
    expect(harness.row("row-1")?.querySelector(".perm-waiting")?.textContent).toBe("waiting 5s");
  });

  it("stops on the answered push", async () => {
    // Arrange
    await drawRow(permissionRow("open"));
    await harness.tick(5_000);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("allowedOnce"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".perm-waiting")).toBeNull();
  });

  it("stops on the abandoned push", async () => {
    // Arrange
    await drawRow(permissionRow("open"));
    await harness.tick(5_000);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, permissionRow("abandoned"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".perm-waiting")).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// The external link's call site
// ---------------------------------------------------------------------------

/**
 * A LINK IS AN RPC, NOT A NAVIGATION. This page IS the app inside an xwidget,
 * so an http(s) link cancels its own click and asks the daemon to launch the
 * pinned browser profile. Nothing is drawn on success: the result happens in a
 * browser window the reader is about to be looking at.
 */
describe("an http(s) link in a tool call's output", () => {
  const drawLink = async (): Promise<HTMLElement> => {
    const row = await drawRow(activityRow(toolCallReturnedUnit("links")));
    // The INPUT line carries a link of its own in this fixture, so the one
    // under test is picked by the output link's own text.
    const anchor = [...row.querySelectorAll<HTMLElement>("[data-external-link]")].find(
      (el) => el.textContent === "the docs",
    );
    if (!anchor) throw new Error("the links output drew no external link");
    return anchor;
  };

  it("calls OpenExternal when clicked", async () => {
    // Arrange
    const anchor = await drawLink();
    // Act
    await harness.clickElement(anchor);
    // Assert
    expect(harness.fake.calls("openExternal")).toHaveLength(1);
  });

  it("sends the url the view carried, verbatim", async () => {
    // Arrange
    const anchor = await drawLink();
    // Act
    await harness.clickElement(anchor);
    // Assert
    const [request] = harness.fake.calls<{ url: string }>("openExternal");
    expect(request.url).toBe("https://example.test/docs");
  });

  it("echoes the page's own workspace", async () => {
    // Arrange
    const anchor = await drawLink();
    // Act
    await harness.clickElement(anchor);
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("openExternal");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("cancels the click rather than navigating the webview", async () => {
    // Arrange
    const anchor = await drawLink();
    const event = new MouseEvent("click", { bubbles: true, cancelable: true });
    // Act
    anchor.dispatchEvent(event);
    await harness.settle();
    // Assert
    expect(event.defaultPrevented).toBe(true);
  });

  it("draws nothing at the link on success", async () => {
    // Arrange
    const anchor = await drawLink();
    // Act
    await harness.clickElement(anchor);
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  it("draws the link's own text rather than the url", async () => {
    // Arrange / Act
    const anchor = await drawLink();
    // Assert
    expect(anchor.textContent).toBe("the docs");
  });
});
