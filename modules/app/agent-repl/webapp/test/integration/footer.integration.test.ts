/**
 * FOOTER — the status strip, its chips, and its fully-resolved panels.
 *
 * The footer is where "server-driven" is easiest to break: the old client had
 * a phase-to-word table and counted rows to label chips. So the assertions
 * here are deliberately about WORDS AND FIGURES THE DAEMON SENT — the status
 * cell reads lowercase-with-spaces off the arm name, the activity line is
 * drawn not composed, the chip counts are the served numbers, and opening a
 * panel costs no round trip because the panels already arrived.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { create, type DescMessage } from "@bufbuild/protobuf";

import {
  InterruptResponseSchema,
  InterruptSuccessSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import {
  FooterActivityTransientSchema,
  FooterAgentRowWaitingForApiSchema,
  FooterAllowanceSchema,
  FooterExpandedFocusSchema,
  FooterMergeStepSuiteSchema,
  FooterMergeTestRowStateSchema,
  FooterStatusActivityMergeStepSchema,
  FooterStatusSchema,
  FooterTokensCellVerdictSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { panelStorageKey } from "../../src/footer/footer";
import {
  assertVocabCoversArms,
  RENDER_COLORS,
  footerStatusColor,
} from "./vocab";
import {
  FOOTER_ENDURING,
  FOOTER_SALIENT_KINDS,
  FOOTER_ALLOWANCE_ARMS,
  FOOTER_CHIPS,
  FOOTER_PANELS,
  FOOTER_STATUS_SALIENTS,
  FOOTER_TRANSIENT_KINDS,
  FOOTER_STATUS_ARMS,
  FOOTER_STATUS_SUBSTATUSES,
  FOOTER_STATUS_WITHOUT_SUBSTATUS,
  FOOTER_AGENT_TARGET,
  FOOTER_SHELL_TARGET,
  FOOTER_MONITOR_TARGET,
  FOOTER_TOKENS_VERDICTS,
  FOOTER_FOCUS_PANELS,
  MERGE_STEP_LINES,
  MERGE_SUBSTATUS_WORDS,
  MERGE_SUITE_EDGES,
  MERGE_TEST_ROWS,
  MERGE_TEST_ROW_STATES,
  WORKSPACE_ID,
  activityRow,
  detachedShellRow,
  monitorCallUnit,
  feedId,
  feedPageSuccess,
  footerView,
  enduringUsage,
  subagentUnit,
  assertCoversOneof,
} from "./fixtures";
import { ROOT_FEED } from "./fake-daemon";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one footer view already scripted. */
/** A failed deploy's fault activity, as the daemon composes it. */
const FAILED_DEPLOY = { kind: "deploy_failed", detail: "build webapp: error TS2322" };

async function withFooter(init: Parameters<typeof footerView>[0]): Promise<Harness> {
  harness = await startHarness({ arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView(init)) });
  return harness;
}

/**
 * The salient message STATUS's activity cell holds, read off the descriptors:
 * the cell's `salient` field, directly or under its `tier` oneof.
 */
function salientSchemaOf(status: string): DescMessage {
  const arm = FooterStatusSchema.fields.find((f) => f.localName === status);
  const cell = arm?.message?.fields.find((f) => f.localName === "activity")?.message;
  const salient = cell?.fields.find((f) => f.localName === "salient")?.message;
  if (salient === undefined) throw new Error(`no salient message under ${status}`);
  return salient;
}

describe("arm coverage", () => {
  it("covers every footer status arm", () => {
    assertCoversOneof(FooterStatusSchema, "status", FOOTER_STATUS_ARMS);
  });

  it("gives every status arm a color in the vocabulary", () => {
    assertVocabCoversArms(RENDER_COLORS.footer_status, FOOTER_STATUS_ARMS, "footer_status");
  });

  it("covers every allowance verdict arm", () => {
    assertCoversOneof(FooterAllowanceSchema, "status", [...FOOTER_ALLOWANCE_ARMS]);
  });

  it("covers every tokens-cell verdict", () => {
    assertCoversOneof(FooterTokensCellVerdictSchema, "verdict", [...FOOTER_TOKENS_VERDICTS]);
  });
});

describe("the status cell", () => {
  it.each(FOOTER_STATUS_ARMS)("carries the %s arm", async (status) => {
    // Arrange / Act
    await withFooter({ status });
    // Assert
    expect(harness.$(".footer-status")?.dataset.arm).toBe(status);
  });

  it.each(FOOTER_STATUS_ARMS)("paints the %s arm the vocabulary's color", async (status) => {
    // Arrange / Act
    await withFooter({ status });
    // Assert
    expect(harness.$(".footer-status")?.className).toContain(`tone-${footerStatusColor(status)}`);
  });

  it("reads a single-word arm lowercase", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "testing" });
    // Assert
    expect(harness.text(".footer-status")).toContain("merging");
  });

  it("reads a compound substatus arm as lowercase words separated by spaces", async () => {
    // Arrange / Act: FooterSubStatusCloseBlocked under `closing`.
    await withFooter({ status: "closing", substatus: "blocked" });
    // Assert
    expect(harness.$('[data-component="footer"]')?.textContent).toContain("close blocked");
  });

  it("reads the conflict resolution merging substatus as two words", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "conflictResolution" });
    // Assert
    expect(harness.text(".footer-substatus")).toBe("conflict resolution");
  });

  it("reads the host-shutdown interrupt substatus as words", async () => {
    // Arrange / Act
    await withFooter({ status: "interrupted", substatus: "hostShutdown" });
    // Assert
    expect(harness.$('[data-component="footer"]')?.textContent).toContain("host shutdown");
  });
});

describe("substatuses", () => {
  it.each(
    FOOTER_STATUS_ARMS.flatMap((status) =>
      FOOTER_STATUS_SUBSTATUSES[status].map((substatus) => ({ status, substatus })),
    ),
  )("draws $status/$substatus", async ({ status, substatus }) => {
    // Arrange / Act
    await withFooter({ status, substatus });
    // Assert
    expect(harness.$(".footer-substatus")?.dataset.arm).toBe(substatus);
  });

  it("merges the status into the status cell when the arm has no substatus", async () => {
    // Arrange / Act: `background` declares no substatus oneof at all.
    await withFooter({ status: FOOTER_STATUS_WITHOUT_SUBSTATUS });
    // Assert
    expect(harness.$(".footer-substatus")).toBeNull();
  });

  it("still names the status when there is no substatus to merge", async () => {
    // Arrange / Act
    await withFooter({ status: FOOTER_STATUS_WITHOUT_SUBSTATUS });
    // Assert
    expect(harness.text(".footer-status")).toContain(FOOTER_STATUS_WITHOUT_SUBSTATUS);
  });

  it.each(
    Object.entries(MERGE_SUBSTATUS_WORDS).flatMap(([status, words]) =>
      Object.entries(words).map(([substatus, drawn]) => ({ status, substatus, drawn })),
    ),
  )("reads $status/$substatus as '$drawn'", async ({ status, substatus, drawn }) => {
    // Arrange / Act
    await withFooter({ status, substatus });
    // Assert
    expect(harness.text(".footer-substatus")).toBe(drawn);
  });

  it("gives every merge substatus its words", () => {
    // Assert: the words table names exactly the substatuses the contract has.
    expect(
      Object.entries(MERGE_SUBSTATUS_WORDS).map(([status, words]) => [status, Object.keys(words).sort()]),
    ).toEqual(
      Object.keys(MERGE_SUBSTATUS_WORDS).map((status) => [status, [...FOOTER_STATUS_SUBSTATUSES[status]].sort()]),
    );
  });

  it("reads merge failed as its status word", async () => {
    // Arrange / Act
    await withFooter({ status: "mergeFailed", substatus: "tests" });
    // Assert
    expect(harness.text(".footer-status")).toBe("merge failed");
  });

  it("paints merge failed turquoise", async () => {
    // Arrange / Act
    await withFooter({ status: "mergeFailed", substatus: "conflicts" });
    // Assert
    expect(harness.$(".footer-status")?.classList.contains("tone-turquoise")).toBe(true);
  });
});

describe("the merge step line", () => {
  it("covers every merge step arm", () => {
    assertCoversOneof(FooterStatusActivityMergeStepSchema, "step", Object.keys(MERGE_STEP_LINES));
  });

  it("covers every suite edge", () => {
    assertCoversOneof(FooterMergeStepSuiteSchema, "edge", Object.keys(MERGE_SUITE_EDGES));
  });

  it.each(Object.entries(MERGE_STEP_LINES).map(([step, line]) => ({ step, ...line })))(
    "draws the $step step's line as '$words'",
    async ({ step, value, words }) => {
      // Arrange / Act
      await withFooter({
        status: "merging",
        substatus: "testing",
        activity: "mergeStep",
        activityOverride: { step: { case: step, value } },
      });
      // Assert
      expect(harness.text(".footer-activity-merge-step")).toBe(words);
    },
  );

  it.each(Object.entries(MERGE_SUITE_EDGES).map(([edge, drawn]) => ({ edge, ...drawn })))(
    "draws a suite's $edge edge as '$words'",
    async ({ edge, words }) => {
      // Arrange / Act
      await withFooter({
        status: "merging",
        substatus: "testing",
        activity: "mergeStep",
        activityOverride: { step: { case: "testing", value: { name: "webapp", edge: { case: edge, value: {} } } } },
      });
      // Assert
      expect(harness.text(".footer-activity-merge-step")).toBe(words);
    },
  );

  it.each(
    Object.entries(MERGE_SUITE_EDGES)
      .filter(([, drawn]) => drawn.tone !== null)
      .map(([edge, drawn]) => ({ edge, tone: drawn.tone as string })),
  )("paints a suite's $edge edge $tone", async ({ edge, tone }) => {
    // Arrange / Act
    await withFooter({
      status: "merging",
      substatus: "testing",
      activity: "mergeStep",
      activityOverride: { step: { case: "testing", value: { name: "webapp", edge: { case: edge, value: {} } } } },
    });
    // Assert
    expect(harness.$(".footer-activity-merge-step")?.classList.contains(tone)).toBe(true);
  });

  it("stands under merge failed, which carries the merge's own cell", async () => {
    // Arrange / Act
    await withFooter({ status: "mergeFailed", substatus: "tests", activity: "mergeStep" });
    // Assert
    expect(harness.$(".footer-activity")?.dataset.arm).toBe("mergeStep");
  });
});

describe("the merge tests chip and panel", () => {
  it("covers every suite state the panel draws", () => {
    assertCoversOneof(FooterMergeTestRowStateSchema, "state", [...MERGE_TEST_ROW_STATES]);
  });

  it("covers every panel the daemon's focus can name", () => {
    assertCoversOneof(FooterExpandedFocusSchema, "panel", [...FOOTER_FOCUS_PANELS]);
  });

  it("draws the 🧪 chip as the gate's served fraction", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "testing" });
    // Assert
    expect(harness.text('.footer-chip[data-chip="mergeTests"]')).toBe("🧪 8/12");
  });

  it("draws one panel row per suite, in the gate's order", async () => {
    // Arrange
    await withFooter({ status: "merging", substatus: "testing" });
    // Act
    await harness.click('.footer-chip[data-chip="mergeTests"]');
    // Assert
    expect(
      harness.$$('.footer-expanded[data-panel="mergeTests"] .footer-row-label').map((el) => el.textContent),
    ).toEqual(MERGE_TEST_ROWS.map((row) => row.name?.text));
  });

  it("stamps each row with its suite's state", async () => {
    // Arrange
    await withFooter({ status: "merging", substatus: "testing" });
    // Act
    await harness.click('.footer-chip[data-chip="mergeTests"]');
    // Assert
    expect(
      harness.$$('.footer-expanded[data-panel="mergeTests"] [data-suite-state]').map((el) => el.dataset.suiteState),
    ).toEqual([...MERGE_TEST_ROW_STATES]);
  });

  it("shows a finished suite's run time", async () => {
    // Arrange
    await withFooter({ status: "merging", substatus: "testing" });
    // Act
    await harness.click('.footer-chip[data-chip="mergeTests"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="mergeTests"] [data-suite-state="passed"] [data-duration]')).toBe(
      "1m 35s",
    );
  });

  it("ticks a running suite's clock", async () => {
    // Arrange
    await withFooter({ status: "merging", substatus: "testing" });
    // Act
    await harness.click('.footer-chip[data-chip="mergeTests"]');
    // Assert
    expect(harness.$('.footer-expanded[data-panel="mergeTests"] [data-suite-state="running"] .footer-row-clock')).not.toBeNull();
  });

  it("opens and selects the merge tests panel when the daemon focuses it", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "testing", focus: { panel: "mergeTests", generation: 1n } });
    // Assert
    expect(harness.$$(".footer-expanded[data-panel]").map((el) => el.dataset.panel)).toEqual(["mergeTests"]);
  });

  it("marks the 🧪 chip selected when the focus opens its panel", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "testing", focus: { panel: "mergeTests", generation: 1n } });
    // Assert
    expect(harness.$('.footer-chip[data-chip="mergeTests"]')?.dataset.selected).toBe("true");
  });

  it("moves the reader's open panel onto the merge tests when testing begins", async () => {
    // Arrange: the reader has the agents panel open.
    await withFooter({ status: "working" });
    await harness.click('.footer-chip[data-chip="agents"]');
    // Act: the merge begins testing and the daemon mints a focus.
    harness.fake.setFooter(
      WORKSPACE_ID,
      footerView({ status: "merging", substatus: "testing", focus: { panel: "mergeTests", generation: 1n } }),
    );
    await harness.settle();
    // Assert
    expect(harness.$$(".footer-expanded[data-panel]").map((el) => el.dataset.panel)).toEqual(["mergeTests"]);
  });

  it("closes the section when testing ends and the panel empties", async () => {
    // Arrange
    await withFooter({ status: "merging", substatus: "testing", focus: { panel: "mergeTests", generation: 1n } });
    // Act: testing ended — the chip is unset and the panel empty.
    harness.fake.setFooter(
      WORKSPACE_ID,
      footerView({
        status: "merging",
        substatus: "committing",
        focus: { panel: "mergeTests", generation: 1n },
        chips: { agents: true, tasks: true, shells: true, monitors: true, crons: true, mergeTests: false },
      }),
    );
    await harness.settle();
    // Assert
    expect(harness.$(".footer-expanded")).toBeNull();
  });

  it("does not reopen the panel on a push repeating an applied focus", async () => {
    // Arrange: focused, then the reader closes the section with the chip.
    await withFooter({ status: "merging", substatus: "testing", focus: { panel: "mergeTests", generation: 1n } });
    await harness.click('.footer-chip[data-chip="mergeTests"]');
    // Act
    harness.fake.setFooter(
      WORKSPACE_ID,
      footerView({ status: "merging", substatus: "testing", focus: { panel: "mergeTests", generation: 1n } }),
    );
    await harness.settle();
    // Assert
    expect(harness.$(".footer-expanded")).toBeNull();
  });
});

describe("the activity cell", () => {
  it.each(
    FOOTER_STATUS_ARMS.flatMap((status) =>
      FOOTER_STATUS_SALIENTS[status].map((activity) => ({ status, activity })),
    ),
  )("draws the $activity salient line under $status", async ({ status, activity }) => {
    // Arrange / Act
    await withFooter({ status, activity });
    // Assert
    expect(harness.$(".footer-activity")?.dataset.arm).toBe(activity);
  });

  it.each(Object.keys(FOOTER_TRANSIENT_KINDS))(
    "draws the %s transient over the enduring line",
    async (activity) => {
      // Arrange / Act
      await withFooter({ status: "working", activity });
      // Assert
      expect(harness.$(".footer-activity")?.dataset.arm).toBe(activity);
    },
  );

  it.each(FOOTER_STATUS_ARMS.filter((status) => status !== "waiting"))(
    "draws the enduring line under %s when nothing else stands",
    async (status) => {
      // Arrange / Act
      await withFooter({ status });
      // Assert
      expect(harness.$(".footer-activity")?.dataset.tier).toBe("enduring");
    },
  );

  it("marks a salient line's tier on the cell", async () => {
    await withFooter({ status: "working", activity: "retrying" });
    expect(harness.$(".footer-activity")?.dataset.tier).toBe("salient");
  });

  it("marks a live transient's tier on the cell", async () => {
    await withFooter({ status: "working", activity: "hook" });
    expect(harness.$(".footer-activity")?.dataset.tier).toBe("transient");
  });

  // THE CLIENT'S ONE CLOCK DECISION: the daemon pushes nothing at the lapse.
  it("gives way to the enduring line when the transient's expiry passes", async () => {
    // Arrange: live for five seconds past the suite's epoch.
    await withFooter({ status: "working", activity: "hook", expiresAtMs: 15_000n });
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.$(".footer-activity")?.dataset.tier).toBe("enduring");
  });

  it("draws a transient that already lapsed as the enduring line", async () => {
    await withFooter({ status: "working", activity: "hook", expiresAtMs: 5_000n });
    expect(harness.$(".footer-activity")?.dataset.tier).toBe("enduring");
  });

  it("prefixes a subagent's transient with its label", async () => {
    await withFooter({ status: "working", activity: "toolCall", agent: "Explore" });
    expect(harness.text(".footer-activity-transient")).toBe("Explore · Bash: npm test");
  });

  // A FAILED DEPLOY (owner request, 2026-09-28) stands on every strip's
  // ACTIVITY line through the fault arm, and comes down with the fault.
  it("draws a failed deploy on the activity line", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", activity: "fault", activityOverride: FAILED_DEPLOY });
    // Assert
    expect(harness.text(".footer-activity-fault")).toBe("deploy failed \u00b7 build webapp: error TS2322");
  });

  it("takes a failed deploy off the activity line when the fault closes", async () => {
    // Arrange
    await withFooter({ status: "idle", activity: "fault", activityOverride: FAILED_DEPLOY });
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
    await harness.settle();
    // Assert
    expect(harness.$(".footer-activity-fault")).toBeNull();
  });

  it("draws the notification's composed text verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", activity: "notification" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("the agent addressed you");
  });

  // THE SENTENCE, NOT MERELY THE ARM. The table above pins `data-arm` for every
  // status/activity pair, which a build that drew an empty cell would still
  // satisfy. A dead query is a FAILED TURN (owner ruling, 2026-09-28), so its
  // salient line stands under `turn_failed`.
  it("draws the dead-query sentence under the turn_failed arm", async () => {
    // Arrange / Act
    await withFooter({ status: "turnFailed", activity: "queryDied" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("the vendor query died");
  });

  it("draws the hook's own name verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "working", activity: "hook" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("PreToolUse");
  });

  it("colors the commit updating main fast-forwards to as a typed datum", async () => {
    // Arrange / Act
    await withFooter({
      status: "merging",
      substatus: "updatingMain",
      activity: "mergeStep",
      activityOverride: { step: { case: "updatingMain", value: MERGE_STEP_LINES.updatingMain.value } },
    });
    // Assert
    expect(harness.$(".footer-activity [data-datum='sha']")?.textContent).toBe("4f2a1c9");
  });

  it("colors a conflict's file count as a typed datum", async () => {
    // Arrange / Act
    await withFooter({
      status: "merging",
      substatus: "conflictResolution",
      activity: "mergeStep",
      activityOverride: { step: { case: "conflictResolution", value: MERGE_STEP_LINES.conflictResolution.value } },
    });
    // Assert
    expect(harness.$(".footer-activity [data-datum='count']")?.textContent).toBe("3");
  });

  it("colors the retry attempt as a typed datum", async () => {
    // Arrange / Act
    await withFooter({ status: "working", activity: "retrying" });
    // Assert
    expect(harness.$(".footer-activity [data-datum='attempt']")?.textContent).toContain("3");
  });

  it.each(FOOTER_ALLOWANCE_ARMS)("paints no tone on the %s allowance, whose percentage alone is colored", async (arm) => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({
            status: "idle",
            activity: FOOTER_ENDURING,
            activityOverride: enduringUsage(arm),
          }),
        ),
    });
    // Assert
    expect(harness.$(`.footer-activity [data-allowance][data-arm="${arm}"]`)?.className).not.toContain("tone-");
  });

  it.each(FOOTER_ALLOWANCE_ARMS)("draws the %s allowance verdict as its own arm", async (arm) => {
    // Arrange: the free-text status string was retired for a typed oneof, so
    // the verdict is an ARM the client draws, never a word it echoes.
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({
            status: "idle",
            activity: FOOTER_ENDURING,
            activityOverride: enduringUsage(arm),
          }),
        ),
    });
    // Assert
    expect(harness.$(`.footer-activity [data-allowance][data-arm="${arm}"]`)).not.toBeNull();
  });

  it("draws an allowance with no verdict yet, figures and all", async () => {
    // Arrange: UNSET is legal — no rate-limit event observed for the window
    // yet, so the sampled figures stand with no verdict class.
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({
            status: "idle",
            activity: FOOTER_ENDURING,
            activityOverride: enduringUsage(undefined),
          }),
        ),
    });
    // Assert
    expect(harness.$(".footer-activity [data-allowance]")).not.toBeNull();
  });

  it("gives an unset verdict no arm class at all", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({
            status: "idle",
            activity: FOOTER_ENDURING,
            activityOverride: enduringUsage(undefined),
          }),
        ),
    });
    // Assert: unset is not a fourth verdict, and not a malformed view either.
    expect(harness.$(".footer-activity [data-allowance]")?.dataset.arm).toBeUndefined();
  });

  it("reports no malformed frame for an unset verdict", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({
            status: "idle",
            activity: FOOTER_ENDURING,
            activityOverride: enduringUsage(undefined),
          }),
        ),
    });
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });

  it("draws both the session and the weekly allowance", async () => {
    // Arrange / Act
    await withFooter({
      status: "idle",
      activity: FOOTER_ENDURING,
      activityOverride: enduringUsage("allowedWarning"),
    });
    // Assert
    expect(harness.$$(".footer-activity [data-allowance]")).toHaveLength(2);
  });

  it("marks the session allowance apart from the weekly one", async () => {
    // Arrange / Act
    await withFooter({
      status: "idle",
      activity: FOOTER_ENDURING,
      activityOverride: enduringUsage("allowedWarning"),
    });
    // Assert
    expect(
      harness.$$(".footer-activity [data-allowance]").map((el) => el.dataset.allowance),
    ).toEqual(["session", "weekly"]);
  });

  it("counts down the wakeup activity from its served instant", async () => {
    // Arrange
    await withFooter({ status: "waiting", activity: "wakeup" });
    const before = harness.text(".footer-activity");
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.text(".footer-activity")).not.toBe(before);
  });

  it("draws the wakeup reason verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "waiting", activity: "wakeup" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("the cron fires");
  });

  it("covers every salient kind the fixtures declare", () => {
    // Assert: the union of the per-status tables is the whole salient vocabulary.
    const used = new Set(Object.values(FOOTER_STATUS_SALIENTS).flat());
    expect([...used].sort()).toEqual(Object.keys(FOOTER_SALIENT_KINDS).sort());
  });

  it.each(FOOTER_STATUS_ARMS)("covers every salient kind the %s arm declares", (status) => {
    assertCoversOneof(salientSchemaOf(status), "kind", FOOTER_STATUS_SALIENTS[status]);
  });

  it("covers every transient kind", () => {
    assertCoversOneof(FooterActivityTransientSchema, "kind", Object.keys(FOOTER_TRANSIENT_KINDS));
  });
});

describe("the agents chip's waiting-for-the-API glyph", () => {
  it("draws the glyph with the daemon's waiting count", async () => {
    await withFooter({ status: "background", agentsWaitingForApi: 2 });
    expect(harness.$(".footer-chip-waiting")?.dataset.waitingForApi).toBe("2");
  });

  it("draws no glyph while no agent waits", async () => {
    await withFooter({ status: "background" });
    expect(harness.$(".footer-chip-waiting")).toBeNull();
  });
});

describe("the agents panel's waiting row", () => {
  it("draws an agent waiting for the API with its give-up countdown", async () => {
    // Arrange: the fixture's one agent, waiting until 24m 30s past the epoch (the countdown truncates to 24m).
    const view = footerView({ status: "background", agentsWaitingForApi: 1 });
    const row = view.expanded?.agents?.rows[0];
    if (row === undefined) throw new Error("the fixture carries no agent row");
    row.state = {
      case: "waitingForApi",
      value: create(FooterAgentRowWaitingForApiSchema, {
        failedAtMs: 1_000n,
        givesUpAtMs: 10_000n + 24n * 60_000n + 30_000n,
        resumesDelivered: 0,
      }),
    };
    harness = await startHarness({ arrange: (fake) => fake.setFooter(WORKSPACE_ID, view) });
    // Act
    await harness.click('[data-chip="agents"]');
    // Assert
    expect(harness.text(".footer-row-state")).toBe("waiting for the API · gives up in 24m");
  });
});

describe("the clock", () => {
  it("ticks up from the served turn start", async () => {
    // Arrange
    await withFooter({ status: "working", turnStartedAtMs: 0n });
    const before = harness.text(".footer-clock");
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.text(".footer-clock")).not.toBe(before);
  });

  it("reads as not live when no turn is running", async () => {
    // Arrange / Act: turn_started_at_ms is `optional`.
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({ status: "idle", substatus: "ready", turnStartedAtMs: undefined }),
        ),
    });
    // Assert: the cell keeps the baseline strip's idle dash (preamble §6 — the
    // existing look does not change); what it must not do is claim a turn.
    expect(harness.$(".footer-clock")?.dataset.live).toBe("false");
  });

  it("offers no stop control when no turn is running", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({ status: "idle", substatus: "ready", turnStartedAtMs: undefined }),
        ),
    });
    // Assert: a control whose only answer is "nothing running" is chrome.
    expect(harness.$(".footer-clock [data-interrupt]")).toBeNull();
  });
});

describe("the tokens cell", () => {
  it("draws the served figure verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", tokensText: "77.7k" });
    // Assert
    expect(harness.text(".footer-tokens")).toContain("77.7k");
  });

  it("draws the alarm glyph when the alarm marker is present", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", alarm: true });
    // Assert
    expect(harness.$(".footer-tokens [data-alarm]")).not.toBeNull();
  });

  it("draws no alarm glyph when the marker is absent", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", alarm: false });
    // Assert
    expect(harness.$(".footer-tokens [data-alarm]")).toBeNull();
  });

  it.each(FOOTER_TOKENS_VERDICTS)("draws the %s verdict badge", async (verdict) => {
    // Arrange / Act
    await withFooter({ status: "idle", verdict });
    // Assert
    expect(harness.$(`.footer-tokens [data-verdict="${verdict}"]`)).not.toBeNull();
  });

  it("draws no verdict badge when none is served", async () => {
    // Arrange / Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(".footer-tokens [data-verdict]")).toBeNull();
  });
});

describe("the live-work chips", () => {
  it.each(FOOTER_CHIPS)("draws the %s chip when it is set", async (chip) => {
    // Arrange / Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(`.footer-chip[data-chip="${chip}"]`)).not.toBeNull();
  });

  it.each(FOOTER_CHIPS)("draws no %s chip when it is unset", async (chip) => {
    // Arrange / Act: an unset chip is not a zero — it is not drawn at all.
    await withFooter({ status: "idle", chips: { [chip]: false } });
    // Assert
    expect(harness.$(`.footer-chip[data-chip="${chip}"]`)).toBeNull();
  });

  it("draws the agents count verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.text('.footer-chip[data-chip="agents"]')).toContain("3");
  });

  it("draws the tasks chip as done over total", async () => {
    // Arrange / Act
    await withFooter({ status: "idle" });
    // Assert
    const drawn = harness.text('.footer-chip[data-chip="tasks"]') ?? "";
    expect(drawn).toContain("2");
    expect(drawn).toContain("5");
  });

  it("does not derive a chip count from the feed's rows", async () => {
    // Arrange: one subagent row in the feed, but the chip says three.
    harness = await startHarness({
      arrange: (fake) => {
        fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([activityRow(subagentUnit("live"))]));
      },
    });
    // Assert: the served figure wins; counting rows would say one.
    expect(harness.text('.footer-chip[data-chip="agents"]')).toContain("3");
  });
});

describe("the expanded panels", () => {
  it.each(FOOTER_CHIPS)("opens the %s panel when its chip is selected", async (chip) => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click(`.footer-chip[data-chip="${chip}"]`);
    // Assert
    expect(harness.$(`.footer-expanded[data-panel="${chip}"]`)).not.toBeNull();
  });

  it.each(FOOTER_CHIPS)("opens ONLY the %s panel", async (chip) => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click(`.footer-chip[data-chip="${chip}"]`);
    // Assert
    expect(harness.$$(".footer-expanded[data-panel]").map((el) => el.dataset.panel)).toEqual([chip]);
  });

  it("costs no round trip to open a panel", async () => {
    // Arrange: the panels arrive fully resolved on every push.
    await withFooter({ status: "idle" });
    harness.fake.clearCalls();
    // Act
    await harness.click('.footer-chip[data-chip="agents"]');
    // Assert
    expect(harness.fake.log()).toEqual([]);
  });

  it("draws no panel before a chip is selected", async () => {
    // Arrange / Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(".footer-expanded[data-panel]")).toBeNull();
  });

  it("names every panel the strip can open", () => {
    // Assert: tokens plus the six chips.
    expect(FOOTER_PANELS).toHaveLength(7);
  });

  it("draws every tokens line verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click(".footer-tokens");
    // Assert
    const drawn = harness.text('.footer-expanded[data-panel="tokens"]') ?? "";
    expect(drawn).toContain("180k");
  });

  it("draws the tokens panel's context growth verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click(".footer-tokens");
    // Assert
    expect(
      harness.text('.footer-expanded[data-panel="tokens"] [data-token-line="contextGrowth"] .footer-token-value'),
    ).toBe("18.2k");
  });

  it("draws every agent's share in the tokens panel", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click(".footer-tokens");
    // Assert
    expect(harness.text('.footer-expanded[data-panel="tokens"] [data-token-agent="reviewer · review the diff"]')).toBe(
      "reviewer · review the diff",
    );
  });

  it("opens the tokens panel from the idle cell's stated dash", async () => {
    // Arrange
    await withFooter({ status: "idle", tokensText: "--" });
    // Act
    await harness.click(".footer-tokens");
    // Assert
    expect(harness.$('.footer-expanded[data-panel="tokens"]')).not.toBeNull();
  });

  it("draws the tokens panel's verdict text verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click(".footer-tokens");
    // Assert
    expect(harness.text('.footer-expanded[data-panel="tokens"]')).toContain("usage still arriving");
  });

  it("draws every agents row", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="agents"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="agents"]')).toContain("reviewer");
  });

  it("draws every task row's status arm", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="tasks"]');
    // Assert
    const arms = harness
      .$$('.footer-expanded[data-panel="tasks"] [data-task-status]')
      .map((el) => el.dataset.taskStatus);
    expect(arms).toEqual(["pending", "running", "completed"]);
  });

  it("draws the running task's active form verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="tasks"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="tasks"]')).toContain("writing the harness");
  });

  it("draws the shells panel's command verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="shells"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="shells"]')).toContain("npm run watch");
  });

  it("draws the monitors panel's description verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="monitors"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="monitors"]')).toContain("watch the daemon log");
  });

  it("draws the crons panel's schedule and prompt verbatim", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="crons"]');
    // Assert
    const drawn = harness.text('.footer-expanded[data-panel="crons"]') ?? "";
    expect(drawn).toContain("every 5 minutes");
    expect(drawn).toContain("check the queue");
  });

  it("counts the cron's next fire down as time passes", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="crons"]');
    const before = harness.text('.footer-expanded[data-panel="crons"]');
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.text('.footer-expanded[data-panel="crons"]')).not.toBe(before);
  });

  it("counts the monitor's runtime up as time passes", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="monitors"]');
    const before = harness.text('.footer-expanded[data-panel="monitors"]');
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.text('.footer-expanded[data-panel="monitors"]')).not.toBe(before);
  });
});

describe("panel jump rows", () => {
  it("makes the agents row a jump target for its served FeedId", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="agents"]');
    // Assert
    expect(harness.$(`[data-jump="${FOOTER_AGENT_TARGET}"]`)).not.toBeNull();
  });

  it("makes the shells row a jump target for its served FeedId", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="shells"]');
    // Assert
    expect(harness.$(`[data-jump="${FOOTER_SHELL_TARGET}"]`)).not.toBeNull();
  });

  it("reveals the row when a jump target is clicked", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => {
        fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId(FOOTER_AGENT_TARGET) })]),
        );
      },
    });
    await harness.click('.footer-chip[data-chip="agents"]');
    // Act
    await harness.click(`[data-jump="${FOOTER_AGENT_TARGET}"]`);
    // Assert
    expect(harness.row(FOOTER_AGENT_TARGET)?.dataset.revealed).toBe("true");
  });

  it("makes a monitor row a jump row naming its tool-call card", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="monitors"]');
    // Assert
    expect(
      harness.$(`.footer-expanded[data-panel="monitors"] [data-jump="${FOOTER_MONITOR_TARGET}"]`),
    ).not.toBeNull();
  });

  it("says not on screen when an unplaced monitor's row is clicked", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="monitors"]');
    // Act
    await harness.click('.footer-expanded[data-panel="monitors"] [data-jump-unresolved="notDrawn"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="monitors"] .footer-row-unreachable')).toBe(
      "not on screen",
    );
  });

  it("makes no jump target of a cron row", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="crons"]');
    // Assert
    expect(harness.$('.footer-expanded[data-panel="crons"] [data-jump]')).toBeNull();
  });

  it("makes no jump target of a task row", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="tasks"]');
    // Assert
    expect(harness.$('.footer-expanded[data-panel="tasks"] [data-jump]')).toBeNull();
  });
});

describe("whole-view replacement", () => {
  it("drops a chip the next push omits", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", chips: { agents: false } }));
    await harness.settle();
    // Assert
    expect(harness.$('.footer-chip[data-chip="agents"]')).toBeNull();
  });

  it("replaces the status rather than accumulating it", async () => {
    // Arrange
    await withFooter({ status: "working" });
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", substatus: "done" }));
    await harness.settle();
    // Assert
    expect(harness.$$(".footer-status")).toHaveLength(1);
  });
});

// ---------------------------------------------------------------------------
// THE FOOTER'S TWO STOPS (audit 1, item 1)
//
// Stopping is ALWAYS an Interrupt rpc and THE ARM IS THE TARGET: the strip's
// stop beside the clock is the running TURN, the agents panel's header stop is
// EVERY live agent. The response's arm is the OUTCOME, and two of the three
// outcomes are answers rather than failures — a stop that found nothing running
// says so calmly, and a fan-wide stop reports the count the daemon reached.
// ---------------------------------------------------------------------------

describe("the turn stop", () => {
  it("sends the turn target", async () => {
    // Arrange
    await withFooter({ status: "working" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    const [request] = harness.fake.calls<{ target: { case?: string } }>("interrupt");
    expect(request.target.case).toBe("turn");
  });

  it("echoes the page's own workspace", async () => {
    // Arrange
    await withFooter({ status: "working" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("interrupt");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("draws the interrupted-turn outcome as a note, not a refusal", async () => {
    // Arrange
    await withFooter({ status: "working" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    expect(harness.$('.footer-stop-turn [data-stop-outcome="interruptedTurn"]')).not.toBeNull();
  });

  it("draws no refusal for an interrupted turn", async () => {
    // Arrange
    await withFooter({ status: "working" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  it("draws the nothing-running outcome as a note", async () => {
    // Arrange: a domain outcome, not an error — the stop found the session
    // already quiet, which is a legitimate reply to a legitimate ask.
    await withFooter({ status: "working" });
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: { case: "success", value: { outcome: { case: "nothingRunning", value: {} } } },
      }),
    );
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    expect(harness.$('.footer-stop-turn [data-stop-outcome="nothingRunning"]')).not.toBeNull();
  });

  it("draws no refusal for a nothing-running answer", async () => {
    // Arrange
    await withFooter({ status: "working" });
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: { case: "success", value: { outcome: { case: "nothingRunning", value: {} } } },
      }),
    );
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  it("covers every Interrupt success outcome", () => {
    assertCoversOneof(InterruptSuccessSchema, "outcome", [
      "interruptedTurn",
      "interruptedDetached",
      "nothingRunning",
    ]);
  });
});

describe("the agents panel's stop-all", () => {
  /** Open the agents panel, where the fan-wide stop lives. */
  const openAgents = async (): Promise<void> => {
    await withFooter({ status: "working" });
    await harness.click('.footer-chip[data-chip="agents"]');
  };

  it("sends the all-agents target", async () => {
    // Arrange
    await openAgents();
    // Act
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Assert
    const [request] = harness.fake.calls<{ target: { case?: string } }>("interrupt");
    expect(request.target.case).toBe("allAgents");
  });

  it("draws the count the daemon reported", async () => {
    // Arrange
    await openAgents();
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "interruptedDetached", value: { count: 4n } } },
        },
      }),
    );
    // Act
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Assert: the figure is the daemon's; counting the panel's rows would say 2.
    expect(harness.text('[data-stop-outcome="interruptedDetached"]')).toContain("4");
  });

  it("draws no refusal for an interrupted-detached answer", async () => {
    // Arrange
    await openAgents();
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "interruptedDetached", value: { count: 4n } } },
        },
      }),
    );
    // Act
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  // THE PUSH THE STOP CAUSED MUST NOT ERASE THE STOP'S ANSWER. The footer draws
  // its whole view per push and a stop always changes the view -- the live set
  // it just emptied is in it -- so a control rebuilt per draw loses the one
  // statement of the count anywhere in the product. Measured in the G51
  // playbook: the daemon answered `interrupted_detached count=3` and the
  // footer that came back carried a bare "stop all" with no note on it.
  it("keeps the count on the control when the next footer push lands", async () => {
    // Arrange
    await openAgents();
    harness.fake.answer(
      "interrupt",
      create(InterruptResponseSchema, {
        result: {
          case: "success",
          value: { outcome: { case: "interruptedDetached", value: { count: 3n } } },
        },
      }),
    );
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Act: the push the stop caused -- nothing is live any more.
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", substatus: "done" }));
    await harness.settle();
    // Assert
    expect(harness.text('[data-stop-outcome="interruptedDetached"]')).toContain("3");
  });

  it("keeps a refusal on the control when the next footer push lands", async () => {
    // Arrange: the same guarantee for the other thing a click can be told.
    await openAgents();
    harness.fake.refuse("interrupt", "confirmRequired");
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", substatus: "done" }));
    await harness.settle();
    // Assert
    expect(harness.$('.footer-stop-all .refusal[data-arm="confirmRequired"]')).not.toBeNull();
  });

  it("offers no confirm step on the fan-wide target", async () => {
    // Arrange: `confirm_agents` is meaningless on all_agents, so the challenge
    // arm arriving there is a dead end rather than a second button.
    await openAgents();
    harness.fake.refuse("interrupt", "confirmRequired");
    // Act
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Assert
    expect(harness.$("[data-interrupt-confirm]")).toBeNull();
  });

  it("draws the challenge arm as an ordinary refusal on the fan-wide target", async () => {
    // Arrange
    await openAgents();
    harness.fake.refuse("interrupt", "confirmRequired");
    // Act
    await harness.click('.footer-expanded[data-panel="agents"] [data-interrupt]');
    // Assert
    expect(harness.$('.footer-stop-all .refusal[data-arm="confirmRequired"]')).not.toBeNull();
  });
});

// ---------------------------------------------------------------------------
// R14: THE OPEN PANEL IS A WEBVIEW-LOCAL PREFERENCE (audit 1, item 16)
//
// Nothing is persisted client-side except the webview's own preferences, in
// `localStorage`, behind try/catch. So the selection survives a reload, and a
// storage that throws costs the memory and nothing else.
// ---------------------------------------------------------------------------

describe("the remembered panel", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("stores the panel a chip opened", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="shells"]');
    // Assert
    expect(window.localStorage.getItem(panelStorageKey(WORKSPACE_ID))).toBe("shells");
  });

  it("forgets the panel when it is closed again", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="shells"]');
    // Act
    await harness.click('.footer-chip[data-chip="shells"]');
    // Assert
    expect(window.localStorage.getItem(panelStorageKey(WORKSPACE_ID))).toBeNull();
  });

  it("re-opens the stored panel on a fresh mount", async () => {
    // Arrange: what a reload looks like — the preference is all that survives.
    window.localStorage.setItem(panelStorageKey(WORKSPACE_ID), "crons");
    // Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$('.footer-expanded[data-panel="crons"]')).not.toBeNull();
  });

  it("opens no panel on a fresh mount with nothing stored", async () => {
    // Arrange / Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(".footer-expanded[data-panel]")).toBeNull();
  });

  it("discards a stored value that is not a panel name", async () => {
    // Arrange: an older bundle's spelling, or a hand-edited entry.
    window.localStorage.setItem(panelStorageKey(WORKSPACE_ID), "not-a-panel");
    // Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(".footer-expanded[data-panel]")).toBeNull();
  });

  it("still draws the strip when reading storage throws", async () => {
    // Arrange: a webview with site data disabled throws on the accessor.
    vi.spyOn(window.localStorage, "getItem").mockImplementation(() => {
      throw new Error("site data is disabled");
    });
    // Act
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(".footer-status")).not.toBeNull();
  });

  it("still opens a panel when writing storage throws", async () => {
    // Arrange
    vi.spyOn(window.localStorage, "setItem").mockImplementation(() => {
      throw new Error("site data is disabled");
    });
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="tasks"]');
    // Assert: the preference is lost, the footer is not.
    expect(harness.$('.footer-expanded[data-panel="tasks"]')).not.toBeNull();
  });
});

/**
 * THE PANELS' OWN CLOCKS. `FooterAgentRowRuntime` and `FooterShellRowRuntime`
 * ship a start instant and nothing else; the count-up is the client's, through
 * the one shared ticker. Every fixture stamps 1 s absolute and the page's clock
 * starts at the harness epoch (10 s), so a first paint reads 9s.
 */
describe("the agents panel's runtime clock", () => {
  it("counts up from the served start instant", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="agents"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="agents"] .footer-row-clock')).toBe("9s");
  });

  it("grows as time passes", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="agents"]');
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.text('.footer-expanded[data-panel="agents"] .footer-row-clock')).toBe("14s");
  });
});

describe("the shells panel's runtime clock", () => {
  it("counts up from the served start instant", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="shells"]');
    // Assert
    expect(harness.text('.footer-expanded[data-panel="shells"] .footer-row-clock')).toBe("9s");
  });

  it("grows as time passes", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="shells"]');
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.text('.footer-expanded[data-panel="shells"] .footer-row-clock')).toBe("14s");
  });
});

describe("the activity line's relative age", () => {
  it("reads the age since the served instant", async () => {
    // Arrange / Act: stamped AT the epoch, so the age is what the clock has run.
    await withFooter({ status: "working", activity: "hook", activityAtMs: 10_000n });
    // Assert
    expect(harness.text(".footer-activity-age")).toBe("· 0s ago");
  });

  it("grows into minutes as time passes", async () => {
    // Arrange
    await withFooter({ status: "working", activity: "hook", activityAtMs: 10_000n });
    // Act
    await harness.tick(120_000);
    // Assert
    expect(harness.text(".footer-activity-age")).toBe("· 2m ago");
  });

  it("truncates rather than rounding the second level", async () => {
    // Arrange
    await withFooter({ status: "working", activity: "hook", activityAtMs: 10_000n });
    // Act
    await harness.tick(130_000);
    // Assert
    expect(harness.text(".footer-activity-age")).toBe("· 2m 10s ago");
  });
});

/**
 * A JUMP INTO A SHELL ROW. A detached shell is a CARD, not a sub-feed, so the
 * jump degrades to scroll-if-rendered: the row is found on the root feed and
 * landed on, and no `OpenFeed` is issued for it.
 */
describe("a jump into a rendered shell row", () => {
  const withShellRow = async (): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => {
        fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([detachedShellRow("live", { id: feedId(FOOTER_SHELL_TARGET) })]),
        );
      },
    });
    await harness.click('.footer-chip[data-chip="shells"]');
  };

  it("lands on the row that is already drawn", async () => {
    // Arrange
    await withShellRow();
    // Act
    await harness.click(`[data-jump="${FOOTER_SHELL_TARGET}"]`);
    // Assert
    expect(harness.row(FOOTER_SHELL_TARGET)?.dataset.revealed).toBe("true");
  });

  it("asks the daemon nothing to get there", async () => {
    // Arrange
    await withShellRow();
    harness.fake.clearCalls();
    // Act
    await harness.click(`[data-jump="${FOOTER_SHELL_TARGET}"]`);
    // Assert: a drawn row is scrolled to, never re-opened.
    expect(harness.fake.calls("openFeed")).toHaveLength(0);
  });
});

/**
 * A JUMP INTO A MONITOR'S CARD. A monitor's entry is its Monitor call's
 * ordinary tool-call card, selected exactly as a shell's head is.
 */
describe("a jump into a monitor's tool-call card", () => {
  const withMonitorCard = async (): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => {
        fake.setFooter(WORKSPACE_ID, footerView({ status: "idle" }));
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(monitorCallUnit(), { id: feedId(FOOTER_MONITOR_TARGET) })]),
        );
      },
    });
    await harness.click('.footer-chip[data-chip="monitors"]');
  };

  it("lands on the monitor's card", async () => {
    // Arrange
    await withMonitorCard();
    // Act
    await harness.click(`[data-jump="${FOOTER_MONITOR_TARGET}"]`);
    // Assert
    expect(harness.row(FOOTER_MONITOR_TARGET)?.dataset.revealed).toBe("true");
  });

  it("rings the monitor's card with the selected-entry mark", async () => {
    // Arrange
    await withMonitorCard();
    // Act
    await harness.click(`[data-jump="${FOOTER_MONITOR_TARGET}"]`);
    // Assert
    expect(harness.row(FOOTER_MONITOR_TARGET)?.firstElementChild?.classList.contains("entry-selected")).toBe(
      true,
    );
  });

  it("draws no not-on-screen notice for a card it reached", async () => {
    // Arrange
    await withMonitorCard();
    // Act
    await harness.click(`[data-jump="${FOOTER_MONITOR_TARGET}"]`);
    // Assert
    expect(harness.$('.footer-expanded[data-panel="monitors"] .footer-row-unreachable')).toBeNull();
  });
});

describe("a jump whose target is not drawn", () => {
  it("leaves the feed as it was", async () => {
    // Arrange: the shells panel names a FeedId the root page never carried.
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="shells"]');
    // Act
    await harness.click(`[data-jump="${FOOTER_SHELL_TARGET}"]`);
    // Assert: the walk finds nothing, and the reader is left where they were.
    expect(harness.row(FOOTER_SHELL_TARGET)).toBeNull();
  });

  it("says so at the row rather than doing nothing", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="shells"]');
    // Act
    await harness.click(`[data-jump="${FOOTER_SHELL_TARGET}"]`);
    // Assert: the known entry could not be brought on screen, so the notice.
    expect(harness.text('.footer-expanded[data-panel="shells"] .footer-row-unreachable')).toBe(
      "not on screen",
    );
  });
});

describe("the usage line the strip cannot fit", () => {
  /**
   * The state a real page shows: both windows figured, the figures read a
   * moment ago. On the strip that is more line than the dock's one row can
   * hold, so the sheet carries the window the strip cuts. The figures were
   * READ at 1 s absolute, so at the harness epoch (10 s) they read "9s ago".
   */
  const FIGURED_RATE_LIMITED = {
    session: {
      newsworthy: true,
      utilization: 0.82,
      resetsAtS: 1_700n,
      status: { case: "allowedWarning", value: {} },
    },
    weekly: {
      newsworthy: false,
      utilization: 0.63,
      resetsAtS: 9_000n,
      status: { case: "allowed", value: {} },
    },
  };

  /** Boot with that line standing. */
  async function withFiguredRateLine(): Promise<void> {
    await withFooter({
      status: "idle",
      activity: FOOTER_ENDURING,
      activityOverride: { usage: FIGURED_RATE_LIMITED },
    });
  }

  it("draws no reading age on the enduring line", async () => {
    // Arrange / Act
    await withFiguredRateLine();
    // Assert
    expect(harness.$(".footer-strip .footer-activity-enduring [data-age]")).toBeNull();
  });

  it("draws no usage-unread cell any more", async () => {
    // Arrange / Act
    await withFiguredRateLine();
    // Assert
    expect(harness.$(".footer-strip .footer-allowance-unread")).toBeNull();
  });

  it("leads the strip's figures with the newsworthy window", async () => {
    // Arrange / Act
    await withFiguredRateLine();
    // Assert
    expect(harness.$(".footer-rate-figures [data-allowance]")?.dataset.allowance).toBe("session");
  });

  it("titles the activity cell with the whole line", async () => {
    // Arrange / Act
    await withFiguredRateLine();
    // Assert
    expect(harness.$(".footer-activity")?.title).toBe(
      harness.text(".footer-activity-enduring"),
    );
  });

  it("carries the weekly window into the tokens sheet", async () => {
    // Arrange
    await withFiguredRateLine();
    // Act
    await harness.click(".footer-tokens");
    // Assert
    expect(
      harness.text('.footer-expanded[data-panel="tokens"] [data-usage-allowance="weekly"]'),
    ).toContain("weekly 63%");
  });
});
