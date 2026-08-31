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
import { afterEach, describe, expect, it } from "vitest";

import {
  FooterAllowanceSchema,
  FooterStatusSchema,
  FooterTokensCellVerdictSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";

import { startHarness, type Harness } from "./harness";
import {
  assertVocabCoversArms,
  RENDER_COLORS,
  footerAllowanceColor,
  footerStatusColor,
} from "./vocab";
import {
  FOOTER_ACTIVITY_KINDS,
  FOOTER_ALLOWANCE_ARMS,
  FOOTER_CHIPS,
  FOOTER_PANELS,
  FOOTER_STATUS_ACTIVITIES,
  FOOTER_STATUS_ARMS,
  FOOTER_STATUS_SUBSTATUSES,
  FOOTER_STATUS_WITHOUT_SUBSTATUS,
  FOOTER_AGENT_TARGET,
  FOOTER_SHELL_TARGET,
  FOOTER_TOKENS_VERDICTS,
  WORKSPACE_ID,
  activityRow,
  feedId,
  feedPageSuccess,
  footerView,
  rateLimitedActivity,
  subagentUnit,
  assertCoversOneof,
} from "./fixtures";
import { ROOT_FEED } from "./fake-daemon";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one footer view already scripted. */
async function withFooter(init: Parameters<typeof footerView>[0]): Promise<Harness> {
  harness = await startHarness({ arrange: (fake) => fake.setFooter(WORKSPACE_ID, footerView(init)) });
  return harness;
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

  it("gives every allowance verdict a color in the vocabulary", () => {
    assertVocabCoversArms(RENDER_COLORS.footer_allowance, FOOTER_ALLOWANCE_ARMS, "footer_allowance");
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
    await withFooter({ status: "merging", substatus: "merge" });
    // Assert
    expect(harness.text(".footer-status")).toContain("merging");
  });

  it("reads a compound substatus arm as lowercase words separated by spaces", async () => {
    // Arrange / Act: FooterSubStatusCloseBlocked under `closing`.
    await withFooter({ status: "closing", substatus: "blocked" });
    // Assert
    expect(harness.$('[data-component="footer"]')?.textContent).toContain("close blocked");
  });

  it("reads the pre-prompt merging substatus as two words", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "prePrompt" });
    // Assert
    expect(harness.$('[data-component="footer"]')?.textContent).toContain("pre prompt");
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

  it("draws the queued substatus's served position and depth", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "queued" });
    // Assert
    const drawn = harness.$('[data-component="footer"]')?.textContent ?? "";
    expect(drawn).toContain("2");
    expect(drawn).toContain("5");
  });

  it("draws the parked substatus's composed line verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", substatus: "parked" });
    // Assert
    expect(harness.$('[data-component="footer"]')?.textContent).toContain("waiting on your answer");
  });
});

describe("the activity cell", () => {
  it.each(
    FOOTER_STATUS_ARMS.flatMap((status) =>
      FOOTER_STATUS_ACTIVITIES[status].map((activity) => ({ status, activity })),
    ),
  )("draws the $activity activity under $status", async ({ status, activity }) => {
    // Arrange / Act
    await withFooter({ status, activity });
    // Assert
    expect(harness.$(".footer-activity")?.dataset.arm).toBe(activity);
  });

  it("draws no activity cell when the view carries none", async () => {
    // Arrange / Act: activity is `optional` — absent means draw nothing.
    await withFooter({ status: "idle" });
    // Assert
    expect(harness.$(".footer-activity")).toBeNull();
  });

  it("draws the notification's composed text verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", activity: "notification" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("the agent addressed you");
  });

  it("draws the hook's own name verbatim", async () => {
    // Arrange / Act
    await withFooter({ status: "thinking", activity: "hook" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("PreToolUse");
  });

  it("colors the merging commit's sha as a typed datum", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", activity: "mergingCommit" });
    // Assert
    expect(harness.$(".footer-activity [data-datum='sha']")?.textContent).toBe("abc1234");
  });

  it("draws the merging commit's subject beside its sha", async () => {
    // Arrange / Act
    await withFooter({ status: "merging", activity: "mergingCommit" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("port the transport");
  });

  it("colors the retry attempt as a typed datum", async () => {
    // Arrange / Act
    await withFooter({ status: "thinking", activity: "retrying" });
    // Assert
    expect(harness.$(".footer-activity [data-datum='attempt']")?.textContent).toContain("3");
  });

  it.each(FOOTER_ALLOWANCE_ARMS)("paints the %s verdict the vocabulary's color", async (arm) => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({
            status: "idle",
            activity: "rateLimited",
            activityOverride: rateLimitedActivity(arm),
          }),
        ),
    });
    // Assert
    expect(harness.$(`.footer-activity [data-allowance][data-arm="${arm}"]`)?.className).toContain(
      `tone-${footerAllowanceColor(arm)}`,
    );
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
            activity: "rateLimited",
            activityOverride: rateLimitedActivity(arm),
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
            activity: "rateLimited",
            activityOverride: rateLimitedActivity(undefined),
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
            activity: "rateLimited",
            activityOverride: rateLimitedActivity(undefined),
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
            activity: "rateLimited",
            activityOverride: rateLimitedActivity(undefined),
          }),
        ),
    });
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });

  it("draws both the session and the weekly allowance", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", activity: "rateLimited" });
    // Assert
    expect(harness.$$(".footer-activity [data-allowance]")).toHaveLength(2);
  });

  it("marks the session allowance apart from the weekly one", async () => {
    // Arrange / Act
    await withFooter({ status: "idle", activity: "rateLimited" });
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

  it("covers every activity kind the fixtures declare", () => {
    // Assert: the union of the per-status tables is the whole kind vocabulary.
    const used = new Set(Object.values(FOOTER_STATUS_ACTIVITIES).flat());
    expect([...used].sort()).toEqual(Object.keys(FOOTER_ACTIVITY_KINDS).sort());
  });
});

describe("the clock", () => {
  it("ticks up from the served turn start", async () => {
    // Arrange
    await withFooter({ status: "thinking", turnStartedAtMs: 0n });
    const before = harness.text(".footer-clock");
    // Act
    await harness.tick(5_000);
    // Assert
    expect(harness.text(".footer-clock")).not.toBe(before);
  });

  it("draws nothing when no turn is live", async () => {
    // Arrange / Act: turn_started_at_ms is `optional`.
    harness = await startHarness({
      arrange: (fake) =>
        fake.setFooter(
          WORKSPACE_ID,
          footerView({ status: "idle", substatus: "ready", turnStartedAtMs: undefined }),
        ),
    });
    // Assert
    expect(harness.text(".footer-clock") || "").toBe("");
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
    // Assert: tokens plus the five chips.
    expect(FOOTER_PANELS).toHaveLength(6);
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

  it("makes no jump target of a monitor row", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="monitors"]');
    // Assert: monitors carry no FeedId — there is nothing to jump to.
    expect(harness.$('.footer-expanded[data-panel="monitors"] [data-jump]')).toBeNull();
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
    await withFooter({ status: "thinking" });
    // Act
    harness.fake.setFooter(WORKSPACE_ID, footerView({ status: "idle", substatus: "done" }));
    await harness.settle();
    // Assert
    expect(harness.$$(".footer-status")).toHaveLength(1);
  });
});
