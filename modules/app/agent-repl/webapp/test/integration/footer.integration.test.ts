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
import { create } from "@bufbuild/protobuf";

import {
  InterruptResponseSchema,
  InterruptSuccessSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import {
  FooterAllowanceSchema,
  FooterStatusSchema,
  FooterTokensCellVerdictSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { panelStorageKey } from "../../src/footer/footer";
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
  detachedShellRow,
  feedId,
  feedPageSuccess,
  footerView,
  rateLimitedActivity,
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

  // THE SENTENCE, NOT MERELY THE ARM. The table above pins `data-arm` for every
  // status/activity pair, which a build that drew an empty cell would still
  // satisfy. This is the dead-query line specifically, and it is drawn under
  // `vendorError` on purpose: the death is a SESSION fact and the daemon draws
  // it under whichever blocked step is standing, so the client must not tie the
  // sentence to the query_died STEP either.
  it("draws the dead-query sentence under a blocked step that is not query_died", async () => {
    // Arrange / Act
    await withFooter({ status: "blocked", substatus: "vendorError", activity: "queryDied" });
    // Assert
    expect(harness.text(".footer-activity")).toContain("the vendor query died");
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

  it("makes a monitor row a jump row that names no FeedId", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    // Act
    await harness.click('.footer-chip[data-chip="monitors"]');
    // Assert: a monitor draws no feed entry, so the daemon states why.
    expect(harness.$('.footer-expanded[data-panel="monitors"] [data-jump]')).toBeNull();
    expect(
      harness.$('.footer-expanded[data-panel="monitors"] [data-jump-unresolved="noFeedEntry"]'),
    ).not.toBeNull();
  });

  it("says not on screen when a monitor row is clicked", async () => {
    // Arrange
    await withFooter({ status: "idle" });
    await harness.click('.footer-chip[data-chip="monitors"]');
    // Act
    await harness.click('.footer-expanded[data-panel="monitors"] [data-jump-unresolved]');
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
    await withFooter({ status: "thinking" });
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
    await withFooter({ status: "thinking" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    const [request] = harness.fake.calls<{ target: { case?: string } }>("interrupt");
    expect(request.target.case).toBe("turn");
  });

  it("echoes the page's own workspace", async () => {
    // Arrange
    await withFooter({ status: "thinking" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("interrupt");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("draws the interrupted-turn outcome as a note, not a refusal", async () => {
    // Arrange
    await withFooter({ status: "thinking" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    expect(harness.$('.footer-stop-turn [data-stop-outcome="interruptedTurn"]')).not.toBeNull();
  });

  it("draws no refusal for an interrupted turn", async () => {
    // Arrange
    await withFooter({ status: "thinking" });
    // Act
    await harness.click(".footer-clock [data-interrupt]");
    // Assert
    expect(harness.refusalArms()).toEqual([]);
  });

  it("draws the nothing-running outcome as a note", async () => {
    // Arrange: a domain outcome, not an error — the stop found the session
    // already quiet, which is a legitimate reply to a legitimate ask.
    await withFooter({ status: "thinking" });
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
    await withFooter({ status: "thinking" });
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
    await withFooter({ status: "thinking" });
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
    await withFooter({ status: "thinking", activity: "hook", activityAtMs: 10_000n });
    // Assert
    expect(harness.text(".footer-activity-age")).toBe("· 0s ago");
  });

  it("grows into minutes as time passes", async () => {
    // Arrange
    await withFooter({ status: "thinking", activity: "hook", activityAtMs: 10_000n });
    // Act
    await harness.tick(120_000);
    // Assert
    expect(harness.text(".footer-activity-age")).toBe("· 2m ago");
  });

  it("truncates rather than rounding the second level", async () => {
    // Arrange
    await withFooter({ status: "thinking", activity: "hook", activityAtMs: 10_000n });
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
    figuresReadAtMs: 1_000n,
  };

  /** Boot with that line standing. */
  async function withFiguredRateLine(): Promise<void> {
    await withFooter({
      status: "idle",
      activity: "rateLimited",
      activityOverride: FIGURED_RATE_LIMITED,
    });
  }

  it("renders the age of the last usage reading on the strip", async () => {
    // Arrange / Act
    await withFiguredRateLine();
    // Assert: read at 1 s, drawn at the 10 s epoch, so nine seconds ago.
    // (harness.text trims the leading separator space.)
    expect(harness.text(".footer-strip .footer-rate-age")).toBe("· 9s ago");
  });

  it("ticks the usage read-age on the shared clock", async () => {
    // Arrange
    await withFiguredRateLine();
    // Act
    await harness.tick(1_000);
    // Assert
    expect(harness.text(".footer-strip .footer-rate-age")).toBe("· 10s ago");
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
      harness.text(".footer-activity-rate-limited"),
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
  it("carries the context-budget sentence into the tokens sheet in full", async () => {
    // Arrange
    await withFooter({
      status: "idle",
      activity: "contextBudget",
      activityOverride: { text: "84% of the window" },
    });
    // Act
    await harness.click(".footer-tokens");
    // Assert
    expect(
      harness.text('.footer-expanded[data-panel="tokens"] [data-usage="context-budget"]'),
    ).toBe("84% of the window");
  });
});
