// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FooterAllowanceSchema,
  FooterStatusSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  FOOTER_RATE_SEPARATOR_GAP,
  drawFooterRateDivider,
  drawFooterStatusActivity,
  enduringUsage,
  orderedAllowances,
  salientKind,
  type FooterActivity,
} from "../../src/footer/activity.js";
import { footerStatusActivity } from "../../src/footer/strip.js";
import ACTIVITY_SOURCE from "../../src/footer/activity.ts?raw";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { FOOTER_STATUS_CASES, activityDatumClass } from "../../src/footer/tones.js";
import { coldGatePercentColor, pressurePercentColor } from "../../src/pressure-color.js";

/**
 * Every `FooterAllowance.status` arm, read off the generated schema.
 *
 * Read directly here rather than through `src/footer/tones.ts`: the colour
 * chain that once lived there (`ALLOWANCE_ARM_CLASS`) was removed as dead
 * code once the salient rate-limit line stopped painting an allowance
 * colour, but this suite still needs the arm set itself to parametrize its
 * assertions about `drawFooterAllowance`.
 */
const FOOTER_ALLOWANCE_STATUS_CASES: readonly string[] = (
  FooterAllowanceSchema.oneofs.find((oneof) => oneof.name === "status")
    ?.fields ?? []
).map((field) => field.localName);

/** The color a footer percent of PERCENT is painted, as the DOM reports it. */
function paintedAs(percent: number): string {
  const probe = document.createElement("span");
  probe.style.color = pressurePercentColor(percent);
  return probe.style.color;
}
import { harness } from "./harness.js";
import { enduringLine } from "./enduring-line.js";

/** Every test's clock reads from here, so a countdown's arithmetic is exact. */
const NOW = 1_800_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
});
afterEach(() => {
  vi.useRealTimers();
});

// ---- fixtures ---------------------------------------------------------------

/** Every status arm whose cell has an unpinned branch (all but waiting). */
const UNPINNED_ARMS: readonly string[] = FOOTER_STATUS_CASES.filter(
  (arm) => arm !== "waiting",
);

/**
 * The activity cell of STATUSCASE carrying INIT, built through the status arm
 * so the per-arm container is the generated one. The one cast: the arm is
 * named at run time, so there is no static type to build it under.
 */
function activityOf(
  statusCase: string,
  init: Record<string, unknown>,
): FooterActivity {
  const status = create(FooterStatusSchema, {
    status: { case: statusCase, value: { activity: init } },
  } as never);
  return footerStatusActivity(status);
}

/** A salient line of KINDCASE, in the shape STATUSCASE's cell holds it. */
function salientInit(
  statusCase: string,
  kindCase: string,
  value: Record<string, unknown>,
  atMs: bigint = BigInt(NOW),
): Record<string, unknown> {
  const salient = { at: { atMs }, kind: { case: kindCase, value } };
  return statusCase === "waiting"
    ? { salient }
    : { tier: { case: "salient", value: salient } };
}

/** What a test varies about a transient. */
interface TransientInit {
  atMs?: bigint;
  expiresAtMs?: bigint;
  agent?: string;
}

/** One transient of KINDCASE, live for ten seconds from NOW unless told otherwise. */
function transientInit(
  kindCase: string,
  value: Record<string, unknown>,
  init: TransientInit = {},
): Record<string, unknown> {
  return {
    at: { atMs: init.atMs ?? BigInt(NOW) },
    expiry: { expiresAtMs: init.expiresAtMs ?? BigInt(NOW + 10_000) },
    ...(init.agent === undefined ? {} : { agent: { label: init.agent } }),
    kind: { case: kindCase, value },
  };
}

/** An unpinned cell: TRANSIENT (if any) over ENDURING (empty unless given). */
function unpinnedInit(
  transient?: Record<string, unknown>,
  enduring: Record<string, unknown> = {},
): Record<string, unknown> {
  return {
    tier: {
      case: "unpinned",
      value: {
        ...(transient === undefined ? {} : { transient }),
        enduring: enduringLine(enduring),
      },
    },
  };
}

interface Drawn {
  cell: HTMLElement;
  /** Every instant the cell asked the mount to re-render at. */
  scheduled: number[];
}

/** Draw ACTIVITY's cell, recording the expiry re-renders it asks for. */
function draw(activity: FooterActivity): Drawn {
  const scheduled: number[] = [];
  const h = harness();
  const cell = drawFooterStatusActivity(
    activity,
    { ctx: h.ctx, expiry: { schedule: (atMs) => scheduled.push(atMs) } },
    "FooterStatus.activity",
  );
  return { cell, scheduled };
}

/** Draw STATUSCASE's cell carrying INIT. */
function drawCell(statusCase: string, init: Record<string, unknown>): Drawn {
  return draw(activityOf(statusCase, init));
}

/** The idle cell's salient line of KINDCASE. */
function salientCell(
  kindCase: string,
  value: Record<string, unknown>,
  statusCase = "idle",
): HTMLElement {
  return drawCell(statusCase, salientInit(statusCase, kindCase, value)).cell;
}

/** The idle cell drawing a live transient of KINDCASE. */
function transientCell(
  kindCase: string,
  value: Record<string, unknown>,
  init: TransientInit = {},
): HTMLElement {
  return drawCell("idle", unpinnedInit(transientInit(kindCase, value, init)))
    .cell;
}

/** The idle cell drawing the enduring line ENDURING. */
function enduringCell(enduring: Record<string, unknown>): HTMLElement {
  return drawCell("idle", unpinnedInit(undefined, enduring)).cell;
}

/** An allowance resetting RESETINMS from NOW. */
function allowance(
  utilization: number,
  newsworthy: boolean,
  resetInMs: number,
  arm?: string,
): Record<string, unknown> {
  return {
    newsworthy,
    utilization,
    resetsAtS: BigInt((NOW + resetInMs) / 1000),
    ...(arm === undefined ? {} : { status: { case: arm, value: {} } }),
  };
}

// ---- the tier walk ------------------------------------------------------------

describe("drawFooterStatusActivity: the tier the daemon resolved", () => {
  it("draws a salient line as the salient tier", () => {
    const cell = salientCell("queryDied", {
      text: "the next prompt restarts it",
    });
    expect(cell.getAttribute("data-tier")).toBe("salient");
  });

  it("names the salient kind on the cell", () => {
    const cell = salientCell("queryDied", { text: "x" });
    expect(cell.getAttribute("data-arm")).toBe("queryDied");
  });

  it("reads the waiting cell, which has no tier oneof, as salient", () => {
    const cell = salientCell(
      "gatedCall",
      { text: "Bash: rm -rf …" },
      "waiting",
    );
    expect(cell.getAttribute("data-tier")).toBe("salient");
  });

  it("draws a live transient as the transient tier", () => {
    const cell = transientCell("hook", { name: "done." });
    expect(cell.getAttribute("data-tier")).toBe("transient");
  });

  it("names the transient kind on the cell", () => {
    const cell = transientCell("hook", { name: "done." });
    expect(cell.getAttribute("data-arm")).toBe("hook");
  });

  it("draws the enduring line when no transient was raised", () => {
    const cell = enduringCell({});
    expect(cell.getAttribute("data-tier")).toBe("enduring");
  });

  it("names the enduring line on the cell", () => {
    expect(enduringCell({}).getAttribute("data-arm")).toBe("enduring");
  });

  it("refuses a cell whose tier oneof sets no arm", () => {
    expect(() => drawCell("idle", {})).toThrow(MalformedView);
  });

  it("refuses a waiting cell with no salient line", () => {
    expect(() => drawCell("waiting", {})).toThrow(MalformedView);
  });

  it("refuses a salient line whose kind sets no arm", () => {
    expect(() =>
      drawCell("idle", {
        tier: { case: "salient", value: { at: { atMs: BigInt(NOW) } } },
      }),
    ).toThrow(MalformedView);
  });

  it("refuses a salient line with no standing instant", () => {
    expect(() =>
      drawCell("idle", {
        tier: {
          case: "salient",
          value: { kind: { case: "queryDied", value: { text: "x" } } },
        },
      }),
    ).toThrow(MalformedView);
  });

  it("refuses an unpinned pair with no enduring line", () => {
    expect(() =>
      drawCell("idle", { tier: { case: "unpinned", value: {} } }),
    ).toThrow(MalformedView);
  });

  it("refuses a salient KIND the bundle cannot name", () => {
    // ARRANGE — a legal cell, then the arm a NEWER daemon set, poked in: the
    // generated `create` drops a case its descriptors do not know.
    const activity = activityOf(
      "idle",
      salientInit("idle", "queryDied", { text: "x" }),
    );
    const tier = (activity as unknown as { tier: { value: { kind: unknown } } })
      .tier;
    tier.value.kind = { case: "teleporting", value: {} };
    // ACT / ASSERT
    expect(() => draw(activity)).toThrow(MalformedView);
  });

  it("refuses a TIER the bundle cannot name", () => {
    const activity = activityOf("idle", unpinnedInit());
    (activity as unknown as { tier: unknown }).tier = {
      case: "pinned",
      value: {},
    };
    expect(() => draw(activity)).toThrow(MalformedView);
  });

  it("titles the cell with the whole drawn line", () => {
    const cell = transientCell(
      "hook",
      { name: "a long line the cell may cut" },
      { agent: "Explore" },
    );
    expect(cell.title).toBe("Explore · a long line the cell may cut");
  });
});

// ---- the one clock decision ---------------------------------------------------

describe("the transient's expiry: the client's one clock decision", () => {
  it("draws the transient while the clock is before its expiry", () => {
    const cell = transientCell(
      "hook",
      { name: "still here" },
      { expiresAtMs: BigInt(NOW + 1) },
    );
    expect(cell.querySelector(".footer-activity-hook")?.textContent).toBe(
      "still here",
    );
  });

  it("draws the enduring line once the clock reaches the expiry", () => {
    const cell = transientCell(
      "hook",
      { name: "gone" },
      { expiresAtMs: BigInt(NOW) },
    );
    expect(cell.getAttribute("data-tier")).toBe("enduring");
  });

  it("draws nothing of a lapsed transient", () => {
    const cell = transientCell(
      "hook",
      { name: "gone" },
      { expiresAtMs: BigInt(NOW - 1) },
    );
    expect(cell.textContent).not.toContain("gone");
  });

  it("schedules exactly one re-render at a drawn transient's expiry", () => {
    const { scheduled } = drawCell(
      "idle",
      unpinnedInit(
        transientInit(
          "hook",
          { name: "x" },
          { expiresAtMs: BigInt(NOW + 7_000) },
        ),
      ),
    );
    expect(scheduled).toEqual([NOW + 7_000]);
  });

  it("schedules nothing for a lapsed transient", () => {
    const { scheduled } = drawCell(
      "idle",
      unpinnedInit(
        transientInit("hook", { name: "x" }, { expiresAtMs: BigInt(NOW - 1) }),
      ),
    );
    expect(scheduled).toEqual([]);
  });

  it("schedules nothing for the enduring line alone", () => {
    expect(drawCell("idle", unpinnedInit()).scheduled).toEqual([]);
  });

  it("schedules nothing for a salient line", () => {
    expect(
      drawCell("idle", salientInit("idle", "queryDied", { text: "x" }))
        .scheduled,
    ).toEqual([]);
  });

  it("refuses a lapsed transient whose kind sets no arm, drawn or not", () => {
    const transient = transientInit(
      "hook",
      { name: "x" },
      { expiresAtMs: BigInt(NOW - 1) },
    );
    delete transient.kind;
    expect(() => drawCell("idle", unpinnedInit(transient))).toThrow(
      MalformedView,
    );
  });

  it("refuses a transient with no expiry", () => {
    const transient = transientInit("hook", { name: "x" });
    delete transient.expiry;
    expect(() => drawCell("idle", unpinnedInit(transient))).toThrow(
      MalformedView,
    );
  });

  it("refuses a transient with no event instant", () => {
    const transient = transientInit("hook", { name: "x" });
    delete transient.at;
    expect(() => drawCell("idle", unpinnedInit(transient))).toThrow(
      MalformedView,
    );
  });

  it.each(UNPINNED_ARMS.map((arm) => [arm]))(
    "draws a transient under the %s arm",
    (arm) => {
      const { cell } = drawCell(
        arm,
        unpinnedInit(transientInit("hook", { name: "fmt" })),
      );
      expect(cell.querySelector(".footer-activity-hook")?.textContent).toBe(
        "fmt",
      );
    },
  );
});

// ---- the salient tier -----------------------------------------------------------

describe("the unpinned tiers under working and background", () => {
  // THE QUIET TIER IS RETIRED (owner ruling, 2026-10-01): these statuses carry
  // the same transient-over-enduring cell every other status does.
  it.each(["working", "background"])(
    "draws a live transient under %s",
    (statusCase) => {
      const { cell } = drawCell(
        statusCase,
        unpinnedInit(transientInit("hook", { name: "fmt" })),
      );
      expect(cell.getAttribute("data-tier")).toBe("transient");
    },
  );

  it.each(["working", "background"])(
    "draws the enduring line once the transient under %s lapses",
    (statusCase) => {
      const { cell } = drawCell(
        statusCase,
        unpinnedInit(
          transientInit(
            "hook",
            { name: "fmt" },
            { expiresAtMs: BigInt(NOW) },
          ),
        ),
      );
      expect(cell.getAttribute("data-tier")).toBe("enduring");
    },
  );

  it.each(["working", "background"])(
    "draws the enduring line under %s with no transient",
    (statusCase) => {
      const { cell } = drawCell(statusCase, unpinnedInit());
      expect(cell.getAttribute("data-tier")).toBe("enduring");
    },
  );
});

describe("the salient kinds", () => {
  it.each([
    ["waiting", "gatedCall", { text: "Bash: rm -rf …" }, "Bash: rm -rf …"],
    [
      "waiting",
      "questionLead",
      { text: "2 questions · which?" },
      "2 questions · which?",
    ],
    [
      "waiting",
      "blockedOnUser",
      { detail: "requires action" },
      "requires action",
    ],
    [
      "waiting",
      "coldGateCost",
      { text: "cold at 182k tokens", lead: "cold at ", figure: { text: "182k", windowFill: 0.2 }, tail: " tokens" },
      "cold at 182k tokens",
    ],
    [
      "waiting",
      "interrupting",
      { text: "stopping the turn…" },
      "stopping the turn…",
    ],
    [
      "working",
      "compaction",
      { text: "compacting · 412 of 900" },
      "compacting · 412 of 900",
    ],
    ["vendorFault", "authenticating", { line: "open the login" }, "open the login"],
    // A dead query is a FAILED TURN (owner ruling, 2026-09-28): its line
    // stands in the idle cell the turn_failed arm shares.
    [
      "turnFailed",
      "queryDied",
      { text: "the next prompt restarts it" },
      "the next prompt restarts it",
    ],
    [
      "closing",
      "closeBlocked",
      { text: "a turn is in flight" },
      "a turn is in flight",
    ],
    [
      "agentReplFault",
      "fault",
      { kind: "resume_failed", detail: "the shim refused" },
      "resume failed · the shim refused",
    ],
    [
      "agentReplFault",
      "fault",
      { kind: "prompts_dir_missing", detail: "no prompts" },
      "prompts dir missing · no prompts",
    ],
    [
      "networkFault",
      "offline",
      { text: "cannot reach api.anthropic.com: no route to host" },
      "cannot reach api.anthropic.com: no route to host",
    ],
    [
      "idle",
      "notification",
      { text: "the agent addressed you" },
      "the agent addressed you",
    ],
    [
      "working",
      "contextBudget",
      { text: "compaction failed — the summary was empty" },
      "compaction failed — the summary was empty",
    ],
  ])("draws the %s cell's %s line", (statusCase, kindCase, value, expected) => {
    expect(salientCell(kindCase, value, statusCase).textContent).toContain(
      expected,
    );
  });

  it("draws the bring-up failure's cause verbatim", () => {
    const cell = salientCell(
      "startFailed",
      { detail: "exit 1: no module" },
      "agentReplFault",
    );
    expect(
      cell.querySelector(".footer-activity-start-failed")?.textContent,
    ).toBe("exit 1: no module");
  });

  it.each([
    "Claude SDK did not start (attempt 3): timed out · retrying",
    "Claude SDK refused to start: auth rejected · restart: SPC o C-c",
    "Claude SDK failed to start · restart: SPC o C-c",
  ])("draws the daemon's vendor-start line verbatim: %s", (text) => {
    const cell = salientCell("vendorStart", { text }, "vendorFault");
    expect(
      cell.querySelector(".footer-activity-vendor-start")?.textContent,
    ).toBe(text);
  });

  it.each([
    ["vendorFault", "the vendor API is overloaded"],
    ["agentReplFault", "the agent process died, and the turn it was running ended with it"],
  ])("draws a %s turn fault's per-cause sentence verbatim", (status, text) => {
    const cell = salientCell("turnEnded", { text }, status);
    expect(cell.querySelector(".footer-activity-turn-ended")?.textContent).toBe(text);
  });

  it("counts a turn fault's stated wait down beside its sentence", () => {
    const cell = salientCell(
      "turnEnded",
      { text: "rate limited by the vendor", retryAt: { atMs: BigInt(NOW + 42_000) } },
      "vendorFault",
    );
    expect(cell.querySelector(".footer-activity-turn-ended")?.textContent).toBe(
      "rate limited by the vendor · retry in 42s",
    );
  });

  it("says a turn fault's wait is over once it has passed", () => {
    const cell = salientCell(
      "turnEnded",
      { text: "rate limited by the vendor", retryAt: { atMs: BigInt(NOW - 1_000) } },
      "vendorFault",
    );
    expect(cell.querySelector(".footer-turn-retry")?.textContent).toBe("ready to retry");
  });

  it("draws no countdown for a turn fault that stated no wait", () => {
    const cell = salientCell("turnEnded", { text: "stopped at the turn limit" }, "vendorFault");
    expect(cell.querySelector(".footer-turn-retry")).toBeNull();
  });

  it("colours the retry ATTEMPT as its own datum", () => {
    const cell = salientCell(
      "retrying",
      { attempt: 2, status: "overloaded" },
      "working",
    );
    expect(cell.querySelector('[data-datum="attempt"]')?.textContent).toBe(
      "#2",
    );
  });

  it("draws the retry's status verbatim beside the attempt", () => {
    const cell = salientCell(
      "retrying",
      { attempt: 2, status: "overloaded" },
      "working",
    );
    expect(cell.querySelector(".footer-activity-retrying")?.textContent).toBe(
      "retry #2 · overloaded",
    );
  });

  it("draws the vendor's attempt limit after the attempt", () => {
    const cell = salientCell(
      "retrying",
      { attempt: 9, status: "overloaded", maxAttempt: 11 },
      "working",
    );
    expect(cell.querySelector(".footer-activity-retrying")?.textContent).toBe(
      "retry #9 of 11 · overloaded",
    );
  });

  it("counts down to the vendor's next attempt at second resolution", () => {
    const cell = salientCell(
      "retrying",
      {
        attempt: 9,
        status: "overloaded",
        nextAttempt: { atMs: BigInt(NOW + 12_000) },
      },
      "working",
    );
    expect(cell.querySelector(".footer-activity-retrying")?.textContent).toBe(
      "retry #9 · next try in 12s · overloaded",
    );
  });

  it("re-reads the next-attempt countdown on the shared tick", () => {
    const cell = salientCell(
      "retrying",
      {
        attempt: 9,
        status: "overloaded",
        nextAttempt: { atMs: BigInt(NOW + 12_000) },
      },
      "working",
    );
    vi.advanceTimersByTime(1000);
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(
      "next try in 11s",
    );
  });

  it("says a next attempt that has passed is overdue, counting up", () => {
    const cell = salientCell(
      "retrying",
      {
        attempt: 9,
        status: "overloaded",
        nextAttempt: { atMs: BigInt(NOW - 120_000) },
      },
      "working",
    );
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(
      "next try overdue by 2m",
    );
  });

  it("turns the countdown overdue when the promised instant passes", () => {
    const cell = salientCell(
      "retrying",
      {
        attempt: 9,
        status: "overloaded",
        nextAttempt: { atMs: BigInt(NOW + 1000) },
      },
      "working",
    );
    vi.advanceTimersByTime(3000);
    const countdown = cell.querySelector("[data-countdown]");
    expect(countdown?.textContent).toBe("next try overdue by 2s");
    expect(countdown?.hasAttribute("data-overdue")).toBe(true);
  });

  it("does not mark a pending next attempt overdue", () => {
    const cell = salientCell(
      "retrying",
      {
        attempt: 9,
        status: "overloaded",
        nextAttempt: { atMs: BigInt(NOW + 1000) },
      },
      "working",
    );
    expect(
      cell.querySelector("[data-countdown]")?.hasAttribute("data-overdue"),
    ).toBe(false);
  });

  it("draws a merge's step line as the salient line under merging", () => {
    const cell = salientCell(
      "mergeStep",
      {
        step: {
          case: "committing",
          value: { subject: "Merge branch 'fix-reconnect'" },
        },
      },
      "merging",
    );
    expect(cell.querySelector(".footer-activity-merge-step")?.textContent).toBe(
      "Merge branch 'fix-reconnect'",
    );
  });

  it("names the merge step line's arm on the cell", () => {
    const cell = salientCell(
      "mergeStep",
      {
        step: {
          case: "committing",
          value: { subject: "Merge branch 'fix-reconnect'" },
        },
      },
      "merging",
    );
    expect(cell.getAttribute("data-arm")).toBe("mergeStep");
  });

  it("counts a wakeup down at second resolution", () => {
    const cell = salientCell(
      "wakeup",
      { wakeAtMs: BigInt(NOW + 252_000) },
      "waiting",
    );
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(
      "wakes in 4m 12s",
    );
  });

  it("re-reads the wakeup countdown on the shared tick", () => {
    const cell = salientCell(
      "wakeup",
      { wakeAtMs: BigInt(NOW + 252_000) },
      "waiting",
    );
    vi.advanceTimersByTime(1000);
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(
      "wakes in 4m 11s",
    );
  });

  it("draws the wakeup's reason when the agent gave one", () => {
    const cell = salientCell(
      "wakeup",
      { wakeAtMs: BigInt(NOW + 60_000), reason: { text: "check the deploy" } },
      "waiting",
    );
    expect(
      cell.querySelector(".footer-activity-wakeup")?.textContent,
    ).toContain("check the deploy");
  });

  it("floors a wakeup whose deadline has passed rather than counting backwards", () => {
    const cell = salientCell(
      "wakeup",
      { wakeAtMs: BigInt(NOW - 5000) },
      "waiting",
    );
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(
      "wakes in 0s",
    );
  });

  it("ticks the salient line's relative age", () => {
    const { cell } = drawCell(
      "idle",
      salientInit("idle", "queryDied", { text: "x" }, BigInt(NOW - 120_000)),
    );
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 2m ago");
  });

  it("reads the nearest second when a tick samples just short of one", () => {
    const { cell } = drawCell(
      "idle",
      salientInit("idle", "queryDied", { text: "x" }, BigInt(NOW - 4920)),
    );
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 5s ago");
  });

  it("re-reads the age on the shared tick", () => {
    const { cell } = drawCell(
      "idle",
      salientInit("idle", "queryDied", { text: "x" }, BigInt(NOW - 120_000)),
    );
    vi.advanceTimersByTime(60_000);
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 3m ago");
  });
});

describe("the fault line", () => {
  it("draws the kind lowercase with spaces, never underscores", () => {
    const cell = salientCell(
      "fault",
      { kind: "watch_open_refused", detail: "handle 7" },
      "agentReplFault",
    );
    expect(cell.textContent).not.toContain("_");
  });

  it("draws the kind alone when the fault carries no detail", () => {
    const cell = salientCell(
      "fault",
      { kind: "session_absent", detail: "" },
      "agentReplFault",
    );
    expect(cell.querySelector(".footer-activity-fault")?.textContent).toBe(
      "session absent",
    );
  });

  // A NON-ESCALATING fault is a TRANSIENT (footer.proto, THE FAULT
  // PARTITION): the session is serving, so it is announced, never pinned.
  it("draws a non-escalating fault as a transient", () => {
    const cell = transientCell("fault", {
      kind: "deploy_failed",
      detail: "build webapp: error TS2322",
    });
    expect(cell.querySelector(".footer-activity-fault")?.textContent).toBe(
      "deploy failed · build webapp: error TS2322",
    );
  });

  it.each([
    ["the turn named no answering response"],
    ["the named answer has no drawn row"],
  ])("draws the unresolved final answer with its detail %s", (detail) => {
    const cell = transientCell("fault", {
      kind: "final_answer_unresolved",
      detail,
    });
    expect(cell.querySelector(".footer-activity-fault")?.textContent).toBe(
      `final answer unresolved · ${detail}`,
    );
  });
});

describe("the update line: a deploy's progress", () => {
  const updateText = (
    update: Record<string, unknown>,
  ): string | null | undefined =>
    salientCell("update", update).querySelector(".footer-activity-update")
      ?.textContent;

  it("draws the building phase with the components it builds", () => {
    expect(
      updateText({
        phase: {
          case: "building",
          value: {
            components: [
              { component: { case: "shim", value: {} } },
              { component: { case: "webapp", value: {} } },
            ],
          },
        },
      }),
    ).toBe("building · shim, webapp");
  });

  it("draws a phase with no payload as its arm name alone", () => {
    expect(updateText({ phase: { case: "handingOver", value: {} } })).toBe(
      "handing over",
    );
  });

  it("draws the services being restarted", () => {
    expect(
      updateText({
        phase: {
          case: "restartingServices",
          value: {
            services: [
              { component: { case: "store", value: {} } },
              { component: { case: "sidecar", value: {} } },
            ],
          },
        },
      }),
    ).toBe("restarting services · store, sidecar");
  });

  it("draws what a waiting workspace's move waits on", () => {
    expect(
      updateText({
        phase: { case: "waiting", value: { turns: 1, background: 2 } },
      }),
    ).toBe("waiting · 1 turn, 2 background");
  });

  it("colours the waiting counts as figures", () => {
    const cell = salientCell("update", {
      phase: { case: "waiting", value: { turns: 0, background: 3 } },
    });
    const figures = cell.querySelectorAll(
      ".footer-activity-update [data-datum='count']",
    );
    expect([...figures].map((f) => f.textContent)).toEqual(["3"]);
  });

  it("draws the notes after the phase", () => {
    expect(
      updateText({
        phase: { case: "installing", value: {} },
        notes: [{ note: { case: "shimWhenIdle", value: {} } }],
      }),
    ).toBe("installing · shim when idle");
  });

  it("stamps the phase arm on the line", () => {
    const cell = salientCell("update", {
      phase: { case: "installing", value: {} },
    });
    expect(
      cell.querySelector(".footer-activity-update")?.getAttribute("data-phase"),
    ).toBe("installing");
  });

  it("refuses an update line whose phase is unset", () => {
    expect(() => salientCell("update", {})).toThrow(MalformedView);
  });

  it("refuses a component whose arm is unset", () => {
    expect(() =>
      salientCell("update", {
        phase: { case: "building", value: { components: [{}] } },
      }),
    ).toThrow(MalformedView);
  });

  it("refuses a note whose arm is unset", () => {
    expect(() =>
      salientCell("update", {
        phase: { case: "installing", value: {} },
        notes: [{}],
      }),
    ).toThrow(MalformedView);
  });

  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))(
    "stands under the %s arm",
    (arm) => {
      const cell = salientCell(
        "update",
        { phase: { case: "installing", value: {} } },
        arm,
      );
      expect(cell.querySelector(".footer-activity-update")?.textContent).toBe(
        "installing",
      );
    },
  );
});

// ---- the transient tier ---------------------------------------------------------

describe("the transient kinds", () => {
  it.each([
    [
      "task",
      { subject: "port the footer", completed: 3, total: 7 },
      ".footer-activity-task",
      "port the footer · 3/7",
    ],
    [
      "hook",
      { name: "protect-master" },
      ".footer-activity-hook",
      "protect-master",
    ],
    [
      "contextInjected",
      { text: "webapp/CLAUDE.md" },
      ".footer-activity-context-injected",
      "webapp/CLAUDE.md",
    ],
    [
      "daemonWarning",
      { operation: "feed.row-order-changed", message: "kept in place" },
      ".footer-activity-daemon-warning",
      "feed.row-order-changed · kept in place",
    ],
    [
      "daemonError",
      { operation: "store.write", message: "disk full" },
      ".footer-activity-daemon-error",
      "store.write · disk full",
    ],
    [
      "sessionChange",
      { text: "model → opus" },
      ".footer-activity-session-change",
      "model → opus",
    ],
    ["updated", {}, ".footer-activity-update", "updated"],
    [
      "updated",
      { notes: [{ note: { case: "shimWhenIdle", value: {} } }] },
      ".footer-activity-update",
      "updated · shim when idle",
    ],
    [
      "compactionConcluded",
      { text: "compacted and resumed (101.6k → 12.4k)" },
      ".footer-activity-compaction",
      "compacted and resumed (101.6k → 12.4k)",
    ],
    [
      "apiRestored",
      { failedAttempts: 8 },
      ".footer-activity-api-restored",
      "API answering again after 8 failed attempts",
    ],
    [
      "apiRestored",
      { failedAttempts: 1 },
      ".footer-activity-api-restored",
      "API answering again after 1 failed attempt",
    ],
    [
      "networkResume",
      { edge: { case: "resumed", value: {} } },
      ".footer-activity-network-resume",
      "network resume · resumed",
    ],
    [
      "networkResume",
      { edge: { case: "gaveUp", value: {} } },
      ".footer-activity-network-resume",
      "network resume · gave up",
    ],
    [
      "networkResume",
      { edge: { case: "abandoned", value: { reason: "the shim stood down" } } },
      ".footer-activity-network-resume",
      "network resume · abandoned · the shim stood down",
    ],
  ])("draws the %s transient %j", (kindCase, value, selector, expected) => {
    expect(
      transientCell(kindCase, value).querySelector(selector)?.textContent,
    ).toBe(expected);
  });

  it("names a concluded compaction's kind on the cell", () => {
    const cell = transientCell("compactionConcluded", { text: "compacted" });
    expect(cell.getAttribute("data-arm")).toBe("compactionConcluded");
  });

  it("colours the task tracker's progress as a figure", () => {
    const cell = transientCell("task", {
      subject: "port",
      completed: 3,
      total: 7,
    });
    expect(cell.querySelector('[data-datum="count"]')?.className).toBe(
      activityDatumClass("count"),
    );
  });

  it("stamps the finished deploy's line with the updated phase", () => {
    const cell = transientCell("updated", {});
    expect(
      cell.querySelector(".footer-activity-update")?.getAttribute("data-phase"),
    ).toBe("updated");
  });

  it("draws a wait's opening edge with its give-up countdown", () => {
    const cell = transientCell("networkResume", {
      edge: {
        case: "waiting",
        value: { givesUpAtMs: BigInt(NOW + 30 * 60_000) },
      },
    });
    expect(
      cell.querySelector(".footer-activity-network-resume")?.textContent,
    ).toBe("network resume · waiting · gives up in 30m");
  });

  it("ticks the give-up countdown down on the shared clock", () => {
    const cell = transientCell("networkResume", {
      edge: {
        case: "waiting",
        value: { givesUpAtMs: BigInt(NOW + 30 * 60_000) },
      },
    });
    vi.advanceTimersByTime(1000);
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(
      "gives up in 29m",
    );
  });

  it("stamps the wait's edge on the line", () => {
    const cell = transientCell("networkResume", {
      edge: { case: "gaveUp", value: {} },
    });
    expect(
      cell
        .querySelector(".footer-activity-network-resume")
        ?.getAttribute("data-edge"),
    ).toBe("gaveUp");
  });

  it("refuses a network-resume transient whose edge sets no arm", () => {
    expect(() => transientCell("networkResume", {})).toThrow(MalformedView);
  });

  it("refuses a transient KIND the bundle cannot name", () => {
    const activity = activityOf(
      "idle",
      unpinnedInit(transientInit("hook", { name: "fmt" })),
    );
    const pair = (
      activity as unknown as {
        tier: { value: { transient: { kind: unknown } } };
      }
    ).tier.value;
    pair.transient.kind = { case: "teleporting", value: {} };
    expect(() => draw(activity)).toThrow(MalformedView);
  });

  it("prefixes the subagent's label when the transient came from one", () => {
    const cell = transientCell(
      "hook",
      { name: "fmt" },
      { agent: "Explore" },
    );
    expect(cell.querySelector(".footer-activity-transient")?.textContent).toBe(
      "Explore · fmt",
    );
  });

  it("colours the subagent's label as an identity", () => {
    const cell = transientCell(
      "hook",
      { name: "fmt" },
      { agent: "Explore" },
    );
    expect(cell.querySelector('[data-datum="agent"]')?.className).toContain(
      activityDatumClass("agent"),
    );
  });

  it("draws no prefix for the main agent's work", () => {
    const cell = transientCell("hook", { name: "fmt" });
    expect(cell.querySelector('[data-datum="agent"]')).toBeNull();
  });

  it("draws a long line whole, leaving the cut to the stylesheet", () => {
    const name = "x".repeat(500);
    expect(
      transientCell("hook", { name }).querySelector(".footer-activity-hook")
        ?.textContent,
    ).toBe(name);
  });

  it("ticks the transient's relative age", () => {
    const cell = transientCell(
      "hook",
      { name: "fmt" },
      { atMs: BigInt(NOW - 3000) },
    );
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 3s ago");
  });
});

// ---- the enduring tier ------------------------------------------------------------

describe("the enduring line", () => {
  it("draws an empty line when neither figure has been observed", () => {
    expect(
      enduringCell({}).querySelector(".footer-activity-enduring")?.textContent,
    ).toBe("");
  });

  it("draws the session and weekly allowances as percentages", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.72, true, 3_900_000, "allowedWarning"),
        weekly: allowance(0.31, false, 259_200_000, "allowed"),
      },
    });
    expect(cell.querySelector(".footer-activity-enduring")?.textContent).toBe(
      `session 72% · resets in 1h 5m${FOOTER_RATE_SEPARATOR_GAP}|${FOOTER_RATE_SEPARATOR_GAP}weekly 31% · resets in 3d`,
    );
  });

  it("draws the overage window beside the two when the vendor reported one", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.4, false, 3_900_000),
        weekly: allowance(0.3, false, 259_200_000),
        overage: allowance(0.1, false, 7_200_000),
      },
    });
    expect(cell.querySelector('[data-allowance="overage"]')?.textContent).toBe(
      "overage 10% · resets in 2h",
    );
  });

  it("refuses an enduring line that sets no line", () => {
    const activity = activityOf("idle", unpinnedInit());
    const pair = (
      activity as unknown as {
        tier: { value: { enduring: { line: unknown } } };
      }
    ).tier.value;
    pair.enduring.line = { case: undefined };
    expect(() => draw(activity)).toThrow(MalformedView);
  });
  it("colours an allowance's percent by how full it is", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.85, true, 60_000) },
    });
    const percent = cell.querySelector<HTMLElement>(
      '[data-allowance="session"] [data-datum="percent"]',
    );
    expect(percent?.textContent).toBe("85%");
    expect(percent?.style.color).toBe(paintedAs(85));
  });

  it("emphasizes the newsworthy allowance", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.85, true, 60_000) },
    });
    expect(
      cell
        .querySelector('[data-allowance="session"]')
        ?.getAttribute("data-newsworthy"),
    ).toBe("true");
  });

  it("leaves an allowance that is not newsworthy unemphasized", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.2, false, 60_000) },
    });
    expect(
      cell
        .querySelector('[data-allowance="session"]')
        ?.hasAttribute("data-newsworthy"),
    ).toBe(false);
  });

  it("draws the newsworthy window first even when it is the weekly one", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.31, false, 60_000),
        weekly: allowance(0.91, true, 60_000),
      },
    });
    expect(
      cell
        .querySelector(".footer-rate-figures")
        ?.firstElementChild?.getAttribute("data-allowance"),
    ).toBe("weekly");
  });

  it("draws one blue-classed separator between two allowances", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.31, false, 60_000),
        weekly: allowance(0.2, false, 60_000),
      },
    });
    const separators = [
      ...cell.querySelectorAll(".footer-rate-figures .footer-rate-separator"),
    ];
    expect(separators.map((s) => s.textContent)).toEqual(["|"]);
  });

  it("draws no separator beside a single allowance", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.31, false, 60_000) },
    });
    expect(cell.querySelector(".footer-rate-separator")).toBeNull();
  });

  it("keeps the gaps around the separator in the line's own color", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.31, false, 60_000),
        weekly: allowance(0.2, false, 60_000),
      },
    });
    const separator = cell.querySelector(".footer-rate-separator");
    expect([
      separator?.previousSibling?.nodeType,
      separator?.nextSibling?.nodeType,
    ]).toEqual([Node.TEXT_NODE, Node.TEXT_NODE]);
  });

  it("spaces the separator with exactly three spaces on each side", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.31, false, 60_000),
        weekly: allowance(0.2, false, 60_000),
      },
    });
    const separator = cell.querySelector(".footer-rate-separator");
    expect([
      separator?.previousSibling?.textContent,
      separator?.nextSibling?.textContent,
    ]).toEqual(["\u00a0\u00a0\u00a0", "\u00a0\u00a0\u00a0"]);
  });

  it("spaces the separator with no-break spaces, which whitespace collapsing never folds", () => {
    expect([...FOOTER_RATE_SEPARATOR_GAP]).toEqual([
      "\u00a0",
      "\u00a0",
      "\u00a0",
    ]);
  });

  it("composes the divider as gap, bar, gap", () => {
    const [before, bar, after] = drawFooterRateDivider();
    expect([before, bar.className, bar.textContent, after]).toEqual([
      FOOTER_RATE_SEPARATOR_GAP,
      "footer-rate-separator",
      "|",
      FOOTER_RATE_SEPARATOR_GAP,
    ]);
  });

  it("draws the divider between the allowances through the one helper", () => {
    expect(ACTIVITY_SOURCE).not.toMatch(
      /append\([^)]*drawFooterRateSeparator\(\)/,
    );
  });

  it("sets the separator bold in the stylesheet", () => {
    // Arrange
    const teardown = installStylesheet();
    const cell = enduringCell({
      usage: {
        session: allowance(0.31, false, 60_000),
        weekly: allowance(0.2, false, 60_000),
      },
    });
    document.body.replaceChildren(cell);
    // Act
    const weight = cascadedValue(
      cell.querySelector(".footer-rate-separator") as Element,
      "font-weight",
    );
    teardown();
    // Assert
    expect(weight).toBe("700");
  });

  it("ticks an allowance's reset countdown on the shared clock", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.2, false, 3_600_000) },
    });
    vi.advanceTimersByTime(60_000);
    expect(
      cell.querySelector('[data-allowance="session"] [data-countdown]')
        ?.textContent,
    ).toBe(" · resets in 59m");
  });

  it.each([
    ["usage", { usage: { session: allowance(0.2, false, 60_000) } }],
    ["unobserved", {}],
  ] as const)("stamps the %s line on the enduring line", (line, figures) => {
    expect(
      enduringCell(figures)
        .querySelector(".footer-activity-enduring")
        ?.getAttribute("data-line"),
    ).toBe(line);
  });

  it("draws no reading age beside the usage figures", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.41, false, 60_000) },
    });
    expect(cell.querySelector("[data-age]")).toBeNull();
  });

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)(
    "colours nothing of a %s allowance but its percentage",
    (arm) => {
      const cell = armCell(arm) as HTMLElement | null;
      expect([
        cell?.className,
        cell?.style.color,
        cell?.querySelector<HTMLElement>("[data-countdown]")?.style.color,
      ]).toEqual([
        `footer-allowance arm-${arm} footer-allowance-newsworthy`,
        "",
        "",
      ]);
    },
  );

  it("draws no age for the enduring line itself", () => {
    expect(enduringCell({}).querySelector(".footer-activity-age")).toBeNull();
  });

  /** An enduring line whose SESSION allowance stands at ARM. */
  const armCell = (arm: string): Element | null =>
    enduringCell({
      usage: { session: allowance(0.5, true, 60_000, arm) },
    }).querySelector('[data-allowance="session"]');

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)(
    "carries the %s arm on the allowance",
    (arm) => {
      expect(armCell(arm)?.getAttribute("data-arm")).toBe(arm);
    },
  );

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)(
    "titles the %s arm with its own sentence",
    (arm) => {
      expect((armCell(arm) as HTMLElement | null)?.title).not.toBe("");
    },
  );

  it("paints no verdict colour before the vendor has given one", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.5, true, 60_000) },
    });
    expect(cell.querySelector('[data-allowance="session"]')?.className).toBe(
      "footer-allowance footer-allowance-newsworthy",
    );
  });

  it("marks no arm on an allowance with no vendor verdict yet", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.5, true, 60_000) },
    });
    expect(
      cell
        .querySelector('[data-allowance="session"]')
        ?.hasAttribute("data-arm"),
    ).toBe(false);
  });
});

// ---- the readers of one tier ------------------------------------------------------

describe("salientKind", () => {
  it("names the salient kind of a pinned cell", () => {
    const activity = activityOf(
      "working",
      salientInit("working", "compaction", { text: "x" }),
    );
    expect(salientKind(activity, "p")?.case).toBe("compaction");
  });

  it("says nothing of an unpinned cell", () => {
    expect(
      salientKind(activityOf("working", unpinnedInit()), "p"),
    ).toBeUndefined();
  });
});

describe("enduringUsage", () => {
  it("hands over the usage an unpinned cell carries", () => {
    const activity = activityOf(
      "idle",
      unpinnedInit(undefined, {
        usage: { session: allowance(0.1, false, 1000) },
      }),
    );
    expect(enduringUsage(activity, "p")?.session?.utilization).toBe(0.1);
  });

  it("says nothing of a pinned cell", () => {
    expect(
      enduringUsage(
        activityOf("idle", salientInit("idle", "queryDied", { text: "x" })),
        "p",
      ),
    ).toBeUndefined();
  });
});

describe("orderedAllowances", () => {
  it("keeps the contract's order when nothing is newsworthy", () => {
    const activity = activityOf(
      "idle",
      unpinnedInit(undefined, {
        usage: {
          session: allowance(0.1, false, 1000),
          weekly: allowance(0.1, false, 1000),
          overage: allowance(0.1, false, 1000),
        },
      }),
    );
    const usage = enduringUsage(activity, "p");
    if (usage === undefined) throw new Error("no usage");
    expect(orderedAllowances(usage).map((a) => a.label)).toEqual([
      "session",
      "weekly",
      "overage",
    ]);
  });
});

describe("the cold gate's cost line", () => {
  const cost = {
    text: "the conversation is cold at 409,051 context tokens",
    lead: "the conversation is cold at ",
    figure: { text: "409,051", windowFill: 0.41 },
    tail: " context tokens",
  };

  /** The color PERCENT paints on the cold-gate gradient, as the DOM normalizes it. */
  function painted(percent: number): string {
    const probe = document.createElement("span");
    probe.style.color = coldGatePercentColor(percent);
    return probe.style.color;
  }

  it("draws the figure in its own span between the lead and the tail", () => {
    // Act
    const cell = salientCell("coldGateCost", cost, "waiting");
    // Assert
    expect(cell.querySelector(".footer-cold-gate-figure")?.textContent).toBe("409,051");
  });

  it("colors the figure by its window fill on the cold-gate gradient", () => {
    // Act
    const cell = salientCell("coldGateCost", cost, "waiting");
    // Assert
    expect(cell.querySelector<HTMLElement>(".footer-cold-gate-figure")?.style.color).toBe(painted(41));
  });

  it("refuses parts that do not make the composed line", () => {
    // Arrange
    const broken = { ...cost, tail: " tokens" };
    // Act / Assert
    expect(() => salientCell("coldGateCost", broken, "waiting")).toThrow(MalformedView);
  });
});
