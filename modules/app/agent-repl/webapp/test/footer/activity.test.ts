// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FooterStatusSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawFooterStatusActivity,
  enduringUsage,
  orderedAllowances,
  salientKind,
  type FooterActivity,
} from "../../src/footer/activity.js";
import { footerStatusActivity } from "../../src/footer/strip.js";
import {
  FOOTER_ALLOWANCE_STATUS_CASES,
  FOOTER_STATUS_CASES,
  activityDatumClass,
  allowanceStatusClass,
  footerPercentColor,
} from "../../src/footer/tones.js";

/** The color a footer percent of PERCENT is painted, as the DOM reports it. */
function paintedAs(percent: number): string {
  const probe = document.createElement("span");
  probe.style.color = footerPercentColor(percent);
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
const UNPINNED_ARMS: readonly string[] = FOOTER_STATUS_CASES.filter((arm) => arm !== "waiting");

/**
 * The activity cell of STATUSCASE carrying INIT, built through the status arm
 * so the per-arm container is the generated one. The one cast: the arm is
 * named at run time, so there is no static type to build it under.
 */
function activityOf(statusCase: string, init: Record<string, unknown>): FooterActivity {
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
  return statusCase === "waiting" ? { salient } : { tier: { case: "salient", value: salient } };
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
      value: { ...(transient === undefined ? {} : { transient }), enduring: enduringLine(enduring) },
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
function salientCell(kindCase: string, value: Record<string, unknown>, statusCase = "idle"): HTMLElement {
  return drawCell(statusCase, salientInit(statusCase, kindCase, value)).cell;
}

/** The idle cell drawing a live transient of KINDCASE. */
function transientCell(
  kindCase: string,
  value: Record<string, unknown>,
  init: TransientInit = {},
): HTMLElement {
  return drawCell("idle", unpinnedInit(transientInit(kindCase, value, init))).cell;
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
    const cell = salientCell("queryDied", { text: "the next prompt restarts it" });
    expect(cell.getAttribute("data-tier")).toBe("salient");
  });

  it("names the salient kind on the cell", () => {
    const cell = salientCell("queryDied", { text: "x" });
    expect(cell.getAttribute("data-arm")).toBe("queryDied");
  });

  it("reads the waiting cell, which has no tier oneof, as salient", () => {
    const cell = salientCell("gatedCall", { text: "Bash: rm -rf …" }, "waiting");
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
      drawCell("idle", { tier: { case: "salient", value: { at: { atMs: BigInt(NOW) } } } }),
    ).toThrow(MalformedView);
  });

  it("refuses a salient line with no standing instant", () => {
    expect(() =>
      drawCell("idle", {
        tier: { case: "salient", value: { kind: { case: "queryDied", value: { text: "x" } } } },
      }),
    ).toThrow(MalformedView);
  });

  it("refuses an unpinned pair with no enduring line", () => {
    expect(() => drawCell("idle", { tier: { case: "unpinned", value: {} } })).toThrow(MalformedView);
  });

  it("refuses a salient KIND the bundle cannot name", () => {
    // ARRANGE — a legal cell, then the arm a NEWER daemon set, poked in: the
    // generated `create` drops a case its descriptors do not know.
    const activity = activityOf("idle", salientInit("idle", "queryDied", { text: "x" }));
    const tier = (activity as unknown as { tier: { value: { kind: unknown } } }).tier;
    tier.value.kind = { case: "teleporting", value: {} };
    // ACT / ASSERT
    expect(() => draw(activity)).toThrow(MalformedView);
  });

  it("refuses a TIER the bundle cannot name", () => {
    const activity = activityOf("idle", unpinnedInit());
    (activity as unknown as { tier: unknown }).tier = { case: "pinned", value: {} };
    expect(() => draw(activity)).toThrow(MalformedView);
  });

  it("titles the cell with the whole drawn line", () => {
    const cell = transientCell("hook", { name: "a long line the cell may cut" }, { agent: "Explore" });
    expect(cell.title).toBe("Explore · a long line the cell may cut");
  });
});

// ---- the one clock decision ---------------------------------------------------

describe("the transient's expiry: the client's one clock decision", () => {
  it("draws the transient while the clock is before its expiry", () => {
    const cell = transientCell("hook", { name: "still here" }, { expiresAtMs: BigInt(NOW + 1) });
    expect(cell.querySelector(".footer-activity-hook")?.textContent).toBe("still here");
  });

  it("draws the enduring line once the clock reaches the expiry", () => {
    const cell = transientCell("hook", { name: "gone" }, { expiresAtMs: BigInt(NOW) });
    expect(cell.getAttribute("data-tier")).toBe("enduring");
  });

  it("draws nothing of a lapsed transient", () => {
    const cell = transientCell("hook", { name: "gone" }, { expiresAtMs: BigInt(NOW - 1) });
    expect(cell.textContent).not.toContain("gone");
  });

  it("schedules exactly one re-render at a drawn transient's expiry", () => {
    const { scheduled } = drawCell(
      "idle",
      unpinnedInit(transientInit("hook", { name: "x" }, { expiresAtMs: BigInt(NOW + 7_000) })),
    );
    expect(scheduled).toEqual([NOW + 7_000]);
  });

  it("schedules nothing for a lapsed transient", () => {
    const { scheduled } = drawCell(
      "idle",
      unpinnedInit(transientInit("hook", { name: "x" }, { expiresAtMs: BigInt(NOW - 1) })),
    );
    expect(scheduled).toEqual([]);
  });

  it("schedules nothing for the enduring line alone", () => {
    expect(drawCell("idle", unpinnedInit()).scheduled).toEqual([]);
  });

  it("schedules nothing for a salient line", () => {
    expect(drawCell("idle", salientInit("idle", "queryDied", { text: "x" })).scheduled).toEqual([]);
  });

  it("refuses a lapsed transient whose kind sets no arm, drawn or not", () => {
    const transient = transientInit("hook", { name: "x" }, { expiresAtMs: BigInt(NOW - 1) });
    delete transient.kind;
    expect(() => drawCell("idle", unpinnedInit(transient))).toThrow(MalformedView);
  });

  it("refuses a transient with no expiry", () => {
    const transient = transientInit("hook", { name: "x" });
    delete transient.expiry;
    expect(() => drawCell("idle", unpinnedInit(transient))).toThrow(MalformedView);
  });

  it("refuses a transient with no event instant", () => {
    const transient = transientInit("hook", { name: "x" });
    delete transient.at;
    expect(() => drawCell("idle", unpinnedInit(transient))).toThrow(MalformedView);
  });

  it.each(UNPINNED_ARMS.map((arm) => [arm]))("draws a transient under the %s arm", (arm) => {
    const { cell } = drawCell(arm, unpinnedInit(transientInit("hook", { name: "fmt" })));
    expect(cell.querySelector(".footer-activity-hook")?.textContent).toBe("fmt");
  });
});

// ---- the salient tier -----------------------------------------------------------

describe("the quiet tier: the quiet-stretch line under working and background", () => {
  /** A cell of STATUSCASE carrying QUIET beneath TRANSIENT (if any). */
  function quietInit(
    quiet: string,
    transient?: Record<string, unknown>,
  ): Record<string, unknown> {
    return {
      tier: {
        case: "unpinned",
        value: {
          ...(transient === undefined ? {} : { transient }),
          quietStretch: { at: { atMs: BigInt(NOW - 4000) }, text: quiet },
          enduring: enduringLine(),
        },
      },
    };
  }

  it.each([
    ["working", "✅ Bash finished — handling result..."],
    ["background", "✅ Subagent finished"],
  ])("draws the %s cell's quiet-stretch line verbatim", (statusCase, text) => {
    const { cell } = drawCell(statusCase, quietInit(text));
    expect(cell.getAttribute("data-tier")).toBe("quiet");
    expect(cell.querySelector(".footer-activity-quiet-stretch")?.textContent).toBe(text);
  });

  it("ticks the quiet-stretch line's age from when it began standing", () => {
    const { cell } = drawCell("working", quietInit("✅ Bash finished — handling result..."));
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 4s ago");
  });

  it("refuses a quiet-stretch line with no instant", () => {
    const init = quietInit("✅ Bash finished — handling result...") as {
      tier: { value: { quietStretch: { at?: unknown } } };
    };
    delete init.tier.value.quietStretch.at;
    expect(() => drawCell("working", init)).toThrow(MalformedView);
  });

  it("draws a live transient over the quiet-stretch line", () => {
    const { cell } = drawCell(
      "working",
      quietInit("✅ Bash finished — handling result...", transientInit("toolCall", { tool: "Read" })),
    );
    expect(cell.getAttribute("data-tier")).toBe("transient");
  });

  it("draws the quiet-stretch line again once the transient over it lapses", () => {
    const { cell } = drawCell(
      "working",
      quietInit(
        "✅ Bash finished — handling result...",
        transientInit("toolCall", { tool: "Read" }, { expiresAtMs: BigInt(NOW) }),
      ),
    );
    expect(cell.getAttribute("data-tier")).toBe("quiet");
  });

  it("draws the enduring line under working when no quiet-stretch line stands", () => {
    const { cell } = drawCell("working", unpinnedInit());
    expect(cell.getAttribute("data-tier")).toBe("enduring");
  });
});

describe("the salient kinds", () => {
  it.each([
    ["waiting", "gatedCall", { text: "Bash: rm -rf …" }, "Bash: rm -rf …"],
    ["waiting", "questionLead", { text: "2 questions · which?" }, "2 questions · which?"],
    ["waiting", "blockedOnUser", { detail: "requires action" }, "requires action"],
    ["waiting", "coldGateCost", { text: "182k to re-read" }, "182k to re-read"],
    ["waiting", "interrupting", { text: "stopping the turn…" }, "stopping the turn…"],
    ["working", "compaction", { text: "compacting · 412 of 900" }, "compacting · 412 of 900"],
    ["blocked", "authenticating", { line: "open the login" }, "open the login"],
    // A dead query is a FAILED TURN (owner ruling, 2026-09-28): its line
    // stands in the idle cell the turn_failed arm shares.
    ["turnFailed", "queryDied", { text: "the next prompt restarts it" }, "the next prompt restarts it"],
    ["closing", "closeBlocked", { text: "a turn is in flight" }, "a turn is in flight"],
    ["disconnected", "fault", { kind: "resume_failed", detail: "the shim refused" }, "resume failed · the shim refused"],
    ["blocked", "fault", { kind: "prompts_dir_missing", detail: "no prompts" }, "prompts dir missing · no prompts"],
    ["idle", "notification", { text: "the agent addressed you" }, "the agent addressed you"],
    ["working", "contextBudget", { text: "compaction failed — the summary was empty" }, "compaction failed — the summary was empty"],
    ["background", "rateLimit", { window: { window: { case: "weekly", value: {} } }, verdict: { case: "allowedWarning", value: {} }, utilization: 0.85 }, "weekly nearly spent 85%"],
    ["blocked", "rateLimit", { window: { window: { case: "session", value: {} } }, verdict: { case: "rejected", value: {} } }, "session spent"],
    ["merging", "rateLimit", { verdict: { case: "rejected", value: {} } }, "usage spent"],
    ["loading", "rateLimit", { window: { window: { case: "weeklyOverageIncluded", value: {} } }, verdict: { case: "allowedWarning", value: {} } }, "weekly overage included nearly spent"],
  ])("draws the %s cell's %s line", (statusCase, kindCase, value, expected) => {
    expect(salientCell(kindCase, value, statusCase).textContent).toContain(expected);
  });

  it("counts a rate-limit event's reset down on the shared clock", () => {
    const cell = salientCell(
      "rateLimit",
      { verdict: { case: "rejected", value: {} }, resetsAtS: BigInt(Math.floor(NOW / 1000) + 3600) },
      "idle",
    );
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe(" · resets in 1h");
  });

  it("colours a rate-limit event by its verdict", () => {
    const cell = salientCell("rateLimit", { verdict: { case: "rejected", value: {} } }, "idle");
    expect(cell.querySelector(".footer-activity-rate-limit")?.className).toContain(allowanceStatusClass("rejected"));
  });

  it("colours a rate-limit event's utilization by how full it is", () => {
    const cell = salientCell("rateLimit", { verdict: { case: "allowedWarning", value: {} }, utilization: 0.92 }, "idle");
    const percent = cell.querySelector<HTMLElement>('.footer-activity-rate-limit [data-datum="percent"]');
    expect(percent?.textContent).toBe("92%");
    expect(percent?.style.color).toBe(paintedAs(92));
  });

  it("refuses a rate-limit event with no verdict", () => {
    expect(() => salientCell("rateLimit", {}, "idle")).toThrow(MalformedView);
  });

  it("draws the bring-up failure's cause verbatim", () => {
    const cell = salientCell("startFailed", { detail: "exit 1: no module", droppedPrompts: 0 }, "disconnected");
    expect(cell.querySelector(".footer-activity-start-failed")?.textContent).toBe("exit 1: no module");
  });

  it("names the ONE held prompt a bring-up failure dropped in the singular", () => {
    const cell = salientCell("startFailed", { detail: "exit 1", droppedPrompts: 1 }, "disconnected");
    expect(cell.textContent).toContain("· 1 held prompt dropped");
  });

  it("counts the held prompts a bring-up failure dropped in the plural", () => {
    const cell = salientCell("startFailed", { detail: "exit 1", droppedPrompts: 3 }, "disconnected");
    expect(cell.textContent).toContain("· 3 held prompts dropped");
  });

  it("colours the retry ATTEMPT as its own datum", () => {
    const cell = salientCell("retrying", { attempt: 2, status: "overloaded" }, "working");
    expect(cell.querySelector('[data-datum="attempt"]')?.textContent).toBe("#2");
  });

  it("draws the retry's status verbatim beside the attempt", () => {
    const cell = salientCell("retrying", { attempt: 2, status: "overloaded" }, "working");
    expect(cell.querySelector(".footer-activity-retrying")?.textContent).toBe("retry #2 · overloaded");
  });

  it("colours the landing commit's SHA as its own datum", () => {
    const cell = salientCell("mergingCommit", { sha: "4f2a1c", subject: "fold tokens" }, "merging");
    expect(cell.querySelector('[data-datum="sha"]')?.textContent).toBe("4f2a1c");
  });

  it("draws the commit's subject after its sha", () => {
    const cell = salientCell("mergingCommit", { sha: "4f2a1c", subject: "fold tokens" }, "merging");
    expect(cell.querySelector(".footer-activity-merging-commit")?.textContent).toBe("4f2a1c: fold tokens");
  });

  it("counts a wakeup down at second resolution", () => {
    const cell = salientCell("wakeup", { wakeAtMs: BigInt(NOW + 252_000) }, "waiting");
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe("wakes in 4m 12s");
  });

  it("re-reads the wakeup countdown on the shared tick", () => {
    const cell = salientCell("wakeup", { wakeAtMs: BigInt(NOW + 252_000) }, "waiting");
    vi.advanceTimersByTime(1000);
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe("wakes in 4m 11s");
  });

  it("draws the wakeup's reason when the agent gave one", () => {
    const cell = salientCell(
      "wakeup",
      { wakeAtMs: BigInt(NOW + 60_000), reason: { text: "check the deploy" } },
      "waiting",
    );
    expect(cell.querySelector(".footer-activity-wakeup")?.textContent).toContain("check the deploy");
  });

  it("floors a wakeup whose deadline has passed rather than counting backwards", () => {
    const cell = salientCell("wakeup", { wakeAtMs: BigInt(NOW - 5000) }, "waiting");
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe("wakes in 0s");
  });

  it("ticks the salient line's relative age", () => {
    const { cell } = drawCell("idle", salientInit("idle", "queryDied", { text: "x" }, BigInt(NOW - 120_000)));
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 2m ago");
  });

  it("reads the nearest second when a tick samples just short of one", () => {
    const { cell } = drawCell("idle", salientInit("idle", "queryDied", { text: "x" }, BigInt(NOW - 4920)));
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 5s ago");
  });

  it("re-reads the age on the shared tick", () => {
    const { cell } = drawCell("idle", salientInit("idle", "queryDied", { text: "x" }, BigInt(NOW - 120_000)));
    vi.advanceTimersByTime(60_000);
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 3m ago");
  });
});

describe("the fault line", () => {
  it("draws the kind lowercase with spaces, never underscores", () => {
    const cell = salientCell("fault", { kind: "watch_open_refused", detail: "handle 7" }, "disconnected");
    expect(cell.textContent).not.toContain("_");
  });

  it("draws the kind alone when the fault carries no detail", () => {
    const cell = salientCell("fault", { kind: "session_absent", detail: "" }, "disconnected");
    expect(cell.querySelector(".footer-activity-fault")?.textContent).toBe("session absent");
  });

  // A NON-ESCALATING fault is a TRANSIENT (footer.proto, THE FAULT
  // PARTITION): the session is serving, so it is announced, never pinned.
  it("draws a non-escalating fault as a transient", () => {
    const cell = transientCell("fault", { kind: "deploy_failed", detail: "build webapp: error TS2322" });
    expect(cell.querySelector(".footer-activity-fault")?.textContent).toBe(
      "deploy failed · build webapp: error TS2322",
    );
  });

  it.each([
    ["the turn named no answering response"],
    ["the named answer has no drawn row"],
  ])("draws the unresolved final answer with its detail %s", (detail) => {
    const cell = transientCell("fault", { kind: "final_answer_unresolved", detail });
    expect(cell.querySelector(".footer-activity-fault")?.textContent).toBe(
      `final answer unresolved · ${detail}`,
    );
  });
});

describe("the update line: a deploy's progress", () => {
  const updateText = (update: Record<string, unknown>): string | null | undefined =>
    salientCell("update", update).querySelector(".footer-activity-update")?.textContent;

  it("draws the building phase with the components it builds", () => {
    expect(
      updateText({
        phase: {
          case: "building",
          value: { components: [{ component: { case: "shim", value: {} } }, { component: { case: "webapp", value: {} } }] },
        },
      }),
    ).toBe("building · shim, webapp");
  });

  it("draws a phase with no payload as its arm name alone", () => {
    expect(updateText({ phase: { case: "handingOver", value: {} } })).toBe("handing over");
  });

  it("draws the services being restarted", () => {
    expect(
      updateText({
        phase: {
          case: "restartingServices",
          value: { services: [{ component: { case: "store", value: {} } }, { component: { case: "sidecar", value: {} } }] },
        },
      }),
    ).toBe("restarting services · store, sidecar");
  });

  it("draws what a waiting workspace's move waits on", () => {
    expect(updateText({ phase: { case: "waiting", value: { turns: 1, background: 2 } } })).toBe(
      "waiting · 1 turn, 2 background",
    );
  });

  it("colours the waiting counts as figures", () => {
    const cell = salientCell("update", { phase: { case: "waiting", value: { turns: 0, background: 3 } } });
    const figures = cell.querySelectorAll(".footer-activity-update [data-datum='count']");
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
    const cell = salientCell("update", { phase: { case: "installing", value: {} } });
    expect(cell.querySelector(".footer-activity-update")?.getAttribute("data-phase")).toBe("installing");
  });

  it("refuses an update line whose phase is unset", () => {
    expect(() => salientCell("update", {})).toThrow(MalformedView);
  });

  it("refuses a component whose arm is unset", () => {
    expect(() => salientCell("update", { phase: { case: "building", value: { components: [{}] } } })).toThrow(
      MalformedView,
    );
  });

  it("refuses a note whose arm is unset", () => {
    expect(() =>
      salientCell("update", { phase: { case: "installing", value: {} }, notes: [{}] }),
    ).toThrow(MalformedView);
  });

  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))("stands under the %s arm", (arm) => {
    const cell = salientCell("update", { phase: { case: "installing", value: {} } }, arm);
    expect(cell.querySelector(".footer-activity-update")?.textContent).toBe("installing");
  });
});

// ---- the transient tier ---------------------------------------------------------

describe("the transient kinds", () => {
  it.each([
    ["toolCall", { tool: "Bash", summary: "npm test" }, ".footer-activity-tool-call", "Bash: npm test"],
    ["toolCall", { tool: "TodoWrite" }, ".footer-activity-tool-call", "TodoWrite"],
    ["task", { subject: "port the footer", completed: 3, total: 7 }, ".footer-activity-task", "port the footer · 3/7"],
    ["submitting", { promptLead: "fix the footer", stage: { case: "held", value: { position: 2, queued: 3 } } }, ".footer-activity-submitting", "queued 2/3 · fix the footer"],
    ["submitting", { promptLead: "fix the footer", stage: { case: "classifying", value: {} } }, ".footer-activity-submitting", "classifying · fix the footer"],
    ["submitting", { promptLead: "fix the footer", stage: { case: "interjecting", value: {} } }, ".footer-activity-submitting", "interrupting the turn · fix the footer"],
    ["submitting", { promptLead: "fix the footer", stage: { case: "coalesced", value: {} } }, ".footer-activity-submitting", "coalesced · fix the footer"],
    ["submitting", { promptLead: "fix the footer", stage: { case: "delivered", value: {} } }, ".footer-activity-submitting", "sent · fix the footer"],
    ["hook", { name: "protect-master" }, ".footer-activity-hook", "protect-master"],
    ["contextInjected", { text: "webapp/CLAUDE.md" }, ".footer-activity-context-injected", "webapp/CLAUDE.md"],
    ["daemonWarning", { operation: "feed.row-order-changed", message: "kept in place" }, ".footer-activity-daemon-warning", "feed.row-order-changed · kept in place"],
    ["daemonError", { operation: "store.write", message: "disk full" }, ".footer-activity-daemon-error", "store.write · disk full"],
    ["sessionChange", { text: "model → opus" }, ".footer-activity-session-change", "model → opus"],
    ["updated", {}, ".footer-activity-update", "updated"],
    ["updated", { notes: [{ note: { case: "shimWhenIdle", value: {} } }] }, ".footer-activity-update", "updated · shim when idle"],
    ["compactionConcluded", { text: "compacted and resumed (101.6k → 12.4k)" }, ".footer-activity-compaction", "compacted and resumed (101.6k → 12.4k)"],
    ["networkResume", { edge: { case: "resumed", value: {} } }, ".footer-activity-network-resume", "network resume · resumed"],
    ["networkResume", { edge: { case: "gaveUp", value: {} } }, ".footer-activity-network-resume", "network resume · gave up"],
    ["networkResume", { edge: { case: "abandoned", value: { reason: "the shim stood down" } } }, ".footer-activity-network-resume", "network resume · abandoned · the shim stood down"],
  ])("draws the %s transient %j", (kindCase, value, selector, expected) => {
    expect(transientCell(kindCase, value).querySelector(selector)?.textContent).toBe(expected);
  });

  it("names a concluded compaction's kind on the cell", () => {
    const cell = transientCell("compactionConcluded", { text: "compacted" });
    expect(cell.getAttribute("data-arm")).toBe("compactionConcluded");
  });

  it("refuses a submitting transient whose stage sets no arm", () => {
    expect(() => transientCell("submitting", { promptLead: "x" })).toThrow(MalformedView);
  });

  it("colours a held prompt's place in the queue as a figure", () => {
    const cell = transientCell("submitting", {
      promptLead: "x",
      stage: { case: "held", value: { position: 1, queued: 2 } },
    });
    expect(cell.querySelector('[data-datum="position"]')?.className).toBe(activityDatumClass("position"));
  });

  it("stamps the submitting line with its stage", () => {
    const cell = transientCell("submitting", { promptLead: "x", stage: { case: "classifying", value: {} } });
    expect(cell.querySelector(".footer-activity-submitting")?.getAttribute("data-stage")).toBe("classifying");
  });

  it("colours the task tracker's progress as a figure", () => {
    const cell = transientCell("task", { subject: "port", completed: 3, total: 7 });
    expect(cell.querySelector('[data-datum="count"]')?.className).toBe(activityDatumClass("count"));
  });

  it("stamps the finished deploy's line with the updated phase", () => {
    const cell = transientCell("updated", {});
    expect(cell.querySelector(".footer-activity-update")?.getAttribute("data-phase")).toBe("updated");
  });

  it("draws a wait's opening edge with its give-up countdown", () => {
    const cell = transientCell("networkResume", {
      edge: { case: "waiting", value: { givesUpAtMs: BigInt(NOW + 30 * 60_000) } },
    });
    expect(cell.querySelector(".footer-activity-network-resume")?.textContent).toBe(
      "network resume · waiting · gives up in 30m",
    );
  });

  it("ticks the give-up countdown down on the shared clock", () => {
    const cell = transientCell("networkResume", {
      edge: { case: "waiting", value: { givesUpAtMs: BigInt(NOW + 30 * 60_000) } },
    });
    vi.advanceTimersByTime(1000);
    expect(cell.querySelector("[data-countdown]")?.textContent).toBe("gives up in 29m");
  });

  it("stamps the wait's edge on the line", () => {
    const cell = transientCell("networkResume", { edge: { case: "gaveUp", value: {} } });
    expect(cell.querySelector(".footer-activity-network-resume")?.getAttribute("data-edge")).toBe("gaveUp");
  });

  it("refuses a network-resume transient whose edge sets no arm", () => {
    expect(() => transientCell("networkResume", {})).toThrow(MalformedView);
  });

  it("refuses a transient KIND the bundle cannot name", () => {
    const activity = activityOf("idle", unpinnedInit(transientInit("hook", { name: "fmt" })));
    const pair = (activity as unknown as { tier: { value: { transient: { kind: unknown } } } }).tier.value;
    pair.transient.kind = { case: "teleporting", value: {} };
    expect(() => draw(activity)).toThrow(MalformedView);
  });

  it("prefixes the subagent's label when the transient came from one", () => {
    const cell = transientCell("toolCall", { tool: "Bash", summary: "ls" }, { agent: "Explore" });
    expect(cell.querySelector(".footer-activity-transient")?.textContent).toBe("Explore · Bash: ls");
  });

  it("colours the subagent's label as an identity", () => {
    const cell = transientCell("toolCall", { tool: "Bash" }, { agent: "Explore" });
    expect(cell.querySelector('[data-datum="agent"]')?.className).toContain(activityDatumClass("agent"));
  });

  it("draws no prefix for the main agent's work", () => {
    const cell = transientCell("toolCall", { tool: "Bash" });
    expect(cell.querySelector('[data-datum="agent"]')).toBeNull();
  });

  it("draws a long line whole, leaving the cut to the stylesheet", () => {
    const name = "x".repeat(500);
    expect(transientCell("hook", { name }).querySelector(".footer-activity-hook")?.textContent).toBe(name);
  });

  it("ticks the transient's relative age", () => {
    const cell = transientCell("hook", { name: "fmt" }, { atMs: BigInt(NOW - 3000) });
    expect(cell.querySelector("[data-age]")?.textContent).toBe(" · 3s ago");
  });
});

// ---- the enduring tier ------------------------------------------------------------

describe("the enduring line", () => {
  it("draws an empty line when neither figure has been observed", () => {
    expect(enduringCell({}).querySelector(".footer-activity-enduring")?.textContent).toBe("");
  });

  it("draws the session and weekly allowances as percentages", () => {
    const cell = enduringCell({
      usage: {
        session: allowance(0.72, true, 3_900_000, "allowedWarning"),
        weekly: allowance(0.31, false, 259_200_000, "allowed"),
      },
    });
    expect(cell.querySelector(".footer-activity-enduring")?.textContent).toBe(
      "session 72% · resets in 1h 5m | weekly 31% · resets in 3d",
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
    expect(cell.querySelector('[data-allowance="overage"]')?.textContent).toBe("overage 10% · resets in 2h");
  });

  it("draws the context window's fill alone when no usage has been read", () => {
    const cell = enduringCell({ contextWindow: { usedTokens: 84_000n, windowTokens: 200_000n, fill: 0.42 } });
    expect(cell.querySelector(".footer-activity-enduring")?.textContent).toBe("context 42%");
  });

  it("stamps the chosen line on the enduring line", () => {
    const cell = enduringCell({ contextWindow: { usedTokens: 1n, windowTokens: 2n, fill: 0.5 } });
    expect(cell.querySelector(".footer-activity-enduring")?.getAttribute("data-line")).toBe("contextWindow");
  });

  it("refuses an enduring line that sets no line", () => {
    const activity = activityOf("idle", unpinnedInit());
    const pair = (activity as unknown as { tier: { value: { enduring: { line: unknown } } } }).tier.value;
    pair.enduring.line = { case: undefined };
    expect(() => draw(activity)).toThrow(MalformedView);
  });
  it("colours the context fill by how full it is", () => {
    const cell = enduringCell({ contextWindow: { usedTokens: 1n, windowTokens: 2n, fill: 0.5 } });
    const percent = cell.querySelector<HTMLElement>('.footer-context-window [data-datum="percent"]');
    expect(percent?.textContent).toBe("50%");
    expect(percent?.style.color).toBe(paintedAs(50));
  });

  it("colours an allowance's percent by how full it is", () => {
    const cell = enduringCell({ usage: { session: allowance(0.85, true, 60_000) } });
    const percent = cell.querySelector<HTMLElement>('[data-allowance="session"] [data-datum="percent"]');
    expect(percent?.textContent).toBe("85%");
    expect(percent?.style.color).toBe(paintedAs(85));
  });

  it("emphasizes the newsworthy allowance", () => {
    const cell = enduringCell({ usage: { session: allowance(0.85, true, 60_000) } });
    expect(cell.querySelector('[data-allowance="session"]')?.getAttribute("data-newsworthy")).toBe("true");
  });

  it("leaves an allowance that is not newsworthy unemphasized", () => {
    const cell = enduringCell({ usage: { session: allowance(0.2, false, 60_000) } });
    expect(cell.querySelector('[data-allowance="session"]')?.hasAttribute("data-newsworthy")).toBe(false);
  });

  it("draws the newsworthy window first even when it is the weekly one", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.31, false, 60_000), weekly: allowance(0.91, true, 60_000) },
    });
    expect(
      cell.querySelector(".footer-rate-figures")?.firstElementChild?.getAttribute("data-allowance"),
    ).toBe("weekly");
  });

  it("ticks an allowance's reset countdown on the shared clock", () => {
    const cell = enduringCell({ usage: { session: allowance(0.2, false, 3_600_000) } });
    vi.advanceTimersByTime(60_000);
    expect(cell.querySelector('[data-allowance="session"] [data-countdown]')?.textContent).toBe(
      " · resets in 59m",
    );
  });

  it("renders the age of the last usage reading beside the figures", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.41, false, 60_000), figuresReadAtMs: BigInt(NOW - 630_000) },
    });
    expect(cell.querySelector(".footer-rate-age")?.textContent).toBe(" · 10m 30s ago");
  });

  it("re-reads the usage read-age on the shared tick", () => {
    const cell = enduringCell({
      usage: { session: allowance(0.41, false, 60_000), figuresReadAtMs: BigInt(NOW - 630_000) },
    });
    vi.advanceTimersByTime(1000);
    expect(cell.querySelector(".footer-rate-age")?.textContent).toBe(" · 10m 31s ago");
  });

  it("draws no read-age when the figures carry no read instant", () => {
    const cell = enduringCell({ usage: { session: allowance(0.41, false, 60_000) } });
    expect(cell.querySelector(".footer-rate-age")).toBeNull();
  });

  it("draws no age for the enduring line itself", () => {
    expect(enduringCell({}).querySelector(".footer-activity-age")).toBeNull();
  });

  /** An enduring line whose SESSION allowance stands at ARM. */
  const armCell = (arm: string): Element | null =>
    enduringCell({ usage: { session: allowance(0.5, true, 60_000, arm) } }).querySelector(
      '[data-allowance="session"]',
    );

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)("carries the %s arm on the allowance", (arm) => {
    expect(armCell(arm)?.getAttribute("data-arm")).toBe(arm);
  });

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)("paints the %s arm its own colour", (arm) => {
    expect(armCell(arm)?.className).toContain(allowanceStatusClass(arm));
  });

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)("titles the %s arm with its own sentence", (arm) => {
    expect((armCell(arm) as HTMLElement | null)?.title).not.toBe("");
  });

  it("paints no verdict colour before the vendor has given one", () => {
    const cell = enduringCell({ usage: { session: allowance(0.5, true, 60_000) } });
    expect(cell.querySelector('[data-allowance="session"]')?.className).toBe(
      "footer-allowance footer-allowance-newsworthy",
    );
  });

  it("marks no arm on an allowance with no vendor verdict yet", () => {
    const cell = enduringCell({ usage: { session: allowance(0.5, true, 60_000) } });
    expect(cell.querySelector('[data-allowance="session"]')?.hasAttribute("data-arm")).toBe(false);
  });
});

// ---- the readers of one tier ------------------------------------------------------

describe("salientKind", () => {
  it("names the salient kind of a pinned cell", () => {
    const activity = activityOf("working", salientInit("working", "compaction", { text: "x" }));
    expect(salientKind(activity, "p")?.case).toBe("compaction");
  });

  it("says nothing of an unpinned cell", () => {
    expect(salientKind(activityOf("working", unpinnedInit()), "p")).toBeUndefined();
  });
});

describe("enduringUsage", () => {
  it("hands over the usage an unpinned cell carries", () => {
    const activity = activityOf("idle", unpinnedInit(undefined, { usage: { session: allowance(0.1, false, 1000) } }));
    expect(enduringUsage(activity, "p")?.session?.utilization).toBe(0.1);
  });

  it("says nothing of a pinned cell", () => {
    expect(enduringUsage(activityOf("idle", salientInit("idle", "queryDied", { text: "x" })), "p")).toBeUndefined();
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
    expect(orderedAllowances(usage).map((a) => a.label)).toEqual(["session", "weekly", "overage"]);
  });
});
