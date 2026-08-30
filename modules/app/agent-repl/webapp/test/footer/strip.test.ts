// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import type { DescField } from "@bufbuild/protobuf";
import {
  FooterStatusSchema,
  FooterStripSchema,
  type FooterStatus,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { protoArmName } from "../../src/vocab.js";
import {
  FOOTER_ALLOWANCE_STATUS_CASES,
  FOOTER_STATUS_CASES,
  allowanceStatusClass,
} from "../../src/footer/tones.js";
import {
  IDLE_CLOCK_LABEL,
  drawFooterStrip,
  statusWords,
  subStatusWords,
} from "../../src/footer/strip.js";
import type { FooterPanel } from "../../src/footer/expanded.js";
import { harness, strip, type Harness, type StripInit } from "./harness.js";

/** Every test's clock reads from here, so a countdown's arithmetic is exact. */
const NOW = 1_800_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
});
afterEach(() => {
  vi.useRealTimers();
});

interface Drawn {
  row: HTMLElement;
  selected: FooterPanel[];
  h: Harness;
}

/** Draw a strip, recording every panel the user's clicks would open. */
function drawStrip(init: StripInit = {}, selection: FooterPanel | null = null, h = harness()): Drawn {
  const selected: FooterPanel[] = [];
  const row = drawFooterStrip(strip(init), {
    ctx: h.ctx,
    selection,
    onSelect: (panel) => selected.push(panel),
  });
  return { row, selected, h };
}

// ---- the arm inventories, read off the generated schemas -------------------

/** The `status` oneof's fields, so a new arm reaches these tables by itself. */
const STATUS_FIELDS: readonly DescField[] =
  FooterStatusSchema.oneofs.find((oneof) => oneof.name === "status")?.fields ?? [];

/** Every (status arm, substatus arm) pair the contract declares. */
const SUBSTATUS_PAIRS: ReadonlyArray<[string, string]> = STATUS_FIELDS.flatMap((field) => {
  const message = field.fieldKind === "message" ? field.message : undefined;
  const substatus = message?.oneofs.find((oneof) => oneof.name === "substatus");
  return (substatus?.fields ?? []).map(
    (sub): [string, string] => [field.localName, sub.localName],
  );
});

/**
 * A status arm built by NAME.
 *
 * The one cast in this suite, and a deliberate one: these fixtures are built
 * from arm names the schema enumerated at run time, so there is no static type
 * to build them under — which is exactly what makes the tables below fail when
 * a new arm lands rather than quietly skipping it.
 */
function status(
  statusCase: string,
  value: Record<string, unknown>,
): FooterStatus["status"] {
  return create(FooterStatusSchema, {
    status: { case: statusCase, value },
  } as never).status;
}

/** The activity `waiting` and `loading` always carry. */
const REQUIRED_ACTIVITY = {
  at: { atMs: BigInt(NOW) },
  kind: { case: "notification", value: { text: "a line" } },
};

/** A status arm with SUB set, and the required activity where the arm needs it. */
function withSubStatus(
  statusCase: string,
  subCase: string,
  subValue: Record<string, unknown> = {},
): FooterStatus["status"] {
  const needsActivity = statusCase === "waiting" || statusCase === "loading";
  return status(statusCase, {
    substatus: { case: subCase, value: subValue },
    ...(needsActivity ? { activity: REQUIRED_ACTIVITY } : {}),
  });
}

/** A status arm carrying one ACTIVITY kind, with the arm's substatus when it has one. */
function withActivity(
  statusCase: string,
  subCase: string | null,
  kindCase: string,
  kindValue: Record<string, unknown>,
  atMs: bigint = BigInt(NOW),
): FooterStatus["status"] {
  return status(statusCase, {
    ...(subCase === null ? {} : { substatus: { case: subCase, value: {} } }),
    activity: { at: { atMs }, kind: { case: kindCase, value: kindValue } },
  });
}

// ---- the status cell -------------------------------------------------------

describe("drawFooterStatus: every arm the contract declares", () => {
  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))("draws the %s arm", (arm) => {
    // Background is the only arm with no substatus oneof; waiting and loading
    // require an activity, which `withSubStatus` supplies.
    const firstSub = SUBSTATUS_PAIRS.find(([statusCase]) => statusCase === arm)?.[1];
    const value = firstSub === undefined ? status(arm, {}) : withSubStatus(arm, firstSub);
    const { row } = drawStrip({ status: value });
    expect(row.querySelector(".footer-status")?.getAttribute("data-arm")).toBe(arm);
  });

  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))("words the %s arm lowercase", (arm) => {
    expect(statusWords(arm)).toBe(protoArmName(arm).replace(/_/g, " "));
  });

  it("paints the status cell with the vocabulary's tone class", () => {
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });
    expect(row.querySelector(".footer-status")?.classList.contains("tone-red")).toBe(true);
  });

  it("carries the arm as a class so an arm can be styled on its own", () => {
    const { row } = drawStrip({ status: withSubStatus("merging", "merge") });
    expect(row.querySelector(".footer-status")?.classList.contains("arm-merging")).toBe(true);
  });

  it("refuses a status that sets no arm", () => {
    const bare = create(FooterStripSchema, {
      status: {},
      clock: {},
      tokens: { input: { text: "0 in" } },
      liveWork: {},
    });
    const h = harness();
    expect(() =>
      drawFooterStrip(bare, { ctx: h.ctx, selection: null, onSelect: () => {} }),
    ).toThrow(MalformedView);
  });
});

// ---- the substatus cell and the merge rule --------------------------------

describe("drawFooterSubStatus: the word is the arm, lowercase, with spaces", () => {
  it.each(SUBSTATUS_PAIRS)("draws %s/%s without underscores", (statusCase, subCase) => {
    const { row } = drawStrip({ status: withSubStatus(statusCase, subCase) });
    expect(row.querySelector(".footer-substatus")?.textContent).not.toContain("_");
  });

  it.each(SUBSTATUS_PAIRS)("words %s/%s as the schema spells it", (statusCase, subCase) => {
    const { row } = drawStrip({ status: withSubStatus(statusCase, subCase) });
    expect(row.querySelector(".footer-substatus")?.textContent).toContain(
      subStatusWords(statusCase, subCase),
    );
  });

  it.each([
    ["closing", "blocked", "close blocked"],
    ["merging", "prePrompt", "pre prompt"],
    ["disconnected", "startFailed", "start failed"],
    ["interrupted", "byUser", "by user"],
    ["interrupted", "hostShutdown", "host shutdown"],
    ["blocked", "queryDied", "query died"],
  ])("spells %s/%s as '%s'", (statusCase, subCase, expected) => {
    expect(subStatusWords(statusCase, subCase)).toBe(expected);
  });

  it("names the arm on the cell", () => {
    const { row } = drawStrip({ status: withSubStatus("blocked", "auth") });
    expect(row.querySelector(".footer-substatus")?.getAttribute("data-arm")).toBe("auth");
  });

  it("appends the queue place to the queued step", () => {
    const { row } = drawStrip({
      status: withSubStatus("merging", "queued", { position: 3, depth: 7 }),
    });
    expect(row.querySelector('.footer-substatus [data-datum="position"]')?.textContent).toBe("3/7");
  });

  it("draws the parked step's composed line", () => {
    const { row } = drawStrip({
      status: withSubStatus("merging", "parked", { line: "waiting on your call" }),
    });
    expect(row.querySelector(".footer-substatus-line")?.textContent).toBe("waiting on your call");
  });

  it("MERGES the cell for an arm with no substatus oneof at all", () => {
    const { row } = drawStrip({ status: status("background", {}) });
    expect(row.querySelector(".footer-substatus")).toBeNull();
  });

  it("marks the merged status cell as spanning", () => {
    const { row } = drawStrip({ status: status("background", {}) });
    expect(row.querySelector(".footer-status")?.getAttribute("data-merged")).toBe("true");
  });

  it("MERGES the cell for an arm whose substatus oneof is unset", () => {
    const { row } = drawStrip({ status: status("idle", {}) });
    expect(row.querySelector(".footer-substatus")).toBeNull();
  });
});

// ---- the activity cell -----------------------------------------------------

describe("drawFooterStatusActivity", () => {
  it("draws no arm attribute when the arm legitimately has no activity", () => {
    const { row } = drawStrip({ status: status("idle", {}) });
    expect(row.querySelector(".footer-activity")?.hasAttribute("data-arm")).toBe(false);
  });

  it("refuses a WAITING push with no activity — the schema requires one", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({ status: status("waiting", { substatus: { case: "permission", value: {} } }) }),
        { ctx: h.ctx, selection: null, onSelect: () => {} },
      ),
    ).toThrow(MalformedView);
  });

  it("refuses a LOADING push with no activity", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({ status: status("loading", { substatus: { case: "memory", value: {} } }) }),
        { ctx: h.ctx, selection: null, onSelect: () => {} },
      ),
    ).toThrow(MalformedView);
  });

  it("refuses an activity whose kind oneof sets no arm", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({ status: status("idle", { activity: { at: { atMs: BigInt(NOW) } } }) }),
        { ctx: h.ctx, selection: null, onSelect: () => {} },
      ),
    ).toThrow(MalformedView);
  });

  it("refuses an activity with no standing instant", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({
          status: status("idle", {
            activity: { kind: { case: "notification", value: { text: "hi" } } },
          }),
        }),
        { ctx: h.ctx, selection: null, onSelect: () => {} },
      ),
    ).toThrow(MalformedView);
  });

  it.each([
    ["idle", null, "notification", { text: "the agent has a question" }, "the agent has a question"],
    ["idle", null, "contextBudget", { text: "context is 80% spent" }, "context is 80% spent"],
    ["waiting", "permission", "gatedCall", { text: "Bash: rm -rf …" }, "Bash: rm -rf …"],
    ["waiting", "question", "questionLead", { text: "2 questions · which?" }, "2 questions · which?"],
    ["waiting", "permission", "blockedOnUser", { detail: "requires action" }, "requires action"],
    ["waiting", "coldGate", "coldGateCost", { text: "182k to re-read" }, "182k to re-read"],
    ["waiting", "interrupting", "interrupting", { text: "stopping the turn…" }, "stopping the turn…"],
    ["thinking", "thinking", "hook", { name: "protect-master" }, "protect-master"],
    ["thinking", "thinking", "contextInjected", { text: "webapp/CLAUDE.md" }, "webapp/CLAUDE.md"],
    ["blocked", "auth", "authenticating", { line: "open the login" }, "open the login"],
    ["blocked", "queryDied", "queryDied", { text: "the next prompt restarts it" }, "the next prompt restarts it"],
    ["closing", "blocked", "closeBlocked", { text: "a turn is in flight" }, "a turn is in flight"],
  ])("draws the %s/%s %s line verbatim", (statusCase, subCase, kindCase, value, expected) => {
    const { row } = drawStrip({
      status: withActivity(statusCase, subCase, kindCase, value as Record<string, unknown>),
    });
    expect(row.querySelector(".footer-activity")?.textContent).toContain(expected as string);
  });

  it("names the activity's kind on the cell", () => {
    const { row } = drawStrip({
      status: withActivity("thinking", "thinking", "hook", { name: "fmt" }),
    });
    expect(row.querySelector(".footer-activity")?.getAttribute("data-arm")).toBe("hook");
  });

  it("colours the retry ATTEMPT as its own datum", () => {
    const { row } = drawStrip({
      status: withActivity("thinking", "thinking", "retrying", { attempt: 2, status: "overloaded" }),
    });
    expect(row.querySelector('[data-datum="attempt"]')?.textContent).toBe("#2");
  });

  it("draws the retry's status verbatim beside the attempt", () => {
    const { row } = drawStrip({
      status: withActivity("thinking", "thinking", "retrying", { attempt: 2, status: "overloaded" }),
    });
    expect(row.querySelector(".footer-activity-retrying")?.textContent).toContain("· overloaded");
  });

  it("colours the landing commit's SHA as its own datum", () => {
    const { row } = drawStrip({
      status: withActivity("merging", "merge", "mergingCommit", {
        sha: "4f2a1c",
        subject: "fold tokens into api",
      }),
    });
    expect(row.querySelector('[data-datum="sha"]')?.textContent).toBe("4f2a1c");
  });

  it("draws the commit's subject after its sha", () => {
    const { row } = drawStrip({
      status: withActivity("merging", "merge", "mergingCommit", {
        sha: "4f2a1c",
        subject: "fold tokens into api",
      }),
    });
    expect(row.querySelector(".footer-activity-merging-commit")?.textContent).toBe(
      "4f2a1c: fold tokens into api",
    );
  });
});

describe("the ticking activity figures", () => {
  it("counts a wakeup down at second resolution", () => {
    const { row } = drawStrip({
      status: withActivity("waiting", "wakeup", "wakeup", {
        wakeAtMs: BigInt(NOW + 252_000),
      }),
    });
    expect(row.querySelector("[data-countdown]")?.textContent).toBe("wakes in 4m 12s");
  });

  it("re-reads the wakeup countdown on the shared tick", () => {
    const { row } = drawStrip({
      status: withActivity("waiting", "wakeup", "wakeup", {
        wakeAtMs: BigInt(NOW + 252_000),
      }),
    });
    vi.advanceTimersByTime(1000);
    expect(row.querySelector("[data-countdown]")?.textContent).toBe("wakes in 4m 11s");
  });

  it("draws the wakeup's reason when the agent gave one", () => {
    const { row } = drawStrip({
      status: withActivity("waiting", "wakeup", "wakeup", {
        wakeAtMs: BigInt(NOW + 60_000),
        reason: { text: "check the deploy" },
      }),
    });
    expect(row.querySelector(".footer-activity-wakeup")?.textContent).toContain("check the deploy");
  });

  it("floors a wakeup whose deadline has passed rather than counting backwards", () => {
    const { row } = drawStrip({
      status: withActivity("waiting", "wakeup", "wakeup", { wakeAtMs: BigInt(NOW - 5000) }),
    });
    expect(row.querySelector("[data-countdown]")?.textContent).toBe("wakes in 0s");
  });

  it("draws BOTH rate-limit allowances as percentages", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: true, utilization: 0.72, resetsAtS: BigInt((NOW + 3_900_000) / 1000), status: { case: "allowedWarning", value: {} } },
        weekly: { newsworthy: false, utilization: 0.31, resetsAtS: BigInt((NOW + 259_200_000) / 1000), status: { case: "allowed", value: {} } },
      }),
    });
    expect(row.querySelector(".footer-activity-rate-limited")?.textContent).toBe(
      "session 72% · resets in 1h 5m | weekly 31% · resets in 3d",
    );
  });

  it("emphasizes the newsworthy allowance", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: true, utilization: 0.72, resetsAtS: BigInt((NOW + 3_900_000) / 1000), status: { case: "allowedWarning", value: {} } },
        weekly: { newsworthy: false, utilization: 0.31, resetsAtS: BigInt((NOW + 259_200_000) / 1000), status: { case: "allowed", value: {} } },
      }),
    });
    expect(row.querySelector('[data-allowance="session"]')?.getAttribute("data-newsworthy")).toBe(
      "true",
    );
    expect(row.querySelector('[data-allowance="weekly"]')?.hasAttribute("data-newsworthy")).toBe(
      false,
    );
  });

  /** One rate-limit line whose SESSION allowance stands at ARM. */
  function allowanceRow(arm: string): HTMLElement {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: {
          newsworthy: true,
          utilization: 0.5,
          resetsAtS: BigInt(NOW / 1000),
          status: { case: arm as never, value: {} },
        },
        weekly: {
          newsworthy: false,
          utilization: 0.1,
          resetsAtS: BigInt(NOW / 1000),
          status: { case: "allowed", value: {} },
        },
      }),
    });
    return row;
  }

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)("carries the %s arm on the cell", (arm) => {
    expect(allowanceRow(arm).querySelector('[data-allowance="session"]')?.getAttribute("data-arm")).toBe(
      arm,
    );
  });

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)("paints the %s arm its own colour", (arm) => {
    const cell = allowanceRow(arm).querySelector('[data-allowance="session"]');
    expect(cell?.className).toContain(allowanceStatusClass(arm));
  });

  it.each(FOOTER_ALLOWANCE_STATUS_CASES)("titles the %s arm with its own sentence", (arm) => {
    const cell = allowanceRow(arm).querySelector<HTMLElement>('[data-allowance="session"]');
    expect(cell?.title).not.toBe("");
  });

  it("draws a rejected allowance in the error register, not the warning one", () => {
    const rejected = allowanceRow("rejected").querySelector('[data-allowance="session"]');
    const warning = allowanceRow("allowedWarning").querySelector('[data-allowance="session"]');
    expect(rejected?.className).not.toBe(warning?.className);
  });

  it("refuses an allowance whose vendor status is unset", () => {
    expect(() =>
      drawStrip({
        status: withActivity("idle", null, "rateLimited", {
          session: { newsworthy: true, utilization: 0.5, resetsAtS: BigInt(NOW / 1000) },
          weekly: {
            newsworthy: false,
            utilization: 0.1,
            resetsAtS: BigInt(NOW / 1000),
            status: { case: "allowed", value: {} },
          },
        }),
      }),
    ).toThrow(MalformedView);
  });

  it("ticks the activity's relative age", () => {
    const { row } = drawStrip({
      status: withActivity(
        "idle",
        null,
        "notification",
        { text: "done" },
        BigInt(NOW - 120_000),
      ),
    });
    expect(row.querySelector("[data-age]")?.textContent).toBe(" · 2m ago");
  });

  it("re-reads the age on the shared tick", () => {
    const { row } = drawStrip({
      status: withActivity(
        "idle",
        null,
        "notification",
        { text: "done" },
        BigInt(NOW - 120_000),
      ),
    });
    vi.advanceTimersByTime(60_000);
    expect(row.querySelector("[data-age]")?.textContent).toBe(" · 3m ago");
  });
});

// ---- the clock -------------------------------------------------------------

describe("drawFooterClock", () => {
  it("draws the idle dash when no turn is in flight", () => {
    const { row } = drawStrip();
    expect(row.querySelector(".footer-clock")?.textContent).toBe(IDLE_CLOCK_LABEL);
  });

  it("ticks the elapsed turn time from the shipped instant", () => {
    const { row } = drawStrip({ turnStartedAtMs: BigInt(NOW - 42_000) });
    expect(row.querySelector(".footer-clock .info-time")?.textContent).toBe("42s");
  });

  it("re-reads the clock on the shared tick", () => {
    const { row } = drawStrip({ turnStartedAtMs: BigInt(NOW - 42_000) });
    vi.advanceTimersByTime(3000);
    expect(row.querySelector(".footer-clock .info-time")?.textContent).toBe("45s");
  });

  it("mounts the turn stop ONLY while the clock is live", () => {
    const { row } = drawStrip({ turnStartedAtMs: BigInt(NOW - 1000) });
    expect(row.querySelector(".footer-clock [data-interrupt]")).not.toBeNull();
  });

  it("draws no stop control for an idle clock", () => {
    const { row } = drawStrip();
    expect(row.querySelector("[data-interrupt]")).toBeNull();
  });
});

// ---- the tokens cell -------------------------------------------------------

describe("drawFooterTokensCell", () => {
  it("draws the daemon's figure verbatim", () => {
    const { row } = drawStrip({ tokens: { input: { text: "18.2k in" } } });
    expect(row.querySelector(".footer-tokens-input")?.textContent).toBe("18.2k in");
  });

  it("draws the alarm glyph only when the alarm tripped", () => {
    const { row } = drawStrip({ tokens: { input: { text: "18.2k in" }, alarm: {} } });
    expect(row.querySelector(".footer-tokens [data-alarm]")).not.toBeNull();
  });

  it("draws no alarm glyph when it did not", () => {
    const { row } = drawStrip({ tokens: { input: { text: "18.2k in" } } });
    expect(row.querySelector(".footer-tokens [data-alarm]")).toBeNull();
  });

  it("draws no verdict badge while the turn runs", () => {
    const { row } = drawStrip({ tokens: { input: { text: "18.2k in" } } });
    expect(row.querySelector("[data-verdict]")).toBeNull();
  });

  it.each([
    ["complete", "✓"],
    ["incomplete", "✗"],
    ["invalid", "✗"],
  ])("draws the %s verdict as %s", (arm, glyph) => {
    const { row } = drawStrip({
      tokens: { input: { text: "18.2k in" }, verdict: { verdict: { case: arm as never, value: {} } } },
    });
    const badge = row.querySelector("[data-verdict]");
    expect(badge?.getAttribute("data-verdict")).toBe(arm);
    expect(badge?.textContent).toBe(glyph);
  });

  it("gives the invalid verdict a title of its own, distinct from incomplete's", () => {
    const { row: invalid } = drawStrip({
      tokens: { input: { text: "x" }, verdict: { verdict: { case: "invalid", value: {} } } },
    });
    const { row: incomplete } = drawStrip({
      tokens: { input: { text: "x" }, verdict: { verdict: { case: "incomplete", value: {} } } },
    });
    expect(invalid.querySelector<HTMLElement>("[data-verdict]")?.title).not.toBe(
      incomplete.querySelector<HTMLElement>("[data-verdict]")?.title,
    );
  });

  it("refuses a verdict badge whose oneof sets no arm", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(strip({ tokens: { input: { text: "x" }, verdict: {} } }), {
        ctx: h.ctx,
        selection: null,
        onSelect: () => {},
      }),
    ).toThrow(MalformedView);
  });

  it("selects the tokens panel when clicked — no rpc involved", () => {
    const { row, selected, h } = drawStrip();
    row.querySelector<HTMLElement>(".footer-tokens")?.dispatchEvent(new MouseEvent("click"));
    expect(selected).toEqual(["tokens"]);
    expect(h.calls.interrupt).toHaveLength(0);
  });

  it("marks the cell while its panel is the open one", () => {
    const { row } = drawStrip({}, "tokens");
    expect(row.querySelector(".footer-tokens")?.getAttribute("data-selected")).toBe("true");
  });
});

// ---- the live-work chips ---------------------------------------------------

describe("drawFooterLiveWorkChips", () => {
  it("draws NO chips for a quiet workspace", () => {
    const { row } = drawStrip();
    expect(row.querySelectorAll(".footer-chip")).toHaveLength(0);
  });

  it.each([
    ["agents", { agents: { count: 3 } }, "3"],
    ["shells", { shells: { count: 2 } }, "2"],
    ["monitors", { monitors: { count: 1 } }, "1"],
    ["crons", { crons: { count: 4 } }, "4"],
  ])("draws the %s chip's count verbatim", (chip, liveWork, expected) => {
    const { row } = drawStrip({ liveWork });
    expect(row.querySelector(`[data-chip="${chip}"]`)?.textContent).toContain(expected);
  });

  it("draws the tasks chip as the tracker's fraction", () => {
    const { row } = drawStrip({ liveWork: { tasks: { done: 3, total: 7 } } });
    expect(row.querySelector('[data-chip="tasks"]')?.textContent).toContain("3/7");
  });

  it.each([
    ["agents", { agents: { count: 1 } }],
    ["tasks", { tasks: { done: 0, total: 1 } }],
    ["shells", { shells: { count: 1 } }],
    ["monitors", { monitors: { count: 1 } }],
    ["crons", { crons: { count: 1 } }],
  ])("gives the %s chip a glyph rather than a bare number", (chip, liveWork) => {
    const { row } = drawStrip({ liveWork });
    expect(row.querySelector(`[data-chip="${chip}"] [data-glyph]`)?.textContent).not.toBe("");
  });

  it("omits an unset chip entirely", () => {
    const { row } = drawStrip({ liveWork: { agents: { count: 1 } } });
    expect(row.querySelector('[data-chip="shells"]')).toBeNull();
  });

  it("selects a chip's panel when clicked", () => {
    const { row, selected } = drawStrip({ liveWork: { agents: { count: 2 } } });
    row.querySelector<HTMLElement>('[data-chip="agents"]')?.dispatchEvent(new MouseEvent("click"));
    expect(selected).toEqual(["agents"]);
  });

  it("highlights the chip whose panel is open", () => {
    const { row } = drawStrip({ liveWork: { agents: { count: 2 } } }, "agents");
    expect(row.querySelector('[data-chip="agents"]')?.getAttribute("data-selected")).toBe("true");
  });

  it("leaves an unselected chip unmarked", () => {
    const { row } = drawStrip(
      { liveWork: { agents: { count: 2 }, shells: { count: 1 } } },
      "agents",
    );
    expect(row.querySelector('[data-chip="shells"]')?.hasAttribute("data-selected")).toBe(false);
  });
});
