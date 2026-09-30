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
import {
  FOOTER_STATUS_CASES,
  statusArmClass,
} from "../../src/footer/tones.js";
import {
  IDLE_CLOCK_LABEL,
  drawClientDisconnectedStrip,
  drawFooterStrip,
  WAITING_FOR_API_GLYPH,
  chipGlyph,
  footerStatusActivity,
  subStatusWords,
} from "../../src/footer/strip.js";
import type { FooterPanel } from "../../src/footer/expanded.js";
import {
  STATUS_WAVE_CYCLE_MS,
  STATUS_WAVE_LETTER_OFFSET_MS,
  WAVING_STATUS_ARMS,
} from "../../src/breathing.js";
import {
  IGNORED_EXPIRY,
  harness,
  quietActivity,
  strip,
  type Harness,
  type StripInit,
} from "./harness.js";
import { createStopControls } from "../../src/footer/stop.js";
import { statusWords } from "../../src/footer/parts.js";

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
  const row = drawFooterStrip(strip(init), { ctx: h.ctx, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY,
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
 * A status arm built by NAME, carrying the arm's quietest legal activity
 * unless VALUE states one (an explicit `activity: undefined` leaves it unset).
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
  const full = "activity" in value ? value : { ...value, activity: quietActivity(statusCase) };
  return create(FooterStatusSchema, {
    status: { case: statusCase, value: full },
  } as never).status;
}

/**
 * ARM with its first declared substatus set, so an arm whose substatus the
 * contract requires (merge failed's area) is drawn legally by a table that
 * walks every arm. VALUE is merged in as `status` takes it.
 */
function armStatus(arm: string, value: Record<string, unknown> = {}): FooterStatus["status"] {
  const firstSub = SUBSTATUS_PAIRS.find(([statusCase]) => statusCase === arm)?.[1];
  return firstSub === undefined
    ? status(arm, value)
    : status(arm, { ...value, substatus: { case: firstSub, value: {} } });
}

/** A status arm with SUB set. */
function withSubStatus(
  statusCase: string,
  subCase: string,
  subValue: Record<string, unknown> = {},
): FooterStatus["status"] {
  return status(statusCase, { substatus: { case: subCase, value: subValue } });
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

  it("paints the status cell with the vocabulary's tone class", () => {
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });
    expect(row.querySelector(".footer-status")?.classList.contains("tone-red")).toBe(true);
  });

  it("carries the arm as a class so an arm can be styled on its own", () => {
    const { row } = drawStrip({ status: withSubStatus("merging", "rebasing", { replayed: 1, total: 2 }) });
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
      drawFooterStrip(bare, { ctx: h.ctx, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY, selection: null, onSelect: () => {} }),
    ).toThrow(MalformedView);
  });
});

// ---- the activity cell ------------------------------------------------------

describe("drawFooterStatus: the activity cell every arm carries", () => {
  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))("draws the %s arm's activity cell", (arm) => {
    const { row } = drawStrip({ status: armStatus(arm) });
    expect(row.querySelector(".footer-activity")).not.toBeNull();
  });

  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))(
    "refuses a %s push with no activity — the schema always sets one",
    (arm) => {
      expect(() => drawStrip({ status: armStatus(arm, { activity: undefined }) })).toThrow(MalformedView);
    },
  );

  it("keeps the activity cell as the grow cell that owns the strip's slack", () => {
    const { row } = drawStrip();
    expect(row.querySelector(".footer-activity")?.classList.contains("pfooter-grow")).toBe(true);
  });
});

describe("footerStatusActivity", () => {
  it("hands over the cell the arm carries", () => {
    const u = create(FooterStatusSchema, { status: status("background", {}) });
    expect(footerStatusActivity(u).$typeName).toBe("frontend.v1.FooterStatusBackgroundActivity");
  });

  it("refuses an arm with no activity", () => {
    const u = create(FooterStatusSchema, { status: status("background", { activity: undefined }) });
    expect(() => footerStatusActivity(u)).toThrow(MalformedView);
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
    ["merging", "conflictResolution", "conflict resolution"],
    ["merging", "updatingMain", "updating main"],
    ["merging", "postprocessing", "postprocessing"],
    ["mergeFailed", "conflicts", "conflicts"],
    ["mergeFailed", "tests", "tests"],
    ["mergeFailed", "other", "merge"],
    ["disconnected", "startFailed", "start failed"],
    ["interrupted", "byUser", "by user"],
    ["interrupted", "hostShutdown", "host shutdown"],
    ["blocked", "queryDied", "query died"],
    // Every step of a working turn reads as its own name.
    ["working", "thinking", "thinking"],
    ["working", "executing", "executing"],
    ["working", "reading", "reading"],
    ["working", "writing", "writing"],
    ["working", "searching", "searching"],
    ["working", "fetching", "fetching"],
    ["working", "delegating", "delegating"],
  ])("spells %s/%s as '%s'", (statusCase, subCase, expected) => {
    expect(subStatusWords(statusCase, subCase)).toBe(expected);
  });

  it("names the arm on the cell", () => {
    const { row } = drawStrip({ status: withSubStatus("blocked", "auth") });
    expect(row.querySelector(".footer-substatus")?.getAttribute("data-arm")).toBe("auth");
  });

  it.each([
    ["enqueued", { place: 2, waiting: 5 }, "enqueued 2/5"],
    ["preprocessing", {}, "preprocessing"],
    ["rebasing", { replayed: 3, total: 7 }, "rebasing 3/7"],
    ["conflictResolution", {}, "conflict resolution"],
    ["testing", {}, "testing"],
    ["fixing", { attempt: 2, maxAttempts: 3 }, "fixing attempt 2/3"],
    ["committing", {}, "committing"],
    ["updatingMain", {}, "updating main"],
    ["postprocessing", {}, "postprocessing"],
  ])("draws the merging %s step as '%s'", (subCase, value, words) => {
    const { row } = drawStrip({ status: withSubStatus("merging", subCase, value) });
    expect(row.querySelector(".footer-substatus")?.textContent).toBe(words);
  });

  it("colours the enqueued place as a queue position", () => {
    const { row } = drawStrip({ status: withSubStatus("merging", "enqueued", { place: 2, waiting: 5 }) });
    expect(row.querySelector('.footer-substatus [data-datum="position"]')?.textContent).toBe("2/5");
  });

  it("colours the rebase progress as a count", () => {
    const { row } = drawStrip({ status: withSubStatus("merging", "rebasing", { replayed: 3, total: 7 }) });
    expect(row.querySelector('.footer-substatus [data-datum="count"]')?.textContent).toBe("3/7");
  });

  it("colours the fixing attempt as an attempt", () => {
    const { row } = drawStrip({ status: withSubStatus("merging", "fixing", { attempt: 2, maxAttempts: 3 }) });
    expect(row.querySelector('.footer-substatus [data-datum="attempt"]')?.textContent).toBe("2/3");
  });

  it.each([
    ["conflicts", "conflicts"],
    ["tests", "tests"],
    ["other", "merge"],
  ])("draws merge failed's %s area as '%s'", (subCase, words) => {
    const { row } = drawStrip({ status: withSubStatus("mergeFailed", subCase) });
    expect(row.querySelector(".footer-substatus")?.textContent).toBe(words);
  });

  it("words the merge failed status 'merge failed'", () => {
    const { row } = drawStrip({ status: withSubStatus("mergeFailed", "tests") });
    expect(row.querySelector(".footer-status")?.textContent).toBe("merge failed");
  });

  it("paints the merge failed status turquoise", () => {
    const { row } = drawStrip({ status: withSubStatus("mergeFailed", "conflicts") });
    expect(row.querySelector(".footer-status")?.classList.contains("tone-turquoise")).toBe(true);
  });

  it("refuses a merge failed push that names no area", () => {
    expect(() => drawStrip({ status: status("mergeFailed", {}) })).toThrow(MalformedView);
  });

  it("MERGES the cell for the merged arm, which declares no substatus oneof", () => {
    const { row } = drawStrip({ status: status("merged", {}) });
    expect(row.querySelector(".footer-status")?.getAttribute("data-merged")).toBe("true");
  });

  it("words a landed merge 'merged' in tone-green", () => {
    const { row } = drawStrip({ status: status("merged", {}) });
    const cell = row.querySelector(".footer-status");
    expect([cell?.textContent, cell?.classList.contains("tone-green")]).toEqual(["merged", true]);
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

  it("reads the nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the turn's start does not share the shared ticker's phase.
    const { row } = drawStrip({ turnStartedAtMs: BigInt(NOW - 4920) });
    // Assert: five real seconds of turn reads 5s, not the lagging 4s.
    expect(row.querySelector(".footer-clock .info-time")?.textContent).toBe("5s");
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
      drawFooterStrip(strip({ tokens: { input: { text: "x" }, verdict: {} } }), { ctx: h.ctx, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY,
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

  it("colors a figure with no heat not at all", () => {
    const { row } = drawStrip({ tokens: { input: { text: "--" } } });
    const input = row.querySelector<HTMLElement>(".footer-tokens-input");
    expect(input?.hasAttribute("data-heat")).toBe(false);
    expect(input?.style.color).toBe("");
  });

  it("colors the figure from its heat", () => {
    const { row } = drawStrip({ tokens: { input: { text: "40k in", heat: { position: 0.5 } } } });
    const input = row.querySelector<HTMLElement>(".footer-tokens-input");
    expect(input?.getAttribute("data-heat")).toBe("0.5");
    expect(input?.style.color).toContain("--token-heat-1");
  });

  it("refuses a heat outside the gradient", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(strip({ tokens: { input: { text: "x", heat: { position: 1.5 } } } }), { ctx: h.ctx, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY,
        selection: null,
        onSelect: () => {},
      }),
    ).toThrow(MalformedView);
  });

  it("draws the idle figure the daemon states, verbatim", () => {
    const { row } = drawStrip({ tokens: { input: { text: "--" } } });
    expect(row.querySelector(".footer-tokens-input")?.textContent).toBe("--");
  });

  it("still selects the tokens panel when the idle figure is drawn", () => {
    const { row, selected } = drawStrip({ tokens: { input: { text: "--" } } });
    row.querySelector<HTMLElement>(".footer-tokens")?.dispatchEvent(new MouseEvent("click"));
    expect(selected).toEqual(["tokens"]);
  });

  it("marks the cell while its panel is the open one", () => {
    const { row } = drawStrip({}, "tokens");
    expect(row.querySelector(".footer-tokens")?.getAttribute("data-selected")).toBe("true");
  });
});

// ---- the live-work chips ---------------------------------------------------

describe("drawFooterChipAgents: the waiting-for-the-API glyph", () => {
  it("draws no waiting glyph while no agent waits for the API", () => {
    const { row } = drawStrip({ liveWork: { agents: { count: 2 } } });
    expect(row.querySelector('[data-glyph="waitingForApi"]')).toBeNull();
  });

  it("draws the waiting glyph with its count beside the chip's count", () => {
    const { row } = drawStrip({ liveWork: { agents: { count: 3, waitingForApi: { count: 1 } } } });
    expect(row.querySelector('[data-chip="agents"]')?.textContent).toBe(`⚙ 3 ${WAITING_FOR_API_GLYPH} 1`);
  });

  it("carries the waiting count on the glyph's holder", () => {
    const { row } = drawStrip({ liveWork: { agents: { count: 3, waitingForApi: { count: 2 } } } });
    expect(row.querySelector(".footer-chip-waiting")?.getAttribute("data-waiting-for-api")).toBe("2");
  });

  it("says what the glyph counts on hover", () => {
    const { row } = drawStrip({ liveWork: { agents: { count: 3, waitingForApi: { count: 2 } } } });
    expect(row.querySelector<HTMLElement>(".footer-chip-waiting")?.title).toBe("2 waiting for the API");
  });
});

describe("chipGlyph", () => {
  it("draws the character named by its glyph", () => {
    const mark = chipGlyph("agents", "⚙");
    expect([mark.textContent, mark.getAttribute("data-glyph")]).toEqual(["⚙", "agents"]);
  });

  it("hides the glyph from assistive tech", () => {
    expect(chipGlyph("agents", "⚙").getAttribute("aria-hidden")).toBe("true");
  });
});

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

// ---- arms a newer daemon set that this bundle cannot draw -------------------
//
// Each of these is the run-time half of an exhaustive switch: the compiler
// cannot see an arm whose descriptor this bundle does not carry, so the
// `default` refuses the frame instead of drawing something else.

describe("an arm this build has no case for", () => {
  it("refuses a STATUS arm the bundle cannot name", () => {
    // ARRANGE — a legal frame, then the arm a NEWER daemon set, poked in: the
    // generated `create` drops a case its descriptors do not know, which would
    // trip the unset-oneof refusal instead of the exhaustive switch's default.
    const h = harness();
    const view = strip();
    (view.status as unknown as { status: { case: string; value: unknown } }).status = {
      case: "hibernated",
      value: {},
    };
    // ACT / ASSERT
    expect(() =>
      drawFooterStrip(view, { ctx: h.ctx, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY, selection: null, onSelect: () => {} }),
    ).toThrow(MalformedView);
  });

  it("refuses a TOKENS VERDICT arm the bundle cannot name", () => {
    const h = harness();
    const view = strip({
      tokens: { input: { text: "x" }, verdict: { verdict: { case: "complete", value: {} } } },
    });
    (
      view.tokens as unknown as { verdict: { verdict: { case: string; value: unknown } } }
    ).verdict.verdict = { case: "unaudited", value: {} };
    expect(() =>
      drawFooterStrip(view, { ctx: h.ctx, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY, selection: null, onSelect: () => {} }),
    ).toThrow(MalformedView);
  });
});

describe("drawClientDisconnectedStrip: the one strip this client composes", () => {
  it("draws the status word disconnected", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable");
    expect(row.querySelector(".footer-status")?.textContent).toBe("disconnected");
  });

  it("marks the status cell with the disconnected arm, as a pushed strip does", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable");
    expect(row.querySelector(".footer-status")?.getAttribute("data-arm")).toBe("disconnected");
  });

  it("paints the status cell the disconnected tone from the shared vocabulary", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable");
    expect(row.querySelector(".footer-status")?.className).toContain(statusArmClass("disconnected"));
  });

  it("draws the substatus it was handed", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable");
    expect(row.querySelector(".footer-substatus")?.textContent).toBe("daemon unreachable");
  });

  it("draws the ad-hoc activity line it was handed", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable");
    expect(row.querySelector(".footer-activity-client-verdict")?.textContent).toBe(
      "AnswerColdGate: unavailable",
    );
  });

  it("hovers the whole line, which the elastic cell ellipsizes", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "a very long ad-hoc line");
    expect(row.querySelector<HTMLElement>(".footer-activity")?.title).toBe(
      "a very long ad-hoc line",
    );
  });

  it("draws NO clock, tokens or chips when nothing was ever pushed", () => {
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable");
    expect(row.querySelectorAll(".footer-clock, .footer-tokens, .footer-chips")).toHaveLength(0);
  });

  it("keeps the last pushed tokens cell, the figures being the last ones and not untrue", () => {
    const h = harness();
    const row = drawClientDisconnectedStrip("daemon unreachable", "AnswerColdGate: unavailable", {
      strip: strip({ tokens: { input: { text: "12.3k in" } } }),
      deps: { ctx: h.ctx, selection: null, onSelect: () => {}, stops: createStopControls(h.ctx), expiry: IGNORED_EXPIRY },
    });
    expect(row.querySelector(".footer-tokens")?.textContent).toContain("12.3k in");
  });
});

// ---- the status word's letter wave ----------------------------------------

describe("the footer status word's letter wave", () => {
  /** The delay each `.pfooter-wave-letter` of the drawn status word renders with. */
  function letterDelays(row: HTMLElement): number[] {
    return [...row.querySelectorAll(".footer-status .pfooter-wave-letter")].map((letter) => {
      const got = /animation-delay:\s*-(\d+)ms/.exec(letter.getAttribute("style") ?? "");
      if (got === null) throw new Error(`no negative delay on ${letter.outerHTML}`);
      return Number(got[1]);
    });
  }

  /** The arm's fixture, with its first substatus where the arm declares one. */
  function armStatus(arm: string): FooterStatus["status"] {
    const sub = SUBSTATUS_PAIRS.find(([statusCase]) => statusCase === arm)?.[1];
    return sub === undefined ? status(arm, {}) : withSubStatus(arm, sub);
  }

  it.each([...WAVING_STATUS_ARMS].map((arm) => [arm]))(
    "splits the %s word into one span per letter",
    (arm) => {
      // Arrange / Act
      const { row } = drawStrip({ status: armStatus(arm) });

      // Assert
      expect(letterDelays(row)).toHaveLength(statusWords(arm).length);
    },
  );

  it("waves from the submitting phase, not only once the first activity lands", () => {
    // Arrange / Act — the daemon publishes `working · submitting` the moment a
    // prompt is accepted (before the shim answers), so the wave must be present
    // under the submitting substatus, not deferred to `working · thinking`.
    const { row } = drawStrip({ status: withSubStatus("working", "submitting") });

    // Assert — the word is split into waving letters straight away.
    expect(letterDelays(row).length).toBeGreaterThan(0);
    expect(row.querySelector(".footer-status")?.getAttribute("data-status-wave")).toBe("progress");
  });

  it("leaves the split word reading exactly as the arm's own word", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });

    // Assert
    expect(row.querySelector(".footer-status")?.textContent).toBe("working");
  });

  it("marks the waving cell as a progress status", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });

    // Assert
    expect(row.querySelector(".footer-status")?.getAttribute("data-status-wave")).toBe("progress");
  });

  it("advances the delay by one letter offset from each letter to the next", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });

    // Assert — modular, because the cycle can wrap between any two letters.
    const delays = letterDelays(row);
    const steps = delays.slice(1).map((delay, index) => {
      const previous = delays[index] ?? 0;
      return (delay - previous + STATUS_WAVE_CYCLE_MS) % STATUS_WAVE_CYCLE_MS;
    });
    expect(steps).toEqual(steps.map(() => STATUS_WAVE_LETTER_OFFSET_MS));
  });

  it("continues the phase across a redraw rather than restarting at zero", () => {
    // Arrange
    const before = letterDelays(drawStrip({ status: withSubStatus("working", "thinking") }).row);
    vi.setSystemTime(NOW + 900);

    // Act
    const after = letterDelays(drawStrip({ status: withSubStatus("working", "thinking") }).row);

    // Assert — the redraw is 900ms further along the same cycle, not back at 0.
    expect(after[0]).toBe(((before[0] ?? 0) + 900) % STATUS_WAVE_CYCLE_MS);
  });

  it("keeps the redrawn wave off the cycle's start, so no push reads as a stutter", () => {
    // Arrange
    drawStrip({ status: withSubStatus("working", "thinking") });
    vi.setSystemTime(NOW + 900);

    // Act
    const after = letterDelays(drawStrip({ status: withSubStatus("working", "thinking") }).row);

    // Assert
    expect(after[0]).not.toBe(0);
  });

  it.each(
    FOOTER_STATUS_CASES.filter((arm) => !WAVING_STATUS_ARMS.has(arm)).map((arm) => [arm]),
  )("draws the %s word as plain text, with no letter spans", (arm) => {
    // Arrange / Act
    const { row } = drawStrip({ status: armStatus(arm) });

    // Assert
    expect(row.querySelectorAll(".footer-status .pfooter-wave-letter")).toHaveLength(0);
  });

  it("draws the client's own disconnected verdict as plain text too", () => {
    // Arrange / Act
    const row = drawClientDisconnectedStrip("daemon unreachable", "a line");

    // Assert
    expect(row.querySelectorAll(".pfooter-wave-letter")).toHaveLength(0);
  });
});

// ---- the status word's colour sweep (the blanking-bug regression lock) -----

describe("the footer status word's per-letter colour sweep", () => {
  /** Every drawn `.pfooter-wave-letter` of the status word. */
  function statusLetters(row: HTMLElement): HTMLElement[] {
    return [...row.querySelectorAll<HTMLElement>(".footer-status .pfooter-wave-letter")];
  }

  it("gives each thinking letter a colour that is not transparent", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });

    // Assert — never the transparent fill that blanked the word before.
    for (const letter of statusLetters(row)) {
      const colour = getComputedStyle(letter).color;
      expect(colour).not.toBe("transparent");
      expect(colour).not.toBe("rgba(0, 0, 0, 0)");
    }
  });

  it("sets no clipped-gradient or transparent fill on any thinking letter", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });

    // Assert — the exact properties whose combination blanked the word.
    for (const letter of statusLetters(row)) {
      const style = letter.getAttribute("style") ?? "";
      expect(style).not.toMatch(/background-clip:\s*text/);
      expect(style).not.toMatch(/-webkit-text-fill-color:\s*transparent/);
      expect(style).not.toMatch(/color:\s*transparent/);
    }
  });

  it("sets no clipped-gradient or transparent fill on the word holder either", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("working", "thinking") });
    const holder = row.querySelector(".footer-status .pfooter-status-word");
    const style = holder?.getAttribute("style") ?? "";

    // Assert — the holder is where the reverted gradient rode; it must be clean.
    expect(style).not.toMatch(/background-clip:\s*text/);
    expect(style).not.toMatch(/-webkit-text-fill-color:\s*transparent/);
    expect(style).not.toMatch(/color:\s*transparent/);
  });

  it("carries no colour-swept letters at all under a non-progress arm", () => {
    // Arrange — a settled status stands still and hosts no colour sweep.
    const { row } = drawStrip({ status: status("idle", {}) });

    // Assert — the sweep lives only on `.pfooter-wave-letter`, absent here.
    expect(statusLetters(row)).toHaveLength(0);
  });
});

