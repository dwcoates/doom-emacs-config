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
  statusArmClass,
} from "../../src/footer/tones.js";
import {
  IDLE_CLOCK_LABEL,
  drawClientDisconnectedStrip,
  drawFooterStrip,
  footerTokensHeatColor,
  statusWords,
  subStatusWords,
} from "../../src/footer/strip.js";
import type { FooterPanel } from "../../src/footer/expanded.js";
import {
  STATUS_WAVE_CYCLE_MS,
  STATUS_WAVE_LETTER_OFFSET_MS,
  WAVING_STATUS_ARMS,
} from "../../src/breathing.js";
import { harness, strip, type Harness, type StripInit } from "./harness.js";
import { createStopControls } from "../../src/footer/stop.js";

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
  const row = drawFooterStrip(strip(init), { ctx: h.ctx, stops: createStopControls(h.ctx),
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
      drawFooterStrip(bare, { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} }),
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
    // The running step of a thinking turn reads "working", never "thinking
    // · thinking".
    ["thinking", "thinking", "working"],
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

  it("draws a parked merge_conflict's composed line", () => {
    const { row } = drawStrip({
      status: withSubStatus("mergeConflict", "parked", { line: "waiting on your call" }),
    });
    expect(row.querySelector(".footer-substatus-line")?.textContent).toBe("waiting on your call");
  });

  it("MERGES the cell for a merge_conflict stopped on a conflict with no finer step", () => {
    const { row } = drawStrip({ status: status("mergeConflict", {}) });
    expect(row.querySelector(".footer-substatus")).toBeNull();
  });

  it.each([["mergeFailed"], ["merged"]])(
    "MERGES the cell for the %s arm, which declares no substatus oneof",
    (arm) => {
      const { row } = drawStrip({ status: status(arm, {}) });
      expect(row.querySelector(".footer-status")?.getAttribute("data-merged")).toBe("true");
    },
  );

  it.each([
    ["mergeConflict", "merge conflict", "tone-green"],
    ["mergeFailed", "merge failed", "tone-blue"],
    ["merged", "merged", "tone-green"],
  ])("words a stopped merge's %s arm '%s' in %s", (arm, word, tone) => {
    const { row } = drawStrip({ status: status(arm, {}) });
    const cell = row.querySelector(".footer-status");
    expect(cell?.textContent).toBe(word);
    expect(cell?.classList.contains(tone)).toBe(true);
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
  it("draws no activity cell at all when the arm legitimately has none", () => {
    const { row } = drawStrip({ status: status("idle", {}) });
    // Absence means draw nothing: the grow cell stays (it owns the strip's
    // slack and the grabber notch) but is not an activity cell.
    expect(row.querySelector(".footer-activity")).toBeNull();
  });

  it("still keeps the grow cell that owns the strip's slack", () => {
    const { row } = drawStrip({ status: status("idle", {}) });
    expect(row.querySelector(".pfooter-grow")).not.toBeNull();
  });

  it("refuses a WAITING push with no activity — the schema requires one", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({ status: status("waiting", { substatus: { case: "permission", value: {} } }) }),
        { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} },
      ),
    ).toThrow(MalformedView);
  });

  it("refuses a LOADING push with no activity", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({ status: status("loading", { substatus: { case: "memory", value: {} } }) }),
        { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} },
      ),
    ).toThrow(MalformedView);
  });

  it("refuses an activity whose kind oneof sets no arm", () => {
    const h = harness();
    expect(() =>
      drawFooterStrip(
        strip({ status: status("idle", { activity: { at: { atMs: BigInt(NOW) } } }) }),
        { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} },
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
        { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} },
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
    ["thinking", "compacting", "compaction", { text: "compacting · 412 of 900 messages" }, "compacting · 412 of 900 messages"],
    ["thinking", "thinking", "contextInjected", { text: "webapp/CLAUDE.md" }, "webapp/CLAUDE.md"],
    ["blocked", "auth", "authenticating", { line: "open the login" }, "open the login"],
    ["blocked", "queryDied", "queryDied", { text: "the next prompt restarts it" }, "the next prompt restarts it"],
    // A dead query is a FAILED TURN (owner ruling, 2026-09-28): its line
    // stands under `idle · turn failed`.
    ["idle", "turnFailed", "queryDied", { text: "the next prompt restarts it" }, "the next prompt restarts it"],
    ["closing", "blocked", "closeBlocked", { text: "a turn is in flight" }, "a turn is in flight"],
    ["disconnected", "startFailed", "fault", { kind: "resume_failed", detail: "the shim refused" }, "resume failed \u00b7 the shim refused"],
    ["blocked", "daemonImpaired", "fault", { kind: "prompts_dir_missing", detail: "no ~/.claude/prompts" }, "prompts dir missing \u00b7 no ~/.claude/prompts"],
    ["idle", null, "fault", { kind: "conversation_abandoned", detail: "no transcript on disk" }, "conversation abandoned \u00b7 no transcript on disk"],
    ["thinking", "thinking", "fault", { kind: "classifier_failed", detail: "the run died" }, "classifier failed \u00b7 the run died"],
  ])("draws the %s/%s %s line verbatim", (statusCase, subCase, kindCase, value, expected) => {
    const { row } = drawStrip({
      status: withActivity(statusCase, subCase, kindCase, value),
    });
    expect(row.querySelector(".footer-activity")?.textContent).toContain(expected);
  });

  it("draws the bring-up failure's cause verbatim", () => {
    const { row } = drawStrip({
      status: withActivity("disconnected", "startFailed", "startFailed", {
        detail: "exit 1: Cannot find module",
        droppedPrompts: 0,
      }),
    });
    expect(row.querySelector(".footer-activity")?.textContent).toContain(
      "exit 1: Cannot find module",
    );
  });

  it("says nothing about dropped prompts when the failure dropped none", () => {
    const { row } = drawStrip({
      status: withActivity("disconnected", "startFailed", "startFailed", {
        detail: "exit 1: Cannot find module",
        droppedPrompts: 0,
      }),
    });
    expect(row.querySelector(".footer-activity")?.textContent).not.toContain("dropped");
  });

  it("names the ONE held prompt a bring-up failure dropped in the singular", () => {
    const { row } = drawStrip({
      status: withActivity("disconnected", "startFailed", "startFailed", {
        detail: "exit 1: Cannot find module",
        droppedPrompts: 1,
      }),
    });
    expect(row.querySelector(".footer-activity")?.textContent).toContain(
      "· 1 held prompt dropped",
    );
  });

  it("counts the held prompts a bring-up failure dropped in the plural", () => {
    const { row } = drawStrip({
      status: withActivity("disconnected", "startFailed", "startFailed", {
        detail: "exit 1: Cannot find module",
        droppedPrompts: 3,
      }),
    });
    expect(row.querySelector(".footer-activity")?.textContent).toContain(
      "· 3 held prompts dropped",
    );
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

  // THE OVERAGE WINDOW STAYS OFF THE STRIP while either of the pair is
  // figured: a third figure on a line that already loses its second to the
  // cut would push the pair a reader needs off the glass. The tokens sheet
  // draws it instead.
  it("keeps the overage window off the strip beside the two it draws", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: true, utilization: 0.72, resetsAtS: BigInt((NOW + 3_900_000) / 1000) },
        weekly: { newsworthy: false, utilization: 0.31, resetsAtS: BigInt((NOW + 259_200_000) / 1000) },
        overage: { newsworthy: true, utilization: 0.91, resetsAtS: BigInt((NOW + 7_200_000) / 1000) },
      }),
    });
    expect(row.querySelector('[data-allowance="overage"]')).toBeNull();
  });

  // AN OVERAGE EVENT CAN LAND BEFORE THE FIRST USAGE SAMPLE, leaving neither
  // of the pair figured. The line the daemon opened would then have nothing
  // in it, so the overage window takes the slot rather than the strip drawing
  // a blank.
  it("draws the overage window when it is the only one figured", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        overage: { newsworthy: true, utilization: 0.91, resetsAtS: BigInt((NOW + 7_200_000) / 1000) },
      }),
    });
    expect(row.querySelector(".footer-activity-rate-limited")?.textContent).toBe(
      "overage 91% · resets in 2h",
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

  // ---- The last-read age: the strip renders the figures LAST READ and how
  // long ago they were read, and no longer a "usage unread" caveat.

  /** One rate-limit line whose figures were read `agoMs` ago. */
  function ageLine(agoMs: number | null): HTMLElement {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: true, utilization: 0.41, resetsAtS: BigInt((NOW + 3_900_000) / 1000) },
        ...(agoMs === null ? {} : { figuresReadAtMs: BigInt(NOW - agoMs) }),
      }),
    });
    return row;
  }

  // THE OWNER'S SHAPE: "<usage figures> 10m 30s ago", ticking from the shipped
  // read instant.
  it("renders the age of the last usage reading beside the figures", () => {
    expect(ageLine(630_000).querySelector(".footer-rate-age")?.textContent).toBe(" · 10m 30s ago");
  });

  // IT TICKS ON THE SHARED CLOCK, like every other footer duration.
  it("re-reads the usage read-age on the shared tick", () => {
    const row = ageLine(630_000);
    vi.advanceTimersByTime(1000);
    expect(row.querySelector(".footer-rate-age")?.textContent).toBe(" · 10m 31s ago");
  });

  // NO READ INSTANT, NO AGE: figures from a rate-limit event carry none, and
  // the strip draws them with no age rather than inventing one.
  it("draws no read-age when the figures carry no read instant", () => {
    expect(ageLine(null).querySelector(".footer-rate-age")).toBeNull();
  });

  // THE UNREAD MESSAGE IS GONE: no sample outcome ever draws a cell now.
  it("draws no usage-unread cell any more", () => {
    const row = ageLine(630_000);
    expect(row.querySelector(".footer-allowance-unread")).toBeNull();
    expect(row.querySelector(".footer-activity-rate-limited")?.textContent).not.toContain(
      "usage unread",
    );
  });
  // THE NEWSWORTHY WINDOW LEADS: it is the figure that changes what the reader
  // does, so it is the half of the line that survives the cut.
  it("draws the newsworthy window first even when it is the weekly one", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: false, utilization: 0.31, resetsAtS: BigInt(NOW / 1000) },
        weekly: { newsworthy: true, utilization: 0.91, resetsAtS: BigInt(NOW / 1000) },
      }),
    });
    expect(
      row.querySelector(".footer-rate-figures")?.firstElementChild?.getAttribute("data-allowance"),
    ).toBe("weekly");
  });

  // THE CELL'S HOVER IS THE WHOLE LINE. The cell ellipsizes by design, so the
  // title is what a reader who cannot open the sheet still has.
  it("titles the activity cell with the full line it may be cutting", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: true, utilization: 0.82, resetsAtS: BigInt((NOW + 3_540_000) / 1000) },
        weekly: { newsworthy: false, utilization: 0.63, resetsAtS: BigInt((NOW + 259_200_000) / 1000) },
        figuresReadAtMs: BigInt(NOW - 630_000),
      }),
    });
    const cell = row.querySelector<HTMLElement>(".footer-activity");
    expect(cell?.title).toBe(row.querySelector(".footer-activity-rate-limited")?.textContent);
  });

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

  /** The same line with the session allowance carrying NO vendor verdict. */
  function unverdictedRow(): HTMLElement {
    const { row } = drawStrip({
      status: withActivity("idle", null, "rateLimited", {
        session: { newsworthy: true, utilization: 0.5, resetsAtS: BigInt(NOW / 1000) },
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

  it("draws the figures of an allowance the vendor has not yet ruled on", () => {
    expect(
      unverdictedRow().querySelector('[data-allowance="session"]')?.textContent,
    ).toContain("50%");
  });

  it("marks no arm on an allowance with no vendor verdict yet", () => {
    expect(
      unverdictedRow().querySelector('[data-allowance="session"]')?.hasAttribute("data-arm"),
    ).toBe(false);
  });

  it("paints no verdict colour before the vendor has given one", () => {
    expect(unverdictedRow().querySelector('[data-allowance="session"]')?.className).toBe(
      "footer-allowance footer-allowance-newsworthy",
    );
  });

  it("titles nothing on an allowance with no vendor verdict yet", () => {
    expect(
      unverdictedRow().querySelector<HTMLElement>('[data-allowance="session"]')?.title,
    ).toBe("");
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

  it("reads the nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the stamp does not share the shared ticker's phase.
    const { row } = drawStrip({
      status: withActivity("idle", null, "notification", { text: "done" }, BigInt(NOW - 4920)),
    });
    // Assert: five real seconds old reads 5s, not the lagging 4s.
    expect(row.querySelector("[data-age]")?.textContent).toBe(" \u00b7 5s ago");
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
      drawFooterStrip(strip({ tokens: { input: { text: "x" }, verdict: {} } }), { ctx: h.ctx, stops: createStopControls(h.ctx),
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
      drawFooterStrip(strip({ tokens: { input: { text: "x", heat: { position: 1.5 } } } }), { ctx: h.ctx, stops: createStopControls(h.ctx),
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
      drawFooterStrip(view, { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} }),
    ).toThrow(MalformedView);
  });

  it("refuses an ACTIVITY KIND the bundle cannot name", () => {
    const h = harness();
    const view = strip({
      status: withActivity("idle", null, "notification", { text: "a line" }),
    });
    const activity = (
      view.status as unknown as {
        status: { value: { activity: { kind: { case: string; value: unknown } } } };
      }
    ).status.value.activity;
    activity.kind = { case: "teleporting", value: {} };
    expect(() =>
      drawFooterStrip(view, { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} }),
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
      drawFooterStrip(view, { ctx: h.ctx, stops: createStopControls(h.ctx), selection: null, onSelect: () => {} }),
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
      deps: { ctx: h.ctx, selection: null, onSelect: () => {}, stops: createStopControls(h.ctx) },
    });
    expect(row.querySelector(".footer-tokens")?.textContent).toContain("12.3k in");
  });
});

// ---- the standing daemon fault, on every status arm ------------------------

describe("the fault activity: every daemon fault kind reaches the strip", () => {
  it("draws the kind lowercase with spaces, never underscores", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "fault", {
        kind: "watch_open_refused",
        detail: "handle 7",
      }),
    });
    expect(row.querySelector(".footer-activity")?.textContent).not.toContain("_");
  });

  it("draws the kind alone when the fault carries no detail", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "fault", { kind: "session_absent", detail: "" }),
    });
    expect(row.querySelector(".footer-activity-fault")?.textContent).toBe("session absent");
  });

  it("draws it in the fault's own cell class, beside the bring-up failure's", () => {
    const { row } = drawStrip({
      status: withActivity("disconnected", "dead", "fault", { kind: "shim_died", detail: "exit 1" }),
    });
    expect(row.querySelector(".footer-activity-fault")).not.toBeNull();
  });

  it.each([
    ["idle", null],
    ["thinking", "thinking"],
    ["waiting", "permission"],
    ["interrupted", "byUser"],
    ["merging", "merge"],
    ["background", null],
    ["blocked", "daemonImpaired"],
    ["disconnected", "dead"],
    ["closing", "blocked"],
    ["loading", "memory"],
    ["mergeConflict", "parked"],
    ["mergeFailed", null],
    ["merged", null],
  ])("stands under the %s arm", (statusCase, subCase) => {
    const { row } = drawStrip({
      status: withActivity(statusCase, subCase, "fault", {
        kind: "shim_reported",
        detail: "the shim said so",
      }),
    });
    expect(row.querySelector(".footer-activity-fault")?.textContent).toBe(
      "shim reported \u00b7 the shim said so",
    );
  });

  // THE TURN THAT CONCLUDED WITH NO GREEN ANSWER. The kind is new, the
  // rendering is not: the chip holds no table keyed on the kind, so
  // `final_answer_unresolved` draws through the same line every other fault
  // kind draws through, with the daemon's terse detail beside it.
  it.each([
    ["the turn named no answering response"],
    ["the named answer has no drawn row"],
    ["no response frame for 1m30s"],
  ])("draws the unresolved final answer with its detail %s", (detail) => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "fault", {
        kind: "final_answer_unresolved",
        detail,
      }),
    });
    expect(row.querySelector(".footer-activity-fault")?.textContent).toBe(
      `final answer unresolved \u00b7 ${detail}`,
    );
  });

  // A FAILED DEPLOY. The daemon names the step and its last line of output;
  // the strip draws it through the same line, with no table keyed on it.
  it("draws a failed deploy with the step it failed at", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "fault", {
        kind: "deploy_failed",
        detail: "build webapp: error TS2322",
      }),
    });
    expect(row.querySelector(".footer-activity-fault")?.textContent).toBe(
      "deploy failed \u00b7 build webapp: error TS2322",
    );
  });

  it("leaves the idle status standing under a failed deploy", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "fault", {
        kind: "deploy_failed",
        detail: "restart services store: exit 5",
      }),
    });
    expect(row.querySelector(".footer-status")?.textContent?.toLowerCase()).toContain("idle");
  });

  // IT NEVER ESCALATES THE STATUS. The session is serving and the prose is on
  // screen; `disconnected` would close the composer over a healthy session.
  it("stands as the activity line under an ordinary idle status", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "fault", {
        kind: "final_answer_unresolved",
        detail: "the named answer has no drawn row",
      }),
    });
    expect(row.querySelector(".footer-status")?.textContent?.toLowerCase()).toContain("idle");
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
    // Arrange / Act — the daemon publishes `thinking · submitting` the moment a
    // prompt is accepted (before the shim answers), so the wave must be present
    // under the submitting substatus, not deferred to `thinking · thinking`.
    const { row } = drawStrip({ status: withSubStatus("thinking", "submitting") });

    // Assert — the word is split into waving letters straight away.
    expect(letterDelays(row).length).toBeGreaterThan(0);
    expect(row.querySelector(".footer-status")?.getAttribute("data-status-wave")).toBe("progress");
  });

  it("leaves the split word reading exactly as the arm's own word", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });

    // Assert
    expect(row.querySelector(".footer-status")?.textContent).toBe("thinking");
  });

  it("marks the waving cell as a progress status", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });

    // Assert
    expect(row.querySelector(".footer-status")?.getAttribute("data-status-wave")).toBe("progress");
  });

  it("advances the delay by one letter offset from each letter to the next", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });

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
    const before = letterDelays(drawStrip({ status: withSubStatus("thinking", "thinking") }).row);
    vi.setSystemTime(NOW + 900);

    // Act
    const after = letterDelays(drawStrip({ status: withSubStatus("thinking", "thinking") }).row);

    // Assert — the redraw is 900ms further along the same cycle, not back at 0.
    expect(after[0]).toBe(((before[0] ?? 0) + 900) % STATUS_WAVE_CYCLE_MS);
  });

  it("keeps the redrawn wave off the cycle's start, so no push reads as a stutter", () => {
    // Arrange
    drawStrip({ status: withSubStatus("thinking", "thinking") });
    vi.setSystemTime(NOW + 900);

    // Act
    const after = letterDelays(drawStrip({ status: withSubStatus("thinking", "thinking") }).row);

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
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });

    // Assert — never the transparent fill that blanked the word before.
    for (const letter of statusLetters(row)) {
      const colour = getComputedStyle(letter).color;
      expect(colour).not.toBe("transparent");
      expect(colour).not.toBe("rgba(0, 0, 0, 0)");
    }
  });

  it("sets no clipped-gradient or transparent fill on any thinking letter", () => {
    // Arrange / Act
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });

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
    const { row } = drawStrip({ status: withSubStatus("thinking", "thinking") });
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

// ---- a deploy's progress: the update line ----------------------------------

describe("the update activity: a deploy's progress on the strip", () => {
  const updateText = (update: Record<string, unknown>): string | null | undefined => {
    const { row } = drawStrip({ status: withActivity("idle", null, "update", update) });
    return row.querySelector(".footer-activity-update")?.textContent;
  };

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
    const { row } = drawStrip({
      status: withActivity("idle", null, "update", { phase: { case: "waiting", value: { turns: 0, background: 3 } } }),
    });
    const figures = row.querySelectorAll(".footer-activity-update [data-datum='count']");
    expect([...figures].map((f) => f.textContent)).toEqual(["3"]);
  });

  it("draws the notes after the phase", () => {
    expect(
      updateText({
        phase: { case: "updated", value: {} },
        notes: [{ note: { case: "shimWhenIdle", value: {} } }],
      }),
    ).toBe("updated · shim when idle");
  });

  it("stamps the phase arm on the line", () => {
    const { row } = drawStrip({
      status: withActivity("idle", null, "update", { phase: { case: "installing", value: {} } }),
    });
    expect(row.querySelector(".footer-activity-update")?.getAttribute("data-phase")).toBe("installing");
  });

  it("refuses an update line whose phase is unset", () => {
    expect(() => drawStrip({ status: withActivity("idle", null, "update", {}) })).toThrow(MalformedView);
  });

  it("refuses a component whose arm is unset", () => {
    expect(() =>
      drawStrip({
        status: withActivity("idle", null, "update", {
          phase: { case: "building", value: { components: [{}] } },
        }),
      }),
    ).toThrow(MalformedView);
  });

  it.each([
    ["idle", null],
    ["thinking", "thinking"],
    ["waiting", "permission"],
    ["interrupted", "byUser"],
    ["merging", "merge"],
    ["background", null],
    ["blocked", "daemonImpaired"],
    ["disconnected", "dead"],
    ["closing", "blocked"],
    ["loading", "memory"],
    ["mergeConflict", "parked"],
    ["mergeFailed", null],
    ["merged", null],
  ])("stands under the %s arm", (statusCase, subCase) => {
    const { row } = drawStrip({
      status: withActivity(statusCase, subCase, "update", { phase: { case: "installing", value: {} } }),
    });
    expect(row.querySelector(".footer-activity-update")?.textContent).toBe("installing");
  });
});

describe("footerTokensHeatColor", () => {
  it.each([
    [0, 0, 1, 0],
    [1 / 6, 0, 1, 50],
    [1 / 3, 1, 2, 0],
    [0.5, 1, 2, 50],
    [2 / 3, 2, 3, 0],
    [5 / 6, 2, 3, 50],
    [1, 2, 3, 100],
  ])("places %d between heat color %d and %d at %d%%", (position, lower, upper, share) => {
    expect(footerTokensHeatColor(position, "p")).toBe(
      `color-mix(in oklab, var(--token-heat-${String(lower)}), var(--token-heat-${String(upper)}) ${String(share)}%)`,
    );
  });

  it.each([[-0.01], [1.01], [Number.NaN], [Number.POSITIVE_INFINITY]])("refuses %d", (position) => {
    expect(() => footerTokensHeatColor(position, "p")).toThrow(MalformedView);
  });
});
