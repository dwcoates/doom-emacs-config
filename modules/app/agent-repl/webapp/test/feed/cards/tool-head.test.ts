// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedToolCardClockSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { createTicker } from "../../../src/clock.js";
import { RESERVED_CLOCK_CLASS } from "../../../src/elapsed-clock.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { TICKING_ATTRIBUTE } from "../../../src/feed/ticking.js";
import {
  HEAD_CLOCK_CLASS,
  TOOL_CARD_HEAD_ATTRIBUTE,
  TOOL_CLOCK_CLASS,
  TOOL_HEAD_CLASS,
  TOOL_NAME_CLASS,
  drawToolCardBadge,
  drawToolCardClock,
  drawToolCardHead,
  drawToolHead,
  asHeadClock,
} from "../../../src/feed/cards/tool-head.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import { runningClock, stoppedClock } from "./tool-card-head-shape.js";
import { filesUnder } from "../../files-under.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src");


/** A badge reading WORD. */
function badge(word: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "badge";
  el.textContent = word;
  return el;
}

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(0);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("drawToolCardHead", () => {
  it("draws the title, the badge and the clock, in that order and nothing else", () => {
    // Arrange
    vi.setSystemTime(9_000);

    // Act
    const head = drawToolCardHead({
      title: "Bash",
      badge: badge("running…"),
      clock: runningClock(0),
      clockPath: "X.clock",
      ticker: createTicker(),
    });

    // Assert
    expect([...head.children].map((c) => [c.className.split(" ")[0], c.textContent])).toEqual([
      [TOOL_NAME_CLASS, "Bash"],
      ["badge", "running…"],
      [TOOL_CLOCK_CLASS, "9s"],
    ]);
  });

  it("marks the head as the shared head, wearing the shared class", () => {
    const head = drawToolCardHead({ title: "Bash", badge: badge("done"), clock: undefined, clockPath: "X.clock", ticker: createTicker() });
    expect([head.className, head.hasAttribute(TOOL_CARD_HEAD_ATTRIBUTE)]).toEqual([TOOL_HEAD_CLASS, true]);
  });

  it("marks its clock as the head's clock", () => {
    const head = drawToolCardHead({ title: "Bash", badge: badge("done"), clock: stoppedClock(0, 3_000), clockPath: "X.clock", ticker: createTicker() });
    expect(head.querySelector(`.${TOOL_CLOCK_CLASS}`)?.classList.contains(HEAD_CLOCK_CLASS)).toBe(true);
  });

  it("draws no clock when the view carries none", () => {
    const head = drawToolCardHead({ title: "Bash", badge: badge("done"), clock: undefined, clockPath: "X.clock", ticker: createTicker() });
    expect(head.querySelector(`.${TOOL_CLOCK_CLASS}`)).toBeNull();
  });

  it("puts the shared title class on a title the card composed", () => {
    // Arrange
    const title = document.createElement("span");
    title.className = "perm-headline";
    title.textContent = "Claude wants to read foo.txt";

    // Act
    const head = drawToolCardHead({ title, badge: badge("waiting"), clock: undefined, clockPath: "X.clock", ticker: createTicker() });

    // Assert
    expect([head.firstElementChild === title, [...title.classList]]).toEqual([true, ["perm-headline", TOOL_NAME_CLASS]]);
  });

  it("adds the card's own classes beside the shared one", () => {
    const head = drawToolCardHead({
      title: "x",
      badge: badge("waiting"),
      clock: undefined,
      clockPath: "X.clock",
      ticker: createTicker(),
      classes: ["perm-head"],
    });
    expect(head.className).toBe(`${TOOL_HEAD_CLASS} perm-head`);
  });
});

describe("drawToolCardClock", () => {
  it("counts a running clock up on the shared ticker", async () => {
    // Arrange
    const el = drawToolCardClock(runningClock(0), "X.clock", createTicker());
    document.body.replaceChildren(el);

    // Act
    await vi.advanceTimersByTimeAsync(3_000);

    // Assert
    expect([el.textContent, el.hasAttribute(TICKING_ATTRIBUTE)]).toEqual(["3s", true]);
  });

  it("draws a stopped clock's span once, ticking nothing", () => {
    const el = drawToolCardClock(stoppedClock(1_000, 135_000), "X.clock", createTicker());
    expect([el.textContent, el.hasAttribute(TICKING_ATTRIBUTE)]).toEqual(["2m 14s", false]);
  });

  it.each([
    { name: "running", clock: () => runningClock(0) },
    { name: "stopped", clock: () => stoppedClock(0, 1_000) },
  ])("holds a $name clock at its reserved width, so a tick never reflows the head", ({ clock }) => {
    const el = drawToolCardClock(clock(), "X.clock", createTicker());
    expect(el.classList.contains(RESERVED_CLOCK_CLASS)).toBe(true);
  });

  it("refuses a clock that stops before it starts", () => {
    expect(() => drawToolCardClock(stoppedClock(5_000, 1_000), "X.clock", createTicker())).toThrow(MalformedView);
  });

  it("refuses a clock whose span is unset", () => {
    const clock = create(FeedToolCardClockSchema, { startedAtMs: 1_000n });
    expect(() => drawToolCardClock(clock, "X.clock", createTicker())).toThrow(MalformedView);
  });
});

describe("asHeadClock", () => {
  it("adds the head clock class beside the clock's own and answers the same element", () => {
    // Arrange
    const clock = document.createElement("span");
    clock.className = "shell-clock reserved-clock";

    // Act
    const marked = asHeadClock(clock);

    // Assert
    expect([marked === clock, [...clock.classList]]).toEqual([true, ["shell-clock", "reserved-clock", HEAD_CLOCK_CLASS]]);
  });

  it("sends the head clock to its row's right end, last whatever the draw order", () => {
    // Arrange
    const teardown = installStylesheet();
    const clock = asHeadClock(document.createElement("span"));
    document.body.append(clock);
    try {
      // Act
      const placed = [cascadedValue(clock, "margin-left"), cascadedValue(clock, "order")];

      // Assert
      expect(placed).toEqual(["auto", "99"]);
    } finally {
      clock.remove();
      teardown();
    }
  });
});

/**
 * Every module drawing a head's elapsed clock: the tool card head and each
 * bubble head. A module here that draws a reserved elapsed clock not wrapped
 * in `asHeadClock` has a head clock the stylesheet leaves where it was drawn.
 */
const HEAD_CLOCK_SITES = ["feed/cards/tool-head.ts", "feed/cards/shell.ts", "feed/rows/subagent.ts", "feed/merge/merge.ts"] as const;

describe("every head marks its clock through asHeadClock", () => {
  it.each(HEAD_CLOCK_SITES)("wraps every reserved elapsed clock %s draws", (file) => {
    // Arrange
    const source = readFileSync(path.join(SRC, file), "utf8");

    // Act
    const drawn = [...source.matchAll(/reserved(?:Live|Settled)ElapsedClock\(/g)].length;
    const marked = [...source.matchAll(/asHeadClock\(\s*reserved(?:Live|Settled)ElapsedClock\(/g)].length;
    const viaCardClock = /asHeadClock\(drawToolCardClock\(/.test(source);

    // Assert
    expect(file === "feed/cards/tool-head.ts" ? viaCardClock : drawn > 0 && marked === drawn).toBe(true);
  });
});

describe("drawToolCardBadge", () => {
  it("draws the word in a span wearing the shared badge class and its tones", () => {
    const badge = drawToolCardBadge("done", ["ok"]);
    expect([badge.localName, badge.className, badge.textContent]).toEqual(["span", "badge ok", "done"]);
  });

  it("names the card's arm when given", () => {
    expect(drawToolCardBadge("blocked", ["err"], "blocked").getAttribute("data-arm")).toBe("blocked");
  });

  it("names no arm when none is given", () => {
    expect(drawToolCardBadge("waiting").hasAttribute("data-arm")).toBe(false);
  });
});

describe("drawToolHead", () => {
  it("draws a bare head row wearing the shared class first", () => {
    const head = drawToolHead("bubble-head");
    expect([head.localName, head.className, head.children.length]).toEqual(["div", `${TOOL_HEAD_CLASS} bubble-head`, 0]);
  });
});

/** The five purple-titled tool cards, by the module that draws each. */
const TOOL_CARDS = [
  "feed/cards/tool-call.ts",
  "feed/cards/skill.ts",
  "feed/cards/hook.ts",
  "feed/asks/permission.ts",
  "feed/asks/question.ts",
] as const;

describe("every tool card heads through the one builder", () => {
  it.each(TOOL_CARDS)("heads %s through drawToolCardHead", (file) => {
    expect(readFileSync(path.join(SRC, file), "utf8")).toContain("drawToolCardHead({");
  });

  it("leaves only the bubbles' head row building a bare head", () => {
    // Bubbles are their own category and lay out their own head inside the
    // shared row; any other module building a bare head row is a hand-rolled
    // tool card head.
    const own = path.join(SRC, "feed/cards/tool-head.ts");
    const users = filesUnder(SRC)
      .filter((file) => file !== own)
      .filter((file) => /drawToolHead\(/.test(readFileSync(file, "utf8")));
    expect(users.map((file) => path.relative(SRC, file))).toEqual(["feed/bubble.ts"]);
  });

  it("leaves no module building a tool head or tool name by hand", () => {
    // A hand-rolled head drifts from the shared one; this fails it by name.
    const own = path.join(SRC, "feed/cards/tool-head.ts");
    const offenders = filesUnder(SRC)
      .filter((file) => file !== own)
      .filter((file) => /className\s*=\s*["'`]tool-(head|name|clock)\b/.test(readFileSync(file, "utf8")));
    expect(offenders.map((file) => path.relative(SRC, file))).toEqual([]);
  });
});

describe("every tool card badges through the one builder", () => {
  it.each(TOOL_CARDS)("builds no badge by hand in %s", (file) => {
    // An exit code is content, not the head's badge, and keeps its own chip.
    const source = readFileSync(path.join(SRC, file), "utf8");
    const handRolled = [...source.matchAll(/className\s*=\s*[^;]*["'`]badge\b[^;]*;/g)]
      .map((m) => m[0])
      .filter((assignment) => !/exit|q-chip/.test(assignment));
    expect(handRolled).toEqual([]);
  });
});
