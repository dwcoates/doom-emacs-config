// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedSubagentLostSchema,
  FeedSubagentSchema,
  FeedSubagentSettledSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { InterruptResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { INTERRUPT_ERROR_ARMS } from "../../../src/interrupt-error.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  SUBAGENT_LOST_CAUSE_ARMS,
  SUBAGENT_SETTLED_ARMS,
  drawFeedSubagent,
} from "../../../src/feed/rows/subagent.js";
import { countingTicker, harness, rowContext, subagentRow, type Harness } from "../harness.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import {
  HAS_MORE_CLASS,
  TITLE_FOLD_CLASS,
  TITLE_FOLD_STANDALONE_CLASS,
} from "../../../src/feed/bubble-more.js";
import { fireResize } from "../../resize-observer.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import { inBubbleFold, measureTitle } from "../title-measure.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

/** Draw the head of a row built by the fixture. */
function drawRow(
  row: ReturnType<typeof subagentRow>,
  h: Harness = harness(),
): { el: HTMLElement; h: Harness } {
  const subagent =
    row.row.case === "detachedSubagent"
      ? row.row.value.subagent
      : row.row.case === "activity" && row.row.value.unit.case === "subagent"
        ? row.row.value.unit.value
        : undefined;
  if (subagent === undefined) throw new Error("fixture is not a subagent row");
  return { el: drawFeedSubagent(subagent, rowContext(h.ctx, row)), h };
}

/** Let the scripted answer land. */
async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("drawFeedSubagent: the head's parts", () => {
  it("draws the label the daemon resolved", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-label")?.textContent).toBe("Explore");
  });

  it("draws the description when the spawn carried one", () => {
    const { el } = drawRow(subagentRow("b1", { description: "sweep the repo" }));
    expect(el.querySelector(".subagent-description")?.textContent).toBe("sweep the repo");
  });

  it("draws NO description when the spawn carried none, never a synthesized one", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-description")).toBeNull();
  });

  it("draws the daemon-formatted token sum verbatim", () => {
    const { el } = drawRow(subagentRow("b1", { tokens: "12.4k tok" }));
    expect(el.querySelector(".subagent-tokens")?.textContent).toBe("12.4k tok");
  });

  it("draws no token figure before any usage was observed", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-tokens")).toBeNull();
  });

  it("wears the shared token-count hue class on the token figure", () => {
    const { el } = drawRow(subagentRow("b1", { tokens: "12.4k tok" }));
    expect(el.querySelector(".subagent-tokens")?.classList.contains("token-count")).toBe(true);
  });

  it("refuses a head with no label", () => {
    const { ctx } = harness();
    const row = subagentRow("b1");
    const bad = create(FeedSubagentSchema, {
      runtime: { startedAtMs: 0n },
      state: { case: "live", value: {} },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, row))).toThrow(MalformedView);
  });

  it("refuses a head with no runtime, there being no clock to draw", () => {
    const { ctx } = harness();
    const bad = create(FeedSubagentSchema, {
      label: { text: "Explore" },
      state: { case: "live", value: {} },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, subagentRow("b1")))).toThrow(MalformedView);
  });

  it("refuses a head whose state arm is unset", () => {
    const { ctx } = harness();
    const bad = create(FeedSubagentSchema, {
      label: { text: "Explore" },
      runtime: { startedAtMs: 0n },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, subagentRow("b1")))).toThrow(MalformedView);
  });
});

describe("drawFeedSubagent: the clocks", () => {
  it("counts up from the ORIGINAL start while live", () => {
    const { el } = drawRow(subagentRow("b1", { startedAtMs: 940_000n }));
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("1m");
  });

  it("ticks the live clock on the shared clock", () => {
    const { el } = drawRow(subagentRow("b1", { startedAtMs: 1_000_000n }));
    document.body.append(el);
    vi.advanceTimersByTime(5_000);
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("5s");
  });

  it("reads the live clock's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the start does not share the shared ticker's phase.
    const { el } = drawRow(subagentRow("b1", { startedAtMs: 1_000_000n - 4920n }));
    // Assert: five real seconds of running reads 5s, not the lagging 4s.
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("5s");
  });

  it("stops the clock at the settled instant", () => {
    const { el } = drawRow(
      subagentRow("b1", {
        startedAtMs: 1_000_000n,
        settled: { endedAtMs: 1_030_000n, outcome: "succeeded" },
      }),
    );
    document.body.append(el);
    vi.advanceTimersByTime(60_000);
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("30s");
  });

  it("ticks 'quiet for' from the last beat the daemon observed", () => {
    const { el } = drawRow(subagentRow("b1", { lastProgressMs: 990_000n }));
    expect(el.querySelector(".subagent-quiet")?.textContent).toBe("quiet for 10s");
  });

  it("reads the quiet-for's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the last beat does not share the shared ticker's phase.
    const { el } = drawRow(subagentRow("b1", { lastProgressMs: 1_000_000n - 4920n }));
    // Assert: five real seconds of silence reads 5s, not the lagging 4s.
    expect(el.querySelector(".subagent-quiet")?.textContent).toBe("quiet for 5s");
  });

  it("draws no quietness figure before the first beat", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-quiet")).toBeNull();
  });
});

describe("drawFeedSubagent: the settled arms", () => {
  it("words every settled arm the schema declares", () => {
    const schemaArms = FeedSubagentSettledSchema.oneofs
      .filter((oneof) => oneof.name === "outcome")
      .flatMap((oneof) => oneof.fields.map((field) => field.localName));
    expect([...SUBAGENT_SETTLED_ARMS].sort()).toEqual([...schemaArms].sort());
  });

  it.each(["succeeded", "failed", "cancelled", "lost"] as const)(
    "says %s as its own state",
    (outcome) => {
      const { el } = drawRow(
        subagentRow("b1", { settled: { endedAtMs: 1_000_000n, outcome } }),
      );
      expect(el.getAttribute("data-state")).toBe(outcome);
    },
  );

  it('draws "done" as a green ok badge, the same shape the tool-call verdict draws', () => {
    const { el } = drawRow(
      subagentRow("b1", { settled: { endedAtMs: 1_000_000n, outcome: "succeeded" } }),
    );
    const word = el.querySelector(".subagent-outcome");
    expect(word?.textContent).toBe("done");
    expect(word?.classList.contains("badge")).toBe(true);
    expect(word?.classList.contains("ok")).toBe(true);
  });

  it("draws a failure as an err badge", () => {
    const { el } = drawRow(
      subagentRow("b1", { settled: { endedAtMs: 1_000_000n, outcome: "failed" } }),
    );
    const word = el.querySelector(".subagent-outcome");
    expect(word?.classList.contains("badge")).toBe(true);
    expect(word?.classList.contains("err")).toBe(true);
  });

  it("never draws `lost` as a failure", () => {
    const lost = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost" } })).el;
    const failed = drawRow(subagentRow("b2", { settled: { endedAtMs: 1n, outcome: "failed" } })).el;
    expect(lost.querySelector(".subagent-outcome")?.textContent).not.toBe(
      failed.querySelector(".subagent-outcome")?.textContent,
    );
  });

  it('says "file vanished" as the lost cause when the file went away', () => {
    const { el } = drawRow(
      subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost", lostHow: "fileVanished" } }),
    );
    expect(el.querySelector(".subagent-outcome")?.textContent).toBe("lost sight of: file vanished");
  });

  it('says "went silent" as the lost cause when the run produced nothing', () => {
    const { el } = drawRow(
      subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost", lostHow: "wentSilent" } }),
    );
    expect(el.querySelector(".subagent-outcome")?.textContent).toBe("lost sight of: went silent");
  });

  it('says "swept up at boot" as the lost cause when a boot sweep closed it', () => {
    const { el } = drawRow(
      subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost", lostHow: "sweptUp" } }),
    );
    expect(el.querySelector(".subagent-outcome")?.textContent).toBe(
      "lost sight of: swept up at boot",
    );
  });

  it("says the plain word when an older daemon ruled no cause", () => {
    const { el } = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost" } }));
    expect(el.querySelector(".subagent-outcome")?.textContent).toBe("lost sight of");
  });

  it("words every lost cause the schema declares", () => {
    const schemaArms = FeedSubagentLostSchema.oneofs
      .filter((oneof) => oneof.name === "how")
      .flatMap((oneof) => oneof.fields.map((field) => field.localName));
    expect([...SUBAGENT_LOST_CAUSE_ARMS].sort()).toEqual([...schemaArms].sort());
  });

  it.each([
    ["succeeded", "hollow", "○", "tone-none"],
    ["failed", "filled", "●", "tone-red"],
    ["cancelled", "hollow", "○", "tone-none"],
    ["lost", "filled", "●", "tone-turquoise"],
  ] as const)("dots a %s head %s (%s) in %s", (outcome, shape, glyph, tone) => {
    const { el } = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome } }));
    const dot = el.querySelector(".agent-dot");
    expect([dot?.getAttribute("data-dot"), dot?.textContent, dot?.classList.contains(tone)]).toEqual([
      shape,
      glyph,
      true,
    ]);
  });

  it("dots a live head filled green, breathing", () => {
    const { el } = drawRow(subagentRow("b1"));
    const dot = el.querySelector(".agent-dot");
    expect([
      dot?.getAttribute("data-dot"),
      dot?.textContent,
      dot?.classList.contains("tone-green"),
      dot?.classList.contains("work-dot-live"),
    ]).toEqual(["filled", "●", true, true]);
  });

  it.each([
    ["failed", "var(--err)"],
    ["lost", "var(--turquoise)"],
  ] as const)("lets the vocabulary's tone paint a %s dot %s through the real stylesheet", (outcome, color) => {
    const uninstall = installStylesheet();
    const { el } = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome } }));
    document.body.append(el);
    const painted = cascadedValue(el.querySelector(".agent-dot") as Element, "color");
    el.remove();
    uninstall();
    expect(painted).toBe(color);
  });

  it("stops a settled head's dot breathing", () => {
    const { el } = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "succeeded" } }));
    expect(el.querySelector(".agent-dot")?.classList.contains("work-dot-live")).toBe(false);
  });

  it("refuses a settled head whose outcome arm is unset", () => {
    const { ctx } = harness();
    const bad = create(FeedSubagentSchema, {
      label: { text: "Explore" },
      runtime: { startedAtMs: 0n },
      state: { case: "settled", value: { endedAtMs: 1n } },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, subagentRow("b1")))).toThrow(MalformedView);
  });
});

describe("drawFeedSubagent: the stop control", () => {
  it("offers a stop on a LIVE DETACHED bubble, which outlives its turn", () => {
    const { el } = drawRow(subagentRow("b1", { detached: true }));
    expect(el.querySelector("[data-interrupt]")).not.toBeNull();
  });

  it("offers none on a synchronous spawn, whose stop is the turn's", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });

  it("offers none on a settled detached bubble", () => {
    const { el } = drawRow(
      subagentRow("b1", { detached: true, settled: { endedAtMs: 1n, outcome: "succeeded" } }),
    );
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });

  it("interrupts by the bubble row's OWN id, echoed verbatim", async () => {
    const { el, h } = drawRow(subagentRow("b1", { detached: true }));
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt[0]?.target).toEqual({
      case: "detached",
      value: { $typeName: "frontend.v1.FeedId", value: "b1" },
    });
  });

  it("draws the interrupted_detached answer at the control", async () => {
    const { el } = drawRow(subagentRow("b1", { detached: true }));
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".subagent-stop-outcome")?.getAttribute("data-outcome")).toBe(
      "interruptedDetached",
    );
  });

  it("draws nothing_running as an ANSWER, not as a failure", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "success",
            value: { outcome: { case: "nothingRunning", value: {} } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("clears the answer once it has been on screen long enough to read", async () => {
    const { el } = drawRow(subagentRow("b1", { detached: true }));
    document.body.append(el);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    await vi.advanceTimersByTimeAsync(5_000);
    expect(el.querySelector(".subagent-stop-outcome")).toBeNull();
  });

  it("draws the confirm_required refusal at the control that made the call", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("confirmRequired");
  });

  it("names the count the challenge carried", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.textContent).toContain("3");
  });

  it.each(INTERRUPT_ERROR_ARMS)("draws the %s refusal by its own arm", async (arm) => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: { case: "error", value: { kind: { case: arm as never, value: {} as never } } },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe(arm);
  });

  it("names the registry's directory when the workspace ref disagrees with it", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "workspaceRefMismatch", value: { registryDir: "/w/other" } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.textContent).toContain("/w/other");
  });

  it("offers no confirm step, the challenge not applying to a detached target", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 1n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector("[data-interrupt-confirm]")).toBeNull();
  });

  it("does not resend the stop after the challenge", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 1n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt).toHaveLength(1);
  });

  it("tells the reader when the click never reached the daemon", async () => {
    const h = harness({
      interrupt: () => {
        throw new Error("gone");
      },
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("drawFeedSubagent: the record of the head", () => {
  it("records the drawn head at info, a head being drawn exactly once", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawRow(subagentRow("b1"));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-subagent");
    expect(record.level.case).toBe("info");
  });
});

describe("drawFeedSubagent: a settled head's clocks", () => {
  it("subscribes to nothing once the subagent has settled", () => {
    // Arrange: a ticker whose live subscriptions the test can count.
    const ticker = countingTicker();
    // Act.
    drawRow(
      subagentRow("s", { settled: { endedAtMs: 12_000n, outcome: "succeeded" } }),
      harness({ ticker }),
    );
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("freezes the settled figure at the message's own span, not the wall clock", () => {
    const { el } = drawRow(
      subagentRow("s", { startedAtMs: 1000n, settled: { endedAtMs: 8000n, outcome: "succeeded" } }),
    );
    document.body.append(el);
    vi.advanceTimersByTime(60_000);
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("7s");
    el.remove();
  });
});

/**
 * THE DESCRIPTION IS THE BUBBLE'S TITLE (owner ruling, 2026-09-23): the one
 * two-line title fold (title-fold.ts), owned by the bubble's fold (bubble.ts).
 */
describe("drawFeedSubagent: the title fold", () => {
  const DESCRIPTION = "sweep the repo for every caller of the old fold";

  /** A head of ROW seated in a collapsed bubble, and its description. */
  function seated(row = subagentRow("b1", { description: DESCRIPTION })) {
    const { el } = drawRow(row);
    const bubble = inBubbleFold(el);
    return { bubble, title: el.querySelector(".subagent-description") as HTMLElement };
  }

  it("marks the description with the one title-fold class", () => {
    // Arrange / Act
    const { title } = seated();

    // Assert
    expect(title.classList.contains(TITLE_FOLD_CLASS)).toBe(true);
  });

  it("defers the description's fold to the bubble rather than making it its own", () => {
    // Arrange / Act
    const { title } = seated();

    // Assert
    expect(title.classList.contains(TITLE_FOLD_STANDALONE_CLASS)).toBe(false);
  });

  it("folds nothing when the spawn carried no description", () => {
    // Arrange / Act
    const { el } = drawRow(subagentRow("b1"));

    // Assert
    expect(el.querySelector(`.${TITLE_FOLD_CLASS}`)).toBeNull();
  });

  it("wears has-more when the description overflows its two lines", () => {
    // Arrange
    const { title } = seated();
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps has-more off a description that fits its two lines", () => {
    // Arrange
    const { title } = seated();
    measureTitle(title, false);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("drops has-more once the bubble is expanded", () => {
    // Arrange
    const { bubble, title } = seated();
    measureTitle(title, true);
    fireResize(title);
    bubble.setAttribute("data-expanded", "true");

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("keeps measuring a settled head's description after the head's terminal stop", () => {
    // Arrange
    const { title } = seated(
      subagentRow("b1", {
        description: DESCRIPTION,
        settled: { endedAtMs: 5000n, outcome: "succeeded" },
      }),
    );
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("clamps the description to two lines while the bubble is collapsed", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { title } = seated();

      // Act / Assert
      expect(cascadedValue(title, "-webkit-line-clamp")).toBe("2");
    } finally {
      remove();
    }
  });

  it("wraps the description onto its two lines rather than a one-line ellipsis", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { title } = seated();

      // Act / Assert
      expect(cascadedValue(title, "white-space")).not.toBe("nowrap");
    } finally {
      remove();
    }
  });

  it("shows the whole description once the bubble is expanded", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble, title } = seated();
      bubble.setAttribute("data-expanded", "true");

      // Act / Assert
      expect(cascadedValue(title, "-webkit-line-clamp")).toBe("none");
    } finally {
      remove();
    }
  });
});

describe("drawFeedSubagent: the detached-work id", () => {
  it.each([
    { name: "a detached bubble names its work in the head", opts: { detached: true, workId: "work-1" }, want: "work-1" },
    { name: "a synchronous spawn names none", opts: { detached: false }, want: null },
  ])("$name", ({ opts, want }) => {
    // Arrange, Act
    const { el } = drawRow(subagentRow("b1", opts));

    // Assert
    expect(el.querySelector(".async-work-id")?.textContent ?? null).toBe(want);
  });
});
