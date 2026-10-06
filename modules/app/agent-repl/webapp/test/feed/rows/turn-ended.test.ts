// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedTurnEndedErroredSchema,
  FeedTurnEndedSchema,
  type FeedTurnEnded,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedTurnEnded } from "../../../src/feed/rows/turn-ended.js";
import { OUTCOME_MARKER_CLASS } from "../../../src/feed/marker.js";
import { feedId, harness, rowContext, userPromptRow, type countingTicker } from "../harness.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

/** A terminal row with the given outcome. */
type EndedInit = MessageInitShape<typeof FeedTurnEndedSchema>;

function ended(outcome: EndedInit["outcome"], endedAtMs = 1_000_000n): FeedTurnEnded {
  return create(FeedTurnEndedSchema, { endedAtMs, outcome });
}

/** A context whose feed holds one drawable row, for the answer lookup. */
function contextWithRow(element: HTMLElement | null, ticker?: ReturnType<typeof countingTicker>) {
  const { ctx } = harness({ ticker });
  return rowContext(ctx, userPromptRow("p1", "hi"), {
    findRowElement: () => element,
  });
}

/** The neutral "interrupted" marker the daemon sends with a stop. */
const INTERRUPTED_MARKER = {
  label: { text: "interrupted" },
  family: { case: "neutral" as const, value: {} },
};

/** A vendor-fault marker, as the daemon composes one. */
function vendorMarker(detail: string) {
  return {
    label: { text: "vendor error" },
    detail: { text: detail },
    family: {
      case: "vendorFault" as const,
      value: { expansion: { time: { atMs: 1_000_000n }, errorType: { text: detail } } },
    },
  };
}

describe("drawFeedTurnEnded: the arms", () => {
  it("draws a concluded turn as an invisible marker, saying nothing in prose", () => {
    const el = drawFeedTurnEnded(
      ended({ case: "concluded", value: {} }),
      contextWithRow(null),
    );
    expect(el.textContent).toBe("");
  });

  it("draws the user's stop as the user's act, not as a failure", () => {
    const el = drawFeedTurnEnded(
      ended({ case: "interrupted", value: { marker: INTERRUPTED_MARKER } }),
      contextWithRow(null),
    );
    expect(el.getAttribute("data-arm")).toBe("interrupted");
  });

  it("refuses an unset outcome, every turn having ended somehow", () => {
    expect(() =>
      drawFeedTurnEnded(create(FeedTurnEndedSchema, { endedAtMs: 0n }), contextWithRow(null)),
    ).toThrow(MalformedView);
  });
});

describe("drawFeedTurnEnded: an interrupt's command", () => {
  /** An interrupted ending stating the given command. */
  function interruptedBy(command: "direct" | "interjection" | undefined): FeedTurnEnded {
    const marker = command === "interjection" ? {} : { marker: INTERRUPTED_MARKER };
    return ended({
      case: "interrupted",
      value: command === undefined ? { ...marker } : { command: { case: command, value: {} }, ...marker },
    });
  }

  it("draws no marker for an interjection", () => {
    // ACT
    const el = drawFeedTurnEnded(interruptedBy("interjection"), contextWithRow(null));
    // ASSERT
    expect(el.querySelector(`.${OUTCOME_MARKER_CLASS}`)).toBeNull();
  });

  it("draws no words for an interjection", () => {
    // ACT
    const el = drawFeedTurnEnded(interruptedBy("interjection"), contextWithRow(null));
    // ASSERT
    expect(el.textContent).toBe("");
  });

  it("still draws an interjection's row, the turn's liveness anchor, as an interrupted ending", () => {
    // ACT
    const el = drawFeedTurnEnded(interruptedBy("interjection"), contextWithRow(null));
    // ASSERT
    expect({ state: el.getAttribute("data-state"), arm: el.getAttribute("data-arm") }).toEqual({
      state: "interrupted",
      arm: "interrupted",
    });
  });

  it("records the interjection's suppressed draw at debug", async () => {
    // ARRANGE
    const capture = captureLogRecords("debug");
    // ACT
    drawFeedTurnEnded(interruptedBy("interjection"), contextWithRow(null));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-turn-ended-interjection");
    expect(record.level.case).toBe("debug");
  });

  it("draws the neutral interrupted marker for a direct stop", () => {
    // ACT
    const el = drawFeedTurnEnded(interruptedBy("direct"), contextWithRow(null));
    // ASSERT
    const marker = el.querySelector(`.${OUTCOME_MARKER_CLASS}`);
    expect([marker?.getAttribute("data-family"), marker?.textContent]).toEqual(["neutral", "◼interrupted"]);
  });

  it("draws the neutral interrupted marker for a stop whose command is unset", () => {
    // ACT
    const el = drawFeedTurnEnded(interruptedBy(undefined), contextWithRow(null));
    // ASSERT
    expect(el.querySelector(`.${OUTCOME_MARKER_CLASS}`)?.getAttribute("data-family")).toBe("neutral");
  });

  it("refuses a direct stop that carries no marker", () => {
    // ARRANGE
    const bare = ended({ case: "interrupted", value: { command: { case: "direct", value: {} } } });
    // ACT, ASSERT
    expect(() => drawFeedTurnEnded(bare, contextWithRow(null))).toThrow(MalformedView);
  });

  it("draws no bubble for a stop", () => {
    // ACT
    const el = drawFeedTurnEnded(interruptedBy("direct"), contextWithRow(null));
    // ASSERT
    expect(el.querySelector(".bubble")).toBeNull();
  });

  it("refuses a command arm this client does not know", () => {
    // ARRANGE
    const unknown = interruptedBy(undefined);
    if (unknown.outcome.case !== "interrupted") throw new Error("the fixture is not an interrupt");
    unknown.outcome.value.command = { case: "invented" as never, value: {} as never };
    // ACT, ASSERT
    expect(() => drawFeedTurnEnded(unknown, contextWithRow(null))).toThrow(MalformedView);
  });
});

describe("drawFeedTurnEnded: the final answer", () => {
  it("marks the row the producer named", () => {
    const row = document.createElement("article");
    drawFeedTurnEnded(
      ended({ case: "concluded", value: { answer: feedId("r1") } }),
      contextWithRow(row),
    );
    expect(row.getAttribute("data-final-answer")).toBe("true");
  });

  it("does not paint the answer bubble green itself; the green is data-driven", () => {
    // The green final-answer border is now a DATA property the daemon stamps on
    // the answering response row (FeedResponse.final_answer), drawn by
    // cards/response.ts on every draw — so the concluded arm records only the
    // marker and never touches the bubble's border classes.
    const row = document.createElement("article");
    const bubble = document.createElement("div");
    bubble.className = "bubble assistant";
    row.append(bubble);
    drawFeedTurnEnded(
      ended({ case: "concluded", value: { answer: feedId("r1") } }),
      contextWithRow(row),
    );
    expect(bubble.classList.contains("final-response")).toBe(false);
  });

  it("never puts the amber async-live class on a settled answer bubble", () => {
    // Arrange — a settled answer bubble (owner ruling 2026-09-16: the amber
    // async-quiescence border is gone; a settled answer goes green, never amber,
    // even while background/detached work is still running).
    const row = document.createElement("article");
    const bubble = document.createElement("div");
    bubble.className = "bubble assistant";
    row.append(bubble);
    // Act
    drawFeedTurnEnded(
      ended({ case: "concluded", value: { answer: feedId("r1") } }),
      contextWithRow(row),
    );
    // Assert — the concluded arm leaves the bubble's border classes alone.
    expect(bubble.classList.contains("async-live")).toBe(false);
  });

  it("marks nothing when the turn concluded with no answering prose", () => {
    const row = document.createElement("article");
    drawFeedTurnEnded(ended({ case: "concluded", value: {} }), contextWithRow(row));
    expect(row.hasAttribute("data-final-answer")).toBe(false);
  });

  it("marks nothing when the named row is not on this feed", () => {
    expect(() =>
      drawFeedTurnEnded(
        ended({ case: "concluded", value: { answer: feedId("gone") } }),
        contextWithRow(null),
      ),
    ).not.toThrow();
  });

  it("records an answer on a page not loaded yet at debug, never as a warning", async () => {
    // Arrange: the daemon names only rows it drew, so an absent row is older
    // history this page has not loaded.
    const capture = captureLogRecords("debug");
    // Act
    drawFeedTurnEnded(
      ended({ case: "concluded", value: { answer: feedId("older") } }),
      contextWithRow(null),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.final-answer-row-absent");
    expect(record.level.case).toBe("debug");
  });
});

describe("drawFeedTurnEnded: every error arm", () => {
  /** Every arm the SCHEMA declares — so a new cause fails this suite. */
  const schemaArms = FeedTurnEndedErroredSchema.oneofs
    .filter((oneof) => oneof.name === "error")
    .flatMap((oneof) => oneof.fields.map((field) => field.localName));

  /** An errored ending of ARM, carrying its marker. */
  function errored(arm: string): FeedTurnEnded {
    return ended({
      case: "errored",
      value: create(FeedTurnEndedErroredSchema, {
        headline: { text: "the turn died" },
        error: { case: arm as never, value: { type: "x" } as never },
        marker: vendorMarker(arm),
      }),
    });
  }

  it.each(schemaArms)("draws %s distinctly, by its own arm", (arm) => {
    const el = drawFeedTurnEnded(errored(arm), contextWithRow(null));
    expect(el.getAttribute("data-turn-error")).toBe(arm);
  });

  it.each(schemaArms)("draws %s as its outcome marker alone", (arm) => {
    const el = drawFeedTurnEnded(errored(arm), contextWithRow(null));
    expect(el.querySelector(`.${OUTCOME_MARKER_CLASS} .outcome-marker-pill`)?.textContent).toBe(
      `◆vendor error · ${arm}›`,
    );
  });

  it.each(schemaArms)("draws no bubble for %s", (arm) => {
    const el = drawFeedTurnEnded(errored(arm), contextWithRow(null));
    expect(el.querySelector(".bubble")).toBeNull();
  });

  it("draws no headline of its own beside the marker", () => {
    const el = drawFeedTurnEnded(errored("overloaded"), contextWithRow(null));
    expect(el.textContent).not.toContain("the turn died");
  });

  it("refuses an errored row with no marker to draw", () => {
    const bare = ended({
      case: "errored",
      value: create(FeedTurnEndedErroredSchema, {
        headline: { text: "the turn died" },
        error: { case: "internal", value: {} },
      }),
    });
    expect(() => drawFeedTurnEnded(bare, contextWithRow(null))).toThrow(MalformedView);
  });

  it("refuses an errored row whose cause arm is unset", () => {
    const bare = ended({
      case: "errored",
      value: create(FeedTurnEndedErroredSchema, { headline: { text: "x" }, marker: vendorMarker("x") }),
    });
    expect(() => drawFeedTurnEnded(bare, contextWithRow(null))).toThrow(MalformedView);
  });
});

describe("drawFeedTurnEnded: an arm this build has no case for", () => {
  it("refuses an outcome arm a NEWER daemon set, quoting the arm it could not draw", () => {
    // Arrange
    const msg = ended({ case: "concluded", value: {} });
    (msg as unknown as { outcome: unknown }).outcome = { case: "abandoned", value: {} };

    // Act
    let thrown: unknown;
    try {
      drawFeedTurnEnded(msg, contextWithRow(null));
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedTurnEnded.outcome");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'abandoned' is not one this build can draw",
    );
  });
});

describe("drawFeedTurnEnded: the records of the turn's end", () => {
  it("records the drawn terminal row at info, a turn ending exactly once", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedTurnEnded(ended({ case: "concluded", value: {} }), contextWithRow(null));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-turn-ended");
    expect(record.level.case).toBe("info");
  });

  it("records the final-answer marking at info, it happening once per turn", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedTurnEnded(
      ended({ case: "concluded", value: { answer: feedId("r1") } }),
      contextWithRow(document.createElement("article")),
    );
    // ASSERT
    const record = await forwardedRecord(capture, "feed.final-answer-marked");
    expect(record.level.case).toBe("info");
  });

  it("records the drawn error arm at info, an errored end being one row too", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "the query died" },
          error: { case: "queryDied", value: {} },
          marker: vendorMarker("query died"),
        }),
      }),
      contextWithRow(null),
    );
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-turn-error");
    expect(record.level.case).toBe("info");
  });
});

