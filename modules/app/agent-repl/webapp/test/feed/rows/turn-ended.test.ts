// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedTurnEndedErroredSchema,
  FeedTurnEndedSchema,
  type FeedTurnEnded,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  INTERRUPTED_SENTENCE,
  QUERY_CAUSE_WORDS,
  TURN_ENDED_BUBBLE_CLASS,
  TURN_ENDED_BUBBLE_SAYS_CLASS,
  TURN_ERROR_WAIT_ARMS,
  drawFeedTurnEnded,
  drawFeedTurnEndedErrored,
} from "../../../src/feed/rows/turn-ended.js";
import { countingTicker, feedId, harness, rowContext, userPromptRow } from "../harness.js";
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
      ended({ case: "interrupted", value: {} }),
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
});

describe("drawFeedTurnEnded: every error arm", () => {
  /** Every arm the SCHEMA declares — so a new cause fails this suite. */
  const schemaArms = FeedTurnEndedErroredSchema.oneofs
    .filter((oneof) => oneof.name === "error")
    .flatMap((oneof) => oneof.fields.map((field) => field.localName));

  /** The arms whose message CARRIES a wait, read off the schema itself. */
  const schemaWaitArms = FeedTurnEndedErroredSchema.oneofs
    .filter((oneof) => oneof.name === "error")
    .flatMap((oneof) => oneof.fields)
    .filter((field) =>
      (field.message?.fields ?? []).some((inner) => inner.name === "retry_after_ms"),
    )
    .map((field) => field.localName);

  it("counts down on exactly the arms the schema gives a wait", () => {
    expect([...TURN_ERROR_WAIT_ARMS].sort()).toEqual([...schemaWaitArms].sort());
  });

  it.each(schemaArms)("draws %s distinctly, by its own arm", (arm) => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "the turn died" },
          error: { case: arm as never, value: { type: "x" } as never },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.getAttribute("data-turn-error")).toBe(arm);
  });

  it.each(schemaArms)("draws %s's headline and no wording of its own", (arm) => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: `the daemon's words for ${arm}` },
          error: { case: arm as never, value: { type: "x" } as never },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-cause")?.textContent).toBe(
      `the daemon's words for ${arm}`,
    );
  });

  it("draws the daemon's headline verbatim, wording nothing itself", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "an API error this build does not model: teapot_error" },
          error: { case: "vendorUnmodeled", value: { type: "teapot_error" } },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-cause")?.textContent).toBe(
      "an API error this build does not model: teapot_error",
    );
  });

  it("states the vendor's own type name on the unmodeled arm", () => {
    // Arrange / Act: the field is "the vendor's type name, drawn verbatim"
    // (feed.proto), and it is the only handle the reader has on what happened.
    const el = drawFeedTurnEndedErrored(
      create(FeedTurnEndedErroredSchema, {
        headline: { text: "an API error this build does not model" },
        error: { case: "vendorUnmodeled", value: { type: "teapot_error" } },
      }),
      9_000,
      contextWithRow(null),
    );
    // Assert
    expect(el.querySelector("[data-vendor-type]")?.textContent).toBe("teapot_error");
  });

  it("draws no vendor-type element on a modeled arm", () => {
    // Arrange / Act
    const el = drawFeedTurnEndedErrored(
      create(FeedTurnEndedErroredSchema, {
        headline: { text: "the vendor failed internally" },
        error: { case: "internal", value: {} },
      }),
      9_000,
      contextWithRow(null),
    );
    // Assert
    expect(el.querySelector("[data-vendor-type]")).toBeNull();
  });

  it("refuses an errored row with no headline to draw", () => {
    expect(() =>
      drawFeedTurnEnded(
        ended({
          case: "errored",
          value: create(FeedTurnEndedErroredSchema, {
            error: { case: "internal", value: {} },
          }),
        }),
        contextWithRow(null),
      ),
    ).toThrow(MalformedView);
  });

  it("draws the vendor's sentence when the record carried one", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
          message: { text: "overloaded_error" },
          error: { case: "internal", value: {} },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-vendor")?.textContent).toBe("overloaded_error");
  });

  it("draws no vendor line for a cause with no vendor wording", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
          error: { case: "queryDied", value: {} },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-vendor")).toBeNull();
  });

  it("names an unexpected eof as the cause the query died of", () => {
    // Arrange / Act
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "the query died" },
          error: {
            case: "queryDied",
            value: { cause: { case: "unexpectedEof", value: {} } },
          },
        }),
      }),
      contextWithRow(null),
    );

    // Assert
    expect(el.querySelector("[data-query-cause]")?.getAttribute("data-query-cause")).toBe(
      "unexpectedEof",
    );
  });

  it("names an iterator failure as the cause the query died of", () => {
    // Arrange / Act
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "the query died" },
          error: {
            case: "queryDied",
            value: { cause: { case: "iteratorFailure", value: {} } },
          },
        }),
      }),
      contextWithRow(null),
    );

    // Assert
    expect(el.querySelector("[data-query-cause]")?.textContent).toBe(
      QUERY_CAUSE_WORDS.iteratorFailure,
    );
  });

  it("keeps the line as it was when the query death names no cause", () => {
    // Arrange / Act
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "the query died" },
          error: { case: "queryDied", value: {} },
        }),
      }),
      contextWithRow(null),
    );

    // Assert
    expect(el.querySelector("[data-query-cause]")).toBeNull();
  });

  it("refuses a query-died cause this build does not know", () => {
    // Arrange — set after construction: the fixture builder drops an arm the
    // schema does not carry, and the case under test is exactly such an arm
    // reaching the renderer.
    const errored = create(FeedTurnEndedErroredSchema, {
      headline: { text: "the query died" },
      error: { case: "queryDied", value: {} },
    });
    const died = errored.error.value as { cause: { case: string; value: unknown } };
    died.cause = { case: "invented", value: {} };

    // Act / Assert
    expect(() =>
      drawFeedTurnEnded(ended({ case: "errored", value: errored }), contextWithRow(null)),
    ).toThrow(MalformedView);
  });

  it("distinguishes the cut response from the refused request by headline", () => {
    const cut = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "cut short at the output ceiling" },
          error: { case: "maxTokens", value: {} },
        }),
      }),
      contextWithRow(null),
    ).querySelector(".turn-ended-cause")?.textContent;
    const refused = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          headline: { text: "refused: asked for more output than the model produces" },
          error: { case: "maxOutputTokens", value: {} },
        }),
      }),
      contextWithRow(null),
    ).querySelector(".turn-ended-cause")?.textContent;
    expect(cut).not.toBe(refused);
  });

  it("refuses an errored row whose cause arm is unset", () => {
    expect(() =>
      drawFeedTurnEnded(
        ended({ case: "errored", value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },}) }),
        contextWithRow(null),
      ),
    ).toThrow(MalformedView);
  });
});

describe("drawFeedTurnEnded: the retry countdown", () => {
  it("counts down from the turn's end plus the vendor's wait", () => {
    const el = drawFeedTurnEnded(
      ended(
        {
          case: "errored",
          value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
            error: { case: "rateLimited", value: { retryAfterMs: 30_000n } },
          }),
        },
        1_000_000n,
      ),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-retry")?.textContent).toBe("retry in 30s");
  });

  it("ticks the figure down on the shared clock", () => {
    const el = drawFeedTurnEnded(
      ended(
        {
          case: "errored",
          value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
            error: { case: "overloaded", value: { retryAfterMs: 30_000n } },
          }),
        },
        1_000_000n,
      ),
      contextWithRow(null),
    );
    document.body.append(el);
    vi.advanceTimersByTime(10_000);
    expect(el.querySelector(".turn-ended-retry")?.textContent).toBe("retry in 20s");
  });

  it("says the wait is over once the deadline passes", () => {
    const el = drawFeedTurnEnded(
      ended(
        {
          case: "errored",
          value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
            error: { case: "rateLimited", value: { retryAfterMs: 1_000n } },
          }),
        },
        1_000_000n,
      ),
      contextWithRow(null),
    );
    vi.advanceTimersByTime(5_000);
    expect(el.querySelector(".turn-ended-retry")?.textContent).toBe("ready to retry");
  });

  it("stops the countdown the moment it expires, rather than rewriting its last line", () => {
    // Arrange: a wait that runs out a second after the turn ended.
    const ticker = countingTicker();
    const el = drawFeedTurnEnded(
      ended(
        {
          case: "errored",
          value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
            error: { case: "rateLimited", value: { retryAfterMs: 1_000n } },
          }),
        },
        1_000_000n,
      ),
      contextWithRow(null, ticker),
    );
    document.body.append(el);
    // Act.
    vi.advanceTimersByTime(5_000);
    // Assert: an expired countdown holds no subscription.
    expect(ticker.live()).toBe(0);
    el.remove();
  });

  it("keeps counting while the wait is still running", () => {
    const ticker = countingTicker();
    const el = drawFeedTurnEnded(
      ended(
        {
          case: "errored",
          value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
            error: { case: "rateLimited", value: { retryAfterMs: 30_000n } },
          }),
        },
        1_000_000n,
      ),
      contextWithRow(null, ticker),
    );
    document.body.append(el);
    vi.advanceTimersByTime(5_000);
    expect(ticker.live()).toBe(1);
    el.remove();
  });

  it("words an UNSET wait as its own fact, distinct from a zero", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" },
          error: { case: "rateLimited", value: {} },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-retry")?.textContent).toBe("retry when ready");
  });

  it("draws no countdown on a cause that carries no wait", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, { headline: { text: "the turn died" }, error: { case: "notFound", value: {} } }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-retry")).toBeNull();
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
        }),
      }),
      contextWithRow(null),
    );
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-turn-error");
    expect(record.level.case).toBe("info");
  });
});

/** An errored ending with this arm and headline. */
function erroredEnding(arm: string, headline: string): FeedTurnEnded {
  return ended({
    case: "errored",
    value: create(FeedTurnEndedErroredSchema, {
      headline: { text: headline },
      error: { case: arm as never, value: {} as never },
    }),
  });
}

describe("drawFeedTurnEnded: the ended-turn bubble (owner ruling 2026-09-24)", () => {
  it("draws no bubble for a normal completed turn", () => {
    // ACT
    const el = drawFeedTurnEnded(ended({ case: "concluded", value: {} }), contextWithRow(null));
    // ASSERT
    expect(el.querySelector(`.${TURN_ENDED_BUBBLE_CLASS}`)).toBeNull();
  });

  it("draws a failed turn's bubble as a response bubble of the turn-ended variant", () => {
    // ACT
    const el = drawFeedTurnEnded(erroredEnding("internal", "the vendor failed"), contextWithRow(null));
    // ASSERT
    const bubble = el.querySelector(`.${TURN_ENDED_BUBBLE_CLASS}`);
    expect([bubble?.getAttribute("data-role"), bubble?.getAttribute("data-variant")]).toEqual([
      "response",
      "turn-ended",
    ]);
  });

  it("states the daemon's headline in a failed turn's bubble", () => {
    // ACT
    const el = drawFeedTurnEnded(erroredEnding("internal", "the vendor failed"), contextWithRow(null));
    // ASSERT
    expect(el.querySelector(`.${TURN_ENDED_BUBBLE_SAYS_CLASS}`)?.textContent).toBe("the vendor failed");
  });

  it("states the agent process's death in its bubble", () => {
    // ACT
    const el = drawFeedTurnEnded(
      erroredEnding("agentProcessDied", "the agent process died, and the turn it was running ended with it"),
      contextWithRow(null),
    );
    // ASSERT
    expect(el.querySelector(`.${TURN_ENDED_BUBBLE_SAYS_CLASS}`)?.textContent).toBe(
      "the agent process died, and the turn it was running ended with it",
    );
  });

  it("states an interrupt in plain words in its bubble", () => {
    // ACT
    const el = drawFeedTurnEnded(ended({ case: "interrupted", value: {} }), contextWithRow(null));
    // ASSERT
    expect(el.querySelector(`.${TURN_ENDED_BUBBLE_SAYS_CLASS}`)?.textContent).toBe(INTERRUPTED_SENTENCE);
  });

  it("draws the bubble above the row's own line", () => {
    // ACT
    const el = drawFeedTurnEnded(ended({ case: "interrupted", value: {} }), contextWithRow(null));
    // ASSERT
    expect([...el.children].map((child) => child.classList.contains(TURN_ENDED_BUBBLE_CLASS))).toEqual([
      true,
      false,
    ]);
  });

  it("updates the bubble in place on a re-push", () => {
    // ARRANGE
    const first = drawFeedTurnEnded(erroredEnding("internal", "first"), contextWithRow(null));
    const bubble = first.querySelector(`.${TURN_ENDED_BUBBLE_CLASS}`);
    const { ctx } = harness({});
    // ACT
    const second = drawFeedTurnEnded(
      erroredEnding("internal", "second"),
      rowContext(ctx, userPromptRow("p1", "hi"), { previous: first, findRowElement: () => null }),
    );
    // ASSERT
    expect(second.querySelector(`.${TURN_ENDED_BUBBLE_CLASS}`)).toBe(bubble);
  });
});
