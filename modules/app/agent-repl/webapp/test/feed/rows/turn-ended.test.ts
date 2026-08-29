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
  TURN_ERROR_ARMS,
  drawFeedTurnEnded,
} from "../../../src/feed/rows/turn-ended.js";
import { feedId, harness, rowContext, userPromptRow } from "../harness.js";

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
function contextWithRow(element: HTMLElement | null) {
  const { ctx } = harness();
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

  it("puts the existing green-border class on the answering response bubble", () => {
    const row = document.createElement("article");
    const bubble = document.createElement("div");
    bubble.className = "bubble assistant";
    row.append(bubble);
    drawFeedTurnEnded(
      ended({ case: "concluded", value: { answer: feedId("r1") } }),
      contextWithRow(row),
    );
    expect(bubble.classList.contains("final-response")).toBe(true);
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

  it("words every arm the schema declares, and no others", () => {
    expect([...TURN_ERROR_ARMS].sort()).toEqual([...schemaArms].sort());
  });

  it.each(schemaArms)("draws %s distinctly, by its own arm", (arm) => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          error: { case: arm as never, value: { type: "x" } as never },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.getAttribute("data-turn-error")).toBe(arm);
  });

  it("names the vendor's own type on the unmodeled arm", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          error: { case: "vendorUnmodeled", value: { type: "teapot_error" } },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.textContent).toContain("teapot_error");
  });

  it("draws the vendor's sentence when the record carried one", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
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
        value: create(FeedTurnEndedErroredSchema, {
          error: { case: "queryDied", value: {} },
        }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-vendor")).toBeNull();
  });

  it("distinguishes the cut response from the refused request", () => {
    const cut = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, { error: { case: "maxTokens", value: {} } }),
      }),
      contextWithRow(null),
    ).textContent;
    const refused = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
          error: { case: "maxOutputTokens", value: {} },
        }),
      }),
      contextWithRow(null),
    ).textContent;
    expect(cut).not.toBe(refused);
  });

  it("refuses an errored row whose cause arm is unset", () => {
    expect(() =>
      drawFeedTurnEnded(
        ended({ case: "errored", value: create(FeedTurnEndedErroredSchema, {}) }),
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
          value: create(FeedTurnEndedErroredSchema, {
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
          value: create(FeedTurnEndedErroredSchema, {
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
          value: create(FeedTurnEndedErroredSchema, {
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

  it("words an UNSET wait as its own fact, distinct from a zero", () => {
    const el = drawFeedTurnEnded(
      ended({
        case: "errored",
        value: create(FeedTurnEndedErroredSchema, {
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
        value: create(FeedTurnEndedErroredSchema, { error: { case: "notFound", value: {} } }),
      }),
      contextWithRow(null),
    );
    expect(el.querySelector(".turn-ended-retry")).toBeNull();
  });
});
