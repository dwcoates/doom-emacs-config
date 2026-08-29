// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedPageSchema,
  FeedRowSchema,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { GetFeedPageResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  createFeedController,
  isBubbleRow,
  type BubbleLike,
  type FeedController,
} from "../../src/feed/feed-view.js";
import { defaultBubbleBody, type RowContext } from "../../src/feed/renderers.js";
import {
  feedId,
  harness,
  mergeRow,
  mergeTabRow,
  page,
  responseRow,
  stubRenderers,
  subagentRow,
  userPromptRow,
  type Harness,
} from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** A stub bubble: an element and a record of what was asked of it. */
function stubBubble(row: FeedRow): BubbleLike & { updates: number } {
  const el = document.createElement("div");
  el.className = "stub-bubble";
  el.setAttribute("data-bubble", row.id?.value ?? "");
  return {
    element: el,
    updates: 0,
    update(): void {
      this.updates += 1;
    },
    expand: async () => true,
    isExpanded: () => false,
    child: () => null,
    dispose: () => el.remove(),
  };
}

interface Fixture {
  h: Harness;
  host: HTMLElement;
  controller: FeedController;
  bubbles: Map<string, ReturnType<typeof stubBubble>>;
}

function fixture(h: Harness = harness(), overrides: Partial<RowContext> = {}): Fixture {
  const host = document.createElement("div");
  document.body.replaceChildren(host);
  const bubbles = new Map<string, ReturnType<typeof stubBubble>>();
  const controller = createFeedController({
    ctx: h.ctx,
    host,
    feed: "root",
    renderers: stubRenderers(),
    body: defaultBubbleBody,
    revealRow: async () => false,
    bubble: (row) => {
      const bubble = stubBubble(row);
      bubbles.set(row.id?.value ?? "", bubble);
      return bubble;
    },
    bodyContext: {
      ctx: h.ctx,
      feed: "root",
      row: create(FeedRowSchema, {}),
      revealRow: async () => false,
      ...overrides,
    },
  });
  return { h, host, controller, bubbles };
}

/** The row ids the host currently draws, in order. */
function drawnIds(host: HTMLElement): string[] {
  return [...host.querySelectorAll(".feed-rows > [data-feed-row]")].map(
    (el) => el.getAttribute("data-feed-row") ?? "",
  );
}

async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("isBubbleRow", () => {
  it("recognizes a sync subagent", () => {
    expect(isBubbleRow(subagentRow("b"))).toBe(true);
  });

  it("recognizes a detached subagent", () => {
    expect(isBubbleRow(subagentRow("b", { detached: true }))).toBe(true);
  });

  it("recognizes a merge", () => {
    expect(isBubbleRow(mergeRow("m"))).toBe(true);
  });

  it("does not mistake an ordinary activity for a bubble", () => {
    expect(isBubbleRow(responseRow("r"))).toBe(false);
  });
});

describe("createFeedController: painting a page", () => {
  it("names the feed it draws", () => {
    const { host } = fixture();
    expect(host.getAttribute("data-feed")).toBe("root");
  });

  it("paints a page oldest → newest, in the served order", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1"), responseRow("b")]), "replace");
    expect(drawnIds(host)).toEqual(["a", "b"]);
  });

  it("stamps each row with its own kind", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1")]), "replace");
    expect(host.querySelector("[data-feed-row]")?.getAttribute("data-row-kind")).toBe("userPrompt");
  });

  it("stamps an activity row with its unit as well", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("b")]), "replace");
    expect(host.querySelector('[data-feed-row="b"]')?.getAttribute("data-unit")).toBe("response");
  });

  it("stamps the turn a row belongs to, so a composer can find its prompt", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    expect(host.querySelector('[data-feed-row="a"]')?.getAttribute("data-turn")).toBe("turn-7");
  });

  it("stamps no turn on a row that belongs to none", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1")]), "replace");
    expect(host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-turn")).toBe(false);
  });

  it("dispatches an activity unit to its own renderer", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("b")]), "replace");
    expect(host.querySelector(".stub-response")).not.toBeNull();
  });

  it("replaces the rows whole on a newest page, accumulating nothing", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.applyPage(page([responseRow("b")]), "replace");
    expect(drawnIds(host)).toEqual(["b"]);
  });

  it("refuses a page whose result arm is unset", () => {
    const { controller } = fixture();
    expect(() => controller.applyPage(create(FeedPageSchema, {}), "replace")).toThrow(
      MalformedView,
    );
  });
});

describe("createFeedController: upserts", () => {
  it("APPENDS an id it has not seen", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.upsert(responseRow("b"));
    expect(drawnIds(host)).toEqual(["a", "b"]);
  });

  it("REPLACES a known id in place, which is how a response grows", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a", "one"), responseRow("b")]), "replace");
    controller.upsert(responseRow("a", "two"));
    expect(drawnIds(host)).toEqual(["a", "b"]);
  });

  it("redraws the replaced row's body", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const before = host.querySelector('[data-feed-row="a"]')?.firstElementChild;
    controller.upsert(responseRow("a", "two"));
    expect(host.querySelector('[data-feed-row="a"]')?.firstElementChild).not.toBe(before);
  });

  it("keeps the row element itself across a replacement", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const element = host.querySelector('[data-feed-row="a"]');
    controller.upsert(responseRow("a", "two"));
    expect(host.querySelector('[data-feed-row="a"]')).toBe(element);
  });

  it("refuses a row with no id, the id being the upsert key", () => {
    const { controller } = fixture();
    expect(() => controller.upsert(create(FeedRowSchema, {}))).toThrow(MalformedView);
  });
});

describe("createFeedController: nesting", () => {
  it("nests a row inside the container it names", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b", "x", "a")]), "replace");
    expect(host.querySelector('[data-feed-row="a"] [data-nest] [data-feed-row="b"]')).not.toBeNull();
  });

  it("draws a row whose container is unknown at the top level rather than dropping it", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("b", "x", "never-seen")]), "replace");
    expect(drawnIds(host)).toEqual(["b"]);
  });
});

describe("createFeedController: the walk", () => {
  it("shows the load-more control while older rows exist", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    expect(host.querySelector<HTMLElement>("[data-load-more]")?.hidden).toBe(false);
  });

  it("hides it once the walk reaches the start", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(host.querySelector<HTMLElement>("[data-load-more]")?.hidden).toBe(true);
  });

  it("asks for the NEXT page, continuing the daemon's own walk", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: { case: "success", value: page([responseRow("older")]) },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(h.calls.getFeedPage[0]?.page.case).toBe("next");
  });

  it("prepends the older page above what is already drawn", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: { case: "success", value: page([responseRow("older")]) },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(drawnIds(host)).toEqual(["older", "a"]);
  });

  it("draws the daemon's refusal at the control that asked", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("error");
  });

  it("says so when the walk never reached the daemon", async () => {
    const h = harness({
      getFeedPage: () => {
        throw new Error("gone");
      },
    });
    const { controller, host } = fixture(h);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("createFeedController: the page error", () => {
  /** A page that could not be completed. */
  function errorPage() {
    return create(FeedPageSchema, {
      result: {
        case: "error",
        value: {
          headline: { text: "history has a gap", tone: "red" },
          kind: {
            case: "historyReplayTruncated",
            value: { fromSeq: 1n, stopAtSeq: 9n, delivered: 2n, reason: "store closed" },
          },
        },
      },
    });
  }

  it("draws the daemon's own sentence where the rows would be", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector(".feed-page-error-headline")?.textContent).toBe("history has a gap");
  });

  it("names the typed evidence's arm", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector("[data-page-error]")?.getAttribute("data-page-error")).toBe(
      "historyReplayTruncated",
    );
  });

  it("draws the evidence's own reason", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector(".feed-page-error-evidence")?.textContent).toBe("store closed");
  });

  it("paints the headline in the tone the daemon chose", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector(".feed-page-error")?.className).toContain("tone-red");
  });

  it("refuses a tone outside the shared vocabulary", () => {
    const { controller } = fixture();
    const bad = create(FeedPageSchema, {
      result: {
        case: "error",
        value: {
          headline: { text: "x", tone: "teal" },
          kind: { case: "historyReplayTruncated", value: { reason: "r" } },
        },
      },
    });
    expect(() => controller.applyPage(bad, "replace")).toThrow(MalformedView);
  });
});

describe("createFeedController: a malformed row", () => {
  /** A response row whose unit arm is unset. */
  function unreadableRow(): FeedRow {
    return create(FeedRowSchema, {
      id: feedId("bad"),
      row: { case: "activity", value: {} },
    });
  }

  it("draws the compact placeholder rather than blanking the feed", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([unreadableRow(), responseRow("ok")]), "replace");
    expect(host.querySelector(".row-malformed")).not.toBeNull();
  });

  it("keeps drawing every other row", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([unreadableRow(), responseRow("ok")]), "replace");
    expect(host.querySelector(".stub-response")).not.toBeNull();
  });

  it("names the path the refusal happened at", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([unreadableRow()]), "replace");
    expect(host.querySelector(".row-malformed")?.textContent).toContain("FeedTurnActivity.unit");
  });

  it("reports the failure once, as frame_undecodable", () => {
    const { controller, h } = fixture();
    controller.applyPage(page([unreadableRow()]), "replace");
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });

  it("refuses a merge tab that arrived on a feed that is not a merge bubble", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([mergeTabRow("t")]), "replace");
    // The tab is not drawn as a row of its own, so nothing is placed for it.
    expect(drawnIds(host)).toEqual([]);
  });
});

describe("createFeedController: bubbles", () => {
  it("hands a bubble row to the bubble factory rather than a card renderer", () => {
    const { controller, bubbles } = fixture();
    controller.applyPage(page([subagentRow("b1")]), "replace");
    expect(bubbles.has("b1")).toBe(true);
  });

  it("puts the bubble's own element in the row", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([subagentRow("b1")]), "replace");
    expect(host.querySelector('[data-feed-row="b1"] .stub-bubble')).not.toBeNull();
  });

  it("UPDATES the bubble on a re-push rather than rebuilding it", () => {
    const { controller, bubbles } = fixture();
    controller.applyPage(page([subagentRow("b1")]), "replace");
    const element = bubbles.get("b1")?.element;
    controller.upsert(subagentRow("b1", { tokens: "9k" }));
    expect(bubbles.get("b1")?.element).toBe(element);
  });

  it("counts the re-push as an update on the bubble", () => {
    const { controller, bubbles } = fixture();
    controller.applyPage(page([subagentRow("b1")]), "replace");
    controller.upsert(subagentRow("b1", { tokens: "9k" }));
    expect(bubbles.get("b1")?.updates).toBe(1);
  });
});

describe("createFeedController: the rolling highlight", () => {
  it("marks the newest user prompt", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    expect(host.querySelector('[data-feed-row="p1"]')?.getAttribute("data-latest-prompt")).toBe(
      "true",
    );
  });

  it("moves to the newer prompt when one arrives", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    controller.upsert(userPromptRow("p2", "two"));
    expect(host.querySelector('[data-feed-row="p2"]')?.getAttribute("data-latest-prompt")).toBe(
      "true",
    );
  });

  it("leaves the older prompt unmarked once it has moved", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    controller.upsert(userPromptRow("p2", "two"));
    expect(host.querySelector('[data-feed-row="p1"]')?.hasAttribute("data-latest-prompt")).toBe(
      false,
    );
  });

  it("marks nothing on a feed with no prompt at all", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("r")]), "replace");
    expect(host.querySelector("[data-latest-prompt]")).toBeNull();
  });
});

describe("createFeedController: lookups and disposal", () => {
  it("finds a row's element by its id", () => {
    const { controller } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(controller.findRowElement(feedId("a"))).not.toBeNull();
  });

  it("answers null for a row this feed does not hold", () => {
    const { controller } = fixture();
    expect(controller.findRowElement(feedId("nope"))).toBeNull();
  });

  it("reports the rows in order for a body renderer", () => {
    const { controller } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    expect(controller.rows().map((row) => row.id?.value)).toEqual(["a", "b"]);
  });

  it("keeps merge tabs in the rows, since the merge body consumes them there", () => {
    const { controller } = fixture();
    controller.applyPage(page([mergeTabRow("t")]), "replace");
    expect(controller.rows()).toHaveLength(1);
  });

  it("empties its host on dispose", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.dispose();
    expect(host.children).toHaveLength(0);
  });
});
