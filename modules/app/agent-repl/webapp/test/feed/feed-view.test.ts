// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import {
  FeedBreadcrumbSchema,
  FeedIdSchema,
  FeedPageSchema,
  FeedRowRemovedSchema,
  FeedRowSchema,
  FeedSelectionSchema,
  FeedTurnEndedInterruptedInterjectionSchema,
  type FeedId,
  type FeedResponse,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { GetFeedPageResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { TurnIdSchema } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import {
  forgetOwnTurns,
  rememberOwnTurn,
} from "../../src/composer/own-turns.js";
import {
  createFeedController,
  isBubbleRow,
  type BubbleLike,
  type FeedController,
} from "../../src/feed/feed-view.js";
import {
  defaultBubbleBody,
  type RowContext,
} from "../../src/feed/renderers.js";
import {
  SELECTED_ENTRY_CLASS,
  SELECTED_ROW_ATTRIBUTE,
} from "../../src/feed/selected-entry.js";
import type { SelectionVisibility } from "../../src/feed/selection-visibility.js";
import type { Overscan } from "../../src/feed/overscan.js";
import { drawFeedSimpleToolCall } from "../../src/feed/cards/tool-call.js";
import { onDiscard, tickWhileShown } from "../../src/feed/ticking.js";
import { FIXTURE_WALK,
  agentPromptRow,
  countingTicker,
  feedId,
  harness,
  toolCallRow,
  type CountingTicker,
  mergeRow,
  mergeTabRow,
  page,
  responseRow,
  selectionOf,
  separationRow,
  stubRenderers,
  subagentRow,
  turnEndedRow,
  userPromptRow,
  type Harness,
} from "./harness.js";
import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
} from "../../src/breathing.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { orderFor, withOrder, withoutOrder } from "../feed-order.js";
import {
  TailFollow,
  centerDelta,
  type CenterGeometry,
} from "../../src/scroll.js";
import { responseCapLines } from "../../src/feed/cards/response.js";
import { codeOf } from "../source-text.js";
import { EXPANDED_CLASS } from "../../src/expand.js";
import { selectable } from "../selectable.js";

/** A scroll box's rect, for fixtures whose box is never measured for a collapse. */
const boxRect = (): DOMRect => ({ top: 0 }) as DOMRect;

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
    collapse: () => undefined,
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

function fixture(
  h: Harness = harness(),
  overrides: Partial<RowContext> = {},
  opts: {
    feed?: FeedId;
    renderers?: Partial<Parameters<typeof stubRenderers>[0]>;
    overscan?: Overscan;
  } = {},
): Fixture {
  const host = document.createElement("div");
  document.body.replaceChildren(host);
  const bubbles = new Map<string, ReturnType<typeof stubBubble>>();
  const controller = createFeedController({
    ctx: h.ctx,
    host,
    feed: opts.feed ?? "root",
    renderers: stubRenderers(opts.renderers),
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
    overscan: opts.overscan,
  });
  return { h, host, controller, bubbles };
}

/** An overscan spy: records the elements it is asked to observe and unobserve. */
function spyOverscan(): Overscan & {
  observed: HTMLElement[];
  unobserved: HTMLElement[];
} {
  const observed: HTMLElement[] = [];
  const unobserved: HTMLElement[] = [];
  return {
    observed,
    unobserved,
    observe: (row) => observed.push(row),
    unobserve: (row) => unobserved.push(row),
    dispose: () => {},
  };
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

  it("recognizes a detached shell head", () => {
    const row = create(FeedRowSchema, {
      id: feedId("s"),
      order: orderFor("s"),
      row: { case: "shellHead", value: { command: { text: "npm run dev" } } },
    });
    expect(isBubbleRow(row)).toBe(true);
  });

  it("does not mistake a shell's spool body for a bubble", () => {
    // The spool BODY (detached_shell) rides the sub-feed as an ordinary row; only
    // the HEAD (shell_head) is the expandable bubble.
    const row = create(FeedRowSchema, {
      id: feedId("s"),
      order: orderFor("s"),
      row: { case: "detachedShell", value: { shell: {} } },
    });
    expect(isBubbleRow(row)).toBe(false);
  });
});

describe("createFeedController: painting a page", () => {
  it("names the feed it draws", () => {
    const { host } = fixture();
    expect(host.getAttribute("data-feed")).toBe("root");
  });

  it("paints a page oldest → newest, in the served order", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("a", "1"), responseRow("b")]),
      "replace",
    );
    expect(drawnIds(host)).toEqual(["a", "b"]);
  });

  it("stamps each row with its own kind", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1")]), "replace");
    expect(
      host.querySelector("[data-feed-row]")?.getAttribute("data-row-kind"),
    ).toBe("userPrompt");
  });

  it("stamps an activity row with its unit as well", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("b")]), "replace");
    expect(
      host.querySelector('[data-feed-row="b"]')?.getAttribute("data-unit"),
    ).toBe("response");
  });

  it("stamps the turn a row belongs to, so a composer can find its prompt", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    expect(
      host.querySelector('[data-feed-row="a"]')?.getAttribute("data-turn"),
    ).toBe("turn-7");
  });

  it("marks a row whose turn this page submitted", () => {
    const { controller, host } = fixture();
    rememberOwnTurn(create(TurnIdSchema, { value: "turn-7" }));
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    expect(
      host.querySelector('[data-feed-row="a"]')?.getAttribute("data-mine"),
    ).toBe("true");
    forgetOwnTurns();
  });

  it("makes no claim on a row from another submitter's turn", () => {
    const { controller, host } = fixture();
    rememberOwnTurn(create(TurnIdSchema, { value: "turn-mine" }));
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    expect(
      host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-mine"),
    ).toBe(false);
    forgetOwnTurns();
  });

  it("drops the turn from a re-pushed row that no longer names one", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    controller.upsert(userPromptRow("a", "1"));
    expect(
      host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-turn"),
    ).toBe(false);
  });

  it("stamps no turn on a row that belongs to none", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("a", "1")]), "replace");
    expect(
      host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-turn"),
    ).toBe(false);
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
    expect(() =>
      controller.applyPage(create(FeedPageSchema, {}), "replace"),
    ).toThrow(MalformedView);
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
    controller.applyPage(
      page([responseRow("a", "one"), responseRow("b")]),
      "replace",
    );
    controller.upsert(responseRow("a", "two"));
    expect(drawnIds(host)).toEqual(["a", "b"]);
  });

  it("redraws the replaced row's body", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const before = host.querySelector('[data-feed-row="a"]')?.firstElementChild;
    controller.upsert(responseRow("a", "two"));
    expect(
      host.querySelector('[data-feed-row="a"]')?.firstElementChild,
    ).not.toBe(before);
  });

  it("keeps the row element itself across a replacement", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const element = host.querySelector('[data-feed-row="a"]');
    controller.upsert(responseRow("a", "two"));
    expect(host.querySelector('[data-feed-row="a"]')).toBe(element);
  });

  it("redraws nothing for a row re-pushed exactly as drawn", () => {
    // Arrange -- REMOVED TRIGGER: the daemon repaints its opening page on
    // every turn open, and each unchanged row used to be rebuilt.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const before = host.querySelector('[data-feed-row="a"]')?.firstElementChild;
    // Act
    controller.upsert(responseRow("a", "one"));
    // Assert
    expect(host.querySelector('[data-feed-row="a"]')?.firstElementChild).toBe(
      before,
    );
  });

  it("does not ask the tail owner to follow for an unchanged re-push", () => {
    // Arrange
    const h = harness();
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const follows: number[] = [];
    const tail = {
      follow: () => follows.push(1),
      initialPlacement: () => undefined,
    };
    const controller = createFeedController({
      ctx: h.ctx,
      host,
      feed: "root",
      renderers: stubRenderers(),
      body: defaultBubbleBody,
      revealRow: async () => false,
      bubble: (row) => stubBubble(row),
      bodyContext: {
        ctx: h.ctx,
        feed: "root",
        row: create(FeedRowSchema, {}),
        revealRow: async () => false,
      },
      scroll: {
        box: {
          scrollTop: 0,
          scrollHeight: 0,
          clientHeight: 0,
          getBoundingClientRect: boxRect,
        },
        tail: tail as never,
      },
    });
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    follows.length = 0;
    // Act
    controller.upsert(responseRow("a", "one"));
    // Assert
    expect(follows).toEqual([]);
  });

  it("records an unchanged re-push at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { controller } = fixture();
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    // Act
    controller.upsert(responseRow("a", "one"));
    // Assert
    const record = await forwardedRecord(capture, "feed.row-unchanged");
    expect(record.context).toMatchObject({ feed: "root", row: "a" });
  });

  it("leaves a body its renderer updated in place in the document", () => {
    // Arrange -- a renderer that hands back the element it drew before, as the
    // response bubble does so its scroll box keeps the reader's position.
    const inPlace = (_u: unknown, rc: RowContext): HTMLElement =>
      rc.previous ?? document.createElement("div");
    const { controller, host } = fixture(
      undefined,
      {},
      { renderers: { response: inPlace } },
    );
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const row = host.querySelector('[data-feed-row="a"]');
    if (row === null) throw new Error("row a is not drawn");
    const observer = new MutationObserver(() => undefined);
    observer.observe(row, { childList: true });
    // Act
    controller.upsert(responseRow("a", "two"));
    // Assert -- nothing was removed from the row, so nothing re-attached.
    expect(
      observer.takeRecords().flatMap((record) => [...record.removedNodes]),
    ).toEqual([]);
  });

  it("keeps a box the reader scrolled when its card is re-pushed", () => {
    // Arrange -- a card whose output box the reader scrolled 80px into.
    const boxed = (): HTMLElement => {
      const card = document.createElement("div");
      const box = document.createElement("pre");
      box.className = "tool-output";
      card.append(box);
      return card;
    };
    const { controller, host } = fixture(
      undefined,
      {},
      { renderers: { response: boxed } },
    );
    controller.applyPage(page([responseRow("a", "one")]), "replace");
    const box = host.querySelector<HTMLElement>(
      '[data-feed-row="a"] .tool-output',
    );
    if (box === null) throw new Error("the card drew no box");
    box.scrollTop = 80;
    // Act
    controller.upsert(responseRow("a", "two"));
    // Assert -- the same box, still where the reader left it.
    expect([
      host.querySelector('[data-feed-row="a"] .tool-output') === box,
      box.scrollTop,
    ]).toEqual([true, 80]);
  });

  it("refuses a row with no id, the id being the upsert key", () => {
    const { controller } = fixture();
    expect(() => controller.upsert(create(FeedRowSchema, {}))).toThrow(
      MalformedView,
    );
  });
});

// THE FEED BEGINS AT THE NEWEST SEPARATION. A compaction or a clear arriving on
// the tail says the rows above it are no longer the conversation; the daemon
// stops serving them, and the client stops showing them.

describe("createFeedController: the newest separation bounds the feed", () => {
  it("drops every row above a compaction that arrives", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    controller.upsert(separationRow("cut"));
    expect(drawnIds(host)).toEqual(["cut"]);
  });

  it("drops every row above a clear that arrives", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.upsert(separationRow("cut", "cleared"));
    expect(drawnIds(host)).toEqual(["cut"]);
  });

  it("keeps the rows that arrive after the separation", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.upsert(separationRow("cut"));
    controller.upsert(responseRow("b"));
    expect(drawnIds(host)).toEqual(["cut", "b"]);
  });

  it("collapses consecutive compactions onto the newest divider", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.upsert(separationRow("cut-1"));
    controller.upsert(responseRow("b"));
    controller.upsert(separationRow("cut-2"));
    expect(drawnIds(host)).toEqual(["cut-2"]);
  });

  it("keeps the rows above a compaction that FAILED, which cut nothing", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.upsert(separationRow("cut", "compactionFailed"));
    expect(drawnIds(host)).toEqual(["a", "cut"]);
  });

  it("records the drop at INFO with the count it dropped", async () => {
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    controller.upsert(separationRow("cut"));
    const record = await forwardedRecord(
      capture,
      "feed.truncated-at-separation",
    );
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ dropped: 2 });
  });
});

// A REMOVAL is the DUAL of an upsert, delivered live on the same tail: the
// daemon retired a row (a foreground Bash that detached, its running tool card
// retired in favor of the shell bubble), so an already-open feed must drop it
// live rather than show it stale until a reload.

/** A removal row: only its id, and the arm that says "drop the row it keys". */
function removedRow(id: string): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: { case: "removed", value: create(FeedRowRemovedSchema, {}) },
  });
}

describe("createFeedController: a live removal drops the row", () => {
  it("drops the removed row's element from the feed", () => {
    // Arrange.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    // Act.
    controller.upsert(removedRow("a"));
    // Assert.
    expect(drawnIds(host)).toEqual(["b"]);
  });

  it("disposes the removed row's bubble", () => {
    // Arrange: a bubble row, its dispose counted.
    const { controller, bubbles } = fixture();
    controller.applyPage(page([subagentRow("b1")]), "replace");
    let disposals = 0;
    const bubble = bubbles.get("b1")!;
    const inner = bubble.dispose.bind(bubble);
    bubble.dispose = () => {
      disposals += 1;
      inner();
    };
    // Act.
    controller.upsert(removedRow("b1"));
    // Assert.
    expect(disposals).toBe(1);
  });

  it("stops the removed row's clocks", () => {
    // Arrange: a running tool-call card holding a live clock.
    const ticker = countingTicker();
    const { controller } = fixture(
      harness({ ticker }),
      {},
      {
        renderers: { simpleToolCall: drawFeedSimpleToolCall },
      },
    );
    controller.applyPage(page([toolCallRow("t", "running")]), "replace");
    expect(ticker.live()).toBe(1);
    // Act.
    controller.upsert(removedRow("t"));
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("is a no-op for a row it does not hold", () => {
    // Arrange.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    // Act.
    controller.upsert(removedRow("ghost"));
    // Assert.
    expect(drawnIds(host)).toEqual(["a"]);
  });
});

// A FEED EMPTIED WHOLE. A workspace bound to a different vendor conversation
// has its feed RESET daemon-side: every row is retired at once, each as an
// ordinary removal on the same tail. The reader must end up looking at an
// empty feed and then at the newly bound conversation alone — never at the
// last rows of the conversation it no longer runs.

describe("createFeedController: a feed emptied whole", () => {
  it("draws nothing once every row has been removed", () => {
    // Arrange.
    const { controller, host } = fixture();
    controller.applyPage(
      page([responseRow("a"), responseRow("b"), responseRow("c")]),
      "replace",
    );
    // Act.
    for (const id of ["a", "b", "c"]) controller.upsert(removedRow(id));
    // Assert.
    expect(drawnIds(host)).toEqual([]);
  });

  it("draws the newly bound conversation alone after the emptying", () => {
    // Arrange.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    for (const id of ["a", "b"]) controller.upsert(removedRow(id));
    // Act.
    controller.upsert(responseRow("new-1"));
    // Assert.
    expect(drawnIds(host)).toEqual(["new-1"]);
  });

  it("disposes every emptied row's bubble", () => {
    // Arrange: two bubble rows, their disposals counted.
    const { controller, bubbles } = fixture();
    controller.applyPage(
      page([subagentRow("b1"), subagentRow("b2")]),
      "replace",
    );
    let disposals = 0;
    for (const id of ["b1", "b2"]) {
      const bubble = bubbles.get(id)!;
      const inner = bubble.dispose.bind(bubble);
      bubble.dispose = () => {
        disposals += 1;
        inner();
      };
    }
    // Act.
    for (const id of ["b1", "b2"]) controller.upsert(removedRow(id));
    // Assert.
    expect(disposals).toBe(2);
  });

  it("stops every emptied row's clocks", () => {
    // Arrange: two running tool-call cards, each holding a live clock.
    const ticker = countingTicker();
    const { controller } = fixture(
      harness({ ticker }),
      {},
      {
        renderers: { simpleToolCall: drawFeedSimpleToolCall },
      },
    );
    controller.applyPage(
      page([toolCallRow("t1", "running"), toolCallRow("t2", "running")]),
      "replace",
    );
    expect(ticker.live()).toBe(2);
    // Act.
    for (const id of ["t1", "t2"]) controller.upsert(removedRow(id));
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("draws a row id the emptying dropped as a fresh row rather than twice", () => {
    // Arrange: the new conversation happens to reuse an id of the old one.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    controller.upsert(removedRow("a"));
    // Act.
    controller.upsert(responseRow("a"));
    // Assert.
    expect(drawnIds(host)).toEqual(["a"]);
  });

  it("empties on a page that serves no rows at all", () => {
    // Arrange: the reader re-opens the feed after the bind instead of
    // streaming the removals, and the daemon serves it an empty newest page.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    // Act.
    controller.applyPage(page([]), "replace");
    // Assert.
    expect(drawnIds(host)).toEqual([]);
  });
});

// THE OVERSCAN WIRING: a row is handed to the pre-render buffer the moment its
// chrome is born and handed back the moment the feed drops it, so the buffer
// can force the layout of rows near the viewport without leaking a watch on a
// row that is no longer on the page.

describe("createFeedController: the overscan buffer watches a row's whole life", () => {
  it("hands a newly adopted row to the overscan to observe", () => {
    // Arrange.
    const overscan = spyOverscan();
    const { controller, host } = fixture(harness(), {}, { overscan });
    // Act.
    controller.applyPage(page([responseRow("a")]), "replace");
    // Assert.
    const row = host.querySelector<HTMLElement>('[data-feed-row="a"]');
    expect(overscan.observed).toContain(row);
  });

  it("hands a live-appended row to the overscan to observe", () => {
    // Arrange.
    const overscan = spyOverscan();
    const { controller, host } = fixture(harness(), {}, { overscan });
    // Act.
    controller.upsert(responseRow("a"));
    // Assert.
    const row = host.querySelector<HTMLElement>('[data-feed-row="a"]');
    expect(overscan.observed).toContain(row);
  });

  it("hands a removed row back to the overscan to unobserve, so nothing leaks", () => {
    // Arrange.
    const overscan = spyOverscan();
    const { controller, host } = fixture(harness(), {}, { overscan });
    controller.applyPage(page([responseRow("a")]), "replace");
    const row = host.querySelector<HTMLElement>('[data-feed-row="a"]');
    // Act.
    controller.upsert(removedRow("a"));
    // Assert.
    expect(overscan.unobserved).toContain(row);
  });

  it("hands rows dropped above a separation back to the overscan to unobserve", () => {
    // Arrange.
    const overscan = spyOverscan();
    const { controller, host } = fixture(harness(), {}, { overscan });
    controller.applyPage(page([responseRow("a")]), "replace");
    const row = host.querySelector<HTMLElement>('[data-feed-row="a"]');
    // Act: a separation lands after the row, so the row above it is truncated.
    controller.upsert(separationRow("s"));
    // Assert.
    expect(overscan.unobserved).toContain(row);
  });
});

describe("createFeedController: nesting", () => {
  it("nests a row inside the container it names", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([responseRow("a"), responseRow("b", "x", "a")]),
      "replace",
    );
    expect(
      host.querySelector('[data-feed-row="a"] [data-nest] [data-feed-row="b"]'),
    ).not.toBeNull();
  });

  it("draws a row whose container is unknown at the top level rather than dropping it", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([responseRow("b", "x", "never-seen")]),
      "replace",
    );
    expect(drawnIds(host)).toEqual(["b"]);
  });
});

describe("createFeedController: the walk", () => {
  it("shows the load-more control while older rows exist", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    expect(host.querySelector<HTMLElement>("[data-load-more]")?.hidden).toBe(
      false,
    );
  });

  it("takes it away once the walk reaches the start", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    // A feed at its start offers no way further back, and an inert control the
    // reader can see is a promise the feed cannot keep: it is detached, not
    // merely hidden.
    expect(host.querySelector("[data-load-more]")).toBeNull();
  });

  it("asks for the NEXT page, continuing the daemon's own walk", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: {
            case: "success",
            value: page([withOrder(responseRow("older"), "a")]),
          },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(h.calls.getFeedPage[0]?.page.case).toBe("next");
  });

  it("names the walk the last page with more named", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: { case: "success", value: page([withOrder(responseRow("older"), "a")]) },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    const ask = h.calls.getFeedPage[0]?.page;
    expect(ask?.case === "next" ? ask.value.walk?.value : undefined).toBe(FIXTURE_WALK);
  });

  it("prepends the older page above what is already drawn", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: {
            case: "success",
            value: page([withOrder(responseRow("older"), "a")]),
          },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(drawnIds(host)).toEqual(["older", "a"]);
  });

  it("draws the daemon's refusal at the control that asked", async () => {
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: { case: "error", value: {} },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe(
      "error",
    );
  });

  it("says so when the walk never reached the daemon", async () => {
    const h = harness({
      getFeedPage: () => {
        throw new Error("gone");
      },
    });
    const { controller, host } = fixture(h);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe(
      "transport",
    );
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
            value: {
              fromSeq: 1n,
              stopAtSeq: 9n,
              delivered: 2n,
              reason: "store closed",
            },
          },
        },
      },
    });
  }

  it("draws the daemon's own sentence where the rows would be", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector(".feed-page-error-headline")?.textContent).toBe(
      "history has a gap",
    );
  });

  it("names the typed evidence's arm", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(
      host.querySelector("[data-page-error]")?.getAttribute("data-page-error"),
    ).toBe("historyReplayTruncated");
  });

  it("draws the evidence's own reason", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector(".feed-page-error-evidence")?.textContent).toBe(
      "store closed",
    );
  });

  it("paints the headline in the tone the daemon chose", () => {
    const { controller, host } = fixture();
    controller.applyPage(errorPage(), "replace");
    expect(host.querySelector(".feed-page-error")?.className).toContain(
      "tone-red",
    );
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
      order: orderFor("bad"),
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
    expect(host.querySelector(".row-malformed")?.textContent).toContain(
      "FeedTurnActivity.unit",
    );
  });

  it("reports the failure once, as frame_undecodable", () => {
    const { controller, h } = fixture();
    controller.applyPage(page([unreadableRow()]), "replace");
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });

  it("draws a merge tab that arrived on a feed that is not a merge bubble", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([mergeTabRow("t")]), "replace");
    // INSIDE a merge bubble the strip consumes the tab and this path is never
    // reached; anywhere else the tab is still a row the daemon served, and
    // dropping it would hide a phase of a real run (src/feed/merge/tab-row.ts).
    expect(drawnIds(host)).toEqual(["t"]);
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
    expect(
      host.querySelector('[data-feed-row="b1"] .stub-bubble'),
    ).not.toBeNull();
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
    expect(
      host
        .querySelector('[data-feed-row="p1"]')
        ?.getAttribute("data-latest-prompt"),
    ).toBe("true");
  });

  it("moves to the newer prompt when one arrives", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    controller.upsert(userPromptRow("p2", "two"));
    expect(
      host
        .querySelector('[data-feed-row="p2"]')
        ?.getAttribute("data-latest-prompt"),
    ).toBe("true");
  });

  it("leaves the older prompt unmarked once it has moved", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    controller.upsert(userPromptRow("p2", "two"));
    expect(
      host
        .querySelector('[data-feed-row="p1"]')
        ?.hasAttribute("data-latest-prompt"),
    ).toBe(false);
  });

  it("marks nothing on a feed with no prompt at all", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("r")]), "replace");
    expect(host.querySelector("[data-latest-prompt]")).toBeNull();
  });
});

describe("createFeedController: the working prompt's thinking wave", () => {
  /** The prompt bubble inside row ID, as the feed drew it. */
  function promptBubble(host: HTMLElement, id: string): HTMLElement {
    const bubble = host.querySelector<HTMLElement>(
      `[data-feed-row="${id}"] .bubble.user`,
    );
    if (bubble === null)
      throw new Error(`no prompt bubble drawn for row ${id}`);
    return bubble;
  }

  it("waves a prompt whose row says its turn is working", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", true)]),
      "replace",
    );
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("does not wave a prompt whose row says its turn is not working", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", false)]),
      "replace",
    );
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      false,
    );
  });

  it("keeps waving a working prompt when a turn_ended row for its turn arrives", () => {
    // The row's flag is the whole answer; the terminal row infers nothing.
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", true)]),
      "replace",
    );
    controller.upsert(turnEndedRow("e1", "t1"));
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("keeps waving a working prompt when its turn's answer is marked final", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([
        userPromptRow("p1", "one", "t1", true),
        responseRow("r1", "the answer", undefined, "t1"),
        turnEndedRow("e1", undefined, "concluded", "r1"),
      ]),
      "replace",
    );
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("does not wave a prompt whose row is not working though no turn_ended arrived", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([
        userPromptRow("p1", "one", "t1", false),
        responseRow("r1", "going", undefined, "t1"),
      ]),
      "replace",
    );
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      false,
    );
  });

  it("does not wave an unstamped prompt whose row is not working", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      false,
    );
  });

  it("waves an agent-addressed prompt whose row says working", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([agentPromptRow("a1", "\u2192 Explore", "go", "t1", true)]),
      "replace",
    );
    expect(promptBubble(host, "a1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("stops the wave when the prompt row is re-pushed not working", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", true)]),
      "replace",
    );
    controller.upsert(userPromptRow("p1", "one", "t1", false));
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      false,
    );
  });

  it("starts the wave when the prompt row is re-pushed working", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", false)]),
      "replace",
    );
    controller.upsert(userPromptRow("p1", "one", "t1", true));
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("stops an agent prompt's wave when its row is re-pushed not working", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([agentPromptRow("a1", "\u2192 Explore", "go", "t1", true)]),
      "replace",
    );
    controller.upsert(
      agentPromptRow("a1", "\u2192 Explore", "go", "t1", false),
    );
    expect(promptBubble(host, "a1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      false,
    );
  });

  it("stops the wave without redrawing the bubble it stopped", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", true)]),
      "replace",
    );
    const before = promptBubble(host, "p1");
    controller.upsert(userPromptRow("p1", "one", "t1", false));
    expect(promptBubble(host, "p1")).toBe(before);
  });

  it("stops the wave without redrawing the prompt's text", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", true)]),
      "replace",
    );
    const body = promptBubble(host, "p1").querySelector(".bubble-body");
    controller.upsert(userPromptRow("p1", "one", "t1", false));
    expect(promptBubble(host, "p1").querySelector(".bubble-body")).toBe(body);
  });

  it("redraws a re-push that changes more than the flag, drawing the new flag", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1", true)]),
      "replace",
    );
    controller.upsert(userPromptRow("p1", "one, edited", "t1", false));
    expect({
      text: promptBubble(host, "p1").textContent?.includes("one, edited"),
      waving: promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE),
    }).toEqual({ text: true, waving: false });
  });
});

describe("createFeedController: following the tail", () => {
  /**
   * A scroll box and a tail owner recording which named cause the feed asked
   * for. Every park records the rows drawn at that moment, so a test can say
   * what was already painted when the feed moved. The follow decision itself is
   * TailFollow's (test/scroll.test.ts); FOLLOWING only says whether this stub's
   * `follow` records a move.
   */
  function scrollStub(following: boolean, host: HTMLElement) {
    const acts: string[] = [];
    const parkedOver: string[][] = [];
    const shifts: number[] = [];
    const box = {
      scrollTop: 0,
      scrollHeight: 1000,
      clientHeight: 100,
      getBoundingClientRect: boxRect,
    };
    const park = (cause: string) => (): void => {
      acts.push(cause);
      parkedOver.push(drawnIds(host));
    };
    const tail = {
      isFollowing: () => following,
      follow: () => {
        if (following) park("follow")();
      },
      promptSent: park("promptSent"),
      initialPlacement: park("initialPlacement"),
      replaceRestore: park("replaceRestore"),
      prependCompensation: (grown: number) => shifts.push(grown),
    };
    return { box, tail, acts, parkedOver, shifts };
  }

  /** A controller wired to that box. */
  function scrolled(following: boolean) {
    const h = harness();
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const scroll = scrollStub(following, host);
    const controller = createFeedController({
      ctx: h.ctx,
      host,
      feed: "root",
      renderers: stubRenderers(),
      body: defaultBubbleBody,
      revealRow: async () => false,
      bubble: (row) => stubBubble(row),
      bodyContext: {
        ctx: h.ctx,
        feed: "root",
        row: create(FeedRowSchema, {}),
        revealRow: async () => false,
      },
      scroll: { box: scroll.box, tail: scroll.tail as never },
    });
    return { controller, host, ...scroll };
  }

  /**
   * Lay row ID out 400px per row above it, as jsdom lays out nothing: a row's
   * top moves down by 400 for every row that lands above it.
   */
  function layOutByIndex(host: HTMLElement, id: string): HTMLElement {
    const el = host.querySelector<HTMLElement>(`[data-feed-row="${id}"]`);
    if (el === null) throw new Error(`row ${id} is not drawn`);
    el.getBoundingClientRect = () => {
      const index =
        el.parentElement === null
          ? 0
          : [...el.parentElement.children].indexOf(el);
      return { top: 100 + 400 * index } as DOMRect;
    };
    return el;
  }

  it("asks the tail owner to keep a standing follow when a row arrives", () => {
    const { controller, acts } = scrolled(true);
    acts.length = 0;
    controller.upsert(responseRow("r1"));
    expect(acts).toContain("follow");
  });

  it("asks the tail owner to keep a standing follow when older rows land above", () => {
    const { controller, acts } = scrolled(true);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    acts.length = 0;
    controller.applyPage(
      page([withOrder(responseRow("older"), "a")]),
      "prepend",
    );
    expect(acts).toContain("follow");
  });

  it("places a feed's first page at its tail as initialPlacement", () => {
    // Arrange
    const { controller, acts } = scrolled(false);
    // Act
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    // Assert
    expect(acts).toEqual(["initialPlacement"]);
  });

  it("lands a later replace at the tail as replaceRestore", () => {
    // Arrange — the feed already painted once; a re-open replaces the page.
    const { controller, acts } = scrolled(false);
    controller.applyPage(page([responseRow("a")]), "replace");
    acts.length = 0;
    // Act
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    // Assert — there is no saved spot: a replace lands at the tail.
    expect(acts).toEqual(["replaceRestore"]);
  });

  it("paints the replaced rows before it parks", () => {
    // Arrange
    const { controller, parkedOver } = scrolled(false);
    // Act
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    // Assert
    expect(parkedOver).toEqual([["a", "b"]]);
  });

  it("records the park a replace makes", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { controller } = scrolled(false);
    // Act
    controller.applyPage(page([responseRow("a")]), "replace");
    // Assert
    const record = await forwardedRecord(capture, "feed.replace-parked");
    expect(record.context).toMatchObject({
      feed: "root",
      rows: 1,
      first: true,
    });
  });

  it("parks at the tail when a new prompt is drawn while the reader was scrolled up", () => {
    // Arrange
    const { controller, acts } = scrolled(false);
    controller.applyPage(page([responseRow("a")]), "replace");
    acts.length = 0;
    // Act
    controller.upsert(userPromptRow("p", "hi", "t1", true));
    // Assert
    expect(acts).toEqual(["promptSent"]);
  });

  it("paints the new prompt before it parks", () => {
    // Arrange
    const { controller, parkedOver } = scrolled(false);
    controller.applyPage(page([responseRow("a")]), "replace");
    parkedOver.length = 0;
    // Act
    controller.upsert(userPromptRow("p", "hi", "t1", true));
    // Assert
    expect(parkedOver).toEqual([["a", "p"]]);
  });

  it("records the park a new prompt makes", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { controller } = scrolled(false);
    controller.applyPage(page([responseRow("a")]), "replace");
    // Act
    controller.upsert(userPromptRow("p", "hi", "t1", true));
    // Assert
    const record = await forwardedRecord(capture, "feed.sent-prompt-parked");
    expect(record.context).toMatchObject({ feed: "root", row: "p" });
  });

  it("does not re-park on a redraw of a prompt already drawn", () => {
    // Arrange
    const { controller, acts } = scrolled(false);
    controller.applyPage(
      page([userPromptRow("p", "hi", "t1", true)]),
      "replace",
    );
    acts.length = 0;
    // Act
    controller.upsert(userPromptRow("p", "hi, edited", "t1", true));
    // Assert
    expect(acts).not.toContain("promptSent");
  });

  it("does not re-park when a replace redraws a prompt it already drew", async () => {
    // Arrange — the replace's own park stands; the prompt adds none.
    const capture = captureLogRecords("debug");
    const { controller } = scrolled(false);
    controller.upsert(userPromptRow("p", "hi", "t1", true));
    capture.logger.flush();
    await Promise.resolve();
    capture.sent.length = 0;
    // Act
    controller.applyPage(
      page([userPromptRow("p", "hi", "t1", true)]),
      "replace",
    );
    // Assert
    await expect(
      forwardedRecord(capture, "feed.sent-prompt-parked"),
    ).rejects.toThrow();
  });

  it("does not park when a prepend draws a prompt from history", () => {
    // Arrange
    const { controller, acts } = scrolled(false);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    acts.length = 0;
    // Act
    controller.applyPage(
      page([withOrder(userPromptRow("old", "hi", "t0", true), "a")]),
      "prepend",
    );
    // Assert
    expect(acts).not.toContain("promptSent");
  });

  it("does not park on a turn's terminal re-push of a prompt this feed never drew", () => {
    // Arrange — the prompt scrolled out of the newest page; its turn ends.
    const { controller, acts } = scrolled(false);
    controller.applyPage(page([responseRow("a")]), "replace");
    acts.length = 0;
    // Act
    controller.upsert(userPromptRow("p", "hi", "t1", false));
    // Assert
    expect(acts).not.toContain("promptSent");
  });

  it("keeps the reader's content in place when older rows land above it", () => {
    // Arrange — `a` sits 100px down; the older row lands above it.
    const { controller, host, shifts } = scrolled(false);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    layOutByIndex(host, "a");
    // Act
    controller.applyPage(
      page([withOrder(responseRow("older"), "a")]),
      "prepend",
    );
    // Assert — the view moves down by exactly the 400px that grew above.
    expect(shifts).toEqual([400]);
  });

  it("does not shift when a prepend lands on an empty feed", () => {
    // Arrange
    const { controller, shifts } = scrolled(false);
    controller.applyPage(page([], { hasMore: true }), "replace");
    // Act
    controller.applyPage(
      page([withOrder(responseRow("older"), "a")]),
      "prepend",
    );
    // Assert
    expect(shifts).toEqual([]);
  });

  it("records an error, and does not shift, when a prepend detaches the measured row", async () => {
    // Arrange — a listener that tears the first row out mid-page.
    const capture = captureLogRecords();
    const { controller, host, shifts } = scrolled(false);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    const a = layOutByIndex(host, "a");
    controller.onChange(() => a.remove());
    // Act
    controller.applyPage(
      page([withOrder(responseRow("older"), "a")]),
      "prepend",
    );
    // Assert
    const record = await forwardedRecord(
      capture,
      "feed.prepend-anchor-detached",
    );
    expect({
      level: record.level.case,
      context: record.context,
      shifts,
    }).toEqual({
      level: "error",
      context: expect.objectContaining({ feed: "root", row: "a" }) as unknown,
      shifts: [],
    });
  });

  /**
   * A LATE ROW LANDING ABOVE THE READER (owner ruling, 2026-09-27). A row whose
   * key sorts between two held rows is inserted just above the later one; when
   * that row started above the viewport, the view shifts by exactly how far it
   * moved, through `prependCompensation` — the one cause every change above the
   * reader already goes through.
   */
  function lateAbove(opts: { following: boolean; boxTop: number }) {
    const fx = scrolled(opts.following);
    fx.box.getBoundingClientRect = () => ({ top: opts.boxTop }) as DOMRect;
    fx.controller.applyPage(
      page([
        withOrder(responseRow("a"), "k10"),
        withOrder(responseRow("b"), "k30"),
      ]),
      "replace",
    );
    layOutByIndex(fx.host, "b");
    fx.acts.length = 0;
    return fx;
  }

  it("keeps the reader's content in place when a late row lands above the viewport", () => {
    // Arrange — `b` sits at 500px, above a viewport whose top is at 1000px.
    const { controller, shifts } = lateAbove({
      following: false,
      boxTop: 1000,
    });
    // Act — a late row whose key sorts between `a` and `b`.
    controller.upsert(withOrder(responseRow("late"), "k20"));
    // Assert — `b` moved down one 400px row, and the view follows it.
    expect(shifts).toEqual([400]);
  });

  it("moves nothing when a late row lands inside the viewport", () => {
    // Arrange — `b` sits at 500px, below a viewport top at 0.
    const { controller, shifts } = lateAbove({ following: false, boxTop: 0 });
    // Act
    controller.upsert(withOrder(responseRow("late"), "k20"));
    // Assert
    expect(shifts).toEqual([]);
  });

  it("keeps a following reader at the tail when a late row lands above", () => {
    // Arrange
    const { controller, acts } = lateAbove({ following: true, boxTop: 1000 });
    // Act
    controller.upsert(withOrder(responseRow("late"), "k20"));
    // Assert — the follow keeps the tail after the row is drawn in its place.
    expect(acts).toEqual(["follow"]);
  });

  it("records an error, and does not shift, when an insert detaches the measured row", async () => {
    // Arrange — a listener that tears the measured row out mid-insert.
    const capture = captureLogRecords();
    const { controller, host, shifts } = lateAbove({
      following: false,
      boxTop: 1000,
    });
    const b = host.querySelector('[data-feed-row="b"]');
    controller.onChange(() => b?.remove());
    // Act
    controller.upsert(withOrder(responseRow("late"), "k20"));
    // Assert
    const record = await forwardedRecord(
      capture,
      "feed.insert-anchor-detached",
    );
    expect({
      level: record.level.case,
      context: record.context,
      shifts,
    }).toEqual({
      level: "error",
      context: expect.objectContaining({ feed: "root", row: "b" }) as unknown,
      shifts: [],
    });
  });
});

/**
 * A THINKING BUBBLE ABOVE THE READER COLLAPSES WITHOUT MOVING THEM (owner rule,
 * 2026-09-23). The daemon re-pushes a thinking row settled once its own final
 * text has arrived; its redraw drops it to one line. No later row is involved. jsdom lays nothing out, so
 * the row's bottom edge is stubbed from the cap its body was drawn at, and the
 * tail owner is the REAL `TailFollow` over a fake box.
 */
describe("createFeedController: a landed thinking row collapsing", () => {
  /** A thinking response row, landed (settled) or still arriving. */
  function thinkingRow(
    id: string,
    landed: boolean,
    markdown = "weighing",
  ): FeedRow {
    return create(FeedRowSchema, {
      id: feedId(id),
      order: orderFor(id),
      row: {
        case: "activity",
        value: {
          unit: {
            case: "response",
            value: {
              thinking: true,
              result: {
                case: landed ? "success" : "update",
                value: { prose: { markdown } },
              },
            },
          },
        },
      },
    });
  }

  /** A response renderer stating the cap the real card would draw, and nothing else. */
  function cappedResponse(u: FeedResponse): HTMLElement {
    const el = document.createElement("div");
    el.setAttribute("data-cap-lines", String(responseCapLines(u)));
    el.textContent =
      u.result.case === undefined ? "" : (u.result.value.prose?.markdown ?? "");
    return el;
  }

  /**
   * A feed over a 300px viewport whose top edge sits at 100, scrolled to 500
   * by the READER (so no follow stands) unless FOLLOWING, holding thinking row
   * t1 whose bottom edge is BEFORE while at the response cap and AFTER once
   * collapsed to one line.
   */
  function collapsing(opts: {
    following: boolean;
    before: number;
    after: number;
  }) {
    const h = harness();
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const box = {
      scrollTop: 0,
      scrollHeight: 2000,
      clientHeight: 300,
      getBoundingClientRect: () => ({ top: 100 }) as DOMRect,
    };
    const tail = new TailFollow(box);
    const controller = createFeedController({
      ctx: h.ctx,
      host,
      feed: "root",
      renderers: stubRenderers({ response: cappedResponse }),
      body: defaultBubbleBody,
      revealRow: async () => false,
      bubble: (row) => stubBubble(row),
      bodyContext: {
        ctx: h.ctx,
        feed: "root",
        row: create(FeedRowSchema, {}),
        revealRow: async () => false,
      },
      scroll: { box, tail },
    });
    controller.applyPage(page([thinkingRow("t1", false)]), "replace");
    const row = host.querySelector<HTMLElement>('[data-feed-row="t1"]');
    if (row === null) throw new Error("t1 is not drawn");
    row.getBoundingClientRect = () => {
      const cap = row
        .querySelector("[data-cap-lines]")
        ?.getAttribute("data-cap-lines");
      return { bottom: cap === "1" ? opts.after : opts.before } as DOMRect;
    };
    if (!opts.following) {
      tail.onInput();
      box.scrollTop = 500;
      tail.onScroll();
    }
    return { controller, box, tail, host };
  }

  it.each([
    {
      name: "a collapse wholly above the viewport shifts the view by the height it lost",
      following: false,
      before: 90,
      after: 40,
      want: 450,
    },
    {
      name: "a collapse the reader can see moves nothing",
      following: false,
      before: 250,
      after: 200,
      want: 500,
    },
    {
      name: "a landed row whose height did not change (the reader expanded it) moves nothing",
      following: false,
      before: 90,
      after: 90,
      want: 500,
    },
    {
      name: "a following feed stays at its tail",
      following: true,
      before: 90,
      after: 40,
      want: 2000,
    },
  ])("$name", ({ following, before, after, want }) => {
    // Arrange
    const { controller, box } = collapsing({ following, before, after });
    // Act — the daemon re-pushes t1 settled; no later row is drawn.
    controller.upsert(thinkingRow("t1", true));
    // Assert
    expect(box.scrollTop).toBe(want);
  });

  it("keeps a following feed following", () => {
    // Arrange
    const { controller, tail } = collapsing({
      following: true,
      before: 90,
      after: 40,
    });
    // Act
    controller.upsert(thinkingRow("t1", true));
    // Assert
    expect(tail.isFollowing()).toBe(true);
  });

  it("moves nothing for a re-push above the viewport that is not the landing edge", () => {
    // Arrange — a re-push that changes the text, not the flag.
    const { controller, box, host } = collapsing({
      following: false,
      before: 90,
      after: 40,
    });
    const row = host.querySelector<HTMLElement>(
      '[data-feed-row="t1"]',
    ) as HTMLElement;
    row.getBoundingClientRect = () =>
      ({ bottom: row.textContent?.includes("longer") ? 140 : 90 }) as DOMRect;
    // Act
    controller.upsert(thinkingRow("t1", false, "weighing, longer"));
    // Assert
    expect(box.scrollTop).toBe(500);
  });

  it("measures no collapse when an already landed row is re-pushed", async () => {
    // Arrange — t1 lands once; only the records after that are read.
    const capture = captureLogRecords("debug");
    const { controller } = collapsing({
      following: false,
      before: 90,
      after: 40,
    });
    controller.upsert(thinkingRow("t1", true));
    capture.logger.flush();
    await Promise.resolve();
    capture.sent.length = 0;
    // Act — the daemon re-pushes the settled row again.
    controller.upsert(thinkingRow("t1", true, "weighing, restated"));
    // Assert
    await expect(
      forwardedRecord(capture, "feed.collapse-kept-place"),
    ).rejects.toThrow();
  });

  it("collapses on its own landing without waiting for a later response", () => {
    // Arrange
    const { controller, host } = collapsing({
      following: false,
      before: 90,
      after: 40,
    });
    // Act
    controller.upsert(thinkingRow("t1", true));
    // Assert — t1 is the only row, and it wears the thinking cap.
    expect([
      host.querySelectorAll("[data-feed-row]").length,
      host
        .querySelector('[data-feed-row="t1"] [data-cap-lines]')
        ?.getAttribute("data-cap-lines"),
    ]).toEqual([1, "1"]);
  });

  it("records the collapse it measured at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { controller } = collapsing({
      following: false,
      before: 90,
      after: 40,
    });
    // Act
    controller.upsert(thinkingRow("t1", true));
    // Assert
    const record = await forwardedRecord(capture, "feed.collapse-kept-place");
    expect(record.context).toMatchObject({
      feed: "root",
      row: "t1",
      box_top: 100,
      before: 90,
      after: 40,
    });
  });

  it("records a collapsing row its redraw detached as an ERROR and moves nothing", async () => {
    // Arrange — a listener that detaches the row during the redraw.
    const capture = captureLogRecords("debug");
    const { controller, box, host } = collapsing({
      following: false,
      before: 90,
      after: 40,
    });
    controller.onChange(() =>
      host.querySelector('[data-feed-row="t1"]')?.remove(),
    );
    // Act
    controller.upsert(thinkingRow("t1", true));
    // Assert
    const record = await forwardedRecord(
      capture,
      "feed.collapse-anchor-detached",
    );
    expect([record.level.case, box.scrollTop]).toEqual(["error", 500]);
  });
});

describe("createFeedController: the feed selection", () => {
  /** A response renderer that draws the real `.bubble.assistant` chrome. */
  function assistantResponse(): HTMLElement {
    const el = document.createElement("div");
    el.className = "bubble assistant final-response";
    return el;
  }

  /** A scroll box and tail owner recording the named causes asked for. */
  function selectionScroll(geometry: {
    scrollTop: number;
    scrollHeight: number;
    clientHeight: number;
  }) {
    const acts: string[] = [];
    const shifts: number[] = [];
    const box = {
      ...geometry,
      querySelector: () => null,
      getBoundingClientRect: boxRect,
    };
    const tail = {
      isFollowing: () => false,
      follow: () => undefined,
      selectionCleared: () => acts.push("selectionCleared"),
      selectionEnded: () => acts.push("selectionEnded"),
      selectionMoved: (g: CenterGeometry | null) => {
        acts.push("selectionMoved");
        if (g !== null) shifts.push(centerDelta(g));
      },
    };
    return { box, tail, acts, shifts };
  }

  /** A controller drawing assistant bubbles into a recording scroll box. */
  function selecting(
    geometry = { scrollTop: 0, scrollHeight: 1000, clientHeight: 300 },
    withScroll = true,
  ) {
    const watched: Array<{ element: HTMLElement; id: string } | null> = [];
    const selectionVisibility: SelectionVisibility = {
      watch: (row) => {
        watched.push(
          row === null ? null : { element: row.element, id: row.id.value },
        );
      },
      dispose: () => undefined,
    };
    const h = harness();
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const scroll = selectionScroll(geometry);
    const controller = createFeedController({
      ctx: h.ctx,
      host,
      feed: "root",
      renderers: stubRenderers({ response: assistantResponse }),
      body: defaultBubbleBody,
      revealRow: async () => false,
      bubble: (row) => stubBubble(row),
      bodyContext: {
        ctx: h.ctx,
        feed: "root",
        row: create(FeedRowSchema, {}),
        revealRow: async () => false,
      },
      scroll: withScroll
        ? { box: scroll.box, tail: scroll.tail as never }
        : undefined,
      selectionVisibility,
    });
    return {
      controller,
      host,
      acts: scroll.acts,
      shifts: scroll.shifts,
      watched,
    };
  }

  /** Script a row element's box, which jsdom lays out not at all. */
  function withRowGeometry(
    el: HTMLElement,
    offsetTop: number,
    offsetHeight: number,
  ): void {
    Object.defineProperties(el, {
      offsetTop: { get: () => offsetTop },
      offsetHeight: { get: () => offsetHeight },
    });
  }

  it("recolors the selected final-response bubble blue", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    const bubble = host.querySelector('[data-feed-row="r1"] .bubble.assistant');
    expect(bubble?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(true);
  });

  it("puts the selection mark on the card, never on the row's full-width wrapper", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    const row = host.querySelector('[data-feed-row="r1"]');
    expect(row?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("carries the selection mark onto a card a push replaced", () => {
    // Arrange — a tool card's push draws a new card element in the old one's place.
    const { controller, host } = selecting();
    controller.upsert(toolCallRow("t1", "running"));
    controller.applySelection(selectionOf({ response: "t1" }));
    const before = host.querySelector(
      '[data-feed-row="t1"]',
    )?.firstElementChild;
    // Act
    controller.upsert(toolCallRow("t1", "returned"));
    // Assert — a different element, still marked.
    const after = host.querySelector('[data-feed-row="t1"]')?.firstElementChild;
    expect([
      after === before,
      after?.classList.contains(SELECTED_ENTRY_CLASS),
    ]).toEqual([false, true]);
  });

  it("marks the selected row's chrome so the feed's own record names it", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    const row = host.querySelector('[data-feed-row="r1"]');
    expect(row?.getAttribute(SELECTED_ROW_ATTRIBUTE)).toBe("response");
  });

  it("clears the blue from the previously selected bubble when the selection moves", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    controller.upsert(responseRow("r2"));
    controller.applySelection(selectionOf({ response: "r1" }));
    // Act — C-n moves the selection to the next response.
    controller.applySelection(selectionOf({ response: "r2" }));
    // Assert
    const first = host.querySelector('[data-feed-row="r1"] .bubble.assistant');
    expect(first?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("clears the blue from every bubble when the selection is cleared", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    // Act — double-escape clears the selection.
    controller.applySelection(selectionOf({ none: "returnToTail" }));
    // Assert
    const bubble = host.querySelector('[data-feed-row="r1"] .bubble.assistant');
    expect(bubble?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("hands a selected row to the tail owner as selectionMoved", () => {
    // Arrange
    const { controller, acts } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    expect(acts).toEqual(["selectionMoved"]);
  });

  it("returns to the bottom when the selection is dismissed (return_to_tail)", () => {
    // Arrange
    const { controller, acts } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    acts.length = 0;
    // Act
    controller.applySelection(selectionOf({ none: "returnToTail" }));
    // Assert
    expect(acts).toEqual(["selectionCleared"]);
  });

  it("does not move the feed when an empty selection is restated", () => {
    // Arrange — REMOVED TRIGGER: every inactive push used to park the feed,
    // including the one a re-opened stream restates with nothing selected.
    const { controller, acts } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ none: "returnToTail" }));
    // Assert
    expect(acts).toEqual([]);
  });

  it("does not re-center when the same row is restated", () => {
    // Arrange
    const { controller, acts } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    acts.length = 0;
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    expect(acts).toEqual([]);
  });

  it("records a restated selection at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { controller } = selecting();
    // Act
    controller.applySelection(selectionOf({ none: "returnToTail" }));
    // Assert
    const record = await forwardedRecord(capture, "feed.selection-unchanged");
    expect(record.context).toMatchObject({ feed: "root", selected: "none" });
  });

  it("center-scrolls the selected row to the middle of the viewport", () => {
    // Arrange — a 100px row at offset 500 in a 300px viewport over a 1000px
    // feed: centering asks for scrollTop 500 - (300 - 100)/2 = 400.
    const { controller, host, shifts } = selecting();
    controller.upsert(responseRow("r1"));
    const row = host.querySelector<HTMLElement>('[data-feed-row="r1"]');
    withRowGeometry(row as HTMLElement, 500, 100);
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    expect(shifts).toEqual([400]);
  });

  it("does not center a row this feed has not drawn", () => {
    // Arrange — no rows drawn.
    const { controller, shifts } = selecting();
    // Act
    controller.applySelection(selectionOf({ response: "gone" }));
    // Assert — nothing is scrolled, and nothing throws.
    expect(shifts).toEqual([]);
  });

  it("records a center this feed has not drawn at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { controller } = selecting();
    // Act
    controller.applySelection(selectionOf({ response: "gone" }));
    // Assert
    const record = await forwardedRecord(
      capture,
      "feed.selection-center-absent",
    );
    expect(record.context).toMatchObject({ feed: "root", row: "gone" });
  });

  it("reports no selection before the daemon pushes one", () => {
    // Arrange
    const { controller } = selecting();
    // Act
    const active = controller.selectionActive();
    // Assert
    expect(active).toBe(false);
  });

  it("reports the selection active once a selected row is pushed", () => {
    // Arrange
    const { controller } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    expect(controller.selectionActive()).toBe(true);
  });

  it("reports the selection inactive once the daemon pushes none", () => {
    // Arrange
    const { controller } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    // Act
    controller.applySelection(selectionOf({ none: "returnToTail" }));
    // Assert
    expect(controller.selectionActive()).toBe(false);
  });

  it("applies the border even with no scroll box", () => {
    // Arrange — a sub-feed fixture has no scroll box; the border must still land.
    const { controller, host } = selecting(undefined, false);
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    const bubble = host.querySelector('[data-feed-row="r1"] .bubble.assistant');
    expect(bubble?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(true);
  });

  it("marks a selected prompt's card with the selection mark", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(userPromptRow("p1", "hello"));
    // Act
    controller.applySelection(selectionOf({ prompt: "p1" }));
    // Assert
    const card = host.querySelector('[data-feed-row="p1"]')?.firstElementChild;
    expect([
      card?.getAttribute("data-role"),
      card?.classList.contains(SELECTED_ENTRY_CLASS),
    ]).toEqual(["prompt", true]);
  });

  it("names a selected prompt's kind on its row", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(userPromptRow("p1", "hello"));
    // Act
    controller.applySelection(selectionOf({ prompt: "p1" }));
    // Assert
    const row = host.querySelector('[data-feed-row="p1"]');
    expect(row?.getAttribute(SELECTED_ROW_ATTRIBUTE)).toBe("prompt");
  });

  it("moves the mark off a response when the selection moves to a prompt", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    controller.upsert(userPromptRow("p1", "hello"));
    controller.applySelection(selectionOf({ response: "r1" }));
    // Act — C-S-p replaces a selected response with the newest prompt.
    controller.applySelection(selectionOf({ prompt: "p1" }));
    // Assert
    const bubble = host.querySelector('[data-feed-row="r1"] .bubble.assistant');
    expect(bubble?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("centers again each time the selection moves to a new row", () => {
    // Arrange
    const { controller, acts } = selecting();
    controller.upsert(responseRow("r1"));
    controller.upsert(userPromptRow("p1", "hello"));
    controller.applySelection(selectionOf({ response: "r1" }));
    acts.length = 0;
    // Act
    controller.applySelection(selectionOf({ prompt: "p1" }));
    // Assert
    expect(acts).toEqual(["selectionMoved"]);
  });

  it("stays where the reader is when the selection ends with stay", () => {
    // Arrange
    const { controller, acts } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    acts.length = 0;
    // Act — the selected row left the viewport.
    controller.applySelection(selectionOf({ none: "stay" }));
    // Assert — no park, no centering: only the hold-off ends.
    expect(acts).toEqual(["selectionEnded"]);
  });

  it("drops the mark when the selection ends with stay", () => {
    // Arrange
    const { controller, host } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    // Act
    controller.applySelection(selectionOf({ none: "stay" }));
    // Assert
    const bubble = host.querySelector('[data-feed-row="r1"] .bubble.assistant');
    expect(bubble?.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("does not move the feed when stay is restated with nothing selected", () => {
    // Arrange
    const { controller, acts } = selecting();
    // Act
    controller.applySelection(selectionOf({ none: "stay" }));
    // Assert
    expect(acts).toEqual([]);
  });

  it("points the left-view watch at the selected row", () => {
    // Arrange
    const { controller, host, watched } = selecting();
    controller.upsert(responseRow("r1"));
    // Act
    controller.applySelection(selectionOf({ response: "r1" }));
    // Assert
    const row = host.querySelector('[data-feed-row="r1"]');
    expect(watched.at(-1)).toEqual({ element: row, id: "r1" });
  });

  it("stops the left-view watch when nothing is selected", () => {
    // Arrange
    const { controller, watched } = selecting();
    controller.upsert(responseRow("r1"));
    controller.applySelection(selectionOf({ response: "r1" }));
    // Act
    controller.applySelection(selectionOf({ none: "returnToTail" }));
    // Assert
    expect(watched.at(-1)).toBeNull();
  });

  it("watches nothing for a selected row this feed has not drawn", () => {
    // Arrange
    const { controller, watched } = selecting();
    // Act
    controller.applySelection(selectionOf({ prompt: "gone" }));
    // Assert
    expect(watched).toEqual([null]);
  });

  it("refuses a selection with no arm set as a malformed view", () => {
    // Arrange
    const { controller } = selecting();
    // Act / Assert
    expect(() =>
      controller.applySelection(create(FeedSelectionSchema, {})),
    ).toThrow(MalformedView);
  });

  it("refuses an empty selection with no viewport set as a malformed view", () => {
    // Arrange
    const { controller } = selecting();
    const bare = create(FeedSelectionSchema, {
      selection: { case: "none", value: {} },
    });
    // Act / Assert
    expect(() => controller.applySelection(bare)).toThrow(MalformedView);
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

/** A row carrying ARM, built straight onto the generated schema. */
function rowWith(
  id: string,
  arm: MessageInitShape<typeof FeedRowSchema>["row"],
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: arm,
  });
}

function unknownArmRow(id: string): FeedRow {
  const row = create(FeedRowSchema, { id: feedId(id), order: orderFor(id) });
  // A wire arm no build knows, past the generated union.
  (row as { row: unknown }).row = { case: "surprise", value: {} };
  return row;
}

/** A row carrying one activity UNIT. */
function unitRow(id: string, unit: unknown): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: { case: "activity", value: { unit: unit as never } },
  });
}

describe("createFeedController: every row arm reaches its own drawing", () => {
  /** Each arm of `FeedRow.row` the ordinary row path draws, and its mark. */
  const ARMS: ReadonlyArray<
    [string, MessageInitShape<typeof FeedRowSchema>["row"], string]
  > = [
    [
      "agentPrompt",
      {
        case: "agentPrompt",
        value: {
          address: { text: "→ Explore" },
          body: {
            blocks: [{ block: { case: "text", value: { text: "go" } } }],
          },
        },
      },
      ".prompt-agent",
    ],
    [
      "turnEnded",
      {
        case: "turnEnded",
        value: { endedAtMs: 1n, outcome: { case: "concluded", value: {} } },
      },
      "[data-arm='concluded']",
    ],
    [
      "separation",
      {
        case: "separation",
        value: {
          label: { text: "context cleared" },
          kind: { case: "cleared", value: {} },
        },
      },
      ".sep-label",
    ],
    [
      "detachedShell",
      { case: "detachedShell", value: { shell: {} } },
      ".stub-shell",
    ],
    ["permission", { case: "permission", value: {} }, ".stub-permission"],
    ["question", { case: "question", value: {} }, ".stub-question"],
    ["coldGate", { case: "coldGate", value: {} }, ".stub-coldGate"],
    ["commandPanel", { case: "commandPanel", value: {} }, ".stub-commandPanel"],
    [
      "commandRefused",
      { case: "commandRefused", value: {} },
      ".stub-commandRefused",
    ],
  ];

  it.each(ARMS)("draws the %s arm", (name, arm, mark) => {
    // Arrange / Act
    const { controller, host } = fixture();
    controller.applyPage(page([rowWith(name, arm)]), "replace");
    // Assert
    expect(
      host.querySelector(`[data-feed-row="${name}"] ${mark}`),
    ).not.toBeNull();
  });

  it("refuses a row arm this build does not know", () => {
    const { controller, h } = fixture();
    controller.applyPage(page([unknownArmRow("x")]), "replace");
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });

  it("names FeedRow.row as the path of an unknown arm's refusal", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([unknownArmRow("x")]), "replace");
    expect(host.querySelector(".row-malformed")?.textContent).toContain(
      "FeedRow.row",
    );
  });

  it("refuses a detached subagent that reached the ordinary row path", () => {
    // Arrange: a plain row takes the id first, so no bubble is built for it.
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("b1")]), "replace");
    // Act: the same id is re-pushed as a detached bubble row.
    controller.upsert(subagentRow("b1", { detached: true }));
    // Assert
    expect(host.querySelector(".row-malformed")?.textContent).toContain(
      "FeedRow.row.detached_subagent",
    );
  });
});

describe("createFeedController: every activity unit reaches its own renderer", () => {
  const UNITS: ReadonlyArray<[string, unknown, string]> = [
    [
      "simpleToolCall",
      { case: "simpleToolCall", value: {} },
      ".stub-simpleToolCall",
    ],
    ["skill", { case: "skill", value: {} }, ".stub-skill"],
    ["hook", { case: "hook", value: {} }, ".stub-hook"],
    ["artifact", { case: "artifact", value: {} }, ".stub-artifact"],
    ["plan", { case: "plan", value: {} }, ".stub-plan"],
    ["findings", { case: "findings", value: {} }, ".stub-findings"],
    [
      "subagentResult",
      { case: "subagentResult", value: {} },
      ".stub-subagentResult",
    ],
  ];

  it.each(UNITS)("draws the %s unit", (name, unit, mark) => {
    const { controller, host } = fixture();
    controller.applyPage(page([unitRow(name, unit)]), "replace");
    expect(
      host.querySelector(`[data-feed-row="${name}"] ${mark}`),
    ).not.toBeNull();
  });

  it("refuses an activity unit this build does not know", () => {
    const { controller, host } = fixture();
    const row = unitRow("x", { case: "response", value: {} });
    // A wire arm no build knows: set past the generated union, which is exactly
    // what a newer daemon's field number looks like once it is decoded.
    (row.row.value as { unit: unknown }).unit = { case: "surprise", value: {} };
    controller.applyPage(page([row]), "replace");
    expect(host.querySelector(".row-malformed")?.textContent).toContain(
      "FeedTurnActivity.unit",
    );
  });

  it("refuses a subagent unit that reached the ordinary row path", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("b1")]), "replace");
    controller.upsert(subagentRow("b1"));
    expect(host.querySelector(".row-malformed")?.textContent).toContain(
      "FeedTurnActivity.unit.subagent",
    );
  });

  it("refuses a merge unit that reached the ordinary row path", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([responseRow("m1")]), "replace");
    controller.upsert(mergeRow("m1"));
    expect(host.querySelector(".row-malformed")?.textContent).toContain(
      "FeedTurnActivity.unit.merge",
    );
  });

  it("stamps no unit on an activity row whose unit is unset", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([
        create(FeedRowSchema, {
          id: feedId("x"),
          order: orderFor("x"),
          row: { case: "activity", value: {} },
        }),
      ]),
      "replace",
    );
    expect(
      host.querySelector('[data-feed-row="x"]')?.hasAttribute("data-unit"),
    ).toBe(false);
  });

  it("stamps a row with no arm at all as malformed rather than dropping it", () => {
    const { controller, host } = fixture();
    controller.upsert(
      create(FeedRowSchema, { id: feedId("x"), order: orderFor("x") }),
    );
    expect(
      host.querySelector('[data-feed-row="x"]')?.getAttribute("data-row-kind"),
    ).toBe("malformed");
  });
});

describe("createFeedController: the page's own arms", () => {
  it("refuses a page result arm this build does not know", () => {
    const { controller } = fixture();
    const bad = page([]);
    (bad as { result: unknown }).result = { case: "surprise", value: {} };
    expect(() => controller.applyPage(bad, "replace")).toThrow(MalformedView);
  });

  it("refuses a page error whose evidence arm this build does not know", () => {
    const { controller } = fixture();
    const bad = create(FeedPageSchema, {
      result: {
        case: "error",
        value: {
          headline: { text: "x", tone: "red" },
          kind: { case: "historyReplayTruncated", value: { reason: "r" } },
        },
      },
    });
    (bad.result.value as { kind: unknown }).kind = {
      case: "surprise",
      value: {},
    };
    expect(() => controller.applyPage(bad, "replace")).toThrow(MalformedView);
  });
});

describe("createFeedController: mirroring the card's state onto the chrome", () => {
  /** A response renderer whose card states STATE, or states nothing. */
  function stating(state: string | null) {
    return {
      response: (): HTMLElement => {
        const el = document.createElement("div");
        el.className = "stub-response";
        if (state !== null) el.setAttribute("data-state", state);
        return el;
      },
    };
  }

  it("copies the card's state up onto the row chrome", () => {
    const { controller, host } = fixture(
      harness(),
      {},
      { renderers: stating("running") },
    );
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(
      host.querySelector('[data-feed-row="a"]')?.getAttribute("data-state"),
    ).toBe("running");
  });

  it("states nothing on the chrome when the card states nothing", () => {
    const { controller, host } = fixture(
      harness(),
      {},
      { renderers: stating(null) },
    );
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(
      host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-state"),
    ).toBe(false);
  });
});

describe("createFeedController: the reader's expansions survive a redraw", () => {
  /** A response renderer whose whole card is a capped, card-level fold. */
  function foldingCard(): Partial<Parameters<typeof stubRenderers>[0]> {
    return {
      response: () => {
        const el = document.createElement("div");
        el.className = "stub-response tool-fold";
        return el;
      },
    };
  }

  it("keeps a card-level fold the reader opened across a re-push of the row", () => {
    // Arrange: the row is drawn, and the reader opens its fold.
    const { controller, host } = fixture(
      harness(),
      {},
      { renderers: foldingCard() },
    );
    controller.applyPage(page([responseRow("a")]), "replace");
    host.querySelector<HTMLElement>(".tool-fold")?.classList.add("expanded");
    // Act: the daemon re-pushes the SAME row — the upsert path.
    controller.upsert(responseRow("a"));
    // Assert: R2 — a push states the INITIAL fold and never un-toggles.
    expect(
      host.querySelector(".tool-fold")?.classList.contains("expanded"),
    ).toBe(true);
  });

  it("leaves a fold the reader never opened collapsed across a re-push", () => {
    // Arrange
    const { controller, host } = fixture(
      harness(),
      {},
      { renderers: foldingCard() },
    );
    controller.applyPage(page([responseRow("a")]), "replace");
    // Act
    controller.upsert(responseRow("a"));
    // Assert
    expect(
      host.querySelector(".tool-fold")?.classList.contains("expanded"),
    ).toBe(false);
  });
});

describe("createFeedController: a feed that is not the root", () => {
  it("names the feed it draws by its own id", () => {
    const { host } = fixture(
      harness(),
      {},
      { feed: create(FeedIdSchema, { value: "b1" }) },
    );
    expect(host.getAttribute("data-feed")).toBe("b1");
  });

  it("walks THAT feed, addressing the page request to its id", async () => {
    // Arrange
    const h = harness();
    const { controller, host } = fixture(
      h,
      {},
      { feed: create(FeedIdSchema, { value: "b1" }) },
    );
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    // Act
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    // Assert
    expect(h.calls.getFeedPage[0]?.feed?.value).toBe("b1");
  });
});

describe("createFeedController: the bubbles it holds", () => {
  it("reports the bubbles on this feed, for the reveal walk", () => {
    const { controller, bubbles } = fixture();
    controller.applyPage(
      page([subagentRow("b1"), responseRow("r1")]),
      "replace",
    );
    expect(controller.bubbles()).toEqual([bubbles.get("b1")]);
  });

  it("reports none on a feed with no bubble row", () => {
    const { controller } = fixture();
    controller.applyPage(page([responseRow("r1")]), "replace");
    expect(controller.bubbles()).toEqual([]);
  });
});

describe("createFeedController: what a body renderer reads", () => {
  it("hands the body renderer the trail the page came with", () => {
    // Arrange / Act
    const { controller } = fixture();
    controller.applyPage(
      page([responseRow("a")], {
        crumbs: [
          create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" }),
        ],
      }),
      "replace",
    );
    // Assert
    expect(controller.breadcrumbs().map((crumb) => crumb.label)).toEqual([
      "outer",
    ]);
  });

  it("exposes the view a body renderer draws from", () => {
    const { controller } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(
      controller
        .view()
        .rows()
        .map((row) => row.id?.value),
    ).toEqual(["a"]);
  });
});

describe("createFeedController: drawing and disposal edges", () => {
  it("refuses to draw a row it does not hold", () => {
    const { controller } = fixture();
    expect(() => controller.drawRow(responseRow("nope"))).toThrow(
      /unheld row nope/,
    );
  });

  it("disposes each bubble exactly once, however often dispose is called", () => {
    // Arrange
    const { controller, bubbles } = fixture();
    controller.applyPage(page([subagentRow("b1")]), "replace");
    let disposals = 0;
    const bubble = bubbles.get("b1")!;
    const inner = bubble.dispose.bind(bubble);
    bubble.dispose = () => {
      disposals += 1;
      inner();
    };
    // Act
    controller.dispose();
    controller.dispose();
    // Assert
    expect(disposals).toBe(1);
  });
});

describe("createFeedController: a failure that is not a malformed view", () => {
  it("lets a renderer's own crash out rather than drawing it as an unreadable row", () => {
    // Arrange: a renderer that fails for a reason the schema has nothing to do
    // with. Only a MalformedView costs one row; anything else is this build's
    // bug and must not be disguised as bad data.
    const { controller } = fixture(
      harness(),
      {},
      {
        renderers: {
          response: () => {
            throw new TypeError("the renderer is broken");
          },
        },
      },
    );
    // Act / Assert
    expect(() =>
      controller.applyPage(page([responseRow("a")]), "replace"),
    ).toThrow("the renderer is broken");
  });
});

describe("createFeedController: the walk's refusal does not accumulate", () => {
  it("drops the previous walk's refusal when the control is used again", async () => {
    // Arrange: a daemon that refuses every walk.
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: { case: "error", value: {} },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(
      page([responseRow("a")], { hasMore: true }),
      "replace",
    );
    // Act: two refused walks in a row.
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    // Assert: one refusal stands, not two.
    expect(host.querySelectorAll(".refusal")).toHaveLength(1);
  });
});

describe("createFeedController: a card's clocks stop when its unit settles", () => {
  /** A fixture drawing REAL tool-call cards on a ticker the test can count. */
  function ticking(): Fixture & { ticker: CountingTicker } {
    const ticker = countingTicker();
    const f = fixture(
      harness({ ticker }),
      {},
      { renderers: { simpleToolCall: drawFeedSimpleToolCall } },
    );
    return { ...f, ticker };
  }

  it("leaves nothing subscribed once the call has returned", () => {
    // Arrange: a running call, holding its quiet-for clock.
    const { controller, ticker } = ticking();
    controller.applyPage(page([toolCallRow("t", "running")]), "replace");
    expect(ticker.live()).toBe(1);
    // Act: the terminal frame for the same row.
    controller.upsert(toolCallRow("t", "returned"));
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("does not leak the previous subscription when a running row is re-pushed", () => {
    // Arrange.
    const { controller, ticker } = ticking();
    controller.applyPage(page([toolCallRow("t", "running")]), "replace");
    // Act: three more live frames of the same row.
    for (let i = 0; i < 3; i += 1)
      controller.upsert(toolCallRow("t", "running"));
    // Assert: one card, one clock — not one per push.
    expect(ticker.live()).toBe(1);
  });

  it("stops every remaining clock in a turn when that turn's end lands", () => {
    // Arrange: a call still drawn as running when its turn finishes.
    const { controller, ticker } = ticking();
    controller.applyPage(
      page([toolCallRow("t", "running", { turn: "turn-1" })]),
      "replace",
    );
    expect(ticker.live()).toBe(1);
    // Act.
    controller.upsert(turnEndedRow("e", "turn-1"));
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("stops the turn's clocks when an interjection's end lands, though that end draws nothing", () => {
    // Arrange: a running call; the controller draws the terminal row itself,
    // and an interjection's draws nothing but is still the turn's ending row.
    const ticker = countingTicker();
    const { controller } = fixture(
      harness({ ticker }),
      {},
      {
        renderers: { simpleToolCall: drawFeedSimpleToolCall },
      },
    );
    controller.applyPage(
      page([toolCallRow("t", "running", { turn: "turn-1" })]),
      "replace",
    );
    const interjected = turnEndedRow("e", "turn-1", "interrupted");
    if (
      interjected.row.case !== "turnEnded" ||
      interjected.row.value.outcome.case !== "interrupted"
    ) {
      throw new Error("the fixture is not an interrupted ending");
    }
    interjected.row.value.outcome.value.command = {
      case: "interjection",
      value: create(FeedTurnEndedInterruptedInterjectionSchema, {}),
    };
    // Act.
    controller.upsert(interjected);
    // Assert.
    expect(ticker.live()).toBe(0);
  });

  it("freezes the reading the backstop stopped on, rather than blanking it", () => {
    // Arrange.
    vi.setSystemTime(10_000);
    const { controller, host, ticker } = ticking();
    controller.applyPage(
      page([toolCallRow("t", "running", { turn: "turn-1", beatAtMs: 7000n })]),
      "replace",
    );
    // Act.
    controller.upsert(turnEndedRow("e", "turn-1"));
    vi.advanceTimersByTime(60_000);
    // Assert: the last reading stands, and no clock is running behind it.
    expect(host.querySelector(".tool-quiet")?.textContent).toBe("quiet for 3s");
    expect(ticker.live()).toBe(0);
  });

  it("leaves another turn's clocks running, because only the ended turn ended", () => {
    // Arrange: two running calls, in two different turns.
    const { controller, ticker } = ticking();
    controller.applyPage(
      page([
        toolCallRow("t1", "running", { turn: "turn-1" }),
        toolCallRow("t2", "running", { turn: "turn-2" }),
      ]),
      "replace",
    );
    expect(ticker.live()).toBe(2);
    // Act: only turn-1 ends.
    controller.upsert(turnEndedRow("e", "turn-1"));
    // Assert: turn-2's call is still counting.
    expect(ticker.live()).toBe(1);
  });

  it("leaves a row's discard hooks in place when its turn ends, since the row stays on screen", () => {
    // Arrange: a running call whose card also holds a non-clock teardown (a
    // measurer's observer, say), in a turn that is about to end.
    let disposed = 0;
    const ticker = countingTicker();
    const { controller } = fixture(
      harness({ ticker }),
      {},
      {
        renderers: {
          simpleToolCall: (u, rc) => {
            const el = drawFeedSimpleToolCall(u, rc);
            onDiscard(el, () => (disposed += 1));
            return el;
          },
        },
      },
    );
    controller.applyPage(
      page([toolCallRow("t", "running", { turn: "turn-1" })]),
      "replace",
    );

    // Act
    controller.upsert(turnEndedRow("e", "turn-1"));

    // Assert: the clock stopped, and the hook did not run.
    expect(ticker.live()).toBe(0);
    expect(disposed).toBe(0);
  });

  it("leaves a present clock counting when its turn ends, since an age stays true", () => {
    // Arrange: a settled card whose only clock is an "ago" reading.
    const ticker = countingTicker();
    const { controller } = fixture(
      harness({ ticker }),
      {},
      {
        renderers: {
          simpleToolCall: (_u, rc) => {
            const el = document.createElement("span");
            tickWhileShown(el, rc.ctx.ticker, () => {});
            return el;
          },
        },
      },
    );
    controller.applyPage(
      page([toolCallRow("t", "returned", { turn: "turn-1" })]),
      "replace",
    );

    // Act
    controller.upsert(turnEndedRow("e", "turn-1"));

    // Assert: the age is still counting.
    expect(ticker.live()).toBe(1);
  });

  it("leaves a turnless row's clocks running, since no turn ended under it", () => {
    // Arrange.
    const { controller, ticker } = ticking();
    controller.applyPage(
      page([
        toolCallRow("t1", "running"),
        toolCallRow("t2", "running", { turn: "turn-1" }),
      ]),
      "replace",
    );
    // Act.
    controller.upsert(turnEndedRow("e", "turn-1"));
    // Assert: the row that names no turn is untouched.
    expect(ticker.live()).toBe(1);
  });
});

// A FOLD SURVIVES A FULL PAGE REPLACE (owner ruling, 2026-09-18: a redraw never
// un-toggles, whatever its shape). An upsert carries the reader's expansions
// off the element it replaces; a replace has no such element, so the feed
// snapshots them by row id across the teardown.

describe("createFeedController: folds across a page replace", () => {
  /** A fixture whose response rows draw one section of CLASSES apiece. */
  function drawing(className: string): Fixture {
    return fixture(
      harness(),
      {},
      {
        renderers: {
          response: () => {
            const el = document.createElement("div");
            el.className = className;
            return el;
          },
        },
      },
    );
  }

  /** The drawn fold of row ID, or null. */
  function fold(
    host: HTMLElement,
    id: string,
    cls = "tool-fold",
  ): HTMLElement | null {
    return host.querySelector<HTMLElement>(`[data-feed-row="${id}"] .${cls}`);
  }

  it("keeps a fold the reader opened open across the replace", () => {
    // Arrange
    const { controller, host } = drawing("tool-card tool-fold");
    controller.applyPage(page([responseRow("a")]), "replace");
    fold(host, "a")?.classList.add("expanded");
    // Act
    controller.applyPage(page([responseRow("a")]), "replace");
    // Assert
    expect(fold(host, "a")?.classList.contains("expanded")).toBe(true);
  });

  it("leaves a fold the reader never opened closed across the replace", () => {
    // Arrange
    const { controller, host } = drawing("tool-card tool-fold");
    controller.applyPage(page([responseRow("a")]), "replace");
    // Act
    controller.applyPage(page([responseRow("a")]), "replace");
    // Assert
    expect(fold(host, "a")?.classList.contains("expanded")).toBe(false);
  });

  it("retains no key for a row the replacing page dropped", () => {
    // Arrange: the reader opens `a`, and the next page no longer serves it.
    const { controller, host } = drawing("tool-card tool-fold");
    controller.applyPage(page([responseRow("a"), responseRow("b")]), "replace");
    fold(host, "a")?.classList.add("expanded");
    controller.applyPage(page([responseRow("b")]), "replace");
    // Act: `a` comes back later as a row nobody has opened.
    controller.applyPage(page([responseRow("a")]), "replace");
    // Assert
    expect(fold(host, "a")?.classList.contains("expanded")).toBe(false);
  });

  it("keeps an expanded bubble's scroll box open across the replace", () => {
    // Arrange: the 50vh response bubble rides the same carry path.
    const { controller, host } = drawing("bubble bubble-scroll");
    controller.applyPage(page([responseRow("a")]), "replace");
    fold(host, "a", "bubble-scroll")?.classList.add("expanded");
    // Act
    controller.applyPage(page([responseRow("a")]), "replace");
    // Assert
    expect(
      fold(host, "a", "bubble-scroll")?.classList.contains("expanded"),
    ).toBe(true);
  });
});

/**
 * EVERY ROW IS PLACED BY THE DAEMON'S KEY, NEVER BY ARRIVAL (owner ruling,
 * 2026-09-27: a late row lands where it would have been had it not been late).
 * `FeedRow.order` is opaque and compared as a string; these tests state every
 * key they depend on rather than leaning on the fixtures' build order.
 */
describe("createFeedController: every row is placed by its order key", () => {
  /** A page of rows keyed exactly as listed. */
  function keyed(rows: ReadonlyArray<[FeedRow, string]>): FeedRow[] {
    return rows.map(([row, key]) => withOrder(row, key));
  }

  /** The incident's feed: a prompt, a response, the answer and its turn end. */
  function incidentPage(hasMore = false) {
    return page(
      keyed([
        [userPromptRow("prompt", "why", "t1"), "k0100"],
        [responseRow("early", "looking", undefined, "t1"), "k0200"],
        [responseRow("answer", "because", undefined, "t1"), "k0500"],
        [turnEndedRow("end", "t1", "concluded", "answer"), "k0600"],
      ]),
      { hasMore },
    );
  }

  it("draws two late rows at their true place, not below the rows that arrived before them", () => {
    // Arrange — the answer and its turn end are already drawn.
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act — two response rows whose keys sort between `early` and `answer`
    // arrive AFTER everything else (the 14:36:58 push).
    controller.upsert(
      withOrder(responseRow("late-1", "one", undefined, "t1"), "k0300"),
    );
    controller.upsert(
      withOrder(responseRow("late-2", "two", undefined, "t1"), "k0400"),
    );
    // Assert
    expect(drawnIds(host)).toEqual([
      "prompt",
      "early",
      "late-1",
      "late-2",
      "answer",
      "end",
    ]);
  });

  it("places a page's rows by their keys, not by the page's sequence", () => {
    // Arrange
    const { controller, host } = fixture();
    // Act
    controller.applyPage(
      page(
        keyed([
          [responseRow("b"), "k2"],
          [responseRow("a"), "k1"],
        ]),
      ),
      "replace",
    );
    // Assert
    expect(drawnIds(host)).toEqual(["a", "b"]);
  });

  it("compares keys code unit by code unit, a prefix first", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(
      page(
        keyed([
          [responseRow("ab"), "ab"],
          [responseRow("b"), "b"],
        ]),
      ),
      "replace",
    );
    // Act
    controller.upsert(withOrder(responseRow("a"), "a"));
    // Assert
    expect(drawnIds(host)).toEqual(["a", "ab", "b"]);
  });

  it("appends a row whose key sorts after every held row at the tail", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act
    controller.upsert(
      withOrder(userPromptRow("next", "and then", "t2"), "k0700"),
    );
    // Assert
    expect(drawnIds(host).at(-1)).toBe("next");
  });

  it("records an inserted row at INFO with its key and position", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act
    controller.upsert(withOrder(responseRow("late-1"), "k0300"));
    // Assert
    const record = await forwardedRecord(capture, "feed.row-placed");
    expect({ level: record.level.case, context: record.context }).toEqual({
      level: "info",
      context: expect.objectContaining({
        row: "late-1",
        key: "k0300",
        outcome: "inserted",
        position: 2,
      }) as unknown,
    });
  });

  it("records a row appended at the tail at INFO", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act
    controller.upsert(withOrder(responseRow("next"), "k0700"));
    // Assert
    const record = await forwardedRecord(capture, "feed.row-placed");
    expect({ level: record.level.case, context: record.context }).toEqual({
      level: "info",
      context: expect.objectContaining({
        row: "next",
        key: "k0700",
        outcome: "appended",
        position: 4,
      }) as unknown,
    });
  });

  it("records a placed page at INFO with its row count and key span", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    // Act
    controller.applyPage(incidentPage(), "replace");
    // Assert
    const record = await forwardedRecord(capture, "feed.page-placed");
    expect({ level: record.level.case, context: record.context }).toEqual({
      level: "info",
      context: expect.objectContaining({
        rows: 4,
        first_key: "k0100",
        last_key: "k0600",
      }) as unknown,
    });
  });

  it("does not draw a late row older than the loaded page while older pages remain", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(true), "replace");
    // Act
    controller.upsert(withOrder(responseRow("history"), "k0050"));
    // Assert
    expect(drawnIds(host)).toEqual(["prompt", "early", "answer", "end"]);
  });

  it("records a late row left to the walk at INFO as unloaded history", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(incidentPage(true), "replace");
    // Act
    controller.upsert(withOrder(responseRow("history"), "k0050"));
    // Assert
    const record = await forwardedRecord(capture, "feed.row-placed");
    expect({ level: record.level.case, context: record.context }).toEqual({
      level: "info",
      context: expect.objectContaining({
        row: "history",
        key: "k0050",
        outcome: "unloadedHistory",
      }) as unknown,
    });
  });

  it("draws the late row in its place once the walk brings its page", async () => {
    // Arrange — the older page the daemon serves holds the late row in place.
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: {
            case: "success",
            value: page(
              keyed([
                [responseRow("oldest"), "k0010"],
                [responseRow("history"), "k0050"],
              ]),
            ),
          },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(incidentPage(true), "replace");
    controller.upsert(withOrder(responseRow("history"), "k0050"));
    // Act
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    // Assert
    expect(drawnIds(host)).toEqual([
      "oldest",
      "history",
      "prompt",
      "early",
      "answer",
      "end",
    ]);
  });

  it("inserts a late row older than every held row at the top when the feed is at its start", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(false), "replace");
    // Act
    controller.upsert(withOrder(responseRow("first"), "k0050"));
    // Assert
    expect(drawnIds(host)[0]).toBe("first");
  });

  it("never moves a held row when it is re-pushed", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act — the early response grows.
    controller.upsert(
      withOrder(
        responseRow("early", "looking harder", undefined, "t1"),
        "k0200",
      ),
    );
    // Assert
    expect(drawnIds(host)).toEqual(["prompt", "early", "answer", "end"]);
  });

  it("keeps a held row where it was when a re-push changes its key", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act — the daemon breaks the fixed-key invariant.
    controller.upsert(
      withOrder(responseRow("early", "moved?", undefined, "t1"), "k0900"),
    );
    // Assert
    expect(drawnIds(host)).toEqual(["prompt", "early", "answer", "end"]);
  });

  it("records a re-push that changed a held row's key at ERROR, naming both keys", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act
    controller.upsert(
      withOrder(responseRow("early", "moved?", undefined, "t1"), "k0900"),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.row-order-changed");
    expect({ level: record.level.case, context: record.context }).toEqual({
      level: "error",
      context: expect.objectContaining({
        row: "early",
        placed_key: "k0200",
        pushed_key: "k0900",
      }) as unknown,
    });
  });

  it("records a removal carrying another key than its row's at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act
    controller.upsert(withOrder(removedRow("early"), "k0900"));
    // Assert
    const record = await forwardedRecord(capture, "feed.row-order-changed");
    expect(record.level.case).toBe("error");
  });

  it("records a new row carrying another row's key at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { controller } = fixture();
    controller.applyPage(incidentPage(), "replace");
    // Act
    controller.upsert(withOrder(responseRow("twin"), "k0200"));
    // Assert
    const record = await forwardedRecord(capture, "feed.row-order-duplicate");
    expect({ level: record.level.case, context: record.context }).toEqual({
      level: "error",
      context: expect.objectContaining({
        row: "twin",
        key: "k0200",
        holder: "early",
      }) as unknown,
    });
  });

  it.each([
    [
      "a pushed row with no order",
      () => withoutOrder(responseRow("x")),
      "FeedRow.order",
    ],
    [
      "a pushed row with an empty key",
      () => withOrder(responseRow("x"), ""),
      "FeedRow.order.key",
    ],
    [
      "a removal with no order",
      () => withoutOrder(removedRow("x")),
      "FeedRow.order",
    ],
  ])("refuses %s as a malformed view", (_name, build, path) => {
    // Arrange
    const { controller } = fixture();
    let refused: unknown = null;
    // Act
    try {
      controller.upsert(build());
    } catch (err) {
      refused = err;
    }
    // Assert
    expect(refused instanceof MalformedView ? refused.path : refused).toBe(
      path,
    );
  });

  it("refuses a page holding a row with no order and keeps the rows already drawn", () => {
    // Arrange
    const { controller, host } = fixture();
    controller.applyPage(incidentPage(), "replace");
    let refused: unknown = null;
    // Act
    try {
      controller.applyPage(
        page([responseRow("a"), withoutOrder(responseRow("b"))]),
        "replace",
      );
    } catch (err) {
      refused = err;
    }
    // Assert
    expect({
      refused: refused instanceof MalformedView,
      drawn: drawnIds(host),
    }).toEqual({
      refused: true,
      drawn: ["prompt", "early", "answer", "end"],
    });
  });

  it("files an older page holding a row with no order as frame_undecodable", async () => {
    // Arrange
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, {
          result: {
            case: "success",
            value: page([withoutOrder(responseRow("older"))]),
          },
        }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(incidentPage(true), "replace");
    // Act
    host.querySelector<HTMLElement>("[data-load-more]")?.click();
    await settle();
    // Assert
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });

  it("never places a new row by arrival order (source scan)", () => {
    // Arrange — feed-view.ts, comments stripped.
    const source = codeOf(
      readFileSync(join(process.cwd(), "src/feed/feed-view.ts"), "utf8"),
    );
    // Act — every insertion into the order, and every arrival-order index.
    const insertions = [
      ...source.matchAll(/\border\.splice\(([^,)]*),\s*0\b/g),
    ].map((m) => m[1].trim());
    const arrival = [
      ...source.matchAll(/\border\.(?:push|unshift)\(/g),
      ...source.matchAll(/\(\s*[^()]*order\.length[^()]*,\s*0\s*,/g),
      ...source.matchAll(
        /(?:adopt|insertAt|adoptPageRow)\([^;]*order\.length/g,
      ),
    ].map((m) => m[0]);
    // Assert — one insertion, at the index the key's binary search found.
    expect({ insertions, arrival }).toEqual({
      insertions: ["index"],
      arrival: [],
    });
  });
});

describe("createFeedController: selected and expanded are one state", () => {
  /** A capped bubble, as drawBubble lays one out: the box under the bubble. */
  function cappedBubble(): HTMLElement {
    const el = document.createElement("div");
    el.className = "bubble";
    const box = document.createElement("div");
    box.className = "bubble-scroll";
    el.append(box);
    return el;
  }

  /** A controller on FEED drawing capped bubbles, its expand hook recorded. */
  function governing(feed: "root" | FeedId = "root") {
    const h = harness();
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const expands: Array<[string, boolean]> = [];
    const controller = createFeedController({
      ctx: h.ctx,
      host,
      feed,
      renderers: stubRenderers({ response: cappedBubble }),
      body: defaultBubbleBody,
      revealRow: async () => false,
      bubble: (row) => stubBubble(row),
      bodyContext: {
        ctx: h.ctx,
        feed,
        row: create(FeedRowSchema, {}),
        revealRow: async () => false,
      },
      onSelectionExpand: (section, expanded) => {
        expands.push([
          section.closest("[data-feed-row]")?.getAttribute("data-feed-row") ??
            "",
          expanded,
        ]);
      },
    });
    return { controller, host, expands };
  }

  /** A response row the daemon published selectable. */
  const selectableRow = (id: string): FeedRow => selectable(responseRow(id));

  /** The box of row ID in HOST. */
  function boxOf(host: HTMLElement, id: string): HTMLElement {
    const box = host.querySelector<HTMLElement>(
      `[data-feed-row="${id}"] .bubble > .bubble-scroll`,
    );
    if (box === null) throw new Error(`row ${id} drew no bubble box`);
    return box;
  }

  /** The daemon's selection of ID as a non-final bubble, or none. */
  const bubbleSelection = (id?: string) =>
    create(FeedSelectionSchema, {
      selection:
        id === undefined
          ? {
              case: "none",
              value: { viewport: { case: "returnToTail", value: {} } },
            }
          : { case: "bubble", value: { row: feedId(id) } },
    });

  it("stamps a root response row governed and, once the daemon says so, selectable", () => {
    // Arrange
    const { controller, host } = governing();
    // Act
    controller.upsert(selectableRow("r1"));
    // Assert
    const el = host.querySelector('[data-feed-row="r1"]');
    expect([
      el?.hasAttribute("data-selection-governed"),
      el?.hasAttribute("data-selectable"),
    ]).toEqual([true, true]);
  });

  it("stamps nothing on a sub-feed's rows", () => {
    // Arrange
    const { controller, host } = governing(feedId("sub"));
    // Act
    controller.upsert(selectableRow("r1"));
    // Assert
    expect(
      host
        .querySelector('[data-feed-row="r1"]')
        ?.hasAttribute("data-selection-governed"),
    ).toBe(false);
  });

  it("marks a bubble-arm selection with the selection mark", () => {
    // Arrange
    const { controller, host } = governing();
    controller.upsert(selectableRow("r1"));
    // Act
    controller.applySelection(bubbleSelection("r1"));
    // Assert
    expect(
      host
        .querySelector('[data-feed-row="r1"]')
        ?.getAttribute(SELECTED_ROW_ATTRIBUTE),
    ).toBe("bubble");
  });

  it("expands the selected bubble", () => {
    // Arrange
    const { controller, host } = governing();
    controller.upsert(selectableRow("r1"));
    // Act
    controller.applySelection(bubbleSelection("r1"));
    // Assert
    expect(boxOf(host, "r1").classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("collapses the bubble a new selection moved off", () => {
    // Arrange
    const { controller, host } = governing();
    controller.upsert(selectableRow("r1"));
    controller.upsert(selectableRow("r2"));
    controller.applySelection(bubbleSelection("r1"));
    // Act
    controller.applySelection(bubbleSelection("r2"));
    // Assert
    expect(
      [boxOf(host, "r1"), boxOf(host, "r2")].map((b) =>
        b.classList.contains(EXPANDED_CLASS),
      ),
    ).toEqual([false, true]);
  });

  it("collapses the selected bubble when the selection ends", () => {
    // Arrange
    const { controller, host } = governing();
    controller.upsert(selectableRow("r1"));
    controller.applySelection(bubbleSelection("r1"));
    // Act
    controller.applySelection(bubbleSelection());
    // Assert
    expect(boxOf(host, "r1").classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("runs the host's expand hook for every box it opens or closes, and no other", () => {
    // Arrange
    const { controller, expands } = governing();
    controller.upsert(selectableRow("r1"));
    controller.upsert(selectableRow("r2"));
    // Act
    controller.applySelection(bubbleSelection("r1"));
    controller.applySelection(bubbleSelection("r2"));
    // Assert
    expect(expands).toEqual([
      ["r1", true],
      ["r1", false],
      ["r2", true],
    ]);
  });

  it("answers the selected row", () => {
    // Arrange
    const { controller } = governing();
    controller.upsert(selectableRow("r1"));
    // Act
    controller.applySelection(bubbleSelection("r1"));
    // Assert
    expect(controller.selectedRow()).toBe("r1");
  });

  it("reports a bubble arm with no row as malformed", () => {
    // Arrange
    const { controller } = governing();
    // Act / Assert
    expect(() =>
      controller.applySelection(
        create(FeedSelectionSchema, {
          selection: { case: "bubble", value: {} },
        }),
      ),
    ).toThrow(MalformedView);
  });
});
