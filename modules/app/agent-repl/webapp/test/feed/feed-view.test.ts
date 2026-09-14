// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedBreadcrumbSchema,
  FeedIdSchema,
  FeedPageSchema,
  FeedRowSchema,
  type FeedId,
  type FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { GetFeedPageResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { TurnIdSchema } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import { forgetOwnTurns, rememberOwnTurn } from "../../src/composer/own-turns.js";
import {
  createFeedController,
  isBubbleRow,
  type BubbleLike,
  type FeedController,
} from "../../src/feed/feed-view.js";
import { defaultBubbleBody, type RowContext } from "../../src/feed/renderers.js";
import { drawFeedSimpleToolCall } from "../../src/feed/cards/tool-call.js";
import {
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
  stubRenderers,
  subagentRow,
  turnEndedRow,
  userPromptRow,
  type Harness,
} from "./harness.js";
import { PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING } from "../../src/breathing.js";

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

function fixture(
  h: Harness = harness(),
  overrides: Partial<RowContext> = {},
  opts: { feed?: FeedId; renderers?: Partial<Parameters<typeof stubRenderers>[0]> } = {},
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

  it("marks a row whose turn this page submitted", () => {
    const { controller, host } = fixture();
    rememberOwnTurn(create(TurnIdSchema, { value: "turn-7" }));
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    expect(host.querySelector('[data-feed-row="a"]')?.getAttribute("data-mine")).toBe("true");
    forgetOwnTurns();
  });

  it("makes no claim on a row from another submitter's turn", () => {
    const { controller, host } = fixture();
    rememberOwnTurn(create(TurnIdSchema, { value: "turn-mine" }));
    controller.applyPage(page([userPromptRow("a", "1", "turn-7")]), "replace");
    expect(host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-mine")).toBe(false);
    forgetOwnTurns();
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

describe("createFeedController: the working prompt's thinking wave", () => {
  /** The prompt bubble inside row ID, as the feed drew it. */
  function promptBubble(host: HTMLElement, id: string): HTMLElement {
    const bubble = host.querySelector<HTMLElement>(
      `[data-feed-row="${id}"] .bubble.user`,
    );
    if (bubble === null) throw new Error(`no prompt bubble drawn for row ${id}`);
    return bubble;
  }

  it("waves the prompt whose turn is still in flight", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one", "t1")]), "replace");
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("does not wave a prompt whose turn concluded", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1"), turnEndedRow("e1", "t1")]),
      "replace",
    );
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("does not wave a prompt whose turn failed", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1"), turnEndedRow("e1", "t1", "errored")]),
      "replace",
    );
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("does not wave a prompt whose turn the user interrupted", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1"), turnEndedRow("e1", "t1", "interrupted")]),
      "replace",
    );
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("waves a prompt the daemon has not yet stamped with a turn", () => {
    // A locally minted, unacknowledged prompt is one the reader can see and
    // cannot possibly have an answer to yet (owner ruling, 2026-09-14).
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one")]), "replace");
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("waves a prompt from the push that drew it, before any other arrives", () => {
    const { controller, host } = fixture();
    controller.upsert(userPromptRow("p1", "one", "t1"));
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("stops the wave when the turn's FINAL ANSWER is marked", () => {
    // The conclusion names the answering row and carries no turn of its own,
    // so the only thing that can settle this prompt is the final-answer mark
    // the feed put on the answer — which carries the turn.
    const { controller, host } = fixture();
    controller.applyPage(
      page([
        userPromptRow("p1", "one", "t1"),
        responseRow("r1", "the answer", undefined, "t1"),
        turnEndedRow("e1", undefined, "concluded", "r1"),
      ]),
      "replace",
    );
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("keeps waving while an answer of ANOTHER turn is marked final", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([
        userPromptRow("p1", "one", "t1"),
        responseRow("r0", "an older answer", undefined, "t0"),
        turnEndedRow("e0", undefined, "concluded", "r0"),
      ]),
      "replace",
    );
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("does not re-add the wave to a settled prompt on a later push", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1"), turnEndedRow("e1", "t1")]),
      "replace",
    );
    controller.upsert(responseRow("r9", "something else"));
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("does not re-add the wave when the settled prompt is itself redrawn", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([userPromptRow("p1", "one", "t1"), turnEndedRow("e1", "t1")]),
      "replace",
    );
    controller.upsert(userPromptRow("p1", "one, edited", "t1"));
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("does not take the wave off a prompt whose turn has not ended", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one", "t1")]), "replace");
    controller.upsert(responseRow("r1", "still working", undefined, "t1"));
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("waves an agent-addressed prompt whose turn is in flight", () => {
    const { controller, host } = fixture();
    controller.applyPage(
      page([agentPromptRow("a1", "\u2192 Explore", "go", "t1")]),
      "replace",
    );
    expect(promptBubble(host, "a1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("goes on waving a prompt while ANOTHER turn ends", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one", "t1")]), "replace");
    controller.upsert(turnEndedRow("e0", "t0"));
    expect(promptBubble(host, "p1").getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(
      PROMPT_WAVE_WORKING,
    );
  });

  it("stops the wave when the turn's end arrives on the tail", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one", "t1")]), "replace");
    controller.upsert(turnEndedRow("e1", "t1"));
    expect(promptBubble(host, "p1").hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("stops the wave without redrawing the bubble it stopped", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one", "t1")]), "replace");
    const before = promptBubble(host, "p1");
    controller.upsert(turnEndedRow("e1", "t1"));
    expect(promptBubble(host, "p1")).toBe(before);
  });

  it("stops the wave without redrawing the prompt's text", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([userPromptRow("p1", "one", "t1")]), "replace");
    const body = promptBubble(host, "p1").querySelector(".bubble-body");
    controller.upsert(turnEndedRow("e1", "t1"));
    expect(promptBubble(host, "p1").querySelector(".bubble-body")).toBe(body);
  });
});

describe("createFeedController: following the tail", () => {
  /** A scroll box and a tail owner whose decisions the test dictates. */
  function scrollStub(following: boolean) {
    const acts: string[] = [];
    const box = {
      scrollTop: 0,
      scrollHeight: 1000,
      clientHeight: 100,
      querySelector: () => null,
    };
    const tail = {
      isFollowing: () => following,
      park: () => acts.push("park"),
      place: () => acts.push("place"),
    };
    return { box, tail, acts };
  }

  /** A controller wired to that box. */
  function scrolled(following: boolean) {
    const h = harness();
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const scroll = scrollStub(following);
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
    return { controller, acts: scroll.acts };
  }

  it("pulls the view to the tail while the reader is following it", () => {
    const { controller, acts } = scrolled(true);
    acts.length = 0;
    controller.upsert(responseRow("r1"));
    expect(acts).toContain("park");
  });

  it("leaves a reader who has scrolled away exactly where they are", () => {
    const { controller, acts } = scrolled(false);
    acts.length = 0;
    controller.upsert(responseRow("r1"));
    expect(acts).not.toContain("park");
  });

  it("anchors a following reader at the tail when older rows land above", () => {
    const { controller, acts } = scrolled(true);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
    acts.length = 0;
    controller.applyPage(page([responseRow("older")]), "prepend");
    expect(acts).toContain("park");
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
function rowWith(id: string, arm: MessageInitShape<typeof FeedRowSchema>["row"]): FeedRow {
  return create(FeedRowSchema, { id: feedId(id), row: arm });
}

function unknownArmRow(id: string): FeedRow {
  const row = create(FeedRowSchema, { id: feedId(id) });
  // A wire arm no build knows, past the generated union.
  (row as { row: unknown }).row = { case: "surprise", value: {} };
  return row;
}

/** A row carrying one activity UNIT. */
function unitRow(id: string, unit: unknown): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
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
          body: { blocks: [{ block: { case: "text", value: { text: "go" } } }] },
        },
      },
      ".prompt-agent",
    ],
    [
      "turnEnded",
      { case: "turnEnded", value: { endedAtMs: 1n, outcome: { case: "concluded", value: {} } } },
      "[data-arm='concluded']",
    ],
    [
      "separation",
      {
        case: "separation",
        value: { label: { text: "context cleared" }, kind: { case: "cleared", value: {} } },
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
    ["commandRefused", { case: "commandRefused", value: {} }, ".stub-commandRefused"],
  ];

  it.each(ARMS)("draws the %s arm", (name, arm, mark) => {
    // Arrange / Act
    const { controller, host } = fixture();
    controller.applyPage(page([rowWith(name, arm)]), "replace");
    // Assert
    expect(host.querySelector(`[data-feed-row="${name}"] ${mark}`)).not.toBeNull();
  });

  it("refuses a row arm this build does not know", () => {
    const { controller, h } = fixture();
    controller.applyPage(page([unknownArmRow("x")]), "replace");
    expect(h.sink.reported).toEqual(["frameUndecodable"]);
  });

  it("names FeedRow.row as the path of an unknown arm's refusal", () => {
    const { controller, host } = fixture();
    controller.applyPage(page([unknownArmRow("x")]), "replace");
    expect(host.querySelector(".row-malformed")?.textContent).toContain("FeedRow.row");
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
  ];

  it.each(UNITS)("draws the %s unit", (name, unit, mark) => {
    const { controller, host } = fixture();
    controller.applyPage(page([unitRow(name, unit)]), "replace");
    expect(host.querySelector(`[data-feed-row="${name}"] ${mark}`)).not.toBeNull();
  });

  it("refuses an activity unit this build does not know", () => {
    const { controller, host } = fixture();
    const row = unitRow("x", { case: "response", value: {} });
    // A wire arm no build knows: set past the generated union, which is exactly
    // what a newer daemon's field number looks like once it is decoded.
    (row.row.value as { unit: unknown }).unit = { case: "surprise", value: {} };
    controller.applyPage(page([row]), "replace");
    expect(host.querySelector(".row-malformed")?.textContent).toContain("FeedTurnActivity.unit");
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
      page([create(FeedRowSchema, { id: feedId("x"), row: { case: "activity", value: {} } })]),
      "replace",
    );
    expect(host.querySelector('[data-feed-row="x"]')?.hasAttribute("data-unit")).toBe(false);
  });

  it("stamps a row with no arm at all as malformed rather than dropping it", () => {
    const { controller, host } = fixture();
    controller.upsert(create(FeedRowSchema, { id: feedId("x") }));
    expect(host.querySelector('[data-feed-row="x"]')?.getAttribute("data-row-kind")).toBe(
      "malformed",
    );
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
    (bad.result.value as { kind: unknown }).kind = { case: "surprise", value: {} };
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
    const { controller, host } = fixture(harness(), {}, { renderers: stating("running") });
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(host.querySelector('[data-feed-row="a"]')?.getAttribute("data-state")).toBe("running");
  });

  it("states nothing on the chrome when the card states nothing", () => {
    const { controller, host } = fixture(harness(), {}, { renderers: stating(null) });
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(host.querySelector('[data-feed-row="a"]')?.hasAttribute("data-state")).toBe(false);
  });
});

describe("createFeedController: a feed that is not the root", () => {
  it("names the feed it draws by its own id", () => {
    const { host } = fixture(harness(), {}, { feed: create(FeedIdSchema, { value: "b1" }) });
    expect(host.getAttribute("data-feed")).toBe("b1");
  });

  it("walks THAT feed, addressing the page request to its id", async () => {
    // Arrange
    const h = harness();
    const { controller, host } = fixture(h, {}, { feed: create(FeedIdSchema, { value: "b1" }) });
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
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
    controller.applyPage(page([subagentRow("b1"), responseRow("r1")]), "replace");
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
        crumbs: [create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" })],
      }),
      "replace",
    );
    // Assert
    expect(controller.breadcrumbs().map((crumb) => crumb.label)).toEqual(["outer"]);
  });

  it("exposes the view a body renderer draws from", () => {
    const { controller } = fixture();
    controller.applyPage(page([responseRow("a")]), "replace");
    expect(controller.view().rows().map((row) => row.id?.value)).toEqual(["a"]);
  });
});

describe("createFeedController: drawing and disposal edges", () => {
  it("refuses to draw a row it does not hold", () => {
    const { controller } = fixture();
    expect(() => controller.drawRow(responseRow("nope"))).toThrow(/unheld row nope/);
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
    const { controller } = fixture(harness(), {}, {
      renderers: {
        response: () => {
          throw new TypeError("the renderer is broken");
        },
      },
    });
    // Act / Assert
    expect(() => controller.applyPage(page([responseRow("a")]), "replace")).toThrow(
      "the renderer is broken",
    );
  });
});

describe("createFeedController: the walk's refusal does not accumulate", () => {
  it("drops the previous walk's refusal when the control is used again", async () => {
    // Arrange: a daemon that refuses every walk.
    const h = harness({
      getFeedPage: () =>
        create(GetFeedPageResponseSchema, { result: { case: "error", value: {} } }),
    });
    const { controller, host } = fixture(h);
    controller.applyPage(page([responseRow("a")], { hasMore: true }), "replace");
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
    const f = fixture(harness({ ticker }), {}, { renderers: { simpleToolCall: drawFeedSimpleToolCall } });
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
    for (let i = 0; i < 3; i += 1) controller.upsert(toolCallRow("t", "running"));
    // Assert: one card, one clock — not one per push.
    expect(ticker.live()).toBe(1);
  });

  it("stops every remaining clock in a turn when that turn's end lands", () => {
    // Arrange: a call still drawn as running when its turn finishes.
    const { controller, ticker } = ticking();
    controller.applyPage(page([toolCallRow("t", "running", { turn: "turn-1" })]), "replace");
    expect(ticker.live()).toBe(1);
    // Act.
    controller.upsert(turnEndedRow("e", "turn-1"));
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

  it("leaves a turnless row's clocks running, since no turn ended under it", () => {
    // Arrange.
    const { controller, ticker } = ticking();
    controller.applyPage(
      page([toolCallRow("t1", "running"), toolCallRow("t2", "running", { turn: "turn-1" })]),
      "replace",
    );
    // Act.
    controller.upsert(turnEndedRow("e", "turn-1"));
    // Assert: the row that names no turn is untouched.
    expect(ticker.live()).toBe(1);
  });
});
