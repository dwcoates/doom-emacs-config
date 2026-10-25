// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  FeedComposerEntrySchema,
  FeedIdSchema,
  FeedResponseSchema,
  FeedRowSchema,
  type FeedResponse,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { OUTCOME_CARD_CLASS } from "../../../src/feed/marker.js";
import { heatColor } from "../../../src/heat.js";
import type { RowContext } from "../../../src/feed/cards/context.js";
import {
  REVEALED_ATTRIBUTE,
  RESPONSE_CUT_SHORT_ACTIONS_CLASS,
  RESPONSE_PROSE_CLASS,
  THINKING_BUBBLE_CLASS,
  INTERIM_RESPONSE_CLASS,
  drawFeedResponse,
  isInterimResponse,
  responseCap,
  responseCapLines,
  carriedNumber,
  revealedSoFar,
} from "../../../src/feed/cards/response.js";
import { proseHtml } from "../../../src/bubble/body.js";
import {
  SIGN_IN_ACTION,
  SIGN_IN_ACTION_ATTRIBUTE,
  answerLoginRequests,
} from "../../../src/login/request.js";
import { visibleWidth } from "../../../src/metaprompt-tree.js";
import { installTreeLayout, stagedCols, useTreeLayout } from "../../tree-layout.js";
import { stopTicking } from "../../../src/feed/ticking.js";
import { fireResize } from "../../resize-observer.js";
import stylesheet from "../../../src/styles.css?raw";
import responseSource from "../../../src/feed/cards/response.ts?raw";
import { cascadedValue, installStylesheet, selectorOf } from "../../stylesheet.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import { HAS_MORE_CLASS, refreshHasMore } from "../../../src/feed/bubble-more.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_MORE_ATTRIBUTE,
  BUBBLE_MORE_ELLIPSIS,
  BUBBLE_UNCAPPED,
} from "../../../src/bubble/draw.js";
import { BUBBLE_BOX_CLASS, BUBBLE_SCROLL_CLASS } from "../../../src/feed/bubble-scroll.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import { fakePageSeen } from "../../page-seen-fake.js";
import type { PageSeen } from "../../../src/page-seen.js";
import { orderFor } from "../../feed-order.js";
import { plainProse, servedProse, treeProse } from "../../prose.js";
import { boundCopyText } from "../../../src/copy-control.js";
import { BUBBLE_COPY_CLASS, COPY_ROOM_CLASS } from "../../../src/bubble/copy-room.js";
import { responseUsageOf } from "../../../src/feed/response-usage.js";
import { COMPOSER_ENTRY_CLASS, createComposerEntryHighlight } from "../../../src/feed/composer-entry.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** The reveal pace every live draw carries: the daemon's thinking default. */
const PACE = { minCharsPerSecond: 45, maxCharsPerSecond: 55, bankLimitMs: 1500 };

/** A row context on a page SEEN says; the response card calls no verb of its own. */
function rowContext(previous?: HTMLElement, seen: PageSeen = fakePageSeen()): RowContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {});
  });
  return {
    ctx: testAppContext({
      client: createAgentReplClient(transport),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      seen,
      composerEnabled: false,
    }),
    feed: "root",
    arrival: "page",
    row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }), order: orderFor("row-1") }),
    revealRow: async () => true,
    previous,
  };
}

function response(init: MessageInitShape<typeof FeedResponseSchema>): FeedResponse {
  return create(FeedResponseSchema, init);
}

/**
 * The rendered prose's length as the reveal counts it (src/bubble/text-reveal.ts):
 * characters of the RENDER, which carries the newline markdown-it ends each
 * block with. Read once the prose is fully revealed.
 */
function renderedLength(el: HTMLElement): string {
  return String(el.querySelector(`.${RESPONSE_PROSE_CLASS}`)?.textContent?.length);
}

/**
 * Place a drawn bubble in the document under its own containing block (the
 * `.feed-item` stand-in), the way feed-view attaches a row after drawing it,
 * and answer that block.
 */
function mount(el: HTMLElement): HTMLElement {
  const column = document.createElement("div");
  column.append(el);
  document.body.append(column);
  return column;
}

/** The tree the metaprompt renderer recognizes, with its header. */
const TREE = ["Response (✏️ changes made)", "", "1 🔧 Fixed it", "├── 1.1 Detail", "└── 1.2 More"].join(
  "\n",
);

/**
 * The type-out runs on animation frames, which vitest does NOT fake by
 * default — so this suite asks for them explicitly alongside the clock the
 * reveal reads (the shared ticker's `Date.now`). Without both, the pacing
 * could only be tested against real time.
 */
beforeEach(() => {
  vi.useFakeTimers({
    toFake: [
      "setTimeout",
      "clearTimeout",
      "setInterval",
      "clearInterval",
      "Date",
      "requestAnimationFrame",
      "cancelAnimationFrame",
    ],
  });
  vi.setSystemTime(0);
});
afterEach(() => {
  vi.useRealTimers();
  // A global stubbed away here must not outlive the test: the unit run shares
  // one jsdom across files, so a missing `requestAnimationFrame` would silently
  // change how every later suite paints.
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

const ANSWERED_TREE = "1 🔧 Did it\n├── 1.1 first\n└── 1.2 second";

describe("a final response's copy control", () => {
  it("copies the response's markdown as served", () => {
    // Act
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: plainProse("**done**") } } } }),
      rowContext(),
    );
    // Assert
    const control = el.querySelector(`.${BUBBLE_COPY_CLASS}`);
    expect([control?.getAttribute("data-copies"), control === null ? null : boundCopyText(control).text]).toEqual([
      "response",
      "**done**",
    ]);
  });

  it("stands alone in the box's corner, before the body", () => {
    // Act
    const el = drawFeedResponse(
      response({
        finalAnswer: true,
        usage: { text: "1.2k", heat: { position: 0.5 } },
        result: { case: "success", value: { prose: { prose: plainProse("done") } } },
      }),
      rowContext(),
    );
    // Assert
    const box = el.querySelector(`:scope > .${BUBBLE_BOX_CLASS}`);
    expect([...(box?.children ?? [])].map((c) => c.classList.contains(BUBBLE_COPY_CLASS))).toEqual([true, false]);
  });

  it("opens the prose's first line with the room it takes", () => {
    // Act
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: plainProse("done\n\nmore") } } } }),
      rowContext(),
    );
    // Assert
    const ghosts = [...el.querySelectorAll(`.${COPY_ROOM_CLASS}`)];
    expect(ghosts.map((g) => [g.parentElement?.tagName, g.parentElement?.textContent])).toEqual([["P", "done"]]);
  });

  it.each([
    ["an interim response", { finalAnswer: false }],
    ["a final thinking block", { finalAnswer: true, thinking: true }],
  ])("is not drawn on %s", (_name, flags) => {
    // Act
    const el = drawFeedResponse(
      response({ ...flags, result: { case: "success", value: { prose: { prose: plainProse("done") } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(`.${BUBBLE_COPY_CLASS}`)).toBeNull();
  });

  it.each([
    ["an interim response", { finalAnswer: false }],
    ["a final thinking block", { finalAnswer: true, thinking: true }],
  ])("leaves no room on %s's first line", (_name, flags) => {
    // Act
    const el = drawFeedResponse(
      response({ ...flags, result: { case: "success", value: { prose: { prose: plainProse("done") } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(`.${COPY_ROOM_CLASS}`)).toBeNull();
  });
});

describe("the composer's box in a response", () => {
  useTreeLayout();

  it("is drawn on the response's own paint", () => {
    // Arrange: the composer answers 1.1 of this row.
    createComposerEntryHighlight(document).apply(
      create(FeedComposerEntrySchema, { at: { case: "node", value: { row: { value: "row-1" }, label: "1.1" } } }),
    );
    // Act
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: treeProse(ANSWERED_TREE) } } } }),
      rowContext(),
    );
    mount(el);
    // Assert
    expect([...el.querySelectorAll(`.${COMPOSER_ENTRY_CLASS}`)].map((l) => l.getAttribute("data-node"))).toEqual(["1.1"]);
    createComposerEntryHighlight(document).apply(create(FeedComposerEntrySchema, { at: { case: "none", value: {} } }));
  });
});

describe("the settled state", () => {
  it("renders the whole markdown", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("**done**") } } } }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body strong")?.textContent).toBe("done");
  });

  it("carries the state arm", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("hi") } } } }),
      rowContext(),
    );
    expect(el.getAttribute("data-state")).toBe("success");
  });

  it("records the whole prose as shown, so a later push does not re-type it", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("hello") } } } }),
      rowContext(),
    );
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe(renderedLength(el));
  });

  it("re-renders a metaprompt tree as tree lines rather than markdown", () => {
    const { uninstall } = installTreeLayout();
    try {
      const el = drawFeedResponse(
        response({ result: { case: "success", value: { prose: { prose: treeProse(TREE) } } } }),
        rowContext(),
      );
      mount(el);
      expect(el.querySelector(".mp-tree")).not.toBeNull();
    } finally {
      uninstall();
    }
  });
});

describe("the broken state", () => {
  it("keeps the prose that landed", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("half a thou") } } } }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body")?.textContent).toContain("half a thou");
  });

  it("marks the bubble as cut short", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("half") } } } }),
      rowContext(),
    );
    expect(el.classList.contains("response-cut-short")).toBe(true);
  });

  it("draws the marker without a reason, which is the turn's to state", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("half") } } } }),
      rowContext(),
    );
    expect(el.querySelector(".response-cut-short-marker")?.textContent).toBe("cut short");
  });

  it("offers the sign-in action inside the bubble when the daemon set it", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("Failed to authenticate.") }, signIn: {} } } }),
      rowContext(),
    );
    const action = el.querySelector(`.${RESPONSE_CUT_SHORT_ACTIONS_CLASS} [${SIGN_IN_ACTION_ATTRIBUTE}="${SIGN_IN_ACTION}"]`);
    expect([action?.localName, action?.textContent]).toEqual(["ar-button", "sign in"]);
  });

  it("offers no sign-in action when the daemon did not set it", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("half") } } } }),
      rowContext(),
    );
    expect(el.querySelector(`.${RESPONSE_CUT_SHORT_ACTIONS_CLASS}`)).toBeNull();
  });

  it("opens the login overlay from the bubble's sign-in without the bubble taking the click", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("Failed to authenticate.") }, signIn: {} } } }),
      rowContext(),
    );
    const opened: HTMLElement[] = [];
    const stop = answerLoginRequests((control) => opened.push(control));
    let bubbleClicks = 0;
    el.addEventListener("click", () => bubbleClicks++);
    const action = el.querySelector(`[${SIGN_IN_ACTION_ATTRIBUTE}="${SIGN_IN_ACTION}"]`) as HTMLElement;
    action.click();
    stop();
    expect([opened, bubbleClicks]).toEqual([[action], 0]);
  });
});

describe("the arriving state", () => {
  it("draws no arriving indicator: a streaming response shows its prose only", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("typing") }, revealPace: PACE } } }),
      rowContext(),
    );
    expect(el.querySelector(".response-arriving")).toBeNull();
    expect(el.querySelector(".animated-ellipsis")).toBeNull();
  });

  it("shows nothing of a first-seen response before the first frame", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("hello world") }, revealPace: PACE } } }),
      rowContext(),
    );
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe("0");
  });

  it("types the prose out as time passes", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("hello world") }, revealPace: PACE } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    vi.advanceTimersByTime(1000);
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe(renderedLength(el));
  });

  it("resumes from the previous draw's position rather than restarting", () => {
    // Arrange: the element the previous push of this same row returned.
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "5");
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("hello world") }, revealPace: PACE } } }),
      rowContext(previous),
    );
    // Assert: the first paint already carries what was on screen.
    expect(el.querySelector(".bubble-body")?.textContent).toContain("hello");
  });

  it("stops animating once the bubble has been replaced", () => {
    // Arrange: prose long enough that the reveal is still behind when the
    // bubble is taken out of the document.
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("x".repeat(5000)) }, revealPace: PACE } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    vi.advanceTimersByTime(100);
    const shown = el.getAttribute(REVEALED_ATTRIBUTE);
    // Act
    el.remove();
    vi.advanceTimersByTime(5000);
    // Assert
    expect([el.getAttribute(REVEALED_ATTRIBUTE), shown === "5000"]).toEqual([shown, false]);
  });
});

describe("the daemon's reveal pace", () => {
  /** The element a previous push of the row returned, LENGTH characters shown. */
  function previousAt(length: number): HTMLElement {
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, String(length));
    return previous;
  }

  /** A paced bubble: MARKDOWN in ARM, at the thinking default pace. */
  function paced(arm: "update" | "success", markdown: string, revealPace = PACE): FeedResponse {
    return response({ result: { case: arm, value: { prose: { prose: servedProse(markdown, { arriving: arm === "update" }) }, revealPace } } });
  }

  it("types an arriving bubble out at the maximum while the arrivals outrun it", () => {
    // Arrange: 60 characters banked, within the 82.5-character limit.
    const el = drawFeedResponse(paced("update", "x".repeat(60)), rowContext());
    document.body.appendChild(el);
    // Act
    vi.advanceTimersByTime(500);
    // Assert: about 27 at 55 chars/sec.
    const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
    expect(shown > 22 && shown < 30).toBe(true);
  });

  it("catches a bank past its limit up to the limit rather than falling further behind", () => {
    // Arrange: 5000 characters banked, far past 1.5s at 55 chars/sec.
    const el = drawFeedResponse(paced("update", "x".repeat(5000)), rowContext());
    document.body.appendChild(el);
    // Act
    vi.advanceTimersByTime(100);
    // Assert: well past the 5.5 the maximum alone would show.
    expect(Number(el.getAttribute(REVEALED_ATTRIBUTE))).toBeGreaterThan(200);
  });

  it("resumes from what the previous push left on screen", () => {
    // Arrange / Act: the previous push had only shown 4 of what had arrived.
    const el = drawFeedResponse(paced("update", "abcdefghijklmnopqrstuvwxyz"), rowContext(previousAt(4)));
    // Assert
    expect(el.querySelector(".bubble-body")?.textContent?.trim()).toBe("abcd");
  });

  it("types out a settled bubble's leftover text instead of drawing it at once", () => {
    // Arrange
    const el = drawFeedResponse(paced("success", "hello world"), rowContext(previousAt(5)));
    document.body.appendChild(el);
    // Act
    const atDraw = el.getAttribute(REVEALED_ATTRIBUTE);
    vi.advanceTimersByTime(1000);
    // Assert
    expect([atDraw, el.getAttribute(REVEALED_ATTRIBUTE)]).toEqual(["5", renderedLength(el)]);
  });

  it("draws an unpaced arriving bubble whole at once: a replay", () => {
    // Arrange / Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("hello world") } } } }),
      rowContext(),
    );
    // Assert
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe(renderedLength(el));
  });

  it("draws an unpaced settled bubble whole at once", () => {
    // Arrange / Act
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("hello world") } } } }),
      rowContext(previousAt(5)),
    );
    // Assert
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe(renderedLength(el));
  });

  it("never shows a half-typed construct's raw syntax while typing it out", () => {
    // Arrange
    const el = drawFeedResponse(paced("update", "**bold** and more"), rowContext());
    document.body.appendChild(el);
    // Act
    vi.advanceTimersByTime(100);
    // Assert
    expect(el.querySelector(`.${RESPONSE_PROSE_CLASS}`)?.textContent?.includes("*")).toBe(false);
  });

  it("refuses a malformed pace", () => {
    // Arrange
    const draw = (): HTMLElement =>
      drawFeedResponse(paced("update", "hi", { minCharsPerSecond: 0, maxCharsPerSecond: 55, bankLimitMs: 1500 }), rowContext());
    // Act / Assert
    expect(draw).toThrow(MalformedView);
  });
});

describe("the usage stamp", () => {
  const STATES = ["update", "success", "error"] as const;

  /** A response in ARM carrying USAGE, if any. */
  function stamped(arm: (typeof STATES)[number], usage?: { text: string; atMs?: bigint; heat?: { position: number } }) {
    return response({
      ...(usage === undefined ? {} : { usage }),
      result: { case: arm, value: { prose: { prose: plainProse("hi") } } },
    });
  }

  it.each(STATES)("draws no figure in the bubble in the %s state", (arm) => {
    // Act
    const el = drawFeedResponse(stamped(arm, { text: "2.1k", atMs: 1_000n, heat: { position: 0.5 } }), rowContext());
    // Assert
    expect(el.textContent).not.toContain("2.1k");
  });

  it.each(STATES)("records the stamp for the footer's hover in the %s state", (arm) => {
    // Act
    const el = drawFeedResponse(stamped(arm, { text: "2.1k", atMs: 1_000n, heat: { position: 0.5 } }), rowContext());
    // Assert
    expect(responseUsageOf(el)).toEqual({ text: "2.1k", color: heatColor(0.5, "p"), settledAtMs: 1_000 });
  });

  it("records an arriving stamp as not settled", () => {
    // Act
    const el = drawFeedResponse(stamped("update", { text: "2.1k", atMs: 0n, heat: { position: 0.5 } }), rowContext());
    // Assert
    expect(responseUsageOf(el)?.settledAtMs).toBeNull();
  });

  it("records no usage when none has been observed", () => {
    // Act
    const el = drawFeedResponse(stamped("success"), rowContext());
    // Assert
    expect(responseUsageOf(el)).toBeNull();
  });

  it("forgets the usage when a redraw carries none", () => {
    // Arrange
    const first = drawFeedResponse(stamped("success", { text: "2.1k", heat: { position: 0.5 } }), rowContext());
    // Act
    const again = drawFeedResponse(stamped("success"), rowContext(first));
    // Assert
    expect([again === first, responseUsageOf(again)]).toEqual([true, null]);
  });

  it("restates the usage when a redraw changes the figure", () => {
    // Arrange
    const first = drawFeedResponse(stamped("update", { text: "1k", heat: { position: 0.5 } }), rowContext());
    // Act
    drawFeedResponse(stamped("update", { text: "2k", heat: { position: 0.5 } }), rowContext(first));
    // Assert
    expect(responseUsageOf(first)?.text).toBe("2k");
  });

  it("refuses a stamp that carries no heat", () => {
    expect(() => drawFeedResponse(stamped("success", { text: "40k" }), rowContext())).toThrow(MalformedView);
  });

  it("refuses a settle instant beyond a safe integer as a malformed view", () => {
    // Arrange
    const malformed = stamped("success", { text: "2.1k", atMs: BigInt(Number.MAX_SAFE_INTEGER) + 1n, heat: { position: 0.5 } });
    // Act
    const draw = (): HTMLElement => drawFeedResponse(malformed, rowContext());
    // Assert
    expect(draw).toThrow(MalformedView);
  });

  it("keeps a bubble's recorded usage when a re-push's stamp is malformed", () => {
    // Arrange
    const first = drawFeedResponse(stamped("success", { text: "1k", heat: { position: 0.5 } }), rowContext());
    // Act
    expect(() => drawFeedResponse(stamped("success", { text: "2k" }), rowContext(first))).toThrow(MalformedView);
    // Assert
    expect(responseUsageOf(first)?.text).toBe("1k");
  });
});

describe("a vendor notice draws as its outcome card", () => {
  /** Every state arm: the notice is a FIELD, so it rides all of them. */
  const STATES = (
    FeedResponseSchema.oneofs.find((oneof) => oneof.name === "result")?.fields ?? []
  ).map((field) => field.localName);

  /** The notice's neutral head, as the daemon composes it. */
  const NOTICE = { marker: { label: { text: "vendor notice" }, detail: { text: "allowance exhausted" }, family: { case: "neutral" as const, value: {} } } };

  it("covers every state arm the schema declares", () => {
    expect([...STATES].sort()).toEqual(["error", "success", "update"].sort());
  });

  it.each(STATES)("draws the card and no bubble in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({ notice: NOTICE, result: { case: arm as never, value: { prose: { prose: plainProse("hi") } } as never } }),
      rowContext(),
    );
    expect([el.classList.contains(OUTCOME_CARD_CLASS), el.classList.contains("bubble")]).toEqual([true, false]);
  });

  it.each(STATES)("draws the vendor's words verbatim as the face in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({ notice: NOTICE, result: { case: arm as never, value: { prose: { prose: plainProse("You've hit your **limit**") } } as never } }),
      rowContext(),
    );
    expect([...el.querySelectorAll(".outcome-card-said")].map((line) => line.textContent)).toEqual(["You've hit your **limit**"]);
  });

  it("heads the card with the daemon's marker verbatim", () => {
    const el = drawFeedResponse(
      response({ notice: NOTICE, result: { case: "success", value: { prose: { prose: servedProse("hi") } } } }),
      rowContext(),
    );
    expect(el.querySelector(".outcome-card-head")?.textContent).toBe("◼vendor notice · allowance exhausted");
  });

  it("draws a notice card that does not open: a notice has nothing behind it", () => {
    const el = drawFeedResponse(
      response({ notice: NOTICE, result: { case: "success", value: { prose: { prose: servedProse("hi") } } } }),
      rowContext(),
    );
    expect([el.classList.contains("tool-fold"), el.querySelector(".outcome-marker-expansion")]).toEqual([false, null]);
  });

  it("refuses a notice with no marker", () => {
    expect(() =>
      drawFeedResponse(response({ notice: {}, result: { case: "success", value: { prose: { prose: servedProse("hi") } } } }), rowContext()),
    ).toThrow(MalformedView);
  });

  it.each(STATES)("draws an ordinary %s response as a bubble, no card", (arm) => {
    const el = drawFeedResponse(
      response({ result: { case: arm as never, value: { prose: { prose: plainProse("hi") } } as never } }),
      rowContext(),
    );
    expect([el.classList.contains("bubble"), el.classList.contains(OUTCOME_CARD_CLASS)]).toEqual([true, false]);
  });
});

describe("revealedSoFar", () => {
  it("starts from nothing on a row's first draw", () => {
    expect(revealedSoFar(undefined)).toBe(0);
  });

  it("starts from nothing when the previous element carried no position", () => {
    expect(revealedSoFar(document.createElement("div"))).toBe(0);
  });

  it("resumes from the position the previous draw recorded", () => {
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "4");
    expect(revealedSoFar(previous)).toBe(4);
  });

  it("starts from nothing for a value that is not a position", () => {
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "not-a-number");
    expect(revealedSoFar(previous)).toBe(0);
  });
});

describe("carriedNumber", () => {
  /** An element carrying VALUE on data-x. */
  function carrying(value: string): HTMLElement {
    const el = document.createElement("div");
    el.setAttribute("data-x", value);
    return el;
  }

  it.each([
    { name: "reads a whole number", value: "4", want: 4 },
    { name: "reads a fraction", value: "0.25", want: 0.25 },
    { name: "reads zero", value: "0", want: 0 },
    { name: "refuses a negative number", value: "-1", want: undefined },
    { name: "refuses a value that is not a number", value: "abc", want: undefined },
  ])("$name", ({ value, want }) => {
    expect(carriedNumber(carrying(value), "data-x")).toBe(want);
  });

  it("answers nothing when there is no previous draw", () => {
    expect(carriedNumber(undefined, "data-x")).toBeUndefined();
  });

  it("answers nothing when the attribute is absent", () => {
    expect(carriedNumber(document.createElement("div"), "data-x")).toBeUndefined();
  });

  it("is the one parse every carried reveal fact goes through", () => {
    expect(responseSource.match(/Number\.parse(?:Int|Float)\(/g)).toHaveLength(1);
  });
});

describe("a malformed response", () => {
  it("refuses an unset result", () => {
    expect(() => drawFeedResponse(response({}), rowContext())).toThrow(MalformedView);
  });

  it("refuses an arriving state with no prose", () => {
    const u = response({ result: { case: "update", value: {} } });
    expect(() => drawFeedResponse(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses a settled state with no prose", () => {
    const u = response({ result: { case: "success", value: {} } });
    expect(() => drawFeedResponse(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses a broken state with no prose", () => {
    const u = response({ result: { case: "error", value: {} } });
    expect(() => drawFeedResponse(u, rowContext())).toThrow(MalformedView);
  });

  it("refuses an arm this build has no case for", () => {
    const u = response({});
    (u as { result: unknown }).result = { case: "teleported", value: {} };
    expect(() => drawFeedResponse(u, rowContext())).toThrow(MalformedView);
  });
});

describe("a host with no animation frames", () => {
  it("draws the arriving prose whole rather than not at all", () => {
    // Arrange: an embedder (or a test host) that offers no frame scheduler.
    vi.stubGlobal("requestAnimationFrame", undefined);
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("hello world") } } } }),
      rowContext(),
    );
    // Assert: the whole prose is on screen with no frame ever having run,
    // where a frame host would have shown nothing yet.
    expect([
      el.querySelector(".bubble-body")?.textContent?.trim(),
      el.getAttribute(REVEALED_ATTRIBUTE),
    ]).toEqual(["hello world", renderedLength(el)]);
  });

  it("wears no arriving indicator even with no animation frames", () => {
    // Arrange
    vi.stubGlobal("requestAnimationFrame", undefined);
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("typing") } } } }),
      rowContext(),
    );
    // Assert — the whole prose is on screen and no indicator node was appended.
    expect(el.querySelector(".bubble-body")?.textContent).toContain("typing");
    expect(el.querySelector(".response-arriving")).toBeNull();
    expect(el.querySelector(".animated-ellipsis")).toBeNull();
  });
});

/**
 * The metaprompt's `!md` showcase tree, as the vendor emits it and the daemon
 * now serves it: BARE and UNWRAPPED, one physical line per branch. The webapp
 * wraps it to the bubble's live width. Branch 1.1 (with a child) and branch 1.2
 * (the last branch) each run well past any reasonable bubble width, so each
 * wraps onto a continuation line.
 */
const SHOWCASE_TREE = [
  "1 🌳 A bare Unicode tree, the shape the metaprompt answers in.",
  "├── 1.1 This branch is deliberately much longer than one hundred and five rendered columns, so the webapp must wrap it onto a continuation line beneath its own text column right here.",
  "│   └── 1.1.1 A child beneath the wrapped branch, so the rail through the wrap is load-bearing here too.",
  "└── 1.2 The last branch, also long enough to run past the limit and wrap, whose continuation carries no rail because nothing follows it once the wrap lands.",
].join("\n");

describe("the wrapped tree a settled response carries", () => {
  useTreeLayout();

  it("wraps a too-wide branch onto continuation lines with real ancestor rails", () => {
    // Arrange
    const host = document.createElement("div");
    // Act — at 105 columns.
    host.innerHTML = proseHtml(treeProse(SHOWCASE_TREE), () => 105);
    const prefixes = [...host.querySelectorAll(".mp-prefix")].map((el) => el.textContent ?? "");
    // Assert — 1.1 wrapped, and its continuation carries the ancestor rail plus
    // 1.1's own held-open child rail as REAL characters, so the wrap does not
    // sever 1.1 from the 1.1.1 beneath it.
    expect(host.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length).toBeGreaterThan(4);
    expect(prefixes.some((text) => text.startsWith("│   │"))).toBe(true);
  });

  it("keeps an interior fenced code block opaque and never markdown-bolds it", () => {
    // Arrange — the reproduction: a fenced block nested under a branch carrying
    // `__name__` (which markdown would bold), with branches after it.
    const markdown = [
      "Response (✏️ changes made)",
      "1 🔧 Subagent created `hello_world.py` at the repo root",
      "├── 1.2 File contents",
      "    ```python",
      '    if __name__ == "__main__":',
      "        main()",
      "    ```",
      "├── 1.3 Follows your Python conventions",
      "└── 1.4 Verified by running `./hello_world.py`",
    ].join("\n");
    // Act
    const host = document.createElement("div");
    host.innerHTML = proseHtml(servedProse(markdown), () => 105);
    // Assert — one tree, the code opaque (in a <pre><code>, `__name__` literal
    // and never bolded), and the last branch kept a tree line rather than
    // spilled into generic markdown after the tree. The daemon labels each
    // branch by its place, so the model's 1.4 is drawn as 1.3.
    const tree = host.querySelector(".mp-tree");
    expect(tree).not.toBeNull();
    expect(tree?.querySelector("pre code")).not.toBeNull();
    expect(host.querySelector("strong")).toBeNull();
    expect(host.textContent).toContain("__name__");
    expect(host.textContent).toContain("1.3 Verified");
  });

  it("wraps a branch the model emitted with a stray leading space, no line overflows", () => {
    // Arrange — the live bug: a `└──` branch arrives with one leading space, so
    // the wrapper must still treat it as a branch or it renders raw and spills.
    const tree = [
      "1 👋 Hello again",
      " └── 1.1 Nothing has changed since the last message, with hello/ (Go) and hello-rs/ (Rust)",
    ].join("\n");
    // Act — a width narrower than the branch text forces a wrap.
    const host = document.createElement("div");
    host.innerHTML = proseHtml(treeProse(tree), () => 60);
    // Assert — a tree rendered, and no rendered tree line exceeds the width.
    const lines = [...host.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)")];
    expect(host.querySelector(".mp-tree")).not.toBeNull();
    expect(lines.length).toBeGreaterThan(2);
    expect(lines.every((el) => visibleWidth(el.textContent ?? "") <= 60)).toBe(true);
  });

  it("re-wraps: a narrower width yields more lines than a wider one", () => {
    // Arrange
    const narrow = document.createElement("div");
    const wide = document.createElement("div");
    // Act — the same tree at two widths, the mechanism a resize drives.
    narrow.innerHTML = proseHtml(treeProse(SHOWCASE_TREE), () => 40);
    wide.innerHTML = proseHtml(treeProse(SHOWCASE_TREE), () => 200);
    // Assert
    const narrowLines = narrow.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length;
    const wideLines = wide.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length;
    expect(narrowLines).toBeGreaterThan(wideLines);
  });

  it("renders streaming and settled identically at the same measured width", () => {
    // Arrange — with no animation frames, the arriving draw paints the whole
    // prose at once, so it is comparable to the settled draw of the same text.
    vi.stubGlobal("requestAnimationFrame", undefined);
    // Act
    const settled = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    const streaming = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: treeProse(SHOWCASE_TREE, { arriving: true }) } } } }),
      rowContext(),
    );
    mount(settled);
    mount(streaming);
    // Assert — the tree element is byte-for-byte the same on both paths.
    expect(streaming.querySelector(".mp-tree")?.outerHTML).toBe(
      settled.querySelector(".mp-tree")?.outerHTML,
    );
  });

  it("links each label a later prompt answers to that prompt", () => {
    // Arrange — the daemon says the prompt p2 answers 1.1.
    const el = drawFeedResponse(
      response({
        result: { case: "success", value: { prose: { prose: treeProse(ANSWERED_TREE) } } },
        answers: [{ label: "1.1", prompt: { value: "p2" } }],
      }),
      rowContext(),
    );
    // Act
    mount(el);
    // Assert
    const linked = [...el.querySelectorAll(".mp-label-linked")].map((l) => [l.getAttribute("data-label"), l.getAttribute("data-answer-prompt")]);
    expect(linked).toEqual([["1.1", "p2"]]);
  });

  it("links nothing in a response nothing answers", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(ANSWERED_TREE) } } } }),
      rowContext(),
    );
    // Act
    mount(el);
    // Assert
    expect([el.querySelector(".mp-label"), el.querySelector(".mp-label-linked")]).toEqual([expect.anything(), null]);
  });

  it("does not pin a settled tree bubble to the cap; it shrinks to fit", () => {
    // Arrange / Act — a settled response that drew a metaprompt tree.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    mount(el);
    // Assert — the tree drew, but the bubble carries NO full-cap width pin
    // (owner ruling 2026-09-15, reversing the fill-cap): it stays fit-content
    // so it shrinks to its widest wrapped line, capped at 77%.
    expect(el.querySelector(".mp-tree")).not.toBeNull();
    expect(el.classList.contains("bubble-fill-cap")).toBe(false);
  });

  it("leaves a plain-prose bubble unpinned too, so a short answer may still shrink", () => {
    // Arrange / Act — prose with no tree.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("**done**") } } } }),
      rowContext(),
    );
    // Assert — no full-cap pin: every response bubble is fit-content capped.
    expect(el.querySelector(".mp-tree")).toBeNull();
    expect(el.classList.contains("bubble-fill-cap")).toBe(false);
  });

  it("does not pin a cut-short (error) tree bubble to the cap", () => {
    // Arrange / Act — the error arm also paints through paintWhole.
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    mount(el);
    // Assert
    expect(el.querySelector(".mp-tree")).not.toBeNull();
    expect(el.classList.contains("bubble-fill-cap")).toBe(false);
  });

  it("does not pin an arriving (update) tree bubble to the cap as it streams in", () => {
    // Arrange — no animation frames, so the arriving draw paints the whole prose
    // at once (the same trick the streaming/settled-identical test uses).
    vi.stubGlobal("requestAnimationFrame", undefined);
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: treeProse(SHOWCASE_TREE, { arriving: true }) } } } }),
      rowContext(),
    );
    mount(el);
    // Assert
    expect(el.querySelector(".mp-tree")).not.toBeNull();
    expect(el.classList.contains("bubble-fill-cap")).toBe(false);
  });

  it("resolves a tree bubble's width to fit-content, capped at 77%, never a full-cap pin", () => {
    // Arrange — the real stylesheet installed, a settled tree bubble attached so
    // getComputedStyle resolves the cascade against it.
    const teardown = installStylesheet();
    try {
      const el = drawFeedResponse(
        response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
        rowContext(),
      );
      document.body.appendChild(el);
      // Act — the winning `width` and the ceiling.
      const width = cascadedValue(el, "width");
      const maxWidth = cascadedValue(el, "max-width");
      // Assert — fit-content up to the 77% cap, never pinned at the cap width.
      expect(width).toBe("fit-content");
      expect(width).not.toBe("77%");
      expect(maxWidth).toBe("var(--bubble-max-width)");
      expect(stylesheet).toMatch(/--bubble-max-width:\s*77%;/);
      el.remove();
    } finally {
      teardown();
    }
  });

  it("subscribes a resize observer to the bubble's containing block", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    // Act
    const column = mount(el);
    // Assert — a resize reaches an observer on the column (fireResize throws
    // when nothing observes the element), and the tree survives the re-wrap.
    expect(() => fireResize(column)).not.toThrow();
    expect(el.querySelector(".mp-tree")).not.toBeNull();
  });

  it("does not observe the fit-content body, whose width never follows the column", () => {
    // Arrange — the turn's answer, drawn uncapped (an interim response is
    // capped, and its has-more measurer observes the body).
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    mount(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Act / Assert — the tree painter observes the column, never the body; and
    // an uncapped response arms no has-more measurer either, so NOTHING
    // observes the body (the stub throws on a resize nothing observes).
    expect(() => fireResize(body)).toThrow();
  });

  it("tears the containing block's observer down with the bubble", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    const column = mount(el);
    // Act — discarding the bubble.
    stopTicking(el);
    // Assert — the observer is gone.
    expect(() => fireResize(column)).toThrow();
  });
});


describe("a tree's first paint waits for the body to join the document", () => {
  const staged = useTreeLayout();

  /** Every rendered tree line's column width. */
  function lineWidths(el: HTMLElement): number[] {
    return [...el.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)")].map((line) =>
      visibleWidth(line.textContent ?? ""),
    );
  }

  it("draws nothing of a tree while the bubble is detached", () => {
    // Arrange / Act
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    // Assert — no wrong-width tree was ever drawn.
    expect(el.querySelector(".mp-tree")).toBeNull();
  });

  it("draws the tree at the cap's budget the moment it is attached", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    // Act
    mount(el);
    // Assert — it wrapped, and no line is wider than the budget.
    const widths = lineWidths(el);
    expect(widths.length).toBeGreaterThan(4);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout, "uncapped"));
  });

  it("never wraps a tree whose lines fit under the cap's budget", () => {
    // Arrange — every branch is far shorter than the 93-column budget, though
    // longer than the 60 a narrow fit-content bubble would give.
    const tree = [
      "1 🌳 A tree whose longest branch is comfortably under the cap.",
      "├── 1.1 A branch of about seventy rendered columns, well inside the cap here.",
      "└── 1.2 Another branch, also short enough to stand on one line.",
    ].join("\n");
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(tree) } } } }),
      rowContext(),
    );
    // Act
    mount(el);
    // Assert — one line per branch: nothing wrapped.
    expect(lineWidths(el)).toHaveLength(3);
  });

  it("draws an arriving tree at attach too", () => {
    // Arrange
    vi.stubGlobal("requestAnimationFrame", undefined);
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: treeProse(SHOWCASE_TREE, { arriving: true }) } } } }),
      rowContext(),
    );
    expect(el.querySelector(".mp-tree")).toBeNull();
    // Act
    mount(el);
    // Assert
    expect(Math.max(...lineWidths(el))).toBeLessThanOrEqual(stagedCols(staged.layout, "uncapped"));
  });

  it("draws plain prose at once, detached, since it needs no width", () => {
    // Arrange / Act
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("**done**") } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(".bubble-body strong")?.textContent).toBe("done");
  });

  it("re-wraps when the containing block's width moves the budget", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    const column = mount(el);
    const before = lineWidths(el).length;
    // Act — the column narrows.
    staged.layout.containingPx = 500;
    fireResize(column);
    vi.runOnlyPendingTimers();
    // Assert — more lines, none past the narrower budget.
    expect(lineWidths(el).length).toBeGreaterThan(before);
    expect(Math.max(...lineWidths(el))).toBeLessThanOrEqual(stagedCols(staged.layout, "uncapped"));
  });

  it("does not repaint when the column resizes without moving the budget", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    const column = mount(el);
    const tree = el.querySelector(".mp-tree");
    // Act — a height-only change: the observer fires, the budget is the same.
    fireResize(column);
    vi.runOnlyPendingTimers();
    // Assert — the very same tree node.
    expect(el.querySelector(".mp-tree")).toBe(tree);
  });

  it("re-measures against the new containing block when the row is moved", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    mount(el);
    const before = lineWidths(el).length;
    // Act — moved into a narrower column.
    staged.layout.containingPx = 500;
    mount(el);
    // Assert
    expect(lineWidths(el).length).toBeGreaterThan(before);
  });

  it("follows the NEW containing block after a move", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(SHOWCASE_TREE) } } } }),
      rowContext(),
    );
    const first = mount(el);
    // Act
    const second = mount(el);
    // Assert — the old column is no longer observed; the new one is.
    expect(() => fireResize(first)).toThrow();
    expect(() => fireResize(second)).not.toThrow();
  });
});


describe("the record of the drawn response", () => {
  it("records a row's FIRST draw at info, the bubble appearing being the action", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("arriving") } } } }),
      rowContext(),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.level.case).toBe("info");
  });

  it("records an intermediate re-push at debug, the daemon re-pushing per fragment", async () => {
    // Arrange — a page booted at `log_level=debug`, which is the only level
    // that admits the record this test is about.
    const capture = captureLogRecords("debug");
    const previous = document.createElement("div");
    // Act
    drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("arriving still") } } } }),
      rowContext(previous),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.level.case).toBe("debug");
  });

  it("records the SETTLED draw of an already-drawn row at info, the answer standing whole", async () => {
    // Arrange
    const capture = captureLogRecords();
    const previous = document.createElement("div");
    // Act
    drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("the answer") } } } }),
      rowContext(previous),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.level.case).toBe("info");
  });

  it("records the SETTLED draw of a cut-short row at info, a broken bubble being terminal too", async () => {
    // Arrange
    const capture = captureLogRecords();
    const previous = document.createElement("div");
    // Act
    drawFeedResponse(
      response({ result: { case: "error", value: { prose: { prose: plainProse("half an ans") } } } }),
      rowContext(previous),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.level.case).toBe("info");
  });

  it("carries the prose's character count, which is what ties a draw to a known text", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("Here is what I found.") } } } }),
      rowContext(),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.context).toMatchObject({ characters: 21 });
  });

  it("carries the prose block count, which the daemon's fold makes exactly one", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("one block") } } } }),
      rowContext(),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.context).toMatchObject({ blocks: 1 });
  });

  it("carries the row the draw is attributed to, so a reader can follow one bubble", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("x") } } } }),
      rowContext(),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.context).toMatchObject({ row: "row-1" });
  });

  it("states the arriving prose's arrived length, not the length the type-out has shown", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act — nothing is revealed on the first frame, and the record is about
    // what ARRIVED.
    drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("0123456789") } } } }),
      rowContext(),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-response");
    expect(record.context).toMatchObject({ characters: 10 });
  });

  it("records nothing for a row whose arm this build cannot read, since it drew none", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    let thrown: unknown;
    try {
      drawFeedResponse(response({}), rowContext());
    } catch (err) {
      thrown = err;
    }
    // Assert
    expect(thrown).toBeInstanceOf(MalformedView);
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.some((rec) => rec.operation === "feed.draw-response")).toBe(false);
  });
});

/**
 * The reveal reworked to RECONCILE the prose in place rather than rebuild the
 * whole subtree every animation frame (which flickered). These pin the two
 * things the rework must hold: at any revealed length the DOM is byte-identical
 * to a fresh whole render (the oracle `paintWhole` writes), and the nodes that
 * did not change keep their identity so the reader sees no teardown.
 */
describe("the incremental reveal reconciles the prose without rebuilding it", () => {
  const staged = useTreeLayout();

  /** The body's prose HTML, compared against the whole-render oracle. The
   * streaming reveal carries no trailing indicator node, so it is exactly the
   * oracle's markup. */
  function prose(body: HTMLElement): string {
    return body.querySelector(`.${RESPONSE_PROSE_CLASS}`)?.innerHTML ?? "";
  }

  it("ends the reveal byte-identical to the whole-render oracle", () => {
    // Arrange — a metaprompt tree, so the tree line-diff path is exercised too.
    const streamingEl = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: treeProse(TREE, { arriving: true }) } } } }),
      rowContext(),
    );
    document.body.appendChild(streamingEl);
    const streamingBody = streamingEl.querySelector<HTMLElement>(".bubble-body");
    if (streamingBody === null) throw new Error("no bubble body");
    // Act — drive the type-out to completion.
    vi.advanceTimersByTime(5000);
    // The settled draw of the same text is the oracle: one settled paint.
    const settled = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: treeProse(TREE) } } } }),
      rowContext(),
    );
    mount(settled);
    const settledBody = settled.querySelector<HTMLElement>(".bubble-body");
    if (settledBody === null) throw new Error("no settled body");
    // Assert — the reconciled reveal lands exactly where the one-shot render does.
    expect(prose(streamingBody)).toBe(prose(settledBody));
  });

  it("keeps a stable leading tree line's node identity while the tail grows", () => {
    // Arrange — a tree with long branches, so the reveal spends many frames
    // inside it and the leading root line settles well before the tail.
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: treeProse(SHOWCASE_TREE, { arriving: true }) }, revealPace: PACE } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Advance until the tree is on screen with a leading line, still arriving.
    let before: Element[] = [];
    for (let i = 0; i < 60 && before.length < 2; i++) {
      vi.advanceTimersByTime(16);
      const lines = [...body.querySelectorAll(".mp-tree .mp-line:not([data-unrevealed])")];
      const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
      if (lines.length >= 2 && shown < SHOWCASE_TREE.length) before = lines;
    }
    expect(before.length).toBeGreaterThanOrEqual(2);
    const firstBefore = before[0];
    // Act — advance further so the tail wraps onto more lines.
    let after: Element[] = [];
    for (let i = 0; i < 60 && after.length <= before.length; i++) {
      vi.advanceTimersByTime(16);
      after = [...body.querySelectorAll(".mp-tree .mp-line:not([data-unrevealed])")];
    }
    // Assert — the tail grew and the leading line is the SAME node, never rebuilt.
    expect(after.length).toBeGreaterThan(before.length);
    expect(after[0]).toBe(firstBefore);
  });

  it("re-wraps the arriving tree to the whole render at a new width on resize", () => {
    // Arrange — the tree types out in a 1000px column (93 columns), in the
    // turn's answer, which is uncapped (a replayed answer still arriving).
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "update", value: { prose: { prose: treeProse(SHOWCASE_TREE, { arriving: true }) }, revealPace: PACE } } }),
      rowContext(),
    );
    const column = mount(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Drive the type-out to completion so no reveal frame is left pending; the
    // resize is then the only work the timers run.
    vi.advanceTimersByTime(5000);
    const before = stagedCols(staged.layout, "uncapped");
    // Act — the column narrows: the observer re-measures and repaints.
    staged.layout.containingPx = 600;
    fireResize(column);
    vi.runOnlyPendingTimers();
    // Assert — the re-wrapped prose equals the whole render at the NEW width,
    // and is no longer the render at the width it first drew at.
    const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
    const after = stagedCols(staged.layout, "uncapped");
    expect(prose(body)).toBe(proseHtml(treeProse(SHOWCASE_TREE.slice(0, shown)), () => after));
    expect(prose(body)).not.toBe(proseHtml(treeProse(SHOWCASE_TREE.slice(0, shown)), () => before));
  });

  it("appends to a growing plain paragraph in place rather than rebuilding it", () => {
    // Arrange — plain prose (no tree), a complete first paragraph and a second
    // that keeps growing.
    const markdown =
      "First paragraph, complete and stable.\n\nSecond paragraph that keeps on growing word by word right here.";
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: servedProse(markdown, { arriving: true }) }, revealPace: PACE } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Advance until the second paragraph exists (the first is then complete),
    // still arriving.
    let secondBefore: Element | null = null;
    let lenBefore = 0;
    for (let i = 0; i < 80 && secondBefore === null; i++) {
      vi.advanceTimersByTime(16);
      const paras = body.querySelectorAll("p");
      const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
      if (paras.length >= 2 && shown < markdown.length) {
        secondBefore = paras[1];
        lenBefore = paras[1].textContent?.length ?? 0;
      }
    }
    expect(secondBefore).not.toBeNull();
    // Act — grow the second paragraph further.
    let secondAfter: Element | null = null;
    let lenAfter = 0;
    for (let i = 0; i < 80 && lenAfter <= lenBefore; i++) {
      vi.advanceTimersByTime(16);
      const paras = body.querySelectorAll("p");
      secondAfter = paras[1] ?? null;
      lenAfter = paras[1]?.textContent?.length ?? 0;
    }
    // Assert — the paragraph is the SAME node with more text: it was appended to
    // in place, not torn down and rebuilt.
    expect(secondAfter).toBe(secondBefore);
    expect(lenAfter).toBeGreaterThan(lenBefore);
  });

  it("appends no arriving ellipsis on any frame of the reveal", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("some words arriving over frames") } } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    vi.advanceTimersByTime(16);
    // Assert — no indicator on the first frame...
    expect(body.querySelector(".response-arriving")).toBeNull();
    expect(body.querySelector(".animated-ellipsis")).toBeNull();
    // Act — a few more frames of reconciliation.
    vi.advanceTimersByTime(48);
    // Assert — ...and none after further frames either.
    expect(body.querySelector(".response-arriving")).toBeNull();
    expect(body.querySelector(".animated-ellipsis")).toBeNull();
  });

  it("carries no arriving ellipsis once the response has settled", () => {
    // Arrange, Act — the settled arm renders whole through paintWhole.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("the whole answer") } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(".response-arriving")).toBeNull();
  });
});

/**
 * A COLLAPSED BUBBLE STOPS TYPING ONCE IT IS FULL (owner request, 2026-10-10):
 * once the revealed text runs past a collapsed bubble's cap, the rest of the
 * arriving prose is drawn whole rather than typed out unseen.
 */
describe("the type-out of a collapsed bubble", () => {
  const LINE_PX = 20;
  /** Enough prose that a first frame reveals only a small part of it. */
  const LONG = "x".repeat(5000);
  /** LONG's rendered length: markdown-it ends its paragraph with a newline. */
  const RENDERED_LONG = LONG.length + 1;
  /** A few animation frames of the faked clock: the slot's first paint, then the type-out's first steps. */
  const FRAME_MS = 100;

  /**
   * An arriving response (THINKING or not) holding MARKDOWN, mounted, whose
   * body renders LINES lines and whose collapsed box shows one; OPEN expands it.
   */
  function arriving(opts: {
    thinking: boolean;
    lines: number;
    open?: boolean;
    markdown?: string;
    previous?: HTMLElement;
  }): HTMLElement {
    const el = drawFeedResponse(
      response({ thinking: opts.thinking, result: { case: "update", value: { prose: { prose: plainProse(opts.markdown ?? LONG) }, revealPace: PACE } } }),
      rowContext(opts.previous),
    );
    if (opts.previous === undefined) mount(el);
    const box = el.querySelector<HTMLElement>(`:scope > .${BUBBLE_BOX_CLASS}`);
    const body = box?.querySelector<HTMLElement>(":scope > .bubble-body");
    if (box === null || box === undefined || body === null || body === undefined) throw new Error("the bubble drew no box");
    if (opts.open === true) box.classList.add(EXPANDED_CLASS);
    Object.defineProperty(box, "clientHeight", { configurable: true, value: LINE_PX });
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: opts.lines * LINE_PX });
    Object.defineProperty(body, "scrollHeight", { configurable: true, value: opts.lines * LINE_PX });
    return el;
  }

  /** The characters ELEMENT has revealed so far. */
  const revealed = (el: HTMLElement): number => Number(el.getAttribute(REVEALED_ATTRIBUTE));

  it("draws the rest whole on the first frame that runs past the collapsed cap", () => {
    // Arrange
    const el = arriving({ thinking: true, lines: 4 });
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect(revealed(el)).toBe(RENDERED_LONG);
  });

  it("keeps typing while the collapsed bubble's text still fits its cap", () => {
    // Arrange
    const el = arriving({ thinking: true, lines: 1 });
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect([revealed(el) > 0, revealed(el) < RENDERED_LONG]).toEqual([true, true]);
  });

  it("keeps typing in a bubble the reader expanded", () => {
    // Arrange
    const el = arriving({ thinking: true, lines: 4, open: true });
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect([revealed(el) > 0, revealed(el) < RENDERED_LONG]).toEqual([true, true]);
  });

  it("keeps typing in an uncapped answer however long it runs", () => {
    // Arrange
    const el = arriving({ thinking: false, lines: 4 });
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect([revealed(el) > 0, revealed(el) < RENDERED_LONG]).toEqual([true, true]);
  });

  it("draws a later push whole on its own first frame once the bubble was full", () => {
    // Arrange
    const first = arriving({ thinking: true, lines: 4 });
    vi.advanceTimersByTime(FRAME_MS);
    // Act
    const again = arriving({ thinking: true, lines: 4, markdown: LONG + "y".repeat(5000), previous: first });
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect(revealed(again)).toBe(RENDERED_LONG + 5000);
  });

  it("records drawing the rest whole with the row and how far the type-out had reached", async () => {
    // Arrange
    arriving({ thinking: true, lines: 4 });
    const capture = captureLogRecords("debug");
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    const record = await forwardedRecord(capture, "feed.cards.response.collapsed-full");
    expect([record.level.case, record.context?.row, Number(record.context?.total)]).toEqual([
      "debug",
      "row-1",
      RENDERED_LONG,
    ]);
  });
});

/**
 * AN UNSEEN PAGE DOES NOT TYPE (owner request, 2026-10-12): a draw on a page
 * no reader can see is drawn whole, and a page turning unseen mid-reveal draws
 * the rest whole at once.
 */
describe("the type-out of a page no reader can see", () => {
  /** Enough prose that a first frame reveals only a small part of it. */
  const LONG = "x".repeat(5000);
  /** LONG's rendered length: markdown-it ends its paragraph with a newline. */
  const RENDERED_LONG = LONG.length + 1;
  /** A few animation frames of the faked clock. */
  const FRAME_MS = 100;

  /** An arriving, uncapped answer holding LONG, mounted on a page SEEN says. */
  function arriving(seen: PageSeen): HTMLElement {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse(LONG) }, revealPace: PACE } } }),
      rowContext(undefined, seen),
    );
    mount(el);
    return el;
  }

  /** The characters ELEMENT has revealed so far. */
  const revealed = (el: HTMLElement): number => Number(el.getAttribute(REVEALED_ATTRIBUTE));

  it("draws the arriving prose whole on an unseen page", () => {
    // Arrange
    const el = arriving(fakePageSeen(false));
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect(revealed(el)).toBe(RENDERED_LONG);
  });

  it("draws the rest whole the moment the page turns unseen mid-reveal", () => {
    // Arrange
    const seen = fakePageSeen(true);
    const el = arriving(seen);
    vi.advanceTimersByTime(FRAME_MS);
    // Act
    seen.set(false);
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect(revealed(el)).toBe(RENDERED_LONG);
  });

  it("keeps typing on a page that stays seen", () => {
    // Arrange
    const el = arriving(fakePageSeen(true));
    // Act
    vi.advanceTimersByTime(FRAME_MS);
    // Assert
    expect([revealed(el) > 0, revealed(el) < RENDERED_LONG]).toEqual([true, true]);
  });

  it("records drawing whole on an unseen page with the row", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    arriving(fakePageSeen(false));
    // Assert
    const record = await forwardedRecord(capture, "feed.cards.response.unseen");
    expect([record.level.case, record.context?.row]).toEqual(["debug", "row-1"]);
  });

  it("records the page turning unseen with how far the type-out had reached", async () => {
    // Arrange
    const seen = fakePageSeen(true);
    arriving(seen);
    vi.advanceTimersByTime(FRAME_MS);
    const capture = captureLogRecords("debug");
    // Act
    seen.set(false);
    // Assert
    const record = await forwardedRecord(capture, "feed.cards.response.unseen");
    expect([record.context?.row, Number(record.context?.shown) > 0]).toEqual(["row-1", true]);
  });

  it("leaves a finished reveal alone when the page later turns unseen", () => {
    // Arrange: a later push owns the body, so the first loop is over.
    const seen = fakePageSeen(true);
    const first = arriving(seen);
    vi.advanceTimersByTime(FRAME_MS);
    const again = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse(LONG + "y") }, revealPace: PACE } } }),
      rowContext(first, seen),
    );
    vi.advanceTimersByTime(FRAME_MS);
    const before = revealed(again);
    seen.set(false);
    // Act: the newer loop draws whole; the older one must not redraw over it.
    // Assert
    expect([before < RENDERED_LONG + 1, revealed(again)]).toEqual([true, RENDERED_LONG + 1]);
  });
});

describe("the thinking bubble", () => {
  it("draws a thinking response as a purple assistant bubble marked thinking", () => {
    // Arrange, Act — a thinking-flagged response.
    const el = drawFeedResponse(
      response({ thinking: true, result: { case: "update", value: { prose: { prose: plainProse("weighing") }, revealPace: PACE } } }),
      rowContext(),
    );
    // Assert — the purple assistant bubble also carries the thinking marker.
    expect(el.classList.contains("assistant")).toBe(true);
    expect(el.classList.contains(THINKING_BUBBLE_CLASS)).toBe(true);
  });

  it("leaves an ordinary response bubble unmarked as thinking", () => {
    // Arrange, Act — no thinking flag.
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("an answer") }, revealPace: PACE } } }),
      rowContext(),
    );
    // Assert
    expect(el.classList.contains(THINKING_BUBBLE_CLASS)).toBe(false);
  });

  it("keeps the green final-answer rule from matching a thinking bubble", () => {
    // Arrange — a thinking bubble that has (defensively) been given the
    // final-response class.
    const el = drawFeedResponse(
      response({ thinking: true, result: { case: "success", value: { prose: { prose: plainProse("reasoning") } } } }),
      rowContext(),
    );
    el.classList.add("final-response");
    // Act, Assert — the stylesheet's green selector excludes thinking bubbles,
    // so it does not match even with the class present.
    expect(el.matches(".bubble.assistant.final-response:not(.thinking-bubble)")).toBe(false);
  });

  it("does match the green rule for a NON-thinking final response", () => {
    // Arrange — an ordinary final answer wearing the class.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("the answer") } } } }),
      rowContext(),
    );
    el.classList.add("final-response");
    // Act, Assert — the green selector matches an ordinary response.
    expect(el.matches(".bubble.assistant.final-response:not(.thinking-bubble)")).toBe(true);
  });

  it("draws multiple thinking blocks as separate bubbles", () => {
    // Arrange, Act — two thinking blocks drawn as two rows.
    const first = drawFeedResponse(
      response({ thinking: true, result: { case: "success", value: { prose: { prose: plainProse("block one") } } } }),
      rowContext(),
    );
    const second = drawFeedResponse(
      response({ thinking: true, result: { case: "success", value: { prose: { prose: plainProse("block two") } } } }),
      rowContext(),
    );
    // Assert — two distinct thinking bubbles, each with its own reasoning.
    expect(first).not.toBe(second);
    expect(first.classList.contains(THINKING_BUBBLE_CLASS)).toBe(true);
    expect(second.classList.contains(THINKING_BUBBLE_CLASS)).toBe(true);
    expect(first.querySelector(".bubble-body")?.textContent).toContain("block one");
    expect(second.querySelector(".bubble-body")?.textContent).toContain("block two");
  });

  it("logs feed.draw-thinking when a thinking bubble is drawn", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    drawFeedResponse(
      response({ thinking: true, result: { case: "update", value: { prose: { prose: plainProse("weighing") } } } }),
      rowContext(),
    );
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-thinking");
    expect(record.level.case).toBe("debug");
  });

  it("does NOT log feed.draw-thinking for an ordinary (non-thinking) response draw", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    drawFeedResponse(
      response({ result: { case: "update", value: { prose: { prose: plainProse("an answer") } } } }),
      rowContext(),
    );
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.some((rec) => rec.operation === "feed.draw-thinking")).toBe(false);
  });
});

/**
 * THE ONE-LINE CAP (owner request, 2026-10-08). A thinking bubble, arriving or
 * landed, and an interim response, arriving or settled into the pear, show
 * ONE line collapsed, wearing `has-more` when more follows it, and a click
 * expands and collapses them exactly as it does any capped bubble.
 *
 * jsdom resolves the cascade but lays nothing out, so `layOut` stands in for
 * the layout engine: the box's body is LINES tall, and while the cascade
 * hands the box its collapsed cap it shows at most `--bubble-cap-lines` of
 * them (the token resolved through `:root`, as the browser would); once the
 * cascade hands it the expanded 50vh ceiling the window is taken to hold every
 * line. Every figure it reads comes from the real stylesheet, so the cap under
 * test is the one the file declares.
 */
describe("the one-line cap", () => {
  const LINE_PX = 20;
  /** A body that runs past the one line. */
  const PAST = 3;

  /** A `var(--token)` value resolved through `:root`, or the literal itself. */
  function resolvedNumber(value: string): number {
    const token = /^var\((--[\w-]+)\)$/.exec(value);
    if (token === null) return Number(value);
    return resolvedNumber(cascadedValue(document.documentElement, token[1] ?? ""));
  }

  /** Give SCROLL a body LINES tall, clipped by whatever cap the cascade hands it. */
  function layOut(scroll: HTMLElement, lines: number): void {
    const body = scroll.querySelector(".bubble-body");
    if (body === null) throw new Error("the drawn bubble's box holds no body");
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: lines * LINE_PX });
    // The ellipsis clamps the body itself; its clipped lines are still laid out.
    Object.defineProperty(body, "scrollHeight", { configurable: true, value: lines * LINE_PX });
    Object.defineProperty(scroll, "scrollHeight", { configurable: true, value: lines * LINE_PX });
    Object.defineProperty(scroll, "clientHeight", {
      configurable: true,
      get: () => {
        if (cascadedValue(scroll, "max-height") === "50vh") return lines * LINE_PX;
        return Math.min(lines, resolvedNumber(cascadedValue(scroll, "--bubble-cap-lines"))) * LINE_PX;
      },
    });
  }

  /** A drawn bubble in a feed host armed with click-to-expand, as feed.ts arms it. */
  function mounted(u: FeedResponse, lines: number): HTMLElement {
    const host = document.createElement("div");
    installClickExpand(host, () => "", (section) => refreshHasMore(section));
    host.append(drawFeedResponse(u, rowContext()));
    document.body.append(host);
    const scroll = host.querySelector(".bubble-scroll") as HTMLElement;
    layOut(scroll, lines);
    refreshHasMore(scroll);
    return scroll;
  }

  /** The two one-line kinds, each in one of its arms. */
  const KINDS = [
    { name: "a landed thinking bubble", thinking: true, state: "success" },
    { name: "an arriving thinking bubble", thinking: true, state: "update" },
    { name: "a settled interim response", thinking: false, state: "success" },
    { name: "an arriving interim response", thinking: false, state: "update" },
  ] as const;

  /** A response of KIND carrying a word of prose. */
  function of(kind: (typeof KINDS)[number]): FeedResponse {
    return response({
      thinking: kind.thinking,
      interim: !kind.thinking,
      result: { case: kind.state, value: { prose: { prose: plainProse("weighing") } } },
    });
  }

  it.each(KINDS)("caps $name's scroll box at one line", (kind) => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(of(kind), PAST);
      // Assert
      expect(cascadedValue(scroll, "--bubble-cap-lines")).toBe("1");
    } finally {
      teardown();
    }
  });

  it.each(KINDS)("marks has-more when $name runs past its one line", (kind) => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(of(kind), PAST);
      // Assert
      expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(true);
    } finally {
      teardown();
    }
  });

  it.each(KINDS)("expands $name on a click, dropping has-more", (kind) => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const scroll = mounted(of(kind), PAST);
      const collapsed = scroll.clientHeight;
      // Act
      scroll.click();
      // Assert — grown from the one line to the full content, nothing left
      // below the fold.
      expect([
        collapsed,
        scroll.classList.contains(EXPANDED_CLASS),
        scroll.clientHeight,
        scroll.classList.contains(HAS_MORE_CLASS),
      ]).toEqual([LINE_PX, true, PAST * LINE_PX, false]);
    } finally {
      teardown();
    }
  });

  it("collapses an expanded one-line bubble on a second click, restoring has-more", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const scroll = mounted(of(KINDS[0]), PAST);
      scroll.click();
      // Act
      scroll.click();
      // Assert — back to the one line, with more below it.
      expect([
        scroll.classList.contains(EXPANDED_CLASS),
        scroll.clientHeight,
        scroll.classList.contains(HAS_MORE_CLASS),
      ]).toEqual([false, LINE_PX, true]);
    } finally {
      teardown();
    }
  });

  it("marks no has-more on a one-line bubble whose content is that line", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(of(KINDS[0]), 1);
      // Assert
      expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(false);
    } finally {
      teardown();
    }
  });

  it("gives a fitting one-line bubble nothing to expand on a click", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const scroll = mounted(of(KINDS[0]), 1);
      const collapsed = scroll.clientHeight;
      // Act
      scroll.click();
      // Assert — the same height and still no more: the click reveals nothing,
      // exactly as it does on a short held prompt.
      expect([scroll.clientHeight, scroll.classList.contains(HAS_MORE_CLASS)]).toEqual([
        collapsed,
        false,
      ]);
    } finally {
      teardown();
    }
  });
});

/**
 * A ONE-LINE BUBBLE SAYS "MORE" WITH THE ELLIPSIS, NEVER THE FADE (owner
 * request, 2026-10-08): the held prompt's own mechanism (`data-more`
 * `ellipsis`), the collapsed body clamped to its line. jsdom lays nothing
 * out, so each case states the geometry the engine would give it.
 */
describe("the one-line bubble's more signal", () => {
  const LINE_PX = 20;

  /** The ellipsis clamp on a collapsed body, as the stylesheet writes it. */
  const CLAMPED_BODY = selectorOf(/([^{}]*\.bubble-body)\s*\{[^{}]*-webkit-line-clamp/, "ellipsis clamp");

  /** The box whose fade the ellipsis hides, as the stylesheet writes it (the pseudo dropped). */
  const FADE_HIDDEN_ON = selectorOf(/([^{}]*\.bubble-scroll)::after\s*\{\s*display:\s*none;\s*\}/, "hidden fade");

  /** A drawn response (THINKING or not) in ARM whose body renders LINES lines under a one-line cap. */
  function mounted(thinking: boolean, arm: "success" | "update", lines: number): { box: HTMLElement; body: HTMLElement } {
    const host = document.createElement("div");
    installClickExpand(host, () => "", (section) => refreshHasMore(section));
    const bubble = drawFeedResponse(
      response({ thinking, interim: !thinking, result: { case: arm, value: { prose: { prose: plainProse("weighing") } } } }),
      rowContext(),
    );
    host.append(bubble);
    document.body.append(host);
    const box = bubble.querySelector(`:scope > .${BUBBLE_SCROLL_CLASS}`) as HTMLElement;
    const body = box.querySelector(":scope > .bubble-body") as HTMLElement;
    const shown = Math.min(lines, 1) * LINE_PX;
    Object.defineProperty(box, "clientHeight", { configurable: true, value: shown });
    // A clamped body's own box ends at its cap; an unclamped one holds every line.
    const own = body.matches(CLAMPED_BODY) ? shown : lines * LINE_PX;
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: own });
    Object.defineProperty(body, "scrollHeight", { configurable: true, value: lines * LINE_PX });
    refreshHasMore(box);
    return { box, body };
  }

  /** Whether the collapsed line ends in the ellipsis: the clamp holds it, and something follows it. */
  const ellipsized = ({ box, body }: { box: HTMLElement; body: HTMLElement }): boolean =>
    body.matches(CLAMPED_BODY) && box.classList.contains(HAS_MORE_CLASS);

  /** Whether the box draws the fade: it has more, and nothing hides the fade. */
  const faded = ({ box }: { box: HTMLElement }): boolean =>
    box.classList.contains(HAS_MORE_CLASS) && !box.matches(FADE_HIDDEN_ON);

  it.each([
    { name: "an arriving thinking bubble", thinking: true, arm: "update" },
    { name: "a landed thinking bubble", thinking: true, arm: "success" },
    { name: "an arriving interim response", thinking: false, arm: "update" },
    { name: "a settled interim response", thinking: false, arm: "success" },
  ] as const)("ends $name's one line in the ellipsis when more follows", ({ thinking, arm }) => {
    // Arrange / Act
    const drawn = mounted(thinking, arm, 4);
    // Assert
    expect([ellipsized(drawn), faded(drawn)]).toEqual([true, false]);
  });

  it("draws neither the ellipsis nor the fade on a one-line bubble with no more", () => {
    // Arrange / Act
    const drawn = mounted(true, "success", 1);
    // Assert
    expect([ellipsized(drawn), faded(drawn)]).toEqual([false, false]);
  });
});

/**
 * THE TURN'S ANSWER IS NEVER ABBREVIATED (owner request, 2026-09-27). The
 * response the daemon named the turn's answer (green), arriving or settled,
 * and a response the turn's death cut short are drawn `BUBBLE_UNCAPPED`: their
 * full height, no fade, no scroll box, no click-to-expand. Thinking bubbles
 * and interim responses are one line (above).
 */
describe("the response bubble at its full height", () => {
  /** A response in ARM carrying MARKDOWN, the turn's answer when FINAL. */
  function said(arm: "update" | "success" | "error", final = false, markdown = "an answer", interim = false): FeedResponse {
    return response({ finalAnswer: final, interim, result: { case: arm, value: { prose: { prose: servedProse(markdown, { arriving: arm === "update" }) } } } as never });
  }

  /** A drawn bubble in a feed host armed with click-to-expand, as feed.ts arms it. */
  function mounted(u: FeedResponse): { bubble: HTMLElement; box: HTMLElement } {
    const host = document.createElement("div");
    installClickExpand(host, () => "", (section) => refreshHasMore(section));
    const bubble = drawFeedResponse(u, rowContext());
    host.append(bubble);
    document.body.append(host);
    return { bubble, box: bubble.querySelector(`:scope > .${BUBBLE_BOX_CLASS}`) as HTMLElement };
  }

  /** Whether BUBBLE is drawn uncapped: the mode stamped, and no scroll box built. */
  function uncapped(bubble: HTMLElement): [string | null, boolean] {
    return [bubble.getAttribute(BUBBLE_CAP_ATTRIBUTE), bubble.querySelector(`.${BUBBLE_SCROLL_CLASS}`) === null];
  }

  it("draws the turn's answer (green) uncapped", () => {
    // Arrange / Act
    const { bubble } = mounted(said("success", true));
    // Assert
    expect(uncapped(bubble)).toEqual([BUBBLE_UNCAPPED, true]);
  });

  it("draws an arriving response the daemon already named the answer uncapped", () => {
    // Arrange / Act — a replayed answer re-delivered while still arriving.
    const { bubble } = mounted(said("update", true));
    // Assert
    expect(uncapped(bubble)).toEqual([BUBBLE_UNCAPPED, true]);
  });

  it("draws a response the turn's death cut short uncapped", () => {
    // Arrange / Act
    const { bubble } = mounted(said("error"));
    // Assert
    expect(uncapped(bubble)).toEqual([BUBBLE_UNCAPPED, true]);
  });

  it("gives the box no height limit from the stylesheet", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const { box } = mounted(said("success", true));
      // Assert
      expect(["", "none"]).toContain(cascadedValue(box, "max-height"));
    } finally {
      teardown();
    }
  });

  it("never wears the fade, however far its text runs", () => {
    // Arrange — a body far taller than any cap.
    const { box } = mounted(said("success", true, "a line\n\n".repeat(100)));
    const body = box.querySelector<HTMLElement>(".bubble-body") as HTMLElement;
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: 4000 });
    Object.defineProperty(box, "clientHeight", { configurable: true, value: 540 });
    // Act
    refreshHasMore(box);
    // Assert
    expect(box.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("does not expand under a click", () => {
    // Arrange
    const { box } = mounted(said("success", true));
    // Act
    box.click();
    // Assert
    expect(box.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("opens to its full height when the turn's terminal names an interim response the answer", () => {
    // Arrange — the settled interim, one line.
    const { bubble } = mounted(said("success", false, "an answer", true));
    // Act — the daemon re-stamps the row final at the turn's terminal.
    const redrawn = drawFeedResponse(said("success", true, "an answer", true), rowContext(bubble));
    // Assert — a fresh, uncapped bubble: a box never switches mode in place.
    expect([redrawn === bubble, ...uncapped(redrawn)]).toEqual([false, BUBBLE_UNCAPPED, true]);
  });
});

/**
 * A WHOLE RESPONSE TURNING INTERIM EASES INTO IT (owner request, 2026-10-13):
 * the interim bubble that replaces it animates from the whole bubble's look
 * (src/bubble/collapse-motion.ts). Only that change eases.
 */
describe("the whole-to-interim easing", () => {
  /** A settled response carrying "an answer", proven interim when INTERIM. */
  function settled(interim: boolean, final = false): FeedResponse {
    return response({ finalAnswer: final, interim, result: { case: "success", value: { prose: { prose: plainProse("an answer") } } } });
  }

  /** A drawn, connected bubble for U, standing HEIGHT px tall. */
  function standing(u: FeedResponse, height: number): HTMLElement {
    const el = drawFeedResponse(u, rowContext());
    el.getBoundingClientRect = () => ({ height }) as DOMRect;
    document.body.append(el);
    return el;
  }

  /** Redraw U over PREVIOUS in its place, let a frame run, and say whether it animated. */
  async function animatesOver(previous: HTMLElement, u: FeedResponse): Promise<boolean> {
    const animate = vi.spyOn(HTMLElement.prototype, "animate");
    const next = drawFeedResponse(u, rowContext(previous));
    if (next !== previous) previous.replaceWith(next);
    await vi.advanceTimersByTimeAsync(50);
    const animated = animate.mock.contexts.includes(next);
    animate.mockRestore();
    return animated;
  }

  it("eases a whole response into its interim look", async () => {
    // Arrange
    const whole = standing(settled(false), 180);

    // Act / Assert
    expect(await animatesOver(whole, settled(true))).toBe(true);
  });

  it("does not ease an interim response redrawn as interim", async () => {
    // Arrange
    const interim = standing(settled(true), 30);

    // Act / Assert
    expect(await animatesOver(interim, settled(true))).toBe(false);
  });

  it("does not ease a whole response redrawn whole", async () => {
    // Arrange
    const whole = standing(settled(false), 180);

    // Act / Assert
    expect(await animatesOver(whole, settled(false))).toBe(false);
  });

  it("does not ease the answer the turn's terminal names", async () => {
    // Arrange
    const whole = standing(settled(false), 180);

    // Act / Assert
    expect(await animatesOver(whole, settled(true, true))).toBe(false);
  });

  it("does not ease a first draw", async () => {
    // Arrange
    const animate = vi.spyOn(HTMLElement.prototype, "animate");

    // Act
    document.body.append(drawFeedResponse(settled(true), rowContext()));
    await vi.advanceTimersByTimeAsync(50);

    // Assert
    expect(animate).not.toHaveBeenCalled();
    animate.mockRestore();
  });
});

describe("isInterimResponse", () => {
  it.each([
    { name: "an arriving response proven interim", thinking: false, interim: true, final: false, state: "update", want: true },
    { name: "a settled response proven interim", thinking: false, interim: true, final: false, state: "success", want: true },
    { name: "an arriving response not proven interim", thinking: false, interim: false, final: false, state: "update", want: false },
    { name: "a settled response not proven interim", thinking: false, interim: false, final: false, state: "success", want: false },
    { name: "a response cut short", thinking: false, interim: true, final: false, state: "error", want: false },
    { name: "the turn's answer", thinking: false, interim: true, final: true, state: "success", want: false },
    { name: "an arriving turn's answer", thinking: false, interim: false, final: true, state: "update", want: false },
    { name: "a thinking bubble", thinking: true, interim: false, final: false, state: "success", want: false },
  ] as const)("calls $name interim: $want", ({ thinking, interim, final, state, want }) => {
    // Arrange
    const u = response({ thinking, interim, finalAnswer: final, result: { case: state, value: { prose: { prose: plainProse("x") } } } });
    // Act
    const got = isInterimResponse(u);
    // Assert
    expect(got).toBe(want);
  });
});

describe("responseCapLines", () => {
  it.each([
    { name: "a settled interim response", thinking: false, final: false, superseded: false, state: "success", want: 1 },
    { name: "an arriving interim response", thinking: false, final: false, superseded: false, state: "update", want: 1 },
    {
      name: "an arriving response not proven interim, which may be the answer",
      thinking: false,
      final: false,
      superseded: false,
      state: "update",
      want: BUBBLE_UNCAPPED,
    },
    { name: "an ordinary response cut short", thinking: false, final: false, superseded: false, state: "error", want: BUBBLE_UNCAPPED },
    { name: "the turn's answer", thinking: false, final: true, superseded: false, state: "success", want: BUBBLE_UNCAPPED },
    { name: "an arriving thinking bubble", thinking: true, final: false, superseded: false, state: "update", want: 1 },
    { name: "a thinking bubble whose text landed", thinking: true, final: false, superseded: false, state: "success", want: 1 },
    { name: "a thinking bubble cut short", thinking: true, final: false, superseded: false, state: "error", want: 1 },
    {
      name: "an arriving thinking bubble a later response superseded",
      thinking: true,
      final: false,
      superseded: true,
      state: "update",
      want: 1,
    },
  ] as const)("draws $name at cap $want", ({ name, thinking, final, superseded, state, want }) => {
    // Arrange
    const u = response({
      thinking,
      interim: !thinking && !name.includes("not proven"),
      finalAnswer: final,
      superseded,
      result: { case: state, value: { prose: { prose: plainProse("x") } } },
    });
    // Act
    const got = responseCapLines(u);
    // Assert
    expect(got).toBe(want);
  });
});

describe("responseCap", () => {
  it.each([
    { name: "a settled interim response", thinking: false, final: false, state: "success", want: BUBBLE_MORE_ELLIPSIS },
    { name: "the turn's answer", thinking: false, final: true, state: "success", want: undefined },
    { name: "an arriving thinking bubble", thinking: true, final: false, state: "update", want: BUBBLE_MORE_ELLIPSIS },
    { name: "a thinking bubble whose text landed", thinking: true, final: false, state: "success", want: BUBBLE_MORE_ELLIPSIS },
    { name: "a thinking bubble cut short", thinking: true, final: false, state: "error", want: BUBBLE_MORE_ELLIPSIS },
  ] as const)("gives $name the more signal $want", ({ thinking, final, state, want }) => {
    // Arrange
    const u = response({ thinking, interim: !thinking, finalAnswer: final, result: { case: state, value: { prose: { prose: plainProse("x") } } } });
    // Act
    const got = responseCap(u).more;
    // Assert
    expect(got).toBe(want);
  });

  it.each([
    { name: "a landed thinking bubble", thinking: true, final: false, state: "success", want: BUBBLE_MORE_ELLIPSIS },
    { name: "an arriving thinking bubble", thinking: true, final: false, state: "update", want: BUBBLE_MORE_ELLIPSIS },
    { name: "an interim response", thinking: false, final: false, state: "success", want: BUBBLE_MORE_ELLIPSIS },
    { name: "the turn's answer", thinking: false, final: true, state: "success", want: null },
  ] as const)("stamps $name's bubble data-more $want", ({ thinking, final, state, want }) => {
    // Arrange
    const u = response({ thinking, interim: !thinking, finalAnswer: final, result: { case: state, value: { prose: { prose: plainProse("x") } } } });
    // Act
    const got = drawFeedResponse(u, rowContext()).getAttribute(BUBBLE_MORE_ATTRIBUTE);
    // Assert
    expect(got).toBe(want);
  });
});

/**
 * A ONE-LINE BUBBLE IS A FIXED HEIGHT. A thinking bubble or an interim
 * response wears the one-line cap while its own text is arriving, and the
 * re-push that settles it keeps that same cap: it never changes height when
 * it lands. A reader's expansion survives the re-push.
 */
describe("the one-line bubble's fixed height", () => {
  /** A thinking bubble (or, THINKING false, an interim response) in STATE. */
  function said(state: "update" | "success" | "error", thinking = true): FeedResponse {
    return response({
      thinking,
      interim: !thinking,
      result: { case: state, value: { prose: { prose: plainProse("weighing") } } },
    });
  }

  /** A drawn bubble in a feed host armed with click-to-expand, as feed.ts arms it. */
  function mounted(u: FeedResponse): { host: HTMLElement; bubble: HTMLElement } {
    const host = document.createElement("div");
    installClickExpand(host, () => "", (section) => refreshHasMore(section));
    const bubble = drawFeedResponse(u, rowContext());
    host.append(bubble);
    document.body.append(host);
    return { host, bubble };
  }

  /** The cap the cascade hands BUBBLE's scroll box. */
  function capOf(bubble: HTMLElement): string {
    return cascadedValue(bubble.querySelector(".bubble-scroll") as HTMLElement, "--bubble-cap-lines");
  }

  it("wears the one-line cap while a thinking bubble's text is arriving", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const { bubble } = mounted(said("update"));
      // Assert
      expect(capOf(bubble)).toBe("1");
    } finally {
      teardown();
    }
  });

  it.each([
    { name: "its own settling re-push", state: "success" },
    { name: "the turn's death cutting it short", state: "error" },
  ] as const)("keeps a thinking bubble's one-line cap on $name", ({ state }) => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const { bubble } = mounted(said("update"));
      // Act — the daemon re-pushes the row in a terminal arm; it redraws in place.
      const redrawn = drawFeedResponse(said(state), rowContext(bubble));
      // Assert
      expect([redrawn === bubble, capOf(redrawn)]).toEqual([true, "1"]);
    } finally {
      teardown();
    }
  });

  it("keeps an interim response's one-line cap, in place, when it settles", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const { bubble } = mounted(said("update", false));
      // Act — the daemon re-pushes the row settled, not the turn's answer.
      const redrawn = drawFeedResponse(said("success", false), rowContext(bubble));
      // Assert
      expect([redrawn === bubble, capOf(redrawn)]).toEqual([true, "1"]);
    } finally {
      teardown();
    }
  });

  it("shows its whole final text in the same draw that settles it", () => {
    // Arrange — an arriving draw that had typed out none of its text yet.
    const teardown = installStylesheet();
    try {
      const { bubble } = mounted(said("update"));
      // Act
      const redrawn = drawFeedResponse(said("success"), rowContext(bubble));
      // Assert
      expect([redrawn.getAttribute(REVEALED_ATTRIBUTE), capOf(redrawn)]).toEqual([
        renderedLength(redrawn),
        "1",
      ]);
    } finally {
      teardown();
    }
  });

  it("draws at the one-line cap when it first arrives already settled", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act — a replayed row: its first draw is its settled one.
      const { bubble } = mounted(said("success"));
      // Assert
      expect(capOf(bubble)).toBe("1");
    } finally {
      teardown();
    }
  });

  it("stays expanded when the reader had opened it before it landed", () => {
    // Arrange — the reader clicked the arriving thinking bubble open.
    const teardown = installStylesheet();
    try {
      const { bubble } = mounted(said("update"));
      (bubble.querySelector(".bubble-scroll") as HTMLElement).click();
      // Act
      const redrawn = drawFeedResponse(said("success"), rowContext(bubble));
      // Assert — only the default limit changed; the reader's toggle stands.
      const scroll = redrawn.querySelector(".bubble-scroll") as HTMLElement;
      expect([scroll.classList.contains(EXPANDED_CLASS), cascadedValue(scroll, "max-height")]).toEqual([
        true,
        "50vh",
      ]);
    } finally {
      teardown();
    }
  });
});

describe("the data-driven final-answer green", () => {
  const FINAL_RESPONSE_CLASS = "final-response";

  it("greens a final answer on its FIRST draw", () => {
    // Arrange, Act — a settled response the daemon stamped as the turn's answer.
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: plainProse("the answer") } } } }),
      rowContext(),
    );
    // Assert — the green class rides the row's own data, on the first draw, with
    // no live turn-ended event.
    expect(el.classList.contains(FINAL_RESPONSE_CLASS)).toBe(true);
  });

  it("KEEPS the green across a redraw/re-arrange", () => {
    // Arrange — the same final-answer data, drawn once...
    const data = response({
      finalAnswer: true,
      result: { case: "success", value: { prose: { prose: plainProse("the answer") } } },
    });
    const first = drawFeedResponse(data, rowContext());
    expect(first.classList.contains(FINAL_RESPONSE_CLASS)).toBe(true);
    // Act — ...and drawn again, as a re-push / tool-group re-arrange rebuilds the
    // bubble from scratch.
    const redrawn = drawFeedResponse(data, rowContext(first));
    // Assert — the rebuilt bubble carries the green from its own data, so no
    // redraw can lose it (the root fix for the recurring disappearing border).
    expect(redrawn.classList.contains(FINAL_RESPONSE_CLASS)).toBe(true);
  });

  it("does NOT green a response that is not the answer", () => {
    // Arrange, Act — final_answer unset.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { prose: plainProse("just talking") } } } }),
      rowContext(),
    );
    // Assert.
    expect(el.classList.contains(FINAL_RESPONSE_CLASS)).toBe(false);
  });

  it("NEVER greens a thinking bubble, even when final_answer is set", () => {
    // Arrange, Act — a thinking bubble that also carries the flag (defensive: the
    // daemon never files a thinking row as an answer, and this is the second
    // guard).
    const el = drawFeedResponse(
      response({
        thinking: true,
        finalAnswer: true,
        result: { case: "success", value: { prose: { prose: plainProse("reasoning") } } },
      }),
      rowContext(),
    );
    // Assert — the green never lands on a purple thinking bubble.
    expect(el.classList.contains(FINAL_RESPONSE_CLASS)).toBe(false);
    expect(el.classList.contains(THINKING_BUBBLE_CLASS)).toBe(true);
  });

  it("keeps the green border on a selected final answer and dims its fill", () => {
    // Arrange — a green final answer that is also the selected reply target, so
    // both classes ride the same bubble.
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: plainProse("the answer") } } } }),
      rowContext(),
    );
    el.classList.add("entry-selected");
    // Act, Assert — the drawn bubble matches BOTH the green rule and the
    // selection's, and the selection's sets the fill alone, so the green border
    // stays while selected. (var()-resolved colours are not observable under
    // jsdom; selector + the declaration is the deterministic proof, mirrored
    // from styles.test.ts.)
    expect(el.matches('.bubble.final-response:not([data-variant="thinking"])')).toBe(true);
    expect(el.matches(".bubble.final-response.entry-selected")).toBe(true);
    expect(stylesheet).toMatch(/\.bubble\.final-response\.entry-selected\s*\{\s*--bubble-bg:\s*var\(--selected-final-response-bg\)\s*!important;\s*\}/);
  });
});

/**
 * A RE-PUSH UPDATES THE BUBBLE IN PLACE (owner rule, 2026-09-23: the user owns
 * the scroll). A fresh bubble per push replaced the scroll box, throwing a
 * reader scrolled inside an expanded response back to its top on every
 * fragment. jsdom lays nothing out, so the invariants are asserted on the DOM:
 * the scroll box is the same element, its `scrollTop` (a plain number under
 * jsdom) is untouched, and unchanged prose nodes keep their identity.
 */
describe("a re-push updates the bubble in place", () => {
  /** What a re-push may carry besides its prose. */
  interface Extra {
    usage?: { text: string; atMs?: bigint; heat?: { position: number } };
  }

  /** An arriving response carrying MARKDOWN. */
  function arriving(markdown: string, extra: Extra = {}) {
    return response({ ...extra, result: { case: "update", value: { prose: { prose: servedProse(markdown, { arriving: true }) } } } });
  }

  /** A settled response carrying MARKDOWN. */
  function settledAs(markdown: string, extra: Extra = {}) {
    return response({ ...extra, result: { case: "success", value: { prose: { prose: servedProse(markdown) } } } });
  }

  /** Draw FIRST, mount it, then draw NEXT over it as feed-view would. */
  function redraw(first: FeedResponse, next: FeedResponse): { before: HTMLElement; after: HTMLElement } {
    const before = drawFeedResponse(first, rowContext());
    mount(before);
    vi.advanceTimersByTime(2000);
    const after = drawFeedResponse(next, rowContext(before));
    return { before, after };
  }

  it("returns the previous bubble itself", () => {
    // Arrange + Act
    const { before, after } = redraw(arriving("hello"), arriving("hello world"));
    // Assert
    expect(after).toBe(before);
  });

  it("keeps the scroll box, so its position survives a streaming push", () => {
    // Arrange -- the reader scrolled 120px inside an expanded thinking bubble
    // (a thinking bubble is the response kind that keeps a scroll box).
    const thinkingArriving = (markdown: string) =>
      response({ thinking: true, result: { case: "update", value: { prose: { prose: servedProse(markdown, { arriving: true }) } } } });
    const before = drawFeedResponse(thinkingArriving("hello"), rowContext());
    mount(before);
    vi.advanceTimersByTime(2000);
    const box = before.querySelector<HTMLElement>(".bubble-scroll");
    if (box === null) throw new Error("the bubble drew no scroll box");
    box.scrollTop = 120;
    // Act
    const after = drawFeedResponse(thinkingArriving("hello world"), rowContext(before));
    // Assert
    expect([after.querySelector(".bubble-scroll") === box, box.scrollTop]).toEqual([true, 120]);
  });

  it("keeps the box when the response settles, so settling reflows nothing", () => {
    // Arrange + Act
    const { before, after } = redraw(arriving("hello"), settledAs("hello"));
    // Assert
    expect(after.querySelector(".bubble-box")).toBe(before.querySelector(".bubble-box"));
  });

  it("keeps an unchanged paragraph's node when the prose grows", () => {
    // Arrange
    const before = drawFeedResponse(settledAs("first paragraph"), rowContext());
    mount(before);
    const paragraph = before.querySelector(".bubble-body p");
    // Act
    drawFeedResponse(settledAs("first paragraph\n\nsecond paragraph"), rowContext(before));
    // Assert
    expect(before.querySelector(".bubble-body p")).toBe(paragraph);
  });

  it("stops the previous push's type-out painting over the update", () => {
    // Arrange -- a long arrival still typing when the row settles to less.
    const before = drawFeedResponse(arriving("x".repeat(5000)), rowContext());
    mount(before);
    vi.advanceTimersByTime(50);
    // Act
    drawFeedResponse(settledAs("done"), rowContext(before));
    vi.advanceTimersByTime(5000);
    // Assert
    expect(before.querySelector(".bubble-body")?.textContent?.trim()).toBe("done");
  });

  it("keeps the controller's blue selection mark", () => {
    // Arrange
    const before = drawFeedResponse(arriving("hello"), rowContext());
    mount(before);
    before.classList.add("entry-selected");
    // Act
    drawFeedResponse(arriving("hello world"), rowContext(before));
    // Assert
    expect(before.classList.contains("entry-selected")).toBe(true);
  });

  it("draws one cut-short marker however often the broken state is pushed", () => {
    // Arrange
    const broken = response({ result: { case: "error", value: { prose: { prose: plainProse("half") } } } });
    // Act
    const { after } = redraw(broken, broken);
    // Assert
    expect(after.querySelectorAll(".response-cut-short-marker")).toHaveLength(1);
  });

  it("leaves the bubble untouched when a re-push is malformed", () => {
    // Arrange
    const before = drawFeedResponse(settledAs("hello"), rowContext());
    mount(before);
    const malformed = response({ result: { case: "success", value: {} } });
    // Act
    let refused: unknown = null;
    try {
      drawFeedResponse(malformed, rowContext(before));
    } catch (err) {
      refused = err;
    }
    // Assert -- refused, and the bubble on screen is the one drawn before.
    expect([refused instanceof MalformedView, before.querySelector(".bubble-body")?.textContent?.trim()]).toEqual([
      true,
      "hello",
    ]);
  });

  it("draws a fresh bubble when the previous body is not a response bubble", () => {
    // Arrange -- a row whose arm changed hands over some other card's element.
    const previous = document.createElement("div");
    // Act
    const after = drawFeedResponse(settledAs("hi"), rowContext(previous));
    // Assert
    expect(after).not.toBe(previous);
  });
});

describe("the interim response's hook class", () => {
  it.each([
    { name: "an arriving response proven interim", interim: true, final: false, state: "update", want: true },
    { name: "a settled response proven interim", interim: true, final: false, state: "success", want: true },
    { name: "an arriving response not proven interim", interim: false, final: false, state: "update", want: false },
    { name: "the turn's answer", interim: true, final: true, state: "success", want: false },
    { name: "a response cut short", interim: true, final: false, state: "error", want: false },
  ] as const)("marks $name interim: $want", ({ interim, final, state, want }) => {
    // Arrange
    const u = response({ interim, finalAnswer: final, result: { case: state, value: { prose: { prose: plainProse("x") } } } });
    // Act
    const el = drawFeedResponse(u, rowContext());
    // Assert
    expect(el.classList.contains(INTERIM_RESPONSE_CLASS)).toBe(want);
  });

  it("takes the class back when the terminal names the interim the answer", () => {
    // Arrange
    const before = drawFeedResponse(
      response({ interim: true, result: { case: "success", value: { prose: { prose: plainProse("x") } } } }),
      rowContext(),
    );
    // Act
    const after = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { prose: plainProse("x") } } } }),
      rowContext(before),
    );
    // Assert
    expect(after.classList.contains(INTERIM_RESPONSE_CLASS)).toBe(false);
  });
});
