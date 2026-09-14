// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
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
import type { RowContext } from "../../../src/feed/cards/context.js";
import {
  REVEALED_ATTRIBUTE,
  USAGE_REVEALED_CLASS,
  drawFeedResponse,
  proseHtml,
  revealedSoFar,
} from "../../../src/feed/cards/response.js";
import { TICKING_ATTRIBUTE, stopTicking } from "../../../src/feed/ticking.js";
import { fireResize } from "../../resize-observer.js";
import stylesheet from "../../../src/styles.css?raw";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** A row context; the response card calls no verb of its own. */
function rowContext(previous?: HTMLElement): RowContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {});
  });
  return {
    ctx: testAppContext({
      client: createAgentReplClient(transport),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      composerEnabled: false,
    }),
    feed: "root",
    row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }) }),
    revealRow: async () => true,
    previous,
  };
}

function response(init: MessageInitShape<typeof FeedResponseSchema>): FeedResponse {
  return create(FeedResponseSchema, init);
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

describe("the settled state", () => {
  it("renders the whole markdown", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: "**done**" } } } }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body strong")?.textContent).toBe("done");
  });

  it("carries the state arm", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: "hi" } } } }),
      rowContext(),
    );
    expect(el.getAttribute("data-state")).toBe("success");
  });

  it("records the whole prose as shown, so a later push does not re-type it", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: "hello" } } } }),
      rowContext(),
    );
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe("5");
  });

  it("re-renders a metaprompt tree as tree lines rather than markdown", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: TREE } } } }),
      rowContext(),
    );
    expect(el.querySelector(".mp-tree")).not.toBeNull();
  });
});

describe("the broken state", () => {
  it("keeps the prose that landed", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { markdown: "half a thou" } } } }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body")?.textContent).toContain("half a thou");
  });

  it("marks the bubble as cut short", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { markdown: "half" } } } }),
      rowContext(),
    );
    expect(el.classList.contains("response-cut-short")).toBe(true);
  });

  it("draws the marker without a reason, which is the turn's to state", () => {
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { markdown: "half" } } } }),
      rowContext(),
    );
    expect(el.querySelector(".response-cut-short-marker")?.textContent).toBe("cut short");
  });
});

describe("the arriving state", () => {
  it("draws the arriving indicator", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "typing" } } } }),
      rowContext(),
    );
    expect(el.querySelector(".response-arriving")).not.toBeNull();
  });

  it("shows nothing of a first-seen response before the first frame", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "hello world" } } } }),
      rowContext(),
    );
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe("0");
  });

  it("types the prose out as time passes", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "hello world" } } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    vi.advanceTimersByTime(1000);
    expect(Number(el.getAttribute(REVEALED_ATTRIBUTE))).toBe("hello world".length);
  });

  it("resumes from the previous draw's position rather than restarting", () => {
    // Arrange: the element the previous push of this same row returned.
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "5");
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "hello world" } } } }),
      rowContext(previous),
    );
    // Assert: the first paint already carries what was on screen.
    expect(el.querySelector(".bubble-body")?.textContent).toContain("hello");
  });

  it("stops animating once the bubble has been replaced", () => {
    // Arrange: prose long enough that the reveal is still behind when the
    // bubble is taken out of the document.
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "x".repeat(5000) } } } }),
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

describe("the usage stamp", () => {
  const STATES = ["update", "success", "error"] as const;

  it.each(STATES)("draws the stamp verbatim in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({
        usage: { text: "2.1k" },
        result: { case: arm, value: { prose: { markdown: "hi" } } },
      }),
      rowContext(),
    );
    expect(el.querySelector(".turn-meta .usage-stamp")?.textContent).toBe("2.1k");
  });

  it("draws no stamp at all when no usage has been observed", () => {
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: "hi" } } } }),
      rowContext(),
    );
    expect(el.querySelector(".usage-stamp")).toBeNull();
  });
});

describe("the usage corner's hover timestamp", () => {
  /** A settled response whose corner carries a token figure and a settle instant. */
  function settled(atMs: bigint) {
    return response({
      usage: { text: "2.1k", atMs },
      result: { case: "success", value: { prose: { markdown: "done" } } },
    });
  }

  it("holds the figure and its timestamp side by side once settled", () => {
    // Arrange, Act
    const el = drawFeedResponse(settled(1_000n), rowContext());
    // Assert: both elements sit in the one corner.
    const corner = el.querySelector(".turn-meta .usage-corner");
    expect([
      corner?.querySelector(".usage-stamp")?.textContent,
      corner?.querySelector(".usage-ago") !== null,
    ]).toEqual(["2.1k", true]);
  });

  it("reads the timestamp from at_ms, relative and human-readable", () => {
    // Arrange: the settle was five and a half minutes ago.
    vi.setSystemTime(331_000);
    // Act
    const el = drawFeedResponse(settled(1_000n), rowContext());
    // Assert
    expect(el.querySelector(".usage-ago")?.textContent).toBe("5m 30s ago");
  });

  it("advances the timestamp on a later tick", async () => {
    // Arrange
    vi.setSystemTime(331_000);
    const el = drawFeedResponse(settled(1_000n), rowContext());
    document.body.appendChild(el);
    // Act: half a minute later, the shared clock has ticked.
    await vi.advanceTimersByTimeAsync(30_000);
    // Assert
    expect(el.querySelector(".usage-ago")?.textContent).toBe("6m ago");
  });

  it("reveals the timestamp when the corner is hovered", () => {
    // Arrange
    const el = drawFeedResponse(settled(1_000n), rowContext());
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    // Act
    corner.dispatchEvent(new Event("mouseenter"));
    // Assert
    expect(corner.classList.contains(USAGE_REVEALED_CLASS)).toBe(true);
  });

  it("hides the timestamp again when the pointer leaves", () => {
    // Arrange: a corner already revealed.
    const el = drawFeedResponse(settled(1_000n), rowContext());
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    corner.dispatchEvent(new Event("mouseenter"));
    // Act
    corner.dispatchEvent(new Event("mouseleave"));
    // Assert
    expect(corner.classList.contains(USAGE_REVEALED_CLASS)).toBe(false);
  });

  it("reveals the timestamp on keyboard focus too", () => {
    // Arrange
    const el = drawFeedResponse(settled(1_000n), rowContext());
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    // Act
    corner.dispatchEvent(new Event("focusin"));
    // Assert: focusable, and the focus reveals the same timestamp a hover does.
    expect([corner.tabIndex, corner.classList.contains(USAGE_REVEALED_CLASS)]).toEqual([0, true]);
  });

  it("draws no timestamp while the response is still arriving", () => {
    // Arrange, Act: usage stated at open, but no settle instant yet.
    const el = drawFeedResponse(
      response({
        usage: { text: "2.1k", atMs: 0n },
        result: { case: "update", value: { prose: { markdown: "typing" } } },
      }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(".usage-ago")).toBeNull();
  });

  it("stops the timestamp's clock when the bubble is disposed", () => {
    // Arrange: a settled corner whose clock is live.
    const el = drawFeedResponse(settled(1_000n), rowContext());
    expect(el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)).toHaveLength(1);
    // Act: whoever discards the bubble stops its clocks.
    stopTicking(el);
    // Assert
    expect(el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)).toHaveLength(0);
  });

  it("takes no clock at all for an arriving corner, since it draws no timestamp", () => {
    // Arrange, Act
    const el = drawFeedResponse(
      response({
        usage: { text: "2.1k", atMs: 0n },
        result: { case: "update", value: { prose: { markdown: "typing" } } },
      }),
      rowContext(),
    );
    // Assert
    expect(el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)).toHaveLength(0);
  });

  it("slides the timestamp over one continuous half-second transition", () => {
    // Arrange / Act: the rule the .usage-ago element is styled by.
    const rule = /\.usage-ago\s*\{([^}]*)\}/.exec(stylesheet)?.[1] ?? "";
    // Assert: a 0.5s transition is what gives the reveal AND the reverse.
    expect(rule).toMatch(/transition:[^;]*0\.5s/);
  });

  it("shows and hides the timestamp with no slide under reduced motion", () => {
    // Arrange: the reduced-motion block.
    const at = stylesheet.indexOf("@media (prefers-reduced-motion: reduce)");
    const block = stylesheet.slice(at);
    // Assert: the timestamp's transition is dropped there.
    expect(block).toMatch(/\.usage-ago\s*\{[^}]*transition:\s*none/);
  });
});

describe("the notice register", () => {
  /** Every state arm: the notice is a FIELD, so it rides all of them. */
  const STATES = (
    FeedResponseSchema.oneofs.find((oneof) => oneof.name === "result")?.fields ?? []
  ).map((field) => field.localName);

  it("covers every state arm the schema declares", () => {
    expect([...STATES].sort()).toEqual(["error", "success", "update"].sort());
  });

  it.each(STATES)("marks the bubble as a notice in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "the turn was interrupted" },
        result: { case: arm as never, value: { prose: { markdown: "hi" } } as never },
      }),
      rowContext(),
    );
    expect(el.hasAttribute("data-notice")).toBe(true);
  });

  it.each(STATES)("draws the daemon's heading verbatim in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "the turn was interrupted" },
        result: { case: arm as never, value: { prose: { markdown: "hi" } } as never },
      }),
      rowContext(),
    );
    expect(el.querySelector(".response-notice-heading")?.textContent).toBe(
      "the turn was interrupted",
    );
  });

  it.each(STATES)("draws the heading ABOVE the prose in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "a system remark" },
        result: { case: arm as never, value: { prose: { markdown: "hi" } } as never },
      }),
      rowContext(),
    );
    // The body is no longer the bubble's own child: it hangs in the shared
    // scroll box (owner ruling, 2026-09-14), so the prose's position among the
    // bubble's children IS that box's position.
    const children = [...el.children];
    const heading = children.findIndex((c) => c.classList.contains("response-notice-heading"));
    const body = children.findIndex((c) => c.classList.contains("bubble-scroll"));
    expect(heading).toBeLessThan(body);
  });

  it.each(STATES)("keeps the heading outside the body the prose rewrites (%s)", (arm) => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "a system remark" },
        result: { case: arm as never, value: { prose: { markdown: "hi" } } as never },
      }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body .response-notice-heading")).toBeNull();
  });

  it.each(STATES)("keeps the notice register's own class in the %s state", (arm) => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "a system remark" },
        result: { case: arm as never, value: { prose: { markdown: "hi" } } as never },
      }),
      rowContext(),
    );
    expect(el.classList.contains("response-notice")).toBe(true);
  });

  // The arriving state is excluded here on purpose: its prose is PACED, so at
  // draw time the body holds only the arriving indicator (the state's own suite
  // covers the type-out). The next test asserts the notice leaves that pacing
  // alone.
  it.each(["success", "error"])("draws the prose itself in the %s state, notice or not", (arm) => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "a system remark" },
        result: { case: arm as never, value: { prose: { markdown: "the words" } } as never },
      }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body")?.textContent).toContain("the words");
  });

  it("leaves the arriving state's pacing alone under a notice", () => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "a system remark" },
        result: { case: "update", value: { prose: { markdown: "the words" } } },
      }),
      rowContext(),
    );
    expect(el.querySelector(".bubble-body .response-arriving")).not.toBeNull();
  });

  it.each(STATES)("marks no notice on an ordinary %s response", (arm) => {
    const el = drawFeedResponse(
      response({ result: { case: arm as never, value: { prose: { markdown: "hi" } } as never } }),
      rowContext(),
    );
    expect(el.hasAttribute("data-notice")).toBe(false);
  });

  it.each(STATES)("draws no heading on an ordinary %s response", (arm) => {
    const el = drawFeedResponse(
      response({ result: { case: arm as never, value: { prose: { markdown: "hi" } } as never } }),
      rowContext(),
    );
    expect(el.querySelector(".response-notice-heading")).toBeNull();
  });

  it("keeps the state's own marker beside the notice", () => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "cut short by a stop" },
        result: { case: "error", value: { prose: { markdown: "hi" } } },
      }),
      rowContext(),
    );
    expect(el.classList.contains("response-cut-short")).toBe(true);
  });

  it("draws the usage stamp beside a notice heading", () => {
    const el = drawFeedResponse(
      response({
        notice: { heading: "a system remark" },
        usage: { text: "2.1k" },
        result: { case: "success", value: { prose: { markdown: "hi" } } },
      }),
      rowContext(),
    );
    expect(el.querySelector(".turn-meta .usage-stamp")?.textContent).toBe("2.1k");
  });
});

describe("revealedSoFar", () => {
  it("starts from nothing on a row's first draw", () => {
    expect(revealedSoFar(undefined, 10)).toBe(0);
  });

  it("starts from nothing when the previous element carried no position", () => {
    expect(revealedSoFar(document.createElement("div"), 10)).toBe(0);
  });

  it("clamps a position past the prose that has arrived", () => {
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "99");
    expect(revealedSoFar(previous, 10)).toBe(10);
  });

  it("resumes from a position within the prose", () => {
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "4");
    expect(revealedSoFar(previous, 10)).toBe(4);
  });

  it("starts from nothing for a value that is not a position", () => {
    const previous = document.createElement("div");
    previous.setAttribute(REVEALED_ATTRIBUTE, "not-a-number");
    expect(revealedSoFar(previous, 10)).toBe(0);
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
      response({ result: { case: "update", value: { prose: { markdown: "hello world" } } } }),
      rowContext(),
    );
    // Assert: the whole prose is on screen with no frame ever having run,
    // where a frame host would have shown nothing yet.
    expect([
      el.querySelector(".bubble-body")?.textContent?.trim(),
      el.getAttribute(REVEALED_ATTRIBUTE),
    ]).toEqual(["hello world", String("hello world".length)]);
  });

  it("still wears the arriving indicator, since the prose has not settled", () => {
    // Arrange
    vi.stubGlobal("requestAnimationFrame", undefined);
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "typing" } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(".bubble-body .response-arriving")).not.toBeNull();
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
  it("wraps a too-wide branch onto continuation lines with real ancestor rails", () => {
    // Arrange
    const host = document.createElement("div");
    // Act — width 105, the fallback the bubble measures to under jsdom.
    host.innerHTML = proseHtml(SHOWCASE_TREE, 105);
    const prefixes = [...host.querySelectorAll(".mp-prefix")].map((el) => el.textContent ?? "");
    // Assert — 1.1 wrapped, and its continuation carries the ancestor rail plus
    // 1.1's own held-open child rail as REAL characters, so the wrap does not
    // sever 1.1 from the 1.1.1 beneath it.
    expect(host.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length).toBeGreaterThan(4);
    expect(prefixes.some((text) => text.startsWith("│   │"))).toBe(true);
  });

  it("re-wraps: a narrower width yields more lines than a wider one", () => {
    // Arrange
    const narrow = document.createElement("div");
    const wide = document.createElement("div");
    // Act — the same tree at two widths, the mechanism a resize drives.
    narrow.innerHTML = proseHtml(SHOWCASE_TREE, 40);
    wide.innerHTML = proseHtml(SHOWCASE_TREE, 200);
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
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    const streaming = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    // Assert — the tree element is byte-for-byte the same on both paths.
    expect(streaming.querySelector(".mp-tree")?.outerHTML).toBe(
      settled.querySelector(".mp-tree")?.outerHTML,
    );
  });

  it("subscribes a resize observer to the bubble body and tears it down with it", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Act + Assert — a resize reaches an attached observer (fireResize throws
    // when nothing observes the element), and the tree survives the re-wrap.
    expect(() => fireResize(body)).not.toThrow();
    expect(el.querySelector(".mp-tree")).not.toBeNull();
    // And discarding the bubble disconnects the observer.
    stopTicking(el);
    expect(() => fireResize(body)).toThrow();
  });
});

describe("the record of the drawn response", () => {
  it("records a row's FIRST draw at info, the bubble appearing being the action", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "arriving" } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: "arriving still" } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "the answer" } } } }),
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
      response({ result: { case: "error", value: { prose: { markdown: "half an ans" } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "Here is what I found." } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "one block" } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "x" } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: "0123456789" } } } }),
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
