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
  RESPONSE_PROSE_CLASS,
  THINKING_BUBBLE_CLASS,
  USAGE_REVEALED_CLASS,
  drawFeedResponse,
  revealedSoFar,
} from "../../../src/feed/cards/response.js";
import { proseHtml } from "../../../src/bubble/body.js";
import { visibleWidth } from "../../../src/metaprompt-tree.js";
import { installTreeLayout, stagedCols, useTreeLayout } from "../../tree-layout.js";
import { TICKING_ATTRIBUTE, stopTicking } from "../../../src/feed/ticking.js";
import { fireResize } from "../../resize-observer.js";
import stylesheet from "../../../src/styles.css?raw";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import { HAS_MORE_CLASS, refreshHasMore } from "../../../src/feed/bubble-more.js";
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
    const { uninstall } = installTreeLayout();
    try {
      const el = drawFeedResponse(
        response({ result: { case: "success", value: { prose: { markdown: TREE } } } }),
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
  it("draws no arriving indicator: a streaming response shows its prose only", () => {
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "typing" } } } }),
      rowContext(),
    );
    expect(el.querySelector(".response-arriving")).toBeNull();
    expect(el.querySelector(".animated-ellipsis")).toBeNull();
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
    expect(el.querySelector(".bubble-scroll .usage-stamp")?.textContent).toBe("2.1k");
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
    const corner = el.querySelector(".bubble-scroll .usage-corner");
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
    // Arrange: a settled corner whose clock is live. The clock rides `.usage-ago`;
    // the `data-ticking` attribute is a SHARED teardown channel (a bubble's
    // "more below" ResizeObserver registers on it too, see bubble-more.ts), so the
    // clock is checked on its own element rather than by a blanket attribute count.
    const el = drawFeedResponse(settled(1_000n), rowContext());
    expect(el.querySelector(".usage-ago")?.hasAttribute(TICKING_ATTRIBUTE)).toBe(true);
    // Act: whoever discards the bubble stops its clocks (and every other hook).
    stopTicking(el);
    // Assert: nothing ticks any more.
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
    // Assert: no timestamp element exists, so no clock subscription was taken.
    // (A blanket `data-ticking` count would now also see the bubble's "more below"
    // ResizeObserver hook — a teardown hook, not a clock — so the check is
    // clock-specific.)
    expect(el.querySelectorAll(`.usage-ago[${TICKING_ATTRIBUTE}]`)).toHaveLength(0);
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

describe("the usage corner's slider markup", () => {
  /** A response whose corner carries the token figure and the given settle instant. */
  function withUsage(atMs: bigint) {
    return response({
      usage: { text: "12.4k", atMs },
      result: { case: "success", value: { prose: { markdown: "done" } } },
    });
  }

  /** The corner's structure as class names, children in document order. */
  function shape(corner: Element): unknown {
    return Array.from(corner.children).map((child) => ({
      className: child.className,
      children: Array.from(child.children).map((grandchild) => grandchild.className),
    }));
  }

  it("carries the token text on the corner, for the width-reserving spacer", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    // Assert
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    expect(corner.dataset.tokens).toBe("12.4k");
  });

  it("carries the token text on an arriving corner too", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(0n), rowContext());
    // Assert
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    expect(corner.dataset.tokens).toBe("12.4k");
  });

  it("nests the token then the duration inside the one slider once settled", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    // Assert — the slider is the corner's only element, and the duration is
    // the slider's only in-flow content besides the out-of-flow token.
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    expect(shape(corner)).toEqual([
      { className: "usage-slider", children: ["usage-stamp", "usage-ago"] },
    ]);
  });

  it("nests only the token inside an empty-width slider while arriving", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(0n), rowContext());
    // Assert — no duration, so the slider has no in-flow content and the
    // token stays at the right edge.
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    expect(shape(corner)).toEqual([{ className: "usage-slider", children: ["usage-stamp"] }]);
  });

  it("marks an arriving corner, so its empty slot never slides out", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(0n), rowContext());
    // Assert
    expect(el.querySelector(".usage-corner")?.hasAttribute("data-arriving")).toBe(true);
  });

  it("does not mark a settled corner as arriving", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    // Assert
    expect(el.querySelector(".usage-corner")?.hasAttribute("data-arriving")).toBe(false);
  });

  it("reserves the same slot width arriving and settled, so the settle moves nothing", () => {
    // Arrange -- the real stylesheet.
    const teardown = installStylesheet();
    const arrivingEl = drawFeedResponse(withUsage(0n), rowContext());
    const settledEl = drawFeedResponse(withUsage(1_000n), rowContext());
    document.body.append(arrivingEl, settledEl);
    try {
      // Act
      const widths = [arrivingEl, settledEl].map((el) =>
        cascadedValue(el.querySelector(".usage-slider") as HTMLElement, "width"),
      );
      // Assert
      expect([widths[0] === widths[1], widths[0] !== "" && widths[0] !== "auto"]).toEqual([true, true]);
    } finally {
      teardown();
    }
  });

  it("keeps the spacer's text and the visible token identical", () => {
    // Arrange / Act
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    // Assert — the reservation is the token's own text, so its width is the token's.
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    expect(corner.dataset.tokens).toBe(corner.querySelector(".usage-stamp")?.textContent);
  });

  it("keeps the markup unchanged as the live clock advances", async () => {
    // Arrange
    vi.setSystemTime(31_000);
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    document.body.appendChild(el);
    const corner = el.querySelector(".usage-corner") as HTMLElement;
    const before = shape(corner);
    try {
      // Act — the duration text grows ("30s ago" to "1m ago").
      await vi.advanceTimersByTimeAsync(30_000);
      // Assert
      expect([corner.querySelector(".usage-ago")?.textContent, shape(corner)]).toEqual([
        "1m ago",
        before,
      ]);
    } finally {
      el.remove();
    }
  });

  it("collapses the slider by 100% of its own width", () => {
    // Arrange
    const teardown = installStylesheet();
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    document.body.appendChild(el);
    try {
      // Act
      const slider = el.querySelector(".usage-slider") as HTMLElement;
      // Assert
      expect(cascadedValue(slider, "transform")).toBe("translateX(100%)");
    } finally {
      el.remove();
      teardown();
    }
  });

  it("slides the slider to rest when the corner is revealed", () => {
    // Arrange
    const teardown = installStylesheet();
    const el = drawFeedResponse(withUsage(1_000n), rowContext());
    document.body.appendChild(el);
    try {
      const corner = el.querySelector(".usage-corner") as HTMLElement;
      // Act
      corner.dispatchEvent(new Event("mouseenter"));
      // Assert
      const slider = el.querySelector(".usage-slider") as HTMLElement;
      expect(cascadedValue(slider, "transform")).toBe("translateX(0)");
    } finally {
      el.remove();
      teardown();
    }
  });

  it("anchors the token to the slider's left edge", () => {
    // Arrange
    const teardown = installStylesheet();
    const el = drawFeedResponse(withUsage(0n), rowContext());
    document.body.appendChild(el);
    try {
      // Act
      const stamp = el.querySelector(".usage-stamp") as HTMLElement;
      // Assert
      expect([cascadedValue(stamp, "position"), cascadedValue(stamp, "right")]).toEqual([
        "absolute",
        "100%",
      ]);
    } finally {
      el.remove();
      teardown();
    }
  });
});

describe("the usage corner's first-line float", () => {
  /** A settled response whose corner carries a token figure and a settle instant. */
  function settled(atMs: bigint) {
    return response({
      usage: { text: "2.1k", atMs },
      result: { case: "success", value: { prose: { markdown: "done" } } },
    });
  }

  it("puts the corner inside the scroll box, before the body, so the first line wraps beside it", () => {
    // Arrange / Act
    const el = drawFeedResponse(settled(1_000n), rowContext());
    // Assert — the corner is the scroll box's FIRST child and the body follows
    // it, the DOM order a `float: right` needs to shorten the body's first line.
    const scroll = el.querySelector(".bubble-scroll") as HTMLElement;
    const corner = scroll.querySelector(".usage-corner") as HTMLElement;
    const body = scroll.querySelector(".bubble-body") as HTMLElement;
    expect([scroll.firstElementChild === corner, corner.nextElementSibling === body]).toEqual([
      true,
      true,
    ]);
  });

  it("floats the corner to the right of the prose", () => {
    // Arrange
    const teardown = installStylesheet();
    const el = drawFeedResponse(settled(1_000n), rowContext());
    document.body.appendChild(el);
    try {
      // Act
      const corner = el.querySelector(".usage-corner") as HTMLElement;
      // Assert — the cascade lands `float: right` on the corner.
      expect(cascadedValue(corner, "float")).toBe("right");
    } finally {
      el.remove();
      teardown();
    }
  });

  it("keeps the duration slot the same layout width whether or not it is revealed", () => {
    // Arrange — the real stylesheet, and a settled corner whose duration exists.
    const teardown = installStylesheet();
    const el = drawFeedResponse(settled(1_000n), rowContext());
    document.body.appendChild(el);
    try {
      const corner = el.querySelector(".usage-corner") as HTMLElement;
      const ago = el.querySelector(".usage-ago") as HTMLElement;
      // Act — read every width-affecting property collapsed, then revealed.
      const widthProps = ["margin-left", "max-width", "width"] as const;
      const collapsed = widthProps.map((p) => cascadedValue(ago, p));
      corner.classList.add(USAGE_REVEALED_CLASS);
      const revealed = widthProps.map((p) => cascadedValue(ago, p));
      // Assert — nothing that sizes the slot changed, so exposing the duration
      // cannot reflow the first prose line that wrapped around the float.
      expect(revealed).toEqual(collapsed);
    } finally {
      el.remove();
      teardown();
    }
  });

  it("adds no left indent to the body, so lines below the corner run full width", () => {
    // Arrange — the corner is a float sibling, never a wrapper the body is
    // inset behind, so subsequent lines are not indented by it.
    const teardown = installStylesheet();
    const el = drawFeedResponse(settled(1_000n), rowContext());
    document.body.appendChild(el);
    try {
      // Act
      const body = el.querySelector(".bubble-body") as HTMLElement;
      // Assert — no left padding and no text-indent reserve corner space; the
      // float alone shortens only the lines it overlaps (its single row).
      expect([cascadedValue(body, "padding-left"), cascadedValue(body, "text-indent")]).toEqual([
        "0",
        "0",
      ]);
    } finally {
      el.remove();
      teardown();
    }
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
  // draw time the body is empty (nothing revealed yet; the state's own suite
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
    // The notice is drawn, and the arriving prose is still PACED (nothing
    // revealed yet at draw time) — the notice does not disturb the type-out.
    expect(el.classList.contains("response-notice")).toBe(true);
    expect(el.getAttribute(REVEALED_ATTRIBUTE)).toBe("0");
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
    expect(el.querySelector(".bubble-scroll .usage-stamp")?.textContent).toBe("2.1k");
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

  it("wears no arriving indicator even with no animation frames", () => {
    // Arrange
    vi.stubGlobal("requestAnimationFrame", undefined);
    // Act
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "typing" } } } }),
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
  const staged = useTreeLayout();

  it("wraps a too-wide branch onto continuation lines with real ancestor rails", () => {
    // Arrange
    const host = document.createElement("div");
    // Act — at 105 columns.
    host.innerHTML = proseHtml(SHOWCASE_TREE, () => 105);
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
    host.innerHTML = proseHtml(markdown, () => 105);
    // Assert — one tree, the code opaque (in a <pre><code>, `__name__` literal
    // and never bolded), and 1.4 kept a tree line rather than spilled into
    // generic markdown after the tree.
    const tree = host.querySelector(".mp-tree");
    expect(tree).not.toBeNull();
    expect(tree?.querySelector("pre code")).not.toBeNull();
    expect(host.querySelector("strong")).toBeNull();
    expect(host.textContent).toContain("__name__");
    expect(host.textContent).toContain("1.4 Verified");
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
    host.innerHTML = proseHtml(tree, () => 60);
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
    narrow.innerHTML = proseHtml(SHOWCASE_TREE, () => 40);
    wide.innerHTML = proseHtml(SHOWCASE_TREE, () => 200);
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
    mount(settled);
    mount(streaming);
    // Assert — the tree element is byte-for-byte the same on both paths.
    expect(streaming.querySelector(".mp-tree")?.outerHTML).toBe(
      settled.querySelector(".mp-tree")?.outerHTML,
    );
  });

  it("does not pin a settled tree bubble to the cap; it shrinks to fit", () => {
    // Arrange / Act — a settled response that drew a metaprompt tree.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "**done**" } } } }),
      rowContext(),
    );
    // Assert — no full-cap pin: every response bubble is fit-content capped.
    expect(el.querySelector(".mp-tree")).toBeNull();
    expect(el.classList.contains("bubble-fill-cap")).toBe(false);
  });

  it("does not pin a cut-short (error) tree bubble to the cap", () => {
    // Arrange / Act — the error arm also paints through paintWhole.
    const el = drawFeedResponse(
      response({ result: { case: "error", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
        response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    mount(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    const tree = el.querySelector(".mp-tree");
    // Act — the column narrows, but the resize is delivered for the body alone.
    staged.layout.containingPx = 500;
    fireResize(body);
    vi.runOnlyPendingTimers();
    // Assert — no re-wrap: the very same tree node.
    expect(el.querySelector(".mp-tree")).toBe(tree);
  });

  it("tears the containing block's observer down with the bubble", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    // Assert — no wrong-width tree was ever drawn.
    expect(el.querySelector(".mp-tree")).toBeNull();
  });

  it("draws the tree at the cap's budget the moment it is attached", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    // Act
    mount(el);
    // Assert — it wrapped, and no line is wider than the budget.
    const widths = lineWidths(el);
    expect(widths.length).toBeGreaterThan(4);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout));
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
      response({ result: { case: "success", value: { prose: { markdown: tree } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    expect(el.querySelector(".mp-tree")).toBeNull();
    // Act
    mount(el);
    // Assert
    expect(Math.max(...lineWidths(el))).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("draws plain prose at once, detached, since it needs no width", () => {
    // Arrange / Act
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: "**done**" } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(".bubble-body strong")?.textContent).toBe("done");
  });

  it("re-wraps when the containing block's width moves the budget", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
    expect(Math.max(...lineWidths(el))).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("does not repaint when the column resizes without moving the budget", () => {
    // Arrange
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: TREE } } } }),
      rowContext(),
    );
    document.body.appendChild(streamingEl);
    const streamingBody = streamingEl.querySelector<HTMLElement>(".bubble-body");
    if (streamingBody === null) throw new Error("no bubble body");
    // Act — drive the type-out to completion.
    vi.advanceTimersByTime(5000);
    // The settled draw of the same text is the oracle: one settled paint.
    const settled = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: TREE } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    document.body.appendChild(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Advance until the tree is on screen with a leading line, still arriving.
    let before: Element[] = [];
    for (let i = 0; i < 60 && before.length < 2; i++) {
      vi.advanceTimersByTime(16);
      const lines = [...body.querySelectorAll(".mp-tree .mp-line")];
      const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
      if (lines.length >= 2 && shown < SHOWCASE_TREE.length) before = lines;
    }
    expect(before.length).toBeGreaterThanOrEqual(2);
    const firstBefore = before[0];
    // Act — advance further so the tail wraps onto more lines.
    let after: Element[] = [];
    for (let i = 0; i < 60 && after.length <= before.length; i++) {
      vi.advanceTimersByTime(16);
      after = [...body.querySelectorAll(".mp-tree .mp-line")];
    }
    // Assert — the tail grew and the leading line is the SAME node, never rebuilt.
    expect(after.length).toBeGreaterThan(before.length);
    expect(after[0]).toBe(firstBefore);
  });

  it("re-wraps the arriving tree to the whole render at a new width on resize", () => {
    // Arrange — the tree types out in a 1000px column (93 columns).
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    const column = mount(el);
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Drive the type-out to completion so no reveal frame is left pending; the
    // resize is then the only work the timers run.
    vi.advanceTimersByTime(5000);
    const before = stagedCols(staged.layout);
    // Act — the column narrows: the observer re-measures and repaints.
    staged.layout.containingPx = 600;
    fireResize(column);
    vi.runOnlyPendingTimers();
    // Assert — the re-wrapped prose equals the whole render at the NEW width,
    // and is no longer the render at the width it first drew at.
    const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
    const after = stagedCols(staged.layout);
    expect(prose(body)).toBe(proseHtml(SHOWCASE_TREE.slice(0, shown), () => after));
    expect(prose(body)).not.toBe(proseHtml(SHOWCASE_TREE.slice(0, shown), () => before));
  });

  it("appends to a growing plain paragraph in place rather than rebuilding it", () => {
    // Arrange — plain prose (no tree), a complete first paragraph and a second
    // that keeps growing.
    const markdown =
      "First paragraph, complete and stable.\n\nSecond paragraph that keeps on growing word by word right here.";
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: "some words arriving over frames" } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "the whole answer" } } } }),
      rowContext(),
    );
    // Assert
    expect(el.querySelector(".response-arriving")).toBeNull();
  });
});

describe("the thinking bubble", () => {
  it("draws a thinking response as a purple assistant bubble marked thinking", () => {
    // Arrange, Act — a thinking-flagged response.
    const el = drawFeedResponse(
      response({ thinking: true, result: { case: "update", value: { prose: { markdown: "weighing" } } } }),
      rowContext(),
    );
    // Assert — the purple assistant bubble also carries the thinking marker.
    expect(el.classList.contains("assistant")).toBe(true);
    expect(el.classList.contains(THINKING_BUBBLE_CLASS)).toBe(true);
  });

  it("leaves an ordinary response bubble unmarked as thinking", () => {
    // Arrange, Act — no thinking flag.
    const el = drawFeedResponse(
      response({ result: { case: "update", value: { prose: { markdown: "an answer" } } } }),
      rowContext(),
    );
    // Assert
    expect(el.classList.contains(THINKING_BUBBLE_CLASS)).toBe(false);
  });

  it("keeps the green final-answer rule from matching a thinking bubble", () => {
    // Arrange — a thinking bubble that has (defensively) been given the
    // final-response class.
    const el = drawFeedResponse(
      response({ thinking: true, result: { case: "success", value: { prose: { markdown: "reasoning" } } } }),
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
      response({ result: { case: "success", value: { prose: { markdown: "the answer" } } } }),
      rowContext(),
    );
    el.classList.add("final-response");
    // Act, Assert — the green selector matches an ordinary response.
    expect(el.matches(".bubble.assistant.final-response:not(.thinking-bubble)")).toBe(true);
  });

  it("draws multiple thinking blocks as separate bubbles", () => {
    // Arrange, Act — two thinking blocks drawn as two rows.
    const first = drawFeedResponse(
      response({ thinking: true, result: { case: "success", value: { prose: { markdown: "block one" } } } }),
      rowContext(),
    );
    const second = drawFeedResponse(
      response({ thinking: true, result: { case: "success", value: { prose: { markdown: "block two" } } } }),
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
      response({ thinking: true, result: { case: "update", value: { prose: { markdown: "weighing" } } } }),
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
      response({ result: { case: "update", value: { prose: { markdown: "an answer" } } } }),
      rowContext(),
    );
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.some((rec) => rec.operation === "feed.draw-thinking")).toBe(false);
  });
});

/**
 * THE THINKING BUBBLE'S TWO-LINE CAP (owner ruling, 2026-09-23). A collapsed
 * thinking bubble shows at most two lines, wearing the response bubble's own
 * fade (`has-more`) when it runs past them, and a click expands and
 * collapses it exactly as it does a response bubble.
 *
 * jsdom resolves the cascade but lays nothing out, so `layOut` stands in for
 * the layout engine: the box's body is LINES tall, and while the cascade
 * hands the box its collapsed cap it shows at most `--cap-lines` of them (the
 * token resolved through `:root`, as the browser would); once the cascade hands
 * it the expanded 50vh ceiling the window is taken to hold every line. Every
 * figure it reads comes from the real stylesheet, so the cap under test is the
 * one the file declares.
 */
describe("the thinking bubble's two-line cap", () => {
  const LINE_PX = 20;

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

  function thinking(state: "update" | "success"): FeedResponse {
    return response({ thinking: true, result: { case: state, value: { prose: { markdown: "weighing" } } } });
  }

  it("caps a thinking bubble's scroll box at two lines", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(thinking("success"), 5);
      // Assert
      expect(cascadedValue(scroll, "--bubble-cap-lines")).toBe("2");
    } finally {
      teardown();
    }
  });

  it("caps a still-streaming thinking bubble at two lines too", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(thinking("update"), 5);
      // Assert
      expect(cascadedValue(scroll, "--bubble-cap-lines")).toBe("2");
    } finally {
      teardown();
    }
  });

  it("wears the response bubble's fade when it runs past two lines", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act — three lines: past the thinking cap, far inside the response cap.
      const scroll = mounted(thinking("success"), 3);
      // Assert
      expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(true);
    } finally {
      teardown();
    }
  });

  it("expands a capped thinking bubble on a click, dropping the fade", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const scroll = mounted(thinking("success"), 3);
      const collapsed = scroll.clientHeight;
      // Act
      scroll.click();
      // Assert — grown from two lines to the full content, nothing left below
      // the fold.
      expect([
        collapsed,
        scroll.classList.contains(EXPANDED_CLASS),
        scroll.clientHeight,
        scroll.classList.contains(HAS_MORE_CLASS),
      ]).toEqual([2 * LINE_PX, true, 3 * LINE_PX, false]);
    } finally {
      teardown();
    }
  });

  it("collapses an expanded thinking bubble on a second click, restoring the fade", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const scroll = mounted(thinking("success"), 3);
      scroll.click();
      // Act
      scroll.click();
      // Assert — back to two lines under the fade.
      expect([
        scroll.classList.contains(EXPANDED_CLASS),
        scroll.clientHeight,
        scroll.classList.contains(HAS_MORE_CLASS),
      ]).toEqual([false, 2 * LINE_PX, true]);
    } finally {
      teardown();
    }
  });

  it("shows no fade on a thinking bubble that fits in two lines", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(thinking("success"), 2);
      // Assert
      expect(scroll.classList.contains(HAS_MORE_CLASS)).toBe(false);
    } finally {
      teardown();
    }
  });

  it("gives a fitting thinking bubble nothing to expand on a click", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      const scroll = mounted(thinking("success"), 2);
      const collapsed = scroll.clientHeight;
      // Act
      scroll.click();
      // Assert — the same height and still no fade: the click reveals nothing,
      // exactly as it does on a short response bubble.
      expect([scroll.clientHeight, scroll.classList.contains(HAS_MORE_CLASS)]).toEqual([
        collapsed,
        false,
      ]);
    } finally {
      teardown();
    }
  });

  it("leaves a normal response bubble on the shared feed cap", () => {
    // Arrange
    const teardown = installStylesheet();
    try {
      // Act
      const scroll = mounted(
        response({ result: { case: "success", value: { prose: { markdown: "an answer" } } } }),
        3,
      );
      // Assert — the shared budget, so three lines fit with no fade.
      expect([
        cascadedValue(scroll, "--bubble-cap-lines"),
        scroll.classList.contains(HAS_MORE_CLASS),
      ]).toEqual(["var(--feed-cap-lines)", false]);
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
      response({ finalAnswer: true, result: { case: "success", value: { prose: { markdown: "the answer" } } } }),
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
      result: { case: "success", value: { prose: { markdown: "the answer" } } },
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
      response({ result: { case: "success", value: { prose: { markdown: "just talking" } } } }),
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
        result: { case: "success", value: { prose: { markdown: "reasoning" } } },
      }),
      rowContext(),
    );
    // Assert — the green never lands on a purple thinking bubble.
    expect(el.classList.contains(FINAL_RESPONSE_CLASS)).toBe(false);
    expect(el.classList.contains(THINKING_BUBBLE_CLASS)).toBe(true);
  });

  it("lets the BLUE selected-response border win over the green", () => {
    // Arrange — a green final answer that is also the selected reply target, so
    // both classes ride the same bubble.
    const el = drawFeedResponse(
      response({ finalAnswer: true, result: { case: "success", value: { prose: { markdown: "the answer" } } } }),
      rowContext(),
    );
    el.classList.add("response-selected");
    // Act, Assert — the drawn bubble matches BOTH the green rule and the
    // higher-specificity blue rule, and the blue rule is declared AFTER the green
    // one in the stylesheet, so the cascade paints it blue while selected and
    // falls back to green the moment the selection clears. (var()-resolved
    // colours are not observable under jsdom; selector + order is the
    // deterministic proof, mirrored from styles.test.ts.)
    expect(el.matches('.bubble.final-response:not([data-variant="thinking"])')).toBe(true);
    expect(el.matches(".bubble.final-response.response-selected")).toBe(true);
    const green = stylesheet.indexOf('.bubble.final-response:not([data-variant="thinking"])');
    const blue = stylesheet.indexOf(".bubble.final-response.response-selected");
    expect(green).toBeGreaterThanOrEqual(0);
    expect(blue).toBeGreaterThan(green);
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
    usage?: { text: string; atMs?: bigint };
    notice?: { heading: string };
  }

  /** An arriving response carrying MARKDOWN. */
  function arriving(markdown: string, extra: Extra = {}) {
    return response({ ...extra, result: { case: "update", value: { prose: { markdown } } } });
  }

  /** A settled response carrying MARKDOWN. */
  function settledAs(markdown: string, extra: Extra = {}) {
    return response({ ...extra, result: { case: "success", value: { prose: { markdown } } } });
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
    // Arrange -- the reader scrolled 120px inside the expanded bubble.
    const before = drawFeedResponse(arriving("hello"), rowContext());
    mount(before);
    vi.advanceTimersByTime(2000);
    const box = before.querySelector<HTMLElement>(".bubble-scroll");
    if (box === null) throw new Error("the bubble drew no scroll box");
    box.scrollTop = 120;
    // Act
    const after = drawFeedResponse(arriving("hello world"), rowContext(before));
    // Assert
    expect([after.querySelector(".bubble-scroll") === box, box.scrollTop]).toEqual([true, 120]);
  });

  it("keeps the scroll box when the response settles", () => {
    // Arrange + Act
    const { before, after } = redraw(arriving("hello"), settledAs("hello"));
    // Assert
    expect(after.querySelector(".bubble-scroll")).toBe(before.querySelector(".bubble-scroll"));
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
    before.classList.add("response-selected");
    // Act
    drawFeedResponse(arriving("hello world"), rowContext(before));
    // Assert
    expect(before.classList.contains("response-selected")).toBe(true);
  });

  it("keeps the usage corner when its figure did not change", () => {
    // Arrange
    const before = drawFeedResponse(arriving("hello", { usage: { text: "1k" } }), rowContext());
    mount(before);
    const corner = before.querySelector(".usage-corner");
    // Act
    drawFeedResponse(arriving("hello world", { usage: { text: "1k" } }), rowContext(before));
    // Assert
    expect(before.querySelector(".usage-corner")).toBe(corner);
  });

  it("replaces the usage corner when its figure changed", () => {
    // Arrange
    const before = drawFeedResponse(arriving("hello", { usage: { text: "1k" } }), rowContext());
    mount(before);
    // Act
    drawFeedResponse(arriving("hello world", { usage: { text: "2k" } }), rowContext(before));
    // Assert
    expect(before.querySelectorAll(".usage-stamp")[0]?.textContent).toBe("2k");
  });

  it("stops the clock of a corner it replaced", () => {
    // Arrange -- a settled corner ticks its "ago".
    const before = drawFeedResponse(settledAs("done", { usage: { text: "1k", atMs: 1_000n } }), rowContext());
    mount(before);
    const corner = before.querySelector(".usage-corner");
    // Act
    drawFeedResponse(settledAs("done", { usage: { text: "2k", atMs: 1_000n } }), rowContext(before));
    // Assert
    expect(corner?.querySelector(`[${TICKING_ATTRIBUTE}]`)).toBeNull();
  });

  it("draws one cut-short marker however often the broken state is pushed", () => {
    // Arrange
    const broken = response({ result: { case: "error", value: { prose: { markdown: "half" } } } });
    // Act
    const { after } = redraw(broken, broken);
    // Assert
    expect(after.querySelectorAll(".response-cut-short-marker")).toHaveLength(1);
  });

  it("drops the notice heading when a re-push carries none", () => {
    // Arrange + Act
    const { after } = redraw(settledAs("hi", { notice: { heading: "interrupted" } }), settledAs("hi"));
    // Assert
    expect([after.querySelector(".response-notice-heading"), after.hasAttribute("data-notice")]).toEqual([
      null,
      false,
    ]);
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
