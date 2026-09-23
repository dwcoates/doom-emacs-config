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
  THINKING_BUBBLE_CLASS,
  USAGE_REVEALED_CLASS,
  drawFeedResponse,
  measureTreeCols,
  proseHtml,
  revealedSoFar,
} from "../../../src/feed/cards/response.js";
import { DEFAULT_TREE_COLS, visibleWidth } from "../../../src/metaprompt-tree.js";
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
    host.innerHTML = proseHtml(markdown, 105);
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
    host.innerHTML = proseHtml(tree, 60);
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

  it("does not pin a settled tree bubble to the cap; it shrinks to fit", () => {
    // Arrange / Act — a settled response that drew a metaprompt tree.
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
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
      expect(maxWidth).toBe("77%");
      el.remove();
    } finally {
      teardown();
    }
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

/**
 * `measureTreeCols` reports the columns the tree wraps to. The regression it
 * guards: it once measured `body.clientWidth` — the bubble's ALREADY-shrunk
 * fit-content width — so a tree wrapped to whatever narrow width its shortest
 * line produced, prematurely. It now measures the MAX width the bubble may
 * occupy (its resolved `max-width` cap), so a line wraps only when it truly
 * exceeds the cap. jsdom does no layout, so each case stages the layout the
 * measure reads: a monospace char width (the internal probe), the bubble's cap
 * (`getComputedStyle().maxWidth`), its containing-block width and current
 * border box, and the body's shrunk `clientWidth`.
 */
describe("the columns the tree wraps to are measured against the bubble cap", () => {
  const CHAR_PX = 8;
  const rect = (width: number): DOMRect =>
    ({ width, height: 0, top: 0, left: 0, right: width, bottom: 0, x: 0, y: 0, toJSON: () => ({}) });

  let protoRect: typeof Element.prototype.getBoundingClientRect;

  beforeEach(() => {
    // The internal probe (a `.mp-tree` div of 100 zeros) is created inside the
    // measure and cannot be reached to stub directly, so the char width is
    // staged on the prototype, keyed on the probe's class.
    // Captured to be ASSIGNED back in afterEach, never called off the reference.
    // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
    protoRect = Element.prototype.getBoundingClientRect;
    Element.prototype.getBoundingClientRect = function staged(this: Element): DOMRect {
      if (this.className === "mp-tree") return rect(CHAR_PX * 100);
      return rect(0);
    };
  });

  afterEach(() => {
    Element.prototype.getBoundingClientRect = protoRect;
    vi.restoreAllMocks();
  });

  /** A staged bubble/body pair with the geometry the measure reads. */
  function stage(opts: {
    maxWidth: string;
    containingWidth: number;
    bubbleOuter: number;
    bodyClientWidth: number;
    bodyPad?: number;
  }): HTMLElement {
    const pad = opts.bodyPad ?? 0;
    const parent = document.createElement("div");
    Object.defineProperty(parent, "clientWidth", { value: opts.containingWidth, configurable: true });
    const bubble = document.createElement("div");
    bubble.className = "bubble assistant md";
    // The bubble's current (fit-content) border box, an own property so it wins
    // over the prototype stub that serves the probe.
    bubble.getBoundingClientRect = () => rect(opts.bubbleOuter);
    const body = document.createElement("div");
    body.className = "bubble-body";
    Object.defineProperty(body, "clientWidth", { value: opts.bodyClientWidth, configurable: true });
    bubble.appendChild(body);
    parent.appendChild(bubble);
    document.body.appendChild(parent);
    vi.spyOn(window, "getComputedStyle").mockImplementation((el: Element) => {
      if (el === bubble) return { maxWidth: opts.maxWidth } as unknown as CSSStyleDeclaration;
      return {
        maxWidth: "none",
        paddingLeft: `${pad}px`,
        paddingRight: `${pad}px`,
      } as unknown as CSSStyleDeclaration;
    });
    return body;
  }

  /**
   * A DETACHED bubble (built but never attached, the shape at synchronous first
   * paint) plus an optional attached `#feed` reference of known content width.
   * The chrome insets are staged on the body's padding — the styles path reads
   * them when there is no geometry to measure. When `feedContentWidth` is
   * omitted, no feed is attached, so nothing can resolve the cap.
   */
  function stageDetached(opts: { maxWidth: string; feedContentWidth?: number; bodyPad?: number }): HTMLElement {
    const pad = opts.bodyPad ?? 0;
    if (opts.feedContentWidth !== undefined) {
      const feed = document.createElement("main");
      feed.id = "feed";
      Object.defineProperty(feed, "clientWidth", { value: opts.feedContentWidth, configurable: true });
      document.body.appendChild(feed);
    }
    // The bubble is NEVER appended: it stays detached, so every rect reads 0.
    const bubble = document.createElement("div");
    bubble.className = "bubble assistant md";
    const scroll = document.createElement("div");
    scroll.className = "bubble-scroll";
    const body = document.createElement("div");
    body.className = "bubble-body";
    scroll.appendChild(body);
    bubble.appendChild(scroll);
    vi.spyOn(window, "getComputedStyle").mockImplementation((el: Element) => {
      if (el === bubble) {
        return { maxWidth: opts.maxWidth, paddingLeft: "0px", paddingRight: "0px" } as unknown as CSSStyleDeclaration;
      }
      if (el === body) {
        return {
          maxWidth: "none",
          paddingLeft: `${pad}px`,
          paddingRight: `${pad}px`,
        } as unknown as CSSStyleDeclaration;
      }
      // The scroll wrapper and #feed: no padding, no border.
      return { maxWidth: "none", paddingLeft: "0px", paddingRight: "0px" } as unknown as CSSStyleDeclaration;
    });
    return body;
  }

  it("resolves the cap against the attached feed reference when the bubble is DETACHED at first paint", () => {
    // Arrange — the bubble is built but not yet attached (drawFeedResponse paints
    // before feed-view attaches the row); the root feed column is attached at a
    // known content width, standing in for the containing block the row lands in.
    const body = stageDetached({ maxWidth: "70.125%", feedContentWidth: 1000, bodyPad: 10 });
    // Act
    const cols = measureTreeCols(body);
    // Assert — cap 701.25px, insets 20px (body padding), 681.25px content,
    // floor(/8)=85: the cap-based count, NOT the detached fallback default.
    expect(cols).toBe(85);
    expect(cols).not.toBe(DEFAULT_TREE_COLS);
  });

  it("resolves the TRUE 77%-of-feed cap at synchronous first paint (detached), never the default", () => {
    // Arrange — the real cap fraction the stylesheet uses (77%), staged against
    // an attached feed of known content width while the bubble is still DETACHED
    // (the shape at first paint, before feed-view attaches the row). This is the
    // Part A(a) regression guard: the percentage cap must resolve to feed-based
    // px, not fall back to DEFAULT_TREE_COLS.
    const body = stageDetached({ maxWidth: "77%", feedContentWidth: 1000, bodyPad: 10 });
    // Act
    const cols = measureTreeCols(body);
    // Assert — cap 770px, insets 20px (body padding) → 750px content,
    // floor(750/8)=93: the feed-based count, NOT the detached fallback default.
    expect(cols).toBe(93);
    expect(cols).not.toBe(DEFAULT_TREE_COLS);
  });

  it("falls back to the default width when nothing is attached to resolve the cap against", () => {
    // Arrange — a detached bubble AND no feed in the document: no reference at all.
    const body = stageDetached({ maxWidth: "70.125%", bodyPad: 10 });
    // Act + Assert — no throw, the genuine no-layout fallback.
    expect(measureTreeCols(body)).toBe(DEFAULT_TREE_COLS);
  });

  it("uses the bubble's OWN containing block, not the feed reference, when it is attached", () => {
    // Arrange — a wide #feed also sits in the document, but the bubble is
    // attached under a NARROWER parent; the measure must follow the bubble's own
    // parent so the feed reference never leaks into a laid-out bubble.
    const feed = document.createElement("main");
    feed.id = "feed";
    Object.defineProperty(feed, "clientWidth", { value: 4000, configurable: true });
    document.body.appendChild(feed);
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    // Act
    const cols = measureTreeCols(body);
    // Assert — 85, from the 1000px own parent, not the ~350 a 4000px feed cap
    // would give.
    expect(cols).toBe(85);
  });

  it("wraps a detached first-paint tree to the SAME cap the attached bubble uses, so it does not overflow", () => {
    // Arrange — the live reproduction shape: a settled tree measured while
    // detached, with the feed reference attached; the attached equivalent wraps
    // at the cap, and the detached first paint must match it, not overflow at the
    // 105-column default.
    const detached = stageDetached({ maxWidth: "70.125%", feedContentWidth: 1000, bodyPad: 10 });
    const detachedCols = measureTreeCols(detached);
    vi.restoreAllMocks();
    const attached = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    const attachedCols = measureTreeCols(attached);
    // Act — wrap the showcase tree at the detached first-paint cap.
    const host = document.createElement("div");
    host.innerHTML = proseHtml(SHOWCASE_TREE, detachedCols);
    // Assert — the detached first paint wraps at the cap, identical to attached,
    // and never at the overflowing default; the tree actually wrapped.
    expect(detachedCols).toBe(attachedCols);
    expect(detachedCols).not.toBe(DEFAULT_TREE_COLS);
    expect(host.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length).toBeGreaterThan(4);
  });

  it("measures the max-width cap, not the shrunk fit-content clientWidth", () => {
    // Arrange — cap 70.125% of a 1000px column = 701.25px; the bubble currently
    // renders at 220px (outer) around a 200px body, so the fixed insets are 20px
    // and the body's content at the cap is 681.25px.
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    // Act
    const cols = measureTreeCols(body);
    // Assert — floor(681.25 / 8) = 85, the cap-based count, NOT floor(200/8)=25.
    expect(cols).toBe(85);
  });

  it("does not let the shrunk clientWidth drag the column count down", () => {
    // Arrange — same cap, but an even narrower current bubble; the cap-based
    // count must not follow the shrink.
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 90, bodyClientWidth: 72 });
    // Act
    const cols = measureTreeCols(body);
    // Assert — insets 18px, body at cap 683.25px, floor(/8)=85; clientWidth would give floor(72/8)=9.
    expect(cols).toBe(85);
    expect(cols).toBeGreaterThan(9);
  });

  it("honors a px max-width cap directly", () => {
    // Arrange — a browser that resolves the cap to px hands it back as px.
    const body = stage({ maxWidth: "560px", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    // Act
    const cols = measureTreeCols(body);
    // Assert — insets 20px, 540px content, floor(540/8)=67.
    expect(cols).toBe(67);
  });

  it("resolves the percentage cap against the containing block, so a wider column yields more columns", () => {
    // Arrange — the same bubble in two feed-column widths.
    const narrow = stage({ maxWidth: "70.125%", containingWidth: 600, bubbleOuter: 220, bodyClientWidth: 200 });
    const narrowCols = measureTreeCols(narrow);
    vi.restoreAllMocks();
    const wide = stage({ maxWidth: "70.125%", containingWidth: 1400, bubbleOuter: 220, bodyClientWidth: 200 });
    // Act
    const wideCols = measureTreeCols(wide);
    // Assert
    expect(wideCols).toBeGreaterThan(narrowCols);
  });

  it("falls back to the default width when there is no resolvable cap and no layout", () => {
    // Arrange — no cap (`none`) and a zero-width body: no measurement at all.
    const body = stage({ maxWidth: "none", containingWidth: 1000, bubbleOuter: 0, bodyClientWidth: 0 });
    // Act + Assert
    expect(measureTreeCols(body)).toBe(DEFAULT_TREE_COLS);
  });

  it("falls back to the clientWidth measure when a laid-out bubble has no cap", () => {
    // Arrange — layout exists but no cap resolves, so the legacy clientWidth
    // measure still applies.
    const body = stage({ maxWidth: "none", containingWidth: 1000, bubbleOuter: 420, bodyClientWidth: 400 });
    // Act + Assert — floor(400/8)=50.
    expect(measureTreeCols(body)).toBe(50);
  });

  it("does not wrap a tree whose longest line fits under the cap though the bubble renders narrower", () => {
    // Arrange — cap-based cols is 85; a tree whose longest branch is ~60 columns
    // fits under the cap but would wrap at the shrunk clientWidth (25 cols).
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    const tree = [
      "1 🌳 A tree whose longest branch is comfortably under the cap.",
      "├── 1.1 A branch of about sixty rendered columns, fitting the cap.",
      "└── 1.2 Another branch, also short enough to stand on one line.",
    ].join("\n");
    // Act
    const cols = measureTreeCols(body);
    const host = document.createElement("div");
    host.innerHTML = proseHtml(tree, cols);
    // Assert — one line per branch: nothing wrapped.
    expect(host.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length).toBe(3);
  });

  it("still wraps a tree whose line genuinely exceeds the cap", () => {
    // Arrange — cap-based cols is 85; a branch far past it must still wrap.
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    // Act
    const cols = measureTreeCols(body);
    const host = document.createElement("div");
    host.innerHTML = proseHtml(SHOWCASE_TREE, cols);
    // Assert — more rendered lines than the four source branches: it wrapped.
    expect(host.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)").length).toBeGreaterThan(4);
  });

  it("measures the cap even when the body is EMPTY, so the first paint is at the final width", () => {
    // Arrange — an EMPTY body (no children yet), but a laid-out bubble: the
    // chrome (insets) is present regardless of content, so the cap-based measure
    // holds before anything is drawn. bubbleOuter 40 around a 20px body = 20px of
    // chrome even with nothing inside.
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 40, bodyClientWidth: 20 });
    expect(body.children.length).toBe(0);
    // Act
    const cols = measureTreeCols(body);
    // Assert — 681.25px content at the cap, floor(/8)=85; NOT the default width,
    // so an empty first-paint body does not fall to DEFAULT_TREE_COLS.
    expect(cols).toBe(85);
    expect(cols).not.toBe(DEFAULT_TREE_COLS);
  });

  it("returns the SAME cols after a content-only fit-content change, so nothing can oscillate", () => {
    // Arrange — the settle transition: the bubble's fit-content shrinks (the
    // streaming ellipsis removed, the final-answer border recolored) while the
    // feed column (the cap's containing block) is unchanged.
    const body = stage({ maxWidth: "70.125%", containingWidth: 1000, bubbleOuter: 220, bodyClientWidth: 200 });
    const before = measureTreeCols(body);
    // Act — both the bubble border box and the body content shrink together (the
    // chrome between them is constant), the shape a content-driven change takes.
    const bubble = body.closest<HTMLElement>(".bubble");
    if (bubble === null) throw new Error("no bubble");
    bubble.getBoundingClientRect = () => rect(180);
    Object.defineProperty(body, "clientWidth", { value: 160, configurable: true });
    const after = measureTreeCols(body);
    // Assert — the measured width did not move, so `reflowOnResize` sees the same
    // integer column count and cannot re-wrap: the flicker cascade has no source.
    expect(after).toBe(before);
  });

  it("keeps the column budget cap-derived when the pin is gone and the bubble shrinks to fit", () => {
    // Arrange — the shrink-to-fit change (owner ruling 2026-09-15): with the
    // `bubble-fill-cap` width pin removed, a tree bubble now collapses to its
    // widest wrapped line, far below the 77% cap. The tree budget must still be
    // measured from the CAP (the containing block), never the shrunk width, or
    // the shrink would feed a re-wrap and oscillate.
    const body = stage({ maxWidth: "77%", containingWidth: 1000, bubbleOuter: 760, bodyClientWidth: 750 });
    const atCap = measureTreeCols(body);
    // Act — the bubble collapses hard to its widest line (a genuine fit-content
    // shrink the pin used to prevent); measure a SECOND time.
    const bubble = body.closest<HTMLElement>(".bubble");
    if (bubble === null) throw new Error("no bubble");
    bubble.getBoundingClientRect = () => rect(300);
    Object.defineProperty(body, "clientWidth", { value: 290, configurable: true });
    const afterShrink = measureTreeCols(body);
    // Assert — the second measure equals the first: the budget is cap-derived
    // (77% of the 1000px containing block, minus the constant chrome insets),
    // so shrinking the bubble cannot change the column count and nothing
    // oscillates.
    expect(afterShrink).toBe(atCap);
  });

  it("does not re-wrap a settled tree when only the bubble's own fit-content width changes", () => {
    // Arrange — a settled response with a tree; its reflow observer is wired at
    // draw (when the bubble is still detached, so it starts at the default
    // width). getComputedStyle is keyed by class because the elements are minted
    // inside the draw.
    vi.spyOn(window, "getComputedStyle").mockImplementation((el: Element) => {
      if (el.classList.contains("bubble")) return { maxWidth: "70.125%" } as unknown as CSSStyleDeclaration;
      return { maxWidth: "none", paddingLeft: "0px", paddingRight: "0px" } as unknown as CSSStyleDeclaration;
    });
    const el = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: SHOWCASE_TREE } } } }),
      rowContext(),
    );
    const body = el.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no bubble body");
    // Attach under a sized containing block and stage the bubble's real box.
    const parent = document.createElement("div");
    Object.defineProperty(parent, "clientWidth", { value: 1000, configurable: true });
    parent.appendChild(el);
    document.body.appendChild(parent);
    el.getBoundingClientRect = () => rect(220);
    Object.defineProperty(body, "clientWidth", { value: 200, configurable: true });
    const treeInitial = body.querySelector(".mp-tree");

    // Act 1 — the mount correction: the observer's first measurement moves from
    // the detached default width to the real cap, so the tree is repainted ONCE.
    fireResize(body);
    vi.runOnlyPendingTimers();
    const treeAfterMount = body.querySelector(".mp-tree");

    // Act 2 — a content-only fit-content shrink (the settle transition), column
    // unchanged: the bubble box and body content shrink together.
    el.getBoundingClientRect = () => rect(180);
    Object.defineProperty(body, "clientWidth", { value: 160, configurable: true });
    fireResize(body);
    vi.runOnlyPendingTimers();
    const treeAfterShrink = body.querySelector(".mp-tree");

    // Assert — the mount correction repainted (a new tree node), but the
    // content-only change did NOT: the tree node is the very same one, so no
    // re-wrap fired and there is no cascade.
    expect(treeAfterMount).not.toBe(treeInitial);
    expect(treeAfterShrink).toBe(treeAfterMount);
    stopTicking(el);
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
  /** The body's prose HTML, compared against the whole-render oracle. The
   * streaming reveal carries no trailing indicator node, so it is exactly the
   * oracle's markup. */
  function prose(body: HTMLElement): string {
    return body.innerHTML;
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
    // The settled draw of the same text is the oracle: one-shot `paintWhole`.
    const settledBody = drawFeedResponse(
      response({ result: { case: "success", value: { prose: { markdown: TREE } } } }),
      rowContext(),
    ).querySelector<HTMLElement>(".bubble-body");
    if (settledBody === null) throw new Error("no settled body");
    // Assert — the reconciled reveal lands exactly where the one-shot render does.
    expect(prose(streamingBody)).toBe(settledBody.innerHTML);
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
    // The measure reads a monospace char width off a hidden `.mp-tree` probe;
    // jsdom lays nothing out, so the probe's width is staged on the prototype.
    const CHAR_PX = 8;
    const rect = (w: number): DOMRect =>
      ({ width: w, height: 0, top: 0, left: 0, right: w, bottom: 0, x: 0, y: 0, toJSON: () => ({}) });
    // eslint-disable-next-line @typescript-eslint/unbound-method -- reassigned back below
    const protoRect = Element.prototype.getBoundingClientRect;
    Element.prototype.getBoundingClientRect = function staged(this: Element): DOMRect {
      return this.className === "mp-tree" ? rect(CHAR_PX * 100) : rect(0);
    };
    try {
      // Arrange — draw detached (cols = default 105), then mount under a sized
      // column and stage a resolvable cap so a resize measures 85 columns.
      const el = drawFeedResponse(
        response({ result: { case: "update", value: { prose: { markdown: SHOWCASE_TREE } } } }),
        rowContext(),
      );
      const body = el.querySelector<HTMLElement>(".bubble-body");
      if (body === null) throw new Error("no bubble body");
      const parent = document.createElement("div");
      Object.defineProperty(parent, "clientWidth", { value: 1000, configurable: true });
      parent.appendChild(el);
      document.body.appendChild(parent);
      // Drive the type-out to completion so no reveal frame is left pending; the
      // resize is then the only work the timers run.
      vi.advanceTimersByTime(5000);
      el.getBoundingClientRect = () => rect(220);
      Object.defineProperty(body, "clientWidth", { value: 200, configurable: true });
      vi.spyOn(window, "getComputedStyle").mockImplementation((node: Element) => {
        if (node === el) return { maxWidth: "70.125%" } as unknown as CSSStyleDeclaration;
        return {
          maxWidth: "none",
          paddingLeft: "0px",
          paddingRight: "0px",
        } as unknown as CSSStyleDeclaration;
      });
      // Act — the width changed: the observer re-measures (85) and repaints.
      fireResize(body);
      vi.runOnlyPendingTimers();
      // Assert — the re-wrapped prose equals the whole render at the NEW width,
      // and is no longer the render at the 105 columns it first drew at.
      const shown = Number(el.getAttribute(REVEALED_ATTRIBUTE));
      expect(prose(body)).toBe(proseHtml(SHOWCASE_TREE.slice(0, shown), 85));
      expect(prose(body)).not.toBe(proseHtml(SHOWCASE_TREE.slice(0, shown), 105));
    } finally {
      Element.prototype.getBoundingClientRect = protoRect;
      vi.restoreAllMocks();
    }
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
 * fade + chevron (`has-more`) when it runs past them, and a click expands and
 * collapses it exactly as it does a response bubble.
 *
 * jsdom resolves the cascade but lays nothing out, so `layOut` stands in for
 * the layout engine: the box's content is LINES tall, and while the cascade
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

  /** Give SCROLL content LINES tall, clipped by whatever cap the cascade hands it. */
  function layOut(scroll: HTMLElement, lines: number): void {
    Object.defineProperty(scroll, "scrollHeight", { configurable: true, value: lines * LINE_PX });
    Object.defineProperty(scroll, "clientHeight", {
      configurable: true,
      get: () => {
        if (cascadedValue(scroll, "max-height") === "50vh") return lines * LINE_PX;
        return Math.min(lines, resolvedNumber(cascadedValue(scroll, "--cap-lines"))) * LINE_PX;
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
      expect(cascadedValue(scroll, "--cap-lines")).toBe("2");
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
      expect(cascadedValue(scroll, "--cap-lines")).toBe("2");
    } finally {
      teardown();
    }
  });

  it("wears the response bubble's fade and chevron when it runs past two lines", () => {
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

  it("shows no fade or chevron on a thinking bubble that fits in two lines", () => {
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
        cascadedValue(scroll, "--cap-lines"),
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
    expect(el.matches(".bubble.assistant.final-response:not(.thinking-bubble)")).toBe(true);
    expect(el.matches(".bubble.assistant.final-response.response-selected")).toBe(true);
    const green = stylesheet.indexOf(".bubble.assistant.final-response:not(.thinking-bubble)");
    const blue = stylesheet.indexOf(".bubble.assistant.final-response.response-selected");
    expect(green).toBeGreaterThanOrEqual(0);
    expect(blue).toBeGreaterThan(green);
  });
});
