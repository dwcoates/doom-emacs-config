// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedBreadcrumbSchema, FeedRowSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  NEST_ATTRIBUTE,
  createRowRenderers,
  arrangeSubfeedRows,
  armName,
  defaultBubbleBody,
  drawBreadcrumbTrail,
  nestSlot,
  type SubfeedView,
} from "../../src/feed/renderers.js";
import { feedId, harness, mergeTabRow, responseRow, rowContext, userPromptRow } from "./harness.js";
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { createTicker } from "../../src/clock.js";
import { tick } from "../../src/feed/ticking.js";
import type { RowRenderers } from "../../src/feed/renderers.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** A view over ROWS whose `drawRow` builds a marked element per row. */
function viewOf(
  rows: FeedRow[],
  extra: Partial<SubfeedView> = {},
): SubfeedView & { fire(): void; drawn: string[] } {
  const listeners = new Set<() => void>();
  const elements = new Map<string, HTMLElement>();
  const drawn: string[] = [];
  return {
    rows: () => rows,
    onChange(fn) {
      listeners.add(fn);
      return () => listeners.delete(fn);
    },
    drawRow(row) {
      const id = row.id?.value ?? "";
      drawn.push(id);
      const held = elements.get(id);
      if (held !== undefined) return held;
      const el = document.createElement("article");
      el.setAttribute("data-feed-row", id);
      elements.set(id, el);
      return el;
    },
    breadcrumbs: () => [],
    fire: () => listeners.forEach((fn) => fn()),
    drawn,
    ...extra,
  };
}

describe("armName", () => {
  it("answers the arm a oneof selected, for a refusal's message", () => {
    expect(armName({ case: "surprise" })).toBe("surprise");
  });
});

describe("nestSlot", () => {
  it("creates the slot on the first child that needs it", () => {
    const container = document.createElement("article");
    expect(nestSlot(container).hasAttribute(NEST_ATTRIBUTE)).toBe(true);
  });

  it("reuses the slot, so nesting does not multiply it", () => {
    const container = document.createElement("article");
    expect(nestSlot(container)).toBe(nestSlot(container));
  });
});

describe("arrangeSubfeedRows", () => {
  it("lays the rows out in the feed's own order", () => {
    const host = document.createElement("div");
    arrangeSubfeedRows(host, viewOf([userPromptRow("a", "1"), responseRow("b")]));
    expect([...host.children].map((el) => el.getAttribute("data-feed-row"))).toEqual(["a", "b"]);
  });

  it("nests a row inside the container it names", () => {
    const host = document.createElement("div");
    arrangeSubfeedRows(host, viewOf([responseRow("a"), responseRow("b", "x", "a")]));
    expect(host.querySelector(`[data-feed-row="a"] [${NEST_ATTRIBUTE}] [data-feed-row="b"]`)).not.toBeNull();
  });

  it("stops the clocks of a row it drops, so a dropped row cannot tick unseen", () => {
    // Arrange: a laid-out row holding a clock.
    const host = document.createElement("div");
    const rows = [responseRow("a"), responseRow("b")];
    const view = viewOf(rows);
    arrangeSubfeedRows(host, view);
    const dropped = host.querySelector('[data-feed-row="b"]');
    if (dropped === null) throw new Error("fixture drew no row b");
    let ticks = 0;
    tick(dropped, createTicker(1000), () => (ticks += 1));
    // Act: the next arrangement omits it (deletion is row OMISSION).
    rows.pop();
    arrangeSubfeedRows(host, view);
    vi.advanceTimersByTime(5000);
    // Assert: only the immediate first paint ever ran.
    expect(ticks).toBe(1);
  });

  it("keeps the clocks of a row it merely MOVES into a container", () => {
    // Arrange.
    const host = document.createElement("div");
    const rows = [responseRow("a"), responseRow("b")];
    const view = viewOf(rows);
    arrangeSubfeedRows(host, view);
    const moved = host.querySelector('[data-feed-row="b"]');
    if (moved === null) throw new Error("fixture drew no row b");
    let ticks = 0;
    tick(moved, createTicker(1000), () => (ticks += 1));
    // Act: b becomes a's child, which is a move rather than a discard.
    rows[1] = responseRow("b", "x", "a");
    arrangeSubfeedRows(host, view);
    vi.advanceTimersByTime(2000);
    // Assert.
    expect(ticks).toBe(3);
  });

  it("places a row whose container this feed never drew at the top level", () => {
    const host = document.createElement("div");
    arrangeSubfeedRows(host, viewOf([responseRow("b", "x", "missing")]));
    expect(host.children).toHaveLength(1);
  });

  it("lays out a merge tab like any other row when the body is not the strip", () => {
    const host = document.createElement("div");
    // The MERGE body consumes its tabs itself and never reaches this arranger;
    // the default body draws whatever the daemon served rather than dropping a
    // row (src/feed/merge/tab-row.ts).
    arrangeSubfeedRows(host, viewOf([mergeTabRow("t1"), responseRow("a")]));
    expect([...host.children].map((el) => el.getAttribute("data-feed-row"))).toEqual(["t1", "a"]);
  });

  it("drops a row that stopped being in the feed, deletion being omission", () => {
    const host = document.createElement("div");
    const rows = [responseRow("a"), responseRow("b")];
    const view = viewOf(rows);
    arrangeSubfeedRows(host, view);
    rows.splice(1, 1);
    arrangeSubfeedRows(host, view);
    expect(host.children).toHaveLength(1);
  });

  it("places an ID-LESS row whose container this feed never drew at the top level", () => {
    // Arrange: no id at all, so the warning's row name falls back to "unset".
    const host = document.createElement("div");
    const orphan = create(FeedRowSchema, {
      parent: { row: feedId("missing") },
      row: {
        case: "activity",
        value: {
          unit: {
            case: "response",
            value: { result: { case: "success", value: { prose: { markdown: "hi" } } } },
          },
        },
      },
    });
    // Act
    arrangeSubfeedRows(host, viewOf([orphan]));
    // Assert: it is drawn, at the top level, rather than dropped.
    expect(host.children).toHaveLength(1);
  });

  it("clears a stale nested child when its parent stops naming it", () => {
    const host = document.createElement("div");
    const rows = [responseRow("a"), responseRow("b", "x", "a")];
    const view = viewOf(rows);
    arrangeSubfeedRows(host, view);
    rows.splice(1, 1);
    arrangeSubfeedRows(host, view);
    expect(host.querySelector(`[${NEST_ATTRIBUTE}] [data-feed-row="b"]`)).toBeNull();
  });
});

describe("drawBreadcrumbTrail", () => {
  it("draws nothing at a feed's own top (R6)", () => {
    const { ctx } = harness();
    const mount = document.createElement("div");
    const host = document.createElement("div");
    mount.append(host);
    drawBreadcrumbTrail(host, [], rowContext(ctx, userPromptRow("a", "x")), mount);
    // An empty trail is NO trail: the line is taken out of the page rather than
    // left standing empty.
    expect(mount.children).toHaveLength(0);
  });

  it("draws the daemon-resolved labels, outermost first", () => {
    const { ctx } = harness();
    const host = document.createElement("div");
    drawBreadcrumbTrail(
      host,
      [
        create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" }),
        create(FeedBreadcrumbSchema, { target: feedId("i"), label: "inner" }),
      ],
      rowContext(ctx, userPromptRow("a", "x")),
    );
    expect([...host.children].map((el) => el.textContent)).toEqual(["outer", "inner"]);
  });

  it("puts the trail at the top of the mount it was given", () => {
    // Arrange: a detached host and a mount that already holds a row.
    const { ctx } = harness();
    const mount = document.createElement("div");
    const row = document.createElement("p");
    mount.append(row);
    const host = document.createElement("div");
    // Act
    drawBreadcrumbTrail(
      host,
      [create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" })],
      rowContext(ctx, userPromptRow("a", "x")),
      mount,
    );
    // Assert: the header line leads the mount.
    expect([...mount.children]).toEqual([host, row]);
  });

  it("leaves the trail where the caller put it when no mount was named", () => {
    // Arrange
    const { ctx } = harness();
    const host = document.createElement("div");
    // Act: no mount argument at all.
    drawBreadcrumbTrail(
      host,
      [create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" })],
      rowContext(ctx, userPromptRow("a", "x")),
    );
    // Assert: the crumbs are drawn and nothing was attached anywhere.
    expect([host.children.length, host.parentElement]).toEqual([1, null]);
  });

  it("does not move a trail that is already in the page", () => {
    // Arrange: the host sits AFTER another child of the mount.
    const { ctx } = harness();
    const mount = document.createElement("div");
    const first = document.createElement("p");
    const host = document.createElement("div");
    mount.append(first, host);
    // Act
    drawBreadcrumbTrail(
      host,
      [create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" })],
      rowContext(ctx, userPromptRow("a", "x")),
      mount,
    );
    // Assert: it was NOT prepended above the sibling it already followed.
    expect([...mount.children]).toEqual([first, host]);
  });

  it("makes a crumb the daemon gave no target inert", () => {
    // Arrange
    const { ctx } = harness();
    const host = document.createElement("div");
    const revealed: string[] = [];
    drawBreadcrumbTrail(
      host,
      [create(FeedBreadcrumbSchema, { label: "nowhere" })],
      rowContext(ctx, userPromptRow("a", "x"), {
        revealRow: async (id) => {
          revealed.push(id.value);
          return true;
        },
      }),
    );
    // Act
    host.querySelector("button")?.click();
    // Assert: a crumb with nothing to jump to jumps nowhere.
    expect(revealed).toEqual([]);
  });

  it("makes a crumb a jump target rather than a navigation", () => {
    const { ctx } = harness();
    const host = document.createElement("div");
    const revealed: string[] = [];
    drawBreadcrumbTrail(
      host,
      [create(FeedBreadcrumbSchema, { target: feedId("o"), label: "outer" })],
      rowContext(ctx, userPromptRow("a", "x"), {
        revealRow: async (id) => {
          revealed.push(id.value);
          return true;
        },
      }),
    );
    host.querySelector("button")?.click();
    expect(revealed).toEqual(["o"]);
  });
});

describe("defaultBubbleBody", () => {
  it("draws the sub-feed's rows into the mount", () => {
    const { ctx } = harness();
    const mount = document.createElement("div");
    defaultBubbleBody(mount, viewOf([responseRow("a")]), rowContext(ctx, responseRow("a")));
    expect(mount.querySelector('[data-feed-row="a"]')).not.toBeNull();
  });

  it("redraws itself when the rows change, rather than being re-invoked", () => {
    const { ctx } = harness();
    const mount = document.createElement("div");
    const rows = [responseRow("a")];
    const view = viewOf(rows);
    defaultBubbleBody(mount, view, rowContext(ctx, responseRow("a")));
    rows.push(responseRow("b"));
    view.fire();
    expect(mount.querySelectorAll("[data-feed-row]")).toHaveLength(2);
  });

  it("puts the bubble's composer slot at the bottom, when this build has one", () => {
    const { ctx } = harness();
    const mount = document.createElement("div");
    const slot = document.createElement("div");
    slot.className = "composer-slot";
    defaultBubbleBody(
      mount,
      viewOf([responseRow("a")], { composerSlot: slot }),
      rowContext(ctx, responseRow("a")),
    );
    expect(mount.lastElementChild).toBe(slot);
  });

  it("mounts no composer slot when this build has none (production, R7)", () => {
    const { ctx } = harness();
    const mount = document.createElement("div");
    defaultBubbleBody(mount, viewOf([]), rowContext(ctx, responseRow("a")));
    expect(mount.querySelector(".composer-slot")).toBeNull();
  });

  it("stops listening once disposed", () => {
    const { ctx } = harness();
    const mount = document.createElement("div");
    const rows = [responseRow("a")];
    const view = viewOf(rows);
    const handle = defaultBubbleBody(mount, view, rowContext(ctx, responseRow("a")));
    handle.dispose();
    rows.push(responseRow("b"));
    view.fire();
    expect(mount.querySelectorAll("[data-feed-row]")).toHaveLength(0);
  });
});

/**
 * THE REGISTRY IS COMPLETE.
 *
 * The type-level half is the annotation on this list: a key added to
 * `RowRenderers` and missing from it does not compile, and a key here that the
 * interface does not declare does not compile either. The runtime half is the
 * sweep below, which catches the one thing the type cannot — a key declared,
 * typed, and left `undefined` by the assembler.
 */
const REGISTRY_KEYS: readonly (keyof RowRenderers)[] = [
  "response",
  "simpleToolCall",
  "hook",
  "skill",
  "artifact",
  "plan",
  "findings",
  "shell",
  "permission",
  "question",
  "coldGate",
  "mergeHead",
  "commandPanel",
  "commandRefused",
  "mergeBody",
];

describe("createRowRenderers", () => {
  for (const key of REGISTRY_KEYS) {
    it(`fills the ${key} seam`, () => {
      const h = harness();
      expect(typeof createRowRenderers(h.ctx)[key]).toBe("function");
    });
  }

  it("fills no seam the interface does not declare", () => {
    const h = harness();
    expect(Object.keys(createRowRenderers(h.ctx)).sort()).toEqual([...REGISTRY_KEYS].sort());
  });
});
