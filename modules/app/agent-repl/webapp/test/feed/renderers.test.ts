// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedBreadcrumbSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
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

  it("places a row whose container this feed never drew at the top level", () => {
    const host = document.createElement("div");
    arrangeSubfeedRows(host, viewOf([responseRow("b", "x", "missing")]));
    expect(host.children).toHaveLength(1);
  });

  it("skips merge tabs, which are the merge body's and not rows of their own", () => {
    const host = document.createElement("div");
    arrangeSubfeedRows(host, viewOf([mergeTabRow("t1"), responseRow("a")]));
    expect([...host.children].map((el) => el.getAttribute("data-feed-row"))).toEqual(["a"]);
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
    const host = document.createElement("div");
    drawBreadcrumbTrail(host, [], rowContext(ctx, userPromptRow("a", "x")));
    expect(host.hidden).toBe(true);
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
