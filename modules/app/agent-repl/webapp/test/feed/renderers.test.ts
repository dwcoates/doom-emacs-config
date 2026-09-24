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
import {
  feedId,
  harness,
  mergeTabRow,
  responseRow,
  rowContext,
  subagentRow,
  toolCallRow,
  userPromptRow,
} from "./harness.js";
import {
  GROUP_TAB_MEMBER_ATTRIBUTE,
  createToolGroupStore,
} from "../../src/feed/tool-group.js";
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

/**
 * THE ARRANGE PASS NEVER RE-ATTACHES A ROW ALREADY IN PLACE (owner rule,
 * 2026-09-23: the user owns the scroll). A re-attached element loses the
 * reader's scroll position inside it, and the pass runs on every live push.
 * jsdom lays nothing out, so the invariant is asserted on the DOM itself: the
 * mutation records name no removed row.
 */
describe("arrangeSubfeedRows leaves placed rows in the document", () => {
  /** Every row a mutation batch on ROOT's subtree removed, by id. */
  function removedRows(observer: MutationObserver): string[] {
    return observer
      .takeRecords()
      .flatMap((record) => [...record.removedNodes])
      .filter((node): node is Element => node instanceof Element && node.hasAttribute("data-feed-row"))
      .map((node) => node.getAttribute("data-feed-row") ?? "");
  }

  /** Watch HOST's whole subtree's child lists. */
  function watch(host: HTMLElement): MutationObserver {
    const observer = new MutationObserver(() => undefined);
    observer.observe(host, { childList: true, subtree: true });
    return observer;
  }

  it("removes no top-level row when a row is appended", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [responseRow("a"), responseRow("b")];
    const view = viewOf(rows);
    arrangeSubfeedRows(host, view);
    const observer = watch(host);
    // Act
    rows.push(responseRow("c"));
    arrangeSubfeedRows(host, view);
    // Assert
    expect(removedRows(observer)).toEqual([]);
  });

  it("removes no row when nothing changed", () => {
    // Arrange -- a push that only redrew a row inside its own chrome.
    const host = document.createElement("div");
    const view = viewOf([responseRow("a"), responseRow("b")]);
    arrangeSubfeedRows(host, view);
    const observer = watch(host);
    // Act
    arrangeSubfeedRows(host, view);
    // Assert
    expect(removedRows(observer)).toEqual([]);
  });

  it("removes no nested row when its container is re-arranged", () => {
    // Arrange
    const host = document.createElement("div");
    const view = viewOf([responseRow("a"), responseRow("b", "x", "a")]);
    arrangeSubfeedRows(host, view);
    const observer = watch(host);
    // Act
    arrangeSubfeedRows(host, view);
    // Assert
    expect(removedRows(observer)).toEqual([]);
  });

  it("removes no grouped member when its group is re-arranged", () => {
    // Arrange
    const host = document.createElement("div");
    const groups = createToolGroupStore();
    const view = viewOf([toolCallRow("t1", "running"), toolCallRow("t2", "running")]);
    arrangeSubfeedRows(host, view, groups);
    if (host.querySelector(".feed-group") === null) throw new Error("the fixture formed no group");
    const observer = watch(host);
    // Act
    arrangeSubfeedRows(host, view, groups);
    // Assert
    expect(removedRows(observer)).toEqual([]);
  });

  it("empties a nesting slot whose row left the feed", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [responseRow("a"), responseRow("b", "x", "a")];
    const view = viewOf(rows);
    arrangeSubfeedRows(host, view);
    // Act
    rows.pop();
    arrangeSubfeedRows(host, view);
    // Assert
    expect(host.querySelector('[data-feed-row="b"]')).toBeNull();
  });
});

describe("arrangeSubfeedRows tabbed grouping", () => {
  it("batches a run of >=2 same-kind tool cards into ONE tabbed container", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [toolCallRow("a", "returned"), toolCallRow("b", "returned")];
    // Act
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // Assert: one top-level child, a group with two tabs.
    expect(host.children).toHaveLength(1);
    expect(host.querySelector(".feed-group")).not.toBeNull();
    expect(host.querySelectorAll(".feed-group-tab")).toHaveLength(2);
  });

  it("does not leave grouped members as separate TOP-LEVEL bubbles", () => {
    const host = document.createElement("div");
    const rows = [toolCallRow("a", "returned"), toolCallRow("b", "returned")];
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // The members are inside the group's panel, not direct children of the host.
    expect(host.querySelector(':scope > [data-feed-row="a"]')).toBeNull();
    expect(host.querySelector('.feed-group-panel [data-feed-row="a"]')).not.toBeNull();
  });

  it("leaves a LONE tool card ungrouped, rendered exactly as today", () => {
    const host = document.createElement("div");
    arrangeSubfeedRows(host, viewOf([toolCallRow("a", "returned")]), createToolGroupStore());
    expect(host.querySelector(".feed-group")).toBeNull();
    expect(host.querySelector(':scope > [data-feed-row="a"]')).not.toBeNull();
  });

  it("breaks a run on a KIND change (Bash,Bash,Edit,Bash)", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [
      toolCallRow("a", "returned", { tool: "Bash" }),
      toolCallRow("b", "returned", { tool: "Bash" }),
      toolCallRow("c", "returned", { tool: "Edit" }),
      toolCallRow("d", "returned", { tool: "Bash" }),
    ];
    // Act
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // Assert: one group of two Bash, then a lone Edit, then a lone Bash.
    expect(host.children).toHaveLength(3);
    expect(host.children[0].querySelectorAll(".feed-group-tab")).toHaveLength(2);
    expect(host.children[1].getAttribute("data-feed-row")).toBe("c");
    expect(host.children[2].getAttribute("data-feed-row")).toBe("d");
  });

  it("breaks a run when a response bubble sits between two same-kind cards", () => {
    const host = document.createElement("div");
    const rows = [
      toolCallRow("a", "returned"),
      responseRow("mid"),
      toolCallRow("b", "returned"),
    ];
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // No group forms; three lone top-level rows.
    expect(host.querySelector(".feed-group")).toBeNull();
    expect(host.children).toHaveLength(3);
  });

  it("breaks a run at a turn boundary with no row between the two turns' cards", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [
      toolCallRow("a", "returned", { turn: "t1" }),
      toolCallRow("b", "returned", { turn: "t1" }),
      toolCallRow("c", "returned", { turn: "t2" }),
      toolCallRow("d", "returned", { turn: "t2" }),
    ];
    // Act
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // Assert: one group per turn, never one across them.
    expect([...host.children].map((child) => child.querySelectorAll(".feed-group-tab").length)).toEqual([2, 2]);
  });

  it("breaks a run between a card of a turn and a card of no turn", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [toolCallRow("a", "returned", { turn: "t1" }), toolCallRow("b", "returned")];
    // Act
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // Assert
    expect(host.querySelector(".feed-group")).toBeNull();
  });

  it("still groups a run of cards that all carry no turn", () => {
    // Arrange
    const host = document.createElement("div");
    const rows = [toolCallRow("a", "returned"), toolCallRow("b", "returned")];
    // Act
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    // Assert
    expect(host.querySelectorAll(".feed-group-tab")).toHaveLength(2);
  });

  it("groups a run of subagent bubbles into tabs", () => {
    const host = document.createElement("div");
    const rows = [subagentRow("a"), subagentRow("b"), subagentRow("c")];
    arrangeSubfeedRows(host, viewOf(rows), createToolGroupStore());
    expect(host.querySelectorAll(".feed-group-tab")).toHaveLength(3);
  });

  it("never groups without a store (a fixture exercising the arranger alone)", () => {
    const host = document.createElement("div");
    const rows = [toolCallRow("a", "returned"), toolCallRow("b", "returned")];
    arrangeSubfeedRows(host, viewOf(rows));
    expect(host.querySelector(".feed-group")).toBeNull();
    expect(host.children).toHaveLength(2);
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

  it("adds a tab live when a same-kind card extends the run", () => {
    // Arrange: a two-card Bash run, drawn once.
    const { ctx } = harness();
    const mount = document.createElement("div");
    const rows = [toolCallRow("a", "returned"), toolCallRow("b", "returned")];
    const view = viewOf(rows);
    defaultBubbleBody(mount, view, rowContext(ctx, responseRow("root")));
    expect(mount.querySelectorAll(".feed-group-tab")).toHaveLength(2);
    // Act: a third same-kind card streams in.
    rows.push(toolCallRow("c", "returned"));
    view.fire();
    // Assert: the group grew a tab rather than spawning a second bubble.
    expect(mount.querySelectorAll(".feed-group")).toHaveLength(1);
    expect(mount.querySelectorAll(".feed-group-tab")).toHaveLength(3);
  });

  it("preserves the reader's selected tab across a live append", () => {
    // Arrange: the reader selects the first tab of a two-card run.
    const { ctx } = harness();
    const mount = document.createElement("div");
    const rows = [toolCallRow("a", "returned"), toolCallRow("b", "returned")];
    const view = viewOf(rows);
    defaultBubbleBody(mount, view, rowContext(ctx, responseRow("root")));
    mount.querySelector<HTMLButtonElement>(`[${GROUP_TAB_MEMBER_ATTRIBUTE}="a"]`)?.click();
    // Act: a third card arrives.
    rows.push(toolCallRow("c", "returned"));
    view.fire();
    // Assert: member a stays the shown one, not yanked onto the newest.
    const shown = mount.querySelector('.feed-group-panel [data-feed-row="a"]') as HTMLElement;
    const newest = mount.querySelector('.feed-group-panel [data-feed-row="c"]') as HTMLElement;
    expect([shown.hidden, newest.hidden]).toEqual([false, true]);
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
  "shellHead",
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
