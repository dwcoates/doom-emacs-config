// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import { TopbarViewSchema, type TopbarView } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import STYLESHEET from "../../src/styles.css?raw";
import { drawTopbarView, mountTopbar } from "../../src/topbar/topbar.js";
import { GEOMETRY, NOW, RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";

/** A complete view; each test overrides only what it is about. */
function view(overrides: Partial<TopbarView> = {}): TopbarView {
  const base = create(TopbarViewSchema, {
    title: { text: "DWC/fix" },
    sessionLine: { text: "session abc" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "green", glyph: "●", title: "connected" },
    modelSelector: { options: [{ model: { name: "opus" }, displayName: "Opus" }] },
    permissionModePicker: {
      current: { mode: "default", displayName: "default" },
      options: [{ mode: "plan", displayName: "plan" }],
    },
    context: { text: "142.3k", breakdown: { sections: [] } },
    warnings: { warnings: [] },
  });
  return { ...base, ...overrides };
}

/**
 * A PARKED workspace's view: the three workspace-scoped elements the daemon
 * can still resolve, and the hibernated state. Nothing session-scoped is set,
 * which is what the state means.
 */
function hibernatedView(): TopbarView {
  return create(TopbarViewSchema, {
    title: { text: "DWC/fix" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "none", glyph: "○", title: "no session" },
    hibernated: { sinceMs: BigInt(NOW - 60_000) },
  });
}

/**
 * A COLD-GATED workspace's view: the same three workspace-scoped elements, and
 * the cold-gate state. Nothing session-scoped is set, because the shim
 * answered `cold` and no session was ever created.
 */
function coldGateView(): TopbarView {
  return create(TopbarViewSchema, {
    title: { text: "DWC/fix" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "none", glyph: "○", title: "no session" },
    coldGate: { contextTokens: 142_300n, sinceMs: BigInt(NOW - 60_000) },
  });
}

describe("drawTopbarView", () => {
  it("draws the three groups", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view(), tc);
    expect([
      row.querySelector(".topbar-left") !== null,
      row.querySelector(".topbar-title") !== null,
      row.querySelector(".topbar-right") !== null,
    ]).toEqual([true, true, true]);
  });

  // THE HIBERNATED STRIP. A parked workspace's view carries the account, the
  // connectivity and the title and nothing session-scoped, because the daemon
  // has no session to resolve those from.
  it("draws the hibernated cell for a parked workspace", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(hibernatedView(), tc);
    expect(row.querySelector(".topbar-hibernated")?.textContent).toBe("hibernated 1m");
  });

  it("draws the account, the connectivity and the title for a parked workspace", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(hibernatedView(), tc);
    expect([
      row.querySelector(".topbar-account") !== null,
      row.querySelector(".topbar-connectivity") !== null,
      row.querySelector(".topbar-title") !== null,
    ]).toEqual([true, true, true]);
  });

  it("draws no session-scoped element for a parked workspace", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(hibernatedView(), tc);
    expect([
      row.querySelector(".topbar-model") !== null,
      row.querySelector(".topbar-permission-mode") !== null,
      row.querySelector(".topbar-context-figure") !== null,
      row.querySelector(".topbar-fast") !== null,
    ]).toEqual([false, false, false, false]);
  });

  // THE COLD-GATE STRIP. A workspace standing at the gate resolves no session
  // fact at all, so before this branch the strip published nothing and stayed
  // blank for as long as the gate stood.
  it("draws the cold-gate cell for a workspace standing at the gate", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(coldGateView(), tc);
    expect(row.querySelector(".topbar-cold-gate")?.textContent).toBe("cold context 142.3k 1m");
  });

  it("draws the account, the connectivity and the title for a cold-gated workspace", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(coldGateView(), tc);
    expect([
      row.querySelector(".topbar-account") !== null,
      row.querySelector(".topbar-connectivity") !== null,
      row.querySelector(".topbar-title") !== null,
    ]).toEqual([true, true, true]);
  });

  it("draws no session-scoped element for a cold-gated workspace", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(coldGateView(), tc);
    expect([
      row.querySelector(".topbar-model") !== null,
      row.querySelector(".topbar-permission-mode") !== null,
      row.querySelector(".topbar-context-figure") !== null,
      row.querySelector(".topbar-fast") !== null,
    ]).toEqual([false, false, false, false]);
  });

  it("draws no fast-mode cell when the vendor has stated no fast mode", () => {
    const { tc } = topbarContext();
    expect(drawTopbarView(view(), tc).querySelector(".topbar-fast")).toBeNull();
  });

  it("draws the fast-mode cell beside the permission-mode picker", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(
      view({
        fastMode: {
          $typeName: "frontend.v1.TopbarFastMode",
          state: { case: "on", value: {} },
        } as never,
      }),
      tc,
    );
    const right = row.querySelector(".topbar-right");
    const cells = [...(right?.children ?? [])].map((el) => el.className);
    expect(cells.indexOf("topbar-fast")).toBe(cells.indexOf("topbar-mode") + 1);
  });

  it("draws no warning chip when the daemon reports nothing wrong", () => {
    const { tc } = topbarContext();
    expect(drawTopbarView(view(), tc).querySelector(".topbar-warning-chip")).toBeNull();
  });

  it("draws the warning chip when the list is non-empty", () => {
    const { tc } = topbarContext();
    const drawn = drawTopbarView(
      view({
        warnings: create(TopbarViewSchema, {
          warnings: {
            warnings: [{ line: { text: "a" }, detail: { case: "accounting", value: { lines: [] } } }],
          },
        }).warnings,
      }),
      tc,
    );
    expect(drawn.querySelector(".topbar-warning-chip")).not.toBeNull();
  });

  it("binds the session reveal to a logged-in chip", () => {
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(view(), tc));
    host
      .querySelector(".topbar-account")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  it("gives a logged-out chip the login, not the session reveal", () => {
    // ARRANGE
    const openLogin = vi.fn();
    const { host, tc } = topbarContext(undefined, openLogin);
    host.append(
      drawTopbarView(
        view({ account: create(TopbarViewSchema, { account: { state: { case: "loggedOut", value: {} } } }).account }),
        tc,
      ),
    );
    // ACT
    host.querySelector(".topbar-account")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect([openLogin.mock.calls.length, openPanel(host)]).toEqual([1, null]);
  });

  it("refuses a view missing a required element message", () => {
    const { tc } = topbarContext();
    expect(() => drawTopbarView(create(TopbarViewSchema, {}), tc)).toThrow(MalformedView);
  });
});

describe("mountTopbar", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  /** Let the router transport's zero-delay frames land. */
  async function settle(): Promise<void> {
    for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
  }

  /** A daemon whose WatchTopbar yields VIEWS then stands. */
  function daemon(views: TopbarView[]) {
    return appContext({
      watchTopbar: async function* () {
        for (const topbar of views) yield create(WatchTopbarResponseSchema, { topbar });
        await new Promise<never>(() => undefined);
      },
    });
  }

  it("draws the strip from the first push", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    // ACT
    const handle = mountTopbar(host, daemon([view()]), {
      openLogin: () => undefined,
      geometry: GEOMETRY,
    });
    await settle();
    // ASSERT
    expect(host.querySelector(".topbar-title")?.textContent).toBe("DWC/fix");
    handle.dispose();
  });

  it("replaces the strip whole on the next push", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const second = view({ title: create(TopbarViewSchema, { title: { text: "other" } }).title });
    // ACT
    const handle = mountTopbar(host, daemon([view(), second]), {
      openLogin: () => undefined,
      geometry: GEOMETRY,
    });
    await settle();
    // ASSERT
    expect(host.querySelectorAll(".topbar-title").length).toBe(1);
    expect(host.querySelector(".topbar-title")?.textContent).toBe("other");
    handle.dispose();
  });

  it("keeps an open reveal across a push, with the new push's content", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const ctx = appContext({
      watchTopbar: async function* () {
        yield create(WatchTopbarResponseSchema, { topbar: view() });
        await new Promise((resolve) => setTimeout(resolve, 50));
        yield create(WatchTopbarResponseSchema, {
          topbar: view({ sessionLine: create(TopbarViewSchema, { sessionLine: { text: "session xyz" } }).sessionLine }),
        });
        await new Promise<never>(() => undefined);
      },
    });
    const handle = mountTopbar(host, ctx, { openLogin: () => undefined, geometry: GEOMETRY });
    await settle();
    host.querySelector(".topbar-account")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ACT
    await vi.advanceTimersByTimeAsync(50);
    await settle();
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session xyz");
    handle.dispose();
  });

  it("reports a malformed push as an unreadable frame rather than tearing down", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const sink = new RecordingSink();
    const ctx = appContext(
      {
        watchTopbar: async function* () {
          yield create(WatchTopbarResponseSchema, {});
          await new Promise<never>(() => undefined);
        },
      },
      sink,
    );
    // ACT
    const handle = mountTopbar(host, ctx, { openLogin: () => undefined, geometry: GEOMETRY });
    await settle();
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toContain("frameUndecodable");
    handle.dispose();
  });

  it("empties the host on dispose", async () => {
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountTopbar(host, daemon([view()]), {
      openLogin: () => undefined,
      geometry: GEOMETRY,
    });
    await settle();
    handle.dispose();
    expect(host.children.length).toBe(0);
  });
});

/**
 * THE STRIP'S LAYOUT (owner rulings 1-4, 2026-09-13).
 *
 * jsdom resolves the cascade but lays nothing out, so a layout claim is
 * asserted where it is DECIDED — the declarations in `src/styles.css` and the
 * DOM order `drawTopbarView` builds — rather than by measuring boxes that are
 * all zero here. The stylesheet is read raw for the same reason
 * `test/feed/rows/separation.test.ts` reads it: the token a rule was written
 * with is the assertion, and jsdom would hand back a resolved-away shorthand.
 */
function ruleBody(selector: string): string {
  const start = STYLESHEET.indexOf(`\n${selector} {`);
  expect(start).toBeGreaterThan(-1);
  const open = STYLESHEET.indexOf("{", start);
  const close = STYLESHEET.indexOf("}", open);
  return STYLESHEET.slice(open + 1, close);
}

/** The value of DECLARATION in SELECTOR's block, comments stripped. */
function declaration(selector: string, property: string): string {
  const body = ruleBody(selector).replace(/\/\*[\s\S]*?\*\//g, "");
  const match = new RegExp(`(?:^|;|\\n)\\s*${property}\\s*:([^;]*);`).exec(body);
  expect(match).not.toBeNull();
  return (match?.[1] ?? "").trim();
}

describe("the strip's layout", () => {
  // RULING 1. Not "both are 0.5rem" — both are the SAME TOKEN, which is what
  // keeps them from drifting apart the next time one of them is tuned.
  it("pads the strip's edges with the very token that gaps its cells", () => {
    // ARRANGE / ACT
    const padding = declaration(".topbar-row", "padding");
    const gap = declaration(".topbar-row", "gap");
    // ASSERT
    expect(padding).toBe(`0 ${gap}`);
  });
});

/**
 * THE STRIP'S GEOMETRY, WORKED FROM THE TRACKS THE STYLESHEET DECLARES.
 *
 * jsdom lays out nothing, so the boxes are stubbed and placed here by the
 * rule `grid-template-columns: 1fr minmax(0, auto) 1fr` states: two equal
 * free tracks either side of a content-sized middle one, each free track
 * floored at its own content (an `fr` track keeps an auto minimum) and the
 * middle one clipped when what is left is less than it wants. The template
 * itself is asserted alongside, so a change to it fails these tests rather
 * than quietly leaving them measuring a layout the app no longer has.
 */
interface StubWidths {
  readonly row: number;
  readonly padding: number;
  readonly gap: number;
  readonly left: number;
  readonly right: number;
  readonly title: number;
}

interface Placed {
  readonly left: { start: number; width: number };
  readonly title: { start: number; width: number };
  readonly right: { start: number; width: number };
}

function placeTracks(w: StubWidths): Placed {
  const inner = w.row - 2 * w.padding - 2 * w.gap;
  const share = (inner - w.title) / 2;
  // THE GROUPS NEVER SHRINK: each free track is at least its own content.
  const left = Math.max(w.left, share);
  const right = Math.max(w.right, share);
  // ...so the title is what gives, down to nothing.
  const title = Math.min(w.title, Math.max(0, inner - left - right));
  const titleStart = w.padding + left + w.gap;
  return {
    left: { start: w.padding, width: left },
    title: { start: titleStart, width: title },
    right: { start: titleStart + title + w.gap, width: right },
  };
}

describe("the strip's geometry", () => {
  it("lays the row out in three tracks whose outer two are the same free size", () => {
    expect(declaration(".topbar-row", "grid-template-columns")).toBe("1fr minmax(0, auto) 1fr");
  });

  // RULING 2. The title is centered on ITS OWN CONTENT against the whole
  // strip: a wide right group and a narrow left one move it not at all.
  it("centers the title on the whole strip with unequal left and right groups", () => {
    // ARRANGE
    const w: StubWidths = { row: 1000, padding: 8, gap: 8, left: 120, right: 340, title: 60 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.title.start + placed.title.width / 2).toBe(w.row / 2);
  });

  // THE OVERFLOW RULE, stated as the ruling states it: the title clips, the
  // groups keep every pixel they asked for.
  it("clips a title too wide for the free space without shrinking either group", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 200, right: 300, title: 400 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect([placed.left.width, placed.right.width, placed.title.width]).toEqual([200, 300, 68]);
  });

  it("ellipsis-clips the title rather than letting it wrap or spill", () => {
    expect([
      declaration(".topbar-title", "min-width"),
      declaration(".topbar-title", "overflow"),
      declaration(".topbar-title", "text-overflow"),
      declaration(".topbar-title", "white-space"),
    ]).toEqual(["0", "hidden", "ellipsis", "nowrap"]);
  });

  it("hangs each flank group on its own edge of the strip", () => {
    expect([
      declaration(".topbar-left", "justify-self"),
      declaration(".topbar-right", "justify-self"),
    ]).toEqual(["start", "end"]);
  });
});

describe("the account cell", () => {
  // RULING 3. The glyph qualifies the label, so it reads BEFORE it — and in
  // the same cell, which is what makes the pair one thing to point at.
  it("draws the connectivity glyph before the account label inside one cell", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const cell = drawTopbarView(view(), tc).querySelector(".topbar-account-cell");
    // ASSERT
    expect([...(cell?.children ?? [])].map((el) => el.className.split(" ")[0])).toEqual([
      "topbar-connectivity",
      "topbar-account",
    ]);
  });

  it("stands the glyph closer to its label than two cells of the strip stand apart", () => {
    expect(declaration(".topbar-account-cell", "gap")).toBe("calc(var(--topbar-cell-gap) / 2)");
  });

  // ONE ELEMENT MEANS ONE ANCHOR: the session line hangs under the pair.
  it("anchors the session-line reveal on the pair rather than on the label alone", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const row = drawTopbarView(view(), tc);
    // ASSERT
    expect([
      row.querySelector(".topbar-account-cell")?.getAttribute("data-reveal-anchor"),
      row.querySelector(".topbar-account")?.getAttribute("data-reveal-anchor"),
    ]).toEqual(["session", null]);
  });
});

describe("the whole-view states", () => {
  // RULING 4. A state that replaces the session-scoped controls stands where
  // they stood: the LAST cell of the right group, at the strip's right edge.
  it("draws the hibernated cell as the right group's own content", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const right = drawTopbarView(hibernatedView(), tc).querySelector(".topbar-right");
    // ASSERT
    expect([...(right?.children ?? [])].map((el) => el.className)).toEqual(["topbar-hibernated"]);
  });

  it("draws the cold-gate cell as the right group's own content", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const right = drawTopbarView(coldGateView(), tc).querySelector(".topbar-right");
    // ASSERT
    expect([...(right?.children ?? [])].map((el) => el.className)).toEqual(["topbar-cold-gate"]);
  });

  // ...and in the right cells' box, so the reader sees the right group's
  // content rather than a stray label.
  it("boxes both state cells exactly as the strip's other right-hand cells", () => {
    expect(declaration(".topbar-fast,\n.topbar-hibernated,\n.topbar-cold-gate", "padding")).toBe(
      declaration(
        ".topbar-model-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-warning-chip",
        "padding",
      ),
    );
  });
});
