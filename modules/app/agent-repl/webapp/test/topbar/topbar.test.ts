// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import { TopbarViewSchema, type TopbarView } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import STYLESHEET from "../../src/styles.css?raw";
import { drawTopbarView, mountTopbar } from "../../src/topbar/topbar.js";
import { GEOMETRY, RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";

/** A complete view; each test overrides only what it is about. */
function view(overrides: Partial<TopbarView> = {}): TopbarView {
  const base = create(TopbarViewSchema, {
    title: { text: "DWC/fix" },
    sessionLine: { text: "session abc" },
    account: {
      state: { case: "loggedIn", value: { email: "a@b.test" } },
      options: [
        { configDir: "/root/.claude", current: true, state: { case: "loggedIn", value: { email: "a@b.test" } } },
      ],
    },
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
 * A SESSION-LESS workspace's view: the cells the daemon can always resolve,
 * and NOT ONE of the three session-scoped controls. The context chip and the
 * warning strip are still set — always — and they are where the state's own
 * facts ride (topbar.proto, FIXED SCHEMA AND ORGANIZATION).
 */
function sessionlessView(reason: string, context = "0"): TopbarView {
  return create(TopbarViewSchema, {
    title: { text: "DWC/fix" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "none", glyph: "○", title: "no session" },
    context: { text: context, breakdown: { sections: [{ heading: { text: reason }, rows: [] }] } },
    warnings: { warnings: [{ line: { text: reason } }] },
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

  // THE SESSION-LESS STRIP. It is the SAME strip: every slot is filled, the
  // three controls with a dash and the chip and the warning strip with the
  // state's own facts. Two whole-view states used to replace the right-hand
  // group here; the fixed-schema ruling retired both.
  it("draws every cell for a workspace with no session", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(sessionlessView("hibernated since 14:03"), tc);
    expect([
      row.querySelector(".topbar-account") !== null,
      row.querySelector(".topbar-connectivity") !== null,
      row.querySelector(".topbar-title") !== null,
      row.querySelector(".topbar-model") !== null,
      row.querySelector(".topbar-mode") !== null,
      row.querySelector(".topbar-fast") !== null,
      row.querySelector(".topbar-context") !== null,
    ]).toEqual([true, true, true, true, true, true, true]);
  });

  it("draws each absent control as a dash in its own slot", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(sessionlessView("hibernated since 14:03"), tc);
    expect([...row.querySelectorAll("[data-no-session]")].map((el) => el.getAttribute("data-no-session")))
      .toEqual(["model", "mode", "fast"]);
  });

  it("keeps the cells in one order whether or not there is a session", () => {
    // THE STRIP DOES NOT REARRANGE ITSELF under the reader; that is the whole
    // point of the fixed schema.
    const { tc } = topbarContext();
    const classOf = (v: TopbarView): string[] =>
      [...(drawTopbarView(v, tc).querySelector(".topbar-right")?.children ?? [])].map((el) =>
        el.className.split(" ")[0],
      );
    const warned = view({
      warnings: create(TopbarViewSchema, {
        warnings: { warnings: [{ line: { text: "a" }, detail: { case: "accounting", value: { lines: [] } } }] },
      }).warnings,
    });
    expect(classOf(sessionlessView("hibernated since 14:03"))).toEqual(classOf(warned));
  });

  it("draws the context figure for a workspace with no session", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(sessionlessView("cold context, awaiting your answer", "101.1k"), tc);
    expect(row.querySelector(".topbar-context-figure")?.textContent).toBe("101.1k");
  });

  it("draws the state as a warning line rather than losing it", () => {
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(sessionlessView("cold context, awaiting your answer"), tc));
    host.querySelector(".topbar-warnings")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.textContent).toBe("cold context, awaiting your answer");
  });

  it("draws the fast-mode cell as a dash when the vendor has stated no fast mode", () => {
    const { tc } = topbarContext();
    expect(
      drawTopbarView(view(), tc).querySelector(".topbar-fast")?.getAttribute("data-no-session"),
    ).toBe("fast");
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

  // THE CELL'S CLICK IS THE LOGIN OPTIONS, in both arms (owner ruling,
  // 2026-09-13). It used to open the session line when logged in — a different
  // fact, and usually an empty one — and the login overlay when logged out.
  it("opens the account options from a logged-in cell", () => {
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(view(), tc));
    host
      .querySelector(".topbar-account")!
      .dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.querySelector("[data-account-option]")).not.toBeNull();
  });

  it("opens the same options from a logged-out cell rather than the login itself", () => {
    // ARRANGE
    const openLogin = vi.fn();
    const { host, tc } = topbarContext(undefined, openLogin);
    host.append(
      drawTopbarView(
        view({
          account: create(TopbarViewSchema, {
            account: {
              state: { case: "loggedOut", value: {} },
              options: [{ configDir: "/root/.claude", current: true, state: { case: "loggedOut", value: {} } }],
            },
          }).account,
        }),
        tc,
      ),
    );
    // ACT
    host.querySelector(".topbar-account")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect([openLogin.mock.calls.length, openPanel(host)?.querySelector("[data-account-option]")])
      .toEqual([0, expect.anything()]);
  });

  it("keeps the session line on the title, which carries it in every account state", () => {
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(view(), tc));
    host.querySelector(".topbar-title")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(openPanel(host)?.textContent).toBe("session abc");
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
    // The TITLE is the session line's anchor; the account cell's own click is
    // the login options.
    host.querySelector(".topbar-title")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
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

/** The rem value of the root custom property NAME, as declared. */
function token(name: string): number {
  const match = new RegExp(`\\n\\s*${name}\\s*:\\s*([\\d.]+)rem\\s*;`).exec(STYLESHEET);
  expect(match).not.toBeNull();
  return Number.parseFloat(match?.[1] ?? "NaN");
}

describe("the strip's layout", () => {
  // RULING 1. Not "both are 0.5rem" — both are the SAME TOKEN, which is what
  // keeps them from drifting apart the next time one of them is tuned.
  it("pads the strip's edges with the very token that gaps its tracks", () => {
    // ARRANGE / ACT
    const padding = declaration(".topbar-row", "padding");
    const gap = declaration(".topbar-row", "gap");
    // ASSERT
    expect(padding).toBe(`0 ${gap}`);
  });

  // THE 2026-09-14 RULING: "a reasonable, and tight spacing between chips, and
  // ALL extra space to be around the center title section". The chips run on
  // their own tighter token while the frame — edge inset and track gap — keeps
  // the cell gap, so the room the chips give back falls to the title.
  it("gaps the chips inside a flank with a tighter token than the row's frame", () => {
    // ARRANGE / ACT
    const flanks = [declaration(".topbar-left", "gap"), declaration(".topbar-right", "gap")];
    const row = declaration(".topbar-row", "gap");
    // ASSERT
    expect([...flanks, row]).toEqual([
      "var(--topbar-chip-gap)",
      "var(--topbar-chip-gap)",
      "var(--topbar-cell-gap)",
    ]);
  });

  // THE 2026-09-14 MEASUREMENT: `header#topbar` spanned x 423-2036 while the
  // row inside it spanned only 423-1009. The header's one in-flow child is a
  // flex item, and a flex item with no width rule shrink-wraps to its content,
  // so the row's two free tracks were dividing the content width instead of
  // the header's. These two declarations are what stop a future edit
  // shrink-wrapping it again.
  it("spans the header rather than shrink-wrapping to its content", () => {
    // ARRANGE / ACT
    const width = declaration(".topbar-row", "width");
    // ASSERT
    expect(width).toBe("100%");
  });

  it("lets the strip between the header and the row take the header's width", () => {
    // ARRANGE / ACT
    const grow = declaration(".topbar-strip", "flex");
    // ASSERT
    expect(grow).toBe("1 1 0");
  });

  it("sets the chip gap tighter than the frame measure it was split from", () => {
    // ARRANGE / ACT
    const chip = token("--topbar-chip-gap");
    const cell = token("--topbar-cell-gap");
    // ASSERT
    expect(chip).toBeLessThan(cell);
  });
});

/**
 * THE STRIP'S GEOMETRY, WORKED FROM THE TRACKS THE STYLESHEET DECLARES.
 *
 * jsdom lays out nothing, so the boxes are stubbed and placed here by the rule
 * `grid-template-columns: minmax(0, 1fr) fit-content(50%) minmax(0, 1fr)`
 * states: a content-sized middle track capped at half the row's content box,
 * and two free tracks with NO content floor of their own splitting whatever is
 * left, evenly. The zero floor is the correction the owner's report forced — a
 * bare `1fr` is `minmax(auto, 1fr)`, and the right group's own min-content
 * floored the right track above the left one's share, which is exactly how the
 * title came to sit against the chips. The template itself is asserted
 * alongside, so a change to it fails these tests rather than quietly leaving
 * them measuring a layout the app no longer has.
 */
interface StubWidths {
  readonly row: number;
  readonly padding: number;
  readonly gap: number;
  /** What each group WANTS; below its track it compresses (`min-width: 0`). */
  readonly left: number;
  readonly right: number;
  /** What the title wants; the cap and the free space are what it gets. */
  readonly title: number;
}

interface Placed {
  readonly left: { start: number; width: number };
  readonly title: { start: number; width: number };
  readonly right: { start: number; width: number };
}

function placeTracks(w: StubWidths): Placed {
  // `fit-content(50%)` measures the percentage against the CONTENT BOX; the
  // gaps come off the space the tracks then divide.
  const content = w.row - 2 * w.padding;
  const inner = content - 2 * w.gap;
  const title = Math.max(0, Math.min(w.title, content / 2, inner));
  // THE FREE TRACKS HAVE NO FLOOR: each is exactly half of what is left, and
  // a group wider than its track compresses into it rather than spilling.
  const share = Math.max(0, (inner - title) / 2);
  const titleStart = w.padding + share + w.gap;
  return {
    left: { start: w.padding, width: share },
    title: { start: titleStart, width: title },
    right: { start: titleStart + title + w.gap, width: share },
  };
}

describe("the strip's geometry", () => {
  it("lays the row out in three tracks whose outer two are free and floorless", () => {
    expect(declaration(".topbar-row", "grid-template-columns")).toBe(
      "minmax(0, 1fr) fit-content(50%) minmax(0, 1fr)",
    );
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

  // THE DEFECT ITSELF. A right group wider than the free share used to floor
  // its own track and leave the left one on the share; both tracks are the
  // same size now, whatever either group holds.
  it("keeps the flank tracks equal when the right group outgrows its share", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 90, right: 320, title: 240 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.left.width).toBe(placed.right.width);
  });

  // ...and the title stays centered in that same strip, which is what the
  // equal tracks were for.
  it("centers the title even when the right group outgrows its share", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 90, right: 320, title: 240 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.title.start + placed.title.width / 2).toBe(w.row / 2);
  });

  // THE CLAMP. The middle track is sized before the free ones, so an
  // unbounded title would take the strip; half the content box is its ceiling
  // and the other half is the flanks', split evenly.
  it("clips a title too wide for the strip at half the row's content box", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 200, right: 300, title: 900 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.title.width).toBe((w.row - 2 * w.padding) / 2);
  });

  it("leaves a quarter of the strip to each flank when the title is too wide", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 200, right: 300, title: 900 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect([placed.left.width, placed.right.width]).toEqual([
      (w.row - 2 * w.padding) / 2 / 2 - w.gap,
      (w.row - 2 * w.padding) / 2 / 2 - w.gap,
    ]);
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

  // A group that would not compress with its floorless track would spill out
  // of it and over the title, which is the one thing the tracks are there to
  // prevent.
  it("lets each flank group compress with its own track", () => {
    expect([
      declaration(".topbar-left", "min-width"),
      declaration(".topbar-right", "min-width"),
    ]).toEqual(["0", "0"]);
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

  it("stands the glyph closer to its label than two chips of the strip stand apart", () => {
    expect(declaration(".topbar-account-cell", "gap")).toBe("calc(var(--topbar-chip-gap) / 2)");
  });

  // ONE ELEMENT MEANS ONE ANCHOR: the options hang under the pair.
  it("anchors the account reveal on the pair rather than on the label alone", () => {
    // ARRANGE
    const { tc } = topbarContext();
    // ACT
    const row = drawTopbarView(view(), tc);
    // ASSERT
    expect([
      row.querySelector(".topbar-account-cell")?.getAttribute("data-reveal-anchor"),
      row.querySelector(".topbar-account")?.getAttribute("data-reveal-anchor"),
    ]).toEqual(["account", null]);
  });
});

describe("the no-session cells", () => {
  // A DASH IS A LABEL, NOT A CONTROL: it takes the right group's box so the
  // slot keeps its width and its neighbours do not move, and it takes neither
  // the cursor nor the hover border that would promise a click.
  it("boxes every no-session cell exactly as the strip's other right-hand cells", () => {
    expect(
      declaration(
        ".topbar-fast,\n.topbar-model[data-no-session],\n.topbar-mode[data-no-session]",
        "padding",
      ),
    ).toBe(
      declaration(
        ".topbar-model-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-warning-chip",
        "padding",
      ),
    );
  });
});
