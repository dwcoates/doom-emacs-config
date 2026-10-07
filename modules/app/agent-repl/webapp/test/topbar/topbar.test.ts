// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import { TopbarViewSchema, type TopbarView } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { createControl } from "../../src/control.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import STYLESHEET from "../../src/styles.css?raw";
import { drawTopbarView, mountTopbar } from "../../src/topbar/topbar.js";
import { createLocalFailures, type LocalFailures } from "../../src/failure/local.js";
import type { AppContext } from "../../src/rpc/context.js";
import { clearClientFailures } from "../../src/rpc/link.js";
import {
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  staleBundle,
  workspaceGone,
} from "../../src/failure/sink.js";
import { GEOMETRY, RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";
import { withoutBlockComments } from "../source-text.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";

/** A complete view; each test overrides only what it is about. */
function view(overrides: Partial<TopbarView> = {}): TopbarView {
  const base = create(TopbarViewSchema, {
    title: { text: "ABC/fix" },
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
      current: { mode: "auto", displayName: "auto" },
      options: [{ mode: "plan", displayName: "plan" }],
    },
    context: { text: "142.3k", breakdown: { sections: [] } },
    warnings: { warnings: [] },
    persistentWifi: {
      wifi: { case: "joined", value: {} },
      mode: { case: "off", value: {} },
      tooltip: { text: "Wi-Fi: joined to Home · persistent wifi off: closing the lid sleeps" },
    },
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
    title: { text: "ABC/fix" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "none", glyph: "○", title: "no session" },
    context: { text: context, breakdown: { sections: [{ heading: { text: reason }, rows: [] }] } },
    warnings: { warnings: [{ line: { text: reason } }] },
    persistentWifi: {
      wifi: { case: "joined", value: {} },
      mode: { case: "off", value: {} },
      tooltip: { text: "Wi-Fi: joined to Home · persistent wifi off: closing the lid sleeps" },
    },
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

  it("places the persistent-wifi chip between the context chip and the warning chip", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view({ warnings: sessionlessView("parked").warnings }), tc);
    const right = [...(row.querySelector(".topbar-right")?.children ?? [])];
    const at = (cls: string): number => right.findIndex((el) => el.classList.contains(cls));
    expect([at("topbar-wifi") === at("topbar-context") + 1, at("topbar-warnings") === at("topbar-wifi") + 1])
      .toEqual([true, true]);
  });

  it("refuses a view with no persistent-wifi chip", () => {
    const { tc } = topbarContext();
    expect(() => drawTopbarView(view({ persistentWifi: undefined }), tc)).toThrow(MalformedView);
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
      row.querySelector(".topbar-context") !== null,
    ]).toEqual([true, true, true, true, true, true]);
  });

  it("draws each absent control as a dash in its own slot", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(sessionlessView("hibernated since 14:03"), tc);
    expect([...row.querySelectorAll("[data-no-session]")].map((el) => el.getAttribute("data-no-session")))
      .toEqual(["model", "effort", "mode"]);
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

  /** Mount the topbar over FAILURES and start it watching CTX. */
  function mountWatching(
    host: HTMLElement,
    ctx: AppContext,
    failures: LocalFailures = createLocalFailures(),
  ) {
    const handle = mountTopbar(host, { failures, geometry: GEOMETRY });
    handle.watch(ctx, { openLogin: () => undefined });
    return handle;
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
    const handle = mountWatching(host, daemon([view()]));
    await settle();
    // ASSERT
    expect(host.querySelector(".topbar-title")?.textContent).toBe("ABC/fix");
    handle.dispose();
  });

  it("replaces the strip whole on the next push", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const second = view({ title: create(TopbarViewSchema, { title: { text: "other" } }).title });
    // ACT
    const handle = mountWatching(host, daemon([view(), second]));
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
    const handle = mountWatching(host, ctx);
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
    const handle = mountWatching(host, ctx);
    await settle();
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toContain("frameUndecodable");
    handle.dispose();
  });

  it("empties the host on dispose", async () => {
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountWatching(host, daemon([view()]));
    await settle();
    handle.dispose();
    expect(host.children.length).toBe(0);
  });

  it("refuses to watch twice", () => {
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountWatching(host, daemon([]));
    expect(() => handle.watch(daemon([]), { openLogin: () => undefined })).toThrow(
      "the topbar was asked to watch twice",
    );
    handle.dispose();
  });
});

/**
 * THE WARNING CHIP LISTS THE PAGE'S OWN FAILURES, WITH OR WITHOUT A PUSH (owner
 * ruling, 2026-09-23). The chip is the one place an error shows, and the
 * failures that most need showing are the ones that stop a push arriving.
 */
describe("mountTopbar: the chip's client-local failures", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
    clearClientFailures();
  });

  async function settle(): Promise<void> {
    for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
  }

  /** A mounted topbar that has NOT started watching: no push can ever arrive. */
  function mountUnwatched(failures: LocalFailures) {
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountTopbar(host, { failures, geometry: GEOMETRY });
    return { host, handle };
  }

  /** The arms of the client-local rows in the chip's opened list. */
  function listedArms(host: HTMLElement): string[] {
    host
      .querySelector(".topbar-warning-chip")
      ?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    const arms = Array.from(openPanel(host)?.querySelectorAll<HTMLElement>("[data-local]") ?? []).map(
      (row) => row.dataset.arm ?? "",
    );
    document.body.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    return arms;
  }

  const MINTS = [
    ["daemonUnreachable", () => daemonUnreachable(1006, "abnormal")],
    ["workspaceGone", () => workspaceGone()],
    ["bootFailed", () => bootFailed("Error: nope")],
    ["controlPlaneFailed", () => controlPlaneFailed("OpenLogin", "unavailable")],
    ["frameUndecodable", () => frameUndecodable("a oneof sets no arm", "FooterView")],
    ["staleBundle", () => staleBundle("schema drift")],
  ] as const;

  for (const [arm, mint] of MINTS) {
    it(`lists ${arm} in the chip when it is filed`, () => {
      // ARRANGE
      const failures = createLocalFailures();
      const { host, handle } = mountUnwatched(failures);
      // ACT
      failures.report(mint());
      // ASSERT
      expect(listedArms(host)).toEqual([arm]);
      handle.dispose();
    });

    it(`takes ${arm} off the chip when it is retracted`, () => {
      // ARRANGE
      const failures = createLocalFailures();
      const { host, handle } = mountUnwatched(failures);
      failures.report(mint());
      // ACT
      failures.retract(arm);
      // ASSERT
      expect(host.querySelector(".topbar-warning-chip")).toBeNull();
      handle.dispose();
    });
  }

  it("draws a failure filed before the topbar was mounted", () => {
    const failures = createLocalFailures();
    failures.report(bootFailed("Error: adoption refused"));
    const { host, handle } = mountUnwatched(failures);
    expect(listedArms(host)).toEqual(["bootFailed"]);
    handle.dispose();
  });

  it("draws no chip at all while nothing stands", () => {
    const { host, handle } = mountUnwatched(createLocalFailures());
    expect(host.querySelector(".topbar-warnings")).toBeNull();
    handle.dispose();
  });

  it("draws the chip at the strip's right edge before any push", () => {
    const failures = createLocalFailures();
    const { host, handle } = mountUnwatched(failures);
    failures.report(staleBundle("drift"));
    expect(host.querySelector(".topbar-row > .topbar-right > .topbar-warnings")).not.toBeNull();
    handle.dispose();
  });

  it("lists the unreachable daemon when the stream fails before its first push", async () => {
    // ARRANGE: the daemon is unreachable from the start, so no view ever
    // arrives; the stream's own failure is filed through the chip's set.
    const failures = createLocalFailures();
    const ctx = appContext(
      {
        watchTopbar: async function* () {
          throw new Error("connection refused");
        },
      },
      failures,
    );
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountTopbar(host, { failures, geometry: GEOMETRY });
    // ACT
    handle.watch(ctx, { openLogin: () => undefined });
    await settle();
    // ASSERT
    expect(listedArms(host)).toEqual(["daemonUnreachable"]);
    handle.dispose();
  });

  it("adds the chip over the last view when the link drops after a push", async () => {
    // ARRANGE
    const failures = createLocalFailures();
    const ctx = appContext(
      {
        watchTopbar: async function* () {
          yield create(WatchTopbarResponseSchema, { topbar: view() });
          throw new Error("connection reset");
        },
      },
      failures,
    );
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountTopbar(host, { failures, geometry: GEOMETRY });
    // ACT
    handle.watch(ctx, { openLogin: () => undefined });
    await settle();
    // ASSERT: the stale view stays drawn, and the chip lists the dropped link.
    expect([host.querySelector(".topbar-title")?.textContent, listedArms(host)]).toEqual([
      "ABC/fix",
      ["daemonUnreachable"],
    ]);
    handle.dispose();
  });

  it("stops drawing failures once disposed", () => {
    const failures = createLocalFailures();
    const { host, handle } = mountUnwatched(failures);
    handle.dispose();
    failures.report(staleBundle("drift"));
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
  const body = withoutBlockComments(ruleBody(selector));
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
      "var(--topbar-right-chip-gap)",
      "var(--topbar-cell-gap)",
    ]);
  });

  // THE 2026-10-02 REQUEST: the right-hand chips "30% closer". The space a
  // reader sees between two of them is the gap plus each side's inset and 1px
  // border, which was 0.3rem + 2 x (0.45rem + 1px) = 21.2px at the 16px root.
  it("spaces the right-hand chips at least 30% closer than the 21.2px they stood apart", () => {
    // ARRANGE
    const root = 16;
    // ACT
    const spacing = (token("--topbar-right-chip-gap") + 2 * token("--topbar-right-chip-inset")) * root + 2;
    // ASSERT
    expect(spacing).toBeLessThanOrEqual(21.2 * 0.7);
  });

  it("spaces the right-hand chips no closer than 30%", () => {
    const spacing = (token("--topbar-right-chip-gap") + 2 * token("--topbar-right-chip-inset")) * 16 + 2;
    expect(spacing).toBeGreaterThan(21.2 * 0.69);
  });

  // ONE RULE FOR EVERY RIGHT-HAND CELL, keyed on the group: a button inside a
  // control's wrap, or a cell that holds no button.
  it("insets every right-hand cell from the group's one rule", () => {
    expect(declaration(".topbar-right ar-button,\n.topbar-right > :not(:has(ar-button))", "padding-inline")).toBe(
      "var(--topbar-right-chip-inset)",
    );
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
 * `grid-template-columns: auto minmax(0, 1fr) auto` states: each flank sized
 * to the group it holds, and the middle track taking everything between them,
 * floored at zero so it compresses instead of pushing a flank off the strip.
 * The title's TEXT is then centered inside that track (`text-align: center`),
 * which is the owner's rule of 2026-09-14: equal clear space either side of
 * the visible text, not a shared midpoint with the strip. The template itself
 * is asserted alongside, so a change to it fails these tests rather than
 * quietly leaving them measuring a layout the app no longer has.
 */
interface StubWidths {
  readonly row: number;
  readonly padding: number;
  readonly gap: number;
  /** What each group WANTS; it gets exactly that while the strip has room. */
  readonly left: number;
  readonly right: number;
  /** What the title wants; the space between the groups is what it gets. */
  readonly title: number;
}

interface Placed {
  readonly left: { start: number; width: number };
  /** The middle TRACK: the space between the two groups. */
  readonly title: { start: number; width: number };
  /** The visible TEXT, centered inside that track. */
  readonly text: { start: number; width: number };
  readonly right: { start: number; width: number };
}

function placeTracks(w: StubWidths): Placed {
  const content = w.row - 2 * w.padding;
  const inner = content - 2 * w.gap;
  // THE FLANKS ARE `auto`: each takes its own content, compressing only when
  // the two of them together outgrow the strip.
  const flankRoom = Math.max(0, inner);
  const scale = w.left + w.right > flankRoom ? flankRoom / (w.left + w.right) : 1;
  const left = w.left * scale;
  const right = w.right * scale;
  // THE MIDDLE TRACK IS WHAT IS LEFT, floored at zero.
  const track = Math.max(0, inner - left - right);
  const trackStart = w.padding + left + w.gap;
  // `text-align: center` inside it; a title wider than the track fills it and
  // ellipsizes.
  const text = Math.min(w.title, track);
  return {
    left: { start: w.padding, width: left },
    title: { start: trackStart, width: track },
    text: { start: trackStart + (track - text) / 2, width: text },
    right: { start: trackStart + track + w.gap, width: right },
  };
}

describe("the strip's geometry", () => {
  it("lays the row out with content-sized flanks and a floorless middle track", () => {
    expect(declaration(".topbar-row", "grid-template-columns")).toBe("auto minmax(0, 1fr) auto");
  });

  // THE RULING. The clear space to the left of the visible text and the clear
  // space to its right are the same measure, whatever the groups hold.
  it("leaves equal clear space either side of the title with unequal groups", () => {
    // ARRANGE
    const w: StubWidths = { row: 1000, padding: 8, gap: 8, left: 120, right: 340, title: 60 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.text.start - placed.title.start).toBe(
      placed.title.start + placed.title.width - (placed.text.start + placed.text.width),
    );
  });

  // ...and that is SUBTLY DIFFERENT from centering on the strip: with a wider
  // right group the text sits left of the strip's midpoint, on purpose.
  it("centers the title between the groups rather than on the whole strip", () => {
    // ARRANGE
    const w: StubWidths = { row: 1000, padding: 8, gap: 8, left: 120, right: 340, title: 60 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.text.start + placed.text.width / 2).toBeLessThan(w.row / 2);
  });

  // Each flank gets exactly the group it holds; neither spreads.
  it("sizes each flank track to its own group", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 90, right: 320, title: 240 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect([placed.left.width, placed.right.width]).toEqual([w.left, w.right]);
  });

  // The title can never reach a group: it gets what they left and no more.
  it("gives an over-wide title exactly the space between the two groups", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 200, right: 300, title: 900 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.text.width).toBe(w.row - 2 * w.padding - 2 * w.gap - w.left - w.right);
  });

  // ...and an over-wide title still clips symmetrically, because the box it
  // clips in IS the space between the groups.
  it("clips an over-wide title symmetrically inside the middle track", () => {
    // ARRANGE
    const w: StubWidths = { row: 600, padding: 8, gap: 8, left: 200, right: 300, title: 900 };
    // ACT
    const placed = placeTracks(w);
    // ASSERT
    expect(placed.text.start).toBe(placed.title.start);
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

describe("the connectivity glyph's session dropdown", () => {
  /** The view with agent-repl's session on its connectivity indicator. */
  const withSession = (): TopbarView =>
    view({
      connectivity: create(TopbarViewSchema, {
        connectivity: {
          tone: "green",
          glyph: "●",
          title: "connected",
          session: { startedAtMs: 1n, began: { case: "editorStart", value: {} } },
        },
      }).connectivity,
    });

  it("opens agent-repl's session from the glyph, not the account options", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(withSession(), tc));
    // ACT
    host.querySelector(".topbar-connectivity")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe("agent-repl-session");
  });

  it("still opens the account options from the label beside it", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(withSession(), tc));
    // ACT
    host.querySelector(".topbar-account")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe("account");
  });

  // NO SESSION, NO DROPDOWN: the glyph is the account cell's, as it was.
  it("leaves a session-less glyph's click to the account options", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(view(), tc));
    // ACT
    host.querySelector(".topbar-connectivity")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.getAttribute("data-reveal")).toBe("account");
  });

  it("wears a pointer only while it opens something", () => {
    expect(declaration(".topbar-connectivity[data-reveal-anchor]", "cursor")).toBe("pointer");
  });
});

describe("the effort selector's place and form", () => {
  it("is drawn between the model selector and the permission-mode picker", () => {
    // Arrange
    const { tc } = topbarContext();
    const withEffort = view({
      effortSelector: create(TopbarViewSchema, {
        effortSelector: {
          support: {
            case: "supported",
            value: { current: { level: 2, displayName: "medium" }, options: [{ level: 2, displayName: "medium" }] },
          },
        },
      }).effortSelector,
    });
    // Act
    const classes = [...(drawTopbarView(withEffort, tc).querySelector(".topbar-right")?.children ?? [])].map(
      (el) => el.className.split(" ")[0],
    );
    // Assert
    expect(classes.slice(classes.indexOf("topbar-model"), classes.indexOf("topbar-model") + 3)).toEqual([
      "topbar-model",
      "topbar-effort",
      "topbar-mode",
    ]);
  });

  // THE SAME FORM AS ITS NEIGHBOURS (owner, 2026-10-01): its button, its rows
  // and its dash compute exactly what the permission-mode picker's do, so a
  // later change to one that leaves the other behind fails here.
  it.each([
    ["the button", "topbar-mode-button", "topbar-effort-button", ""],
    ["a menu row", "topbar-mode-option", "topbar-effort-option", ""],
    ["the dash", "topbar-mode", "topbar-effort", "data-no-session"],
    ["the disabled button", "topbar-mode-button", "topbar-effort-button", "aria-disabled"],
  ])("draws %s as the permission-mode picker draws it", (_what, mode, effort, attribute) => {
    // Arrange
    const remove = installStylesheet();
    const make = (className: string): HTMLElement => {
      const element =
        className.endsWith("-option") || className.endsWith("-button") ? createControl() : document.createElement("span");
      element.className = className;
      if (attribute !== "") element.setAttribute(attribute, attribute === "aria-disabled" ? "true" : "");
      document.body.append(element);
      return element;
    };
    const modeElement = make(mode);
    const effortElement = make(effort);
    const properties = ["padding", "border", "border-radius", "font-size", "font-family", "color", "white-space", "cursor", "background"];
    // Act
    const computed = (element: HTMLElement): string[] => properties.map((property) => cascadedValue(element, property));
    // Assert
    expect(computed(effortElement)).toEqual(computed(modeElement));
    modeElement.remove();
    effortElement.remove();
    remove();
  });
});

describe("the no-session cells", () => {
  // A DASH IS A LABEL, NOT A CONTROL: it takes the right group's box so the
  // slot keeps its width and its neighbours do not move, and it takes neither
  // the cursor nor the hover border that would promise a click.
  it("boxes every no-session cell exactly as the strip's other right-hand cells", () => {
    expect(
      declaration(".topbar-model[data-no-session],\n.topbar-effort[data-no-session],\n.topbar-effort[data-effort-unsupported],\n.topbar-mode[data-no-session]", "padding-block"),
    ).toBe(
      declaration(
        ".topbar-model-button,\n.topbar-effort-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-wifi-button,\n.topbar-warning-chip",
        "padding-block",
      ),
    );
  });

  // The inline inset is the group's, so a cell's own rule setting the
  // `padding` shorthand would quietly win over it for that one cell.
  it.each([
    ".topbar-model[data-no-session],\n.topbar-effort[data-no-session],\n.topbar-effort[data-effort-unsupported],\n.topbar-mode[data-no-session]",
    ".topbar-model-button,\n.topbar-effort-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-wifi-button,\n.topbar-warning-chip",
  ])("leaves the inline inset of %s to the group", (selector) => {
    expect(withoutBlockComments(ruleBody(selector))).not.toMatch(/(?:^|[;\s])padding(?:-inline)?(?:-left|-right)?\s*:/);
  });
});
