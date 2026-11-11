// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import { TopbarViewSchema, type TopbarView, TopbarAccountSignInRequestSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { createControl } from "../../src/control.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import STYLESHEET from "../../src/styles.css?raw";
import { drawChipRun, drawTopbarView, mountTopbar, signInRequestToOpen } from "../../src/topbar/topbar.js";
import { createLocalFailures, type LocalFailures } from "../../src/failure/local.js";
import { pagePresence } from "../../src/presence.js";
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
    workspaceName: { text: "fix" },
    sessionLine: { text: "session abc" },
    account: {
      state: { case: "loggedIn", value: { email: "a@b.test" } },
      options: [
        { configDir: "/root/.claude", current: true, state: { case: "loggedIn", value: { email: "a@b.test" } } },
      ],
    },
    connectivity: { tone: "green", glyph: "●", title: "connected" },
    modelSelector: { options: [{ model: { name: "opus" }, displayName: "Opus" }], defaultModel: { name: "opus" } },
    permissionModePicker: {
      current: { mode: "auto", displayName: "auto" },
      options: [{ mode: "plan", displayName: "plan" }],
    },
    context: { text: "142.3k", breakdown: { sections: [] } },
    warnings: { warnings: [] },
    remoteControl: { mode: { case: "off", value: {} }, tooltip: { text: "remote control off" } },
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
    workspaceName: { text: "fix" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "none", glyph: "○", title: "no session" },
    context: { text: context, breakdown: { sections: [{ heading: { text: reason }, rows: [] }] } },
    warnings: { warnings: [{ line: { text: reason } }] },
    remoteControl: { mode: { case: "off", value: {} }, tooltip: { text: "remote control off" } },
    persistentWifi: {
      wifi: { case: "joined", value: {} },
      mode: { case: "off", value: {} },
      tooltip: { text: "Wi-Fi: joined to Home · persistent wifi off: closing the lid sleeps" },
    },
  });
}

/** An element's first class: the cell's kind. */
const firstClass = (el: Element): string => el.className.split(" ")[0] ?? "";

/** The kinds of the cells directly inside ROW's SELECTOR, in order. */
const childClasses = (row: Element, selector: string): string[] =>
  [...(row.querySelector(selector)?.children ?? [])].map(firstClass);

describe("drawChipRun", () => {
  it("names the run by its own class and the shared chip-run class", () => {
    expect(drawChipRun("topbar-status", []).className).toBe("topbar-status topbar-chips");
  });

  it("holds the chips it is given, in order", () => {
    const [a, b] = [document.createElement("span"), document.createElement("i")];
    expect([...drawChipRun("topbar-right", [a, b]).children]).toEqual([a, b]);
  });

  it("draws an empty run when given no chips", () => {
    expect(drawChipRun("topbar-right", []).children.length).toBe(0);
  });
});

describe("drawTopbarView", () => {
  it("draws the three groups", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view(), tc);
    expect([
      row.querySelector(".topbar-left") !== null,
      row.querySelector(".topbar-heading") !== null,
      row.querySelector(".topbar-right") !== null,
    ]).toEqual([true, true, true]);
  });

  it("draws the workspace's name beneath the summary in the heading", () => {
    const { tc } = topbarContext();
    const heading = drawTopbarView(view(), tc).querySelector(".topbar-heading");
    expect([...(heading?.children ?? [])].map((el) => el.textContent)).toEqual(["ABC/fix", "fix"]);
  });

  it("draws no summary line, and the name alone, when the view carries no title", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view({ title: undefined }), tc);
    expect([row.querySelector(".topbar-title"), row.querySelector(".topbar-workspace-name")?.textContent])
      .toEqual([null, "fix"]);
  });

  it("refuses a view with no workspace name", () => {
    const { tc } = topbarContext();
    expect(() => drawTopbarView(view({ workspaceName: undefined }), tc)).toThrow(MalformedView);
  });

  it("opens the session line from the heading when the view carries no title", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(drawTopbarView(view({ title: undefined }), tc));
    // ACT
    host.querySelector(".topbar-workspace-name")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.textContent).toBe("session abc");
  });

  // THE STATUS CHIPS RIDE THE LEFT (owner request, 2026-11-09): after the
  // account cell, the remote-control chip, the persistent-wifi chip, then the
  // warning chip, which ends the run so its arrival moves no other chip.
  it("draws the left group as the account cell and then the status run", () => {
    const { tc } = topbarContext();
    const left = drawTopbarView(view(), tc).querySelector(".topbar-left");
    expect([...(left?.children ?? [])].map(firstClass)).toEqual(["topbar-account-cell", "topbar-status"]);
  });

  it("orders the status run remote control, persistent wifi, warnings", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view({ warnings: sessionlessView("parked").warnings }), tc);
    expect(childClasses(row, ".topbar-status")).toEqual(["topbar-remote-control", "topbar-wifi", "topbar-warnings"]);
  });

  it("orders the right group model, effort, permission mode, context", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view({ warnings: sessionlessView("parked").warnings }), tc);
    expect(childClasses(row, ".topbar-right")).toEqual(["topbar-model", "topbar-effort", "topbar-mode", "topbar-context"]);
  });

  it("draws the status run without a warning chip while nothing is wrong", () => {
    const { tc } = topbarContext();
    expect(childClasses(drawTopbarView(view(), tc), ".topbar-status")).toEqual(["topbar-remote-control", "topbar-wifi"]);
  });

  // THE ONE SHAPE OF A CHIP RUN: every run the strip draws comes from
  // `drawChipRun`, so each carries the class the stylesheet spaces chips by.
  it("draws both chip runs as chip runs", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view(), tc);
    expect([...row.querySelectorAll(".topbar-chips")].map(firstClass)).toEqual(["topbar-status", "topbar-right"]);
  });

  it("draws the remote-control chip for a workspace with no session", () => {
    const { tc } = topbarContext();
    expect(drawTopbarView(sessionlessView("parked"), tc).querySelector(".topbar-remote-control")).not.toBeNull();
  });

  it("refuses a view with no remote-control chip", () => {
    const { tc } = topbarContext();
    expect(() => drawTopbarView(view({ remoteControl: undefined }), tc)).toThrow(MalformedView);
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
    const row = drawTopbarView(sessionlessView("no session yet"), tc);
    expect([
      row.querySelector(".topbar-account") !== null,
      row.querySelector(".topbar-connectivity") !== null,
      row.querySelector(".topbar-heading") !== null,
      row.querySelector(".topbar-model") !== null,
      row.querySelector(".topbar-mode") !== null,
      row.querySelector(".topbar-context") !== null,
    ]).toEqual([true, true, true, true, true, true]);
  });

  it("draws each absent control as a dash in its own slot", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(sessionlessView("no session yet"), tc);
    expect([...row.querySelectorAll("[data-no-session]")].map((el) => el.getAttribute("data-no-session")))
      .toEqual(["model", "effort", "mode"]);
  });

  it("keeps the cells in one order whether or not there is a session", () => {
    // THE STRIP DOES NOT REARRANGE ITSELF under the reader; that is the whole
    // point of the fixed schema.
    const { tc } = topbarContext();
    const classOf = (v: TopbarView): string[] =>
      [...drawTopbarView(v, tc).querySelectorAll(".topbar-chips > *")].map(firstClass);
    const warned = view({
      warnings: create(TopbarViewSchema, {
        warnings: { warnings: [{ line: { text: "a" }, detail: { case: "accounting", value: { lines: [] } } }] },
      }).warnings,
    });
    expect(classOf(sessionlessView("no session yet"))).toEqual(classOf(warned));
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
    expect(drawTopbarView(view(), tc).querySelector(".topbar-heading-warnings")).toBeNull();
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
    expect(drawn.querySelector(".topbar-heading-warnings")).not.toBeNull();
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

  /** A view whose account carries the sign-in request ID. */
  function requesting(id: string): TopbarView {
    const v = view();
    v.account!.signInRequest = create(TopbarAccountSignInRequestSchema, { requestId: id });
    return v;
  }

  it("opens the login by itself when the daemon asks for a sign-in", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const openLogin = vi.fn();
    const handle = mountTopbar(host, { failures: createLocalFailures(), geometry: GEOMETRY });
    // ACT
    handle.watch(daemon([requesting("1")]), { openLogin });
    await settle();
    // ASSERT
    expect(openLogin).toHaveBeenCalledTimes(1);
    handle.dispose();
  });

  it("opens the login once for a request a later push repeats", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const openLogin = vi.fn();
    const handle = mountTopbar(host, { failures: createLocalFailures(), geometry: GEOMETRY });
    // ACT
    handle.watch(daemon([requesting("1"), requesting("1")]), { openLogin });
    await settle();
    // ASSERT
    expect(openLogin).toHaveBeenCalledTimes(1);
    handle.dispose();
  });

  it("opens the login again for a later request", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const openLogin = vi.fn();
    const handle = mountTopbar(host, { failures: createLocalFailures(), geometry: GEOMETRY });
    // ACT
    handle.watch(daemon([requesting("1"), view(), requesting("2")]), { openLogin });
    await settle();
    // ASSERT
    expect(openLogin).toHaveBeenCalledTimes(2);
    handle.dispose();
  });

  it("opens no login while no sign-in is requested", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const openLogin = vi.fn();
    const handle = mountTopbar(host, { failures: createLocalFailures(), geometry: GEOMETRY });
    // ACT
    handle.watch(daemon([view()]), { openLogin });
    await settle();
    // ASSERT
    expect(openLogin).not.toHaveBeenCalled();
    handle.dispose();
  });

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

  it("draws every push while the webhost hides its page, so a switch to it shows the current strip", async () => {
    // ARRANGE
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    pagePresence().setShown(false);
    const second = view({ title: create(TopbarViewSchema, { title: { text: "other" } }).title });
    try {
      // ACT
      const handle = mountWatching(host, daemon([view(), second]));
      await settle();
      // ASSERT
      expect(host.querySelector(".topbar-title")?.textContent).toBe("other");
      handle.dispose();
    } finally {
      pagePresence().setShown(true);
    }
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
      .querySelector(".topbar-heading-warnings")
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
      expect(host.querySelector(".topbar-heading-warnings")).toBeNull();
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

  it("draws the chip in the left group's status run before any push", () => {
    const failures = createLocalFailures();
    const { host, handle } = mountUnwatched(failures);
    failures.report(staleBundle("drift"));
    expect(host.querySelector(".topbar-row > .topbar-left > .topbar-status > .topbar-warnings")).not.toBeNull();
    handle.dispose();
  });

  it("draws the right group as an empty chip run before any push", () => {
    const failures = createLocalFailures();
    const { host, handle } = mountUnwatched(failures);
    failures.report(staleBundle("drift"));
    expect(host.querySelector(".topbar-row > .topbar-right.topbar-chips")?.children.length).toBe(0);
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
    const flanks = [declaration(".topbar-left", "gap"), declaration(".topbar-chips", "gap")];
    const row = declaration(".topbar-row", "gap");
    // ASSERT
    expect([...flanks, row]).toEqual([
      "var(--topbar-chip-gap)",
      "var(--topbar-chips-gap)",
      "var(--topbar-cell-gap)",
    ]);
  });

  // The chip runs are spaced by their shared class alone, so neither flank
  // group may carry a chip gap of its own that would compete with it.
  it("gaps the right group's chips from the chip run's rule alone", () => {
    expect(withoutBlockComments(ruleBody(".topbar-right"))).not.toMatch(/(?:^|;|\n)\s*gap\s*:/);
  });

  // THE 2026-10-02 REQUEST: the right-hand chips "30% closer". The space a
  // reader sees between two of them is the gap plus each side's inset and 1px
  // border, which was 0.3rem + 2 x (0.45rem + 1px) = 21.2px at the 16px root.
  it("spaces the right-hand chips at least 30% closer than the 21.2px they stood apart", () => {
    // ARRANGE
    const root = 16;
    // ACT
    const spacing = (token("--topbar-chips-gap") + 2 * token("--topbar-chips-inset")) * root + 2;
    // ASSERT
    expect(spacing).toBeLessThanOrEqual(21.2 * 0.7);
  });

  it("spaces the right-hand chips no closer than 30%", () => {
    const spacing = (token("--topbar-chips-gap") + 2 * token("--topbar-chips-inset")) * 16 + 2;
    expect(spacing).toBeGreaterThan(21.2 * 0.69);
  });

  // ONE RULE FOR EVERY CHIP, keyed on its run: a button inside a control's
  // wrap, or a cell that holds no button.
  it("insets every chip from the chip run's one rule", () => {
    expect(declaration(".topbar-chips ar-button,\n.topbar-chips > :not(:has(ar-button))", "padding-inline")).toBe(
      "var(--topbar-chips-inset)",
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

describe("the strip's geometry", () => {
  // jsdom lays out nothing, so the rules that make the centering hold are
  // asserted as declarations; test/webkit/topbar.webkit.test.ts measures it.
  it("lays the row out with equal side tracks around a shrinkable middle track", () => {
    expect(declaration(".topbar-row", "grid-template-columns")).toBe(
      "minmax(min-content, 1fr) minmax(0, auto) minmax(min-content, 1fr)",
    );
  });

  it("centers every cell of the row vertically in the strip", () => {
    expect(declaration(".topbar-row", "align-items")).toBe("center");
  });

  it.each([".topbar-left", ".topbar-right"])("leaves %s no height of its own", (selector) => {
    expect(withoutBlockComments(ruleBody(selector))).not.toMatch(/(?:^|;|\n)\s*height\s*:/);
  });

  it.each(["topbar-title", "topbar-workspace-name"])(
    "ellipsis-clips the %s rather than letting it wrap or spill",
    (className) => {
      // Arrange
      const remove = installStylesheet();
      const line = document.createElement("div");
      line.className = className;
      document.body.append(line);
      // Act
      const values = ["min-width", "overflow", "text-overflow", "white-space"].map((property) =>
        cascadedValue(line, property),
      );
      // Assert
      expect(values).toEqual(["0px", "hidden", "ellipsis", "nowrap"]);
      line.remove();
      remove();
    },
  );

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
    const classes = childClasses(drawTopbarView(withEffort, tc), ".topbar-right");
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
        ".topbar-model-button,\n.topbar-effort-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-remote-control-button,\n.topbar-wifi-button,\n.topbar-heading-warnings",
        "padding-block",
      ),
    );
  });

  // The inline inset is the group's, so a cell's own rule setting the
  // `padding` shorthand would quietly win over it for that one cell.
  it.each([
    ".topbar-model[data-no-session],\n.topbar-effort[data-no-session],\n.topbar-effort[data-effort-unsupported],\n.topbar-mode[data-no-session]",
    ".topbar-model-button,\n.topbar-effort-button,\n.topbar-mode-button,\n.topbar-context-figure,\n.topbar-remote-control-button,\n.topbar-wifi-button,\n.topbar-heading-warnings",
  ])("leaves the inline inset of %s to the group", (selector) => {
    expect(withoutBlockComments(ruleBody(selector))).not.toMatch(/(?:^|[;\s])padding(?:-inline)?(?:-left|-right)?\s*:/);
  });
});

describe("signInRequestToOpen", () => {
  it.each([
    ["no request", undefined, null, null],
    ["a request not yet opened", "7", null, "7"],
    ["the request already opened", "7", "7", null],
    ["a newer request than the one opened", "8", "7", "8"],
  ] as const)("answers %s", (_case, id, opened, want) => {
    // ARRANGE
    const v = view();
    if (id !== undefined) v.account!.signInRequest = create(TopbarAccountSignInRequestSchema, { requestId: id });
    // ACT
    const got = signInRequestToOpen(v, opened);
    // ASSERT
    expect(got).toBe(want);
  });
});
