// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import { TopbarViewSchema, type TopbarView, TopbarAccountSignInRequestSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { createControl } from "../../src/control.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawChipRun, drawTopbarView, mountTopbar, signInRequestToOpen } from "../../src/topbar/topbar.js";
import { pageFocusLeft } from "../../src/focus-left.js";
import { pagePresence } from "../../src/presence.js";
import type { AppContext } from "../../src/rpc/context.js";
import { GEOMETRY, RecordingSink, appContext, openPanel, topbarContext } from "./fixtures.js";
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
 * and NOT ONE of the three session-scoped controls. The context chip is still
 * set — always — and it is where the state's own facts ride (topbar.proto,
 * FIXED SCHEMA AND ORGANIZATION).
 */
function sessionlessView(reason: string, context = "0"): TopbarView {
  return create(TopbarViewSchema, {
    title: { text: "ABC/fix" },
    workspaceName: { text: "fix" },
    account: { state: { case: "loggedIn", value: { email: "a@b.test" } } },
    connectivity: { tone: "none", glyph: "○", title: "no session" },
    context: { text: context, breakdown: { sections: [{ heading: { text: reason }, rows: [] }] } },
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
  // A QUEUE BYPASS HOLDER IS IN REMOTE CONTROL (owner request, 2026-11-10):
  // the daemon serves its chip on, and the strip draws it beside the lit
  // bypass chip as for any remote-controlled workspace.
  it("draws remote control on for a workspace in queue bypass mode", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(
      view({
        queueBypass: create(TopbarViewSchema, {
          queueBypass: { mode: { case: "on", value: {} }, tooltip: { text: "queue bypass on" } },
        }).queueBypass,
        remoteControl: create(TopbarViewSchema, {
          remoteControl: { mode: { case: "on", value: {} }, tooltip: { text: "remote control on" } },
        }).remoteControl,
      }),
      tc,
    );
    expect([
      row.querySelector(".topbar-queue-bypass")?.getAttribute("data-mode"),
      row.querySelector(".topbar-remote-control")?.getAttribute("data-mode"),
    ]).toEqual(["on", "on"]);
  });

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

  // THE STATUS CHIPS OPEN THE STRIP, LEFT OF THE LOGIN (owner requests,
  // 2026-11-09 and 2026-11-10): the status run, then the account cell. The
  // queue-bypass chip opens the run on a personal account (owner request,
  // 2026-11-10), and the warnings are no chip of it: they ride the heading.
  it("draws the left group as the status run and then the account cell", () => {
    const { tc } = topbarContext();
    const left = drawTopbarView(view(), tc).querySelector(".topbar-left");
    expect([...(left?.children ?? [])].map(firstClass)).toEqual(["topbar-status", "topbar-account-cell"]);
  });

  it("orders the status run queue bypass, remote control, persistent wifi", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(
      view({
        queueBypass: create(TopbarViewSchema, {
          queueBypass: { mode: { case: "off", value: {} }, tooltip: { text: "queue bypass off" } },
        }).queueBypass,
      }),
      tc,
    );
    expect(childClasses(row, ".topbar-status")).toEqual(["topbar-queue-bypass", "topbar-remote-control", "topbar-wifi"]);
  });

  it("closes the status run with the production-checks chip, right of persistent wifi, when the repository checks its production", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(
      view({
        productionChecks: create(TopbarViewSchema, {
          productionChecks: {
            standing: { case: "running", value: {} },
            repository: { id: "repo-1", dir: "/repos/one" },
            tooltip: { text: "production checks running" },
          },
        }).productionChecks,
      }),
      tc,
    );
    expect(childClasses(row, ".topbar-status")).toEqual(["topbar-remote-control", "topbar-wifi", "topbar-production-checks"]);
  });

  it("orders the right group model, effort, permission mode, context", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(view(), tc);
    expect(childClasses(row, ".topbar-right")).toEqual(["topbar-model", "topbar-effort", "topbar-mode", "topbar-context"]);
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
    expect(classOf(sessionlessView("no session yet"))).toEqual(classOf(view()));
  });

  it("draws the context figure for a workspace with no session", () => {
    const { tc } = topbarContext();
    const row = drawTopbarView(sessionlessView("cold context, awaiting your answer", "101.1k"), tc);
    expect(row.querySelector(".topbar-context-figure")?.textContent).toBe("101.1k");
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

  /** Mount the topbar and start it watching CTX. */
  function mountWatching(host: HTMLElement, ctx: AppContext) {
    const handle = mountTopbar(host, { focusLeft: pageFocusLeft(), geometry: GEOMETRY });
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
    const handle = mountTopbar(host, { focusLeft: pageFocusLeft(), geometry: GEOMETRY });
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
    const handle = mountTopbar(host, { focusLeft: pageFocusLeft(), geometry: GEOMETRY });
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
    const handle = mountTopbar(host, { focusLeft: pageFocusLeft(), geometry: GEOMETRY });
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
    const handle = mountTopbar(host, { focusLeft: pageFocusLeft(), geometry: GEOMETRY });
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
    // The second push waits for the test to open the reveal: an event the
    // test releases, never a delay.
    let releaseSecondPush!: () => void;
    const secondPush = new Promise<void>((resolve) => {
      releaseSecondPush = resolve;
    });
    const ctx = appContext({
      watchTopbar: async function* () {
        yield create(WatchTopbarResponseSchema, { topbar: view() });
        await secondPush;
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
    releaseSecondPush();
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

describe("mountTopbar before its first push", () => {
  it("draws nothing until a push arrives", () => {
    const host = document.createElement("div");
    document.body.replaceChildren(host);
    const handle = mountTopbar(host, { focusLeft: pageFocusLeft(), geometry: GEOMETRY });
    expect(host.querySelector("[data-topbar-strip]")?.children.length).toBe(0);
    handle.dispose();
  });
});

describe("the strip's geometry", () => {

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
