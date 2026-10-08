// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { createRouterTransport } from "@connectrpc/connect";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import type { ServiceImpl } from "@connectrpc/connect";
import { PREFS_KEY, drawRailHead, mountSidebar, retireStoredPrefs } from "../../src/sidebar/sidebar.js";
import { GROUPING_VIEW_KEY } from "../../src/sidebar/view.js";
import { captureLogRecords, type LogCapture } from "../log-capture.js";
import { fireResize } from "../resize-observer.js";
import {
  NOW,
  SINK,
  WORKSPACE,
  fakeTicker,
  appContext,
  fakeTimers,
  mergedSection,
  repoSection,
  sidebarContext,
  taskSection,
  roster,
  rosterStream,
  row,
} from "./harness.js";

/** A storage a test can inspect, and one that throws on every access. */
function memoryStorage(seed: Record<string, string> = {}): Storage {
  const map = new Map(Object.entries(seed));
  return {
    get length() {
      return map.size;
    },
    clear: () => map.clear(),
    getItem: (key) => map.get(key) ?? null,
    key: (index) => [...map.keys()][index] ?? null,
    removeItem: (key) => void map.delete(key),
    setItem: (key, value) => void map.set(key, value),
  };
}

function throwingStorage(): Storage {
  return {
    get length(): number {
      throw new Error("site data is disabled");
    },
    clear: () => {
      throw new Error("site data is disabled");
    },
    getItem: () => {
      throw new Error("site data is disabled");
    },
    key: () => {
      throw new Error("site data is disabled");
    },
    removeItem: () => {
      throw new Error("site data is disabled");
    },
    setItem: () => {
      throw new Error("site data is disabled");
    },
  };
}

function ctxFor(rosters = [roster()], impl: Partial<ServiceImpl<typeof AgentRepl>> = {}): AppContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      updateSidebarView: () => create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } }),
      ...rosterStream(rosters),
      ...impl,
    });
  });
  return testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: fakeTicker(NOW),
    failures: SINK,
    composerEnabled: false,
  });
}

/** Let the stream deliver its pushes. */
async function settle(): Promise<void> {
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await settle();
}

describe("the preferences a page stored before the view was the daemon's", () => {
  let capture: LogCapture;
  beforeEach(() => {
    capture = captureLogRecords("debug");
  });

  /** The levels of every forwarded record under OPERATION's prefix. */
  async function levels(prefix: string): Promise<Array<string | undefined>> {
    capture.logger.flush();
    await Promise.resolve();
    return capture.sent.filter((r) => r.operation.startsWith(prefix)).map((r) => r.level.case);
  }

  it("are dropped", () => {
    const storage = memoryStorage({ [PREFS_KEY]: JSON.stringify({ grouping: "task", folded: { merged: false } }) });
    retireStoredPrefs(storage);
    expect(storage.getItem(PREFS_KEY)).toBeNull();
  });

  it("are said to be superseded once, at INFO", async () => {
    retireStoredPrefs(memoryStorage({ [PREFS_KEY]: JSON.stringify({ expanded: { "ws-1": true } }) }));
    expect(await levels("sidebar.prefs.superseded")).toEqual(["info"]);
  });

  it("say nothing when there were none", async () => {
    retireStoredPrefs(memoryStorage());
    expect(await levels("sidebar.prefs")).toEqual([]);
  });

  it("cost nothing but a warning when the storage throws", async () => {
    retireStoredPrefs(throwingStorage());
    expect(await levels("sidebar.prefs.retire-failed")).toEqual(["warn"]);
  });

  it("cost nothing at all with no storage available", async () => {
    retireStoredPrefs(null);
    expect(await levels("sidebar.prefs")).toEqual([]);
  });

  it("never reach the view: a page with stored folds draws the daemon's", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([roster({ merged: mergedSection([row({ id: "ws-9" })], 1, true) })]), {
      storage: memoryStorage({ [PREFS_KEY]: JSON.stringify({ grouping: "task", folded: { merged: false } }) }),
      timers: fakeTimers(),
    });
    await settle();
    expect([
      host.querySelector(".merged-section")?.classList.contains("folded"),
      host.querySelector("[data-grouping-pick='repository']")?.classList.contains("active"),
    ]).toEqual([true, true]);
  });
});

describe("the rail's header", () => {
  it("offers both groupings as a picker", () => {
    const head = drawRailHead(sidebarContext());
    const picks = [...head.element.querySelectorAll("[data-grouping-pick]")].map((el) =>
      el.getAttribute("data-grouping-pick"),
    );
    expect(picks).toEqual(["repository", "task"]);
  });

  it("lights the grouping it is painted with", () => {
    const head = drawRailHead(sidebarContext());
    head.paintGrouping("task");
    expect(head.element.querySelector("[data-grouping-pick='task']")?.classList.contains("active")).toBe(true);
  });

  it("asks the daemon to show the chosen grouping in every page", async () => {
    const asked: string[] = [];
    const sc = sidebarContext(
      appContext({
        updateSidebarView: (request) => {
          if (request.change.case === "showGrouping") asked.push(request.change.value.grouping.case ?? "unset");
          return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
    );
    const head = drawRailHead(sc);
    sc.view.track(GROUPING_VIEW_KEY, "repository", head.paintGrouping);
    await click(head.element.querySelector("[data-grouping-pick='task']") as Element);
    expect(asked).toEqual(["task"]);
  });

  it("lights the chosen grouping at once, before any push", async () => {
    const sc = sidebarContext();
    const head = drawRailHead(sc);
    sc.view.track(GROUPING_VIEW_KEY, "repository", head.paintGrouping);
    await click(head.element.querySelector("[data-grouping-pick='task']") as Element);
    expect(head.element.querySelector("[data-grouping-pick='task']")?.classList.contains("active")).toBe(true);
  });

  it("stays clickable after a choice the daemon took, because no push redraws it", async () => {
    const sc = sidebarContext();
    const head = drawRailHead(sc);
    sc.view.track(GROUPING_VIEW_KEY, "repository", head.paintGrouping);
    const pick = head.element.querySelector("[data-grouping-pick='task']") as HTMLButtonElement;
    await click(pick);
    expect([pick.disabled, pick.classList.contains("is-busy")]).toEqual([false, false]);
  });

  it("puts the old grouping back when the daemon cannot be reached", async () => {
    const sc = sidebarContext(
      appContext({
        updateSidebarView: () => {
          throw new Error("down");
        },
      }),
    );
    const head = drawRailHead(sc);
    sc.view.track(GROUPING_VIEW_KEY, "repository", head.paintGrouping);
    await click(head.element.querySelector("[data-grouping-pick='task']") as Element);
    expect(head.element.querySelector("[data-grouping-pick='repository']")?.classList.contains("active")).toBe(true);
  });
});

describe("mounting the rail: the view is the daemon's", () => {
  it("lights the picker for the grouping the push says", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([roster({ shown: "task" })]), { timers: fakeTimers() });
    await settle();
    expect(host.querySelector("[data-grouping-pick='task']")?.classList.contains("active")).toBe(true);
  });

  it("shows the grouping's pane in the rail on a picker click, before any push", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([roster()]), { timers: fakeTimers() });
    await settle();
    await click(host.querySelector("[data-grouping-pick='task']") as Element);
    expect((host.querySelector("[data-grouping='task']") as HTMLElement).hidden).toBe(false);
  });

  it("draws two pages fed the same pushes identically", async () => {
    const pushes = [
      roster({
        shown: "task",
        repos: [repoSection({ id: "repo-1", collapsed: true, rows: [row({ id: "ws-1" })] })],
        tasks: [taskSection({ id: "task-1", collapsed: true, rows: [row({ id: "ws-1" })] })],
        merged: mergedSection([row({ id: "ws-9" })], 1, false),
      }),
    ];
    const one = document.createElement("nav");
    const two = document.createElement("nav");
    mountSidebar(one, ctxFor(pushes), { timers: fakeTimers() });
    mountSidebar(two, ctxFor(pushes), { timers: fakeTimers() });
    await settle();
    expect(one.innerHTML).toBe(two.innerHTML);
  });

  it("draws in one page a fold another page asked for, once the push carries it", async () => {
    const host = document.createElement("nav");
    mountSidebar(
      host,
      ctxFor([
        roster({ merged: mergedSection([row({ id: "ws-9" })], 1, true) }),
        roster({ merged: mergedSection([row({ id: "ws-9" })], 1, false) }),
      ]),
      { timers: fakeTimers() },
    );
    await settle();
    expect(host.querySelector(".merged-section")?.classList.contains("folded")).toBe(false);
  });
});

describe("mounting the rail: the Recently Merged fit", () => {
  it("watches the rail's scroller, so a resize re-fits the band", async () => {
    // Arrange
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([roster({ merged: mergedSection([row({ id: "ws-9" })], 1, false) })]), {
      timers: fakeTimers(),
    });
    await settle();
    // Act / Assert — fireResize throws when nothing observes the element.
    expect(() => fireResize(host.querySelector(".sb-scroll") as Element)).not.toThrow();
  });

  it("watches each drawn pane, so a fold or a grouping switch re-fits the band", async () => {
    // Arrange
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([roster({ merged: mergedSection([row({ id: "ws-9" })], 1, false) })]), {
      timers: fakeTimers(),
    });
    await settle();
    // Act / Assert
    expect(() => fireResize(host.querySelector('.sb-pane[data-grouping="task"]') as Element)).not.toThrow();
  });
});

describe("mounting the rail: a switch to this workspace", () => {
  it("tells the feed when current moves to this page's workspace", async () => {
    // Arrange
    let selected = 0;
    const host = document.createElement("nav");
    // Act
    mountSidebar(host, ctxFor([roster({ current: "ws-other" }), roster({ current: WORKSPACE.id })]), {
      storage: null,
      timers: fakeTimers(),
      workspaceSelected: () => {
        selected += 1;
      },
    });
    await settle();
    // Assert
    expect(selected).toBe(1);
  });

  it("tells the feed nothing when current only restates this workspace", async () => {
    // Arrange
    let selected = 0;
    const host = document.createElement("nav");
    // Act
    mountSidebar(host, ctxFor([roster({ current: WORKSPACE.id }), roster({ current: WORKSPACE.id })]), {
      storage: null,
      timers: fakeTimers(),
      workspaceSelected: () => {
        selected += 1;
      },
    });
    await settle();
    // Assert
    expect(selected).toBe(0);
  });

  it("treats a reopened run's first push as a baseline", async () => {
    // Arrange: one run says another workspace is current and ends; the next
    // run's first push names this one, which restates rather than switches.
    let selected = 0;
    let run = 0;
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        watchWorkspaceRoster: async function* () {
          run += 1;
          yield create(WatchWorkspaceRosterResponseSchema, {
            push: { case: "roster", value: roster({ current: run === 1 ? "ws-other" : WORKSPACE.id }) },
          });
          if (run === 1) return;
          await new Promise<never>(() => undefined);
        },
      });
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: fakeTicker(NOW),
      failures: SINK,
      composerEnabled: false,
    });
    vi.useFakeTimers();
    try {
      const host = document.createElement("nav");
      mountSidebar(host, ctx, {
        storage: null,
        timers: fakeTimers(),
        workspaceSelected: () => {
          selected += 1;
        },
      });
      // Act
      await vi.runAllTimersAsync();
    } finally {
      vi.useRealTimers();
    }
    // Assert
    expect([run >= 2, selected]).toEqual([true, 0]);
  });
});

describe("mounting the rail", () => {
  it("ships hidden until the daemon pushes a roster", () => {
    const host = document.createElement("nav");
    host.hidden = true;
    mountSidebar(host, ctxFor([]), { storage: null, timers: fakeTimers() });
    expect(host.hidden).toBe(true);
  });

  it("reveals itself on the first push", async () => {
    const host = document.createElement("nav");
    host.hidden = true;
    mountSidebar(host, ctxFor(), { storage: null, timers: fakeTimers() });
    await settle();
    expect(host.hidden).toBe(false);
  });

  it("draws the roster whole", async () => {
    const host = document.createElement("nav");
    mountSidebar(
      host,
      ctxFor([roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] })]),
      { storage: null, timers: fakeTimers() },
    );
    await settle();
    expect(host.querySelector("[data-roster-row='ws-1']")).not.toBeNull();
  });

  it("replaces the previous drawing rather than accumulating rows", async () => {
    const host = document.createElement("nav");
    mountSidebar(
      host,
      ctxFor([
        roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] }),
        roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-2" })] })] }),
      ]),
      { storage: null, timers: fakeTimers() },
    );
    await settle();
    expect([
      host.querySelector("[data-roster-row='ws-1']"),
      host.querySelector("[data-roster-row='ws-2']") === null,
    ]).toEqual([null, false]);
  });

  it("keeps a blink running across a re-push that keeps the marker", async () => {
    const host = document.createElement("nav");
    const timers = fakeTimers();
    mountSidebar(
      host,
      ctxFor([
        roster({
          repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", attention: true })] })],
        }),
      ]),
      { storage: null, timers },
    );
    await settle();
    timers.run();
    expect(
      host.querySelector("[data-roster-row='ws-1'] .sb-attn")?.getAttribute("data-blink"),
    ).toBe("off");
  });

  it("mounts its header once, outside the redrawn body", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([roster(), roster()]), { storage: null, timers: fakeTimers() });
    await settle();
    expect(host.querySelectorAll(".sb-head").length).toBe(1);
  });

  it("hides the rail again and empties it on dispose", async () => {
    const host = document.createElement("nav");
    const handle = mountSidebar(host, ctxFor(), { storage: null, timers: fakeTimers() });
    await settle();
    handle.dispose();
    expect([host.hidden, host.childElementCount]).toEqual([true, 0]);
  });

  it("drops the ticker subscriptions its rows opened", async () => {
    const ticker = fakeTicker(NOW);
    const transport = createRouterTransport(({ service }) => {
      service(
        AgentRepl,
        rosterStream([
          roster({
            repos: [
              repoSection({
                id: "repo-1",
                rows: [
                  row({
                    id: "ws-1",
                    when: { case: "active", value: { atMs: BigInt(NOW) } },
                  }),
                ],
              }),
            ],
          }),
        ]),
      );
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker,
      failures: SINK,
      composerEnabled: false,
    });
    const host = document.createElement("nav");
    const handle = mountSidebar(host, ctx, { storage: null, timers: fakeTimers() });
    await settle();
    handle.dispose();
    expect(ticker.subscribers()).toBe(0);
  });

  it("opens the ONE global stream, with no workspace on it", async () => {
    let requests = 0;
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        watchWorkspaceRoster: async function* () {
          requests += 1;
          yield create(WatchWorkspaceRosterResponseSchema, { push: { case: "roster", value: roster() } });
          await new Promise<never>(() => undefined);
        },
      });
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: fakeTicker(NOW),
      failures: SINK,
      composerEnabled: false,
    });
    mountSidebar(document.createElement("nav"), ctx, { storage: null, timers: fakeTimers() });
    await settle();
    expect(requests).toBe(1);
  });
});

describe("the roster stream's planned ending", () => {
  it("files no failure and reopens when the daemon ends the stream as planned", async () => {
    // ARRANGE: run 1 carries a roster, then the planned ending, then ends
    // cleanly; the reopened run 2 stands, as a live daemon's stream does.
    const reported: string[] = [];
    let requests = 0;
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        watchWorkspaceRoster: async function* () {
          requests += 1;
          if (requests === 1) {
            yield create(WatchWorkspaceRosterResponseSchema, { push: { case: "roster", value: roster() } });
            yield create(WatchWorkspaceRosterResponseSchema, { push: { case: "ending", value: {} } });
            return;
          }
          await new Promise<never>(() => undefined);
        },
      });
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: fakeTicker(NOW),
      failures: { report: (kind) => void reported.push(kind.kind.case ?? "unset"), retract: () => undefined },
      composerEnabled: false,
    });
    // ACT
    const handle = mountSidebar(document.createElement("nav"), ctx, { storage: null, timers: fakeTimers() });
    await settle();
    handle.dispose();
    // ASSERT
    expect({ reported, requests }).toEqual({ reported: [], requests: 2 });
  });
});

describe("a click in the rail closes its open dropdowns", () => {
  const mounted: Array<{ dispose(): void }> = [];
  const hosts: HTMLElement[] = [];
  afterEach(() => {
    for (const handle of mounted.splice(0)) handle.dispose();
    for (const host of hosts.splice(0)) host.remove();
  });

  /** A rail with one row (in a repository and a task section), in the document. */
  async function mountRail(): Promise<HTMLElement> {
    const host = document.createElement("nav");
    document.body.appendChild(host);
    hosts.push(host);
    mounted.push(
      mountSidebar(
        host,
        ctxFor([
          roster({
            repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" }), row({ id: "ws-2" })] })],
            tasks: [taskSection({ id: "task-1" })],
          }),
        ]),
        { timers: fakeTimers() },
      ),
    );
    await settle();
    return host;
  }

  const rowOf = (host: HTMLElement, id: string): HTMLElement =>
    host.querySelector(`[data-grouping='repository'] [data-roster-row='${id}']`) as HTMLElement;
  const rowMenu = (host: HTMLElement, id: string): HTMLElement =>
    rowOf(host, id).querySelector(":scope > .sb-menu") as HTMLElement;
  const taskMenu = (host: HTMLElement): HTMLElement =>
    host.querySelector(".task-head .sb-menu") as HTMLElement;
  const emptySpace = (host: HTMLElement): HTMLElement => host.querySelector(".sb-head") as HTMLElement;

  /** Open the row's detail popover the way the keyboard does, at once. */
  const openDetail = (host: HTMLElement, id: string): void => {
    (rowOf(host, id).querySelector(":scope > .row") as HTMLElement).dispatchEvent(
      new FocusEvent("focusin", { bubbles: true }),
    );
  };

  it("closes a row's menu on a click in empty space", async () => {
    const host = await mountRail();
    await click(rowOf(host, "ws-1").querySelector(".sb-more") as Element);
    await click(emptySpace(host));
    expect(rowMenu(host, "ws-1").hidden).toBe(true);
  });

  it("closes a task's menu on a click in empty space", async () => {
    const host = await mountRail();
    await click(host.querySelector(".task-head .sb-more") as Element);
    await click(emptySpace(host));
    expect(taskMenu(host).hidden).toBe(true);
  });

  it("closes a row's detail popover on a click in empty space", async () => {
    const host = await mountRail();
    openDetail(host, "ws-1");
    await click(emptySpace(host));
    expect(rowOf(host, "ws-1").classList.contains("open")).toBe(false);
  });

  it("keeps a row's menu open on a click inside it", async () => {
    const host = await mountRail();
    await click(rowOf(host, "ws-1").querySelector(".sb-more") as Element);
    await click(rowMenu(host, "ws-1"));
    expect(rowMenu(host, "ws-1").hidden).toBe(false);
  });

  it("keeps a row's detail popover open on a click inside it", async () => {
    const host = await mountRail();
    openDetail(host, "ws-1");
    await click(rowOf(host, "ws-1").querySelector(":scope > .detail") as Element);
    expect(rowOf(host, "ws-1").classList.contains("open")).toBe(true);
  });

  it("swaps one row's menu for another's when the other opener is clicked", async () => {
    const host = await mountRail();
    await click(rowOf(host, "ws-1").querySelector(".sb-more") as Element);
    await click(rowOf(host, "ws-2").querySelector(".sb-more") as Element);
    expect([rowMenu(host, "ws-1").hidden, rowMenu(host, "ws-2").hidden]).toEqual([true, false]);
  });

  it("swaps a row's menu for a task's menu when the task's opener is clicked", async () => {
    const host = await mountRail();
    await click(rowOf(host, "ws-1").querySelector(".sb-more") as Element);
    await click(host.querySelector(".task-head .sb-more") as Element);
    expect([rowMenu(host, "ws-1").hidden, taskMenu(host).hidden]).toEqual([true, false]);
  });

  it("closes a row's detail popover when a menu opens", async () => {
    const host = await mountRail();
    openDetail(host, "ws-1");
    await click(rowOf(host, "ws-2").querySelector(".sb-more") as Element);
    expect(rowOf(host, "ws-1").classList.contains("open")).toBe(false);
  });

  it("closes a menu with its own opener, as before", async () => {
    const host = await mountRail();
    const more = rowOf(host, "ws-1").querySelector(".sb-more") as Element;
    await click(more);
    await click(more);
    expect(rowMenu(host, "ws-1").hidden).toBe(true);
  });

  it("does nothing, and logs nothing, on a click with none open", async () => {
    const host = await mountRail();
    const capture = captureLogRecords("debug");
    await click(emptySpace(host));
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation.startsWith("sidebar.dropdowns"))).toEqual([]);
  });
});

describe("the page's own storage, when no storage was injected", () => {
  it("is where the retired preferences are dropped from", async () => {
    globalThis.localStorage.setItem(PREFS_KEY, JSON.stringify({ grouping: "task" }));
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor(), { timers: fakeTimers() });
    await settle();
    expect(globalThis.localStorage.getItem(PREFS_KEY)).toBeNull();
  });
});

describe("a page whose storage cannot even be reached", () => {
  /** The real accessor, put back the moment the test that removed it ends. */
  const real = Object.getOwnPropertyDescriptor(globalThis, "localStorage");

  afterEach(() => {
    if (real === undefined) return;
    Object.defineProperty(globalThis, "localStorage", real);
  });

  it("still draws the rail", async () => {
    // Arrange: an embedding where touching `localStorage` throws outright.
    Object.defineProperty(globalThis, "localStorage", {
      configurable: true,
      get(): Storage {
        throw new Error("site data is disabled");
      },
    });
    const host = document.createElement("nav");
    // Act
    mountSidebar(host, ctxFor(), { timers: fakeTimers() });
    await settle();
    // Assert
    expect(host.hidden).toBe(false);
  });
});

describe("an open row's detail panel, placed by the rail", () => {
  // The panel is fixed-positioned, so where it lands is arithmetic over rects
  // jsdom reports as zeros; they are staged on the prototype exactly as the
  // topbar reveal suite stages its own.
  let original: typeof Element.prototype.getBoundingClientRect;
  const rects = new Map<Element, DOMRect>();

  const rect = (init: { left: number; top: number; width: number; height: number }): DOMRect =>
    ({
      left: init.left,
      top: init.top,
      right: init.left + init.width,
      bottom: init.top + init.height,
      width: init.width,
      height: init.height,
      x: init.left,
      y: init.top,
      toJSON: () => ({}),
    });

  beforeEach(() => {
    rects.clear();
    // eslint-disable-next-line @typescript-eslint/unbound-method -- reassigned in afterEach
    original = Element.prototype.getBoundingClientRect;
    Element.prototype.getBoundingClientRect = function staged(this: Element): DOMRect {
      return rects.get(this) ?? rect({ left: 0, top: 0, width: 0, height: 0 });
    };
    vi.stubGlobal("innerWidth", 1000);
    vi.stubGlobal("innerHeight", 800);
  });

  afterEach(() => {
    Element.prototype.getBoundingClientRect = original;
    vi.unstubAllGlobals();
  });

  /**
   * Mount a rail holding one row with its detail open, and stage its rects.
   *
   * The host is put IN the document, because a captured `scroll` reaches the
   * window only by propagating down to a target the window can see — which is
   * where the real rail always is.
   */
  async function mountExpanded(host: HTMLElement, lineRect: DOMRect): Promise<HTMLElement> {
    document.body.appendChild(host);
    hosts.push(host);
    const handle = mountSidebar(
      host,
      ctxFor([roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] })]),
      { timers: fakeTimers() },
    );
    mounted.push(handle);
    await settle();
    const ws = host.querySelector("[data-roster-row='ws-1']") as HTMLElement;
    // Opened by keyboard focus, which opens at once, with no intent delay.
    (ws.querySelector(":scope > .row") as HTMLElement).dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    rects.set(ws.querySelector(":scope > .row") as Element, lineRect);
    rects.set(
      ws.querySelector(":scope > .detail") as Element,
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    return ws;
  }

  const mounted: Array<{ dispose(): void }> = [];
  const hosts: HTMLElement[] = [];
  afterEach(() => {
    for (const handle of mounted.splice(0)) handle.dispose();
    for (const host of hosts.splice(0)) host.remove();
  });

  it("is placed by the draw, so a remembered expansion is not left at the window's origin", async () => {
    // ARRANGE: the rects are staged AFTER the first draw, so the draw's own
    // placement read zeros; the next push is what has real geometry to read.
    const host = document.createElement("nav");
    const ws = await mountExpanded(host, rect({ left: 8, top: 120, width: 190, height: 24 }));
    // ACT
    window.dispatchEvent(new Event("resize"));
    // ASSERT: hanging under the row's own line.
    expect((ws.querySelector(":scope > .detail") as HTMLElement).style.top).toBe("144px");
  });

  it("follows the row when a scroller moves it, because a fixed panel does not", async () => {
    // ARRANGE
    const host = document.createElement("nav");
    const ws = await mountExpanded(host, rect({ left: 8, top: 120, width: 190, height: 24 }));
    window.dispatchEvent(new Event("resize"));
    // ACT: the rail scrolls the row up, and the scroll is captured at window.
    rects.set(
      ws.querySelector(":scope > .row") as Element,
      rect({ left: 8, top: 60, width: 190, height: 24 }),
    );
    (host.querySelector(".sb-scroll") as HTMLElement).dispatchEvent(
      new Event("scroll", { bubbles: false }),
    );
    // ASSERT
    expect((ws.querySelector(":scope > .detail") as HTMLElement).style.top).toBe("84px");
  });

  it("stops being placed once the rail is disposed", async () => {
    // ARRANGE
    const host = document.createElement("nav");
    const ws = await mountExpanded(host, rect({ left: 8, top: 120, width: 190, height: 24 }));
    const detail = ws.querySelector(":scope > .detail") as HTMLElement;
    window.dispatchEvent(new Event("resize"));
    // ACT
    for (const handle of mounted.splice(0)) handle.dispose();
    rects.set(
      ws.querySelector(":scope > .row") as Element,
      rect({ left: 8, top: 300, width: 190, height: 24 }),
    );
    window.dispatchEvent(new Event("resize"));
    // ASSERT: the panel the disposed rail drew is nobody's to move any more.
    expect(detail.style.top).toBe("144px");
  });
});
