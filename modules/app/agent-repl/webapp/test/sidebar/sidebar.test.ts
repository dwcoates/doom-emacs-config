// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { createRouterTransport } from "@connectrpc/connect";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import {
  PREFS_KEY,
  createSidebarPrefs,
  drawRailHead,
  mountSidebar,
} from "../../src/sidebar/sidebar.js";
import {
  NOW,
  SINK,
  WORKSPACE,
  fakeTicker,
  fakeTimers,
  memoryPrefs,
  repoSection,
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

/** The prefs record as it was actually persisted, typed rather than `any`. */
function storedPrefs(storage: Storage): {
  grouping?: string;
  folded?: Record<string, boolean>;
  expanded?: Record<string, boolean>;
} {
  const raw = storage.getItem(PREFS_KEY);
  if (raw === null) throw new Error("no prefs were written");
  return JSON.parse(raw) as ReturnType<typeof storedPrefs>;
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

function ctxFor(rosters = [roster()]): AppContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, rosterStream(rosters));
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

describe("the rail's preferences", () => {
  it("start on the repository grouping", () => {
    expect(createSidebarPrefs(memoryStorage()).grouping()).toBe("repository");
  });

  it("read a remembered grouping back", () => {
    const storage = memoryStorage({ [PREFS_KEY]: JSON.stringify({ grouping: "task" }) });
    expect(createSidebarPrefs(storage).grouping()).toBe("task");
  });

  it("persist a grouping change", () => {
    const storage = memoryStorage();
    createSidebarPrefs(storage).setGrouping("task");
    expect(storedPrefs(storage).grouping).toBe("task");
  });

  it("persist a fold", () => {
    const storage = memoryStorage();
    createSidebarPrefs(storage).setFolded("repo:one", true);
    expect(storedPrefs(storage).folded?.["repo:one"]).toBe(true);
  });

  it("persist a row's expansion", () => {
    const storage = memoryStorage();
    createSidebarPrefs(storage).setExpanded("ws-1", true);
    expect(storedPrefs(storage).expanded?.["ws-1"]).toBe(true);
  });

  it("keep a fold's own default for a section never folded", () => {
    expect(createSidebarPrefs(memoryStorage()).isFolded("merged", true)).toBe(true);
  });

  it("treat unreadable storage as nothing remembered", () => {
    expect(createSidebarPrefs(throwingStorage()).grouping()).toBe("repository");
  });

  it("keep working when a write throws", () => {
    const prefs = createSidebarPrefs(throwingStorage());
    prefs.setGrouping("task");
    expect(prefs.grouping()).toBe("task");
  });

  it("treat unparsable storage as nothing remembered", () => {
    const storage = memoryStorage({ [PREFS_KEY]: "{not json" });
    expect(createSidebarPrefs(storage).grouping()).toBe("repository");
  });

  it("treat a non-object payload as nothing remembered", () => {
    const storage = memoryStorage({ [PREFS_KEY]: "42" });
    expect(createSidebarPrefs(storage).grouping()).toBe("repository");
  });

  it("work at all with no storage available", () => {
    const prefs = createSidebarPrefs(null);
    prefs.setFolded("repo:one", true);
    expect(prefs.isFolded("repo:one")).toBe(true);
  });
});

describe("the rail's header", () => {
  it("offers both groupings as a picker", () => {
    const head = drawRailHead(memoryPrefs(), document.createElement("div"));
    const picks = [...head.querySelectorAll("[data-grouping-pick]")].map((el) =>
      el.getAttribute("data-grouping-pick"),
    );
    expect(picks).toEqual(["repository", "task"]);
  });

  it("marks the grouping in force", () => {
    const head = drawRailHead(memoryPrefs({ grouping: "task" }), document.createElement("div"));
    expect(
      head.querySelector("[data-grouping-pick='task']")?.classList.contains("active"),
    ).toBe(true);
  });

  it("remembers the choice when it is switched", async () => {
    const prefs = memoryPrefs();
    const head = drawRailHead(prefs, document.createElement("div"));
    await click(head.querySelector("[data-grouping-pick='task']") as Element);
    expect(prefs.state.grouping).toBe("task");
  });

  it("swaps which pane is shown, rather than redrawing anything", async () => {
    const body = document.createElement("div");
    body.innerHTML =
      "<div data-grouping='repository'></div><div data-grouping='task' hidden></div>";
    const head = drawRailHead(memoryPrefs(), body);
    await click(head.querySelector("[data-grouping-pick='task']") as Element);
    const hidden = [...body.querySelectorAll<HTMLElement>("[data-grouping]")].map((el) => el.hidden);
    expect(hidden).toEqual([true, false]);
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

describe("the page's own storage, when no storage was injected", () => {
  /** Everything this section writes into the real jsdom storage. */
  afterEach(() => {
    globalThis.localStorage.removeItem(PREFS_KEY);
  });

  it("persists a preference into the page's localStorage", () => {
    createSidebarPrefs().setGrouping("task");
    expect(storedPrefs(globalThis.localStorage).grouping).toBe("task");
  });

  it("reads a preference back out of the page's localStorage", () => {
    globalThis.localStorage.setItem(PREFS_KEY, JSON.stringify({ grouping: "task" }));
    expect(createSidebarPrefs().grouping()).toBe("task");
  });

  it("mounts on the page's storage, so a remembered grouping is the one drawn", async () => {
    globalThis.localStorage.setItem(PREFS_KEY, JSON.stringify({ grouping: "task" }));
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor(), { timers: fakeTimers() });
    await settle();
    expect(
      host.querySelector("[data-grouping-pick='task']")?.classList.contains("active"),
    ).toBe(true);
  });
});

describe("a page whose storage cannot even be reached", () => {
  /** The real accessor, put back the moment the test that removed it ends. */
  const real = Object.getOwnPropertyDescriptor(globalThis, "localStorage");

  afterEach(() => {
    if (real === undefined) return;
    Object.defineProperty(globalThis, "localStorage", real);
  });

  it("keeps its preferences in memory rather than failing to draw", () => {
    // Arrange: an embedding where touching `localStorage` throws outright.
    Object.defineProperty(globalThis, "localStorage", {
      configurable: true,
      get(): Storage {
        throw new Error("site data is disabled");
      },
    });
    // Act
    const prefs = createSidebarPrefs();
    prefs.setGrouping("task");
    // Assert
    expect(prefs.grouping()).toBe("task");
  });
});

describe("a row's remembered expansion", () => {
  it("is closed for a row nothing was ever remembered about", () => {
    const storage = memoryStorage({ [PREFS_KEY]: JSON.stringify({ grouping: "task" }) });
    expect(createSidebarPrefs(storage).isExpanded("ws-1")).toBe(false);
  });

  it("is closed for a row absent from a remembered set", () => {
    const storage = memoryStorage({
      [PREFS_KEY]: JSON.stringify({ expanded: { "ws-2": true } }),
    });
    expect(createSidebarPrefs(storage).isExpanded("ws-1")).toBe(false);
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
   * Mount a rail holding one row remembered as expanded, and stage its rects.
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
      {
        storage: memoryStorage({ [PREFS_KEY]: JSON.stringify({ expanded: { "ws-1": true } }) }),
        timers: fakeTimers(),
      },
    );
    mounted.push(handle);
    await settle();
    const ws = host.querySelector("[data-roster-row='ws-1']") as HTMLElement;
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
