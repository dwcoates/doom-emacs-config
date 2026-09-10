// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
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
                    when: { case: "lastSelected", value: { atMs: BigInt(NOW) } },
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
          yield create(WatchWorkspaceRosterResponseSchema, { roster: roster() });
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
