// @vitest-environment jsdom
import { CreateWorkspaceResponseSchema, type CreateWorkspaceRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_workspace_pb";
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
import {
  PREFS_KEY,
  drawRailHead,
  mountSidebar as mountSidebarNow,
  mountWebhostSidebar as mountWebhostSidebarNow,
  retireStoredPrefs,
  type SidebarDeps,
  type WebhostSidebarDeps,
} from "../../src/sidebar/sidebar.js";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { createRailSearch } from "../../src/sidebar/search.js";
import { HOVER_CLOSE_GRACE_MS } from "../../src/sidebar/row-hover.js";
import { captureLogRecords, forwardedRecord, type LogCapture } from "../log-capture.js";
import type { PhasedAnimation } from "../../src/sidebar/wall-phase.js";
import type { UnseenEnv } from "../../src/sidebar/unseen.js";
import { createPageSeen } from "../../src/page-seen.js";
import { createPresence, pagePresence } from "../../src/presence.js";
import { visibilityDoc } from "../visibility-doc.js";
import { fireResize } from "../resize-observer.js";
import {
  NOW,
  SINK,
  WORKSPACE,
  fakeTicker,
  fakeTimers,
  mergedSection,
  repoSection,
  roster,
  rosterStream,
  row,
} from "./harness.js";
import { frameQueue, type FrameQueue } from "../frame-queue.js";
import { fireIntersection } from "../intersection-observer.js";
import { MERGED_DRAWN_AT_ONCE } from "../../src/sidebar/roster.js";
import { STAND_IN_ATTRIBUTE } from "../../src/sidebar/row.js";
import type { FailureSink } from "../../src/failure/sink.js";

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

function ctxFor(
  rosters = [roster()],
  impl: Partial<ServiceImpl<typeof AgentRepl>> = {},
  failures: FailureSink = SINK,
): AppContext {
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
    failures,
    composerEnabled: false,
  });
}

/** Type QUERY into HOST's rail search. */
function search(host: HTMLElement, query: string): void {
  (host.querySelector("[data-search-toggle]") as HTMLElement).click();
  const input = host.querySelector("[data-search-input]") as HTMLInputElement;
  input.value = query;
  input.dispatchEvent(new Event("input"));
}

/** The rail's frames, where it draws each push; run by `settle`. */
let frames: FrameQueue;

beforeEach(() => {
  frames = frameQueue();
});

/** Mount the rail drawing on the suite's frames. */
function mountSidebar(host: HTMLElement, ctx: AppContext, deps: SidebarDeps = {}): ReturnType<typeof mountSidebarNow> {
  return mountSidebarNow(host, ctx, { frame: frames.frame, ...deps });
}

/** Mount the webhost's sidebar drawing on the suite's frames. */
function mountWebhostSidebar(
  host: HTMLElement,
  ctx: Parameters<typeof mountWebhostSidebarNow>[1],
  deps: WebhostSidebarDeps = {},
): ReturnType<typeof mountWebhostSidebarNow> {
  return mountWebhostSidebarNow(host, ctx, { frame: frames.frame, ...deps });
}

/** Let the stream deliver its pushes, and the rail draw them. */
async function settle(): Promise<void> {
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
  frames.drain();
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
    expect(host.querySelector(".merged-section")?.classList.contains("folded")).toBe(true);
  });
});

/** The rail's header, with a search no test here types into. */
function railHead(): ReturnType<typeof drawRailHead> {
  return drawRailHead(createRailSearch(() => {}));
}

describe("the rail's header", () => {
  it("draws the search toggle immediately left of the title", () => {
    const head = railHead();
    const title = head.element.querySelector(".sb-title") as Element;
    expect(title.previousElementSibling?.hasAttribute("data-search-toggle")).toBe(true);
  });

});

describe("mounting the rail: the view is the daemon's", () => {
  it("draws two pages fed the same pushes identically", async () => {
    const pushes = [
      roster({
        repos: [repoSection({ id: "repo-1", collapsed: true, rows: [row({ id: "ws-1" })] })],
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

describe("mounting the rail: the search", () => {
  /** Open HOST's search and type QUERY into it. */
  const pushes = (): WorkspaceRoster[] => [
    roster({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" }), row({ id: "ws-2", name: "beta" })] })],
    }),
  ];

  it("hangs the bar between the head and the scroller, so it overlaps the rows", () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([]), { storage: null, timers: fakeTimers() });
    expect([...host.children].map((el) => el.className)).toEqual(["sb-head", "sb-search", "sb-scroll"]);
  });

  it("narrows the drawn rows as the query is typed", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor(pushes()), { storage: null, timers: fakeTimers() });
    await settle();
    search(host, "bet");
    expect(host.querySelector("[data-roster-row='ws-1']")?.classList.contains("sb-search-miss")).toBe(true);
  });

  it("narrows a roster pushed after the query was typed", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor(pushes()), { storage: null, timers: fakeTimers() });
    search(host, "bet");
    await settle();
    expect(host.querySelector("[data-roster-row='ws-1']")?.classList.contains("sb-search-miss")).toBe(true);
  });

  it("logs what each query kept and hid", async () => {
    const capture = captureLogRecords("debug");
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor(pushes()), { storage: null, timers: fakeTimers() });
    await settle();
    search(host, "bet");
    const record = await forwardedRecord(capture, "sidebar.search");
    expect(record.context).toMatchObject({ query: "bet", kept: 1, missed: 1 });
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

});

/** An endless loop of DURATION milliseconds, at no phase until sought. */
function loopAnimation(duration: number): PhasedAnimation {
  return {
    currentTime: null,
    effect: { getTiming: () => ({ duration, iterations: Infinity, delay: 0, direction: "normal" }) },
  };
}

describe("mounting the rail: its looping animations", () => {
  it("seeks the drawn rail's loops to the wall clock after every draw", async () => {
    // Arrange: each draw makes a fresh dot, whose animation starts at 0%, and
    // a frame per push draws each push on its own.
    const drawn: PhasedAnimation[] = [];
    const host = document.createElement("nav");

    // Act
    mountSidebar(host, ctxFor([roster({ current: "ws-other" }), roster({ current: WORKSPACE.id })]), {
      storage: null,
      timers: fakeTimers(),
      frame: (callback) => callback(),
      wallPhase: {
        animations: () => {
          const dot = loopAnimation(1300);
          drawn.push(dot);
          return [dot];
        },
        now: () => 2900,
      },
    });
    await settle();

    // Assert
    expect(drawn.map((dot) => dot.currentTime)).toEqual([300, 300]);
  });

  it("puts two workspaces' pages at one phase at the same instant", async () => {
    // Arrange: one webview per workspace, each drawing its own dot.
    const one = loopAnimation(1300);
    const two = loopAnimation(1300);
    const mount = (dot: PhasedAnimation): void => {
      mountSidebar(document.createElement("nav"), ctxFor([roster()]), {
        storage: null,
        timers: fakeTimers(),
        wallPhase: { animations: () => [dot], now: () => NOW },
      });
    };

    // Act
    mount(one);
    mount(two);
    await settle();

    // Assert
    expect(one.currentTime).toBe(two.currentTime);
  });

  it("stops re-seeking on a page shown again once disposed", async () => {
    // Arrange
    const frames = frameQueue();
    const handle = mountSidebar(document.createElement("nav"), ctxFor([]), {
      storage: null,
      timers: fakeTimers(),
      wallPhase: { animations: () => [], now: () => NOW, frame: frames.frame },
    });

    // Act
    handle?.dispose();
    document.dispatchEvent(new Event("visibilitychange"));

    // Assert
    expect(frames.pending()).toBe(0);
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
    handle?.dispose();
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
                    when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } },
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
    handle?.dispose();
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
    handle?.dispose();
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

  /** A rail with two rows in a repository section, in the document. */
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
          }),
        ]),
        { timers: fakeTimers() },
      ),
    );
    await settle();
    return host;
  }

  const rowOf = (host: HTMLElement, id: string): HTMLElement =>
    host.querySelector(`.sb-pane [data-roster-row='${id}']`) as HTMLElement;
  const rowMenu = (host: HTMLElement, id: string): HTMLElement =>
    rowOf(host, id).querySelector(":scope > .sb-menu") as HTMLElement;
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
    // Opened by keyboard focus.
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
    // ASSERT: on the row's own line.
    expect((ws.querySelector(":scope > .detail") as HTMLElement).style.top).toBe("120px");
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
    expect((ws.querySelector(":scope > .detail") as HTMLElement).style.top).toBe("60px");
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
    expect(detail.style.top).toBe("120px");
  });
});

/** An unseen-gate environment whose visibility the test sets and changes. */
function unseenDoc(initial: DocumentVisibilityState): { env: UnseenEnv; show(): void } {
  const v = visibilityDoc(initial);
  return {
    env: { seen: createPageSeen(WORKSPACE.id, createPresence(v.doc, false)), now: () => 0 },
    show: () => v.set("visible"),
  };
}

describe("mounting the rail: a page no one can see", () => {
  it("leaves the rail undrawn while hidden and another workspace is current", async () => {
    // Arrange
    const host = document.createElement("nav");
    const unseen = unseenDoc("hidden");

    // Act
    mountSidebar(
      host,
      ctxFor([roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] })]),
      { storage: null, timers: fakeTimers(), unseen: unseen.env },
    );
    await settle();

    // Assert
    expect(host.querySelector("[data-roster-row='ws-1']")).toBeNull();
  });

  it("draws the newest roster it held once the page turns visible", async () => {
    // Arrange
    const host = document.createElement("nav");
    const unseen = unseenDoc("hidden");
    mountSidebar(
      host,
      ctxFor([
        roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] }),
        roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-2" })] })] }),
      ]),
      { storage: null, timers: fakeTimers(), unseen: unseen.env },
    );
    await settle();

    // Act
    unseen.show();
    await settle();

    // Assert
    expect([
      host.querySelector("[data-roster-row='ws-1']"),
      host.querySelector("[data-roster-row='ws-2']") === null,
    ]).toEqual([null, false]);
  });

  it("still returns the feed to its tail on the first switch of a page hidden since its first push", async () => {
    // Arrange
    const host = document.createElement("nav");
    const unseen = unseenDoc("hidden");
    let arrived = 0;

    // Act
    mountSidebar(host, ctxFor([roster({ current: "ws-other" }), roster({ current: WORKSPACE.id })]), {
      storage: null,
      timers: fakeTimers(),
      unseen: unseen.env,
      workspaceSelected: () => {
        arrived += 1;
      },
    });
    await settle();

    // Assert
    expect(arrived).toBe(1);
  });
});

describe("mounting the webhost's sidebar", () => {
  afterEach(() => {
    // The page's own presence is shared by every test in the file: put it back.
    pagePresence().setShown(true);
  });

  it("draws every push: there is no page of its own to be unseen", async () => {
    // Arrange
    const host = document.createElement("nav");
    host.hidden = true;

    // Act
    mountWebhostSidebar(
      host,
      ctxFor([
        roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] }),
        roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-2" })] })] }),
      ]),
      { storage: null, timers: fakeTimers() },
    );
    await settle();

    // Assert
    expect(host.querySelector("[data-roster-row='ws-2']")).not.toBeNull();
  });

  it("is told once, when the first push reveals it", async () => {
    // Arrange
    const host = document.createElement("nav");
    host.hidden = true;
    let revealed = 0;

    // Act
    mountWebhostSidebar(host, ctxFor([roster(), roster()]), {
      storage: null,
      timers: fakeTimers(),
      revealed: () => {
        revealed += 1;
      },
    });
    await settle();

    // Assert
    expect([revealed, host.hidden]).toEqual([1, false]);
  });
});

describe("the webhost sidebar's create form", () => {
  it("spawns a child from the workspace the roster names current", async () => {
    // Arrange
    let request: CreateWorkspaceRequest | null = null;
    const host = document.createElement("nav");
    document.body.append(host);
    mountWebhostSidebar(
      host,
      ctxFor([roster({ current: "ws-current", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] })], {
        createWorkspace: (r) => {
          request = r;
          return create(CreateWorkspaceResponseSchema, {
            result: { case: "success", value: { workspace: { id: "ws-new", dir: "/w/new" } } },
          });
        },
      }),
      { storage: null, timers: fakeTimers() },
    );
    await settle();
    await click(host.querySelector(".sb-add") as Element);
    const parent = host.querySelector("input[name='parent']") as HTMLInputElement;
    parent.checked = true;

    // Act
    await click(host.querySelector("[data-create-submit]") as Element);

    // Assert
    expect((request as CreateWorkspaceRequest | null)?.parent?.workspace?.id).toBe("ws-current");
    host.remove();
  });
});

describe("the rail reports where the pointer is to the row hover gesture", () => {
  const mounted: Array<{ dispose(): void }> = [];
  afterEach(() => {
    for (const handle of mounted.splice(0)) handle.dispose();
    vi.useRealTimers();
  });

  /** Mount a rail of one row, with the pointer on its duration field, which opened the panel. */
  async function mountHovered(): Promise<{ host: HTMLElement; ws: HTMLElement }> {
    const host = document.createElement("div");
    mounted.push(
      mountSidebar(host, ctxFor([roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] })]), {
        timers: fakeTimers(),
      }),
    );
    await settle();
    vi.useFakeTimers();
    const ws = host.querySelector(".sb-pane [data-roster-row='ws-1']") as HTMLElement;
    (ws.querySelector(":scope > .row > [data-when]") as HTMLElement).dispatchEvent(new MouseEvent("mouseenter"));
    return { host, ws };
  }

  it("closes the panel when the pointer leaves the rail without the duration field seeing it go", async () => {
    // ARRANGE
    const { host, ws } = await mountHovered();
    // ACT
    host.dispatchEvent(new MouseEvent("mouseleave"));
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(ws.classList.contains("open")).toBe(false);
  });

  it("closes the panel when the pointer moves elsewhere in the rail without the duration field seeing it go", async () => {
    // ARRANGE
    const { host, ws } = await mountHovered();
    // ACT
    (host.querySelector(".sb-head") as HTMLElement).dispatchEvent(new MouseEvent("mousemove", { bubbles: true }));
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(ws.classList.contains("open")).toBe(false);
  });

  it("keeps the panel open while the pointer moves over the duration field", async () => {
    // ARRANGE
    const { ws } = await mountHovered();
    // ACT
    (ws.querySelector(":scope > .row > [data-when]") as HTMLElement).dispatchEvent(
      new MouseEvent("mousemove", { bubbles: true }),
    );
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
    // ASSERT
    expect(ws.classList.contains("open")).toBe(true);
  });
});

// A PAUSED QUEUE ON THE LIVE RAIL (owner ruling, 2026-10-14): each roster
// push is drawn as it stands, so the pause greys a repository's merges and
// the resume brings them back with no reload.
describe("the rail under a paused merge queue", () => {
  /** A roster whose repo-1 merges ws-1 and whose repo-2 merges ws-2; PAUSED names the paused repositories. */
  function mergingRoster(paused: ReadonlySet<string>) {
    return roster({
      repos: [
        repoSection({
          id: "repo-1",
          rows: [row({ id: "ws-1", status: { case: "merging", value: {} }, mergePaused: paused.has("repo-1") })],
        }),
        repoSection({
          id: "repo-2",
          rows: [row({ id: "ws-2", status: { case: "merging", value: {} }, mergePaused: paused.has("repo-2") })],
        }),
      ],
    });
  }
  const markOf = (host: Element, id: string): Element | null => host.querySelector(`[data-roster-row='${id}'] .st`);
  const nameOf = (host: Element, id: string): Element | null => host.querySelector(`[data-roster-row='${id}'] .name`);

  it("greys the paused repository's merge once the pause is pushed", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([mergingRoster(new Set()), mergingRoster(new Set(["repo-1"]))]), { timers: fakeTimers() });
    await settle();
    expect([markOf(host, "ws-1")?.classList.contains("ink-grey"), nameOf(host, "ws-1")?.classList.contains("breathes")]).toEqual([
      true,
      false,
    ]);
  });

  it("leaves the other repository's merge purple and breathing", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([mergingRoster(new Set(["repo-1"]))]), { timers: fakeTimers() });
    await settle();
    expect([markOf(host, "ws-2")?.classList.contains("ink-grey"), nameOf(host, "ws-2")?.classList.contains("breathes")]).toEqual([
      false,
      true,
    ]);
  });

  it("brings the merge back once the resume is pushed", async () => {
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor([mergingRoster(new Set(["repo-1"])), mergingRoster(new Set())]), { timers: fakeTimers() });
    await settle();
    expect([markOf(host, "ws-1")?.classList.contains("ink-grey"), nameOf(host, "ws-1")?.classList.contains("breathes")]).toEqual([
      false,
      true,
    ]);
  });
});

describe("mounting the rail: one draw per frame", () => {
  /** Mount on a two-push stream, counting the rail's draws. */
  async function mountCounting(host: HTMLElement, deps: SidebarDeps = {}): Promise<{ draws: () => number; handle: ReturnType<typeof mountSidebarNow> }> {
    let draws = 0;
    const handle = mountSidebar(
      host,
      ctxFor([
        roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] }),
        roster({ current: "ws-other", repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-2" })] })] }),
      ]),
      {
        storage: null,
        timers: fakeTimers(),
        wallPhase: {
          animations: () => {
            draws += 1;
            return [];
          },
          now: () => NOW,
        },
        ...deps,
      },
    );
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    return { draws: () => draws, handle };
  }

  it("draws nothing before the frame", async () => {
    // Arrange
    const host = document.createElement("nav");

    // Act
    const { draws } = await mountCounting(host);

    // Assert
    expect([draws(), host.querySelector("[data-roster-row]")]).toEqual([0, null]);
  });

  it("draws a burst of pushes once, at the frame", async () => {
    // Arrange
    const host = document.createElement("nav");
    const { draws } = await mountCounting(host);

    // Act
    frames.drain();

    // Assert
    expect(draws()).toBe(1);
  });

  it("draws the newest push of the burst", async () => {
    // Arrange
    const host = document.createElement("nav");
    await mountCounting(host);

    // Act
    frames.drain();

    // Assert
    expect([
      host.querySelector("[data-roster-row='ws-1']"),
      host.querySelector("[data-roster-row='ws-2']") === null,
    ]).toEqual([null, false]);
  });

  it("asks for one frame for the whole burst", async () => {
    // Act
    await mountCounting(document.createElement("nav"));

    // Assert
    expect(frames.asked()).toBe(1);
  });

  it("draws nothing at a frame that comes after the rail is disposed", async () => {
    // Arrange
    const host = document.createElement("nav");
    const { draws, handle } = await mountCounting(host);

    // Act
    handle?.dispose();
    frames.drain();

    // Assert
    expect([draws(), host.querySelector("[data-roster-row]")]).toEqual([0, null]);
  });
});

describe("mounting the rail: a roster its frame cannot draw", () => {
  /** Mount on PUSHES, its failures recorded. */
  function mountRecording(pushes: WorkspaceRoster[]): { host: HTMLElement; reports: string[] } {
    const host = document.createElement("nav");
    const reports: string[] = [];
    const failures: FailureSink = { report: (kind) => void reports.push(kind.kind.case ?? ""), retract: () => undefined };
    mountSidebar(host, ctxFor(pushes, {}, failures), { storage: null, timers: fakeTimers() });
    return { host, reports };
  }

  /** A roster whose repository section names no repository. */
  const malformed = (): WorkspaceRoster => {
    const bad = roster({ repos: [repoSection({ id: "repo-1" })] });
    const section = bad.repository?.sections[0];
    if (section === undefined) throw new Error("the fixture drew no section");
    section.key = undefined;
    return bad;
  };

  it("files the malformed roster as an undecodable frame", async () => {
    // Act
    const { reports } = mountRecording([malformed()]);
    await settle();

    // Assert
    expect(reports).toEqual(["frameUndecodable"]);
  });

  it("logs the malformed roster at error with the path that refused", async () => {
    // Arrange
    const capture = captureLogRecords("debug");

    // Act
    mountRecording([malformed()]);
    await settle();
    capture.logger.flush();
    await settle();

    // Assert
    const record = capture.sent.find((r) => r.operation === "sidebar.roster-draw-undecodable");
    expect([record?.level.case, record?.context]).toEqual([
      "error",
      expect.objectContaining({ path: "WorkspaceRoster.repository.sections[0].key" }),
    ]);
  });

  it("draws the next roster after one it could not draw", async () => {
    // Arrange
    const pushes = [malformed(), roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] })];
    const host = document.createElement("nav");
    mountSidebar(host, ctxFor(pushes), { storage: null, timers: fakeTimers(), frame: (callback) => callback() });

    // Act
    await settle();

    // Assert
    expect(host.querySelector("[data-roster-row='ws-1']")).not.toBeNull();
  });
});

describe("mounting the rail: a merged row drawn when it comes into view", () => {
  /** A rail whose open merged band holds one row past those drawn at once, the last named NAME. */
  async function mountLong(name: string): Promise<{ host: HTMLElement; standIn: Element }> {
    const host = document.createElement("nav");
    const rows = Array.from({ length: MERGED_DRAWN_AT_ONCE }, (_, i) => row({ id: `ws-${i}`, name: "kept" }));
    rows.push(row({ id: "ws-late", name }));
    mountSidebar(host, ctxFor([roster({ merged: mergedSection(rows, rows.length, false) })]), {
      storage: null,
      timers: fakeTimers(),
    });
    await settle();
    const standIn = host.querySelector(`[${STAND_IN_ATTRIBUTE}]`);
    if (standIn === null) throw new Error("the band drew no stand-in");
    return { host, standIn };
  }

  it("draws the row in its stand-in's place", async () => {
    // Arrange
    const { host, standIn } = await mountLong("late");

    // Act
    fireIntersection(standIn, true);

    // Assert
    expect(host.querySelector("[data-roster-row='ws-late']")?.hasAttribute(STAND_IN_ATTRIBUTE)).toBe(false);
  });

  it("narrows the drawn row to the search the rail already holds", async () => {
    // Arrange
    const { host, standIn } = await mountLong("late");
    search(host, "kept");

    // Act
    fireIntersection(standIn, true);

    // Assert
    expect(host.querySelector("[data-roster-row='ws-late']")?.classList.contains("sb-search-miss")).toBe(true);
  });

  it("stops watching its stand-ins once the rail is disposed", async () => {
    // Arrange
    const host = document.createElement("nav");
    const rows = Array.from({ length: MERGED_DRAWN_AT_ONCE + 1 }, (_, i) => row({ id: `ws-${i}` }));
    const handle = mountSidebar(host, ctxFor([roster({ merged: mergedSection(rows, rows.length, false) })]), {
      storage: null,
      timers: fakeTimers(),
    });
    await settle();
    const standIn = host.querySelector(`[${STAND_IN_ATTRIBUTE}]`);

    // Act
    handle?.dispose();

    // Assert
    expect(() => fireIntersection(standIn as Element, true)).toThrow(/no IntersectionObserver/);
  });
});

