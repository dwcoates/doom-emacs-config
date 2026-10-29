// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { GetMemoryOverviewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_memory_overview_pb";
import {
  GetWorkspaceMemoryResponseSchema,
  type GetWorkspaceMemoryRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_workspace_memory_pb";
import {
  SidebarMemoryAgentsSchema,
  SidebarMemoryOverviewSchema,
  SidebarMemoryPagesSchema,
  SidebarMemoryStrainSchema,
  SidebarWorkspaceMemoryPageSchema,
  SidebarWorkspaceMemorySchema,
  type SidebarMemoryOverview,
} from "../../../proto/gen/ts/frontend/v1/sidebar_memory_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  addRowMemory,
  formatBlocked,
  formatBytes,
  formatUnloadIn,
  installVersionHover,
  overviewLines,
  pageLine,
  showRowMemory,
  workspaceLines,
} from "../../src/sidebar/memory.js";
import type { SidebarContext } from "../../src/sidebar/context.js";
import { appContext, fakeTicker } from "./harness.js";

const MIB = 1024 * 1024;
const GIB = 1024 * MIB;

beforeEach(() => {
  vi.stubGlobal("requestAnimationFrame", (fn: FrameRequestCallback) => {
    fn(0);
    return 0;
  });
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
  document.body.replaceChildren();
});

/** Let every pending promise settle. */
const settle = () => new Promise((resolve) => globalThis.setTimeout(resolve, 0));

/** A whole overview, every line set, then EDIT applied to it. */
function overview(edit: (o: SidebarMemoryOverview) => void = () => undefined): SidebarMemoryOverview {
  const o = create(SidebarMemoryOverviewSchema, {
    pages: { footprintBytes: BigInt(Math.round(2.3 * GIB)), loaded: 14, notLoaded: 44 },
    strain: { stalls: 31, blockedMs: 13_600n, windowSeconds: 600 },
    agents: { state: { case: "running", value: { count: 22, footprintBytes: BigInt(22 * 66 * MIB), averageBytes: BigInt(66 * MIB) } } },
    processes: [
      { label: "daemon", footprintBytes: BigInt(925 * MIB) },
      { label: "Emacs", footprintBytes: BigInt(505 * MIB) },
      { label: "store" },
      { label: "sidecar", footprintBytes: BigInt(40 * MIB) },
    ],
    total: { footprintBytes: BigInt(Math.round(5.6 * GIB)) },
  });
  edit(o);
  return o;
}

describe("formatting", () => {
  it.each([
    [98 * MIB, "98 MB"],
    [2.3 * GIB, "2.3 GB"],
    [GIB, "1.0 GB"],
  ])("states %d bytes as %s", (bytes, want) => {
    expect(formatBytes(bytes)).toBe(want);
  });

  it.each([
    [350, "350 ms"],
    [13_600, "13.6 s"],
  ])("states %d blocked milliseconds as %s", (ms, want) => {
    expect(formatBlocked(ms)).toBe(want);
  });

  it.each([
    [30_000, "soon"],
    [-5_000, "soon"],
    [42 * 60_000, "in 42 min"],
    [120 * 60_000, "in 2 h"],
    [185 * 60_000, "in 3 h 5 min"],
  ])("states an unload %d ms away as %s", (left, want) => {
    expect(formatUnloadIn(1_000_000 + left, 1_000_000)).toBe(want);
  });
});

describe("the version hover's lines", () => {
  it("states every line in drawn order", () => {
    expect(overviewLines(overview(), "o")).toEqual([
      ["pages", "2.3 GB · 14 of 58 loaded"],
      ["stalls", "31, 13.6 s blocked in the last 10 min"],
      ["agents", "22 running · 1.4 GB (66 MB each)"],
      ["daemon", "925 MB"],
      ["Emacs", "505 MB"],
      ["store", "not running"],
      ["sidecar", "40 MB"],
      ["total", "5.6 GB"],
    ]);
  });

  it("says no web view when the page process is not running", () => {
    const lines = overviewLines(overview((o) => (o.pages = create(SidebarMemoryPagesSchema, { loaded: 0, notLoaded: 3 }))), "o");
    expect(lines[0]).toEqual(["pages", "no web view · 0 of 3 loaded"]);
  });

  it("says no stalls when the window held none", () => {
    const lines = overviewLines(overview((o) => (o.strain = create(SidebarMemoryStrainSchema, { stalls: 0, blockedMs: 0n, windowSeconds: 600 }))), "o");
    expect(lines[1]).toEqual(["stalls", "none in the last 10 min"]);
  });

  it("says none running when no shim runs", () => {
    const lines = overviewLines(overview((o) => (o.agents = create(SidebarMemoryAgentsSchema, { state: { case: "none", value: {} } }))), "o");
    expect(lines[2]).toEqual(["agents", "none running"]);
  });

  it("refuses an overview with no agents arm", () => {
    expect(() => overviewLines(overview((o) => (o.agents = create(SidebarMemoryAgentsSchema, {}))), "o")).toThrow();
  });
});

describe("a workspace hover's lines", () => {
  const page = (when: Parameters<typeof create<typeof SidebarWorkspaceMemoryPageSchema>>[1]) => create(SidebarWorkspaceMemoryPageSchema, when);

  it.each([
    ["held", { state: { case: "loaded" as const, value: { release: { when: { case: "held" as const, value: {} } } } } }, "loaded, held while working"],
    ["on leave", { state: { case: "loaded" as const, value: { release: { when: { case: "onLeave" as const, value: {} } } } } }, "loaded, unloads when you leave"],
    ["at", { state: { case: "loaded" as const, value: { release: { when: { case: "at" as const, value: { unixMs: 1_000_000n + 42n * 60_000n } } } } } }, "loaded, unloads in 42 min"],
    ["not loaded", { state: { case: "notLoaded" as const, value: {} } }, "not loaded"],
  ])("states a page %s", (_name, init, want) => {
    expect(pageLine(page(init), 1_000_000, "p")).toBe(want);
  });

  it("states a running shim's footprint", () => {
    const memory = create(SidebarWorkspaceMemorySchema, { agent: { state: { case: "running", value: { footprintBytes: BigInt(98 * MIB) } } } });
    expect(workspaceLines(memory, 0, "w")).toEqual([["agent", "98 MB"]]);
  });

  it("states a running shim not measured yet", () => {
    const memory = create(SidebarWorkspaceMemorySchema, { agent: { state: { case: "running", value: {} } } });
    expect(workspaceLines(memory, 0, "w")).toEqual([["agent", "running, not measured yet"]]);
  });

  it("states a shim not running, and the page when reported", () => {
    const memory = create(SidebarWorkspaceMemorySchema, {
      agent: { state: { case: "notRunning", value: {} } },
      page: { state: { case: "notLoaded", value: {} } },
    });
    expect(workspaceLines(memory, 0, "w")).toEqual([
      ["agent", "not running"],
      ["page", "not loaded"],
    ]);
  });
});

describe("the version hover", () => {
  function mounted(answer: () => ReturnType<typeof create<typeof GetMemoryOverviewResponseSchema>>) {
    const ticker = fakeTicker();
    let asked = 0;
    const ctx = appContext(
      {
        getMemoryOverview: () => {
          asked += 1;
          return answer();
        },
      },
      ticker,
    );
    const head = document.createElement("div");
    const version = document.createElement("span");
    head.appendChild(version);
    document.body.appendChild(head);
    installVersionHover(ctx, version, head);
    const panel = head.querySelector<HTMLElement>(".sb-memory")!;
    return { ticker, version, panel, asked: () => asked };
  }

  const measured = () =>
    create(GetMemoryOverviewResponseSchema, { result: { case: "success", value: { outcome: { case: "measured", value: overview() } } } });

  it("opens on hover and draws the overview", async () => {
    const { version, panel } = mounted(measured);

    version.dispatchEvent(new MouseEvent("mouseenter"));
    await settle();

    expect([panel.classList.contains("open"), panel.querySelector("dt")?.textContent]).toEqual([true, "pages"]);
  });

  it("asks again every second tick while open", async () => {
    const { version, ticker, asked } = mounted(measured);
    version.dispatchEvent(new MouseEvent("mouseenter"));
    await settle();

    ticker.tick(1);
    ticker.tick(2);
    await settle();

    expect(asked()).toBe(2);
  });

  it("stops asking and closes when the pointer leaves", async () => {
    const { version, panel, ticker } = mounted(measured);
    version.dispatchEvent(new MouseEvent("mouseenter"));
    await settle();

    version.dispatchEvent(new MouseEvent("mouseleave"));

    expect([panel.classList.contains("open"), ticker.subscribers()]).toEqual([false, 0]);
  });

  it("stays open while the pointer crosses into the panel", async () => {
    const { version, panel } = mounted(measured);
    version.dispatchEvent(new MouseEvent("mouseenter"));
    await settle();

    panel.dispatchEvent(new MouseEvent("mouseenter"));
    version.dispatchEvent(new MouseEvent("mouseleave"));

    expect(panel.classList.contains("open")).toBe(true);
  });

  it("says not measured yet before the first sample", async () => {
    const { version, panel } = mounted(() =>
      create(GetMemoryOverviewResponseSchema, { result: { case: "success", value: { outcome: { case: "notYetMeasured", value: {} } } } }),
    );

    version.dispatchEvent(new MouseEvent("mouseenter"));
    await settle();

    expect(panel.textContent).toBe("not measured yet");
  });

  it("states a failed measurement's reason", async () => {
    const { version, panel } = mounted(() =>
      create(GetMemoryOverviewResponseSchema, {
        result: { case: "error", value: { cause: { case: "measurementFailed", value: { reason: "the sample failed" } } } },
      }),
    );

    version.dispatchEvent(new MouseEvent("mouseenter"));
    await settle();

    expect(panel.textContent).toBe("the sample failed");
  });
});

describe("an open row's memory", () => {
  function rowWith(answer: () => ReturnType<typeof create<typeof GetWorkspaceMemoryResponseSchema>>) {
    const ticker = fakeTicker();
    const sent: GetWorkspaceMemoryRequest[] = [];
    const ctx = appContext(
      {
        getWorkspaceMemory: (request) => {
          sent.push(request);
          return answer();
        },
      },
      ticker,
    );
    const disposers: Array<() => void> = [];
    const sc = { ctx, onDispose: (fn: () => void) => disposers.push(fn) } as unknown as SidebarContext;
    const ws = document.createElement("div");
    const detail = document.createElement("div");
    ws.appendChild(detail);
    addRowMemory(sc, ws, detail, create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }));
    return { ws, detail, ticker, sent, dispose: () => disposers.forEach((fn) => fn()) };
  }

  const notRunning = () =>
    create(GetWorkspaceMemoryResponseSchema, {
      result: { case: "success", value: { memory: { agent: { state: { case: "notRunning", value: {} } } } } },
    });

  it("asks for the row's workspace when its panel opens, and draws the lines", async () => {
    const { ws, detail, sent } = rowWith(notRunning);

    showRowMemory(ws, true);
    await settle();

    expect([sent.map((r) => r.workspace?.id), detail.querySelector(".memory dd")?.textContent]).toEqual([["ws-1"], "not running"]);
  });

  it("asks nothing and draws nothing while the panel is closed", async () => {
    const { sent, detail } = rowWith(notRunning);

    await settle();

    expect([sent, detail.childElementCount]).toEqual([[], 0]);
  });

  it("stops asking when the panel closes", async () => {
    const { ws, ticker } = rowWith(notRunning);
    showRowMemory(ws, true);

    showRowMemory(ws, false);

    expect(ticker.subscribers()).toBe(0);
  });

  it("stops asking when its drawing is thrown away", async () => {
    const { ws, ticker, dispose } = rowWith(notRunning);
    showRowMemory(ws, true);

    dispose();

    expect(ticker.subscribers()).toBe(0);
  });

  it("states a refusal in the shared wording", async () => {
    const { ws, detail } = rowWith(() =>
      create(GetWorkspaceMemoryResponseSchema, { result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } } }),
    );

    showRowMemory(ws, true);
    await settle();

    expect(detail.querySelector(".memory")?.textContent).toBe("the daemon has not finished adopting this workspace yet");
  });
});
