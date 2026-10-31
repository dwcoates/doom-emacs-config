/**
 * memory — the rail's two memory popups (owner request, 2026-10-29;
 * frontend.v1 sidebar_memory.proto).
 *
 * THE VERSION'S HOVER says what agent-repl holds in memory taken as a whole,
 * naming no workspace. AN OPEN ROW'S HOVER adds what that one workspace holds
 * to the row's detail panel. Every figure is the daemon's, measured; this end
 * formats it and derives nothing but the time left before an unload, which it
 * reads off its own clock.
 *
 * FETCHED WHILE OPEN, NEVER PUSHED. A popup asks the daemon when it opens
 * (GetMemoryOverview, GetWorkspaceMemory) and again every REFRESH_TICKS ticks
 * of the shared clock while it stays open, and stops asking when it closes.
 */
import type { AgentReplClient } from "../rpc/client.js";
import type { DaemonContext } from "../rpc/context.js";
import { callUnary } from "../rpc/unary.js";
import { refusalSentence } from "../rpc/refusal.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { log } from "../log.js";
import { nextFrame } from "../frame.js";
import { clampReveal } from "../topbar/clamp.js";
import { GetMemoryOverviewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_memory_overview_pb";
import { GetWorkspaceMemoryResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_workspace_memory_pb";
import type {
  SidebarMemoryOverview,
  SidebarWorkspaceMemory,
  SidebarWorkspaceMemoryPage,
} from "../../../proto/gen/ts/frontend/v1/sidebar_memory_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { SidebarContext } from "./context.js";

/** How many ticks of the shared one-second clock pass between a popup's asks. */
export const REFRESH_TICKS = 2;

/** The class of the version hover's panel. */
export const MEMORY_PANEL_CLASS = "sb-memory";

/** The class an open row's memory block carries inside its detail panel. */
export const ROW_MEMORY_CLASS = "memory";

const MIB = 1024 * 1024;
const GIB = 1024 * MIB;

/** A footprint as the popups state it: "98 MB" below a gigabyte, "2.3 GB" above. */
export function formatBytes(bytes: bigint | number): string {
  const n = Number(bytes);
  if (n >= GIB) return `${(n / GIB).toFixed(1)} GB`;
  return `${Math.round(n / MIB)} MB`;
}

/** A span of milliseconds as the popups state it: "350 ms", "13.6 s". */
export function formatBlocked(ms: bigint | number): string {
  const n = Number(ms);
  if (n < 1000) return `${n} ms`;
  return `${(n / 1000).toFixed(1)} s`;
}

/** The time left before UNIX_MS at NOW_MS: "in 42 min", "in 3 h 5 min", "soon". */
export function formatUnloadIn(unixMs: bigint | number, nowMs: number): string {
  const left = Number(unixMs) - nowMs;
  if (left <= 60_000) return "soon";
  const minutes = Math.round(left / 60_000);
  if (minutes < 60) return `in ${minutes} min`;
  const hours = Math.floor(minutes / 60);
  const rest = minutes % 60;
  return rest === 0 ? `in ${hours} h` : `in ${hours} h ${rest} min`;
}

/** The version hover's lines, as (label, value) pairs in drawn order. */
export function overviewLines(u: SidebarMemoryOverview, path: string): Array<[string, string]> {
  const lines: Array<[string, string]> = [];
  const pages = requireMessage(u.pages, `${path}.pages`);
  const pageCount = `${pages.loaded} of ${pages.loaded + pages.notLoaded} loaded`;
  lines.push([
    "pages",
    pages.footprintBytes === undefined ? `no web view · ${pageCount}` : `${formatBytes(pages.footprintBytes)} · ${pageCount}`,
  ]);
  const strain = requireMessage(u.strain, `${path}.strain`);
  const minutes = Math.round(strain.windowSeconds / 60);
  lines.push([
    "stalls",
    strain.stalls === 0
      ? `none in the last ${minutes} min`
      : `${strain.stalls}, ${formatBlocked(strain.blockedMs)} blocked in the last ${minutes} min`,
  ]);
  const agents = requireCase(requireMessage(u.agents, `${path}.agents`).state, `${path}.agents.state`);
  switch (agents.case) {
    case "none":
      lines.push(["agents", "none running"]);
      break;
    case "running":
      lines.push([
        "agents",
        `${agents.value.count} running · ${formatBytes(agents.value.footprintBytes)} (${formatBytes(agents.value.averageBytes)} each)`,
      ]);
      break;
    default:
      return unreachableArm(`${path}.agents.state`, String((agents as { case: unknown }).case));
  }
  for (const process of u.processes) {
    lines.push([process.label, process.footprintBytes === undefined ? "not running" : formatBytes(process.footprintBytes)]);
  }
  lines.push(["total", formatBytes(requireMessage(u.total, `${path}.total`).footprintBytes)]);
  return lines;
}

/** The page line of a workspace hover, read against NOW_MS. */
export function pageLine(u: SidebarWorkspaceMemoryPage, nowMs: number, path: string): string {
  const state = requireCase(u.state, `${path}.state`);
  switch (state.case) {
    case "notLoaded":
      return "not loaded";
    case "loaded": {
      const release = requireMessage(state.value.release, `${path}.state.loaded.release`);
      const when = requireCase(release.when, `${path}.state.loaded.release.when`);
      switch (when.case) {
        case "held":
          return "loaded, held while working";
        case "onLeave":
          return "loaded, unloads when you leave";
        case "at":
          return `loaded, unloads ${formatUnloadIn(when.value.unixMs, nowMs)}`;
        default:
          return unreachableArm(`${path}.state.loaded.release.when`, String((when as { case: unknown }).case));
      }
    }
    default:
      return unreachableArm(`${path}.state`, String((state as { case: unknown }).case));
  }
}

/** A workspace hover's memory lines, as (label, value) pairs in drawn order. */
export function workspaceLines(u: SidebarWorkspaceMemory, nowMs: number, path: string): Array<[string, string]> {
  const agent = requireCase(requireMessage(u.agent, `${path}.agent`).state, `${path}.agent.state`);
  const lines: Array<[string, string]> = [];
  switch (agent.case) {
    case "running":
      lines.push(["agent", agent.value.footprintBytes === undefined ? "running, not measured yet" : formatBytes(agent.value.footprintBytes)]);
      break;
    case "notRunning":
      lines.push(["agent", "not running"]);
      break;
    default:
      return unreachableArm(`${path}.agent.state`, String((agent as { case: unknown }).case));
  }
  if (u.page !== undefined) lines.push(["page", pageLine(u.page, nowMs, `${path}.page`)]);
  return lines;
}

/** Replace EL's content with LINES as the detail panel's `<dl>`. */
export function fillLines(el: HTMLElement, lines: Array<[string, string]>): void {
  const list = document.createElement("dl");
  for (const [label, value] of lines) {
    const dt = document.createElement("dt");
    dt.textContent = label;
    const dd = document.createElement("dd");
    dd.textContent = value;
    list.append(dt, dd);
  }
  el.replaceChildren(list);
}

/** Replace EL's content with one sentence saying why there are no figures. */
function fillNote(el: HTMLElement, note: string): void {
  const p = document.createElement("p");
  p.className = "summary";
  p.textContent = note;
  el.replaceChildren(p);
}

/** A refresher that runs FETCH now and every REFRESH_TICKS ticks until stopped. */
function refreshWhileOpen(ctx: DaemonContext, fetch: () => void): () => void {
  fetch();
  let ticks = 0;
  return ctx.ticker.subscribe(() => {
    ticks += 1;
    if (ticks % REFRESH_TICKS === 0) fetch();
  });
}

/** Ask for the overview and draw it into PANEL. */
async function fetchOverview(ctx: DaemonContext, panel: HTMLElement): Promise<void> {
  const resp = await callUnary(
    ctx,
    "GetMemoryOverview",
    (client: AgentReplClient) => client.getMemoryOverview({}),
    GetMemoryOverviewResponseSchema,
  );
  const result = requireCase(resp.result, "GetMemoryOverviewResponse.result");
  switch (result.case) {
    case "success": {
      const outcome = requireCase(result.value.outcome, "GetMemoryOverviewSuccess.outcome");
      if (outcome.case === "measured") {
        fillLines(panel, overviewLines(outcome.value, "GetMemoryOverviewSuccess.measured"));
      } else if (outcome.case === "notYetMeasured") {
        fillNote(panel, "not measured yet");
      } else {
        unreachableArm("GetMemoryOverviewSuccess.outcome", String((outcome as { case: unknown }).case));
      }
      return;
    }
    case "error": {
      const cause = requireCase(result.value.cause, "GetMemoryOverviewError.cause");
      if (cause.case !== "measurementFailed") {
        unreachableArm("GetMemoryOverviewError.cause", String((cause as { case: unknown }).case));
      }
      log.warn("the memory overview could not be measured", {
        operation: "sidebar.memory.overview-failed",
        context: { reason: cause.value.reason },
      });
      fillNote(panel, cause.value.reason);
      return;
    }
    default:
      unreachableArm("GetMemoryOverviewResponse.result", String((result as { case: unknown }).case));
  }
}

/**
 * Ask for WORKSPACE's memory and draw it into BLOCK. AT names the daemon that
 * still serves the workspace during a handover, once a `not_yet_adopted`
 * refusal has named it; the page's own daemon is asked otherwise.
 */
async function fetchWorkspace(
  ctx: DaemonContext,
  workspace: WorkspaceRef,
  block: HTMLElement,
  at: string | null = null,
): Promise<void> {
  const unary =
    at === null || ctx.clientAt === undefined
      ? ctx
      : { client: ctx.clientAt(at), failures: ctx.failures, isQuiesced: () => ctx.isQuiesced() };
  const resp = await callUnary(
    unary,
    "GetWorkspaceMemory",
    (client: AgentReplClient) => client.getWorkspaceMemory({ workspace }),
    GetWorkspaceMemoryResponseSchema,
  );
  const result = requireCase(resp.result, "GetWorkspaceMemoryResponse.result");
  switch (result.case) {
    case "success": {
      const memory = requireMessage(result.value.memory, "GetWorkspaceMemorySuccess.memory");
      fillLines(block, workspaceLines(memory, ctx.ticker.now(), "GetWorkspaceMemorySuccess.memory"));
      return;
    }
    case "error": {
      const cause = requireCase(result.value.cause, "GetWorkspaceMemoryError.cause");
      // A HANDOVER IS NEVER DRAWN AS A FAILURE. The daemon still serving the
      // workspace is asked instead, once; with none named, or this one not
      // serving it either, the panel says the workspace is moving.
      if (cause.case === "notYetAdopted") {
        const address = cause.value.address;
        if (at === null && address !== undefined && address !== "" && ctx.clientAt !== undefined) {
          log.info("a workspace's memory is asked of the daemon that still serves it", {
            operation: "sidebar.memory.served-elsewhere",
            context: { workspace: workspace.id, address },
          });
          return fetchWorkspace(ctx, workspace, block, address);
        }
        log.info("a workspace's memory waits on a handover", {
          operation: "sidebar.memory.handover",
          context: { workspace: workspace.id },
        });
        fillNote(block, "moving to the new daemon");
        return;
      }
      const sentence = refusalSentence("GetWorkspaceMemory", cause);
      if (sentence === undefined) {
        unreachableArm("GetWorkspaceMemoryError.cause", String(cause.case));
      }
      log.info("a workspace's memory was refused", {
        operation: "sidebar.memory.workspace-refused",
        context: { workspace: workspace.id, cause: cause.case },
      });
      fillNote(block, sentence);
      return;
    }
    default:
      unreachableArm("GetWorkspaceMemoryResponse.result", String((result as { case: unknown }).case));
  }
}

/** Report a fetch that threw (a transport failure is already logged by `callUnary`). */
function noteFailed(operation: string, err: unknown): void {
  log.error("a memory popup's fetch failed", {
    operation,
    context: { error: err instanceof Error ? err.message : String(err) },
  });
}

/**
 * Hover the rail's version to show the overview. The panel opens on the
 * pointer's arrival, closes on its departure from both the version and the
 * panel, and asks again every REFRESH_TICKS seconds while open.
 */
export function installVersionHover(ctx: DaemonContext, version: HTMLElement, rail: HTMLElement): void {
  const panel = document.createElement("div");
  panel.className = `detail ${MEMORY_PANEL_CLASS}`;
  rail.appendChild(panel);
  let stop: (() => void) | null = null;
  let over = 0;
  const open = (): void => {
    if (stop !== null) return;
    fillNote(panel, "measuring…");
    panel.classList.add("open");
    const place = (): void => {
      const at = clampReveal(version.getBoundingClientRect(), panel.getBoundingClientRect(), {
        width: window.innerWidth,
        height: window.innerHeight,
      });
      panel.style.left = `${at.left}px`;
      panel.style.top = `${at.top}px`;
      panel.style.maxHeight = `${at.maxHeight}px`;
    };
    place();
    stop = refreshWhileOpen(ctx, () => {
      fetchOverview(ctx, panel).then(place, (err: unknown) => noteFailed("sidebar.memory.overview-fetch", err));
    });
  };
  const close = (): void => {
    if (stop === null) return;
    stop();
    stop = null;
    panel.classList.remove("open");
  };
  for (const el of [version, panel]) {
    el.addEventListener("mouseenter", () => {
      over += 1;
      open();
    });
    el.addEventListener("mouseleave", () => {
      over = Math.max(0, over - 1);
      // The pointer crossing from the version into the panel leaves one
      // before it enters the other; the next frame decides.
      nextFrame(() => {
        if (over === 0) close();
      });
    });
  }
}

/** An open row's memory: what it fetches, and its block once the panel first opened. */
interface RowMemory {
  sc: SidebarContext;
  workspace: WorkspaceRef;
  detail: HTMLElement;
  block: HTMLElement | null;
  stop: (() => void) | null;
}

/** Each open row's memory, keyed by its row element. */
const rowMemories = new WeakMap<HTMLElement, RowMemory>();

/**
 * Give open row WS a memory block in its DETAIL panel, fetched while the
 * panel is open (`showRowMemory`). NOTHING IS DRAWN OR REGISTERED UNTIL THE
 * PANEL FIRST OPENS: most rows are never hovered, and a row's drawing stays
 * exactly what the roster states until then.
 */
export function addRowMemory(sc: SidebarContext, ws: HTMLElement, detail: HTMLElement, workspace: WorkspaceRef): void {
  rowMemories.set(ws, { sc, workspace, detail, block: null, stop: null });
}

/** Start or stop row WS's memory fetches as its panel opens or closes. */
export function showRowMemory(ws: HTMLElement, open: boolean): void {
  const row = rowMemories.get(ws);
  if (row === undefined) return;
  if (!open) {
    row.stop?.();
    row.stop = null;
    return;
  }
  if (row.stop !== null) return;
  if (row.block === null) {
    const block = document.createElement("div");
    block.className = ROW_MEMORY_CLASS;
    row.detail.appendChild(block);
    row.block = block;
    // A REDRAW THROWS THE ROW AWAY without closing its panel, so the
    // drawing's own teardown stops its fetches; the new copy starts its own
    // when opened.
    row.sc.onDispose(() => showRowMemory(ws, false));
  }
  const block = row.block;
  row.stop = refreshWhileOpen(row.sc.ctx, () => {
    fetchWorkspace(row.sc.ctx, row.workspace, block).catch((err: unknown) =>
      noteFailed("sidebar.memory.workspace-fetch", err),
    );
  });
}
