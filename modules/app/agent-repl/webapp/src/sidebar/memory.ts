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
import { createLogger } from "../log.js";
import { nextFrame } from "../frame.js";
import { clampReveal } from "../topbar/clamp.js";
import { applyPlacement, viewportOf } from "../placement.js";
import { GetMemoryOverviewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_memory_overview_pb";
import { GetWorkspaceMemoryResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_workspace_memory_pb";
import type {
  SidebarMemoryOverview,
  SidebarMemoryWorkspaces,
  SidebarWorkspaceMemory,
  SidebarWorkspaceMemoryPage,
} from "../../../proto/gen/ts/frontend/v1/sidebar_memory_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { SidebarContext } from "./context.js";
import {
  childPopup,
  detailsPanel,
  labelChildPopup,
  placeDetailsPanel,
  requireFactHome,
  syncDetailsEntry,
  wireDetailsEntry,
} from "./detail-more.js";
import { contextAt, handoverAddress, isHandoverArm } from "./handover.js";
import type { Dropdowns } from "./dropdowns.js";
import type { PopupMirror } from "../host/sidebar-popups.js";

const log = createLogger("webapp.sidebar.memory");

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

/** The version hover's lines below its workspaces entry, as (label, value) pairs in drawn order. */
export function overviewLines(u: SidebarMemoryOverview, path: string): Array<[string, string]> {
  const lines: Array<[string, string]> = [];
  const strain = requireMessage(u.strain, `${path}.strain`);
  const minutes = Math.round(strain.windowSeconds / 60);
  lines.push([
    "stalls",
    strain.stalls === 0
      ? `none in the last ${minutes} min`
      : `${strain.stalls}, ${formatBlocked(strain.blockedMs)} blocked in the last ${minutes} min`,
  ]);
  for (const process of u.processes) {
    lines.push([process.label, process.footprintBytes === undefined ? "not running" : formatBytes(process.footprintBytes)]);
  }
  lines.push(["total", formatBytes(requireMessage(u.total, `${path}.total`).footprintBytes)]);
  return lines;
}

/** The label of the version hover's workspaces entry. */
export const WORKSPACES_LABEL = "workspaces";

/** The workspaces entry's words: the three counts in short form. */
export function workspacesSummary(u: SidebarMemoryWorkspaces, path: string): string {
  const visible = requireMessage(u.visible, `${path}.visible`);
  const shims = requireMessage(u.shims, `${path}.shims`);
  const pages = requireMessage(u.pages, `${path}.pages`);
  return `${WORKSPACES_LABEL} · ${visible.visible} visible · ${shims.up} shims · ${pages.loaded} webpages`;
}

/**
 * The workspaces entry's child popup lines, as (label, value) pairs in drawn
 * order. A count of visible workspaces is never called loaded: only the web
 * view's pages are.
 */
export function workspacesLines(u: SidebarMemoryWorkspaces, path: string): Array<[string, string]> {
  const visible = requireMessage(u.visible, `${path}.visible`);
  const shims = requireMessage(u.shims, `${path}.shims`);
  const pages = requireMessage(u.pages, `${path}.pages`);
  return [
    ["workspaces visible", `${visible.visible} of ${visible.open} open`],
    [
      "workspace shims",
      shims.footprintBytes === undefined || shims.averageBytes === undefined
        ? `${shims.up} up`
        : `${shims.up} up · ${formatBytes(shims.footprintBytes)} (${formatBytes(shims.averageBytes)} each)`,
    ],
    [
      "workspace loaded webpages",
      pages.footprintBytes === undefined ? `${pages.loaded} · no web view` : `${pages.loaded} · ${formatBytes(pages.footprintBytes)}`,
    ],
  ];
}

/**
 * The page line of a workspace hover: whether the web view holds the page.
 * Visibility only makes a page eligible, so a visible workspace's page may
 * be not loaded.
 */
export function pageLine(u: SidebarWorkspaceMemoryPage, path: string): string {
  const state = requireCase(u.state, `${path}.state`);
  switch (state.case) {
    case "notLoaded":
      return "not loaded";
    case "loaded":
      return "loaded";
    default:
      return unreachableArm(`${path}.state`, String((state as { case: unknown }).case));
  }
}

/**
 * A workspace hover's memory lines, as (label, value) pairs in drawn order.
 * Every one is a detail (detail-more.ts `FACT_HOMES`), drawn in the panel's
 * details popup; a line classified otherwise is refused.
 */
export function workspaceLines(u: SidebarWorkspaceMemory, path: string): Array<[string, string]> {
  const lines = workspaceMemoryLines(u, path);
  for (const [label] of lines) requireFactHome(label, "details");
  return lines;
}

function workspaceMemoryLines(u: SidebarWorkspaceMemory, path: string): Array<[string, string]> {
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
  lines.push(["page", pageLine(requireMessage(u.page, `${path}.page`), `${path}.page`)]);
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

/** The version panel's lines, below its workspaces entry. */
const OVERVIEW_LINES_CLASS = "lines";

/** PANEL's lines block, which every fill replaces. */
function overviewBlock(panel: HTMLElement): HTMLElement {
  const block = panel.querySelector<HTMLElement>(`:scope > .${OVERVIEW_LINES_CLASS}`);
  if (block === null) {
    throw new Error("agent-repl: the version hover's panel has no lines block");
  }
  return block;
}

/**
 * Draw a measured overview U into PANEL: its workspaces entry first (the row
 * panel's details mechanism, detail-more.ts), whose child popup holds the
 * three counts, then the other lines.
 */
export function fillOverview(panel: HTMLElement, u: SidebarMemoryOverview, path: string): void {
  const workspaces = requireMessage(u.workspaces, `${path}.workspaces`);
  const sub = childPopup(panel, WORKSPACES_LABEL);
  const entry = sub.parentElement;
  if (entry !== null && panel.firstElementChild !== entry) panel.prepend(entry);
  labelChildPopup(panel, workspacesSummary(workspaces, `${path}.workspaces`));
  fillLines(sub, workspacesLines(workspaces, `${path}.workspaces`));
  syncDetailsEntry(panel);
  fillLines(overviewBlock(panel), overviewLines(u, path));
}

/** Draw NOTE into PANEL in place of the figures, its workspaces entry hidden. */
function fillOverviewNote(panel: HTMLElement, note: string): void {
  const entry = panel.querySelector<HTMLElement>(":scope > .detail-more > .detail-sub");
  entry?.replaceChildren();
  syncDetailsEntry(panel);
  fillNote(overviewBlock(panel), note);
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
        fillOverview(panel, outcome.value, "GetMemoryOverviewSuccess.measured");
      } else if (outcome.case === "notYetMeasured") {
        fillOverviewNote(panel, "not measured yet");
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
      fillOverviewNote(panel, cause.value.reason);
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
  const unary = (at === null ? null : contextAt(ctx, at)) ?? ctx;
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
      fillLines(block, workspaceLines(memory, "GetWorkspaceMemorySuccess.memory"));
      return;
    }
    case "error": {
      const cause = requireCase(result.value.cause, "GetWorkspaceMemoryError.cause");
      // A HANDOVER IS NEVER DRAWN AS A FAILURE, at either end of it
      // (sidebar/handover.ts). The daemon serving the workspace is asked
      // instead, once; with none named, or this one not serving it either,
      // the panel says the workspace is moving.
      if (isHandoverArm(cause.case)) {
        const address = handoverAddress("GetWorkspaceMemoryError.cause", cause);
        if (at === null && address !== null && ctx.clientAt !== undefined) {
          log.info("a workspace's memory is asked of the daemon that serves it", {
            operation: "sidebar.memory.served-elsewhere",
            context: { workspace: workspace.id, arm: cause.case, address },
          });
          return fetchWorkspace(ctx, workspace, block, address);
        }
        log.info("a workspace's memory waits on a handover", {
          operation: "sidebar.memory.handover",
          context: { workspace: workspace.id, arm: cause.case },
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

/** The key the version hover's panel is mirrored under (host/sidebar-popups.ts): no workspace id. */
export const VERSION_POPUP_KEY = "sb-version";

/** What the version hover answers to besides the pointer. */
export interface VersionHoverOptions {
  /** Whether the hover must stay shut: the version's click opened its notices (notices.ts). */
  readonly suppressed?: () => boolean;
  /**
   * The rail's dropdowns: the open hover is one, so a click elsewhere in the
   * rail, another dropdown opening, or the user leaving the rail
   * (transients.ts) closes it.
   */
  readonly dropdowns?: Dropdowns;
  /**
   * The mirror drawing the panel over the feed, beside the version's line, as
   * an open row's is (host/sidebar-popups.ts); the panel stands under the
   * version, in the rail, without one.
   */
  readonly mirror?: PopupMirror | null;
}

/**
 * Hover the rail's version to show the overview. The panel opens on the
 * pointer's arrival, closes on its departure from both the version and the
 * panel, and asks again every REFRESH_TICKS seconds while open.
 */
export function installVersionHover(
  ctx: DaemonContext,
  version: HTMLElement,
  rail: HTMLElement,
  options: VersionHoverOptions = {},
): void {
  const suppressed = options.suppressed ?? (() => false);
  const dropdowns = options.dropdowns ?? null;
  const mirror = options.mirror ?? null;
  const panel = document.createElement("div");
  panel.className = `detail ${MEMORY_PANEL_CLASS}`;
  const lines = document.createElement("div");
  lines.className = OVERVIEW_LINES_CLASS;
  panel.appendChild(lines);
  rail.appendChild(panel);
  // THE WORKSPACES ENTRY OPENS ITS CHILD POPUP as a row panel's details entry
  // does (detail-more.ts); over the feed the mirror wires its copy itself.
  wireDetailsEntry(panel, () => {
    placeDetailsPanel(panel, (el) => el.getBoundingClientRect(), viewportOf());
  });
  let stop: (() => void) | null = null;
  let over = 0;
  /** Close the panel; RELEASE when its own gesture closed it, not the dropdowns. */
  const close = (release: boolean): void => {
    if (stop === null) return;
    stop();
    stop = null;
    panel.classList.remove("open");
    if (release) dropdowns?.released(panel);
    mirror?.hide(VERSION_POPUP_KEY);
  };
  const open = (): void => {
    // The version's click opened its notices (notices.ts): the hover stays shut under them.
    if (stop !== null || suppressed()) return;
    fillOverviewNote(panel, "measuring…");
    panel.classList.add("open");
    // REGISTERED BEFORE IT IS MIRRORED: the row panel it closes takes its
    // copy off the feed first.
    dropdowns?.opened({
      kind: "version-memory",
      element: panel,
      openers: [version],
      close: () => {
        close(false);
      },
    });
    const place = (): void => {
      // THE SAME PANEL SHOWN AGAIN is placed again (sidebar-popups.ts).
      if (mirror !== null) {
        mirror.show(VERSION_POPUP_KEY, panel, version);
        return;
      }
      applyPlacement(panel, clampReveal(version.getBoundingClientRect(), panel.getBoundingClientRect(), viewportOf()));
    };
    place();
    stop = refreshWhileOpen(ctx, () => {
      fetchOverview(ctx, panel).then(place, (err: unknown) => noteFailed("sidebar.memory.overview-fetch", err));
    });
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
        if (over === 0) close(true);
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
    // A WORKSPACE'S MEMORY IS A DETAIL (detail-more.ts): its block is drawn
    // in the panel's details popup, whose entry shows once it is filled.
    detailsPanel(row.detail).appendChild(block);
    row.block = block;
    // A REDRAW THROWS THE ROW AWAY without closing its panel, so the
    // drawing's own teardown stops its fetches; the new copy starts its own
    // when opened.
    row.sc.onDispose(() => showRowMemory(ws, false));
  }
  const block = row.block;
  row.stop = refreshWhileOpen(row.sc.ctx, () => {
    fetchWorkspace(row.sc.ctx, row.workspace, block).then(
      () => syncDetailsEntry(row.detail),
      (err: unknown) => noteFailed("sidebar.memory.workspace-fetch", err),
    );
  });
}
