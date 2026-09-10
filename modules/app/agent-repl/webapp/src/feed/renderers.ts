/**
 * renderers — THE FEED'S SEAMS: what a row renderer is handed, what the feed
 * expects back, and how a bubble's body draws a sub-feed.
 *
 * WHY THE SEAM EXISTS AT ALL. The feed universe (pages, the tail, upserts,
 * bubbles, nesting, reveal) is ONE mechanism, and the cards are many
 * independent drawings. Splitting them here lets the mechanism be written and
 * tested once while each card is written against its own message — and it is
 * what makes the PARITY INVARIANT checkable: a subagent bubble and a merge
 * bubble differ only in which head renderer and which body renderer they are
 * given, never in the plumbing beneath them.
 *
 * A RENDERER IS A PURE FUNCTION of (message, RowContext) → DOM. Re-pushes
 * redraw a row whole, so a renderer must keep no state outside the DOM it
 * returns, with two sanctioned exceptions: the shared ticker (clocks) and local
 * UI toggles, which it re-applies from `data-*` attributes on `rc.previous` —
 * the element its own previous draw produced.
 */
import type { AppContext } from "../rpc/context.js";
import { drawFeedCommandPanel } from "../panels/panels.js";
import { drawFeedCommandRefused } from "../panels/refused.js";
import { drawFeedArtifact } from "./cards/artifact.js";
import { drawFeedFindings } from "./cards/findings.js";
import { drawFeedHook } from "./cards/hook.js";
import { drawFeedPlan } from "./cards/plan.js";
import { drawFeedResponse } from "./cards/response.js";
import { drawFeedShell } from "./cards/shell.js";
import { drawFeedSkill } from "./cards/skill.js";
import { drawFeedSimpleToolCall } from "./cards/tool-call.js";
import { drawFeedColdGate } from "./asks/cold-gate.js";
import { drawFeedPermission } from "./asks/permission.js";
import { drawFeedQuestion } from "./asks/question.js";
import { drawFeedMerge } from "./merge/merge.js";
import { mergeBubbleBody } from "./merge/merge-body.js";
import type { Handle } from "../failure/overlay.js";
import { log } from "../log.js";
import type {
  FeedArtifact,
  FeedBreadcrumb,
  FeedColdGate,
  FeedCommandPanel,
  FeedCommandRefused,
  FeedFindings,
  FeedHook,
  FeedId,
  FeedMerge,
  FeedPermission,
  FeedPlan,
  FeedQuestion,
  FeedResponse,
  FeedRow,
  FeedShell,
  FeedSimpleToolCall,
  FeedSkill,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";

export type { Handle };

/** What every row renderer is handed. */
export interface RowContext {
  /** The page's one context: client, workspace, ticker, failures. */
  ctx: AppContext;
  /** Which feed this row is on — the root feed, or a bubble's own id. */
  feed: FeedId | "root";
  /** The whole row, so a renderer can reach its id, turn and placement arm. */
  row: FeedRow;
  /** Bring another row into view, opening bubbles along the way if needed. */
  readonly revealRow: (id: FeedId) => Promise<boolean>;
  /**
   * The element this row's PREVIOUS draw produced, when it had one.
   *
   * The ONLY channel for local UI state across a re-push: a renderer reads its
   * own `data-*` off it (a fold the reader toggled, a section they expanded)
   * and re-applies it, so a push can never undo the reader's own act.
   */
  previous?: HTMLElement;
  /**
   * Find another row's element ON THIS FEED. Provided by the controller that
   * owns the rows; a renderer with nothing to look up simply never calls it.
   */
  findRowElement?(id: FeedId): HTMLElement | null;
}

/**
 * The card renderers the feed dispatches to, one per drawn message.
 *
 * Each returns the row's BODY element; the feed wraps it in the row chrome
 * (the `<article>` with the identity attributes and the nesting slot), so no
 * renderer draws chrome and none of them can disagree about it.
 */
export interface RowRenderers {
  response(unit: FeedResponse, rc: RowContext): HTMLElement;
  simpleToolCall(unit: FeedSimpleToolCall, rc: RowContext): HTMLElement;
  hook(unit: FeedHook, rc: RowContext): HTMLElement;
  skill(unit: FeedSkill, rc: RowContext): HTMLElement;
  artifact(unit: FeedArtifact, rc: RowContext): HTMLElement;
  plan(unit: FeedPlan, rc: RowContext): HTMLElement;
  findings(unit: FeedFindings, rc: RowContext): HTMLElement;
  shell(unit: FeedShell, rc: RowContext): HTMLElement;
  permission(unit: FeedPermission, rc: RowContext): HTMLElement;
  question(unit: FeedQuestion, rc: RowContext): HTMLElement;
  coldGate(unit: FeedColdGate, rc: RowContext): HTMLElement;
  /** The merge bubble's collapsed HEAD line; its body is `mergeBody`. */
  mergeHead(unit: FeedMerge, rc: RowContext): HTMLElement;
  commandPanel(unit: FeedCommandPanel, rc: RowContext): HTMLElement;
  commandRefused(unit: FeedCommandRefused, rc: RowContext): HTMLElement;
  /** The merge bubble's body: the tab strip over its sub-feed's tab rows. */
  mergeBody: BubbleBodyRenderer;
}

/**
 * A sub-feed as its body renderer sees it: the rows, a change signal, the
 * ordinary row-drawing path, the breadcrumbs, and the bubble's composer slot.
 *
 * The body renderer draws from THIS rather than from a controller, which is
 * what keeps a body renderer from reaching into the plumbing — the merge body's
 * one legitimate difference is the tab strip, not a second loader.
 */
export interface SubfeedView {
  /** Every row of this feed, oldest → newest, merge tabs included. */
  rows(): readonly FeedRow[];
  /** Run FN whenever the rows changed. Returns its unsubscriber. */
  onChange(fn: () => void): () => void;
  /** This row, drawn the ordinary way, chrome included. */
  drawRow(row: FeedRow): HTMLElement;
  /** The containers above this feed, outermost first. Empty at a feed's top. */
  breadcrumbs(): readonly FeedBreadcrumb[];
  /** Where the bubble's own composer draws, when this build has one (R7). */
  composerSlot?: HTMLElement;
}

/** How a bubble's body draws its sub-feed into MOUNT. */
export type BubbleBodyRenderer = (
  mount: HTMLElement,
  view: SubfeedView,
  rc: RowContext,
) => Handle;

/** A per-bubble composer, mounted into SLOT and addressed to FEED. */
export type ComposerFactory = (host: HTMLElement, feed: FeedId) => Handle;

/**
 * THE REGISTRY: every row renderer this build has, assembled once.
 *
 * WHY IT IS ASSEMBLED HERE rather than imported by the feed. The feed core is
 * the mechanism (pages, the tail, upserts, bubbles, reveal) and the cards are
 * the drawings; the feed importing all fifteen would put the mechanism above
 * every card in the import graph and make the seam unusable — a card could not
 * be tested, or replaced, without the whole feed behind it. So the seam is a
 * plain record, this function is the ONE place it is filled in, and both
 * `main.ts` and the integration harness call exactly this. A key added to
 * `RowRenderers` and not here does not compile.
 *
 * CTX is taken (and not yet read) because the seam belongs to the page, not to
 * a row: every renderer is handed its own `RowContext` at draw time. Taking it
 * keeps the call site honest — the registry is built once per page, after the
 * context exists — and leaves room for a renderer that needs a page-level fact
 * without changing every caller.
 */
export function createRowRenderers(_ctx: AppContext): RowRenderers {
  log.debug("assembling the feed's row renderers", {
    operation: "feed.renderers.assemble",
  });
  return {
    response: drawFeedResponse,
    simpleToolCall: drawFeedSimpleToolCall,
    hook: drawFeedHook,
    skill: drawFeedSkill,
    artifact: drawFeedArtifact,
    plan: drawFeedPlan,
    findings: drawFeedFindings,
    shell: drawFeedShell,
    permission: drawFeedPermission,
    question: drawFeedQuestion,
    coldGate: drawFeedColdGate,
    mergeHead: drawFeedMerge,
    commandPanel: drawFeedCommandPanel,
    commandRefused: drawFeedCommandRefused,
    mergeBody: mergeBubbleBody,
  };
}

/**
 * The arm a oneof selected, as a plain string.
 *
 * Written for the DEFAULT branch of an exhaustive arm switch: TypeScript has
 * narrowed the oneof to `never` by then (which is the compile-time half of the
 * exhaustiveness check), so reading `.case` off it directly does not type —
 * while at RUNTIME the value is a real oneof carrying the arm a newer daemon
 * set and this build has no case for, which is exactly the name the refusal
 * must quote.
 */
export function armName(oneof: { case: string }): string {
  return oneof.case;
}

/** The attribute a container row's nesting slot carries. */
export const NEST_ATTRIBUTE = "data-nest";

/**
 * THE DEFAULT BUBBLE BODY: a subagent bubble's rows, exactly as the feed draws
 * rows anywhere else.
 *
 * The whole body is redrawn on every change rather than diffed. The elements
 * themselves are the controller's and are REUSED (appending an element that is
 * already somewhere moves it), so a redraw is a re-ordering of existing nodes,
 * not a rebuild of them — which is what keeps a bubble's open folds, expanded
 * sections and live clocks across a push.
 */
export const defaultBubbleBody: BubbleBodyRenderer = (mount, view, rc) => {
  const breadcrumbs = document.createElement("div");
  breadcrumbs.className = "feed-breadcrumbs";
  breadcrumbs.setAttribute("data-breadcrumbs", "");

  const rows = document.createElement("div");
  rows.className = "feed-rows";

  const draw = (): void => {
    drawBreadcrumbTrail(breadcrumbs, view.breadcrumbs(), rc, mount);
    arrangeSubfeedRows(rows, view);
    if (view.composerSlot !== undefined) mount.append(view.composerSlot);
  };

  mount.append(rows);
  draw();
  const unsubscribe = view.onChange(draw);
  return {
    dispose(): void {
      unsubscribe();
      breadcrumbs.remove();
      rows.remove();
    },
  };
};

/**
 * The breadcrumb header line — drawn ONLY when the trail is non-empty (R6).
 *
 * A crumb is a jump target, and the jump is the feed's own reveal rather than a
 * navigation: sub-feed expansion is INLINE, so "going to" a container means
 * bringing it into view where it already is.
 */
export function drawBreadcrumbTrail(
  host: HTMLElement,
  crumbs: readonly FeedBreadcrumb[],
  rc: RowContext,
  mount?: HTMLElement,
): void {
  host.replaceChildren();
  if (crumbs.length === 0) {
    // AN EMPTY TRAIL IS NO TRAIL. R6 draws the header line only when there is
    // something on it, and an empty element left in the DOM is a header the
    // page still reserves room and meaning for.
    host.remove();
    return;
  }
  for (const crumb of crumbs) host.append(drawFeedBreadcrumb(crumb, rc));
  if (mount !== undefined && host.parentElement === null) mount.prepend(host);
}

/** One crumb: the daemon-resolved label, drawn verbatim, as a jump target. */
export function drawFeedBreadcrumb(crumb: FeedBreadcrumb, rc: RowContext): HTMLElement {
  const el = document.createElement("button");
  el.type = "button";
  el.className = "feed-breadcrumb";
  el.textContent = crumb.label;
  const target = crumb.target;
  el.addEventListener("click", () => {
    if (target === undefined) return;
    void rc.revealRow(target);
  });
  return el;
}

/**
 * Lay a feed's rows out in order, nesting each row that names a container.
 *
 * `parent` is PRESENTATION NESTING WITHIN ONE FEED — a merge phase's rows, work
 * grouped under a skill heading — never the sub-feed relationship, which is the
 * connection itself. A row whose container this feed has not drawn is placed at
 * the top level and REPORTED: dropping it would hide real work, and inventing a
 * container would state a grouping the daemon did not.
 */
export function arrangeSubfeedRows(host: HTMLElement, view: SubfeedView): void {
  // Every nesting slot is emptied first, so a row that stopped being in the
  // feed (deletion is ROW OMISSION on the next push) cannot linger inside a
  // container that is still drawn.
  for (const slot of host.querySelectorAll(`[${NEST_ATTRIBUTE}]`)) slot.replaceChildren();
  const placed = new Map<string, HTMLElement>();
  const top: HTMLElement[] = [];
  for (const row of view.rows()) {
    const el = view.drawRow(row);
    if (row.id !== undefined) placed.set(row.id.value, el);
    const parent = row.parent?.row;
    if (parent === undefined) {
      top.push(el);
      continue;
    }
    const container = placed.get(parent.value);
    if (container === undefined) {
      log.warn("a row names a container this feed has not drawn; placing it top-level", {
        operation: "feed.unknown-nesting-container",
        context: { row: row.id?.value ?? "unset", container: parent.value },
      });
      top.push(el);
      continue;
    }
    nestSlot(container).append(el);
  }
  host.replaceChildren(...top);
}

/** A container row's nesting slot, created on the first child that needs it. */
export function nestSlot(container: HTMLElement): HTMLElement {
  const existing = container.querySelector(`:scope > [${NEST_ATTRIBUTE}]`);
  if (existing instanceof HTMLElement) return existing;
  const slot = document.createElement("div");
  slot.className = "feed-nest";
  slot.setAttribute(NEST_ATTRIBUTE, "");
  container.append(slot);
  return slot;
}
