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
import { createControl } from "../control.js";
import type { AppContext } from "../rpc/context.js";
import { drawFeedCommandPanel } from "../panels/panels.js";
import { drawFeedCommandRefused } from "../panels/refused.js";
import { drawFeedArtifact } from "./cards/artifact.js";
import { drawFeedFindings } from "./cards/findings.js";
import { drawFeedHook } from "./cards/hook.js";
import { drawFeedPlan } from "./cards/plan.js";
import { drawFeedResponse } from "./cards/response.js";
import { drawFeedShellBody, drawFeedShellHead } from "./cards/shell.js";
import { drawFeedSkill } from "./cards/skill.js";
import { drawFeedSubagentResult } from "./cards/subagent-result.js";
import { drawFeedSimpleToolCall } from "./cards/tool-call.js";
import { drawFeedColdGate } from "./asks/cold-gate.js";
import { drawFeedPermission } from "./asks/permission.js";
import { drawFeedQuestion } from "./asks/question.js";
import { drawFeedMerge } from "./merge/merge.js";
import { mergeBubbleBody } from "./merge/merge-body.js";
import type { Handle } from "../failure/local.js";
import { log } from "../log.js";
import { placeChildren } from "../dom.js";
import { stopTicking } from "./ticking.js";
import {
  createToolGroupStore,
  groupKindOf,
  groupKey,
  type GroupMember,
  type ToolGroupStore,
} from "./tool-group.js";
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
  FeedSubagentResult,
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
  /**
   * JUMP to another row (owner request, 2026-10-01): open the bubbles along
   * the way, expand the entry, center it (`entryJumped`), and mark it.
   */
  readonly revealRow: (id: FeedId) => Promise<boolean>;
  /**
   * The element this row's PREVIOUS draw produced, when it had one.
   *
   * The ONLY channel for local UI state across a re-push: a renderer reads its
   * own `data-*` off it (a fold the reader toggled, a section they expanded)
   * and re-applies it, so a push can never undo the reader's own act. A
   * renderer may also update it in place and return it (see context.ts).
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
  /** A subagent's returned result, drawn inside its own card (its feed). */
  subagentResult(unit: FeedSubagentResult, rc: RowContext): HTMLElement;
  /** The detached shell bubble's spool BODY (on its sub-feed); the head is `shellHead`. */
  shell(unit: FeedShell, rc: RowContext): HTMLElement;
  /** The detached shell bubble's collapsed HEAD line; its body is `shell`. */
  shellHead(unit: FeedShell, rc: RowContext): HTMLElement;
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
    subagentResult: drawFeedSubagentResult,
    shell: drawFeedShellBody,
    shellHead: drawFeedShellHead,
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
 * The whole body is re-ARRANGED on every change, but never re-attached: the
 * elements are the controller's and are REUSED, and `placeChildren` moves only
 * an element that is out of place. A push therefore inserts what is new and
 * leaves every row already in its place untouched in the document -- which is
 * what keeps a bubble's open folds, expanded sections, live clocks AND the
 * reader's scroll position inside them across a push (owner rule, 2026-09-23:
 * the user owns the scroll).
 */
export const defaultBubbleBody: BubbleBodyRenderer = (mount, view, rc) => {
  const breadcrumbs = document.createElement("div");
  breadcrumbs.className = "feed-breadcrumbs";
  breadcrumbs.setAttribute("data-breadcrumbs", "");

  const rows = document.createElement("div");
  rows.className = "feed-rows";

  // ONE GROUP STORE PER BODY (so per open feed). It outlives every redraw, which
  // is what lets a reader's tab pick and a run's identity survive the re-arrange
  // each live push triggers.
  const groups = createToolGroupStore();

  const draw = (): void => {
    drawBreadcrumbTrail(breadcrumbs, view.breadcrumbs(), rc, mount);
    arrangeSubfeedRows(rows, view, groups);
    const slot = view.composerSlot;
    if (slot !== undefined && mount.lastElementChild !== slot) mount.append(slot);
  };

  mount.append(rows);
  draw();
  const unsubscribe = view.onChange(draw);
  return {
    dispose(): void {
      unsubscribe();
      for (const part of [breadcrumbs, rows]) {
        stopTicking(part);
        part.remove();
      }
    },
  };
};

/**
 * The breadcrumb header line — drawn ONLY when the trail is non-empty (R6).
 *
 * A crumb is a jump target, and the jump is the feed's own (`revealRow`)
 * rather than a navigation: sub-feed expansion is INLINE, so "going to" a
 * container means expanding it and centering it where it already is.
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
  const el = createControl();
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
 * Lay a feed's rows out in order, nesting each row that names a container and
 * batching each maximal run of consecutive same-kind tool cards into ONE tabbed
 * group.
 *
 * `parent` is PRESENTATION NESTING WITHIN ONE FEED — a merge phase's rows, work
 * grouped under a skill heading — never the sub-feed relationship, which is the
 * connection itself. A row whose container this feed has not drawn is placed at
 * the top level and REPORTED: dropping it would hide real work, and inventing a
 * container would state a grouping the daemon did not.
 *
 * GROUPING IS OPTIONAL and lives here because this is the one place the ORDERED
 * sequence — and so adjacency — is known. A run of >=2 consecutive TOP-LEVEL
 * same-specific-kind tool cards (see `groupKindOf`) OF ONE TURN collapses into
 * one tabbed container; a lone card, a non-groupable row, a kind change, a
 * turn change or a parented row breaks the run and renders unchanged. Callers with no `groups` store (a fixture
 * exercising the arranger alone) get the plain per-row layout.
 */
export function arrangeSubfeedRows(
  host: HTMLElement,
  view: SubfeedView,
  groups?: ToolGroupStore,
): void {
  // WHAT WAS DRAWN BEFORE, so a row this arrangement DROPS can be unsubscribed
  // rather than left ticking against an element nobody can see. A row that is
  // re-placed is merely moved and keeps its clocks; only one the arrangement
  // did not put back is stopped, below. Grouped members sit inside their group's
  // panel, still descendants of the host, so this descendant query still finds
  // them and a member merely regrouped is never mistaken for a dropped one.
  const before = [...host.querySelectorAll("[data-feed-row]")];
  // Every nesting slot drawn before this pass is re-placed at the end, empty if
  // nothing lands in it now, so a row that stopped being in the feed (deletion
  // is ROW OMISSION on the next push) cannot linger inside a container that is
  // still drawn.
  const nests = new Map<Element, HTMLElement[]>();
  for (const slot of host.querySelectorAll(`[${NEST_ATTRIBUTE}]`)) nests.set(slot, []);
  const placed = new Map<string, HTMLElement>();
  const top: HTMLElement[] = [];
  const rows = view.rows();
  let i = 0;
  while (i < rows.length) {
    const row = rows[i];
    // A run is only ever of TOP-LEVEL groupable cards: a parented row nests, and
    // its appearance between two same-kind cards breaks the run.
    const kind = groups !== undefined && row.parent?.row === undefined ? groupKindOf(row) : null;
    if (kind !== null) {
      // A RUN NEVER SPANS TWO TURNS (owner-approved, 2026-09-24): its cards
      // share one `turn`, both unset or equal, so a turn boundary with no row
      // between the two turns' cards still breaks it.
      const turn = row.turn?.value;
      let j = i + 1;
      while (
        j < rows.length &&
        rows[j].parent?.row === undefined &&
        groupKindOf(rows[j]) === kind &&
        rows[j].turn?.value === turn
      ) {
        j += 1;
      }
      if (j - i >= 2) {
        const members: GroupMember[] = [];
        for (const member of rows.slice(i, j)) {
          const el = view.drawRow(member);
          const id = member.id?.value ?? "";
          if (member.id !== undefined) placed.set(id, el);
          members.push({ id, element: el });
        }
        // `groups` is defined here — `kind` is non-null only when it is.
        top.push(groups!.arrange(groupKey(kind, members[0].id), kind, members));
        i = j;
        continue;
      }
      // A run of one is a lone card: fall through to the ordinary placement.
    }
    const el = view.drawRow(row);
    if (row.id !== undefined) placed.set(row.id.value, el);
    const parent = row.parent?.row;
    if (parent === undefined) {
      top.push(el);
      i += 1;
      continue;
    }
    const container = placed.get(parent.value);
    if (container === undefined) {
      log.warn("a row names a container this feed has not drawn; placing it top-level", {
        operation: "feed.unknown-nesting-container",
        context: { row: row.id?.value ?? "unset", container: parent.value },
      });
      top.push(el);
      i += 1;
      continue;
    }
    const slot = nestSlot(container);
    const nested = nests.get(slot);
    if (nested === undefined) nests.set(slot, [el]);
    else nested.push(el);
    i += 1;
  }
  // NOTHING ALREADY IN PLACE IS RE-ATTACHED (see `placeChildren`): the top
  // level first, then each nesting slot, whose container may itself have just
  // been placed.
  let moved = placeChildren(host, top);
  for (const [slot, children] of nests) moved += placeChildren(slot, children);
  log.debug(`arranged ${String(rows.length)} rows, moving ${String(moved)}`, {
    operation: "feed.arrange",
    context: { rows: rows.length, moved },
  });
  // A group not placed this pass (its run broke, or its rows left the feed) is
  // disposed — after `replaceChildren`, so a member re-placed elsewhere is safely
  // out of the old group before it is torn down.
  groups?.prune();
  let stopped = 0;
  for (const el of before) {
    if (host.contains(el)) continue;
    stopped += stopTicking(el);
  }
  if (stopped > 0) {
    log.debug("stopped the clocks of rows this arrangement dropped", {
      operation: "feed.arrange-dropped-rows",
      context: { dropped: stopped },
    });
  }
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
