/**
 * feed-view — the FeedController: ONE feed's rows, its DOM, its page walk.
 *
 * There is one of these per OPEN feed — the root feed while the view is up,
 * and one per expanded bubble — and they are the same class, because a bubble
 * IS a feed. The only thing a caller varies is which body renderer lays the
 * rows out and which head a bubble row wears.
 *
 * WHAT IT OWNS
 *  - THE ORDER, keyed by `FeedId.value`. A page arrives oldest → newest; a tail
 *    push with a known id REPLACES that row in place (which is how a response
 *    grows and how a settled card lands), and an unknown id APPENDS. Nothing is
 *    accumulated across pushes — a row is replaced whole, never merged.
 *  - THE ELEMENTS. Each row gets one `<article>` of chrome that outlives its
 *    body: the body is redrawn on each push while the chrome, the nesting slot
 *    and (for a bubble) the open sub-feed inside it survive.
 *  - THE WALK. `has_more` shows the load-more control, `at_start` hides it, and
 *    a prepend is anchored so the reader is not moved by content arriving above
 *    them.
 *
 * WHAT IT DOES NOT OWN. It draws no card itself beyond the four row kinds the
 * feed core is responsible for; every other arm goes to an injected renderer,
 * and a bubble's expansion is bubble.ts's.
 *
 * A MALFORMED ROW COSTS THAT ROW. The renderer's refusal is caught per row: the
 * row draws as a placeholder naming the path, the failure is reported once, and
 * the rest of the feed goes on working — a feed that blanks itself because one
 * card was unreadable would lose the reader everything, including the evidence.
 */
import { log } from "../log.js";
import { MalformedView, isMalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { frameUndecodable } from "../failure/sink.js";
import { TOPBAR_TONES, toneClass, type Color } from "../vocab.js";
import {
  captureFeedAnchor,
  restoreFeedAnchor,
  type AnchorBox,
  type FeedAnchor,
  type TailFollow,
} from "../scroll.js";
import type { AppContext } from "../rpc/context.js";
import type {
  FeedBreadcrumb,
  FeedId,
  FeedPage,
  FeedPageError,
  FeedTurnActivity,
  FeedRow,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  GetFeedPageResponseSchema,
  type GetFeedPageResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import { buildGetFeedPageRequest } from "./requests.js";
import { armName } from "./renderers.js";
import type {
  BubbleBodyRenderer,
  Handle,
  RowContext,
  RowRenderers,
  SubfeedView,
} from "./renderers.js";
import { replaceTicking, stopTicking } from "./ticking.js";
import { drawFeedUserPrompt } from "./rows/user-prompt.js";
import { drawFeedAgentPrompt } from "./rows/agent-prompt.js";
import { FINAL_ANSWER_ATTRIBUTE, drawFeedTurnEnded } from "./rows/turn-ended.js";
import {
  drawFeedSessionSeparation,
  separationBoundsFeed,
} from "./rows/separation.js";
import { drawFeedMergeTabRow } from "./merge/tab-row.js";
import { isOwnTurn } from "../composer/own-turns.js";
import { PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING } from "../breathing.js";

/** A bubble, as the controller holds it: an element plus its own lifecycle. */
export interface BubbleLike extends Handle {
  /** The element that goes in the row's body slot. */
  readonly element: HTMLElement;
  /** Redraw the head from a re-pushed row, leaving the fold alone. */
  update(row: FeedRow): void;
  /** Open the sub-feed if it is not open. Answers whether it is open now. */
  expand(): Promise<boolean>;
  /** Whether the sub-feed is open right now. */
  isExpanded(): boolean;
  /** The sub-feed's controller, once it has been opened at least once. */
  child(): FeedController | null;
}

/** Builds the bubble for a bubble-shaped row. Injected, to avoid a cycle. */
export type BubbleFactory = (row: FeedRow, rc: RowContext) => BubbleLike;

export interface FeedControllerOptions {
  ctx: AppContext;
  /** The element this feed draws into. */
  host: HTMLElement;
  /** This feed's address: the root feed, or the bubble row's own id. */
  feed: FeedId | "root";
  renderers: RowRenderers;
  /** How the rows are laid out — the default body, or the merge tab strip. */
  body: BubbleBodyRenderer;
  readonly revealRow: (id: FeedId) => Promise<boolean>;
  bubble: BubbleFactory;
  /**
   * The row context the BODY renderer is handed. A bubble passes its own
   * bubble row's context; the root feed passes a context standing for the feed
   * itself, which is not a row.
   */
  bodyContext: RowContext;
  /** The bubble's own composer slot, when this build mounts one (R7). */
  composerSlot?: HTMLElement;
  /** The page's scroll box and tail owner. Root feed only. */
  scroll?: { box: AnchorBox; tail: TailFollow };
}

/** One row, as the controller holds it. */
interface RowState {
  row: FeedRow;
  element: HTMLElement;
  body: HTMLElement | null;
  bubble: BubbleLike | null;
  /** Whether the message changed since the body was last drawn. */
  dirty: boolean;
}

export interface FeedController extends Handle {
  readonly element: HTMLElement;
  /** Paint a page: the newest page from OpenFeed, or an older one prepended. */
  applyPage(page: FeedPage, placement: "replace" | "prepend"): void;
  /** One live upsert. */
  upsert(row: FeedRow): void;
  rows(): readonly FeedRow[];
  breadcrumbs(): readonly FeedBreadcrumb[];
  onChange(fn: () => void): () => void;
  drawRow(row: FeedRow): HTMLElement;
  findRowElement(id: FeedId): HTMLElement | null;
  /** The bubbles on this feed, for the reveal walk. */
  bubbles(): readonly BubbleLike[];
  /** The view a body renderer draws from. */
  view(): SubfeedView;
}

/** Build a controller for ONE feed and draw its shell into the host. */
export function createFeedController(opts: FeedControllerOptions): FeedController {
  const order: string[] = [];
  const states = new Map<string, RowState>();
  const listeners = new Set<() => void>();
  let crumbs: readonly FeedBreadcrumb[] = [];
  let disposed = false;
  // THE SETTLED-TURN SET, MEMOIZED. Every drawn row is held against it, so
  // recomputing it per row would make one page's paint quadratic in its rows.
  // `null` = stale; only the two things that can change it drop it (see
  // `settledTurns`).
  let settled: ReadonlySet<string> | null = null;

  opts.host.setAttribute("data-feed", opts.feed === "root" ? "root" : opts.feed.value);

  const loadMore = document.createElement("button");
  loadMore.type = "button";
  loadMore.className = "feed-load-more";
  loadMore.setAttribute("data-load-more", "");
  loadMore.textContent = "older";

  const errorSlot = document.createElement("div");
  errorSlot.className = "feed-page-error-slot";

  const bodyMount = document.createElement("div");
  bodyMount.className = "feed-body";

  // THE WALK CONTROL IS PRESENT ONLY WHEN THERE IS A WALK. A feed that has
  // reached its start offers no way back further, and an inert control the
  // reader can see is a promise the feed cannot keep — so it is ATTACHED on
  // `has_more` and detached otherwise, never merely hidden.
  opts.host.append(errorSlot, bodyMount);

  const controller: FeedController = {
    element: opts.host,
    applyPage,
    upsert,
    rows,
    breadcrumbs: () => crumbs,
    onChange,
    drawRow,
    findRowElement,
    bubbles,
    view: () => subfeed,
    dispose,
  };

  const rowContextFor = (row: FeedRow, previous?: HTMLElement): RowContext => ({
    ctx: opts.ctx,
    feed: opts.feed,
    row,
    revealRow: opts.revealRow,
    previous,
    findRowElement,
  });

  const subfeed: SubfeedView = {
    rows,
    onChange,
    drawRow,
    breadcrumbs: () => crumbs,
    composerSlot: opts.composerSlot,
  };

  loadMore.addEventListener("click", () => {
    void loadOlder();
  });

  const bodyHandle = opts.body(bodyMount, subfeed, opts.bodyContext);

  return controller;

  // ---- the row store ----------------------------------------------------

  function rows(): readonly FeedRow[] {
    return order.map((id) => {
      const state = states.get(id);
      if (state === undefined) throw new Error(`feed: row ${id} is ordered but not held`);
      return state.row;
    });
  }

  function onChange(fn: () => void): () => void {
    listeners.add(fn);
    return () => {
      listeners.delete(fn);
    };
  }

  function announce(): void {
    for (const fn of [...listeners]) fn();
    markLatestPrompt();
    markWorkingPrompts();
    stopEndedTurns();
    followTail();
  }

  /**
   * THE BACKSTOP: every timer in a turn stops when the turn ends.
   *
   * A card is meant to stop its own clock the moment its unit settles, and a
   * replace site is meant to stop the element it discards. This runs AFTER the
   * listeners have redrawn, and sweeps whatever those two missed: once a turn
   * has a `turn_ended` row on the page, nothing inside that turn's rows is
   * still counting, because nothing in a finished turn can still be running.
   *
   * THE TURN'S OWN END ROW IS EXEMPT. Its retry countdown is a clock about
   * what happens NEXT, not about the work that just stopped, and it stops
   * itself when it expires.
   *
   * A stop here is a DEFECT SIGNAL, not routine housekeeping — the card that
   * needed it failed the first rule — so it is recorded with the rows it
   * stopped, which is what makes that card findable afterwards.
   */
  function stopEndedTurns(): void {
    const ended = new Set<string>();
    for (const state of states.values()) {
      if (state.row.row.case !== "turnEnded") continue;
      const turn = state.row.turn;
      if (turn !== undefined) ended.add(turn.value);
    }
    if (ended.size === 0) return;
    const stopped: string[] = [];
    for (const state of states.values()) {
      if (state.row.row.case === "turnEnded") continue;
      const turn = state.row.turn;
      if (turn === undefined || !ended.has(turn.value)) continue;
      if (stopTicking(state.element) === 0) continue;
      stopped.push(requireMessage(state.row.id, "FeedRow.id").value);
    }
    if (stopped.length === 0) return;
    log.debug("a turn ended with clocks still running; the backstop stopped them", {
      operation: "feed.turn-end-stopped-clocks",
      context: { feed: feedName(), turns: [...ended].join(","), rows: stopped.join(",") },
    });
  }

  /**
   * Paint a page.
   *
   * REPLACE is the newest page (an open, or a re-open on re-expand): the feed's
   * rows become this page's rows. PREPEND is the walk into the past: older rows
   * land above what is already there, anchored so the reader stays on what they
   * were reading.
   */
  function applyPage(page: FeedPage, placement: "replace" | "prepend"): void {
    const result = requireCase(page.result, "FeedPage.result");
    log.debug(`applying a ${placement} page as ${result.case}`, {
      operation: "feed.apply-page",
      context: { feed: feedName(), placement, arm: result.case },
    });
    switch (result.case) {
      case "success": {
        replaceTicking(errorSlot);
        const edge = requireCase(result.value.edge, "FeedPageSuccess.edge");
        if (edge.case === "hasMore") opts.host.prepend(loadMore);
        else loadMore.remove();
        crumbs = requireMessage(result.value.breadcrumbs, "FeedPageSuccess.breadcrumbs").crumbs;
        const anchor = capture();
        if (placement === "replace") clearRows();
        const incoming = result.value.rows;
        for (let i = 0; i < incoming.length; i += 1) {
          adopt(incoming[i], placement === "prepend" ? i : order.length);
        }
        announce();
        restore(anchor);
        return;
      }
      case "error":
        drawPageError(result.value);
        announce();
        return;
      default:
        unreachableArm("FeedPage.result", armName(result));
    }
  }

  /**
   * The page could not be completed: drawn WHERE THE ROWS WOULD BE, with the
   * daemon's own sentence and the typed evidence, so a reader is never shown a
   * feed with a silent hole in it.
   */
  function drawPageError(error: FeedPageError): void {
    const headline = requireMessage(error.headline, "FeedPageError.headline");
    const kind = requireCase(error.kind, "FeedPageError.kind");
    const el = document.createElement("div");
    el.className = `feed-page-error ${headlineToneClass(headline.tone)}`;
    el.setAttribute("data-page-error", kind.case);

    const text = document.createElement("div");
    text.className = "feed-page-error-headline";
    text.textContent = headline.text;
    el.append(text);

    switch (kind.case) {
      case "historyReplayTruncated": {
        const evidence = document.createElement("div");
        evidence.className = "feed-page-error-evidence";
        evidence.textContent = kind.value.reason;
        el.append(evidence);
        break;
      }
      default:
        unreachableArm("FeedPageError.kind", armName(kind));
    }
    log.error(`a feed page could not be served: ${headline.text}`, {
      operation: "feed.page-error",
      context: { feed: feedName(), arm: kind.case },
    });
    replaceTicking(errorSlot, [el]);
  }

  /** One live upsert: replace in place if seen, append if new. */
  function upsert(row: FeedRow): void {
    const id = requireMessage(row.id, "FeedRow.id").value;
    // A REMOVAL is the DUAL of an upsert, delivered on the same tail: the
    // daemon retired the row it keys, so drop it live rather than replace it.
    if (row.row.case === "removed") {
      remove(id);
      announce();
      return;
    }
    const known = states.has(id);
    log.debug(`${known ? "replacing" : "appending"} feed row ${id}`, {
      operation: known ? "feed.row-replaced" : "feed.row-appended",
      context: { feed: feedName(), row: id, kind: row.row.case ?? "unset" },
    });
    adopt(row, known ? -1 : order.length);
    truncateAtSeparation(row, id);
    announce();
  }

  /**
   * Drop one row on a live removal: dispose its bubble, stop its clocks,
   * detach its element, and forget it — the same teardown a row dropped above
   * a separation gets, so nothing keeps ticking or leaks after the daemon
   * retired the row. A removal for a row this feed never held is a no-op.
   */
  function remove(id: string): void {
    const at = order.indexOf(id);
    if (at < 0) {
      log.debug(`a removal named a row this feed does not hold: ${id}`, {
        operation: "feed.row-removed-absent",
        context: { feed: feedName(), row: id },
      });
      return;
    }
    const state = states.get(id);
    if (state !== undefined) {
      if (state.row.row.case === "turnEnded") forgetSettled();
      state.bubble?.dispose();
      stopTicking(state.element);
      state.element.remove();
      states.delete(id);
    }
    order.splice(at, 1);
    log.info(`removed feed row ${id}`, {
      operation: "feed.row-removed",
      context: { feed: feedName(), row: id },
    });
  }

  /**
   * THE FEED BEGINS AT THE NEWEST SEPARATION, on this side too.
   *
   * A compaction or a clear arriving on the tail says the rows above it are no
   * longer the conversation — the daemon stops serving them from this moment,
   * so a client that kept them on screen would be the only place they still
   * existed, and after a second compaction the reader would be looking at two
   * dividers and a superseded summary.
   *
   * The divider itself STAYS, and for a compaction so does the summary it
   * carries: that summary is the whole of what survived, and it lives on this
   * row rather than above it.
   */
  function truncateAtSeparation(row: FeedRow, id: string): void {
    if (!separationBoundsFeed(row)) return;
    const at = order.indexOf(id);
    if (at <= 0) return;
    const dropped = order.splice(0, at);
    for (const gone of dropped) {
      const state = states.get(gone);
      if (state === undefined) continue;
      state.bubble?.dispose();
      stopTicking(state.element);
      state.element.remove();
      states.delete(gone);
    }
    log.info(`dropped ${dropped.length.toString()} rows above the newest separation`, {
      operation: "feed.truncated-at-separation",
      context: { feed: feedName(), row: id, dropped: dropped.length },
    });
  }

  /**
   * Put ROW in the store at INDEX (-1 = keep its place).
   *
   * A row already held keeps its element and its place and is only marked
   * dirty — that is what makes an upsert a REPLACEMENT of the drawing rather
   * than a rebuild of the row, and what lets an open bubble survive a re-push
   * of its own head.
   */
  function adopt(row: FeedRow, index: number): void {
    const id = requireMessage(row.id, "FeedRow.id").value;
    // A terminal row landing (or a row that WAS one being replaced) is one of
    // the two things that move the settled-turn set.
    if (row.row.case === "turnEnded" || states.get(id)?.row.row.case === "turnEnded") {
      forgetSettled();
    }
    const held = states.get(id);
    if (held !== undefined) {
      held.row = row;
      held.dirty = true;
      applyRowAttributes(held.element, row);
      return;
    }
    const element = document.createElement("article");
    element.className = "feed-item";
    applyRowAttributes(element, row);
    const state: RowState = { row, element, body: null, bubble: null, dirty: true };
    states.set(id, state);
    order.splice(index < 0 ? order.length : index, 0, id);
    if (!isBubbleRow(row)) return;
    // A bubble owns its own element for its whole life: the head redraws inside
    // it and the sub-feed hangs beneath it, so the row's body is never replaced.
    const bubble = opts.bubble(row, rowContextFor(row));
    state.bubble = bubble;
    state.body = bubble.element;
    state.dirty = false;
    element.append(bubble.element);
    mirrorState(state);
  }

  /** Drop every row: the feed is being repainted from a fresh newest page. */
  function clearRows(): void {
    forgetSettled();
    for (const state of states.values()) {
      state.bubble?.dispose();
      stopTicking(state.element);
      state.element.remove();
    }
    states.clear();
    order.length = 0;
  }

  // ---- drawing ----------------------------------------------------------

  /**
   * The element for ROW, built once and updated only when the row changed.
   *
   * Reuse is what preserves everything the reader did inside a row — an open
   * fold, an expanded output, a bubble's own sub-feed — across pushes that
   * touched some OTHER row.
   */
  function drawRow(row: FeedRow): HTMLElement {
    const id = requireMessage(row.id, "FeedRow.id").value;
    const state = states.get(id);
    if (state === undefined) throw new Error(`feed: asked to draw unheld row ${id}`);
    if (!state.dirty) return state.element;
    state.dirty = false;
    drawBody(state);
    // A FRESH PROMPT BUBBLE ARRIVES WAVING (`startPromptWave`), so the row it
    // was drawn for is reconciled HERE, on the same synchronous pass, rather
    // than only in `announce`. Every draw path reaches this function — the row
    // list, the merge strip's own redraw on a tab pick — and a settled prompt
    // redrawn outside a push would otherwise start waving again and stay that
    // way until the next row arrived.
    markPromptWave(state, settledTurns());
    return state.element;
  }

  /** Draw (or redraw) one row's body inside its chrome. */
  function drawBody(state: RowState): void {
    const previous = state.body ?? undefined;
    if (state.bubble !== null) {
      // A bubble redraws its own head and keeps its sub-feed; replacing the
      // element here would tear down an open bubble on every push.
      state.bubble.update(state.row);
      mirrorState(state);
      return;
    }
    let body: HTMLElement;
    try {
      body = drawRowBody(state.row, rowContextFor(state.row, previous));
    } catch (err) {
      if (!isMalformedView(err)) throw err;
      body = malformedPlaceholder(err, state.row);
    }
    if (previous !== undefined) {
      stopTicking(previous);
      previous.remove();
    }
    state.body = body;
    state.element.prepend(body);
    mirrorState(state);
    // The concluded arm puts the final-answer mark on the row it NAMES, which
    // is the other thing that can settle a turn.
    if (state.row.row.case === "turnEnded") forgetSettled();
  }

  /**
   * THE CARD'S STATE, REPEATED ON THE ROW CHROME.
   *
   * A card carries `data-state` with its own state/outcome arm (preamble §5).
   * The chrome is what a reader — and every query that starts from a row —
   * holds, so the arm is mirrored up onto the `<article>`: one row, one place
   * to ask what state it is in. It is COPIED, never derived: the row chrome
   * knows nothing about which arms exist and states only what the card stated.
   */
  function mirrorState(state: RowState): void {
    mirror(state, "data-state");
    // A bubble says whether it is open on itself; the row is what a reader —
    // and the reveal walk — holds, so it says the same thing.
    mirror(state, "data-expanded");
  }

  /** Copy one attribute from the row's body up onto its chrome. */
  function mirror(state: RowState, attribute: string): void {
    const drawn = state.body?.getAttribute(attribute) ?? null;
    if (drawn === null) {
      state.element.removeAttribute(attribute);
      return;
    }
    state.element.setAttribute(attribute, drawn);
  }

  /**
   * The body of one row, by arm.
   *
   * The four kinds the feed core draws itself are here; every other arm is an
   * injected renderer's, and the bubble arms never reach this function at all
   * (they are recognized when the chrome is built).
   */
  function drawRowBody(row: FeedRow, rc: RowContext): HTMLElement {
    const arm = requireCase(row.row, "FeedRow.row");
    switch (arm.case) {
      case "userPrompt":
        return drawFeedUserPrompt(arm.value);
      case "agentPrompt":
        return drawFeedAgentPrompt(arm.value);
      case "turnEnded":
        return drawFeedTurnEnded(arm.value, rc);
      case "separation":
        return drawFeedSessionSeparation(arm.value, rc);
      case "activity":
        return drawActivity(arm.value, rc);
      case "detachedShell":
        return opts.renderers.shell(
          requireMessage(arm.value.shell, "FeedDetachedShell.shell"),
          rc,
        );
      case "permission":
        return opts.renderers.permission(arm.value, rc);
      case "question":
        return opts.renderers.question(arm.value, rc);
      case "coldGate":
        return opts.renderers.coldGate(arm.value, rc);
      case "commandPanel":
        return opts.renderers.commandPanel(arm.value, rc);
      case "commandRefused":
        return opts.renderers.commandRefused(arm.value, rc);
      case "mergeTab":
        // INSIDE a merge bubble the strip consumes these and this path is never
        // reached. On any other feed the tab is still a row the daemon served,
        // and dropping it would hide a phase of a real run — so it draws as its
        // own row through the same badge and body the strip uses.
        return drawFeedMergeTabRow(row, arm.value, subfeed, rc);
      case "detachedSubagent":
        throw new MalformedView(
          "FeedRow.row.detached_subagent",
          "a bubble row reached the ordinary row path",
        );
      case "shellHead":
        throw new MalformedView(
          "FeedRow.row.shell_head",
          "a bubble row reached the ordinary row path",
        );
      default:
        return unreachableArm("FeedRow.row", armName(arm));
    }
  }

  /** One unit of synchronous turn progress, by arm. */
  function drawActivity(activity: FeedTurnActivity, rc: RowContext): HTMLElement {
    const unit = requireCase(activity.unit, "FeedTurnActivity.unit");
    switch (unit.case) {
      case "response":
        return opts.renderers.response(unit.value, rc);
      case "simpleToolCall":
        return opts.renderers.simpleToolCall(unit.value, rc);
      case "skill":
        return opts.renderers.skill(unit.value, rc);
      case "hook":
        return opts.renderers.hook(unit.value, rc);
      case "artifact":
        return opts.renderers.artifact(unit.value, rc);
      case "plan":
        return opts.renderers.plan(unit.value, rc);
      case "findings":
        return opts.renderers.findings(unit.value, rc);
      case "merge":
      case "subagent":
        throw new MalformedView(
          `FeedTurnActivity.unit.${unit.case}`,
          "a bubble unit reached the ordinary row path",
        );
      default:
        return unreachableArm("FeedTurnActivity.unit", armName(unit));
    }
  }

  /**
   * The identity attributes every row element carries.
   *
   * TOLERANT OF AN UNREADABLE ARM ON PURPOSE: the chrome is what carries the
   * row's identity to the reader and to the integration suite, and a row whose
   * BODY cannot be drawn still has an id and still occupies its place. The
   * refusal happens where the body is drawn, and produces the placeholder.
   *
   * The ID is the exception — it is the upsert key, so a row without one cannot
   * be held at all and the refusal is the whole row's.
   */
  function applyRowAttributes(el: HTMLElement, row: FeedRow): void {
    el.setAttribute("data-feed-row", requireMessage(row.id, "FeedRow.id").value);
    el.setAttribute("data-row-kind", row.row.case ?? "malformed");
    if (row.row.case === "activity" && row.row.value.unit.case !== undefined) {
      el.setAttribute("data-unit", row.row.value.unit.case);
    }
    if (row.turn !== undefined) {
      el.setAttribute("data-turn", row.turn.value);
      // THIS PAGE'S OWN SUBMISSION, claimed from the TurnId `SubmitPrompt`
      // minted here and echoed back on the row — never guessed from the text.
      if (isOwnTurn(row.turn)) el.setAttribute("data-mine", "true");
      else el.removeAttribute("data-mine");
    }
  }

  /** The compact stand-in for a row this build could not draw. */
  function malformedPlaceholder(err: MalformedView, row: FeedRow): HTMLElement {
    log.error(`a feed row could not be drawn: ${err.message}`, {
      operation: "feed.row-malformed",
      context: {
        feed: feedName(),
        row: row.id?.value ?? "unset",
        path: err.path,
        detail: err.detail,
      },
    });
    opts.ctx.failures.report(frameUndecodable(err.detail, err.path));
    const el = document.createElement("div");
    el.className = "row-malformed";
    el.setAttribute("data-arm", "malformed");
    el.textContent = `unreadable row at ${err.path}`;
    return el;
  }

  // ---- the walk ---------------------------------------------------------

  /**
   * Ask for the next-older page.
   *
   * `next` continues the walk the DAEMON holds for this feed, so there is
   * nothing to send but which feed and which direction — and a `next` with no
   * walk standing is the daemon's refusal, drawn at this control.
   */
  async function loadOlder(): Promise<void> {
    log.info("walking one page older", {
      operation: "feed.load-older",
      context: { feed: feedName() },
    });
    clearRefusals();
    loadMore.disabled = true;
    let response: GetFeedPageResponse;
    try {
      response = await callUnary(
        opts.ctx,
        "GetFeedPage",
        (client) =>
          client.getFeedPage(
            buildGetFeedPageRequest(opts.ctx.workspace, feedId(), "next"),
          ),
        GetFeedPageResponseSchema,
      );
    } catch {
      loadMore.after(refusalAt("transport", "the daemon could not be reached"));
      loadMore.disabled = false;
      return;
    }
    loadMore.disabled = false;
    const result = requireCase(response.result, "GetFeedPageResponse.result");
    switch (result.case) {
      case "success":
        applyPage(result.value, "prepend");
        return;
      case "error":
        loadMore.after(refusalAt("error", "older pages are not available"));
        return;
      default:
        unreachableArm("GetFeedPageResponse.result", armName(result));
    }
  }

  /** Drop whatever a previous walk's refusal left beside the control. */
  function clearRefusals(): void {
    for (const stale of opts.host.querySelectorAll(":scope > .refusal")) stale.remove();
  }

  /** Sample the reader's place before rows land above them. */
  function capture(): FeedAnchor | null {
    if (opts.scroll === undefined) return null;
    const items = [...opts.host.querySelectorAll<HTMLElement>("[data-feed-row]")].map((el) => ({
      key: el.getAttribute("data-feed-row") ?? "",
      offsetTop: el.offsetTop,
    }));
    return captureFeedAnchor(opts.scroll.box, items, opts.scroll.tail.isFollowing());
  }

  /** Put the reader back where the capture found them. */
  function restore(anchor: FeedAnchor | null): void {
    if (opts.scroll === undefined || anchor === null) return;
    restoreFeedAnchor(opts.scroll.box, anchor, opts.scroll.tail);
  }

  /** Follow the tail while output streams, if the reader is following it. */
  function followTail(): void {
    if (opts.scroll === undefined) return;
    if (opts.scroll.tail.isFollowing()) opts.scroll.tail.park();
  }

  // ---- presentation the client owns -------------------------------------

  /**
   * The rolling highlight: the NEWEST user prompt wears the marker and every
   * older one loses it. Client presentation, unchanged by the port — nothing
   * about the message that now carries prompts suggests it should move.
   */
  function markLatestPrompt(): void {
    let latest: HTMLElement | null = null;
    for (const id of order) {
      const state = states.get(id);
      if (state?.row.row.case === "userPrompt") latest = state.element;
    }
    for (const el of opts.host.querySelectorAll<HTMLElement>("[data-latest-prompt]")) {
      if (el !== latest) el.removeAttribute("data-latest-prompt");
    }
    latest?.setAttribute("data-latest-prompt", "true");
  }

  /**
   * THE THINKING WAVE: ON EVERY PROMPT UNTIL ITS TURN IS SETTLED.
   *
   * THE INVARIANT (owner ruling, 2026-09-14): a prompt bubble waves from the
   * moment it is drawn until its turn's final answer lands. The bubble arrives
   * waving — `startPromptWave` stamps it at construction — and this pass is
   * the RECONCILIATION over it: a reload that paints thirty settled prompts
   * ends with thirty still bubbles, because this runs on the same pass that
   * drew them.
   *
   * SO IT ONLY EVER TAKES THE WAVE AWAY FROM A SETTLED TURN. A prompt whose
   * turn has not ended keeps it, and a prompt that names NO TURN keeps it too:
   * a locally minted prompt the daemon has not yet stamped is one the reader
   * can see and cannot possibly have an answer to, and leaving it still would
   * say the page is idle at the one moment it certainly is not. The turn's
   * settlement is the only thing that stops the band.
   *
   * WHAT COUNTS AS SETTLED is `settledTurns` below — the final-answer mark or
   * the `turn_ended` row, whichever this feed saw first.
   *
   * IT TOUCHES ONLY THE ATTRIBUTE. The bubble element is the one the row's last
   * draw produced and it is left standing: nothing here marks a row dirty or
   * redraws a body, so a turn settling stops the band with the prompt's own
   * text untouched beneath it.
   */
  function markWorkingPrompts(): void {
    const turns = settledTurns();
    for (const id of order) {
      const state = states.get(id);
      if (state === undefined) continue;
      markPromptWave(state, turns);
    }
  }

  /**
   * The turns this feed has seen the end of, however they ended.
   *
   * TWO FACTS, EITHER OF WHICH IS THE END, because the ruling names the final
   * answer and the schema names the terminal row and they are not guaranteed
   * to be the same moment:
   *  - the FINAL-ANSWER mark on a row of this turn (`markFinalAnswer` in
   *    turn-ended.ts). This is the one the reader actually sees arrive — the
   *    green-bordered answer — and the ruling ends the wave on it.
   *  - the turn's own `turn_ended` row, which is the terminal fact whose
   *    ABSENCE is what "live" means (feed.proto, FeedRow.turn_ended). It is
   *    what settles a turn that ends with NO answer — errored, interrupted —
   *    so a dead turn never keeps waving.
   */
  function settledTurns(): ReadonlySet<string> {
    if (settled !== null) return settled;
    const found = new Set<string>();
    for (const id of order) {
      const state = states.get(id);
      if (state === undefined) continue;
      const turn = state.row.turn;
      if (turn === undefined) continue;
      if (state.row.row.case === "turnEnded") {
        found.add(turn.value);
        continue;
      }
      if (state.element.getAttribute(FINAL_ANSWER_ATTRIBUTE) === "true") found.add(turn.value);
    }
    settled = found;
    return found;
  }

  /**
   * Drop the memo, because something that decides settlement moved.
   *
   * ONLY TWO THINGS CAN: a `turn_ended` row arriving or being cleared, and a
   * `turn_ended` row being DRAWN (its concluded arm is what puts the
   * final-answer mark on the answering row). Everything else — a response
   * growing, a card settling — leaves the set exactly as it was, which is what
   * keeps a thousand-row paint linear.
   */
  function forgetSettled(): void {
    settled = null;
  }

  /** One row's prompt bubble, held against SETTLED. A non-prompt row is a no-op. */
  function markPromptWave(state: RowState, settled: ReadonlySet<string>): void {
    if (state.row.row.case !== "userPrompt" && state.row.row.case !== "agentPrompt") return;
    const bubble = state.element.querySelector<HTMLElement>(".bubble.user");
    if (bubble === null) return;
    const turn = state.row.turn;
    // REGRESSION WATCH (prompt glimmer, 2026-09-14): the wave was reported as
    // having "stopped", the suspected cause a premature settle clearing the
    // mark before the answer landed. THIS is the only line that clears it, and
    // it fires only for a turn already in `settled` (a turn_ended or a
    // final-answer row) — so an early stop would surface here as this branch
    // taken while the turn is still open. The investigation found the logic and
    // CSS intact and unchanged since 668aad539 (the "stop" reproduced only under
    // an environment-level render suspension / prefers-reduced-motion, not here),
    // and locked the in-flight boundary in prompt-wave.integration.test.ts. If
    // the glimmer stops early again, watch this clear and what populates
    // `settledTurns`. Not a lock — the settlement rule may still change.
    if (turn !== undefined && settled.has(turn.value)) {
      bubble.removeAttribute(PROMPT_WAVE_ATTRIBUTE);
      return;
    }
    // NEVER A REMOVAL HERE. The bubble is already waving from its draw; this
    // re-asserts it for the one case the draw could not cover — a bubble built
    // before this build's `startPromptWave` existed on some other path — and
    // is otherwise a no-op.
    bubble.setAttribute(PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING);
  }

  // ---- lookups ----------------------------------------------------------

  function findRowElement(id: FeedId): HTMLElement | null {
    return states.get(id.value)?.element ?? null;
  }

  function bubbles(): readonly BubbleLike[] {
    const found: BubbleLike[] = [];
    for (const state of states.values()) {
      if (state.bubble !== null) found.push(state.bubble);
    }
    return found;
  }

  function feedId(): FeedId | undefined {
    return opts.feed === "root" ? undefined : opts.feed;
  }

  function feedName(): string {
    return opts.feed === "root" ? "root" : opts.feed.value;
  }

  function dispose(): void {
    if (disposed) return;
    disposed = true;
    bodyHandle.dispose();
    clearRows();
    listeners.clear();
    opts.host.replaceChildren();
  }
}

/** Whether this row's arm is a bubble — a sub-feed with a collapsed head. */
export function isBubbleRow(row: FeedRow): boolean {
  if (row.row.case === "detachedSubagent") return true;
  // A detached shell HEAD is a canonical bubble: its spool streams on the
  // sub-feed its FeedId addresses (the detached_shell BODY row rides there).
  if (row.row.case === "shellHead") return true;
  if (row.row.case !== "activity") return false;
  return row.row.value.unit.case === "subagent" || row.row.value.unit.case === "merge";
}

/** The refusal drawn beside the control that made the call. */
function refusalAt(arm: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "refusal";
  el.setAttribute("data-arm", arm);
  el.textContent = text;
  return el;
}

/**
 * A tone from the wire, validated against the shared vocabulary.
 *
 * The page error's tone is a STRING rather than an arm, so this is one of the
 * few places a colour is not already typed by the schema; the file is
 * authoritative, and a tone outside it is refused rather than dropped into a
 * `.tone-` rule that does not exist.
 */
function headlineToneClass(tone: string): string {
  if (!TOPBAR_TONES.includes(tone)) {
    throw new MalformedView(
      "FeedPageErrorHeadline.tone",
      `tone '${tone}' is not one of render-colors.json#topbar_tones`,
    );
  }
  return toneClass(tone as Color);
}
