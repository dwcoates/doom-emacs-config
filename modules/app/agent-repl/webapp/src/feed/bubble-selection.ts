/**
 * bubble-selection — A CLICK SELECTS A ROOT-FEED PROMPT OR RESPONSE BUBBLE, AND
 * THE SELECTION IS ITS EXPANSION (owner ruling, 2026-10-01).
 *
 * The DAEMON owns the selection (`SelectResponse`, endpoint_select_response.
 * proto) and decides which rows can be selected: it stamps
 * `FeedRow.selectable` on landed root-feed prompts and response bubbles. This
 * module is the webapp's whole share of it:
 *
 *   - which root-feed rows the selection GOVERNS: their bubble never expands
 *     or collapses on a click of its own (`SELECTION_GOVERNED_ATTRIBUTE`), and
 *     which of those the daemon says can be selected right now
 *     (`SELECTABLE_ATTRIBUTE`);
 *   - the click: a selectable bubble asks the daemon to select it, the
 *     selected one asks it to clear, and a governed bubble that has not landed
 *     does nothing;
 *   - the click's requests, sent through the one `SelectFeedRow` call the
 *     webapp makes (`selectFeedRow`, select-feed-row.ts).
 *
 * Nothing here expands or marks a bubble. The daemon's FeedSelection push does,
 * through `applySelection` (feed-view.ts): selected and expanded are one state,
 * so a bubble is expanded exactly while it is the selection. Which arm a click
 * lands as (a final response, a rollback prompt, any other bubble) is the
 * daemon's to decide.
 */
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { CLICK_THROUGH_SELECTOR } from "../expand.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { selectedText } from "../selection.js";
import { unreachableArm } from "../rpc/strict.js";
import { selectFeedRow } from "./select-feed-row.js";

/** The attribute a root-feed row wears while the selection owns its bubble's expansion. */
export const SELECTION_GOVERNED_ATTRIBUTE = "data-selection-governed";

/** The attribute a governed row wears while the daemon says it can be selected. */
export const SELECTABLE_ATTRIBUTE = "data-selectable";

/**
 * Whether ROW's bubble is one the selection governs on the root feed: a user
 * prompt, an agent prompt, or a response bubble, whatever its state. The
 * daemon's `selectable` stamp says which of them can be selected NOW; this says
 * which never expand on a click of their own, so a response still arriving is
 * not expandable either.
 */
export function isSelectionGoverned(row: FeedRow): boolean {
  switch (row.row.case) {
    case "userPrompt":
    case "agentPrompt":
      return true;
    case "activity":
      return row.row.value.unit.case === "response";
    default:
      return false;
  }
}

/**
 * State the selection attributes on a ROOT-feed row's element: governed by
 * kind, selectable by the daemon's stamp. A row of any other feed carries
 * neither, because only the root feed's bubbles can be selected.
 */
export function stampSelection(el: HTMLElement, row: FeedRow, root: boolean): void {
  const governed = root && isSelectionGoverned(row);
  el.toggleAttribute(SELECTION_GOVERNED_ATTRIBUTE, governed);
  el.toggleAttribute(SELECTABLE_ATTRIBUTE, governed && row.selectable !== undefined);
}

/**
 * The governed row a click at TARGET lands in, or null. The innermost row wins,
 * so a click inside a bubble's sub-feed belongs to that sub-feed's row, which
 * is never governed.
 */
export function governedRowAt(target: EventTarget | null, host: HTMLElement): HTMLElement | null {
  if (!(target instanceof Element) || !host.contains(target)) return null;
  const row = target.closest<HTMLElement>("[data-feed-row]");
  if (row === null || !host.contains(row)) return null;
  return row.hasAttribute(SELECTION_GOVERNED_ATTRIBUTE) ? row : null;
}

/** What a click on a governed bubble asks of the daemon. */
export type BubbleClick =
  | { case: "select"; row: string }
  | { case: "clear" }
  | { case: "ignore"; reason: "not-landed" | "interactive" | "text-selection" };

/**
 * The whole click decision for a governed ROW. A click on a link or a control
 * belongs to it, and a click that ends a text highlight is a selection
 * gesture; a row the daemon has not made selectable yet does nothing; the
 * selected row clears; any other selectable row is selected.
 */
export function bubbleClick(opts: {
  row: HTMLElement;
  target: Element;
  selectedRow: string | null;
  selectedText: string;
}): BubbleClick {
  if (opts.target.closest(CLICK_THROUGH_SELECTOR) !== null) return { case: "ignore", reason: "interactive" };
  if (opts.selectedText.trim() !== "") return { case: "ignore", reason: "text-selection" };
  if (!opts.row.hasAttribute(SELECTABLE_ATTRIBUTE)) return { case: "ignore", reason: "not-landed" };
  const id = opts.row.getAttribute("data-feed-row") ?? "";
  return id === opts.selectedRow ? { case: "clear" } : { case: "select", row: id };
}

/**
 * Arm click-to-select on the root feed HOST. SELECTEDROW answers the row the
 * daemon's last push selected (null for none). Answers the uninstall.
 */
export function installBubbleSelect(
  host: HTMLElement,
  ctx: AppContext,
  selectedRow: () => string | null,
  selection: () => string = () => selectedText(),
): () => void {
  const onClick = (event: MouseEvent): void => {
    const row = governedRowAt(event.target, host);
    if (row === null || !(event.target instanceof Element)) return;
    const click = bubbleClick({ row, target: event.target, selectedRow: selectedRow(), selectedText: selection() });
    switch (click.case) {
      case "ignore":
        log.debug("a click on a governed bubble selects nothing", {
          operation: "feed.bubble-click-ignored",
          context: { row: row.getAttribute("data-feed-row") ?? "unset", reason: click.reason },
        });
        return;
      case "clear":
        log.info("a click on the selected bubble clears the selection", {
          operation: "feed.bubble-click-clear",
          context: { row: row.getAttribute("data-feed-row") ?? "unset" },
        });
        void guardMalformed(ctx, "feed.selection-clear", selectFeedRow(ctx, { case: "clear", value: {} }, DESELECT_REQUEST));
        return;
      case "select":
        log.info("a click on a bubble selects it", {
          operation: "feed.bubble-click-select",
          context: { row: click.row },
        });
        void guardMalformed(
          ctx,
          "feed.selection-select",
          selectFeedRow(ctx, { case: "bubble", value: { row: { value: click.row } } }, SELECT_REQUEST),
        );
        return;
      default: {
        const other: { case: string } = click;
        return unreachableArm("BubbleClick", other.case);
      }
    }
  };
  host.addEventListener("click", onClick);
  return () => {
    host.removeEventListener("click", onClick);
  };
}

/** What the chip's `request` evidence names a click's selection as. */
export const SELECT_REQUEST = "select the clicked bubble";

/** What the chip's `request` evidence names a click on the selected bubble as. */
export const DESELECT_REQUEST = "clear the selection of the clicked bubble";
