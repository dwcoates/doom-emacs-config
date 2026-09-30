/**
 * tab-strip — the merge bubble's tabs: what a tab IS, and the strip they draw.
 *
 * A TAB IS A ROW of the bubble's own sub-feed, not a field of the head. That is
 * the whole reason the merge bubble can be LAZY: a collapsed bubble transfers
 * its head and nothing else, and the tabs arrive with the sub-feed when the
 * reader opens it. It is also why the strip is APPEND-ONLY in served order —
 * a second round of a phase is a SECOND TAB ("tests (2)"), never a reopened
 * one — so the strip is a history of the run, read left to right.
 *
 * EACH KIND OWNS ITS STATE ONEOF, and since nothing parks every one of them is
 * live or settled; this module therefore reads the state generically (every
 * kind's oneof selects from the SAME leaf messages) while the kind stays the
 * arm it was served as. An unset kind or an unset state is a malformed view.
 *
 * NO COUNTS ON A BADGE (R5): a tab says its label, its round beyond the first,
 * and its state glyph. "8/12" would be the client counting, and the client
 * counts nothing.
 */
import { log } from "../../log.js";
import { requireCase, requireMessage } from "../../rpc/strict.js";
import type {
  FeedMergeTab,
  FeedMergeTabLabel,
  FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedMergeTab";

/** The kinds whose content the row itself carries. */
export const RESOLVED_KINDS: readonly string[] = [
  "queue",
  "rebasing",
  "tests",
  "committing",
  "updatingMain",
];

/** The kinds whose content is the sub-feed rows parented to them. */
export const AGENTIC_KINDS: readonly string[] = [
  "prePrompt",
  "conflicts",
  "fixes",
  "postPrompt",
];

/** The glyph each tab state reports itself with. Glyphs, never emojis. */
const STATE_GLYPHS = {
  live: "●",
  succeeded: "✓",
  failed: "✗",
} as const satisfies Record<string, string>;

/** A tab as this module reads it: its row, its kind arm, its state arm. */
export interface MergeTab {
  /** The sub-feed row carrying this tab (its id is what rows parent to). */
  row: FeedRow;
  /** The row's id value, verbatim — an address, never parsed. */
  id: string;
  tab: FeedMergeTab;
  /** The `kind` oneof arm ("queue", "prePrompt", …). */
  kind: string;
  /** The `state` oneof arm within that kind ("live", "settled"). */
  state: string;
  /** For a settled tab, the `outcome` arm ("succeeded" | "failed"). */
  outcome?: string;
  /** The kind arm's own message, for the body renderer to read content off. */
  value: Record<string, unknown>;
}

/** Every merge tab of a sub-feed, in served order. */
export function mergeTabsOf(rows: readonly FeedRow[]): MergeTab[] {
  const tabs: MergeTab[] = [];
  for (const row of rows) {
    if (row.row.case !== "mergeTab") continue;
    tabs.push(readMergeTab(row, row.row.value));
  }
  return tabs;
}

/** One tab row, validated into the shape the strip and the body both read. */
export function readMergeTab(row: FeedRow, tab: FeedMergeTab): MergeTab {
  const kind = requireCase(tab.kind, `${PATH}.kind`);
  const value = kind.value as Record<string, unknown>;
  const state = requireCase(
    (value["state"] as { case?: string; value?: unknown } | undefined) ?? {},
    `${PATH}.${kind.case}.state`,
  );
  const settled =
    state.case === "settled"
      ? requireCase(
          (state.value as { outcome?: { case?: string; value?: unknown } }).outcome ?? {},
          `${PATH}.${kind.case}.settled.outcome`,
        )
      : undefined;
  return {
    row,
    id: requireMessage(row.id, "FeedRow.id").value,
    tab,
    kind: kind.case,
    state: state.case,
    outcome: settled?.case,
    value,
  };
}

/**
 * The tab's own badge: the label, the round beyond the first, the state glyph.
 *
 * The element is a BUTTON because the strip is the one place a merge bubble
 * takes a click of its own; selecting a tab is purely local (nothing is
 * fetched, the rows are all already here), so it raises no rpc.
 */
export function drawFeedMergeTab(tab: MergeTab, opts: { active: boolean }): HTMLButtonElement {
  const el = document.createElement("button");
  el.type = "button";
  el.className = "merge-tab";
  el.setAttribute("data-merge-tab", tab.kind);
  el.setAttribute("data-tab-state", tab.state);
  if (tab.outcome !== undefined) el.setAttribute("data-tab-outcome", tab.outcome);
  el.setAttribute("aria-selected", opts.active ? "true" : "false");
  el.classList.toggle("is-active", opts.active);
  el.append(drawFeedMergeTabLabel(requireMessage(tab.tab.label, `${PATH}.label`)));
  el.append(drawTabStateGlyph(tab));
  return el;
}

/**
 * The tab's drawn word, plus its round when it is not the first.
 *
 * ROUND 1 DRAWS NOTHING: the first pass through a phase is just "tests", and
 * decorating it would make an ordinary run look like a retry.
 */
export function drawFeedMergeTabLabel(label: FeedMergeTabLabel): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-tab-label";
  el.textContent = label.round > 1 ? `${label.text} (${label.round})` : label.text;
  return el;
}

/** The state glyph: filled dot live, check or cross settled. */
export function drawTabStateGlyph(tab: MergeTab): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-tab-glyph";
  el.setAttribute("aria-hidden", "true");
  switch (tab.state) {
    case "live":
      el.classList.add("is-live");
      el.textContent = STATE_GLYPHS.live;
      return el;
    case "settled":
      if (tab.outcome === "failed") {
        el.classList.add("is-failed");
        el.textContent = STATE_GLYPHS.failed;
        return el;
      }
      el.classList.add("is-succeeded");
      el.textContent = STATE_GLYPHS.succeeded;
      return el;
    default:
      // Unreachable through `readMergeTab`, which validates the arm first.
      log.warn(`a merge tab reports a state this build has no glyph for: ${tab.state}`, {
        operation: "merge.unknown-tab-state",
        context: { state: tab.state, kind: tab.kind },
      });
      return el;
  }
}

/**
 * Which tab the bubble shows when the reader has not chosen one.
 *
 * THE LAST TAB THAT IS STILL RUNNING, because that is where the merge actually
 * is; failing that the last tab, which on a settled
 * run is where it ended. Never a "most interesting" heuristic — the strip is
 * chronological and the rule is positional.
 */
export function autoSelectedTab(tabs: readonly MergeTab[]): MergeTab | undefined {
  for (let i = tabs.length - 1; i >= 0; i -= 1) {
    const tab = tabs[i];
    if (tab.state === "live") return tab;
  }
  return tabs[tabs.length - 1];
}
