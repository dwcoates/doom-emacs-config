/**
 * tab-strip — the merge bubble's tabs: what a tab IS, and the strip they draw.
 *
 * A TAB IS A ROW of the bubble's own sub-feed, not a field of the head. That is
 * the whole reason the merge bubble can be LAZY: a collapsed bubble transfers
 * its head and nothing else, and the tabs arrive with the sub-feed when the
 * reader opens it. It is also why a strip is APPEND-ONLY in served order —
 * a second round of a phase is a SECOND TAB ("committing (2)"), never a
 * reopened one — so a strip is a history of the run, read left to right.
 *
 * STRIPS NEST BY THE ROW'S PARENT. A tab row with no parent is in the bubble's
 * own strip; a subtab names the tab row it is drawn in (a commit under the
 * rebasing tab, a run under the tests tab), so `childTabsOf` is the one way a
 * strip finds its tabs, at every level. A fixing attempt names its run too,
 * and is found the same way, but drawn as the run's remediation view rather
 * than as a badge.
 *
 * EACH KIND OWNS ITS STATE ONEOF, and since nothing parks every one of them is
 * live or settled, conflict resolution and fixing also waiting on the user
 * (the leaf carries the same start); this module therefore reads the state generically (every
 * kind's oneof selects from the SAME leaf messages) while the kind stays the
 * arm it was served as. An unset kind or an unset state is a malformed view.
 *
 * NO COUNTS ON A BADGE (R5): a tab says its label, its round beyond the first,
 * how long it has been (or was) in its state, and its state glyph. "8/12"
 * would be the client counting, and the client counts nothing.
 *
 * THE DURATION IS THE CLIENT'S CLOCK OVER THE DAEMON'S INSTANTS (owner
 * request, 2026-10-01): a live tab ticks from its `started_at_ms`, a settled
 * one shows `ended_at_ms - started_at_ms` and stops, through the one
 * elapsed-clock builder every other clock on the page uses.
 */
import { createControl, type Control } from "../../control.js";
import { repaintMark } from "../../dom.js";
import type { Ticker } from "../../clock.js";
import { reservedLiveElapsedClock, reservedSettledElapsedClock } from "../../elapsed-clock.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage } from "../../rpc/strict.js";
import { stopTicking } from "../ticking.js";
import type {
  FeedMergeTab,
  FeedMergeTabLabel,
  FeedMergeTabLive,
  FeedMergeTabSettled,
  FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedMergeTab";

/** The kinds whose content the row itself carries. */
export const RESOLVED_KINDS: readonly string[] = [
  "queue",
  "committing",
  "updatingMain",
];

/** The kinds whose content is the sub-feed rows parented to them. */
export const AGENTIC_KINDS: readonly string[] = [
  "resolving",
  "prePrompt",
  "testsRunFixing",
  "postPrompt",
];

/** The kinds whose content is a strip of their own: the tabs parented to them. */
export const PARENT_KINDS: readonly string[] = [
  "rebasing",
  "tests",
];

/**
 * The kinds whose content is VIEWS of one panel rather than a strip: a run,
 * whose tests are its own row's and whose remediation is its child fixing tab,
 * which is therefore never drawn as a badge.
 */
export const VIEWED_KINDS: readonly string[] = ["testsRun"];

/**
 * The kinds that carry content of their own AND hold agent rows: a commit,
 * whose conflict resolution's rows are parented to it.
 */
export const SECTIONED_KINDS: readonly string[] = ["rebaseCommit"];

/**
 * The glyph each tab state reports itself with. Glyphs, never emojis, with one
 * exception the owner asked for by name (2026-10-08): a tab whose agent waits
 * on the user wears the question mark emoji. A commit the replay has not
 * reached yet (`pending`) wears no glyph and no clock. A test run waiting for
 * its repository's test turn (`waitingForTurn`) wears a hollow dot: it is
 * under way, with its clock ticking, but nothing of it runs yet.
 */
const STATE_GLYPHS = {
  live: "●",
  waitingOnUser: "❓",
  waitingForTurn: "○",
  succeeded: "✓",
  failed: "✗",
} as const satisfies Record<string, string>;

/** A tab as this module reads it: its row, its kind arm, its state arm. */
export interface MergeTab {
  /** The sub-feed row carrying this tab (its id is what rows parent to). */
  row: FeedRow;
  /** The strip entry's id: the row's id value, verbatim — an address, never parsed. */
  id: string;
  tab: FeedMergeTab;
  /** The label the badge draws: the row's own, unless the strip names it. */
  label: FeedMergeTabLabel;
  /** The `kind` oneof arm ("queue", "prePrompt", …). */
  kind: string;
  /** The `state` oneof arm within that kind ("pending", "live", "waitingOnUser", "waitingForTurn", "settled"). */
  state: string;
  /** For a settled tab, the `outcome` arm ("succeeded" | "failed"). */
  outcome?: string;
  /** When the tab's work began, epoch ms; unset for a pending tab. */
  startedMs?: number;
  /** For a settled tab, when it settled, epoch ms. */
  endedMs?: number;
  /** The kind arm's own message, for the body renderer to read content off. */
  value: Record<string, unknown>;
}

/**
 * The merge tabs of one strip, in served order: the tab rows parented to
 * PARENT, or, with no parent, the bubble's own top-level strip.
 */
export function childTabsOf(rows: readonly FeedRow[], parent: string | undefined): MergeTab[] {
  const tabs: MergeTab[] = [];
  for (const row of rows) {
    if (row.row.case !== "mergeTab") continue;
    if (row.parent?.row?.value !== parent) continue;
    tabs.push(readMergeTab(row, row.row.value));
  }
  return tabs;
}

/** Every merge tab row's id in a sub-feed, at every level. */
export function mergeTabIdsOf(rows: readonly FeedRow[]): Set<string> {
  const ids = new Set<string>();
  for (const row of rows) {
    if (row.row.case === "mergeTab") ids.add(requireMessage(row.id, "FeedRow.id").value);
  }
  return ids;
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
  // Every kind's state arms are the SHARED leaves, so the instants read the
  // same off whichever kind this is; a pending commit has begun nothing.
  const leaf = state.value as FeedMergeTabLive | FeedMergeTabSettled;
  const statePath = `${PATH}.${kind.case}.${state.case}`;
  return {
    row,
    id: requireMessage(row.id, "FeedRow.id").value,
    tab,
    label: requireMessage(tab.label, `${PATH}.label`),
    kind: kind.case,
    state: state.case,
    outcome: settled?.case,
    startedMs: state.case === "pending" ? undefined : msOf(leaf.startedAtMs, `${statePath}.started_at_ms`),
    endedMs:
      state.case === "settled"
        ? msOf((leaf as FeedMergeTabSettled).endedAtMs, `${statePath}.ended_at_ms`)
        : undefined,
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
export function drawFeedMergeTab(
  tab: MergeTab,
  opts: { active: boolean; ticker: Ticker },
): Control {
  const el = createControl();
  el.className = "merge-tab";
  el.setAttribute("data-merge-tab", tab.kind);
  el.setAttribute("data-tab-state", tab.state);
  if (tab.outcome !== undefined) el.setAttribute("data-tab-outcome", tab.outcome);
  el.setAttribute("aria-selected", opts.active ? "true" : "false");
  el.classList.toggle("is-active", opts.active);
  el.append(drawFeedMergeTabLabel(tab.label));
  el.append(drawTabDuration(tab, opts.ticker));
  el.append(drawTabStateGlyph(tab));
  return el;
}

/**
 * Bring EL, a tab drawn from PREVIOUS, to TAB IN PLACE.
 *
 * THE TAB KEEPS ITS ELEMENTS, so a push that changed nothing the reader sees
 * changes nothing drawn: the state glyph is the same element repainted only
 * where its state moved, so a live dot's breath runs on unbroken (a fresh
 * element restarts it, which read as the dot blinking on every push), and the
 * duration is redrawn only when its clock's instants moved, so a live clock
 * keeps its one subscription.
 */
export function updateFeedMergeTab(
  el: HTMLElement,
  tab: MergeTab,
  previous: MergeTab,
  opts: { active: boolean; ticker: Ticker },
): void {
  el.setAttribute("data-merge-tab", tab.kind);
  el.setAttribute("data-tab-state", tab.state);
  if (tab.outcome !== undefined) el.setAttribute("data-tab-outcome", tab.outcome);
  else el.removeAttribute("data-tab-outcome");
  el.setAttribute("aria-selected", opts.active ? "true" : "false");
  el.classList.toggle("is-active", opts.active);
  const label = el.querySelector(":scope > .merge-tab-label");
  const nextLabel = drawFeedMergeTabLabel(tab.label);
  if (label === null) el.prepend(nextLabel);
  else if (label.textContent !== nextLabel.textContent) label.replaceWith(nextLabel);
  const duration = el.querySelector(`:scope > .${TAB_DURATION_CLASS}`);
  if (duration === null || tab.startedMs !== previous.startedMs || tab.endedMs !== previous.endedMs) {
    const next = drawTabDuration(tab, opts.ticker);
    if (duration === null) el.append(next);
    else {
      stopTicking(duration);
      duration.replaceWith(next);
    }
  }
  const glyph = el.querySelector<HTMLElement>(":scope > .merge-tab-glyph");
  if (glyph === null) el.append(drawTabStateGlyph(tab));
  else paintTabStateGlyph(glyph, tab);
}

/** The class of a tab's duration, beside its label. */
export const TAB_DURATION_CLASS = "merge-tab-duration";

/**
 * How long the tab has been in its state: a live tab ticks from when its work
 * began; a settled one shows how long it ran, fixed; a pending one has no
 * time yet and draws an empty slot.
 */
export function drawTabDuration(tab: MergeTab, ticker: Ticker): HTMLElement {
  if (tab.startedMs === undefined) {
    const el = document.createElement("span");
    el.className = TAB_DURATION_CLASS;
    return el;
  }
  return tab.endedMs === undefined
    ? reservedLiveElapsedClock(ticker, TAB_DURATION_CLASS, tab.startedMs)
    : reservedSettledElapsedClock(TAB_DURATION_CLASS, tab.endedMs - tab.startedMs);
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
  el.setAttribute("aria-hidden", "true");
  paintTabStateGlyph(el, tab);
  return el;
}

/**
 * Paint EL as TAB's state glyph, touching only what differs, so a glyph whose
 * state did not move is left exactly as it stands (its breath unbroken).
 */
export function paintTabStateGlyph(el: HTMLElement, tab: MergeTab): void {
  const [modifier, glyph] = tabGlyphOf(tab);
  repaintMark(el, modifier === null ? "merge-tab-glyph" : `merge-tab-glyph ${modifier}`, glyph);
}

/** TAB's glyph modifier class and its glyph, by its state. */
function tabGlyphOf(tab: MergeTab): [string | null, string] {
  switch (tab.state) {
    case "pending":
      return ["is-pending", ""];
    case "live":
      return ["is-live", STATE_GLYPHS.live];
    case "waitingOnUser":
      // The merge's agent has a permission ask or a question open: the tab
      // says so for exactly as long as the footer reads "waiting on user".
      return ["is-waiting-on-user", STATE_GLYPHS.waitingOnUser];
    case "waitingForTurn":
      // Another merge of the repository runs its tests; this run is in line.
      return ["is-waiting-for-turn", STATE_GLYPHS.waitingForTurn];
    case "settled":
      return tab.outcome === "failed"
        ? ["is-failed", STATE_GLYPHS.failed]
        : ["is-succeeded", STATE_GLYPHS.succeeded];
    default:
      // Unreachable through `readMergeTab`, which validates the arm first.
      log.warn(`a merge tab reports a state this build has no glyph for: ${tab.state}`, {
        operation: "merge.unknown-tab-state",
        context: { state: tab.state, kind: tab.kind },
      });
      return [null, ""];
  }
}

/**
 * Which tab the bubble shows when the reader has not chosen one: the MOST
 * RELEVANT tab, which is the one whose state changed last (owner request,
 * 2026-10-13).
 *
 * A TAB'S LAST CHANGE IS ITS LAST INSTANT: when it settled, else when its work
 * began. Those are the daemon's own instants, so the choice is the same whether
 * the strip followed every push or was first drawn just now (a lazily opened
 * bubble, a reloaded page), and a re-push that only moved a clock or redrew a
 * live tab's content moves nothing.
 *
 * A CONCLUSION HANDS THE VIEW BACK TO WHAT IS STILL RUNNING: when the last
 * change was a tab settling, the tab still running is where the merge is (the
 * queue tab of a merge that went back to waiting after rebasing in line), the
 * most recently started one if several are. With nothing running the
 * concluded tab stays shown, since on a settled run that is where it ended.
 *
 * Equal instants go to the later tab, because the strip is chronological: a
 * step that settles as the next begins hands over to the next. A strip with no
 * instant at all (only commits the replay has not reached) shows its last tab.
 */
export function autoSelectedTab(tabs: readonly MergeTab[]): MergeTab | undefined {
  const latest = latestBy(tabs, (tab) => tab.endedMs ?? tab.startedMs);
  if (latest === undefined) return tabs[tabs.length - 1];
  if (latest.state !== "settled") return latest;
  // A tab waiting on the user, or a run waiting for its test turn, is still
  // under way: the merge is there.
  const running = tabs.filter(isUnderWay);
  return latestBy(running, (tab) => tab.startedMs) ?? latest;
}

/** The tab with the greatest INSTANT, the later one on a tie; tabs without one never win. */
function latestBy(tabs: readonly MergeTab[], instant: (tab: MergeTab) => number | undefined): MergeTab | undefined {
  let latest: MergeTab | undefined;
  let latestMs = -Infinity;
  for (const tab of tabs) {
    const ms = instant(tab);
    if (ms === undefined || ms < latestMs) continue;
    latest = tab;
    latestMs = ms;
  }
  return latest;
}

/** Whether TAB's work is under way: running, held on the user, or in line for its test turn. */
export function isUnderWay(tab: MergeTab): boolean {
  return tab.state === "live" || tab.state === "waitingOnUser" || tab.state === "waitingForTurn";
}
