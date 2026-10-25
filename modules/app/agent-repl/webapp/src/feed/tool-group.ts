/**
 * tool-group — a maximal run of CONSECUTIVE same-kind tool cards, shown one at a
 * time in its own card, with its members selectable as TABS inside that card.
 *
 * WHY IT LIVES AT THE SEQUENCE LAYER. Grouping is a fact about ADJACENCY — ten
 * Bash cards in a row, three subagent cards in a row — and adjacency is known
 * only where the ordered rows are laid out, never inside a single card
 * renderer. So `arrangeSubfeedRows` (renderers.ts) is the one seam that consults
 * this module: it hands each maximal run of >=2 consecutive same-SPECIFIC-kind
 * top-level tool cards to a persistent group, which reuses the members' own
 * elements (chrome, folds, clocks and all) and shows one at a time behind a tab
 * strip. A lone card (run length 1) is never grouped and renders exactly as
 * today.
 *
 * THE MEMBERS ARE NOT REDRAWN. A member is the SAME `<article>` the controller
 * owns and reuses across pushes (feed-view.ts). This module only MOVES it into
 * the active group's panel and toggles its visibility, so every member keeps
 * its streaming, its expand/collapse (`.tool-fold`, expand.ts) and its live
 * clocks untouched inside its tab.
 *
 * SELECTION IS THE READER'S AND IT STICKS. A brand-new group shows its newest
 * member; once the reader picks a tab their choice stands, even as new same-kind
 * cards stream in and extend the run — a live append adds a tab and never yanks
 * the reader off the tab they are reading. The group is PERSISTENT (held in a
 * store across redraws, keyed by its first member) precisely so that pick
 * survives a re-arrange.
 *
 * THE TABS SIT INSIDE THE MEMBER'S OWN CARD (owner ruling, 2026-10-10). There
 * is no outer bubble: the group draws no chrome of its own, and the tabline
 * (the strip and the clock slot after it, tabs.ts) is laid over the top of the
 * shown member's `.tool-card`, whose top padding grows by the tabline's
 * measured height (`--feed-group-tabline-h`, kept current by a
 * `ResizeObserver` on the tabline). The tabline is NOT a child of the card: the
 * card belongs to its renderer, and a redraw that keeps a scrolled box morphs
 * the card's children by index (keep-scroll.ts), which a foreign first child
 * would throw off. So the group owns the tabline and the stylesheet places it.
 *
 * THE SHOWN MEMBER CLOCKS ON THE TABLINE. A card with tabs clocks on its
 * tabline, not its head line (tabs.ts), so the group draws the shown member's
 * tool card clock (`GroupMember.clock`) in the tabline's clock slot, and the
 * stylesheet hides the clock the member's own head still draws while it sits
 * in a group's panel. A member drawn alone again shows its head clock, because
 * it is no longer in a panel.
 *
 * ANCHORING REACHES A GROUPED MEMBER. An inactive tab's member is hidden and so
 * has no layout box; a reveal/scroll-to-row that lands on one first activates
 * its tab (`activateGroupedMember`) so the member is shown before it is scrolled
 * to. `findRowElement` still returns the member's element wherever it sits, so
 * feedids, selection and the reveal walk keep working.
 */
import type { Control } from "../control.js";
import type { Ticker } from "../clock.js";
import { drawFeedTab, drawFeedTabline, drawFeedTabStrip, setFeedTablineClock } from "./tabs.js";
import { drawToolCardClock } from "./cards/tool-head.js";
import { log } from "../log.js";
import { placeChildren } from "../dom.js";
import { createKeyedStore } from "./keyed-store.js";
import { isMalformedView } from "../rpc/malformed.js";
import type { FeedRow, FeedToolCardClock } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { feedRowId, feedRowOf } from "./row-id.js";

/** The class the group container wears; it draws no bubble of its own. */
export const FEED_GROUP_CLASS = "feed-group";

/**
 * The custom property the group carries its tabline's height on, in px: the
 * extra top padding a member card leaves so the tabline sits inside it.
 */
export const TABLINE_HEIGHT_PROPERTY = "--feed-group-tabline-h";

/** The attribute a tab button carries the member id it selects on. */
export const GROUP_TAB_MEMBER_ATTRIBUTE = "data-group-tab-member";

/**
 * The specific tool/card kind a row groups BY, or null when it is not a
 * groupable tool card.
 *
 * SAME SPECIFIC KIND, never a broad "any tool" bucket: Bash groups only with
 * Bash and Edit only with Edit, so `simple_tool_call` keys on the tool's name.
 * The groupable family is exactly the tool-card family — the inline tool call
 * (by tool), skill, hook, and the subagent and detached-shell bubbles. A
 * response, a prompt, a thinking fold, a merge bubble, a plan/findings/artifact
 * card, an ask (question/permission/cold-gate) or any other row returns null and
 * so BREAKS a run rather than joining it.
 *
 * ONE KIND IS NOT A TOOL CARD: a turn's vendor-fault ending, the "vendor
 * error" outcome card, groups with the vendor-fault endings beside it under
 * {@link VENDOR_FAULT_KIND}, so an outage's endings stack as one card. Every
 * other ending (a conclusion, a stop, an agent-repl fault) breaks a run.
 */
export function groupKindOf(row: FeedRow): string | null {
  switch (row.row.case) {
    case "turnEnded":
      // A turn's VENDOR-FAULT ending — the "vendor error" outcome card. A
      // vendor outage ends turn after turn (agent-repl retries every failed
      // turn), so its cards stack exactly as a run of tool cards does.
      return isVendorFaultEnding(row) ? VENDOR_FAULT_KIND : null;
    case "shellHead":
      // A detached shell's collapsed head bubble — "bash/shell".
      return "shell";
    case "detachedSubagent":
      // A subagent bubble that moved to its detached placement; it groups with
      // the synchronous subagent below under the one "subagent" kind.
      return "subagent";
    case "activity": {
      const unit = row.row.value.unit;
      switch (unit.case) {
        case "simpleToolCall":
          return `tool:${unit.value.name?.text ?? ""}`;
        case "skill":
          return "skill";
        case "hook":
          return "hook";
        case "subagent":
          return "subagent";
        default:
          // response, artifact, plan, findings, merge — not a tool card.
          return null;
      }
    }
    default:
      return null;
  }
}

/** The kind a turn's vendor-fault ending groups by. */
export const VENDOR_FAULT_KIND = "vendor-fault";

/** Whether ROW is a turn ending whose outcome marker is the vendor-fault family. */
function isVendorFaultEnding(row: FeedRow): boolean {
  if (row.row.case !== "turnEnded") return false;
  const outcome = row.row.value.outcome;
  return outcome.case === "errored" && outcome.value.marker?.family.case === "vendorFault";
}

/**
 * Whether a run of KIND may span turns.
 *
 * A RUN NEVER SPANS TWO TURNS (owner-approved, 2026-09-24) — except a run of
 * vendor-fault endings, which only ever spans turns: each ending IS its own
 * turn's last row, so one turn never holds two of them.
 */
export function groupSpansTurns(kind: string): boolean {
  return kind === VENDOR_FAULT_KIND;
}

/** The glyphs a few well-known tools tab with. Glyphs, never emojis. */
const TOOL_GLYPHS: Readonly<Record<string, string>> = {
  Bash: "$",
  Read: "≡",
  Edit: "✎",
  Write: "✎",
  MultiEdit: "✎",
  NotebookEdit: "✎",
  Grep: "⌕",
  Glob: "⌕",
  Search: "⌕",
  WebFetch: "⚓",
  WebSearch: "⌕",
};

/** The glyph each non-tool-call groupable kind tabs with. */
const KIND_GLYPHS: Readonly<Record<string, string>> = {
  shell: "$",
  skill: "★",
  subagent: "◆",
  hook: "⎇",
  // The vendor-fault marker's own glyph (marker.ts MARKER_GLYPH_CHARS.diamond).
  [VENDOR_FAULT_KIND]: "◆",
};

/** What the tab of an unrecognized kind draws. */
const GENERIC_GLYPH = "•";

/**
 * The icon a tab draws for KIND: the card's own kind glyph.
 *
 * A known tool or kind draws its curated glyph; an unknown TOOL falls back to
 * its name's initial (a legible monogram rather than a meaningless dot), and any
 * other unknown kind draws the generic glyph.
 */
export function groupGlyph(kind: string): string {
  if (kind.startsWith("tool:")) {
    const tool = kind.slice("tool:".length);
    return TOOL_GLYPHS[tool] ?? (tool.charAt(0).toUpperCase() || GENERIC_GLYPH);
  }
  return KIND_GLYPHS[kind] ?? GENERIC_GLYPH;
}

/** A tool card's clock, and where it sits in the view for a malformed one's report. */
export interface MemberClock {
  readonly clock: FeedToolCardClock;
  readonly path: string;
}

/**
 * One member of a run: its row id, the element the controller drew for it, and
 * the clock its tool card head draws, when it has one.
 */
export interface GroupMember {
  id: string;
  element: HTMLElement;
  clock?: MemberClock;
}

/**
 * The clock ROW's tool card head draws (`drawToolCardHead`), at the path its
 * renderer reports it under, or undefined when the row draws no such clock.
 *
 * Exactly the groupable kinds that head through `drawToolCardHead`: the inline
 * tool call, the skill and the hook. A subagent or shell bubble heads with its
 * own renderer and its own clock, which stays on its head.
 */
export function memberClockOf(row: FeedRow): MemberClock | undefined {
  if (row.row.case !== "activity") return undefined;
  const unit = row.row.value.unit;
  switch (unit.case) {
    case "simpleToolCall":
      return clockAt(unit.value.clock, "FeedSimpleToolCall.clock");
    case "skill":
      return clockAt(unit.value.clock, "FeedSkill.clock");
    case "hook":
      return clockAt(unit.value.clock, "FeedHook.clock");
    default:
      return undefined;
  }
}

/** CLOCK at PATH, or undefined when the producer stated none. */
function clockAt(clock: FeedToolCardClock | undefined, path: string): MemberClock | undefined {
  return clock === undefined ? undefined : { clock, path };
}

/** The activation hooks for open groups, keyed by their `.feed-group` element. */
const activators = new WeakMap<HTMLElement, (id: string) => void>();

/**
 * Show the tab holding ELEMENT, when ELEMENT sits inside a tabbed group.
 *
 * The dual of an inactive tab being hidden: a reveal/scroll-to-row that lands on
 * a grouped member must first bring its tab to the front, or it would scroll to
 * an element with no layout box. A member that is not inside a group is left
 * alone.
 */
export function activateGroupedMember(element: HTMLElement): void {
  const group = element.closest(`.${FEED_GROUP_CLASS}`);
  if (!(group instanceof HTMLElement)) return;
  const member = feedRowOf(element);
  const id = member instanceof HTMLElement ? feedRowId(member) : null;
  if (id === null) return;
  activators.get(group)?.(id);
}

/** One persistent tabbed group: its wrapper element and its lifecycle. */
interface ToolGroup {
  /** The element `arrangeSubfeedRows` places at the top level. */
  readonly wrapper: HTMLElement;
  /** Re-lay the strip and panel for the run's current members. */
  update(members: readonly GroupMember[]): void;
  /** Drop the group: detach it and forget its activation hook. */
  dispose(): void;
}

/**
 * The store of open groups, one per body renderer (so per open feed).
 *
 * It outlives a single arrange pass, which is what lets a reader's tab pick and
 * a group's identity survive the re-arrange every live push triggers. A group is
 * keyed by its kind and its FIRST member's id: appending same-kind cards keeps
 * that key (the first member does not move), so the group — and the pick — is
 * reused, while a run that breaks yields a genuinely different first member and
 * so a genuinely different group.
 */
export interface ToolGroupStore {
  /**
   * Reuse or create the group for KEY, lay it out for MEMBERS, and return the
   * wrapper to place at the top level.
   */
  arrange(key: string, kind: string, members: readonly GroupMember[]): HTMLElement;
  /** Dispose every group not `arrange`d since the previous prune. */
  prune(): void;
}

/** The key a run is stored under: its kind and its first member's id. */
export function groupKey(kind: string, firstMemberId: string): string {
  return `${kind}::${firstMemberId}`;
}

/** Build a store of tabbed groups for one feed, whose clocks repaint on TICKER. */
export function createToolGroupStore(ticker: Ticker): ToolGroupStore {
  const groups = createKeyedStore<ToolGroup>();
  return {
    arrange(key, kind, members) {
      const group = groups.use(key, () => createToolGroup(kind, ticker));
      group.update(members);
      return group.wrapper;
    },
    prune: groups.prune,
  };
}

/** One tabbed group container, persistent across arrange passes. */
function createToolGroup(kind: string, ticker: Ticker): ToolGroup {
  /** The reader's chosen member id, or null while the newest is shown. */
  let picked: string | null = null;
  /** The members of the most recent layout, so a click can re-lay them. */
  let current: readonly GroupMember[] = [];

  // The wrapper is a `.feed-item` so the shared width cap (`.feed-item >
  // .feed-group` in styles.css) applies to the group exactly as it does to a
  // lone tool card. It is arrange-owned, never a controller row, so it carries
  // no `data-feed-row` of its own.
  const wrapper = document.createElement("article");
  wrapper.className = "feed-item feed-group-item";

  const group = document.createElement("div");
  group.className = FEED_GROUP_CLASS;
  group.setAttribute("data-group-kind", kind);

  const strip = drawFeedTabStrip("feed-group-strip");
  const tabline = drawFeedTabline(strip, null);
  tabline.classList.add("feed-group-tabline");

  const panel = document.createElement("div");
  panel.className = "feed-group-panel";

  group.append(tabline, panel);
  wrapper.append(group);

  // THE TABLINE'S HEIGHT IS MEASURED, NOT ASSUMED: the tabs wrap onto more rows
  // as the run grows, and the member card's top padding must follow so the
  // tabline never covers the card's head.
  const tablineSize = new ResizeObserver(() => {
    group.style.setProperty(TABLINE_HEIGHT_PROPERTY, `${String(tabline.offsetHeight)}px`);
  });
  tablineSize.observe(tabline);

  activators.set(group, (id) => {
    picked = id;
    layout(current);
  });

  function layout(members: readonly GroupMember[]): void {
    current = members;
    const ids = members.map((m) => m.id);
    if (picked !== null && !ids.includes(picked)) picked = null;
    // The newest member is the default face of a group the reader has not
    // touched; their pick, once made, stands even as new tabs append.
    const activeId = picked ?? ids[ids.length - 1];

    strip.replaceChildren(...members.map((member, index) => tab(member, index, activeId)));
    // Only a member out of place is moved (`placeChildren`): re-appending one
    // already in place would re-attach it, and a re-attached element loses the
    // reader's scroll position inside it (owner rule, 2026-09-23).
    placeChildren(
      panel,
      members.map((member) => member.element),
    );
    for (const member of members) member.element.hidden = member.id !== activeId;
    setFeedTablineClock(tabline, shownClock(members.find((member) => member.id === activeId)));
    group.setAttribute("data-count", String(members.length));
    group.setAttribute("data-active", activeId ?? "");
    log.debug("laid out a tabbed tool group", {
      operation: "feed.tool-group.layout",
      context: { kind, members: members.length, active: activeId ?? "none", picked: picked ?? "auto" },
    });
  }

  /** One tab: the kind's icon and the member's 1-based index. */
  function tab(member: GroupMember, index: number, activeId: string): Control {
    const btn = drawFeedTab({
      icon: groupGlyph(kind),
      index: index + 1,
      active: member.id === activeId,
      onPick: () => {
        picked = member.id;
        layout(current);
      },
    });
    btn.setAttribute(GROUP_TAB_MEMBER_ATTRIBUTE, member.id);
    return btn;
  }

  /**
   * The tabline clock of the shown MEMBER, or null when it states none.
   *
   * A malformed clock draws none here: the member's own head drew from the
   * same clock first, so its row draw already failed, was logged and was
   * reported (feed-view.ts, the one owner of a malformed row's report), and the
   * member shows that row's placeholder rather than a card to clock.
   */
  function shownClock(member: GroupMember | undefined): HTMLElement | null {
    if (member?.clock === undefined) return null;
    try {
      return drawToolCardClock(member.clock.clock, member.clock.path, ticker);
    } catch (err) {
      if (!isMalformedView(err)) throw err;
      return null;
    }
  }

  return {
    wrapper,
    update: layout,
    dispose(): void {
      activators.delete(group);
      tablineSize.disconnect();
      // The tabline's clock is the group's own, so it is stopped here.
      setFeedTablineClock(tabline, null);
      // The members are the controller's; the arrange pass that dropped this
      // group has already re-placed or discarded each of them, so the wrapper is
      // detached without touching them.
      wrapper.remove();
    },
  };
}
