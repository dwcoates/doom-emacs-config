/**
 * tool-group — a maximal run of CONSECUTIVE same-kind tool cards, drawn as ONE
 * bubble whose members are selectable as TABS.
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
 * ANCHORING REACHES A GROUPED MEMBER. An inactive tab's member is hidden and so
 * has no layout box; a reveal/scroll-to-row that lands on one first activates
 * its tab (`activateGroupedMember`) so the member is shown before it is scrolled
 * to. `findRowElement` still returns the member's element wherever it sits, so
 * feedids, selection and the reveal walk keep working.
 */
import { log } from "../log.js";
import { placeChildren } from "../dom.js";
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";

/** The class the group container (the tabbed bubble) wears. */
export const FEED_GROUP_CLASS = "feed-group";

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
 */
export function groupKindOf(row: FeedRow): string | null {
  switch (row.row.case) {
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

/** One member of a run: its row id and the element the controller drew for it. */
export interface GroupMember {
  id: string;
  element: HTMLElement;
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
  const member = element.closest("[data-feed-row]");
  const id = member instanceof HTMLElement ? member.getAttribute("data-feed-row") : null;
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

/** Build a store of tabbed groups for one feed. */
export function createToolGroupStore(): ToolGroupStore {
  const groups = new Map<string, ToolGroup>();
  let seen = new Set<string>();
  return {
    arrange(key, kind, members) {
      seen.add(key);
      let group = groups.get(key);
      if (group === undefined) {
        group = createToolGroup(kind);
        groups.set(key, group);
      }
      group.update(members);
      return group.wrapper;
    },
    prune() {
      for (const [key, group] of groups) {
        if (seen.has(key)) continue;
        group.dispose();
        groups.delete(key);
      }
      seen = new Set();
    },
  };
}

/** One tabbed group container, persistent across arrange passes. */
function createToolGroup(kind: string): ToolGroup {
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

  const strip = document.createElement("div");
  strip.className = "feed-group-strip";
  strip.setAttribute("role", "tablist");

  const panel = document.createElement("div");
  panel.className = "feed-group-panel";

  group.append(strip, panel);
  wrapper.append(group);

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
    group.setAttribute("data-count", String(members.length));
    group.setAttribute("data-active", activeId ?? "");
    log.debug("laid out a tabbed tool group", {
      operation: "feed.tool-group.layout",
      context: { kind, members: members.length, active: activeId ?? "none", picked: picked ?? "auto" },
    });
  }

  /** One tab: the kind's icon and the member's 1-based index. */
  function tab(member: GroupMember, index: number, activeId: string): HTMLButtonElement {
    const btn = document.createElement("button");
    // A BUTTON so the feed-wide click-to-expand handler treats it as a control
    // (CLICK_THROUGH_SELECTOR in expand.ts) and a tab pick never toggles a
    // capped section.
    btn.type = "button";
    btn.className = "feed-group-tab";
    btn.setAttribute(GROUP_TAB_MEMBER_ATTRIBUTE, member.id);
    btn.setAttribute("role", "tab");
    const active = member.id === activeId;
    btn.setAttribute("aria-selected", active ? "true" : "false");
    btn.classList.toggle("is-active", active);

    const icon = document.createElement("span");
    icon.className = "feed-group-tab-icon";
    icon.setAttribute("aria-hidden", "true");
    icon.textContent = groupGlyph(kind);

    const idx = document.createElement("span");
    idx.className = "feed-group-tab-index";
    idx.textContent = String(index + 1);

    btn.append(icon, idx);
    btn.addEventListener("click", () => {
      picked = member.id;
      layout(current);
    });
    return btn;
  }

  return {
    wrapper,
    update: layout,
    dispose(): void {
      activators.delete(group);
      // The members are the controller's; the arrange pass that dropped this
      // group has already re-placed or discarded each of them, so the wrapper is
      // detached without touching them.
      wrapper.remove();
    },
  };
}
