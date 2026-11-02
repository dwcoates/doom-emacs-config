/**
 * deploys-fold — WHETHER EACH REPOSITORY'S GROUP IN THE MERGE QUEUE AND
 * RECENTLY MERGED SECTIONS IS FOLDED (owner requests, 2026-10-31 and
 * 2026-11-01; frontend.v1.RosterMergeQueueRepoGroup,
 * frontend.v1.RosterMergedRepoGroup).
 *
 * EVERY GROUP IS OPEN UNTIL ITS HEADER IS CLICKED. A click folds or opens
 * it, and that choice lasts only until:
 *   - a Merge Queue group's queue changes: a merge joins or leaves its line
 *     (the group's `DEPLOYS_LINE_ATTRIBUTE`);
 *   - the focus leaves the rail's page (`focus-left.ts`): a click into a
 *     feed, into another Emacs window, or Emacs moving its own focus;
 *   - a workspace outside the groups is clicked: an open row or a hidden
 *     row;
 *   - a workspace is reopened from a group (`row.ts`'s band returns).
 *
 * A GROUP IS KEYED BY ITS SECTION AND ITS REPOSITORY
 * (`DEPLOYS_SECTION_ATTRIBUTE`, `DEPLOYS_REPOSITORY_ATTRIBUTE`), so a
 * repository's queue group and its merged group fold apart.
 *
 * THE CHOICE IS THIS PAGE'S OWN, never the daemon's view state: it is a
 * glance that lasts while the user is in the rail, so it is never shared
 * with another page or kept across a restart. It outlives every draw, so a
 * push that redraws a section draws each group as it stands.
 */
import { queryAll } from "../dom.js";
import type { FocusLeft } from "../focus-left.js";
import { log } from "../log.js";

/** The class of one repository's group, in the Merge Queue and Recently Merged sections alike. */
export const DEPLOYS_GROUP_CLASS = "merged-group";

/** The attribute a group names the section it is drawn in on. */
export const DEPLOYS_SECTION_ATTRIBUTE = "data-group-section";

/** Which section a group is drawn in: `DEPLOYS_SECTION_ATTRIBUTE`'s value. */
export type DeploysGroupSection = "merge-queue" | "recently-merged";

/** The selector a Recently Merged group answers to. */
export const RECENTLY_MERGED_GROUP_SELECTOR = `.${DEPLOYS_GROUP_CLASS}[${DEPLOYS_SECTION_ATTRIBUTE}="recently-merged"]`;

/** The class a folded group wears; the stylesheet hides its body under it. */
export const DEPLOYS_GROUP_FOLDED_CLASS = "folded";

/** The attribute a group names its repository on. */
export const DEPLOYS_REPOSITORY_ATTRIBUTE = "data-repository";

/**
 * The attribute a Merge Queue group states its queue's line on: the
 * workspaces in it, in order. A Recently Merged group carries none. A choice
 * made by hand lasts while this stays as it was when the choice was made.
 */
export const DEPLOYS_LINE_ATTRIBUTE = "data-line";

/** How many merged rows an open group's band keeps room for (owner request, 2026-10-27). */
export const DEPLOYS_FLOOR = 5;

/** What a workspace outside the groups is drawn as, which a click on resets every choice. */
const OTHER_WORKSPACE_SELECTOR = "[data-roster-row]";

/** The page's one set of group folds. */
export interface DeploysFold {
  /** Whether GROUP is folded now: the hand's choice while it stands, else open. */
  folded(group: HTMLElement): boolean;
  /** Fold GROUP when it is open, open it when it is folded; the header's click. */
  toggle(group: HTMLElement): void;
  /** Drop every choice made by hand, WHY naming what the user did; nothing when none stands. */
  reset(why: string): void;
  /** Draw GROUP, freshly drawn, as its fold stands. */
  paint(group: HTMLElement): void;
}

/** One choice made by hand: the fold chosen, and the line it was chosen against. */
interface Choice {
  folded: boolean;
  line: string | null;
}

/**
 * The repository GROUP is drawn for: THE ONE READ every page-own group state
 * (its fold, its older merges) keys a group by. A group always names one, so
 * one that names none is logged under OPERATION and refused.
 */
export function deploysGroupRepository(group: HTMLElement, operation: string): string {
  const repository = group.getAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE);
  if (repository === null || repository === "") {
    log.error("a repository group cannot be keyed: it names no repository", {
      operation,
      context: { attribute: DEPLOYS_REPOSITORY_ATTRIBUTE, found: repository },
    });
    throw new Error(`${operation}: a repository group carries no ${DEPLOYS_REPOSITORY_ATTRIBUTE}`);
  }
  return repository;
}

/**
 * The section GROUP is drawn in. A group always names one of the two, so one
 * that names neither is logged under OPERATION and refused.
 */
export function deploysGroupSection(group: HTMLElement, operation: string): DeploysGroupSection {
  const section = group.getAttribute(DEPLOYS_SECTION_ATTRIBUTE);
  if (section !== "merge-queue" && section !== "recently-merged") {
    log.error("a repository group cannot be keyed: it names no section it is drawn in", {
      operation,
      context: { attribute: DEPLOYS_SECTION_ATTRIBUTE, found: section },
    });
    throw new Error(`${operation}: a repository group carries no ${DEPLOYS_SECTION_ATTRIBUTE}`);
  }
  return section;
}

/** The key GROUP's fold is kept under: its section and its repository. */
export function foldKeyOf(group: HTMLElement): string {
  const operation = "sidebar.deploys-fold.malformed";
  return `${deploysGroupSection(group, operation)}:${deploysGroupRepository(group, operation)}`;
}

/**
 * The folds of the groups drawn in ROOT. Each change repaints every group in
 * ROOT and tells CHANGED, so the bands are fitted again.
 */
export function createDeploysFold(root: ParentNode, changed: () => void): DeploysFold {
  const choices = new Map<string, Choice>();
  const folded = (group: HTMLElement): boolean => {
    const choice = choices.get(foldKeyOf(group));
    if (choice === undefined || choice.line !== group.getAttribute(DEPLOYS_LINE_ATTRIBUTE)) return false;
    return choice.folded;
  };
  const paint = (group: HTMLElement): void => {
    group.classList.toggle(DEPLOYS_GROUP_FOLDED_CLASS, folded(group));
  };
  const repaint = (): void => {
    for (const group of queryAll<HTMLElement>(root, `.${DEPLOYS_GROUP_CLASS}`)) paint(group);
    changed();
  };
  return {
    folded,
    toggle: (group) => {
      const groupKey = foldKeyOf(group);
      const to = !folded(group);
      choices.set(groupKey, { folded: to, line: group.getAttribute(DEPLOYS_LINE_ATTRIBUTE) });
      log.debug(to ? "folded a repository group" : "opened a repository group", {
        operation: "sidebar.deploys-fold",
        context: { group: groupKey, folded: to, why: "its header was clicked" },
      });
      repaint();
    },
    reset: (why) => {
      if (choices.size === 0) return;
      log.debug("opened every repository group folded by hand", {
        operation: "sidebar.deploys-fold",
        context: { groups: [...choices.keys()], why },
      });
      choices.clear();
      repaint();
    },
    paint,
  };
}

/**
 * Drop every choice FOLDS holds whenever FOCUS leaves the page, and whenever
 * a workspace outside the groups is clicked in the rail HOST.
 * Answers the uninstall.
 */
export function installDeploysFoldReset(host: HTMLElement, folds: DeploysFold, focus: FocusLeft): () => void {
  const unsubscribe = focus.onLeft((why) => folds.reset(why));
  // CAPTURED, because a row's own click handler may stop the click.
  const clicked = (event: MouseEvent): void => {
    const target = event.target;
    if (!(target instanceof Element)) return;
    if (target.closest(OTHER_WORKSPACE_SELECTOR) === null || target.closest(`.${DEPLOYS_GROUP_CLASS}`) !== null) return;
    folds.reset("a workspace outside the repository groups was clicked");
  };
  host.addEventListener("click", clicked, true);
  return () => {
    unsubscribe();
    host.removeEventListener("click", clicked, true);
  };
}
