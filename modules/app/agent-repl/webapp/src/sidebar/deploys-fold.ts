/**
 * deploys-fold — WHETHER EACH REPOSITORY'S GROUP IN THE DEPLOYS SECTION IS
 * FOLDED (owner request, 2026-10-31; frontend.v1.RosterMergedRepoGroup).
 *
 * THE GROUP FOLDS BY ITSELF, FROM ITS ACTIVITY: a group whose queue is
 * active (the `queued` arm) is open, and an idle one is folded. A click on
 * the group's header folds or opens it, and that choice lasts only until the
 * automatic rule takes over again (owner ruling, 2026-10-31: "the automatic
 * open/close wins out"):
 *   - the group's queue changes: it starts, drains, or a merge joins or
 *     leaves its line (the group's `DEPLOYS_LINE_ATTRIBUTE`);
 *   - the focus leaves the rail's page (`focus-left.ts`): a click into a
 *     feed, into another Emacs window, or Emacs moving its own focus;
 *   - a workspace outside the Deploys groups is clicked: an open row or a
 *     hidden row;
 *   - a workspace is reopened from a group (`row.ts`'s band returns).
 *
 * THE CHOICE IS THIS PAGE'S OWN, never the daemon's view state: it is a
 * glance that lasts while the user is in the rail, so it is never shared
 * with another page or kept across a restart. It outlives every draw, so a
 * push that redraws the Deploys section draws each group as it stands.
 */
import { queryAll } from "../dom.js";
import type { FocusLeft } from "../focus-left.js";
import { log } from "../log.js";

/** The class of one repository's group in the Deploys section. */
export const DEPLOYS_GROUP_CLASS = "merged-group";

/** The class a folded group wears; the stylesheet hides its body under it. */
export const DEPLOYS_GROUP_FOLDED_CLASS = "folded";

/** The attribute a group names its repository on. */
export const DEPLOYS_REPOSITORY_ATTRIBUTE = "data-repository";

/**
 * The attribute a QUEUED group states its queue's line on: the workspaces in
 * it, in order. An idle group carries none. A choice made by hand lasts while
 * this stays as it was when the choice was made.
 */
export const DEPLOYS_LINE_ATTRIBUTE = "data-line";

/** How many merged rows an open group's band keeps room for (owner request, 2026-10-27). */
export const DEPLOYS_FLOOR = 5;

/** What a workspace outside the Deploys groups is drawn as, which a click on resets every choice. */
const OTHER_WORKSPACE_SELECTOR = "[data-roster-row]";

/** The page's one set of Deploys folds. */
export interface DeploysFold {
  /** Whether GROUP is folded now: the hand's choice while it stands, else its activity's. */
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

/** The repository GROUP is drawn for. A group always names one. */
function repositoryOf(group: HTMLElement): string {
  const repository = group.getAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE);
  if (repository === null || repository === "") {
    throw new Error(`deploys fold: a Deploys group carries no ${DEPLOYS_REPOSITORY_ATTRIBUTE}`);
  }
  return repository;
}

/** GROUP's fold by its activity alone: a queued group is open, an idle one folded. */
export function automaticFold(group: HTMLElement): boolean {
  return !group.hasAttribute(DEPLOYS_LINE_ATTRIBUTE);
}

/**
 * The folds of the groups drawn in ROOT. Each change repaints every group in
 * ROOT and tells CHANGED, so the bands are fitted again.
 */
export function createDeploysFold(root: ParentNode, changed: () => void): DeploysFold {
  const choices = new Map<string, Choice>();
  const folded = (group: HTMLElement): boolean => {
    const choice = choices.get(repositoryOf(group));
    if (choice === undefined || choice.line !== group.getAttribute(DEPLOYS_LINE_ATTRIBUTE)) return automaticFold(group);
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
      const repository = repositoryOf(group);
      const to = !folded(group);
      choices.set(repository, { folded: to, line: group.getAttribute(DEPLOYS_LINE_ATTRIBUTE) });
      log.debug(to ? "folded a Deploys group" : "opened a Deploys group", {
        operation: "sidebar.deploys-fold",
        context: { repository, folded: to, why: "its header was clicked" },
      });
      repaint();
    },
    reset: (why) => {
      if (choices.size === 0) return;
      log.debug("let every Deploys group fold by its activity again", {
        operation: "sidebar.deploys-fold",
        context: { repositories: [...choices.keys()], why },
      });
      choices.clear();
      repaint();
    },
    paint,
  };
}

/**
 * Drop every choice FOLDS holds whenever FOCUS leaves the page, and whenever
 * a workspace outside the Deploys groups is clicked in the rail HOST.
 * Answers the uninstall.
 */
export function installDeploysFoldReset(host: HTMLElement, folds: DeploysFold, focus: FocusLeft): () => void {
  const unsubscribe = focus.onLeft((why) => folds.reset(why));
  // CAPTURED, because a row's own click handler may stop the click.
  const clicked = (event: MouseEvent): void => {
    const target = event.target;
    if (!(target instanceof Element)) return;
    if (target.closest(OTHER_WORKSPACE_SELECTOR) === null || target.closest(`.${DEPLOYS_GROUP_CLASS}`) !== null) return;
    folds.reset("a workspace outside the Deploys groups was clicked");
  };
  host.addEventListener("click", clicked, true);
  return () => {
    unsubscribe();
    host.removeEventListener("click", clicked, true);
  };
}
