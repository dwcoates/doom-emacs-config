/**
 * merged-pages — THE RECENTLY MERGED BAND'S ROWS, LOADED AND LET GO FOR REAL
 * (docs/protobuf-design/sidebar-roster-streams.md, sections 2 and 7).
 *
 * A merged workspace is in no roster stream: the band's stream is a summary
 * (frontend.v1.RosterMergedSection), and its rows come ten at a time from
 * agentrepl.v1.ListMergedWorkspaces, most recently merged first, one
 * repository's group at a time. This module holds the pages this page has
 * loaded, and loads and lets go of them as the reader moves:
 *
 *   - the section folded (or Chat shown in its place): NOTHING is loaded, and
 *     every page held is let go;
 *   - unfolded: each group's page 0 is fetched;
 *   - a group's end (the loading spinner, `feed/walk-control.ts`) scrolled
 *     into view while its last answer was `more`: its next page is fetched;
 *   - a page past a group's first lying wholly below the band's viewport: let
 *     go, the last first;
 *   - a group folded on this page (`deploys-fold.ts`): its pages let go;
 *     opened, its page 0 fetched again.
 *
 * THE DRAW RULE (the contract's frontend.v1.RosterMergedSection
 * .merged_revision). Every page answer names the set of merged workspaces it
 * was resolved from, the token the summary carries, and the loaded pages are
 * drawn only beside a summary carrying the same token (`agree`). So a
 * workspace is never drawn both open and merged, nor in neither: the token
 * moves in the very push that moves the workspace between the open section
 * and the band. When a push moves the token while pages are loaded, the push
 * is HELD undrawn, every stream of it (`agree` answers `hold`), the loaded
 * pages are fetched again, and the push and the pages are drawn together once
 * they agree. A page whose token the summary does not carry though it was
 * asked for at the summary's token is NEWER than the summary, and waits for
 * the push that carries its set. A failed fetch is logged, every page is let
 * go and the held push is drawn without them: the section then shows its
 * count, the honest "partly loaded" state, never a stale page; no page is
 * fetched again on its own until the token moves or the reader folds or opens
 * something.
 *
 * ONLY EQUALITY IS READ OFF A TOKEN, as the contract says: a page is "newer"
 * because it was asked for at the summary's token and answered another, never
 * by comparing the two.
 */
import { ListMergedWorkspacesResponseSchema, type ListMergedWorkspacesResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_list_merged_workspaces_pb";
import type { RosterMergedRepoGroup, RosterMergedRow, RosterMergedSection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { RepositoryRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { queryAll } from "../dom.js";
import { errorCause } from "../error-cause.js";
import { drawWalkControl, setWalkLoading } from "../feed/walk-control.js";
import { createLogger } from "../log.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary, type UnaryContext } from "../rpc/unary.js";
import { RECENTLY_MERGED_GROUP_SELECTOR, deploysGroupRepository } from "./deploys-fold.js";
import { FOLDED_CLASS } from "./folded-class.js";
import type { MergedGroupPages, MergedPages, MergedVerdict } from "./context.js";
import { drawRosterRowWhenMerged } from "./row.js";

const log = createLogger("webapp.sidebar.merged-pages");

/** How many merged rows one page holds: the daemon's fixed page size. */
export const MERGED_PAGE_SIZE = 10;

/**
 * HOW LONG ONE PAGE FETCH MAY TAKE before it is a failure: the
 * ListMergedWorkspaces round trip. Measured 2026-11-16: over Connect against
 * the owner's registry shape (660 merged workspaces in 8 repositories, 500
 * others), one repository's page by merge order took at most 2.9ms over 200
 * calls (`BenchmarkListMergedWorkspaces`, niced and pinned to two cores); the
 * nearest read the live daemon serves under the owner's real load,
 * LocateWorkspace, took at most 156ms over 8 handler records. The bound is
 * about 2.6x that loaded worst case. A fetch that outlives it fails as any
 * transport failure does (`fetch-failed`, naming the bound).
 */
export const MERGED_PAGE_TIMEOUT_MS = 400;

/** The class of one loaded page past a group's first, inside the group's body. */
export const MERGED_PAGE_CLASS = "merged-page";

/** The class of the loading spinner at a group's end while more pages exist. */
export const MERGED_MORE_CLASS = "sb-deploys-more";

/** The attribute marking that spinner as this walk control's face. */
export const MERGED_MORE_ATTRIBUTE = "data-deploys-more";

/** The Recently Merged section's rows region, the viewport a page is in view of or not. */
const MERGED_REGION_SELECTOR = ".merged-section > .rows";

/** What the pages need. */
export interface MergedPagesDeps {
  /** The daemon connection the pages are fetched over. */
  ctx: UnaryContext;
  /** The rail, whose drawn groups and viewport `check` measures. */
  root: ParentNode;
  /** Whether REPOSITORY's group is folded on this page (`DeploysFold.mergedFolded`). */
  folded: (repository: string) => boolean;
  /** Told whenever the loaded pages changed, so the rail draws again. */
  changed: () => void;
  /** Monotonic milliseconds, for how long a push was held. */
  now?: () => number;
}

/** One loaded page, and the token it was answered at. */
interface Page {
  rows: readonly RosterMergedRow[];
  /** The set of merged workspaces it was resolved from. */
  token: bigint;
  /** The summary's token when it was asked for. */
  askedFor: bigint;
  /** Pages follow it. */
  more: boolean;
}

/** One fetch on its way: which ask it is, and at which summary token. */
interface InFlight {
  ticket: number;
  askedFor: bigint;
}

/** One repository's group's pages. */
interface Group {
  repository: RepositoryRef;
  pages: Page[];
  inFlight: Map<number, InFlight>;
}

/** Why an answer could not be loaded. */
class PageRefused extends Error {
  constructor(
    readonly kind: string,
    message: string,
  ) {
    super(message);
    this.name = "PageRefused";
  }
}

/** The empty pages of a group nothing is loaded for. */
const NOTHING_LOADED: MergedGroupPages = { pages: [], more: false, rows: 0 };

/** The pages of the Recently Merged band, loaded through DEPS. */
export function createMergedPages(deps: MergedPagesDeps): MergedPages {
  const now = deps.now ?? (() => performance.now());
  const groups = new Map<string, Group>();
  /** The section is unfolded and drawn: pages are loaded. */
  let open = false;
  /** The ids of the groups the summary last agreed lists, in order. */
  let summaryGroups: RepositoryRef[] = [];
  /** The summary's token, while open. */
  let token: bigint | null = null;
  /** The token a fetch failed at: nothing is fetched on its own until it moves. */
  let latched: bigint | null = null;
  let version = 0;
  let tickets = 0;
  let disposed = false;
  /** Since when a push is held, and whether a failure let the pages go meanwhile. */
  let hold: { since: number; released: boolean } | null = null;

  const groupFor = (repository: RepositoryRef): Group => {
    let group = groups.get(repository.id);
    if (group === undefined) {
      group = { repository, pages: [], inFlight: new Map() };
      groups.set(repository.id, group);
    }
    group.repository = repository;
    return group;
  };

  /** Let go of everything GROUP holds or awaits; answers whether anything was. */
  const release = (group: Group, why: string): boolean => {
    if (group.pages.length === 0 && group.inFlight.size === 0) return false;
    log.debug("let a Recently Merged group's pages go", {
      operation: "sidebar.merged-pages.release",
      context: { repository: group.repository.id, pages: group.pages.length, in_flight: group.inFlight.size, why },
    });
    group.pages = [];
    group.inFlight.clear();
    version += 1;
    return true;
  };

  const releaseAll = (why: string): boolean => {
    let any = false;
    for (const group of groups.values()) any = release(group, why) || any;
    return any;
  };

  const fetchPage = (group: Group, page: number, askedFor: bigint): void => {
    const ticket = ++tickets;
    group.inFlight.set(page, { ticket, askedFor });
    const repository = group.repository;
    log.debug("fetching a page of a Recently Merged group", {
      operation: "sidebar.merged-pages.fetch",
      context: { repository: repository.id, page, asked_for: String(askedFor) },
    });
    callUnary(
      deps.ctx,
      "ListMergedWorkspaces",
      (client) =>
        client.listMergedWorkspaces(
          {
            repository,
            order: { order: { case: "mostRecentlyMerged", value: {} } },
            page,
          },
          { timeoutMs: MERGED_PAGE_TIMEOUT_MS },
        ),
      ListMergedWorkspacesResponseSchema,
    ).then(
      (response) => landed(group, page, ticket, response),
      (err: unknown) => failed(group, page, ticket, "transport", errorCause(err)),
    );
  };

  /** Whether the answer to TICKET for GROUP's PAGE is still wanted. */
  const wanted = (group: Group, page: number, ticket: number): boolean => {
    const wantedNow = !disposed && groups.get(group.repository.id) === group && group.inFlight.get(page)?.ticket === ticket;
    if (!wantedNow) {
      log.debug("dropped a Recently Merged page answer nothing waits for any more", {
        operation: "sidebar.merged-pages.answer-dropped",
        context: { repository: group.repository.id, page, disposed },
      });
    }
    return wantedNow;
  };

  const landed = (group: Group, page: number, ticket: number, response: ListMergedWorkspacesResponse): void => {
    if (!wanted(group, page, ticket)) return;
    let answered: Omit<Page, "askedFor">;
    try {
      answered = readPage(response, page);
    } catch (err) {
      failed(group, page, ticket, err instanceof PageRefused ? err.kind : "malformed", errorCause(err));
      return;
    }
    const askedFor = requireInFlight(group, page).askedFor;
    group.inFlight.delete(page);
    if (page > group.pages.length) {
      // The pages before it were let go while it was asked for.
      log.debug("dropped a Recently Merged page whose earlier pages were let go", {
        operation: "sidebar.merged-pages.answer-dropped",
        context: { repository: group.repository.id, page, loaded: group.pages.length },
      });
      return;
    }
    group.pages[page] = { ...answered, askedFor };
    version += 1;
    log.debug("loaded a page of a Recently Merged group", {
      operation: "sidebar.merged-pages.loaded",
      context: {
        repository: group.repository.id,
        page,
        rows: answered.rows.length,
        more: answered.more,
        token: String(answered.token),
        asked_for: String(askedFor),
      },
    });
    deps.changed();
  };

  const failed = (group: Group, page: number, ticket: number, kind: string, cause: string): void => {
    if (!wanted(group, page, ticket)) return;
    log.error("a Recently Merged page could not be loaded; every page is let go and the band shows its count: {cause}", {
      operation: "sidebar.merged-pages.fetch-failed",
      context: {
        repository: group.repository.id,
        page,
        kind,
        asked_for: String(requireInFlight(group, page).askedFor),
        summary_token: token === null ? null : String(token),
        held: hold !== null,
        bound_ms: MERGED_PAGE_TIMEOUT_MS,
      },
    }, { cause });
    latched = token;
    if (hold !== null) hold.released = true;
    releaseAll("a page could not be loaded");
    deps.changed();
  };

  /** Load what the open section needs and let go of what it does not; answers whether anything was let go. */
  const fit = (): boolean => {
    if (!open || token === null) return false;
    let releasedAny = false;
    for (const repository of summaryGroups) {
      const group = groupFor(repository);
      if (deps.folded(repository.id)) {
        releasedAny = release(group, "its group was folded") || releasedAny;
        continue;
      }
      if (group.pages.length === 0 && group.inFlight.size === 0 && latched === null) fetchPage(group, 0, token);
    }
    return releasedAny;
  };

  const startHold = (mismatched: Array<{ group: Group; page: number }>): void => {
    if (hold !== null) return;
    hold = { since: now(), released: false };
    log.info("the rail holds a roster push until its merged pages agree with its Recently Merged summary", {
      operation: "sidebar.merged-pages.hold",
      context: {
        summary_token: token === null ? null : String(token),
        pages: mismatched.map(({ group, page }) => ({
          repository: group.repository.id,
          page,
          token: String(group.pages[page].token),
        })),
      },
    });
  };

  const endHold = (outcome: "agreed" | "folded"): void => {
    if (hold === null) return;
    log.info("the rail drew the roster push it held for its merged pages", {
      operation: "sidebar.merged-pages.hold-released",
      context: { held_ms: Math.round(now() - hold.since), outcome: hold.released ? "released" : outcome },
    });
    hold = null;
  };

  const check = (): void => {
    if (disposed || !open || token === null) return;
    let releasedAny = fit();
    for (const drawn of queryAll<HTMLElement>(deps.root, RECENTLY_MERGED_GROUP_SELECTOR)) {
      if (drawn.classList.contains(FOLDED_CLASS)) continue;
      const group = groups.get(repositoryOf(drawn));
      if (group === undefined) continue;
      const region = regionOf(drawn, group);
      const spinner = drawn.querySelector<HTMLElement>(`.${MERGED_MORE_CLASS}`);
      if (spinner !== null && inView(spinner, region)) {
        const next = group.pages.length;
        if (group.pages.at(-1)?.more === true && !group.inFlight.has(next) && latched === null) {
          log.debug("a Recently Merged group's end was scrolled into view; fetching its next page", {
            operation: "sidebar.merged-pages.more",
            context: { repository: group.repository.id, page: next },
          });
          fetchPage(group, next, token);
        }
        continue;
      }
      // A PAGE WHOLLY BELOW THE VIEWPORT GOES, THE LAST FIRST; a group's
      // first page stays while the group is open.
      const drawnPages = queryAll<HTMLElement>(drawn, `.${MERGED_PAGE_CLASS}`);
      while (group.pages.length > 1) {
        const last = drawnPages[group.pages.length - 2];
        if (last === undefined || !belowView(last, region)) break;
        group.pages.pop();
        for (const page of [...group.inFlight.keys()]) if (page >= group.pages.length) group.inFlight.delete(page);
        version += 1;
        releasedAny = true;
        log.debug("let a Recently Merged group's last page go: it lies below the band's viewport", {
          operation: "sidebar.merged-pages.release",
          context: { repository: group.repository.id, pages: group.pages.length, why: "it was scrolled back above" },
        });
      }
    }
    if (releasedAny) deps.changed();
  };

  return {
    agree(section, path) {
      if (section === null || section.fold.case !== "expanded") {
        // A FOLDED BAND LOADS NOTHING, and lets go of all it held at the fold.
        if (section !== null) requireCase(section.fold, `${path}.fold`);
        const why = section === null ? "the band is not drawn" : "the band was folded";
        releaseAll(why);
        open = false;
        token = null;
        latched = null;
        summaryGroups = [];
        endHold("folded");
        return "draw";
      }
      const next = requireMessage(section.mergedRevision, `${path}.merged_revision`).revision;
      if (latched !== null && latched !== next) latched = null;
      open = true;
      token = next;
      summaryGroups = section.groups.map((group, index) =>
        requireMessage(requireMessage(group.key, `${path}.groups[${index}].key`).repository, `${path}.groups[${index}].key.repository`),
      );
      const listed = new Set(summaryGroups.map((repository) => repository.id));
      for (const [id, group] of groups) {
        if (listed.has(id)) continue;
        release(group, "its group left the summary");
        groups.delete(id);
      }
      fit();
      const mismatched: Array<{ group: Group; page: number }> = [];
      for (const group of groups.values()) {
        group.pages.forEach((page, index) => {
          if (page.token !== next) mismatched.push({ group, page: index });
        });
      }
      if (mismatched.length === 0) {
        endHold("agreed");
        return "draw";
      }
      // A PAGE ASKED FOR AT AN OLDER TOKEN IS FETCHED AGAIN; one asked for at
      // this token and answered another is newer, and waits for its push.
      for (const { group, page } of mismatched) {
        if (group.pages[page].askedFor !== next && !group.inFlight.has(page)) fetchPage(group, page, next);
      }
      startHold(mismatched);
      return "hold";
    },
    loaded(group, path) {
      const repository = requireMessage(requireMessage(group.key, `${path}.key`).repository, `${path}.key.repository`);
      const held = groups.get(repository.id);
      if (held === undefined) return NOTHING_LOADED;
      const last = held.pages.at(-1);
      return {
        pages: held.pages.map((page) => page.rows),
        more: last === undefined ? held.inFlight.has(0) : last.more,
        rows: held.pages.reduce((sum, page) => sum + page.rows.length, 0),
      };
    },
    version: () => version,
    check,
    foldsChanged() {
      latched = null;
      if (fit()) deps.changed();
      check();
    },
    dispose() {
      disposed = true;
      groups.clear();
      hold = null;
    },
  };
}

/** The repository a drawn GROUP is of: the one read every page-own group state keys a group by (`deploys-fold.ts`). */
const repositoryOf = (group: HTMLElement): string => deploysGroupRepository(group, "sidebar.merged-pages.malformed");

/** The fetch on its way for GROUP's PAGE, which an answer still wanted always has. */
function requireInFlight(group: Group, page: number): InFlight {
  const inFlight = group.inFlight.get(page);
  if (inFlight === undefined) throw new Error(`invariant: page ${page} of ${group.repository.id} is awaited`);
  return inFlight;
}

/** RESPONSE's page PAGE: its rows, token and extent. A refusal or an unreadable answer throws. */
function readPage(response: ListMergedWorkspacesResponse, page: number): Omit<Page, "askedFor"> {
  const path = "ListMergedWorkspacesResponse.result";
  const result = requireCase(response.result, path);
  switch (result.case) {
    case "error": {
      const cause = requireCase(result.value.cause, `${path}.error.cause`);
      throw new PageRefused(cause.case, `the daemon refused page ${page}: ${cause.case}`);
    }
    case "success": {
      const success = result.value;
      const token = requireMessage(success.revision, `${path}.success.revision`).revision;
      const extent = requireCase(success.extent, `${path}.success.extent`);
      let more: boolean;
      switch (extent.case) {
        case "last":
          more = false;
          break;
        case "more":
          more = true;
          break;
        default: {
          const other: { case: string } = extent;
          return unreachableArm(`${path}.success.extent`, other.case);
        }
      }
      const rowsPath = `${path}.success.extent.${extent.case}.rows`;
      const rows = requireMessage(extent.value.rows, rowsPath).rows;
      rows.forEach((row, i) => mergedRowAtMs(row, `${rowsPath}.rows[${i}]`));
      return { rows, token, more };
    }
    default: {
      const other: { case: string } = result;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * When the merged row U at PATH landed, epoch ms: the instant its when-column
 * draws, which it is stamped with (`data-merged-at`, band-fit.ts). A merged
 * row is always on the `when.merged` arm; one that is not is malformed.
 */
export function mergedRowAtMs(u: RosterMergedRow, path: string): number {
  const row = requireMessage(u.row, `${path}.row`);
  const when = requireMessage(row.when, `${path}.row.when`);
  const arm = requireCase(when.shown, `${path}.row.when.shown`);
  if (arm.case !== "merged") return unreachableArm(`${path}.row.when.shown`, arm.case);
  return drawRosterRowWhenMerged(arm.value, `${path}.row.when.merged`);
}

/** The Recently Merged viewport GROUP's ELEMENT is drawn in. A group always hangs in one. */
function regionOf(element: HTMLElement, group: Group): HTMLElement {
  const region = element.closest<HTMLElement>(MERGED_REGION_SELECTOR);
  if (region === null) {
    log.error("a Recently Merged group's pages cannot be looked at: it hangs in no Recently Merged viewport", {
      operation: "sidebar.merged-pages.malformed",
      context: { repository: group.repository.id },
    });
    throw new Error("merged pages: a Recently Merged group hangs in no Recently Merged viewport");
  }
  return region;
}

/** Whether EL shows a sliver inside REGION's viewport. */
export function inView(el: HTMLElement, region: HTMLElement): boolean {
  const view = region.getBoundingClientRect();
  const box = el.getBoundingClientRect();
  return box.height > 0 && box.bottom > view.top && box.top < view.bottom;
}

/** Whether EL lies wholly below REGION's viewport. */
function belowView(el: HTMLElement, region: HTMLElement): boolean {
  const box = el.getBoundingClientRect();
  return box.height > 0 && box.top >= region.getBoundingClientRect().bottom;
}

/** The feed's loading spinner at a group's end, standing while more of its pages exist. */
export function drawMergedMoreSpinner(): HTMLElement {
  const spinner = drawWalkControl(MERGED_MORE_ATTRIBUTE);
  spinner.classList.add(MERGED_MORE_CLASS);
  setWalkLoading(spinner, true);
  return spinner;
}
