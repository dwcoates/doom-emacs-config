/**
 * roster-families — how a test roster carries its rows' families and tabs, in
 * the one shape the daemon sends (frontend.v1.WorkspaceRoster): the shared
 * `frontend.v1.RosterRow` holds no family, so each list wraps it in its own row
 * type with the children nested there, and each open row states its own tab.
 *
 * THE ONE PLACE TEST ROSTERS GET THEIR FAMILIES AND TABS, for the sidebar unit
 * harness (`test/sidebar/harness.ts`) and the integration fixtures
 * (`test/integration/fixtures.ts`) alike, so the two cannot build a family or
 * number a tab two ways.
 */
import { create } from "@bufbuild/protobuf";
import {
  RosterHiddenRowSchema,
  RosterOpenRowSchema,
  RosterOpenRowTabSchema,
  RosterRepoEntrySchema,
  type RosterHiddenRow,
  type RosterOpenRow,
  type RosterRepoEntry,
  type RosterRepoSection,
  type RosterRow,
} from "../../proto/gen/ts/frontend/v1/sidebar_pb";

/** Each row's children, as its builder was told them. */
const familyOf = new WeakMap<RosterRow, readonly RosterRow[]>();

/** Record CHILDREN as ROW's family, for `openRow` and `hiddenRow` to nest. Answers ROW. */
export function withFamily(row: RosterRow, children: readonly RosterRow[]): RosterRow {
  familyOf.set(row, children);
  return row;
}

/**
 * ROW as an open row, its family nested as open rows. Its tab is FOLDED until
 * `numberTabs` numbers it, as the daemon does, in the Open section's walk.
 */
export function openRow(row: RosterRow): RosterOpenRow {
  return create(RosterOpenRowSchema, {
    row,
    tab: { tab: { case: "folded", value: {} } },
    children: (familyOf.get(row) ?? []).map(openRow),
  });
}

/** ROW as a hidden row, its family nested as hidden rows. */
export function hiddenRow(row: RosterRow): RosterHiddenRow {
  return create(RosterHiddenRowSchema, { row, children: (familyOf.get(row) ?? []).map(hiddenRow) });
}

/** ROWS as a repository section's row entries, one each, in order (frontend.v1.RosterRepoEntry). */
export function rowEntries(rows: readonly RosterRow[]): RosterRepoEntry[] {
  return rows.map((row) => create(RosterRepoEntrySchema, { entry: { case: "row", value: openRow(row) } }));
}

/** One repository's open-row regions, in drawn order, and whether its fold withholds them. */
export interface TabRegions {
  regions: ReadonlyArray<{ readonly rows: readonly RosterOpenRow[] } | undefined>;
  folded: boolean;
}

/**
 * SECTION's tab regions in drawn order, entry by entry: a row in no
 * subsection alone, a subsection's rows where it stands.
 */
export function sectionTabRegions(section: RosterRepoSection): TabRegions {
  return {
    regions: section.entries.map((entry) =>
      entry.entry.case === "row" ? { rows: [entry.entry.value] } : entry.entry.case === "subsection" ? entry.entry.value.openRows : undefined,
    ),
    folded: section.fold.case === "collapsed",
  };
}

/**
 * Number the tabs of REPOS' open rows in the Open section's walk: repository
 * by repository, region by region, each row before its family. LISTED, when
 * given, names the drawn tabs first to last, each numbered by its place there
 * and every other row folded; otherwise every row of an unfolded repository is
 * numbered 1, 2, ... and a folded one's are folded, as the daemon numbers them.
 */
export function numberTabs(repos: readonly TabRegions[], listed?: readonly string[]): void {
  let next = 0;
  const visit = (open: RosterOpenRow, folded: boolean): void => {
    const id = open.row?.workspace?.workspace?.id ?? "";
    const at = listed === undefined ? (folded ? -1 : next++) : listed.indexOf(id);
    open.tab = create(
      RosterOpenRowTabSchema,
      at < 0 ? { tab: { case: "folded", value: {} } } : { tab: { case: "numbered", value: { number: at + 1 } } },
    );
    open.children.forEach((child) => visit(child, folded));
  };
  for (const repo of repos) {
    for (const region of repo.regions) region?.rows.forEach((open) => visit(open, repo.folded));
  }
}
