/**
 * entries — THE ONE READ of a repository section's body
 * (frontend.v1.RosterRepoSection.entries): its rows in no subsection and its
 * subsections, INTERLEAVED in drawn order, so a subsection may stand between
 * two rows. Every reader of a section (the drawer, the row patcher, the tab
 * numbers, the drag's refs) walks it here, so none can read the two kinds
 * in some other order.
 */
import type { RosterOpenRow, RosterOpenRows, RosterRepoSection, RosterSubsection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** One open row and its message path. */
export interface PathedOpenRow {
  row: RosterOpenRow;
  path: string;
}

/** One entry of a section's body, validated, with its message path. */
export type RepoEntry =
  | { case: "row"; row: RosterOpenRow; path: string }
  | { case: "subsection"; subsection: RosterSubsection; path: string };

/** U's entries in drawn order; an entry on no arm is malformed. */
export function repoEntries(u: RosterRepoSection, path: string): RepoEntry[] {
  return u.entries.map((entry, index) => {
    const entryPath = `${path}.entries[${index}]`;
    const arm = requireCase(entry.entry, `${entryPath}.entry`);
    switch (arm.case) {
      case "row":
        return { case: "row", row: arm.value, path: `${entryPath}.row` };
      case "subsection":
        return { case: "subsection", subsection: arm.value, path: `${entryPath}.subsection` };
      default: {
        const other: { case: string } = arm;
        return unreachableArm(`${entryPath}.entry`, other.case);
      }
    }
  });
}

/** U's top-level rows in no subsection, in drawn order. */
export function unsectionedRows(u: RosterRepoSection, path: string): PathedOpenRow[] {
  return repoEntries(u, path).flatMap((entry) => (entry.case === "row" ? [{ row: entry.row, path: entry.path }] : []));
}

/** U's subsections, in drawn order. */
export function sectionSubsections(u: RosterRepoSection, path: string): Array<{ subsection: RosterSubsection; path: string }> {
  return repoEntries(u, path).flatMap((entry) =>
    entry.case === "subsection" ? [{ subsection: entry.subsection, path: entry.path }] : [],
  );
}

/** ROWS' top-level rows, each with its path under PATH. */
export function pathedOpenRows(rows: RosterOpenRows, path: string): PathedOpenRow[] {
  return rows.rows.map((row, index) => ({ row, path: `${path}.rows[${index}]` }));
}

/**
 * A section's rows regions, in the order a region index names them: its
 * rows in no subsection first (region 0), then each subsection's (region 1 +
 * its index among the subsections).
 */
export function repoSectionRegions(u: RosterRepoSection, path: string): PathedOpenRow[][] {
  return [
    unsectionedRows(u, path),
    ...sectionSubsections(u, path).map(({ subsection, path: subPath }) =>
      pathedOpenRows(requireMessage(subsection.openRows, `${subPath}.open_rows`), `${subPath}.open_rows`),
    ),
  ];
}

/** Every open row U draws, depth-first in drawn order: entry by entry, each row before its family. */
export function sectionOpenRows(u: RosterRepoSection, path: string): PathedOpenRow[] {
  const flatten = (row: RosterOpenRow, rowPath: string): PathedOpenRow[] => [
    { row, path: rowPath },
    ...row.children.flatMap((child, index) => flatten(child, `${rowPath}.children[${index}]`)),
  ];
  return repoEntries(u, path).flatMap((entry) =>
    entry.case === "row"
      ? flatten(entry.row, entry.path)
      : pathedOpenRows(requireMessage(entry.subsection.openRows, `${entry.path}.open_rows`), `${entry.path}.open_rows`).flatMap(
          ({ row, path: rowPath }) => flatten(row, rowPath),
        ),
  );
}
