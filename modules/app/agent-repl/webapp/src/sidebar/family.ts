/**
 * family — A ROW OF ONE ROSTER LIST AND ITS FAMILY, as data: the shape every
 * list's row and its nested kin are read through, and the walk that gives
 * each family of a rows region its message path.
 *
 * IT IS ITS OWN MODULE, a leaf below every sidebar module that reads a list
 * (the drawer in row.ts, the section read in entries.ts, the counts), so a
 * reader of the rows never imports the drawer that draws them, which would be
 * an import cycle (test/import-cycles.test.ts).
 */
import type {
  RosterOpenRowChildrenFold,
  RosterOpenRowFoldCover,
  RosterOpenRowRepositoryBlock,
  RosterOpenRowTab,
  RosterRow,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

/**
 * ONE ROW OF A LIST AND ITS FAMILY, as each list of the roster spells it: an
 * open row (frontend.v1.RosterOpenRow), a hidden row (RosterBacklogRow) or a
 * merged row (RosterMergedRow, which has no family). The list a row sits in is
 * its state, and every member of its family is in the same list, so a row
 * draws its family exactly as the wire nests it: the daemon has already put an
 * open workspace whose parent is not open in its parent's place, and no list
 * carries a closed row for a client to drop.
 */
export interface RowFamily {
  /** The row as every list draws it. */
  row?: RosterRow;
  /**
   * The row's family in the same list, in drawn order: for an open row, its
   * children of its own repository.
   */
  children?: readonly RowFamily[];
  /**
   * An open row's children of other repositories, one block per repository,
   * drawn after `children` (frontend.v1.RosterOpenRow.repository_blocks).
   */
  repositoryBlocks?: readonly RosterOpenRowRepositoryBlock[];
  /**
   * Whether the row's children are folded, set on an open row with any child,
   * own-repository or in a block (frontend.v1.RosterOpenRow.children_fold).
   */
  childrenFold?: RosterOpenRowChildrenFold;
  /**
   * An open row's standing under the folds: uncovered, hidden by a fold, or
   * drawn although a fold covers it (frontend.v1.RosterOpenRow.fold_cover).
   * Always set on an open row; the other lists carry none.
   */
  foldCover?: RosterOpenRowFoldCover;
  /** An open row's tab: its number, or folded (frontend.v1.RosterOpenRow.tab). */
  tab?: RosterOpenRowTab;
}

/** A row's kin: its own children and its repository blocks, as `RowFamily` carries them. */
export type RowKin = Pick<RowFamily, "children" | "repositoryBlocks">;

/** One family of a rows region, with its message path. */
export interface PathedFamily {
  family: RowFamily;
  path: string;
}

/** The families of a rows region ROWS at PATH, in drawn order, each with its message path. */
export function pathedFamilies(rows: readonly RowFamily[], path: string): PathedFamily[] {
  return rows.map((family, index) => ({ family, path: `${path}[${index}]` }));
}
