// The one read of a repository section's body. See src/sidebar/entries.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterRepoSectionSchema, type RosterRepoSection } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  repoEntries,
  repoSectionRegions,
  sectionOpenRows,
  sectionSubsections,
  unsectionedRows,
} from "../../src/sidebar/entries.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { repoSection, row } from "./harness.js";

/** The owner's example: a, [foo: d e], b, c, with b nesting its child k. */
function example(): RosterRepoSection {
  return repoSection({
    id: "repo-1",
    rows: [row({ id: "a" }), row({ id: "b", children: [row({ id: "k" })] }), row({ id: "c" })],
    subsections: [{ id: "foo", rows: [row({ id: "d" }), row({ id: "e" })], before: "b" }],
  });
}

/** The workspace ids of pathed open rows, in order. */
const ids = (rows: Array<{ row: { row?: { workspace?: { workspace?: { id: string } } } } }>): string[] =>
  rows.map((open) => open.row.row?.workspace?.workspace?.id ?? "?");

describe("repoEntries", () => {
  it("reads every entry in drawn order with its path", () => {
    // Act
    const got = repoEntries(example(), "s").map((entry) => [entry.case, entry.path]);

    // Assert
    expect(got).toEqual([
      ["row", "s.entries[0].row"],
      ["subsection", "s.entries[1].subsection"],
      ["row", "s.entries[2].row"],
      ["row", "s.entries[3].row"],
    ]);
  });

  it("refuses an entry on no arm as a malformed view", () => {
    // Arrange
    const section = create(RosterRepoSectionSchema, { entries: [{}] });

    // Act
    const read = (): unknown => repoEntries(section, "s");

    // Assert
    expect(read).toThrow(MalformedView);
    expect(read).toThrow(/s\.entries\[0\]\.entry/);
  });
});

describe("the section's lists", () => {
  it("reads the rows in no subsection, skipping where a subsection stands", () => {
    expect(ids(unsectionedRows(example(), "s"))).toEqual(["a", "b", "c"]);
  });

  it("reads the subsections in drawn order", () => {
    expect(sectionSubsections(example(), "s").map(({ subsection, path }) => [subsection.key?.subsectionId, path])).toEqual([
      ["foo", "s.entries[1].subsection"],
    ]);
  });

  it("reads the regions, the rows in no subsection first, then each subsection's", () => {
    expect(repoSectionRegions(example(), "s").map(ids)).toEqual([
      ["a", "b", "c"],
      ["d", "e"],
    ]);
  });

  it("reads every open row in drawn order, a subsection's where it stands and each family under its row", () => {
    expect(ids(sectionOpenRows(example(), "s"))).toEqual(["a", "d", "e", "b", "k", "c"]);
  });
});
