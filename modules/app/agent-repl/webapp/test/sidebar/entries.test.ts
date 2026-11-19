// The one read of a repository section's body. See src/sidebar/entries.ts.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  RosterRepoSectionSchema,
  RosterRepositoryViewSchema,
  type RosterRepoSection,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import {
  openKin,
  openRowWorkspaces,
  repoEntries,
  repoSectionRegions,
  sectionOpenRows,
  sectionSubsections,
  unsectionedRows,
} from "../../src/sidebar/entries.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { openRow, repoSection, row } from "./harness.js";
import { withRepositoryBlocks } from "../roster-families.js";

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
    expect(sectionSubsections(example(), "s").map(({ subsection, path }) => [(subsection.kind.case === "named" ? subsection.kind.value.key?.subsectionId : subsection.kind.case), path])).toEqual([
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

describe("openKin", () => {
  /** P with own child o, then a repo-2 block of x and y and a repo-3 block of z. */
  const kin = () =>
    withRepositoryBlocks(openRow(row({ id: "p", children: [row({ id: "o" })] })), [
      { repo: "repo-2", rows: [row({ id: "x" }), row({ id: "y" })] },
      { repo: "repo-3", rows: [row({ id: "z" })] },
    ]);

  it("reads the own-repository children, then every block's rows in order", () => {
    expect(ids(openKin(kin(), "r"))).toEqual(["o", "x", "y", "z"]);
  });

  it("reads a drawn row's kin with neither children nor blocks as none", () => {
    expect(openKin({}, "r")).toEqual([]);
  });

  it("paths each block row under its block", () => {
    expect(openKin(kin(), "r").map((k) => k.path)).toEqual([
      "r.children[0]",
      "r.repositoryBlocks[0].rows[0]",
      "r.repositoryBlocks[0].rows[1]",
      "r.repositoryBlocks[1].rows[0]",
    ]);
  });

  it("reads a section's block rows in drawn order, each under its parent", () => {
    const section = repoSection({ id: "repo-1", rows: [row({ id: "p", children: [row({ id: "o" })] }), row({ id: "q" })] });
    const entry = section.entries[0]?.entry;
    if (entry?.case !== "row") throw new Error("test: a row entry");
    withRepositoryBlocks(entry.value, [{ repo: "repo-2", rows: [row({ id: "x" })] }]);
    expect(ids(sectionOpenRows(section, "s"))).toEqual(["p", "o", "x", "q"]);
  });
});

describe("openRowWorkspaces", () => {
  it("reads every section's open rows with their workspace ids, in drawn order", () => {
    // Arrange
    const u = create(RosterRepositoryViewSchema, { sections: [example(), repoSection({ id: "repo-2", rows: [row({ id: "z" })] })] });

    // Act
    const got = openRowWorkspaces(u, "v").map((open) => open.id);

    // Assert
    expect(got).toEqual(["a", "d", "e", "b", "k", "c", "z"]);
  });

  it("paths each row under its section", () => {
    // Arrange
    const u = create(RosterRepositoryViewSchema, { sections: [example()] });

    // Act
    const first = openRowWorkspaces(u, "v")[0];

    // Assert
    expect(first.path).toBe("v.sections[0].entries[0].row");
  });

  it("refuses a workspace with two open rows", () => {
    // Arrange
    const u = create(RosterRepositoryViewSchema, {
      sections: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "a" })] })],
    });

    // Act
    const read = (): unknown => openRowWorkspaces(u, "v");

    // Assert
    expect(read).toThrow(MalformedView);
    expect(read).toThrow(/workspace a has two open rows/);
  });

  it("refuses an open row naming no workspace", () => {
    // Arrange
    const u = create(RosterRepositoryViewSchema, {
      sections: [{ entries: [{ entry: { case: "row", value: { row: { workspace: {} } } } }] }],
    });

    // Act
    const read = (): unknown => openRowWorkspaces(u, "v");

    // Assert
    expect(read).toThrow(/an open row names no workspace/);
  });

  it("is the read every slot painter keys rows by", async () => {
    // Arrange
    const { readFileSync } = await import("node:fs");
    const source = (file: string): string => readFileSync(new URL(`../../src/sidebar/${file}`, import.meta.url), "utf8");

    // Act / Assert
    for (const file of ["tab-numbers.ts"]) {
      expect(source(file)).toContain("openRowWorkspaces(");
      expect(source(file)).not.toContain("openKin(");
    }
  });
});
