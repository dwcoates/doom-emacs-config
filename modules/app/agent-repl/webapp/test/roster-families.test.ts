import { create } from "@bufbuild/protobuf";
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { RosterOpenRowsSchema, RosterRepoSectionSchema, RosterRowSchema, type RosterRow } from "../../proto/gen/ts/frontend/v1/sidebar_pb";
import { hiddenRow, numberTabs, openRow, rowEntries, sectionTabRegions, withFamily } from "./roster-families.js";

function row(id: string, children: RosterRow[] = []): RosterRow {
  return withFamily(create(RosterRowSchema, { workspace: { workspace: { id, dir: `/w/${id}` } } }), children);
}

function tabOf(rows: ReturnType<typeof openRow>[], at: number[]) {
  let open = rows[at[0]];
  for (const i of at.slice(1)) open = open.children[i]!;
  return open.tab?.tab;
}

describe("openRow", () => {
  it("nests the family recorded by withFamily, each child an open row", () => {
    // Arrange
    const parent = row("p", [row("c1"), row("c2", [row("g")])]);
    // Act
    const open = openRow(parent);
    // Assert
    expect(open.children.map((child) => child.row?.workspace?.workspace?.id)).toEqual(["c1", "c2"]);
    expect(open.children[1]?.children[0]?.row?.workspace?.workspace?.id).toBe("g");
  });

  it("folds every tab until numberTabs numbers it", () => {
    expect(openRow(row("p", [row("c")])).children[0]?.tab?.tab.case).toBe("folded");
  });

  it("gives a row with no recorded family no children", () => {
    expect(openRow(create(RosterRowSchema, {})).children).toEqual([]);
  });
});

describe("hiddenRow", () => {
  it("nests the family recorded by withFamily, each child a hidden row", () => {
    expect(hiddenRow(row("p", [row("c")])).children[0]?.row?.workspace?.workspace?.id).toBe("c");
  });
});

describe("numberTabs", () => {
  it("numbers unfolded repositories' rows 1..N depth first across regions and repositories", () => {
    // Arrange
    const first = [openRow(row("a", [row("a1")])), openRow(row("b"))];
    const sub = [openRow(row("c"))];
    const second = [openRow(row("d"))];
    // Act
    numberTabs([
      { regions: [create(RosterOpenRowsSchema, { rows: first }), create(RosterOpenRowsSchema, { rows: sub })], folded: false },
      { regions: [create(RosterOpenRowsSchema, { rows: second })], folded: false },
    ]);
    // Assert
    expect([tabOf(first, [0]), tabOf(first, [0, 0]), tabOf(first, [1]), tabOf(sub, [0]), tabOf(second, [0])]).toEqual(
      [1, 2, 3, 4, 5].map((number): unknown => expect.objectContaining({ case: "numbered", value: expect.objectContaining({ number }) as unknown })),
    );
  });

  it("folds a folded repository's rows and skips them in the numbering", () => {
    // Arrange
    const folded = [openRow(row("a"))];
    const shown = [openRow(row("b"))];
    // Act
    numberTabs([
      { regions: [create(RosterOpenRowsSchema, { rows: folded })], folded: true },
      { regions: [create(RosterOpenRowsSchema, { rows: shown })], folded: false },
    ]);
    // Assert
    expect(tabOf(folded, [0])?.case).toBe("folded");
    expect(tabOf(shown, [0])).toEqual(expect.objectContaining({ case: "numbered", value: expect.objectContaining({ number: 1 }) as unknown }));
  });

  it("numbers only the listed rows, by their place in the list, folding the rest", () => {
    // Arrange
    const rows = [openRow(row("a")), openRow(row("b")), openRow(row("c"))];
    // Act
    numberTabs([{ regions: [create(RosterOpenRowsSchema, { rows })], folded: false }], ["c", "a"]);
    // Assert
    expect(rows.map((open) => (open.tab?.tab.case === "numbered" ? open.tab.tab.value.number : "folded"))).toEqual([2, "folded", 1]);
  });

  it("skips an absent region", () => {
    expect(() => numberTabs([{ regions: [undefined], folded: false }])).not.toThrow();
  });
});

describe("rowEntries", () => {
  it("wraps each row as an open row entry, in order, its family nested", () => {
    // Act
    const entries = rowEntries([row("a", [row("a1")]), row("b")]);

    // Assert
    expect(
      entries.map((entry) =>
        entry.entry.case === "row" ? [entry.entry.value.row?.workspace?.workspace?.id, entry.entry.value.children.length] : null,
      ),
    ).toEqual([
      ["a", 1],
      ["b", 0],
    ]);
  });
});

describe("sectionTabRegions", () => {
  it("numbers a subsection's rows where it stands between two rows", () => {
    // Arrange
    const [a, b] = rowEntries([row("a"), row("b")]);
    const sub = [openRow(row("x"))];
    const section = create(RosterRepoSectionSchema, {
      entries: [a, { entry: { case: "subsection", value: { openRows: { rows: sub } } } }, b],
      fold: { case: "expanded", value: {} },
    });

    // Act
    numberTabs([sectionTabRegions(section)]);

    // Assert
    const numberOf = (entry: (typeof section.entries)[number]): unknown =>
      entry.entry.case === "row" && entry.entry.value.tab?.tab.case === "numbered" ? entry.entry.value.tab.tab.value.number : null;
    const x = section.entries[1].entry.case === "subsection" ? section.entries[1].entry.value.openRows?.rows[0]?.tab?.tab : undefined;
    expect([numberOf(section.entries[0]), x?.case === "numbered" ? x.value.number : null, numberOf(section.entries[2])]).toEqual([1, 2, 3]);
  });

  it("says a collapsed section's regions are folded", () => {
    // Arrange
    const section = create(RosterRepoSectionSchema, { entries: rowEntries([row("a")]), fold: { case: "collapsed", value: {} } });

    // Act / Assert
    expect(sectionTabRegions(section).folded).toBe(true);
  });
});

describe("the test rosters that build families", () => {
  it.each(["sidebar/harness.ts", "integration/fixtures.ts"])("%s takes them from roster-families", (file) => {
    // A hand-rolled family or numbering would let the two kinds of test roster disagree.
    const source = readFileSync(new URL(file, import.meta.url), "utf8");
    expect(source).toMatch(/from "\.\.\/roster-families\.js"/);
    expect(source).not.toMatch(/new WeakMap<RosterRow/);
    expect(source).not.toMatch(/RosterOpenRowTabSchema/);
  });

  it.each(["sidebar/harness.ts", "integration/fixtures.ts"])("%s numbers a section's tabs through sectionTabRegions", (file) => {
    // A hand-rolled walk of a section's entries could number them out of drawn order.
    const source = readFileSync(new URL(file, import.meta.url), "utf8");
    expect(source).toMatch(/\.map\(sectionTabRegions\)/);
  });
});
