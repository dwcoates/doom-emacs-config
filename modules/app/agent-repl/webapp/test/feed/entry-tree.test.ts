// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ProseEntryTreeSchema } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import {
  ENTRY_ATTRIBUTE,
  ENTRY_CLASS,
  ENTRY_LABEL_CLASS,
  ENTRY_PATH_CLASS,
  entryTreeHtml,
} from "../../src/feed/entry-tree.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { inline } from "../../src/markdown.js";
import { TREE_NODE_ATTRIBUTE, columnMeasure } from "../../src/metaprompt-tree.js";
import { drawnEntries, linesOf } from "./tree-fixture.js";

describe("entryTreeHtml", () => {
  it("draws each entry's line with its label as the click target", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    const entries = [...el.querySelectorAll(`.${ENTRY_CLASS}`)].map((l) => [
      l.getAttribute(ENTRY_ATTRIBUTE),
      l.querySelector(`.${ENTRY_LABEL_CLASS}`)?.textContent,
    ]);
    expect(entries).toEqual([
      ["3.1", "3.1"],
      ["4.2", "4.2"],
      ["5.5.1", "5.5.1"],
    ]);
  });

  it("draws an entry's words as inline markdown", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    expect(linesOf(el, "4.2")[0]?.querySelector("strong")?.textContent).toBe("b");
  });

  it("draws every ancestor a deeper label implies by its label alone, dimmed", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    const paths = [...el.querySelectorAll(`.${ENTRY_PATH_CLASS}`)].map((l) => [
      l.getAttribute(TREE_NODE_ATTRIBUTE),
      l.querySelector(".mp-content")?.textContent,
    ]);
    expect(paths).toEqual([
      ["3", ""],
      ["4", ""],
      ["5", ""],
      ["5.5", ""],
    ]);
  });

  it("draws the rails the labels make, with no blank line between roots", () => {
    // Act
    const el = drawnEntries("3.1 - a\n4.2 - **b**\n5.5.1 - c");
    // Assert
    const prefixes = [...el.querySelectorAll(".mp-line")].map((l) => l.querySelector(".mp-prefix")?.textContent);
    expect(prefixes).toEqual(["3 ", "└── 3.1 ", "4 ", "└── 4.2 ", "5 ", "└── 5.5 ", "    └── 5.5.1 "]);
  });

  it("draws an entry that says nothing as its label alone", () => {
    // Act
    const el = drawnEntries("3.1 -");
    // Assert
    expect(linesOf(el, "3.1")[0]?.querySelector(".mp-content")?.textContent).toBe("");
  });

  it("escapes what an entry says rather than reading it as markup", () => {
    // Act
    const el = drawnEntries("3.1 - <b>x</b>");
    // Assert
    expect([linesOf(el, "3.1")[0]?.querySelector("b"), linesOf(el, "3.1")[0]?.textContent]).toEqual([null, "└── 3.1 <b>x</b>"]);
  });

  it.each([
    ["no label", { nodes: [{ id: { index: 0 }, place: { case: "root", value: {} }, kind: { case: "path", value: {} } }] }],
    ["no kind", { nodes: [{ id: { index: 0 }, place: { case: "root", value: {} }, label: { text: "3" } }] }],
    ["no place", { nodes: [{ id: { index: 0 }, label: { text: "3" }, kind: { case: "path", value: {} } }] }],
  ])("refuses a node served with %s", (_name, init) => {
    // Act + Assert
    expect(() => entryTreeHtml(create(ProseEntryTreeSchema, init as never), inline, columnMeasure(80))).toThrow(MalformedView);
  });
});
