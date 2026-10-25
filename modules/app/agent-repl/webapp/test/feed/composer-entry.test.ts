// @vitest-environment jsdom
import { beforeEach, describe, expect, it } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { FeedComposerEntrySchema, type FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { ProseTreeNodeSchema, ProseTreeSchema, type ProseTree } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import {
  COMPOSER_ENTRY_CLASS,
  COMPOSER_LEFT_PROPERTY,
  COMPOSER_RIGHT_PROPERTY,
  createComposerEntryHighlight,
  markComposerEntry,
} from "../../src/feed/composer-entry.js";
import { renderTreeHtml } from "../../src/metaprompt-tree.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** The push naming LABEL's node in ROW. */
const nodeEntry = (row: string, label: string): FeedComposerEntry =>
  create(FeedComposerEntrySchema, { at: { case: "node", value: { row: { value: row }, label } } });

/** The push naming no node. */
const noEntry = (): FeedComposerEntry => create(FeedComposerEntrySchema, { at: { case: "none", value: {} } });

/** A node's place: a root with no PARENT, else a branch under it. */
function placeOf(parent: number | undefined): MessageInitShape<typeof ProseTreeNodeSchema>["place"] {
  return parent === undefined ? { case: "root", value: {} } : { case: "branch", value: { parent: { index: parent } } };
}

/** A tree of NODES, each (label, text, parent index or none). */
function tree(nodes: [string, string, number?][]): ProseTree {
  return create(ProseTreeSchema, {
    nodes: nodes.map(([label, text, parent], index) => ({
      id: { index },
      place: placeOf(parent),
      label: { text: label },
      text: { markdown: text },
    })),
  });
}

/**
 * The owner's example: 1, its child 1.1 and grandchild 1.1.1, then 1.2 and
 * 1.10, whose label merely opens on 1.1's characters.
 */
const example = tree([
  ["1", "The subtree highlight landed"],
  ["1.1", "It hasn't reached the running build yet", 0],
  ["1.1.1", "It goes live with the next rollout", 1],
  ["1.2", "A sibling", 0],
  ["1.10", "A lookalike", 0],
]);

/** A response row ROW drawing TREES, each in its own `.mp-tree`, in the document. */
function drawnRow(row: string, ...trees: ProseTree[]): HTMLElement {
  const el = document.createElement("article");
  el.setAttribute("data-feed-row", row);
  el.innerHTML = trees.map((t) => `<div class="mp-tree">${renderTreeHtml(t, (s) => s, 120)}</div>`).join("");
  document.body.replaceChildren(el);
  return el;
}

/** Each line of EL inside the box, as (its node, its left edge, its right edge). */
const boxed = (el: HTMLElement): [string | null, string, string][] =>
  [...el.querySelectorAll<HTMLElement>(`.${COMPOSER_ENTRY_CLASS}`)].map((l) => [
    l.getAttribute("data-node"),
    l.style.getPropertyValue(COMPOSER_LEFT_PROPERTY),
    l.style.getPropertyValue(COMPOSER_RIGHT_PROPERTY),
  ]);

const highlight = createComposerEntryHighlight(document);

beforeEach(() => {
  highlight.apply(noEntry());
});

describe("the composer's box", () => {
  it("spans the node's subtree, from its label's column to the widest line", () => {
    // Arrange
    const row = drawnRow("r1", example);
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert: `└── ` is four columns; the grandchild's line is the widest.
    const widest = `    └── 1.1.1 It goes live with the next rollout`.length;
    expect(boxed(row)).toEqual([
      ["1.1", "4ch", `${widest}ch`],
      ["1.1.1", "4ch", `${widest}ch`],
    ]);
  });

  it("starts at the first column for a root", () => {
    // Arrange
    const row = drawnRow("r1", example);
    // Act
    highlight.apply(nodeEntry("r1", "1"));
    // Assert
    expect(boxed(row).map(([, left]) => left)).toEqual(["0ch", "0ch", "0ch", "0ch", "0ch"]);
  });

  it("leaves out the node's parent, its siblings and a label that merely shares its characters", () => {
    // Arrange
    const row = drawnRow("r1", example);
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    expect(boxed(row).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it("boxes nothing in another row", () => {
    // Arrange
    const row = drawnRow("r2", example);
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    expect(boxed(row)).toEqual([]);
  });

  it("is taken down when the composer answers no node", () => {
    // Arrange
    const row = drawnRow("r1", example);
    highlight.apply(nodeEntry("r1", "1.1"));
    // Act
    highlight.apply(noEntry());
    // Assert
    expect(boxed(row)).toEqual([]);
  });

  it("moves when the composer moves to another node", () => {
    // Arrange
    const row = drawnRow("r1", example);
    highlight.apply(nodeEntry("r1", "1.1"));
    // Act
    highlight.apply(nodeEntry("r1", "1.2"));
    // Assert
    expect(boxed(row).map(([node]) => node)).toEqual(["1.2"]);
  });

  it("is drawn again on a repaint of the row's prose", () => {
    // Arrange
    highlight.apply(nodeEntry("r1", "1.1"));
    // Act: the prose is painted afresh, and its paint marks it.
    const row = drawnRow("r1", example);
    markComposerEntry(row, "r1");
    // Assert
    expect(boxed(row).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it("boxes each tree holding the node on its own, and no tree that does not", () => {
    // Arrange
    const other = tree([["2", "Elsewhere"]]);
    const row = drawnRow("r1", example, other, example);
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    expect(boxed(row).map(([node]) => node)).toEqual(["1.1", "1.1.1", "1.1", "1.1.1"]);
  });

  it.each([
    ["no state", create(FeedComposerEntrySchema, {})],
    ["a node naming no row", create(FeedComposerEntrySchema, { at: { case: "node", value: { label: "1.1" } } })],
  ])("refuses a push with %s", (_name, entry) => {
    // Act + Assert
    expect(() => highlight.apply(entry)).toThrow(MalformedView);
  });

  it("records each push at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.composer-entry.applied");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ row: "r1", label: "1.1" })]);
  });
});
