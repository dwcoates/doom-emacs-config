// @vitest-environment jsdom
import { beforeEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedComposerEntrySchema, type FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  COMPOSER_ENTRY_CLASS,
  COMPOSER_LEFT_PROPERTY,
  COMPOSER_RIGHT_PROPERTY,
  createComposerEntryHighlight,
  markComposerEntry,
} from "../../src/feed/composer-entry.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { boxedBy, drawnRow, exampleSubtreeWidth, exampleTree as example, labelledTree as tree } from "./tree-fixture.js";

/** The push naming LABEL's node in ROW. */
const nodeEntry = (row: string, label: string): FeedComposerEntry =>
  create(FeedComposerEntrySchema, { at: { case: "node", value: { row: { value: row }, label } } });

/** The push naming no node. */
const noEntry = (): FeedComposerEntry => create(FeedComposerEntrySchema, { at: { case: "none", value: {} } });

/** Each line of EL inside the composer's box, as (its node, its left edge, its right edge). */
const boxed = (el: HTMLElement): [string | null, string, string][] =>
  boxedBy(el, { className: COMPOSER_ENTRY_CLASS, leftProperty: COMPOSER_LEFT_PROPERTY, rightProperty: COMPOSER_RIGHT_PROPERTY });

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
    const widest = exampleSubtreeWidth;
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
