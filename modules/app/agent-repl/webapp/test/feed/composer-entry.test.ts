// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedComposerEntrySchema, type FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  COMPOSER_EDITED_EVENT,
  COMPOSER_ENTRY_CLASS,
  COMPOSER_LEFT_PROPERTY,
  COMPOSER_RIGHT_PROPERTY,
  composerBoxGeometry,
  createComposerEntryHighlight,
  markComposerEntry,
} from "../../src/feed/composer-entry.js";
import { TailFollow } from "../../src/scroll.js";
import { boxRect } from "../dom-rect.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import {
  boxedBy,
  drawnRow,
  exampleSubtreeWidth,
  exampleTree as example,
  labelledTree as tree,
  lineOf,
} from "./tree-fixture.js";

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

/** A 300px feed viewport at the top of the screen, scrolled 100px into a 1000px feed. */
function feedScroll(): { box: HTMLElement; position: { scrollTop: number; scrollHeight: number; clientHeight: number }; tail: TailFollow } {
  const box = document.createElement("div");
  box.getBoundingClientRect = () => boxRect({ top: 0, height: 300 });
  const position = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
  return { box, position, tail: new TailFollow(position) };
}

/** Lay out each (node, top, height) line of ROW at that place on the screen. */
function layOut(row: HTMLElement, lines: [string, number, number][]): void {
  for (const [node, top, height] of lines) lineOf(row, node).getBoundingClientRect = () => boxRect({ top, height });
}

describe("the composer's box brought into view", () => {
  /** The example's lines, with 1.1's subtree cut by the viewport's bottom (280-320) and 1.2 far below. */
  const laidOut = (row: HTMLElement): void =>
    layOut(row, [
      ["1", 240, 20],
      ["1.1", 280, 20],
      ["1.1.1", 300, 20],
      ["1.2", 600, 20],
      ["1.10", 620, 20],
    ]);

  it("moves the feed only as far as a subtree cut by the fold needs", () => {
    // Arrange
    const row = drawnRow("r1", example);
    laidOut(row);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert: the subtree's bottom (320) moves to the viewport's (300).
    expect(feed.position.scrollTop).toBe(120);
  });

  it("moves nothing for a subtree wholly in view", () => {
    // Arrange
    const row = drawnRow("r1", example);
    layOut(row, [["1.1", 50, 20], ["1.1.1", 70, 20]]);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    expect(feed.position.scrollTop).toBe(100);
  });

  it("moves nothing on a push restating the node already boxed", () => {
    // Arrange: the reader scrolled back after the first reveal.
    const row = drawnRow("r1", example);
    laidOut(row);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    highlight.apply(nodeEntry("r1", "1.1"));
    feed.position.scrollTop = 100;
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    expect(feed.position.scrollTop).toBe(100);
  });

  it("brings each node the composer moves to into view", () => {
    // Arrange
    const row = drawnRow("r1", example);
    laidOut(row);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    highlight.apply(nodeEntry("r1", "1.1"));
    // Act
    highlight.apply(nodeEntry("r1", "1.2"));
    // Assert: 1.2's bottom (620) moves to the viewport's (300), from 120.
    expect(feed.position.scrollTop).toBe(440);
  });

  it("brings the node into view again after the composer answered none", () => {
    // Arrange
    const row = drawnRow("r1", example);
    laidOut(row);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    highlight.apply(nodeEntry("r1", "1.1"));
    highlight.apply(noEntry());
    feed.position.scrollTop = 100;
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    expect(feed.position.scrollTop).toBe(120);
  });

  it("moves nothing and records it at debug when the node is not laid out", async () => {
    // Arrange: jsdom lays nothing out, so every line's box is empty.
    drawnRow("r1", example);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    const capture = captureLogRecords("debug");
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.composer-entry.reveal-absent");
    expect([feed.position.scrollTop, record.level.case, record.context]).toEqual([
      100,
      "debug",
      expect.objectContaining({ row: "r1", label: "1.1" }),
    ]);
  });

  it("records whether the reveal moved the feed at debug", async () => {
    // Arrange
    const row = drawnRow("r1", example);
    laidOut(row);
    const feed = feedScroll();
    const highlight = createComposerEntryHighlight(document, feed);
    const capture = captureLogRecords("debug");
    // Act
    highlight.apply(nodeEntry("r1", "1.1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.composer-entry.revealed");
    expect([record.level.case, record.context]).toEqual([
      "debug",
      expect.objectContaining({ row: "r1", label: "1.1", moved: true }),
    ]);
  });
});

describe("composerBoxGeometry", () => {
  it("spans the boxed lines from the first's top to the last's bottom, against the feed's viewport", () => {
    // Arrange
    const row = drawnRow("r1", example);
    layOut(row, [["1.1", 280, 20], ["1.1.1", 300, 25]]);
    highlight.apply(nodeEntry("r1", "1.1"));
    const box = document.createElement("div");
    box.getBoundingClientRect = () => boxRect({ top: 10, height: 300 });
    // Act
    const got = composerBoxGeometry(document, box);
    // Assert
    expect(got).toEqual({ boxTop: 10, boxHeight: 300, nodeTop: 280, nodeHeight: 45 });
  });

  it("leaves out a boxed line that is not laid out", () => {
    // Arrange
    const row = drawnRow("r1", example);
    layOut(row, [["1.1", 280, 20]]);
    highlight.apply(nodeEntry("r1", "1.1"));
    const box = document.createElement("div");
    box.getBoundingClientRect = () => boxRect({ top: 0, height: 300 });
    // Act
    const got = composerBoxGeometry(document, box);
    // Assert
    expect(got).toEqual({ boxTop: 0, boxHeight: 300, nodeTop: 280, nodeHeight: 20 });
  });

  it("is null when no boxed line is laid out", () => {
    // Arrange
    drawnRow("r1", example);
    highlight.apply(nodeEntry("r1", "1.1"));
    // Act
    const got = composerBoxGeometry(document, document.createElement("div"));
    // Assert
    expect(got).toBeNull();
  });
});

describe("the edit count of a push", () => {
  const withEdits = (edits: bigint): FeedComposerEntry => create(FeedComposerEntrySchema, { at: { case: "none", value: {} }, edits });
  let announced = 0;
  const count = (): void => {
    announced++;
  };
  beforeEach(() => {
    announced = 0;
    document.addEventListener(COMPOSER_EDITED_EVENT, count);
  });
  afterEach(() => document.removeEventListener(COMPOSER_EDITED_EVENT, count));

  it("announces nothing for the first push, which only restates the count", () => {
    // Arrange
    const fresh = createComposerEntryHighlight(document);
    // Act
    fresh.apply(withEdits(5n));
    // Assert
    expect(announced).toBe(0);
  });

  it("announces typing when the count changes", () => {
    // Arrange
    const fresh = createComposerEntryHighlight(document);
    fresh.apply(withEdits(5n));
    // Act
    fresh.apply(withEdits(6n));
    // Assert
    expect(announced).toBe(1);
  });

  it("announces nothing when the count stands", () => {
    // Arrange
    const fresh = createComposerEntryHighlight(document);
    fresh.apply(withEdits(5n));
    // Act
    fresh.apply(withEdits(5n));
    // Assert
    expect(announced).toBe(0);
  });
});
