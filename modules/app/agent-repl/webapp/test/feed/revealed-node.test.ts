// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedSelectionSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  REVEAL_LEFT_PROPERTY,
  REVEAL_RIGHT_PROPERTY,
  REVEALED_NODE_CLASS,
  applyRevealedNode,
  markRevealedNode,
  revealedNodeOf,
} from "../../src/feed/revealed-node.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { selectionOf } from "./harness.js";
import { boxedBy, drawnRow, exampleSubtreeWidth, exampleTree } from "./tree-fixture.js";

const REVEALED_BOX = { className: REVEALED_NODE_CLASS, leftProperty: REVEAL_LEFT_PROPERTY, rightProperty: REVEAL_RIGHT_PROPERTY };

afterEach(() => {
  applyRevealedNode(document, selectionOf({ none: "stay" }));
});

describe("revealedNodeOf", () => {
  it.each([
    ["a final response an entry revealed", selectionOf({ response: "r1", viewport: { revealNode: "1.1" } }), { row: "r1", label: "1.1" }],
    ["another bubble an entry revealed", selectionOf({ bubble: "r1", viewport: { revealNode: "1.1" } }), { row: "r1", label: "1.1" }],
    ["a response a click selected", selectionOf({ response: "r1", viewport: "hold" }), undefined],
    ["a prompt a linked label revealed", selectionOf({ prompt: "p1", viewport: { reveal: "1.1" } }), undefined],
    ["nothing selected", selectionOf({ none: "returnToTail" }), undefined],
  ])("answers %s", (_name, selection, want) => {
    // Act + Assert
    expect(revealedNodeOf(selection)).toEqual(want);
  });

  it.each([
    ["no state", create(FeedSelectionSchema, {})],
    ["a response with no viewport", create(FeedSelectionSchema, { selection: { case: "response", value: { row: { value: "r1" } } } })],
    [
      "a revealed node naming no row",
      create(FeedSelectionSchema, {
        selection: { case: "response", value: { viewport: { viewport: { case: "revealNode", value: { label: "1.1" } } } } },
      }),
    ],
  ])("refuses a selection with %s", (_name, selection) => {
    // Act + Assert
    expect(() => revealedNodeOf(selection)).toThrow(MalformedView);
  });
});

describe("the revealed node's box", () => {
  it("boxes the revealed node's subtree from its label's column to the widest line", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    // Act
    applyRevealedNode(document, selectionOf({ response: "r1", viewport: { revealNode: "1.1" } }));
    // Assert
    expect(boxedBy(row, REVEALED_BOX)).toEqual([
      ["1.1", "4ch", `${exampleSubtreeWidth}ch`],
      ["1.1.1", "4ch", `${exampleSubtreeWidth}ch`],
    ]);
  });

  it("is taken down by a selection revealing no node", () => {
    // Arrange
    const row = drawnRow("r1", exampleTree);
    applyRevealedNode(document, selectionOf({ response: "r1", viewport: { revealNode: "1.1" } }));
    // Act
    applyRevealedNode(document, selectionOf({ response: "r1", viewport: "hold" }));
    // Assert
    expect(boxedBy(row, REVEALED_BOX)).toEqual([]);
  });

  it("is drawn again on a repaint of the response's prose", () => {
    // Arrange
    applyRevealedNode(document, selectionOf({ response: "r1", viewport: { revealNode: "1.1" } }));
    // Act
    const row = drawnRow("r1", exampleTree);
    markRevealedNode(row, "r1");
    // Assert
    expect(boxedBy(row, REVEALED_BOX).map(([node]) => node)).toEqual(["1.1", "1.1.1"]);
  });

  it("records each selection's node at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    applyRevealedNode(document, selectionOf({ response: "r1", viewport: { revealNode: "1.1" } }));
    // Assert
    const record = await forwardedRecord(capture, "feed.revealed-node.applied");
    expect([record.level.case, record.context]).toEqual(["debug", expect.objectContaining({ row: "r1", label: "1.1" })]);
  });
});
