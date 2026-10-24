// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedComposerEntrySchema, type FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  COMPOSER_ENTRY_STYLE_ID,
  composerEntryRule,
  createComposerEntryHighlight,
} from "../../src/feed/composer-entry.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** The push naming LABEL's node in ROW. */
const nodeEntry = (row: string, label: string): FeedComposerEntry =>
  create(FeedComposerEntrySchema, { at: { case: "node", value: { row: { value: row }, label } } });

/** The push naming no node. */
const noEntry = (): FeedComposerEntry => create(FeedComposerEntrySchema, { at: { case: "none", value: {} } });

/** A response row holding two tree lines of node 3.1 and one of its child. */
function rowWithNode(): HTMLElement {
  const row = document.createElement("article");
  row.setAttribute("data-feed-row", "r1");
  row.innerHTML =
    `<div class="mp-line" data-node="3.1">head</div>` +
    `<div class="mp-line" data-node="3.1">wrap</div>` +
    `<div class="mp-line" data-node="3.1.1">child</div>`;
  document.body.replaceChildren(row);
  return row;
}

describe("composerEntryRule", () => {
  it("selects the node's own lines in its row", () => {
    // Arrange
    const row = rowWithNode();
    const selector = composerEntryRule(nodeEntry("r1", "3.1")).split(" {")[0];
    // Act
    const hits = [...document.querySelectorAll(selector)].map((el) => el.textContent);
    // Assert
    expect([hits, row.isConnected]).toEqual([["head", "wrap"], true]);
  });

  it("draws nothing when no node is answered", () => {
    // Act + Assert
    expect(composerEntryRule(noEntry())).toBe("");
  });

  it("quotes a row and a label carrying quotes and backslashes", () => {
    // Arrange
    const row = document.createElement("article");
    row.setAttribute("data-feed-row", 'r"1\\');
    row.innerHTML = `<div class="mp-line" data-node='3"1'>head</div>`;
    document.body.replaceChildren(row);
    const selector = composerEntryRule(nodeEntry('r"1\\', '3"1')).split(" {")[0];
    // Act + Assert
    expect(document.querySelectorAll(selector).length).toBe(1);
  });

  it.each([
    ["no state", create(FeedComposerEntrySchema, {})],
    ["a node naming no row", create(FeedComposerEntrySchema, { at: { case: "node", value: { label: "3.1" } } })],
  ])("refuses a push with %s", (_name, entry) => {
    // Act + Assert
    expect(() => composerEntryRule(entry)).toThrow(MalformedView);
  });
});

describe("createComposerEntryHighlight", () => {
  it("draws each push into one style element", () => {
    // Arrange
    const highlight = createComposerEntryHighlight(document);
    // Act
    highlight.apply(nodeEntry("r1", "3.1"));
    highlight.apply(nodeEntry("r1", "3.2"));
    // Assert
    const styles = document.querySelectorAll(`#${COMPOSER_ENTRY_STYLE_ID}`);
    expect([styles.length, styles[0]?.textContent]).toEqual([1, composerEntryRule(nodeEntry("r1", "3.2"))]);
  });

  it("clears the highlight when no node is answered", () => {
    // Arrange
    const highlight = createComposerEntryHighlight(document);
    highlight.apply(nodeEntry("r1", "3.1"));
    // Act
    highlight.apply(noEntry());
    // Assert
    expect(document.getElementById(COMPOSER_ENTRY_STYLE_ID)?.textContent).toBe("");
  });

  it("keeps highlighting lines a repaint drew after the push", () => {
    // Arrange
    const highlight = createComposerEntryHighlight(document);
    highlight.apply(nodeEntry("r1", "3.1"));
    // Act: the row's prose is repainted from scratch.
    rowWithNode();
    // Assert
    const selector = composerEntryRule(nodeEntry("r1", "3.1")).split(" {")[0];
    expect(document.querySelectorAll(selector).length).toBe(2);
  });

  it("records each highlight at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const highlight = createComposerEntryHighlight(document);
    // Act
    highlight.apply(nodeEntry("r1", "3.1"));
    // Assert
    const record = await forwardedRecord(capture, "feed.composer-entry.applied");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ row: "r1", label: "3.1" })]);
  });
});
