// @vitest-environment jsdom
/**
 * bubble-selection — which root-feed bubbles the selection governs, the click
 * decision, and the one request every webapp selection change is sent through.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { SelectFeedRowResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import { FeedRowSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  OUT_OF_VIEW_COLLAPSE_ATTRIBUTE,
  SELECTABLE_ATTRIBUTE,
  SELECTION_GOVERNED_ATTRIBUTE,
  SELECT_REQUEST,
  bubbleClick,
  expandsOnOwnClick,
  governedRowAt,
  installBubbleSelect,
  isSelectionGoverned,
  stampSelection,
} from "../../src/feed/bubble-selection.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { harness, type FeedScript } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** A row of ARM, optionally stamped selectable by the daemon. */
function rowOf(arm: MessageInitShape<typeof FeedRowSchema>["row"], selectable = false): FeedRow {
  return create(FeedRowSchema, {
    id: { value: "r1" },
    row: arm,
    selectable: selectable ? {} : undefined,
  });
}

const RESPONSE = { case: "activity", value: { unit: { case: "response", value: {} } } } as const;
const TOOL = { case: "activity", value: { unit: { case: "simpleToolCall", value: {} } } } as const;
const THINKING = { case: "activity", value: { unit: { case: "response", value: { thinking: true } } } } as const;
const INTERIM = {
  case: "activity",
  value: { unit: { case: "response", value: { interim: true, result: { case: "success", value: {} } } } },
} as const;

describe("expandsOnOwnClick", () => {
  it.each([
    ["a thinking bubble", THINKING, true],
    ["an interim response", INTERIM, true],
    ["a response that is its turn's latest row", RESPONSE, false],
    [
      "the turn's answer",
      { case: "activity", value: { unit: { case: "response", value: { interim: true, finalAnswer: true } } } },
      false,
    ],
    [
      "a cut-short response",
      { case: "activity", value: { unit: { case: "response", value: { interim: true, result: { case: "error", value: {} } } } } },
      false,
    ],
    ["a user prompt", { case: "userPrompt", value: {} }, false],
    ["a tool call", TOOL, false],
  ] as const)("answers %s: %s", (_name, arm, want) => {
    // Arrange / Act / Assert
    expect(expandsOnOwnClick(rowOf(arm))).toBe(want);
  });
});

describe("isSelectionGoverned", () => {
  it.each([
    ["a user prompt", { case: "userPrompt", value: {} }, true],
    ["an agent prompt", { case: "agentPrompt", value: {} }, true],
    ["a response bubble", RESPONSE, true],
    ["a thinking bubble", THINKING, false],
    ["an interim response", INTERIM, false],
    ["a tool call", TOOL, false],
    ["a separation", { case: "separation", value: {} }, false],
  ] as const)("answers %s: %s", (_name, arm, want) => {
    // Arrange / Act / Assert
    expect(isSelectionGoverned(rowOf(arm))).toBe(want);
  });
});

describe("stampSelection", () => {
  it.each([
    ["a selectable root response", RESPONSE, true, true, [true, true]],
    ["a root response still arriving", RESPONSE, false, true, [true, false]],
    ["a sub-feed response", RESPONSE, true, false, [false, false]],
    ["a root tool call", TOOL, true, true, [false, false]],
  ] as const)("stamps %s", (_name, arm, selectable, root, want) => {
    // Arrange
    const el = document.createElement("article");
    // Act
    stampSelection(el, rowOf(arm, selectable), root);
    // Assert
    expect([el.hasAttribute(SELECTION_GOVERNED_ATTRIBUTE), el.hasAttribute(SELECTABLE_ATTRIBUTE)]).toEqual(want);
  });

  it.each([
    ["a root thinking bubble", THINKING, true, [false, false, true]],
    ["a sub-feed thinking bubble", THINKING, false, [false, false, false]],
    ["a root interim response", INTERIM, true, [false, false, true]],
    ["a sub-feed interim response", INTERIM, false, [false, false, false]],
    ["a root response bubble", RESPONSE, true, [true, false, false]],
  ] as const)("stamps the out-of-view collapse on %s", (_name, arm, root, want) => {
    // Arrange
    const el = document.createElement("article");
    // Act
    stampSelection(el, rowOf(arm), root);
    // Assert
    expect([
      el.hasAttribute(SELECTION_GOVERNED_ATTRIBUTE),
      el.hasAttribute(SELECTABLE_ATTRIBUTE),
      el.hasAttribute(OUT_OF_VIEW_COLLAPSE_ATTRIBUTE),
    ]).toEqual(want);
  });

  it("clears the out-of-view collapse a re-push no longer states", () => {
    // Arrange
    const el = document.createElement("article");
    stampSelection(el, rowOf(THINKING), true);
    // Act
    stampSelection(el, rowOf(RESPONSE), true);
    // Assert
    expect(el.hasAttribute(OUT_OF_VIEW_COLLAPSE_ATTRIBUTE)).toBe(false);
  });

  it("clears a stamp a re-push no longer states", () => {
    // Arrange
    const el = document.createElement("article");
    stampSelection(el, rowOf(RESPONSE, true), true);
    // Act
    stampSelection(el, rowOf(TOOL), true);
    // Assert
    expect([el.hasAttribute(SELECTION_GOVERNED_ATTRIBUTE), el.hasAttribute(SELECTABLE_ATTRIBUTE)]).toEqual([
      false,
      false,
    ]);
  });
});

/**
 * A root feed with a governed selectable prompt-like row r1 holding a sub-feed
 * row n1, a governed row r2 not yet landed, a selectable response r3 drawing a
 * tree, and a response r4 drawing a tree that has not landed.
 */
function feed() {
  const host = document.createElement("main");
  host.innerHTML = `
    <article data-feed-row="r1" data-selection-governed data-selectable>
      <div class="bubble"><div class="bubble-scroll"><p id="prose">words <a id="link" href="#">x</a></p>
        <span class="mp-label mp-label-linked" data-label="1.1" data-answer-prompt="p9" id="linked">1.1</span>
        <div class="mp-line mp-entry" data-node="3.1" data-entry-label="3.1"><span class="mp-prefix">└── <span class="mp-label mp-entry-label" data-label="3.1" id="entry">3.1</span> </span><span class="mp-content" id="entry-text">yes</span></div></div>
        <div class="bubble-subfeed"><article data-feed-row="n1"><span id="nested">inner</span></article></div>
      </div>
    </article>
    <article data-feed-row="r2" data-selection-governed><div class="bubble"><p id="streaming">...</p>
      <div class="mp-entry" data-entry-label="2"><span class="mp-entry-label" id="entry-unlanded">2</span></div></div></article>
    <article data-feed-row="r3" data-unit="response" data-selection-governed data-selectable>
      <div class="bubble"><p id="response-prose">said</p><div class="mp-tree">
        <div class="mp-line" data-node="2"><span class="mp-prefix"><span class="mp-label" data-label="2" id="branch-label">2</span> </span><span class="mp-content" id="branch">a point</span></div>
        <div class="mp-line" data-node="2.1"><span class="mp-prefix">└── <span class="mp-label mp-label-linked" data-label="2.1" data-answer-prompt="p8" id="branch-linked">2.1</span> </span><span class="mp-content" id="branch-child">a finer point</span></div>
      </div></div>
    </article>
    <article data-feed-row="r4" data-unit="response" data-selection-governed><div class="mp-tree">
      <div class="mp-line" data-node="1"><span class="mp-content" id="branch-unlanded">arriving</span></div></div></article>
    <article data-feed-row="t1"><div class="tool-card" id="tool">tool</div></article>`;
  document.body.replaceChildren(host);
  const at = (selector: string): Element => {
    const el = host.querySelector(selector);
    if (el === null) throw new Error(`the feed draws no ${selector}`);
    return el;
  };
  return { host, at };
}

describe("governedRowAt", () => {
  it.each([
    ["the prose of a governed bubble", "#prose", "r1"],
    ["a governed bubble not yet landed", "#streaming", "r2"],
    ["a row in a bubble's sub-feed", "#nested", null],
    ["a tool card", "#tool", null],
  ] as const)("answers %s", (_name, selector, want) => {
    // Arrange
    const f = feed();
    // Act
    const row = governedRowAt(f.at(selector), f.host);
    // Assert
    expect(row?.getAttribute("data-feed-row") ?? null).toBe(want);
  });
});

describe("bubbleClick", () => {
  it.each([
    ["a link inside the bubble", "#link", "", { case: "ignore", reason: "interactive" }],
    ["a click that ends a text highlight", "#prose", "words", { case: "ignore", reason: "text-selection" }],
    ["a bubble that has not landed", "#streaming", "", { case: "ignore", reason: "not-landed" }],
    ["a selectable bubble", "#prose", "", { case: "select", row: "r1" }],
    ["a linked label", "#linked", "", { case: "answer", prompt: "p9", label: "1.1" }],
    ["a linked label ending a text highlight", "#linked", "1.1", { case: "ignore", reason: "text-selection" }],
    ["an entry's label", "#entry", "", { case: "entry", row: "r1", label: "3.1" }],
    ["an entry's words, which select its bubble", "#entry-text", "", { case: "select", row: "r1" }],
    ["an entry of a bubble that has not landed", "#entry-unlanded", "", { case: "ignore", reason: "not-landed" }],
    ["a response branch's words", "#branch", "", { case: "branch", row: "r3", label: "2" }],
    ["a response branch's label", "#branch-label", "", { case: "branch", row: "r3", label: "2" }],
    ["a response's prose outside its tree", "#response-prose", "", { case: "select", row: "r3" }],
    ["a linked label on a response branch", "#branch-linked", "", { case: "answer", prompt: "p8", label: "2.1" }],
    ["a response branch ending a text highlight", "#branch", "a point", { case: "ignore", reason: "text-selection" }],
    ["a branch of a response that has not landed", "#branch-unlanded", "", { case: "ignore", reason: "not-landed" }],
  ] as const)("decides %s", (_name, selector, selectedText, want) => {
    // Arrange
    const f = feed();
    const target = f.at(selector);
    const row = governedRowAt(target, f.host);
    if (row === null) throw new Error("the fixture's target is in no governed row");
    // Act
    const click = bubbleClick({ row, target, selectedText });
    // Assert
    expect(click).toEqual(want);
  });
});

describe("installBubbleSelect", () => {
  /** A feed with click-to-select armed. */
  function armed(script: FeedScript = {}) {
    const f = feed();
    const h = harness(script);
    const reported: FailureKind[] = [];
    vi.spyOn(h.sink, "report").mockImplementation((kind) => {
      reported.push(kind);
    });
    const uninstall = installBubbleSelect(f.host, h.ctx, () => "");
    return { ...f, h, reported, uninstall };
  }

  const click = (el: Element): void => {
    el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
  };

  it("asks the daemon to select the clicked bubble", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    expect(
      a.h.calls.selectFeedRow.map((r) => (r.move.case === "bubble" ? r.move.value.row?.value : r.move.case)),
    ).toEqual(["r1"]);
  });

  it("asks the daemon for the bubble again when it is clicked twice, never a clear", async () => {
    // Arrange: the daemon, not the webapp, toggles the selected bubble off.
    const a = armed();
    // Act
    click(a.at("#prose"));
    click(a.at("#prose"));
    await settle();
    // Assert
    expect(
      a.h.calls.selectFeedRow.map((r) => (r.move.case === "bubble" ? r.move.value.row?.value : r.move.case)),
    ).toEqual(["r1", "r1"]);
  });

  it("asks the daemon to select the prompt a linked label's answer is in", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#linked"));
    await settle();
    // Assert
    expect(
      a.h.calls.selectFeedRow.map((r) => (r.move.case === "answer" ? [r.move.value.row?.value, r.move.value.label] : r.move.case)),
    ).toEqual([["p9", "1.1"]]);
  });

  it("asks the daemon to select a clicked entry's prompt to edit the entry", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#entry"));
    await settle();
    // Assert
    expect(
      a.h.calls.selectFeedRow.map((r) => (r.move.case === "entry" ? [r.move.value.row?.value, r.move.value.label] : r.move.case)),
    ).toEqual([["r1", "3.1"]]);
  });

  it("asks the daemon to select a clicked branch's response to answer the node", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#branch-child"));
    await settle();
    // Assert
    expect(
      a.h.calls.selectFeedRow.map((r) => (r.move.case === "branch" ? [r.move.value.row?.value, r.move.value.label] : r.move.case)),
    ).toEqual([["r3", "2.1"]]);
  });

  it("logs a branch's click at info naming the row and the label", async () => {
    // Arrange
    const capture = captureLogRecords();
    const a = armed();
    // Act
    click(a.at("#branch"));
    // Assert
    const record = await forwardedRecord(capture, "feed.branch-click");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ row: "r3", label: "2" })]);
  });

  it("logs a linked label's click at info naming the prompt and the label", async () => {
    // Arrange
    const capture = captureLogRecords();
    const a = armed();
    // Act
    click(a.at("#linked"));
    // Assert
    const record = await forwardedRecord(capture, "feed.label-click-answer");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ prompt: "p9", label: "1.1" })]);
  });

  it("logs an entry's click at info naming the row and the label", async () => {
    // Arrange
    const capture = captureLogRecords();
    const a = armed();
    // Act
    click(a.at("#entry"));
    // Assert
    const record = await forwardedRecord(capture, "feed.entry-click");
    expect([record.level.case, record.context]).toEqual(["info", expect.objectContaining({ row: "r1", label: "3.1" })]);
  });

  it("asks nothing for a bubble that has not landed", async () => {
    // Arrange
    const a = armed();
    // Act
    click(a.at("#streaming"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("asks nothing once uninstalled", async () => {
    // Arrange
    const a = armed();
    a.uninstall();
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow).toEqual([]);
  });

  it("files a refused selection on the warning chip under the click's request", async () => {
    // Arrange
    const a = armed({
      selectFeedRow: () =>
        create(SelectFeedRowResponseSchema, {
          result: { case: "error", value: { cause: { case: "notSelectable", value: { row: { value: "r1" } } } } },
        }),
    });
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    const kind = a.reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? [kind.value.what, kind.value.cause] : kind?.case).toEqual([
      SELECT_REQUEST,
      "that bubble cannot be selected (r1)",
    ]);
  });

  it("logs a refused selection at error with its arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    armed({
      selectFeedRow: () =>
        create(SelectFeedRowResponseSchema, {
          result: { case: "error", value: { cause: { case: "notSelectable", value: { row: { value: "r1" } } } } },
        }),
    });
    // Act
    click(document.querySelector("#prose") as Element);
    await settle();
    // Assert
    const record = await forwardedRecord(capture, "feed.select-feed-row-refused");
    expect([record.level.case, record.context?.arm, record.context?.move]).toEqual([
      "error",
      "notSelectable",
      "bubble",
    ]);
  });

  it("files a click that failed at the transport on the warning chip", async () => {
    // Arrange
    const a = armed({
      selectFeedRow: () => {
        throw new ConnectError("connection refused", Code.Unavailable);
      },
    });
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    const kind = a.reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.what : kind?.case).toBe(SELECT_REQUEST);
  });
});
