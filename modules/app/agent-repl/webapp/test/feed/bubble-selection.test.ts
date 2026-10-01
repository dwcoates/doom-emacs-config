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
  DESELECT_REQUEST,
  SELECTABLE_ATTRIBUTE,
  SELECTION_GOVERNED_ATTRIBUTE,
  SELECT_REQUEST,
  bubbleClick,
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

describe("isSelectionGoverned", () => {
  it.each([
    ["a user prompt", { case: "userPrompt", value: {} }, true],
    ["an agent prompt", { case: "agentPrompt", value: {} }, true],
    ["a response bubble", RESPONSE, true],
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

/** A root feed with a governed selectable row r1 holding a sub-feed row n1, and a governed row r2 not yet landed. */
function feed() {
  const host = document.createElement("main");
  host.innerHTML = `
    <article data-feed-row="r1" data-selection-governed data-selectable>
      <div class="bubble"><div class="bubble-scroll"><p id="prose">words <a id="link" href="#">x</a></p></div>
        <div class="bubble-subfeed"><article data-feed-row="n1"><span id="nested">inner</span></article></div>
      </div>
    </article>
    <article data-feed-row="r2" data-selection-governed><div class="bubble"><p id="streaming">...</p></div></article>
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
    ["a link inside the bubble", "#link", "", null, { case: "ignore", reason: "interactive" }],
    ["a click that ends a text highlight", "#prose", "words", null, { case: "ignore", reason: "text-selection" }],
    ["a bubble that has not landed", "#streaming", "", null, { case: "ignore", reason: "not-landed" }],
    ["an unselected bubble", "#prose", "", null, { case: "select", row: "r1" }],
    ["the selected bubble", "#prose", "", "r1", { case: "clear" }],
  ] as const)("decides %s", (_name, selector, selectedText, selectedRow, want) => {
    // Arrange
    const f = feed();
    const target = f.at(selector);
    const row = governedRowAt(target, f.host);
    if (row === null) throw new Error("the fixture's target is in no governed row");
    // Act
    const click = bubbleClick({ row, target, selectedRow, selectedText });
    // Assert
    expect(click).toEqual(want);
  });
});

describe("installBubbleSelect", () => {
  /** A feed with click-to-select armed, the daemon's last selection SELECTED. */
  function armed(script: FeedScript = {}, selected: string | null = null) {
    const f = feed();
    const h = harness(script);
    const reported: FailureKind[] = [];
    vi.spyOn(h.sink, "report").mockImplementation((kind) => {
      reported.push(kind);
    });
    const uninstall = installBubbleSelect(f.host, h.ctx, () => selected, () => "");
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

  it("asks the daemon to clear when the selected bubble is clicked", async () => {
    // Arrange
    const a = armed({}, "r1");
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    expect(a.h.calls.selectFeedRow.map((r) => r.move.case)).toEqual(["clear"]);
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

  it("files a clear that failed at the transport on the warning chip", async () => {
    // Arrange
    const a = armed(
      {
        selectFeedRow: () => {
          throw new ConnectError("connection refused", Code.Unavailable);
        },
      },
      "r1",
    );
    // Act
    click(a.at("#prose"));
    await settle();
    // Assert
    const kind = a.reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.what : kind?.case).toBe(DESELECT_REQUEST);
  });
});
