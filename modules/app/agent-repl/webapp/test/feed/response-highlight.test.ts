// @vitest-environment jsdom
/**
 * response-highlight — words highlighted in a final response, told to the
 * daemon as the selection settles.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  SelectResponseTextResponseSchema,
  type SelectResponseTextRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_response_text_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { RESPONSE_PROSE_CLASS } from "../../src/feed/cards/response.js";
import {
  RESPONSE_HIGHLIGHT_NAME,
  cssHighlightPaint,
  installResponseHighlight,
  onResponseHighlightWords,
  responseHighlightIn,
  responseHighlightTaken,
  type HighlightPaint,
} from "../../src/feed/response-highlight.js";
import { createWebhostPresses, type WebhostPressSource } from "../../src/webhost-pressed.js";
import { FEED_ROW_ATTRIBUTE } from "../../src/feed/row-id.js";
import { FINAL_RESPONSE_CLASS } from "../../src/feed/rows/turn-ended.js";
import { scriptedAppContext } from "../rpc/app-context.js";
import { recordingFailures } from "../failure-sink.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

let teardown: (() => void) | undefined;

afterEach(() => {
  teardown?.();
  teardown = undefined;
  document.getSelection()?.removeAllRanges();
  document.body.replaceChildren();
});

/** A feed host holding row ROW, a response whose prose says TEXT; FINAL when it is the final answer. */
function feedWith(rows: { row: string; text: string; final?: boolean }[]): HTMLElement {
  const host = document.createElement("div");
  for (const { row, text, final } of rows) {
    const item = document.createElement("div");
    item.setAttribute(FEED_ROW_ATTRIBUTE, row);
    const bubble = document.createElement("div");
    bubble.className = final === false ? "bubble" : `bubble ${FINAL_RESPONSE_CLASS}`;
    const prose = document.createElement("div");
    prose.className = RESPONSE_PROSE_CLASS;
    prose.id = `prose-${row}`;
    prose.textContent = text;
    bubble.append(prose);
    item.append(bubble);
    host.append(item);
  }
  document.body.append(host);
  return host;
}

/** Select characters FROM to TO of ROW's prose text, or across two rows' prose. */
function select(start: { row: string; at: number }, end: { row: string; at: number } = start): void {
  const range = document.createRange();
  const text = (row: string): Text => document.getElementById(`prose-${row}`)?.firstChild as Text;
  range.setStart(text(start.row), start.at);
  range.setEnd(text(end.row), end.at);
  document.getSelection()?.removeAllRanges();
  document.getSelection()?.addRange(range);
}

/** The selection settled by a keyboard extension: no pointer is down. */
function settled(): void {
  document.dispatchEvent(new Event("selectionchange"));
}

/** Let the RPCs the reports sent answer. */
async function answered(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}

/** A press that comes up: the browser's pointer gesture around a selection. */
function clicked(): void {
  document.dispatchEvent(new Event("pointerdown"));
  document.dispatchEvent(new Event("pointerup"));
}

/** A page whose daemon takes every highlight, recording each request and each paint. */
function scripted(answer?: () => unknown): {
  calls: SelectResponseTextRequest[];
  reported: FailureKind[];
  painted: (string | null)[];
  presses: WebhostPressSource;
  install: (host: HTMLElement) => void;
} {
  const painted: (string | null)[] = [];
  const paint: HighlightPaint = { paint: (range) => painted.push(range === null ? null : range.toString()) };
  const presses = createWebhostPresses();
  const calls: SelectResponseTextRequest[] = [];
  const sink = recordingFailures();
  const reported = sink.reports;
  const ctx = scriptedAppContext(
    {
      selectResponseText: (req) => {
        calls.push(req);
        if (answer !== undefined) return answer() as never;
        return create(SelectResponseTextResponseSchema, { result: { case: "success", value: {} } });
      },
    },
    {
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      failures: sink,
    },
  );
  return {
    calls,
    reported,
    painted,
    presses,
    install: (host) => {
      teardown = installResponseHighlight(host, ctx, { paint, webhostPresses: presses });
    },
  };
}

/** What each request held: the row and words, or "cleared". */
function sent(calls: SelectResponseTextRequest[]): string[] {
  return calls.map((call) => {
    const state = call.selection?.state;
    return state?.case === "highlighted" ? `${state.value.row?.value ?? ""}:${state.value.words}` : (state?.case ?? "unset");
  });
}

describe("responseHighlightIn", () => {
  it("answers the words and the row of a highlight inside one final response's prose", () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    // Act, Assert
    expect(responseHighlightIn(host, document)).toEqual({ row: "r1", words: "entropy" });
  });

  it("answers nothing for a highlight in a response that is no final answer", () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises", final: false }]);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    // Act, Assert
    expect(responseHighlightIn(host, document)).toBeNull();
  });

  it("answers nothing for a highlight reaching across two responses", () => {
    // Arrange
    const host = feedWith([
      { row: "r1", text: "first answer" },
      { row: "r2", text: "second answer" },
    ]);
    select({ row: "r1", at: 6 }, { row: "r2", at: 6 });
    // Act, Assert
    expect(responseHighlightIn(host, document)).toBeNull();
  });

  it("answers nothing for a highlight of only whitespace", () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "a   b" }]);
    select({ row: "r1", at: 1 }, { row: "r1", at: 4 });
    // Act, Assert
    expect(responseHighlightIn(host, document)).toBeNull();
  });
});

describe("installResponseHighlight", () => {
  it("holds the words a settled selection highlights", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(sent(page.calls)).toEqual(["r1:entropy"]);
  });

  it("addresses the page's own workspace", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(page.calls[0]?.workspace?.id).toBe("ws-1");
  });

  it("reports nothing while the pointer is still dragging, and the settled highlight once it lifts", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    // Act
    document.dispatchEvent(new Event("pointerdown"));
    select({ row: "r1", at: 4 }, { row: "r1", at: 8 });
    settled();
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    const duringDrag = page.calls.length;
    document.dispatchEvent(new Event("pointerup"));
    await answered();
    // Assert
    expect(duringDrag).toBe(0);
    expect(sent(page.calls)).toEqual(["r1:entropy"]);
  });

  it("does not report the same highlight twice", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    settled();
    await answered();
    // Assert
    expect(sent(page.calls)).toEqual(["r1:entropy"]);
  });

  it("paints the held words on their own", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(page.painted).toEqual(["entropy"]);
  });

  it("keeps the highlight when the selection empties with no press behind it", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    document.getSelection()?.removeAllRanges();
    settled();
    await answered();
    // Assert
    expect([sent(page.calls), page.painted]).toEqual([["r1:entropy"], ["entropy"]]);
  });

  it("records that the highlight stands when the selection empties with no press behind it", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    const records = captureLogRecords("debug");
    // Act
    document.getSelection()?.removeAllRanges();
    settled();
    // Assert
    const record = await forwardedRecord(records, "feed.response-highlight.stands");
    expect(record.context?.row).toBe("r1");
  });

  it("clears the held highlight once when a press settles on no highlight", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    document.getSelection()?.removeAllRanges();
    settled();
    // Act
    clicked();
    clicked();
    await answered();
    // Assert
    expect([sent(page.calls), page.painted]).toEqual([["r1:entropy", "cleared"], ["entropy", null]]);
  });

  it("records why the highlight ended", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    document.getSelection()?.removeAllRanges();
    const records = captureLogRecords("debug");
    // Act
    clicked();
    // Assert
    const record = await forwardedRecord(records, "feed.response-highlight.cleared");
    expect([record.context?.row, record.context?.why]).toEqual(["r1", "a press settled on no highlight"]);
  });

  it("clears the held highlight when other words are selected with no press", async () => {
    // Arrange
    const host = feedWith([
      { row: "r1", text: "the entropy rises" },
      { row: "r2", text: "an interim note", final: false },
    ]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    select({ row: "r2", at: 3 }, { row: "r2", at: 10 });
    settled();
    await answered();
    // Assert
    expect(sent(page.calls)).toEqual(["r1:entropy", "cleared"]);
  });

  it("clears the held highlight and the selection in the feed when the reader presses the webhost", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    page.presses.pressed();
    await answered();
    // Assert
    expect([sent(page.calls), page.painted, document.getSelection()?.toString()]).toEqual([
      ["r1:entropy", "cleared"],
      ["entropy", null],
      "",
    ]);
  });

  it("sends nothing when the reader presses the webhost with nothing held", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    // Act
    page.presses.pressed();
    await answered();
    // Assert
    expect([page.calls, page.painted]).toEqual([[], []]);
  });

  it("drops the paint without telling the daemon when the sent prompt takes the held row", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act: as the revising bubble does, the selection goes, then the row is taken.
    document.getSelection()?.removeAllRanges();
    responseHighlightTaken(document, "r1");
    await answered();
    // Assert
    expect([sent(page.calls), page.painted]).toEqual([["r1:entropy"], ["entropy", null]]);
  });

  it("holds the same words again once the sent prompt took them", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    responseHighlightTaken(document, "r1");
    // Act
    settled();
    await answered();
    // Assert
    expect(sent(page.calls)).toEqual(["r1:entropy", "r1:entropy"]);
  });

  it("keeps the highlight when the sent prompt took another row", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    responseHighlightTaken(document, "r9");
    await answered();
    // Assert
    expect(page.painted).toEqual(["entropy"]);
  });

  it("tells the page's listeners the words it holds, then none once they end", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    const told: (string | null)[] = [];
    const stop = onResponseHighlightWords(document, (words) => told.push(words));
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    page.presses.pressed();
    await answered();
    // Assert
    stop();
    expect(told).toEqual(["entropy", null]);
  });

  it("tells the page's listeners none once the sent prompt took the words", () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    const told: (string | null)[] = [];
    const stop = onResponseHighlightWords(document, (words) => told.push(words));
    // Act
    responseHighlightTaken(document, "r1");
    // Assert
    stop();
    expect(told).toEqual([null]);
  });

  it("drops the paint once uninstalled", () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    teardown?.();
    teardown = undefined;
    // Assert
    expect(page.painted).toEqual(["entropy", null]);
  });

  it("sends nothing for a selection that never touched a final response", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises", final: false }]);
    const page = scripted();
    page.install(host);
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(page.calls).toEqual([]);
  });

  it("files the daemon's refusal on the chip and in the log", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted(() =>
      create(SelectResponseTextResponseSchema, { result: { case: "error", value: { cause: { case: "notAFinalResponse", value: {} } } } }),
    );
    page.install(host);
    const records = captureLogRecords();
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(page.reported).toHaveLength(1);
    const record = await forwardedRecord(records, "feed.response-highlight.refused");
    expect([record.level.case, record.context?.arm]).toEqual(["error", "notAFinalResponse"]);
  });

  it("files a daemon it cannot reach on the chip", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted(() => {
      throw new ConnectError("connection refused", Code.Unavailable);
    });
    page.install(host);
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(page.reported).toHaveLength(1);
  });

  it("stops reporting once uninstalled", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    teardown?.();
    teardown = undefined;
    // Act
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    await answered();
    // Assert
    expect(page.calls).toEqual([]);
  });
});

describe("cssHighlightPaint", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  /** A stand-in registry and Highlight constructor, recording what was set under which name. */
  function stubHighlights(): Map<string, Range[]> {
    const registry = new Map<string, Range[]>();
    vi.stubGlobal("CSS", { highlights: registry });
    vi.stubGlobal(
      "Highlight",
      class {
        constructor(...ranges: Range[]) {
          return ranges;
        }
      },
    );
    return registry;
  }

  it("registers the range under the response highlight's name", () => {
    // Arrange
    const registry = stubHighlights();
    const range = document.createRange();
    // Act
    cssHighlightPaint.paint(range);
    // Assert
    expect(registry.get(RESPONSE_HIGHLIGHT_NAME)).toEqual([range]);
  });

  it("removes the registration when painting nothing", () => {
    // Arrange
    const registry = stubHighlights();
    cssHighlightPaint.paint(document.createRange());
    // Act
    cssHighlightPaint.paint(null);
    // Assert
    expect(registry.has(RESPONSE_HIGHLIGHT_NAME)).toBe(false);
  });

  it("fails loudly on a page without the Custom Highlight API", () => {
    // Arrange
    vi.stubGlobal("CSS", {});
    // Act, Assert
    expect(() => cssHighlightPaint.paint(document.createRange())).toThrow(/CSS Custom Highlight API is missing/);
  });

  it("records the missing API at error", async () => {
    // Arrange
    vi.stubGlobal("CSS", {});
    const records = captureLogRecords();
    // Act
    expect(() => cssHighlightPaint.paint(null)).toThrow();
    // Assert
    const record = await forwardedRecord(records, "feed.response-highlight.unpaintable");
    expect(record.level.case).toBe("error");
  });
});
