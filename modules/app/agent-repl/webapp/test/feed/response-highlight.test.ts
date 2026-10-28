// @vitest-environment jsdom
/**
 * response-highlight — words highlighted in a final response, told to the
 * daemon as the selection settles.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  SelectResponseTextResponseSchema,
  type SelectResponseTextRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_response_text_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { RESPONSE_PROSE_CLASS } from "../../src/feed/cards/response.js";
import { installResponseHighlight, responseHighlightIn } from "../../src/feed/response-highlight.js";
import { FEED_ROW_ATTRIBUTE } from "../../src/feed/row-id.js";
import { FINAL_RESPONSE_CLASS } from "../../src/feed/rows/turn-ended.js";
import { scriptedAppContext } from "../rpc/app-context.js";
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

/** A page whose daemon takes every highlight, recording each request. */
function scripted(answer?: () => unknown): {
  calls: SelectResponseTextRequest[];
  reported: FailureKind[];
  install: (host: HTMLElement) => void;
} {
  const calls: SelectResponseTextRequest[] = [];
  const reported: FailureKind[] = [];
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
      failures: { report: (kind) => reported.push(kind), retract: () => {} },
    },
  );
  return {
    calls,
    reported,
    install: (host) => {
      teardown = installResponseHighlight(host, ctx);
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

  it("clears the held highlight once when the selection collapses", async () => {
    // Arrange
    const host = feedWith([{ row: "r1", text: "the entropy rises" }]);
    const page = scripted();
    page.install(host);
    select({ row: "r1", at: 4 }, { row: "r1", at: 11 });
    settled();
    // Act
    document.getSelection()?.removeAllRanges();
    settled();
    settled();
    await answered();
    // Assert
    expect(sent(page.calls)).toEqual(["r1:entropy", "cleared"]);
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
