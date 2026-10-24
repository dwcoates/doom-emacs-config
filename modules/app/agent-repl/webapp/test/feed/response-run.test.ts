// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedRowSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  RESPONSE_RUN_CLASS,
  RESPONSE_RUN_ITEM_CLASS,
  createResponseRunStore,
  isFinalResponseRow,
  responseRunEnd,
  responseRunKey,
} from "../../src/feed/response-run.js";
import { orderFor } from "../feed-order.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { plainProse } from "../prose.js";
import {
  faultEndingRow,
  feedId,
  finalResponseRow,
  interimRow,
  responseRow,
  thinkingRow,
  toolCallRow,
  turnEndedRow,
  userPromptRow,
} from "./harness.js";

/** A final response row that is ALSO flagged THINKING. */
function thinkingFinalRow(id: string): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: {
      case: "activity",
      value: {
        unit: {
          case: "response",
          value: { finalAnswer: true, thinking: true, result: { case: "success", value: { prose: { prose: plainProse("x") } } } },
        },
      },
    },
  });
}

/** A final response row carrying a vendor NOTICE. */
function noticeFinalRow(id: string): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: {
      case: "activity",
      value: {
        unit: {
          case: "response",
          value: {
            finalAnswer: true,
            notice: { marker: {} },
            result: { case: "success", value: { prose: { prose: plainProse("x") } } },
          },
        },
      },
    },
  });
}

/** A member element: an `<article data-feed-row>` wrapping a `.bubble`. */
function member(id: string): HTMLElement {
  const el = document.createElement("article");
  el.className = "feed-item";
  el.setAttribute("data-feed-row", id);
  const bubble = document.createElement("div");
  bubble.className = "bubble";
  el.append(bubble);
  return el;
}

describe("isFinalResponseRow", () => {
  it("accepts a response the daemon stamped as the turn's answer", () => {
    expect(isFinalResponseRow(finalResponseRow("a"))).toBe(true);
  });

  it("rejects a response with no final-answer stamp", () => {
    expect(isFinalResponseRow(responseRow("a"))).toBe(false);
  });

  it("rejects an interim response", () => {
    expect(isFinalResponseRow(interimRow("a"))).toBe(false);
  });

  it("rejects a thinking bubble", () => {
    expect(isFinalResponseRow(thinkingRow("a"))).toBe(false);
  });

  it("rejects a thinking bubble even when stamped final", () => {
    expect(isFinalResponseRow(thinkingFinalRow("a"))).toBe(false);
  });

  it("rejects a vendor notice even when stamped final", () => {
    expect(isFinalResponseRow(noticeFinalRow("a"))).toBe(false);
  });

  it("rejects a row that is not an activity", () => {
    expect(isFinalResponseRow(userPromptRow("a", "hi"))).toBe(false);
  });

  it("rejects an activity that is not a response", () => {
    expect(isFinalResponseRow(toolCallRow("a", "returned"))).toBe(false);
  });
});

describe("responseRunEnd", () => {
  it("spans two adjacent final responses", () => {
    expect(responseRunEnd([finalResponseRow("a"), finalResponseRow("b")], 0)).toBe(2);
  });

  it("spans a concluded turn ending between two final responses", () => {
    const rows = [finalResponseRow("a"), turnEndedRow("e", "t1"), finalResponseRow("b")];
    expect(responseRunEnd(rows, 0)).toBe(3);
  });

  it("ends on the last final response, leaving a trailing concluded ending out", () => {
    const rows = [
      finalResponseRow("a"),
      turnEndedRow("e1", "t1"),
      finalResponseRow("b"),
      turnEndedRow("e2", "t2"),
    ];
    expect(responseRunEnd(rows, 0)).toBe(3);
  });

  it("spans every final response of a longer run", () => {
    const rows = [finalResponseRow("a"), finalResponseRow("b"), finalResponseRow("c")];
    expect(responseRunEnd(rows, 0)).toBe(3);
  });

  it("is no run for a lone final response", () => {
    expect(responseRunEnd([finalResponseRow("a"), turnEndedRow("e", "t1")], 0)).toBe(1);
  });

  it("is no run when a prompt sits between two final responses", () => {
    const rows = [finalResponseRow("a"), userPromptRow("p", "hi"), finalResponseRow("b")];
    expect(responseRunEnd(rows, 0)).toBe(1);
  });

  it("is no run when a tool card sits between two final responses", () => {
    const rows = [finalResponseRow("a"), toolCallRow("t", "returned"), finalResponseRow("b")];
    expect(responseRunEnd(rows, 0)).toBe(1);
  });

  it("is no run when an errored ending, which draws a marker, sits between", () => {
    const rows = [finalResponseRow("a"), faultEndingRow("e", "t1"), finalResponseRow("b")];
    expect(responseRunEnd(rows, 0)).toBe(1);
  });

  it("is no run when an interrupted ending sits between", () => {
    const rows = [finalResponseRow("a"), turnEndedRow("e", "t1", "interrupted"), finalResponseRow("b")];
    expect(responseRunEnd(rows, 0)).toBe(1);
  });

  it("is no run when a non-final response follows", () => {
    expect(responseRunEnd([finalResponseRow("a"), responseRow("b")], 0)).toBe(1);
  });

  it("is no run from a row that is not a final response", () => {
    expect(responseRunEnd([responseRow("a"), finalResponseRow("b")], 0)).toBe(1);
  });

  it("is no run from a parented final response", () => {
    expect(responseRunEnd([finalResponseRow("a", "x", "c"), finalResponseRow("b")], 0)).toBe(1);
  });

  it("stops before a parented final response", () => {
    const rows = [finalResponseRow("a"), finalResponseRow("b"), finalResponseRow("c", "x", "b")];
    expect(responseRunEnd(rows, 0)).toBe(2);
  });

  it("starts a run mid-feed at the given index", () => {
    const rows = [userPromptRow("p", "hi"), finalResponseRow("a"), finalResponseRow("b")];
    expect(responseRunEnd(rows, 1)).toBe(3);
  });

  it("is no run past the end of the rows", () => {
    expect(responseRunEnd([finalResponseRow("a")], 1)).toBe(2);
  });
});

describe("createResponseRunStore", () => {
  it("wraps the members in one tabless run inside a feed item", () => {
    // Arrange
    const store = createResponseRunStore();
    // Act
    const wrapper = store.arrange(responseRunKey("a"), [member("a"), member("b")]);
    // Assert
    expect(wrapper.classList.contains("feed-item")).toBe(true);
    expect(wrapper.classList.contains(RESPONSE_RUN_ITEM_CLASS)).toBe(true);
    expect(wrapper.querySelectorAll(`:scope > .${RESPONSE_RUN_CLASS}`)).toHaveLength(1);
    expect(wrapper.querySelector(".feed-group-tab, .feed-tab-strip")).toBeNull();
  });

  it("shows every member, in order, none hidden", () => {
    // Arrange
    const store = createResponseRunStore();
    const members = [member("a"), member("b"), member("c")];
    // Act
    const wrapper = store.arrange(responseRunKey("a"), members);
    // Assert
    const run = wrapper.querySelector(`.${RESPONSE_RUN_CLASS}`)!;
    expect([...run.children]).toEqual(members);
    expect(members.every((m) => !m.hidden)).toBe(true);
  });

  it("carries no data-feed-row of its own", () => {
    const wrapper = createResponseRunStore().arrange(responseRunKey("a"), [member("a"), member("b")]);
    expect(wrapper.hasAttribute("data-feed-row")).toBe(false);
  });

  it("reuses the SAME wrapper for a run placed again", () => {
    // Arrange
    const store = createResponseRunStore();
    const a = member("a");
    const b = member("b");
    const first = store.arrange(responseRunKey("a"), [a, b]);
    store.prune();
    // Act
    const again = store.arrange(responseRunKey("a"), [a, b, member("c")]);
    // Assert
    expect(again).toBe(first);
  });

  it("re-attaches no member already in place when the run grows", () => {
    // Arrange
    const store = createResponseRunStore();
    const a = member("a");
    const b = member("b");
    store.arrange(responseRunKey("a"), [a, b]);
    let reattached = 0;
    const observer = new MutationObserver((records) => {
      for (const r of records) for (const n of r.removedNodes) if (n === a || n === b) reattached += 1;
    });
    const run = a.parentElement!;
    observer.observe(run, { childList: true });
    // Act
    store.arrange(responseRunKey("a"), [a, b, member("c")]);
    // Assert
    observer.takeRecords();
    observer.disconnect();
    expect(reattached).toBe(0);
    expect(run.children).toHaveLength(3);
  });

  it("disposes a run not placed since the previous prune, detaching its wrapper", () => {
    // Arrange
    const store = createResponseRunStore();
    const host = document.createElement("div");
    host.append(store.arrange(responseRunKey("a"), [member("a"), member("b")]));
    store.prune();
    // Act
    store.prune();
    // Assert
    expect(host.children).toHaveLength(0);
  });

  it("records each layout at DEBUG with its member count", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    createResponseRunStore().arrange(responseRunKey("a"), [member("a"), member("b")]);
    // Assert
    const record = await forwardedRecord(capture, "feed.response-run.layout");
    expect([record.level.case, record.context]).toEqual(["debug", expect.objectContaining({ members: 2 })]);
  });
});
