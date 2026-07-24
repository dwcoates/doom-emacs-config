import { describe, expect, it } from "vitest";
import { QueuedCard, queuedCardKey } from "../src/render.js";
import { QueuedItem } from "../src/protocol.js";
import { META_CLOSE, META_OPEN } from "../src/meta.js";

/** A queued item, classifying unless overridden. */
function queued(over: Partial<QueuedItem> = {}): QueuedItem {
  return {
    queue_id: "q1",
    request_id: "r1",
    content: [{ type: "text", text: "run this later" }],
    status: "classifying",
    ...over,
  };
}

describe("QueuedCard", () => {
  it("renders the queued message text", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued())).toContain("run this later");
  });

  it("is a subdued queued-card, not a live user bubble", () => {
    // Arrange + Act — the whole point: a parked message must read as distinct
    // from a live turn.
    const html = QueuedCard(queued());
    // Assert
    expect(html).toContain("queued-card");
    expect(html).not.toContain("bubble user");
  });

  it("labels a pre-verdict item as classifying", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued({ status: "classifying" }))).toContain("queued — classifying…");
  });

  it("labels a wait verdict as waiting", () => {
    // Arrange + Act + Assert
    expect(
      QueuedCard(queued({ status: "waiting", verdict: "wait", reason: "unrelated" })),
    ).toContain("queued — waiting");
  });

  it("labels an interrupt verdict as interrupting", () => {
    // Arrange + Act
    const html = QueuedCard(queued({ status: "interrupt", verdict: "interrupt", reason: "same file" }));
    // Assert — the escalating verdict carries its own badge class for the loud colour.
    expect(html).toContain("interrupting…");
    expect(html).toContain("queued-badge interrupt");
  });

  it("shows the classifier reason once known", () => {
    // Arrange + Act + Assert
    expect(
      QueuedCard(queued({ status: "waiting", verdict: "wait", reason: "touches other files" })),
    ).toContain("touches other files");
  });

  it("omits the reason line before a verdict lands", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued({ status: "classifying" }))).not.toContain("queued-reason");
  });

  it("carries the queue_id on the cancel button for delegated handling", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued({ queue_id: "abc" }))).toContain(`data-queue-cancel="abc"`);
  });

  it("carries the queue_id on the run-now button for delegated handling", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued({ queue_id: "abc" }))).toContain(`data-queue-run-now="abc"`);
  });

  it("strips harness-injected meta spans, as a user turn does", () => {
    // Arrange
    const item = queued({
      content: [{ type: "text", text: `${META_OPEN}read the metaprompt${META_CLOSE}real ask` }],
    });
    // Act
    const html = QueuedCard(item);
    // Assert
    expect(html).toContain("real ask");
    expect(html).not.toContain("read the metaprompt");
  });

  it("escapes markup in the message text", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued({ content: [{ type: "text", text: "<img src=x>" }] }))).not.toContain(
      "<img",
    );
  });

  it("escapes markup in the queue_id it puts in a data attribute", () => {
    // Arrange + Act + Assert
    expect(QueuedCard(queued({ queue_id: `"><img>` }))).not.toContain("<img>");
  });
});

describe("queuedCardKey", () => {
  it("keys a queued card by its queue_id", () => {
    // Arrange + Act + Assert
    expect(queuedCardKey(queued({ queue_id: "q7" }))).toBe("queued:q7");
  });
});
