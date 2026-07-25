/**
 * The held-prompt queue card (E4): the subdued affordance for a prompt the
 * DAEMON is holding because a turn was already running. One edge per test.
 */
import { describe, expect, it } from "vitest";
import { QueuedCard, queuedCardKey } from "../src/render.js";
import { QueuedItem } from "../src/protocol.js";

/** A held prompt, still being classified unless overridden. */
function queued(over: Partial<QueuedItem> = {}): QueuedItem {
  return {
    id: "q1",
    text: "run this later",
    queuedAtMs: 1_700_000_000_000,
    classification: "pending",
    rationale: "",
    accepted: false,
    ...over,
  };
}

describe("QueuedCard", () => {
  it("renders the held prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).toContain("run this later");
  });

  it("is a subdued queued-card, not a live user bubble", () => {
    // Arrange / Act — the whole point: a held prompt must read as distinct
    // from a message that actually reached the agent.
    const html = QueuedCard(queued());
    // Assert
    expect(html).toContain("queued-card");
    expect(html).not.toContain("bubble user");
  });

  it("labels a pre-verdict entry as classifying", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "pending" }))).toContain("queued — classifying…");
  });

  it("labels a hold verdict by when it will run", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold" }))).toContain(
      "queued — will run after this turn",
    );
  });

  it("labels an interject verdict as interrupting", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "interject" }))).toContain("interrupting…");
  });

  it("labels an error entry as unclassified, never as a verdict", () => {
    // Arrange / Act — ERROR means NOTHING decided this; it must not read as a
    // hold the classifier actually chose.
    const html = QueuedCard(queued({ classification: "error" }));
    // Assert
    expect(html).toContain("queued — unclassified");
    expect(html).not.toContain("will run after this turn");
  });

  it("shows the classifier's reason once known", () => {
    // Arrange / Act / Assert
    expect(
      QueuedCard(queued({ classification: "hold", rationale: "independent of the running turn" })),
    ).toContain("independent of the running turn");
  });

  it("omits the reason line before a verdict lands", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).not.toContain("queued-reason");
  });

  it("carries the entry id on the cancel button for delegated handling", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ id: "q_abc" }))).toContain('data-queue-cancel="q_abc"');
  });

  it("carries the entry id on the run-now button for delegated handling", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ id: "q_abc" }))).toContain('data-queue-run-now="q_abc"');
  });

  it("offers accept once a verdict has landed", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold", id: "q_abc" }))).toContain(
      'data-queue-accept="q_abc"',
    );
  });

  it("offers no accept while the entry is still pending", () => {
    // Arrange / Act — there is nothing to confirm about a verdict that has not
    // been reached.
    expect(QueuedCard(queued({ classification: "pending" }))).not.toContain("data-queue-accept");
  });

  it("offers no accept on an already-accepted entry", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold", accepted: true }))).not.toContain(
      "data-queue-accept",
    );
  });

  it("marks an accepted entry as accepted", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold", accepted: true }))).toContain(
      "queued-accepted",
    );
  });

  it("escapes markup in the prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ text: "<img src=x onerror=1>" }))).not.toContain("<img");
  });

  it("escapes markup in the entry id it puts in a data attribute", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ id: `"><img>` }))).not.toContain("<img>");
  });
});

describe("queuedCardKey", () => {
  it("keys a queued card by its entry id", () => {
    // Arrange / Act / Assert
    expect(queuedCardKey(queued({ id: "q7" }))).toBe("queued:q7");
  });
});
