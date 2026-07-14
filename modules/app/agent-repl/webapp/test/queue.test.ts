import { describe, expect, it } from "vitest";
import { QueuedCard, queuedCardKey, renderItem } from "../src/render.js";
import { QueuedItem } from "../src/protocol.js";
import { META_CLOSE, META_OPEN } from "../src/meta.js";
import { ConversationStore, UserTurnItem } from "../src/store.js";

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

describe("draining a queued card into a user turn", () => {
  it("drops the queued card and renders the drained message as a user bubble", () => {
    // Arrange — drive the store through a drain: a turn in flight, a message
    // queued behind it, then the turn ends and the daemon drains the queue.
    const store = new ConversationStore();
    let seq = 0;
    const send = (type: string, f: Record<string, unknown> = {}): void => {
      store.applyRaw(JSON.stringify({ type, seq: seq++, ts: "T", session_id: "s1", ...f }));
    };
    send("hello", {
      daemon_version: "0",
      resume_from_seq: 0,
      permission_mode: "default",
      model: "m",
      cwd: "/w",
    });
    send("user-turn", { request_id: "r1", content: [{ type: "text", text: "first" }] });
    send("queue-added", {
      queue_id: "q1",
      request_id: "r2",
      content: [{ type: "text", text: "queued ask" }],
      status: "classifying",
    });
    send("result", {
      subtype: "success",
      duration_ms: 1,
      duration_api_ms: 1,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 1, output_tokens: 1 },
      is_error: false,
    });
    // Act — the daemon drains: remove the parked item (reason "drained",
    // carrying the request_id) then broadcast the user-turn it became.
    send("queue-removed", { queue_id: "q1", reason: "drained", request_id: "r2" });
    send("user-turn", { request_id: "r2", content: [{ type: "text", text: "queued ask" }] });
    // Assert — no queued cards remain, and the message is now a user bubble.
    expect(store.state.queued).toHaveLength(0);
    const drained = store.state.items.find(
      (i): i is UserTurnItem => i.kind === "user-turn" && i.requestId === "r2",
    );
    expect(drained).toBeDefined();
    const html = renderItem(drained as UserTurnItem);
    expect(html).toContain("bubble user");
    expect(html).toContain("queued ask");
  });
});
