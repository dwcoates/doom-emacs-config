// @vitest-environment jsdom
/**
 * User prompts, END TO END: protojson `ConversationDelta` frames →
 * `StateAdapter` → `ConversationStore` → `FeedRenderer` → the DOM.
 *
 * The defect these exist for: the REAL pipeline delivers a prompt as a
 * transcript user line off the file plane, and that line's request id is
 * EMPTY on every live push. The store keyed a user turn on that id alone, so
 * every prompt after the first reconciled onto the first one — 70 prompts
 * ingested, one bubble on screen, and the reader never saw what they sent.
 *
 * The stage-crossing contract pinned here:
 *
 * - a prompt reaches the feed ONLY when its durable line round-trips, never at
 *   submit time — the optimistic bubble and its reconciliation are gone, and
 *   with them the two identities one prompt used to have;
 * - N prompts INGESTED is N prompt bubbles DRAWN, whatever their request ids;
 * - a replayed redelivery of a prompt reconciles on its uuid, which is the
 *   only identity a prompt has.
 */
import { describe, expect, it } from "vitest";

import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { Actions, FeedRenderer } from "../src/render.js";
import { StateAdapter, userTurnReceipt } from "../src/state-adapter.js";
import { ConversationStore } from "../src/store.js";

/** One protojson `ConversationItem`, as it rides inside a `ConversationDelta`. */
type WireItem = Record<string, unknown>;

/** When every item in these streams was stamped. */
const TS_MS = Date.parse("2026-07-27T09:00:00.000Z");

/** A feed renderer's action sink: these tests assert on markup, never click. */
const FLOW_ACTIONS: Actions = {
  decidePermission() {},
  answerQuestions() {},
  cancelQueued() {},
  runQueuedNow() {},
  acceptQueued() {},
};

/**
 * A user prompt as the REAL pipeline sends it: a transcript user line with a
 * record uuid and NO request id.
 */
function transcriptPrompt(uuid: string, text: string): WireItem {
  return { uuid, tsMs: String(TS_MS), userMessage: { contentString: text } };
}

/**
 * A user prompt whose envelope carries a request id. It is a vendor API
 * correlation and NOTHING keys on it: two prompts sharing one are still two
 * prompts.
 */
function echoedPrompt(uuid: string, requestId: string, text: string): WireItem {
  return { uuid, tsMs: String(TS_MS), requestId, userMessage: { contentString: text } };
}

/** One assistant text block, so a test can see where a prompt RANKS. */
function assistantText(uuid: string, text: string): WireItem {
  return { uuid, tsMs: String(TS_MS), assistantMessage: { id: uuid, content: [{ text: { text } }] } };
}

interface Flow {
  container: HTMLElement;
  /** How many arriving batches carried a user turn (the ingest receipt). */
  ingested: number;
  /** Fold one `ConversationDelta` carrying ITEMS onto the store, then paint. */
  send(items: readonly WireItem[]): void;
  /**
   * Fold a DAEMON-COMPOSED delta — `through_seq: 0`, the shape every seq-less
   * daemon push has (permission cards, failure cards, prompt receipts).
   */
  sendLocal(items: readonly WireItem[]): void;
  /** The prompt bubbles currently on screen. */
  prompts(): HTMLElement[];
  /** Every feed bubble on screen, in DOM order. */
  bubbles(): HTMLElement[];
}

function flow(): Flow {
  const container = document.createElement("div");
  const feed = new FeedRenderer(container, FLOW_ACTIONS);
  const store = new ConversationStore();
  const adapter = new StateAdapter();
  let seq = 0;
  function deliver(throughSeq: number, items: readonly WireItem[]): void {
    const frame = decodeFrontendFrame(
      JSON.stringify({
        conversationDelta: {
          workspace: "/ws",
          fence: "s1",
          throughSeq: String(throughSeq),
          // The daemon stamps a provenance on every item it builds; the
          // adapter's provenance gate refuses an envelope without one.
          messages: items.map((item) => ({ source: "CONVERSATION_SOURCE_USER", ...item })),
        },
      }),
    );
    const effects = adapter.apply(frame);
    // Read BEFORE ingest, exactly as main.ts does — ingesting advances it.
    if (userTurnReceipt(effects, store.state.lastSeq) !== null) self.ingested += 1;
    store.ingest(effects);
    feed.render(store.state);
  }
  const self: Flow = {
    container,
    ingested: 0,
    send(items) {
      seq += items.length;
      deliver(seq, items);
    },
    sendLocal(items) {
      deliver(0, items);
    },
    prompts() {
      return [...container.querySelectorAll<HTMLElement>(".bubble.user")];
    },
    bubbles() {
      return [...container.querySelectorAll<HTMLElement>(".bubble")];
    },
  };
  return self;
}

describe("live prompts with no request id", () => {
  it("draws two consecutive transcript prompts as two bubbles", () => {
    // Arrange
    const f = flow();
    // Act — two live pushes, distinct uuids, both request-id-less.
    f.send([transcriptPrompt("u1", "first prompt")]);
    f.send([transcriptPrompt("u2", "second prompt")]);
    // Assert
    expect(f.prompts()).toHaveLength(2);
  });

  it("keeps both prompts' text on screen", () => {
    // Arrange
    const f = flow();
    // Act
    f.send([transcriptPrompt("u1", "first prompt")]);
    f.send([transcriptPrompt("u2", "second prompt")]);
    // Assert — the earlier prompt was not overwritten by the later one.
    expect(f.container.textContent).toContain("first prompt");
  });

  it("draws every one of a long live run — N ingested is N rendered", () => {
    // Arrange — the regression exactly as observed in production.
    const f = flow();
    const n = 12;
    // Act
    for (let i = 0; i < n; i++) f.send([transcriptPrompt(`u${i}`, `prompt ${i}`)]);
    // Assert
    expect(f.prompts()).toHaveLength(f.ingested);
  });

  it("reconciles a replayed redelivery onto the same bubble", () => {
    // Arrange — a resync replays a transcript line already ingested.
    const f = flow();
    f.send([transcriptPrompt("u1", "only prompt")]);
    // Act
    f.send([transcriptPrompt("u1", "only prompt")]);
    // Assert — one bubble, not a duplicate.
    expect(f.prompts()).toHaveLength(1);
  });
});

describe("the request id names no prompt", () => {
  it("draws two bubbles for two prompts sharing one request id", () => {
    // Arrange — the request id is a vendor API correlation, not an identity.
    // Keying on it collapsed distinct prompts onto one bubble.
    const f = flow();
    // Act
    f.send([echoedPrompt("u1", "r1", "first")]);
    f.send([echoedPrompt("u2", "r1", "second")]);
    // Assert
    expect(f.prompts()).toHaveLength(2);
  });

  it("draws two bubbles for two prompts whose request ids are both empty", () => {
    // Arrange — THE COLLAPSE DEFECT: an empty string used to act as a valid
    // key, so every prompt the real pipeline delivers shared one bucket.
    const f = flow();
    // Act
    f.send([transcriptPrompt("u1", "continue")]);
    f.send([transcriptPrompt("u2", "continue")]);
    // Assert
    expect(f.prompts()).toHaveLength(2);
  });
});

/**
 * PART 2 OF THE FROZEN CONTRACT, walked end to end: a prompt renders when it
 * round-trips through the SDK, and not before. The webapp files nothing at
 * submit time, so there is no second identity to reconcile and no duplicate to
 * collapse.
 */
describe("a prompt renders only after its round trip", () => {
  it("draws no bubble before the durable line arrives", () => {
    // Arrange — a live feed with a submit outstanding: nothing in the submit
    // path touches the store, so the feed has seen nothing at all.
    const f = flow();
    // Act — no delivery.
    // Assert
    expect(f.prompts()).toHaveLength(0);
  });

  it("draws exactly one bubble once the durable line arrives", () => {
    // Arrange
    const f = flow();
    // Act
    f.send([transcriptPrompt("u1", "the prompt itself")]);
    // Assert
    expect(f.prompts()).toHaveLength(1);
  });

  it("carries the prompt's text on that one bubble", () => {
    // Arrange
    const f = flow();
    // Act
    f.send([transcriptPrompt("u1", "the prompt itself")]);
    // Assert
    expect(f.container.textContent).toContain("the prompt itself");
  });

  it("draws one bubble when a resync replays the prompt", () => {
    // Arrange — the durable line, then a reconnect replaying it.
    const f = flow();
    f.send([transcriptPrompt("u1", "the prompt itself")]);
    // Act
    f.send([transcriptPrompt("u1", "the prompt itself")]);
    // Assert
    expect(f.prompts()).toHaveLength(1);
  });

  it("ranks the prompt at the feed TAIL, below the history it follows", () => {
    // Arrange — history first, then the prompt.
    const f = flow();
    f.send([assistantText("a1", "an earlier answer")]);
    // Act
    f.send([transcriptPrompt("u1", "the prompt itself")]);
    // Assert
    const last = f.bubbles()[f.bubbles().length - 1];
    expect(last.classList.contains("user")).toBe(true);
  });
});
