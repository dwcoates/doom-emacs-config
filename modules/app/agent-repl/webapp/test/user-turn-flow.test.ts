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
 * - N prompts INGESTED is N prompt bubbles DRAWN, whatever their request ids;
 * - a request-id-bearing prompt still reconciles on that id, so the flows that
 *   DO carry one (fake-mode e2e, optimistic echo) never double up;
 * - a replayed redelivery of a transcript prompt reconciles on its uuid.
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

/** A user prompt that DOES carry a request id — the echo shape. */
function echoedPrompt(uuid: string, requestId: string, text: string): WireItem {
  return { uuid, tsMs: String(TS_MS), requestId, userMessage: { contentString: text } };
}

/**
 * The DAEMON'S OWN prompt receipt: a user turn keyed on the submit's request
 * id, pushed the instant the daemon forwards the prompt. It carries NO store
 * seq — nothing in the store produced it — which is what the `through_seq: 0`
 * on `sendLocal` stands for.
 */
function receipt(requestId: string, text: string): WireItem {
  return {
    uuid: `prompt-echo:${requestId}`,
    tsMs: String(TS_MS),
    requestId,
    userMessage: { contentString: text },
  };
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
          items: items.map((item) => ({ source: "CONVERSATION_SOURCE_USER", ...item })),
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

describe("prompts that DO carry a request id", () => {
  it("reconciles a redelivered echo rather than duplicating it", () => {
    // Arrange — the fake-mode / optimistic-echo shape.
    const f = flow();
    f.send([echoedPrompt("u1", "r1", "echoed prompt")]);
    // Act — the same request id arrives again under a later uuid.
    f.send([echoedPrompt("u2", "r1", "echoed prompt")]);
    // Assert
    expect(f.prompts()).toHaveLength(1);
  });

  it("still draws two bubbles for two distinct request ids", () => {
    // Arrange
    const f = flow();
    // Act
    f.send([echoedPrompt("u1", "r1", "first echo")]);
    f.send([echoedPrompt("u2", "r2", "second echo")]);
    // Assert
    expect(f.prompts()).toHaveLength(2);
  });
});

/**
 * THE DAEMON RECEIPT FLOW, the pipeline as it now runs: the daemon pushes the
 * prompt bubble itself the moment it forwards a submit (seq-less, keyed on the
 * request id), and the transcript's own line follows later carrying a store
 * seq and — stamped by the daemon — the SAME request id.
 */
describe("the daemon's prompt receipt", () => {
  it("draws one bubble when the durable line follows the receipt", () => {
    // Arrange
    const f = flow();
    // Act
    f.sendLocal([receipt("r1", "the prompt itself")]);
    f.send([echoedPrompt("u1", "r1", "the prompt itself")]);
    // Assert — the two deliveries of one prompt reconcile, never double up.
    expect(f.prompts()).toHaveLength(1);
  });

  it("keeps the prompt's text when the durable line replaces the receipt", () => {
    // Arrange
    const f = flow();
    // Act
    f.sendLocal([receipt("r1", "the prompt itself")]);
    f.send([echoedPrompt("u1", "r1", "the prompt itself")]);
    // Assert
    expect(f.container.textContent).toContain("the prompt itself");
  });

  it("shows the prompt even when no durable line ever arrives", () => {
    // Arrange — the shim died between the submit and the transcript write.
    const f = flow();
    // Act
    f.sendLocal([receipt("r1", "the prompt itself")]);
    // Assert — the receipt is the only evidence the prompt was sent.
    expect(f.prompts()).toHaveLength(1);
  });

  it("ranks the receipt at the feed TAIL, not above the history", () => {
    // Arrange — history first, then a fresh submit.
    const f = flow();
    f.send([assistantText("a1", "an earlier answer")]);
    // Act
    f.sendLocal([receipt("r1", "the prompt itself")]);
    // Assert — a seq-less push carries no rank of its own; ranking it at 0
    // would file the user's newest prompt ABOVE everything they have read.
    const last = f.bubbles()[f.bubbles().length - 1];
    expect(last.classList.contains("user")).toBe(true);
  });

  it("leaves the reconciled bubble where the receipt put it", () => {
    // Arrange
    const f = flow();
    f.send([assistantText("a1", "an earlier answer")]);
    f.sendLocal([receipt("r1", "the prompt itself")]);
    // Act — the durable line lands at its own store seq.
    f.send([echoedPrompt("u1", "r1", "the prompt itself")]);
    // Assert — a redelivery replaces content, never position.
    const last = f.bubbles()[f.bubbles().length - 1];
    expect(last.classList.contains("user")).toBe(true);
  });
});
