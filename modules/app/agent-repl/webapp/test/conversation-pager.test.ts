import { describe, expect, it } from "vitest";
import { ConversationPager, type PageRequest } from "../src/conversation-pager.js";
import { RESYNC_FAILURE_CEILING } from "../src/connect-resync.js";

/**
 * THE PAGER'S BOUNDS ARE THE RESYNC'S BOUNDS.
 *
 * The failure this end has already had once is a history request that goes
 * unanswered, provoking another, forever — an observed command queue 5,069
 * deep. A page is served from a store read of a conversation that may hold a
 * quarter of a million events, so it is exactly that shape, and these cases
 * pin the three bounds that keep it finite.
 */

interface Sent {
  request: PageRequest;
  requestId: string;
  accept: () => void;
  refuse: (cause: string) => void;
}

/** A pager over a controllable transport, with every clock injected. */
function harness(overrides: { fence?: string; workspace?: string } = {}): {
  pager: ConversationPager;
  sent: Sent[];
  advance: (ms: number) => void;
  fence: (next: string) => void;
} {
  const sent: Sent[] = [];
  let nowMs = 1_000;
  let fence = overrides.fence ?? "f1";
  const workspace = overrides.workspace ?? "/ws/a";
  let n = 0;
  const pager = new ConversationPager({
    request: (cursor) => (workspace === "" ? null : { workspace, cursor, limit: 0, fence }),
    send: (request) => {
      n += 1;
      const requestId = `r-${n}`;
      let accept = (): void => {};
      let refuse = (_cause: string): void => {};
      const ack = new Promise<void>((resolve, reject) => {
        accept = resolve;
        refuse = (cause: string) => reject(new Error(cause));
      });
      // A rejection nobody else observes must not become an unhandled
      // rejection in the test runner; the pager's own handler is what acts on
      // it.
      void ack.catch(() => {});
      sent.push({ request, requestId, accept, refuse });
      return { requestId, ack };
    },
    now: () => nowMs,
    random: () => 0,
  });
  return {
    pager,
    sent,
    advance: (ms) => {
      nowMs += ms;
    },
    fence: (next) => {
      fence = next;
    },
  };
}

/** Let the injected acks settle before asserting on what they changed. */
const settle = (): Promise<void> => new Promise((r) => setTimeout(r, 0));

describe("ConversationPager", () => {
  it("the cold open asks for the tail, with no cursor", async () => {
    // Arrange
    const h = harness();
    // Act
    void h.pager.openTail().catch(() => {});
    // Assert — a tail anchor is an EMPTY cursor; a client never invents one.
    expect(h.sent).toHaveLength(1);
    expect(h.sent[0].request.cursor).toBe("");
    expect(h.sent[0].request.workspace).toBe("/ws/a");
  });

  it("load-more sends the daemon's own cursor back byte-for-byte", async () => {
    // Arrange — the token is opaque; this end must not touch it.
    const h = harness();
    // Act
    void h.pager.loadMore("cp1-OPAQUE-TOKEN").catch(() => {});
    // Assert
    expect(h.sent[0].request.cursor).toBe("cp1-OPAQUE-TOKEN");
  });

  it("a second request while one is in flight is DROPPED, not queued", async () => {
    // Arrange — the user clicking a button that is already working. Unlike a
    // resync, a later page request asks a DIFFERENT question, so coalescing it
    // into the outstanding one would answer neither.
    const h = harness();
    void h.pager.openTail().catch(() => {});
    // Act
    await expect(h.pager.loadMore("c1")).rejects.toThrow(/already in flight/);
    // Assert
    expect(h.sent).toHaveLength(1);
  });

  it("a page that ARRIVES frees the next request", async () => {
    // Arrange
    const h = harness();
    void h.pager.openTail().catch(() => {});
    h.sent[0].accept();
    await settle();
    // Act
    h.pager.observePage(h.sent[0].requestId);
    void h.pager.loadMore("c1").catch(() => {});
    // Assert
    expect(h.sent).toHaveLength(2);
    expect(h.sent[1].request.cursor).toBe("c1");
  });

  it("a page that never arrives keeps the single-flight bound", async () => {
    // Arrange — the ACCEPTANCE is not the page. A pager that freed itself on
    // the ack would let every heartbeat mint another request while the read
    // was still running, which is the flood.
    const h = harness();
    void h.pager.openTail().catch(() => {});
    h.sent[0].accept();
    await settle();
    // Act
    await expect(h.pager.loadMore("c1")).rejects.toThrow(/already in flight/);
    // Assert
    expect(h.sent).toHaveLength(1);
  });

  it("a refused request backs off before the next one is allowed", async () => {
    // Arrange
    const h = harness();
    void h.pager.openTail().catch(() => {});
    h.sent[0].refuse("daemon says no");
    await settle();
    // Act
    await expect(h.pager.loadMore("c1")).rejects.toThrow(/backing off/);
    // Assert — and the delay is real: waiting it out lets the next one go.
    h.advance(60_000);
    void h.pager.loadMore("c1").catch(() => {});
    expect(h.sent).toHaveLength(2);
  });

  it("a late refusal for an ACCEPTED page settles it as a failure", async () => {
    // Arrange — the daemon acked at acceptance and the read then failed. This
    // is the only thing that stops the load-more spinning forever against a
    // request that will never come back.
    const h = harness();
    void h.pager.openTail().catch(() => {});
    h.sent[0].accept();
    await settle();
    // Act
    const observed = h.pager.observeRefusal(h.sent[0].requestId, "the store could not be read");
    // Assert
    expect(observed).toBe(true);
    expect(h.pager.isInFlight).toBe(false);
  });

  it("repeated failures reach a ceiling that REPORTS itself", async () => {
    // Arrange — silent spinning is what made the resync flood invisible.
    const reported: string[] = [];
    const h = harness();
    const pager = h.pager;
    // Act — drive the ceiling, waiting out each backoff.
    for (let i = 0; i < RESYNC_FAILURE_CEILING; i += 1) {
      void pager.openTail().catch(() => {});
      h.sent[h.sent.length - 1].refuse(`failure ${i}`);
      await settle();
      h.advance(120_000);
    }
    reported.push(pager.view.givenUp ? "given_up" : "still_asking");
    // Assert
    expect(reported).toEqual(["given_up"]);
    await expect(pager.openTail()).rejects.toThrow(/stopped asking/);
  });

  it("a stale-fence page is re-requested ONCE, for the SAME anchor", async () => {
    // Arrange — a load-more whose answer was minted under a generation this
    // page no longer reads. Re-anchoring to the tail would silently turn the
    // user's load-more into a cold open.
    const h = harness();
    void h.pager.loadMore("c-deep").catch(() => {});
    h.fence("f2");
    // Act
    const retried = h.pager.observeStaleFence(h.sent[0].requestId);
    // Assert
    expect(retried).toBe(true);
    expect(h.sent).toHaveLength(2);
    expect(h.sent[1].request.cursor).toBe("c-deep");
    expect(h.sent[1].request.fence).toBe("f2");
  });

  it("a re-request that is ITSELF stale is not asked a third time", async () => {
    // Arrange — a second stale answer means this end cannot name a current
    // fence from what it has, and a third attempt would be the same guess.
    const h = harness();
    void h.pager.loadMore("c-deep").catch(() => {});
    h.pager.observeStaleFence(h.sent[0].requestId);
    // Act
    const retried = h.pager.observeStaleFence(h.sent[1].requestId);
    // Assert
    expect(retried).toBe(false);
    expect(h.sent).toHaveLength(2);
  });

  it("a fresh socket discharges a request the dead one left in flight", async () => {
    // Arrange — that request can never settle, and holding it would block this
    // connection's cold open forever.
    const h = harness();
    void h.pager.openTail().catch(() => {});
    expect(h.pager.isInFlight).toBe(true);
    // Act
    h.pager.reset();
    void h.pager.openTail().catch(() => {});
    // Assert
    expect(h.sent).toHaveLength(2);
  });

  it("load-more with no cursor is refused rather than read as a tail", async () => {
    // Arrange — a tail read wearing the wrong name would silently discard the
    // user's place in the history.
    const h = harness();
    // Act / Assert
    await expect(h.pager.loadMore("")).rejects.toThrow(/no cursor/);
    expect(h.sent).toHaveLength(0);
  });

  it("a page cannot be asked for before a workspace and fence are known", async () => {
    // Arrange — the daemon looks a workspace up by exact key; an empty one is
    // a loud nack, not a defaulted match.
    const h = harness({ workspace: "" });
    // Act / Assert
    await expect(h.pager.openTail()).rejects.toThrow(/no live workspace/);
    expect(h.sent).toHaveLength(0);
  });
});
