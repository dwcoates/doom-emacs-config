import { describe, expect, it } from "vitest";

import { PromptQueue, drainableRenderState, type QueuedPrompt } from "../src/prompt-queue.js";
import { PromptOrigin } from "../src/frontend-command.js";
import type { WebRenderState } from "../src/state-adapter.js";

const WS = "/w/one";

interface Harness {
  queue: PromptQueue;
  submitted: string[];
  failures: Array<{ text: string; reason: string }>;
  deadlines: Array<{ fn: () => void; ms: number }>;
  setLinkDown: (down: boolean) => void;
  setRevived: (revived: boolean) => void;
  advance: (ms: number) => void;
  /** Settle the submit for the oldest un-settled drained prompt. */
  settle: (verdict: "accept" | Error) => Promise<void>;
}

function harness(options: { autoSubmit?: boolean; revivalBoundMs?: number } = {}): Harness {
  const submitted: string[] = [];
  const failures: Array<{ text: string; reason: string }> = [];
  const deadlines: Array<{ fn: () => void; ms: number }> = [];
  const settlers: Array<(verdict: "accept" | Error) => void> = [];
  let linkDown = true;
  let revived = false;
  let clock = 1_000;

  const queue = new PromptQueue({
    linkDown: () => linkDown,
    revived: () => revived,
    submit: (e: QueuedPrompt) => {
      submitted.push(e.text);
      if (options.autoSubmit !== false) return Promise.resolve();
      return new Promise<void>((resolve, reject) => {
        settlers.push((verdict) => (verdict === "accept" ? resolve() : reject(verdict)));
      });
    },
    fail: (e: QueuedPrompt, reason: string) => failures.push({ text: e.text, reason }),
    now: () => clock,
    schedule: (fn, ms) => {
      deadlines.push({ fn, ms });
    },
    ...(options.revivalBoundMs !== undefined ? { revivalBoundMs: options.revivalBoundMs } : {}),
  });

  return {
    queue,
    submitted,
    failures,
    deadlines,
    setLinkDown: (down) => {
      linkDown = down;
    },
    setRevived: (r) => {
      revived = r;
    },
    advance: (ms) => {
      clock += ms;
    },
    settle: async (verdict) => {
      const next = settlers.shift();
      if (next === undefined) throw new Error("no submit is awaiting a verdict");
      next(verdict);
      await Promise.resolve();
      await Promise.resolve();
    },
  };
}

describe("drainableRenderState", () => {
  it("refuses a severed workspace", () => {
    expect(drainableRenderState("severed")).toBe(false);
  });

  it("refuses a hibernated workspace", () => {
    expect(drainableRenderState("hibernated")).toBe(false);
  });

  it("refuses a workspace with no applied WorkspaceState", () => {
    expect(drainableRenderState(null)).toBe(false);
  });

  it("allows a ready workspace", () => {
    expect(drainableRenderState("ready" as WebRenderState)).toBe(true);
  });
});

describe("PromptQueue.offer", () => {
  it("takes a prompt while the link is down", () => {
    // Arrange
    const h = harness();
    // Act
    const taken = h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    // Assert
    expect(taken).toBe(true);
  });

  it("declines a prompt while the link is up and nothing is held", () => {
    // Arrange
    const h = harness();
    h.setLinkDown(false);
    // Act
    const taken = h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    // Assert
    expect(taken).toBe(false);
  });

  it("files no feed item for the held prompt", () => {
    // Arrange — a held prompt used to be drawn as a pending bubble. A prompt
    // renders when it round-trips through the SDK and never before, so the
    // queue has nothing to draw with.
    const h = harness();
    // Act
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    // Assert — it is held, and only held.
    expect(h.queue.pending(WS).map((e) => e.text)).toEqual(["first"]);
  });

  it("keeps a workspace's held prompts in submission order", () => {
    // Arrange
    const h = harness();
    // Act
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    // Assert
    expect(h.queue.pending(WS).map((e) => e.text)).toEqual(["first", "second"]);
  });

  it("holds a prompt for one workspace without holding it for another", () => {
    // Arrange
    const h = harness();
    // Act
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    // Assert
    expect(h.queue.pending("/w/two")).toEqual([]);
  });

  it("takes a later prompt while the link is up but an earlier one is still held", () => {
    // Arrange
    const h = harness();
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.setLinkDown(false);
    // Act
    const taken = h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    // Assert
    expect(taken).toBe(true);
  });
});

describe("PromptQueue.drain", () => {
  it("sends the held prompts in order once the session revives", async () => {
    // Arrange
    const h = harness();
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    h.setLinkDown(false);
    h.setRevived(true);
    // Act
    await h.queue.drain(WS);
    // Assert
    expect(h.submitted).toEqual(["first", "second"]);
  });

  it("empties the queue after a full drain", async () => {
    // Arrange
    const h = harness();
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.setRevived(true);
    // Act
    await h.queue.drain(WS);
    // Assert
    expect(h.queue.pending(WS)).toEqual([]);
  });

  it("sends nothing into a workspace that has not revived", async () => {
    // Arrange
    const h = harness();
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.setLinkDown(false);
    // Act — the socket is back but the workspace is still unwired
    await h.queue.drain(WS);
    // Assert
    expect(h.submitted).toEqual([]);
  });

  it("keeps the prompt held when the workspace has not revived", async () => {
    // Arrange
    const h = harness();
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.setLinkDown(false);
    // Act
    await h.queue.drain(WS);
    // Assert
    expect(h.queue.pending(WS).map((e) => e.text)).toEqual(["first"]);
  });

  it("does nothing when the workspace holds no prompts", async () => {
    // Arrange
    const h = harness();
    h.setRevived(true);
    // Act
    await h.queue.drain(WS);
    // Assert
    expect(h.submitted).toEqual([]);
  });

  it("stops the drain when the link drops midway", async () => {
    // Arrange
    const h = harness({ autoSubmit: false });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    h.setRevived(true);
    const drain = h.queue.drain(WS);
    // Act — the first submit lands, then the workspace goes away again
    h.setRevived(false);
    await h.settle("accept");
    await drain;
    // Assert
    expect(h.submitted).toEqual(["first"]);
  });

  it("leaves the undrained remainder held when the link drops midway", async () => {
    // Arrange
    const h = harness({ autoSubmit: false });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    h.setRevived(true);
    const drain = h.queue.drain(WS);
    // Act
    h.setRevived(false);
    await h.settle("accept");
    await drain;
    // Assert
    expect(h.queue.pending(WS).map((e) => e.text)).toEqual(["second"]);
  });
});

describe("PromptQueue drain failure", () => {
  it("surfaces the refusal for the prompt that was refused", async () => {
    // Arrange
    const h = harness({ autoSubmit: false });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.setRevived(true);
    const drain = h.queue.drain(WS);
    // Act
    await h.settle(new Error("submitPrompt was refused: no session"));
    await drain;
    // Assert
    expect(h.failures).toEqual([
      { text: "first", reason: "submitPrompt was refused: no session" },
    ]);
  });

  it("still sends the prompts behind a refused one", async () => {
    // Arrange
    const h = harness({ autoSubmit: false });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    h.setRevived(true);
    const drain = h.queue.drain(WS);
    // Act
    await h.settle(new Error("refused"));
    await h.settle("accept");
    await drain;
    // Assert
    expect(h.submitted).toEqual(["first", "second"]);
  });
});

describe("PromptQueue revival bound", () => {
  it("fails a held prompt whose workspace never revived", async () => {
    // Arrange
    const h = harness({ revivalBoundMs: 60_000 });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.advance(60_000);
    // Act — the armed deadline fires while the workspace is still unwired
    const deadline = h.deadlines[0];
    expect(deadline?.ms).toBe(60_000);
    deadline?.fn();
    await Promise.resolve();
    // Assert
    expect(h.failures.map((f) => f.text)).toEqual(["first"]);
  });

  it("names the bound in the failure it surfaces", async () => {
    // Arrange
    const h = harness({ revivalBoundMs: 60_000 });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.advance(60_000);
    // Act
    h.deadlines[0]?.fn();
    await Promise.resolve();
    // Assert
    expect(h.failures[0]?.reason).toContain("did not come back within 60s");
  });

  it("leaves a younger held prompt alone when an older one expires", async () => {
    // Arrange
    const h = harness({ revivalBoundMs: 60_000 });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.advance(30_000);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    h.advance(30_000);
    // Act — the FIRST entry's deadline fires; the second is only halfway there
    h.deadlines[0]?.fn();
    await Promise.resolve();
    // Assert
    expect(h.queue.pending(WS).map((e) => e.text)).toEqual(["second"]);
  });

  it("arms a deadline per held prompt", () => {
    // Arrange
    const h = harness({ revivalBoundMs: 60_000 });
    // Act
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.queue.offer(WS, "second", PromptOrigin.WEBAPP_USER_SENT);
    // Assert
    expect(h.deadlines.length).toBe(2);
  });

  it("sends rather than expires when the workspace revived before the deadline", async () => {
    // Arrange
    const h = harness({ revivalBoundMs: 60_000 });
    h.queue.offer(WS, "first", PromptOrigin.WEBAPP_USER_SENT);
    h.advance(60_000);
    h.setRevived(true);
    // Act
    h.deadlines[0]?.fn();
    await Promise.resolve();
    // Assert
    expect(h.failures).toEqual([]);
  });
});
