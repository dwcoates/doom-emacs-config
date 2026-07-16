import { describe, expect, it, vi } from "vitest";
import { FetchTail, TaskTailResponse, WatcherPoller } from "../src/watcher-poll.js";

/** A fetchTail returning the queued chunks in order, recording its offsets. */
function scriptedFetch(chunks: Array<Partial<TaskTailResponse>>): {
  fetch: FetchTail;
  offsets: number[];
} {
  const offsets: number[] = [];
  let i = 0;
  const fetch: FetchTail = async (_id, offset) => {
    offsets.push(offset);
    const c = chunks[Math.min(i, chunks.length - 1)];
    i++;
    const text = c.text ?? "";
    return {
      text,
      offset: c.offset ?? offset + text.length,
      done: c.done ?? false,
      elapsed_ms: c.elapsed_ms ?? 0,
    };
  };
  return { fetch, offsets };
}

describe("WatcherPoller.poll", () => {
  it("accumulates text across polls", async () => {
    // Arrange
    const { fetch } = scriptedFetch([{ text: "ab" }, { text: "cd" }]);
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    await poller.poll("a");
    // Assert
    expect(poller.tail("a")?.text).toBe("abcd");
  });

  it("advances the offset it sends by the bytes it received", async () => {
    // Arrange
    const { fetch, offsets } = scriptedFetch([{ text: "abc" }, { text: "de" }]);
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    await poller.poll("a");
    // Assert — second poll resumes at the first's next cursor.
    expect(offsets).toEqual([0, 3]);
  });

  it("stops re-polling a task once it reports done", async () => {
    // Arrange
    const { fetch, offsets } = scriptedFetch([{ text: "x", done: true }]);
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    await poller.poll("a");
    // Assert — the second poll returned early on the cached done tail.
    expect(offsets).toEqual([0]);
    expect(poller.tail("a")?.done).toBe(true);
  });

  it("exposes the elapsed the daemon reported", async () => {
    // Arrange
    const { fetch } = scriptedFetch([{ text: "x", elapsed_ms: 4200 }]);
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    // Assert
    expect(poller.tail("a")?.elapsedMs).toBe(4200);
  });

  it("survives a failed fetch and retries from the same offset", async () => {
    // Arrange — first fetch throws, second succeeds.
    let calls = 0;
    const offsets: number[] = [];
    const fetch: FetchTail = async (_id, offset) => {
      offsets.push(offset);
      calls++;
      if (calls === 1) throw new Error("network");
      return { text: "ok", offset: offset + 2, done: false, elapsed_ms: 0 };
    };
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    await poller.poll("a");
    // Assert — the failed poll left the cursor at 0 for the retry.
    expect(offsets).toEqual([0, 0]);
    expect(poller.tail("a")?.text).toBe("ok");
  });

  it("fires the update callback after a successful poll", async () => {
    // Arrange
    const { fetch } = scriptedFetch([{ text: "x" }]);
    const onUpdate = vi.fn();
    const poller = new WatcherPoller(fetch, onUpdate);
    // Act
    await poller.poll("a");
    // Assert
    expect(onUpdate).toHaveBeenCalledOnce();
  });
});

describe("WatcherPoller.sync", () => {
  it("runs the interval only while at least one id is active", () => {
    // Arrange — a fetch that never resolves, so no accumulation noise.
    const poller = new WatcherPoller(() => new Promise<TaskTailResponse>(() => {}), () => {});
    // Act + Assert
    poller.sync(new Set(["a"]));
    expect(poller.pollingActive()).toBe(true);
    poller.sync(new Set());
    expect(poller.pollingActive()).toBe(false);
    poller.dispose();
  });

  it("does not resume polling for an already-done task", async () => {
    // Arrange — a task polled to completion.
    const { fetch } = scriptedFetch([{ text: "x", done: true }]);
    const poller = new WatcherPoller(fetch, () => {});
    await poller.poll("a");
    // Act — reopening its fold offers it back as active.
    poller.sync(new Set(["a"]));
    // Assert — a done task is never re-polled, so the interval stays idle.
    expect(poller.pollingActive()).toBe(false);
    poller.dispose();
  });
});
