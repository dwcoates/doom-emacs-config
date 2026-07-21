import { afterEach, describe, expect, it, vi } from "vitest";
import { FetchTail, TaskTailResponse, WatcherPoller } from "../src/watcher-poll.js";
import { ForwardingLogger, resetLoggingForTests, setLogger } from "../src/wslog.js";

/** A logger wired to spies: captured console lines, no forwarding. */
function spyLogger(): { logger: ForwardingLogger; consoleLines: string[] } {
  const consoleLines: string[] = [];
  const logger = new ForwardingLogger(
    () => true,
    (level, line) => consoleLines.push(`${level}: ${line}`),
    () => 0,
  );
  return { logger, consoleLines };
}

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

describe("WatcherPoller.poll failure logging", () => {
  afterEach(() => {
    resetLoggingForTests();
  });

  it("logs a failing task-tail fetch exactly once across repeated failures", async () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    const fetch: FetchTail = async () => {
      throw new Error("network");
    };
    const poller = new WatcherPoller(fetch, () => {});
    // Act — the 1s loop retries from the same offset every tick.
    await poller.poll("a");
    await poller.poll("a");
    await poller.poll("a");
    // Assert
    const warnLines = spy.consoleLines.filter((l) => l.startsWith("warn:"));
    expect(warnLines).toHaveLength(1);
    expect(warnLines[0]).toContain("a");
    expect(warnLines[0]).toContain("network");
  });

  it("logs again when the failure's error message changes", async () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    let calls = 0;
    const fetch: FetchTail = async () => {
      calls++;
      throw new Error(calls === 1 ? "network" : "timeout");
    };
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    await poller.poll("a");
    // Assert
    const warnLines = spy.consoleLines.filter((l) => l.startsWith("warn:"));
    expect(warnLines).toHaveLength(2);
    expect(warnLines[0]).toContain("network");
    expect(warnLines[1]).toContain("timeout");
  });

  it("logs one recovery line after a failure and re-arms the dedup key", async () => {
    // Arrange — fail once, then succeed, then fail again with the same message.
    const spy = spyLogger();
    setLogger(spy.logger);
    let calls = 0;
    const fetch: FetchTail = async (_id, offset) => {
      calls++;
      if (calls === 1 || calls === 3) throw new Error("network");
      return { text: "ok", offset: offset + 2, done: false, elapsed_ms: 0 };
    };
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a"); // fails
    await poller.poll("a"); // succeeds -> recovery
    await poller.poll("a"); // fails again, same message as before
    // Assert
    const infoLines = spy.consoleLines.filter((l) => l.startsWith("info:"));
    const warnLines = spy.consoleLines.filter((l) => l.startsWith("warn:"));
    expect(infoLines).toHaveLength(1);
    expect(infoLines[0]).toContain("a");
    // The dedup key re-armed on recovery, so the identical error logs again.
    expect(warnLines).toHaveLength(2);
  });

  it("stays silent on steady-state success with no prior failure", async () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    const { fetch } = scriptedFetch([{ text: "a" }, { text: "b" }]);
    const poller = new WatcherPoller(fetch, () => {});
    // Act
    await poller.poll("a");
    await poller.poll("a");
    // Assert — no recovery noise absent a preceding failure.
    expect(spy.consoleLines).toHaveLength(0);
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
