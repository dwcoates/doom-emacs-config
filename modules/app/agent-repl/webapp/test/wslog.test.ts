import { afterEach, describe, expect, it, vi } from "vitest";
import { ClientLogCmd } from "../src/protocol.js";
import {
  FORWARD_WINDOW_MS,
  ForwardingLogger,
  MAX_FORWARDS_PER_WINDOW,
  clearDedup,
  log,
  logDedup,
  resetLoggingForTests,
  setLogger,
} from "../src/wslog.js";

/** A logger wired to spies: captured forwards, captured console lines. */
function spyLogger(sendResult = true): {
  logger: ForwardingLogger;
  forwarded: ClientLogCmd[];
  consoleLines: string[];
  setClock(t: number): void;
} {
  const forwarded: ClientLogCmd[] = [];
  const consoleLines: string[] = [];
  let clock = 0;
  const logger = new ForwardingLogger(
    (cmd) => {
      forwarded.push(cmd);
      return sendResult;
    },
    (level, line) => consoleLines.push(`${level}: ${line}`),
    () => clock,
  );
  return { logger, forwarded, consoleLines, setClock: (t) => (clock = t) };
}

describe("ForwardingLogger", () => {
  it("forwards a line to the daemon and echoes it to the console", () => {
    // Arrange
    const spy = spyLogger();
    // Act
    spy.logger.log("warn", "seq gap: have 3, got 7");
    // Assert
    // Both copies carry the WEBAPP's clock (the spy clock reads 0), stamped
    // once inside the helper: the daemon's own receipt stamp cannot show how
    // long a line took to get there.
    expect(spy.forwarded).toEqual([
      { type: "client-log", level: "warn", message: "00:00:00.000 seq gap: have 3, got 7" },
    ]);
    expect(spy.consoleLines).toEqual(["warn: 00:00:00.000 seq gap: have 3, got 7"]);
  });

  it("still echoes to the console when the socket refuses the forward", () => {
    // Arrange — send reports not-open.
    const spy = spyLogger(false);
    // Act
    spy.logger.log("error", "boom");
    // Assert
    expect(spy.consoleLines).toEqual(["error: 00:00:00.000 boom"]);
  });

  it("suppresses forwards past the per-window cap with one notice", () => {
    // Arrange
    const spy = spyLogger();
    // Act — one more line than the budget allows.
    for (let i = 0; i < MAX_FORWARDS_PER_WINDOW + 1; i++) {
      spy.logger.log("info", `line ${i}`);
    }
    // Assert — cap forwards, then exactly one rate-limit notice.
    expect(spy.forwarded).toHaveLength(MAX_FORWARDS_PER_WINDOW + 1);
    expect(spy.forwarded[MAX_FORWARDS_PER_WINDOW].message).toContain("rate limit");
  });

  it("emits the suppression notice only once per window", () => {
    // Arrange — a runaway loop far past the cap.
    const spy = spyLogger();
    // Act
    for (let i = 0; i < MAX_FORWARDS_PER_WINDOW + 50; i++) {
      spy.logger.log("info", `line ${i}`);
    }
    // Assert
    const notices = spy.forwarded.filter((c) => c.message.includes("rate limit"));
    expect(notices).toHaveLength(1);
  });

  it("keeps the console stream unlimited while forwards are suppressed", () => {
    // Arrange
    const spy = spyLogger();
    // Act
    for (let i = 0; i < MAX_FORWARDS_PER_WINDOW + 50; i++) {
      spy.logger.log("info", `line ${i}`);
    }
    // Assert
    expect(spy.consoleLines).toHaveLength(MAX_FORWARDS_PER_WINDOW + 50);
  });

  it("resets the forwarding budget when the window rolls over", () => {
    // Arrange — a window exhausted to suppression.
    const spy = spyLogger();
    for (let i = 0; i < MAX_FORWARDS_PER_WINDOW + 1; i++) {
      spy.logger.log("info", `line ${i}`);
    }
    // Act — the next line lands in a fresh window.
    spy.setClock(FORWARD_WINDOW_MS);
    spy.logger.log("info", "after rollover");
    // Assert
    expect(spy.forwarded[spy.forwarded.length - 1]).toEqual({
      type: "client-log",
      level: "info",
      message: "00:01:00.000 after rollover",
    });
  });
});

describe("module-level singleton", () => {
  afterEach(() => {
    resetLoggingForTests();
    vi.restoreAllMocks();
  });

  it("routes log() through the installed ForwardingLogger", () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    // Act
    log("warn", "routed");
    // Assert
    expect(spy.consoleLines).toEqual(["warn: 00:00:00.000 routed"]);
  });

  it("falls back to the console before a logger is installed", () => {
    // Arrange
    const warn = vi.spyOn(console, "warn").mockImplementation(() => {});
    // Act
    log("warn", "early line");
    // Assert
    expect(warn).toHaveBeenCalledWith("early line");
  });

  it("logDedup suppresses a repeated message under the same key", () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    // Act — a per-frame guard fires three times with the same error.
    logDedup("feed-render", "error", "boom");
    logDedup("feed-render", "error", "boom");
    logDedup("feed-render", "error", "boom");
    // Assert
    expect(spy.consoleLines).toEqual(["error: 00:00:00.000 boom"]);
  });

  it("logDedup logs again when the message under the key changes", () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    logDedup("k", "warn", "first error");
    // Act
    logDedup("k", "warn", "second error");
    // Assert
    expect(spy.consoleLines).toEqual([
      "warn: 00:00:00.000 first error",
      "warn: 00:00:00.000 second error",
    ]);
  });

  it("logDedup keys are independent", () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    // Act
    logDedup("a", "info", "same");
    logDedup("b", "info", "same");
    // Assert
    expect(spy.consoleLines).toHaveLength(2);
  });

  it("clearDedup re-arms a key for an identical message", () => {
    // Arrange
    const spy = spyLogger();
    setLogger(spy.logger);
    logDedup("poll:t1", "warn", "tail fetch failed");
    // Act — recovery observed, then the same failure returns.
    clearDedup("poll:t1");
    logDedup("poll:t1", "warn", "tail fetch failed");
    // Assert
    expect(spy.consoleLines).toHaveLength(2);
  });
});

describe("structured context (E4 ClientLogCmd.context)", () => {
  it("forwards a context object on the frame", () => {
    // Arrange
    const sent: ClientLogCmd[] = [];
    const logger = new ForwardingLogger((cmd) => {
      sent.push(cmd);
      return true;
    }, () => {});
    // Act
    logger.log("warn", "render stall", { pendingMs: 1200, visibility: "visible" });
    // Assert
    expect(sent).toHaveLength(1);
    expect(sent[0].context).toEqual({ pendingMs: 1200, visibility: "visible" });
  });

  it("omits context entirely when the call site has none", () => {
    // Arrange — an absent Struct must stay absent, not become {}.
    const sent: ClientLogCmd[] = [];
    const logger = new ForwardingLogger((cmd) => {
      sent.push(cmd);
      return true;
    }, () => {});
    // Act
    logger.log("info", "plain line");
    // Assert
    expect(sent[0].context).toBeUndefined();
    expect("context" in sent[0]).toBe(false);
  });

  it("still writes only the message to the console", () => {
    // Arrange — a console line is read by a human; the struct is for the log.
    const lines: string[] = [];
    const logger = new ForwardingLogger(
      () => true,
      (_l, line) => lines.push(line),
      () => 0,
    );
    // Act
    logger.log("warn", "render stall", { pendingMs: 1200 });
    // Assert — the client stamp, and nothing of the struct.
    expect(lines).toEqual(["00:00:00.000 render stall"]);
  });

  it("carries context through the module-level log()", () => {
    // Arrange
    const sent: ClientLogCmd[] = [];
    const logger = new ForwardingLogger((cmd) => {
      sent.push(cmd);
      return true;
    }, () => {});
    setLogger(logger);
    // Act
    log("error", "poll failed", { taskId: "bg1" });
    // Assert
    expect(sent[0].context).toEqual({ taskId: "bg1" });
  });
});

describe("logLocalOnly (the rejected-forward path)", () => {
  it("writes to the console without forwarding", () => {
    // Arrange — forwarding a clientLog-rejection report would earn another
    // rejection and loop, so this path must never send.
    const sent: ClientLogCmd[] = [];
    const lines: string[] = [];
    const logger = new ForwardingLogger(
      (cmd) => {
        sent.push(cmd);
        return true;
      },
      (_l, line) => lines.push(line),
      () => 0,
    );
    // Act
    logger.logLocalOnly("error", "clientLog rejected: no message");
    // Assert
    expect(lines).toEqual(["00:00:00.000 clientLog rejected: no message"]);
    expect(sent).toEqual([]);
  });

  it("stamps the console-only line with the webapp clock", () => {
    // Arrange — a console-only line is still evidence, so it carries the same
    // clock a forwarded line does.
    const lines: string[] = [];
    const logger = new ForwardingLogger(
      () => true,
      (_l, line) => lines.push(line),
      () => 3_723_004,
    );
    // Act
    logger.logLocalOnly("warn", "ack rejected");
    // Assert
    expect(lines).toEqual(["01:02:03.004 ack rejected"]);
  });
});
