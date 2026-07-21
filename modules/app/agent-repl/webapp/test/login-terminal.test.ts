// @vitest-environment jsdom
/**
 * The xterm.js binding (login-terminal.ts).
 *
 * xterm itself cannot be imported under the test runner, so both xterm and
 * its fit addon are mocked here to a bare stand-in that records what the
 * binding does with them. What is actually under test is the glue: that a
 * screen chunk of a type `decodeTerminalChunk` cannot render, and input sent
 * while the socket is not OPEN, are LOGGED rather than silently eaten — the
 * failure mode login.ts's own comments warn a half-drawn OAuth screen would
 * otherwise hide with no trace.
 */
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  ForwardingLogger,
  resetLoggingForTests,
  setLogger,
} from "../src/wslog.js";

const { createdTerminals } = vi.hoisted(() => {
  return { createdTerminals: [] as FakeTerminal[] };
});

interface FakeTerminal {
  rows: number;
  cols: number;
  written: unknown[];
  dataHandler: ((chunk: string) => void) | null;
}

vi.mock("@xterm/xterm", () => {
  class Terminal implements FakeTerminal {
    rows = 24;
    cols = 80;
    written: unknown[] = [];
    dataHandler: ((chunk: string) => void) | null = null;
    constructor() {
      createdTerminals.push(this);
    }
    loadAddon(): void {}
    open(): void {}
    write(chunk: unknown): void {
      this.written.push(chunk);
    }
    onData(fn: (chunk: string) => void): void {
      this.dataHandler = fn;
    }
    focus(): void {}
    dispose(): void {}
  }
  return { Terminal };
});

vi.mock("@xterm/addon-fit", () => {
  class FitAddon {
    fit(): void {}
  }
  return { FitAddon };
});

// Imported after the mocks above so attachLoginTerminal picks up the fakes.
const { attachLoginTerminal } = await import("../src/login-terminal.js");

/** A WebSocket stand-in with a settable readyState and captured sends. */
class FakeSocket {
  readyState = 0; // CONNECTING
  binaryType = "";
  sent: unknown[] = [];
  private listeners = new Map<string, Array<(e: unknown) => void>>();

  addEventListener(type: string, fn: (e: unknown) => void): void {
    const list = this.listeners.get(type) ?? [];
    list.push(fn);
    this.listeners.set(type, list);
  }
  removeEventListener(): void {}
  send(data: unknown): void {
    this.sent.push(data);
  }
  close(): void {
    this.readyState = 3; // CLOSED
  }
  dispatch(type: string, event: unknown = {}): void {
    for (const fn of this.listeners.get(type) ?? []) fn(event);
  }
}

/** A logger wired to spies: captured console lines, no forwarding needed. */
function spyLogger(): { logger: ForwardingLogger; consoleLines: string[] } {
  const consoleLines: string[] = [];
  const logger = new ForwardingLogger(
    () => true,
    (level, line) => consoleLines.push(`${level}: ${line}`),
    () => 0,
  );
  return { logger, consoleLines };
}

/** Mounts a login terminal against a fresh FakeSocket. */
function mount(): { socket: FakeSocket; term: FakeTerminal } {
  const socket = new FakeSocket();
  attachLoginTerminal(document.createElement("div"), "ws://d", "s_1", {
    openSocket: () => socket as unknown as WebSocket,
  });
  const term = createdTerminals[createdTerminals.length - 1];
  return { socket, term };
}

describe("attachLoginTerminal", () => {
  afterEach(() => {
    resetLoggingForTests();
    vi.restoreAllMocks();
  });

  describe("dropped screen chunks", () => {
    it("warns naming the type when a message frame cannot be decoded", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket, term } = mount();
      // Act — an object is neither ArrayBuffer nor string.
      socket.dispatch("message", { data: { not: "a frame" } });
      // Assert
      expect(term.written).toHaveLength(0);
      expect(spy.consoleLines).toEqual([
        expect.stringContaining("warn:"),
      ]);
      expect(spy.consoleLines[0]).toContain("Object");
      expect(spy.consoleLines[0]).toContain("dropped a screen chunk");
    });

    it("still writes a decodable chunk and logs nothing", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket, term } = mount();
      // Act
      socket.dispatch("message", { data: "hello" });
      // Assert
      expect(term.written).toEqual(["hello"]);
      expect(spy.consoleLines).toEqual([]);
    });

    it("dedups a burst of the same unexpected type to one line", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket } = mount();
      // Act — three drops of the same shape in a row.
      socket.dispatch("message", { data: 1 });
      socket.dispatch("message", { data: 2 });
      socket.dispatch("message", { data: 3 });
      // Assert — same constructor each time, so one line.
      expect(spy.consoleLines).toHaveLength(1);
      expect(spy.consoleLines[0]).toContain("Number");
    });

    it("logs again for a different unexpected type", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket } = mount();
      // Act
      socket.dispatch("message", { data: 1 });
      socket.dispatch("message", { data: {} });
      // Assert
      expect(spy.consoleLines).toHaveLength(2);
      expect(spy.consoleLines[0]).toContain("Number");
      expect(spy.consoleLines[1]).toContain("Object");
    });
  });

  describe("dropped input while the socket is not open", () => {
    it("warns and drops a keystroke typed before the socket opens", () => {
      // Arrange — readyState starts CONNECTING.
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket, term } = mount();
      // Act
      term.dataHandler?.("hello");
      // Assert
      expect(socket.sent).toEqual([]);
      expect(spy.consoleLines).toEqual([
        expect.stringContaining("dropped keyboard input"),
      ]);
    });

    it("sends a keystroke once the socket is open", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket, term } = mount();
      socket.readyState = 1; // OPEN
      // Act
      term.dataHandler?.("hi");
      // Assert
      expect(socket.sent).toHaveLength(1);
      expect(spy.consoleLines).toEqual([]);
    });

    it("dedups repeated dropped keystrokes to one line", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { term } = mount();
      // Act
      term.dataHandler?.("a");
      term.dataHandler?.("b");
      term.dataHandler?.("c");
      // Assert
      expect(spy.consoleLines).toHaveLength(1);
    });

    it("warns and drops a resize report made while the socket is connecting", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket } = mount();
      // Act — a window resize before the socket has opened.
      window.dispatchEvent(new Event("resize"));
      // Assert
      expect(socket.sent).toEqual([]);
      expect(spy.consoleLines).toEqual([
        expect.stringContaining("dropped a resize report"),
      ]);
    });

    it("sends the resize report once the socket is open", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket } = mount();
      // Act — the browser only ever fires "open" once readyState is OPEN.
      socket.readyState = 1; // OPEN
      socket.dispatch("open");
      // Assert
      expect(socket.sent).toHaveLength(1);
      expect(spy.consoleLines).toEqual([]);
    });
  });

  describe("socket error", () => {
    it("warns when the socket reports an error", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket } = mount();
      // Act
      socket.dispatch("error");
      // Assert
      expect(spy.consoleLines).toEqual([
        expect.stringContaining("socket error"),
      ]);
    });

    it("dedups repeated error events to one line", () => {
      // Arrange
      const spy = spyLogger();
      setLogger(spy.logger);
      const { socket } = mount();
      // Act
      socket.dispatch("error");
      socket.dispatch("error");
      // Assert
      expect(spy.consoleLines).toHaveLength(1);
    });
  });

  describe("close", () => {
    it("still calls onClosed when the socket closes", () => {
      // Arrange — unrelated to logging, guards the existing contract still
      // works alongside the new listeners.
      const socket = new FakeSocket();
      let closed = false;
      attachLoginTerminal(document.createElement("div"), "ws://d", "s_1", {
        openSocket: () => socket as unknown as WebSocket,
        onClosed: () => {
          closed = true;
        },
      });
      // Act
      socket.dispatch("close");
      // Assert
      expect(closed).toBe(true);
    });
  });
});
