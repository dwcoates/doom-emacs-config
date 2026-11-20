import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync, mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  armRuntimeProfiling,
  disarm,
  recordProfilingEvent,
  RuntimeProfiler,
  startExitProfile,
  type InspectorPort,
  type ProfilingEvent,
  type SyncInspectorPort,
} from "../src/profiling.js";
import { LevelWindow, WINDOW_SECONDS } from "../../../logging/ts/level-window.js";

interface Captured {
  level: "info" | "error";
  fields: Record<string, unknown>;
  message: string;
}

function captureLog(): { log: { info: (f: Record<string, unknown>, m: string) => void; error: (f: Record<string, unknown>, m: string) => void }; records: Captured[] } {
  const records: Captured[] = [];
  return {
    records,
    log: {
      info: (fields, message) => records.push({ level: "info", fields, message }),
      error: (fields, message) => records.push({ level: "error", fields, message }),
    },
  };
}

let root: string;
let events: ProfilingEvent[];
let armed: (() => void)[];
let level: LevelWindow;

function profiler(inspector?: () => InspectorPort): RuntimeProfiler {
  return new RuntimeProfiler({
    runtime: "shim",
    root,
    pid: 4242,
    events: (event) => events.push(event),
    level,
    inspector,
    after: (_ms, fn) => {
      armed.push(fn);
      return { clear: () => undefined };
    },
  });
}

function open(session: string): void {
  writeFileSync(join(root, "active"), `${session}\n`);
}

function close(): void {
  rmSync(join(root, "active"));
}

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "prof-"));
  events = [];
  armed = [];
  level = new LevelWindow({ level: "info", untilMs: null }, () => 1_000_000);
});

afterEach(() => {
  rmSync(root, { recursive: true, force: true });
});

describe("RuntimeProfiler", () => {
  it("starts a profile into the active session", async () => {
    // Arrange
    const p = profiler();
    open("s1");

    // Act
    await p.reconcile();

    // Assert
    expect(events.map((e) => e.kind)).toEqual(["started"]);
    expect(p.current()).toBe("s1");
    await p.stop("test_cleanup");
  });

  it("writes both profiles when the session closes", async () => {
    // Arrange
    const p = profiler();
    open("s1");
    await p.reconcile();
    close();

    // Act
    await p.reconcile();

    // Assert
    const stop = events.at(-1) as ProfilingEvent;
    expect(stop.kind).toBe("stopped");
    expect(stop.reason).toBe("session_closed");
    expect(stop.files).toEqual([join(root, "s1", "shim-4242.cpuprofile"), join(root, "s1", "shim-4242.heapprofile")]);
    for (const file of stop.files) expect(statSync(file).size).toBeGreaterThan(0);
  });

  it("writes a CPU profile Chrome can read", async () => {
    // Arrange
    const p = profiler();
    open("s1");
    await p.reconcile();
    close();

    // Act
    await p.reconcile();

    // Assert
    const profile = JSON.parse(readFileSync(join(root, "s1", "shim-4242.cpuprofile"), "utf8")) as { nodes: unknown[] };
    expect(profile.nodes.length).toBeGreaterThan(0);
  });

  it("moves the profile to a changed session", async () => {
    // Arrange
    const p = profiler();
    open("s1");
    await p.reconcile();
    open("s2");

    // Act
    await p.reconcile();

    // Assert
    expect(events.map((e) => [e.kind, e.reason])).toEqual([
      ["started", undefined],
      ["stopped", "session_changed"],
      ["started", undefined],
    ]);
    expect(p.current()).toBe("s2");
    await p.stop("test_cleanup");
  });

  it("is idempotent for the running session", async () => {
    // Arrange
    const p = profiler();
    open("s1");
    await p.reconcile();

    // Act
    await p.reconcile();

    // Assert
    expect(events).toHaveLength(1);
    await p.stop("test_cleanup");
  });

  it("starts nothing with no session open", async () => {
    // Arrange
    const p = profiler();

    // Act
    await p.reconcile();

    // Assert
    expect(events).toEqual([]);
  });

  it("refuses an invalid session loudly", async () => {
    // Arrange
    const p = profiler();
    open("../escape");

    // Act
    await p.reconcile();

    // Assert
    expect(events.map((e) => e.kind)).toEqual(["failed"]);
    expect(p.current()).toBe("");
  });

  it("stops by itself at the max duration", async () => {
    // Arrange
    const p = profiler();
    open("s1");
    await p.reconcile();

    // Act
    armed[0]();
    await p.stop("noop_after_expiry");

    // Assert
    expect(events.map((e) => [e.kind, e.reason])).toEqual([
      ["started", undefined],
      ["stopped", "max_duration"],
    ]);
  });

  it("leaves a later profile running when an earlier one's max duration fires", async () => {
    // Arrange
    const p = profiler();
    open("s1");
    await p.reconcile();
    open("s2");
    await p.reconcile();

    // Act
    armed[0]();
    await p.reconcile();

    // Assert
    expect(p.current()).toBe("s2");
    expect(events).toHaveLength(3);
    await p.stop("test_cleanup");
  });

  it("records an inspector that refuses to start", async () => {
    // Arrange
    const refusing: InspectorPort = {
      connect: () => undefined,
      post: () => Promise.reject(new Error("refused")),
      disconnect: () => undefined,
    };
    const p = profiler(() => refusing);
    open("s1");

    // Act
    await p.reconcile();

    // Assert
    expect(events.map((e) => [e.kind, e.error?.message])).toEqual([["failed", "refused"]]);
    expect(p.current()).toBe("");
  });

  it("writes its armed marker", () => {
    // Arrange
    const p = profiler();

    // Act
    const marker = p.arm();

    // Assert
    expect(marker).toBe(join(root, "armed", "shim-4242"));
    expect(existsSync(marker)).toBe(true);
  });
});

describe("recordProfilingEvent", () => {
  it.each([
    [{ kind: "started", session: "s1", files: [] }, "info", "shim.profiling.started"],
    [{ kind: "stopped", session: "s1", reason: "exit", files: [] }, "info", "shim.profiling.stopped"],
    [{ kind: "failed", session: "", files: [], error: new Error("boom") }, "error", "shim.profiling.failed"],
  ] as [ProfilingEvent, string, string][])("records %o", (event, level, operation) => {
    // Arrange
    const { log, records } = captureLog();

    // Act
    recordProfilingEvent(log, event);

    // Assert
    expect(records.map((r) => [r.level, r.fields.operation])).toEqual([[level, operation]]);
  });

  it("carries a failure's cause", () => {
    // Arrange
    const { log, records } = captureLog();

    // Act
    recordProfilingEvent(log, { kind: "failed", session: "", files: [], error: new Error("boom") });

    // Assert
    expect(records[0].fields.cause).toBe("boom");
  });
});

describe("disarm", () => {
  it("removes the marker", () => {
    // Arrange
    const marker = join(root, "m");
    writeFileSync(marker, "shim\n");
    const { log, records } = captureLog();

    // Act
    disarm(marker, log);

    // Assert
    expect(existsSync(marker)).toBe(false);
    expect(records).toEqual([]);
  });

  it("treats an absent marker as removed", () => {
    // Arrange
    const { log, records } = captureLog();

    // Act
    disarm(join(root, "absent"), log);

    // Assert
    expect(records).toEqual([]);
  });

  it("records a marker it cannot remove", () => {
    // Arrange: a directory is not removable by rmSync without recursive.
    const marker = join(root, "dir");
    mkdirSync(marker);
    const { log, records } = captureLog();

    // Act
    disarm(marker, log);

    // Assert
    expect(records.map((r) => r.level)).toEqual(["error"]);
  });
});

describe("armRuntimeProfiling", () => {
  it("arms, reconciles on SIGWINCH and disarms at exit", async () => {
    // Arrange
    const { log } = captureLog();
    let signalHandler: (() => void) | undefined;
    let exitHandler: (() => void) | undefined;

    // Act
    const p = armRuntimeProfiling(log, {
      env: { AGENT_REPL_PROFILE_ROOT: root },
      home: "/unused",
      pid: 77,
      onSignal: (signal, handler) => {
        expect(signal).toBe("SIGWINCH");
        signalHandler = handler;
      },
      onExit: (handler) => {
        exitHandler = handler;
      },
      level,
    });

    // Assert
    expect(p).toBeDefined();
    expect(readdirSync(join(root, "armed"))).toEqual(["shim-77"]);
    expect(signalHandler).toBeDefined();
    exitHandler?.();
    expect(readdirSync(join(root, "armed"))).toEqual([]);
  });

  it("records a profiler that cannot arm and runs on", () => {
    // Arrange
    const { log, records } = captureLog();

    // Act
    const p = armRuntimeProfiling(log, {
      env: {},
      home: "",
      pid: 77,
      onSignal: () => undefined,
      level,
      onExit: () => undefined,
    });

    // Assert
    expect(p).toBeUndefined();
    expect(records.map((r) => [r.level, r.fields.operation])).toEqual([["error", "shim.profiling.failed"]]);
  });
});

describe("RuntimeProfiler: a session's log level", () => {
  /** Write NAME into SESSION's log level file. */
  function askLevel(session: string, name: string): void {
    mkdirSync(join(root, session), { recursive: true });
    writeFileSync(join(root, session, "log-level"), `${name}\n`);
  }

  it("raises the shim to the session's level for the longest window", async () => {
    // Arrange
    const p = profiler();
    askLevel("s1", "debug");
    open("s1");

    // Act
    await p.reconcile();

    // Assert
    expect([level.level(), events.at(-1)?.kind, events.at(-1)?.untilMs]).toEqual(["debug", "level_opened", 1_000_000 + WINDOW_SECONDS * 1000]);
    await p.stop("test_cleanup");
  });

  it("opens no window for a session naming no level", async () => {
    // Arrange
    const p = profiler();
    open("s1");

    // Act
    await p.reconcile();

    // Assert
    expect([level.level(), events.map((e) => e.kind)]).toEqual(["info", ["started"]]);
    await p.stop("test_cleanup");
  });

  it("fails loudly on an unknown level and keeps the profile", async () => {
    // Arrange
    const p = profiler();
    askLevel("s1", "trace");
    open("s1");

    // Act
    await p.reconcile();

    // Assert
    expect([events.at(-1)?.kind, p.current(), level.level()]).toEqual(["failed", "s1", "info"]);
    await p.stop("test_cleanup");
  });

  it("closes the window when the profile stops", async () => {
    // Arrange
    const p = profiler();
    askLevel("s1", "debug");
    open("s1");
    await p.reconcile();
    close();

    // Act
    await p.reconcile();

    // Assert
    const closed = events.find((e) => e.kind === "level_closed");
    expect([level.level(), closed?.level, closed?.reason]).toEqual(["info", "debug", "session_closed"]);
  });
});

describe("recordProfilingEvent: a session's log level", () => {
  it.each([
    ["level_opened", "shim.profiling.level_opened"],
    ["level_closed", "shim.profiling.level_closed"],
  ] as const)("records %s at info as %s", (kind, operation) => {
    // Arrange
    const { log, records } = captureLog();

    // Act
    recordProfilingEvent(log, { kind, session: "s1", files: [], level: "debug", untilMs: 1_000_000 });

    // Assert
    expect([records[0]?.level, records[0]?.fields.operation]).toEqual(["info", operation]);
  });
});


/** A same-thread inspector whose answers the test scripts: METHOD's error, or no answer at all. */
function scriptedInspector(script: { fail?: string; silent?: string }): SyncInspectorPort & { disconnected: boolean } {
  const port = {
    disconnected: false,
    connect: () => undefined,
    post: (method: string, callback: (err: Error | null, result?: unknown) => void) => {
      if (method === script.silent) return;
      if (method === script.fail) {
        callback(new Error(`${method} refused`));
        return;
      }
      callback(null, method === "Profiler.stop" ? { profile: { nodes: [], startTime: 0, endTime: 1 } } : {});
    },
    disconnect: () => {
      port.disconnected = true;
    },
  };
  return port;
}

describe("startExitProfile", () => {
  it("profiles the whole life and writes a Chrome-readable profile at exit", () => {
    // Arrange
    const { log, records } = captureLog();
    let exitHandler: (() => void) | undefined;
    const dir = join(root, "unit", "TestX");

    // Act
    const started = startExitProfile(log, { dir, pid: 77, onExit: (h) => (exitHandler = h) });
    exitHandler?.();

    // Assert
    const path = join(dir, "shim-77.cpuprofile");
    expect(started).toBe(true);
    expect(records.map((r) => [r.level, r.fields.operation, r.fields.files])).toEqual([
      ["info", "shim.profiling.exit_started", [path]],
      ["info", "shim.profiling.exit_stopped", [path]],
    ]);
    const profile = JSON.parse(readFileSync(path, "utf8")) as { nodes: unknown[]; startTime: number; endTime: number };
    expect(profile.nodes.length).toBeGreaterThan(0);
    expect(profile.endTime).toBeGreaterThanOrEqual(profile.startTime);
  });

  it.each([
    { name: "an inspector that refuses to start", script: { fail: "Profiler.start" }, cause: "Profiler.start refused" },
    { name: "an inspector that does not answer at once", script: { silent: "Profiler.enable" }, cause: "did not answer Profiler.enable at once" },
  ])("records $name and runs on unprofiled", ({ script, cause }) => {
    // Arrange
    const { log, records } = captureLog();
    const port = scriptedInspector(script);
    let exitHandlers = 0;

    // Act
    const started = startExitProfile(log, { dir: root, pid: 77, onExit: () => (exitHandlers += 1), inspector: () => port });

    // Assert
    expect(started).toBe(false);
    expect(exitHandlers).toBe(0);
    expect(port.disconnected).toBe(true);
    expect(records).toHaveLength(1);
    expect(records[0]).toMatchObject({ level: "error", message: "the shim's whole-life profile could not start" });
    expect(records[0].fields).toMatchObject({ operation: "shim.profiling.failed", reason: "exit_profile", files: [join(root, "shim-77.cpuprofile")] });
    expect(String(records[0].fields.cause)).toContain(cause);
  });

  it("records a directory it cannot make", () => {
    // Arrange
    const { log, records } = captureLog();
    const file = join(root, "a-file");
    writeFileSync(file, "");

    // Act
    const started = startExitProfile(log, { dir: join(file, "sub"), pid: 77, onExit: () => undefined, inspector: () => scriptedInspector({}) });

    // Assert
    expect(started).toBe(false);
    expect(records.map((r) => [r.level, r.fields.operation, r.fields.reason])).toEqual([["error", "shim.profiling.failed", "exit_profile"]]);
  });

  it("records a profile it cannot write at exit, and nothing written", () => {
    // Arrange
    const { log, records } = captureLog();
    let exitHandler: (() => void) | undefined;
    const port = scriptedInspector({ fail: "Profiler.stop" });

    // Act
    startExitProfile(log, { dir: root, pid: 77, onExit: (h) => (exitHandler = h), inspector: () => port });
    exitHandler?.();

    // Assert
    expect(records.map((r) => [r.level, r.fields.operation])).toEqual([
      ["info", "shim.profiling.exit_started"],
      ["error", "shim.profiling.failed"],
    ]);
    expect(records[1].message).toBe("the shim's whole-life profile could not be written");
    expect(String(records[1].fields.cause)).toContain("Profiler.stop refused");
    expect(existsSync(join(root, "shim-77.cpuprofile"))).toBe(false);
    expect(port.disconnected).toBe(true);
  });
});

describe("armRuntimeProfiling: a whole-life profile", () => {
  it("profiles the whole life instead of arming a session", () => {
    // Arrange
    const { log, records } = captureLog();
    const dir = join(root, "whole-life");
    let signalHandlers = 0;

    // Act
    const p = armRuntimeProfiling(log, {
      env: { AGENT_REPL_PROFILE_ROOT: root, AGENT_REPL_EXIT_PROFILE_DIR: dir },
      home: "/unused",
      pid: 77,
      onSignal: () => (signalHandlers += 1),
      onExit: () => undefined,
      level,
      exitInspector: () => scriptedInspector({}),
    });

    // Assert
    expect(p).toBeUndefined();
    expect(signalHandlers).toBe(0);
    expect(existsSync(join(root, "armed"))).toBe(false);
    expect(records.map((r) => r.fields.operation)).toEqual(["shim.profiling.exit_started"]);
  });
});
