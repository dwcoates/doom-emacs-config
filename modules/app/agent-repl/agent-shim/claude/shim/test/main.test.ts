/**
 * The process shell: argv, environment, --version, the signal boundary, and the
 * lock directory.
 *
 * The suite deliberately does NOT call `main()`. main() binds a socket, takes a
 * kernel lock and then parks forever, so the parts worth asserting are exported
 * as pure functions and asserted directly; the end-to-end behavior of the built
 * bundle is the dist smoke's job.
 */
import { mkdirSync, mkdtempSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi } from "vitest";
import { LOCK_DIR_ENV, lockBinaryPath, lockDir } from "../src/locks.js";
import { DEFAULT_RETRY_POLICY } from "../src/store/persistence.js";
import {
  DEFAULT_STATE_DIR_NAME,
  EXIT_QUIET_BUDGET_MS,
  FAKE_EXIT_QUIET_BUDGET_ENV,
  FAKE_KEEPALIVE_INTERVAL_ENV,
  FAKE_STORE_BACKOFF_ENV,
  FAKE_WATCHER_CONCLUSION_BUDGET_ENV,
  resolveExitQuietBudgetMs,
  resolveRetryPolicy,
  resolveWatcherConclusionBudgetMs,
  logCorrelation,
  queryFactory,
  resolveKeepaliveIntervalMs,
  OWNED_ENV,
  STORE_SOCKET_ENV,
  packageVersion,
  parseArgs,
  processIdentity,
  requireServingArgs,
  workspaceIdFromListenSocket,
  resolveEnvironment,
  shutdownSignalHandlers,
  keepaliveResetSignalHandler,
  versionLine,
  type CliArgs,
  type ShimEnvironment,
} from "../src/main.js";
import { VendorCallsForbiddenError } from "../src/vendor-guard.js";
import type { Engine } from "../src/engine/engine.js";
import type { QuerySpec, SessionEngine } from "../src/engine/session.js";

/** A complete, legal spawn environment. */
function spawnEnv(overrides: NodeJS.ProcessEnv = {}): NodeJS.ProcessEnv {
  return {
    CLAUDE_CONFIG_DIR: "/accounts/primary",
    [OWNED_ENV]: "1",
    SHIM_BUILD_SHA: "abc1234",
    ...overrides,
  };
}

function servingArgs(overrides: Partial<CliArgs> = {}): CliArgs {
  return {
    listen: "/tmp/0100059cb65649bc.sock",
    storeSocket: "/tmp/store.sock",
    logFd: 3,
    fake: false,
    version: false,
    ...overrides,
  };
}

describe("parseArgs", () => {
  it("accepts the whole spawn contract", () => {
    // Arrange, Act.
    const args = parseArgs([
      "--listen",
      "/tmp/0100059cb65649bc.sock",
      "--store-socket",
      "/tmp/store.sock",
      "--log-fd",
      "3",
      "--fake",
    ]);

    // Assert.
    expect(args).toEqual({
      listen: "/tmp/0100059cb65649bc.sock",
      storeSocket: "/tmp/store.sock",
      logFd: 3,
      fake: true,
      version: false,
    });
  });

  it("defaults --fake off", () => {
    // Arrange, Act.
    const args = parseArgs(["--listen", "/tmp/0100059cb65649bc.sock", "--log-fd", "3"]);

    // Assert.
    expect(args.fake).toBe(false);
  });

  it("accepts --version alone", () => {
    // Arrange, Act.
    const args = parseArgs(["--version"]);

    // Assert.
    expect(args.version).toBe(true);
  });

  it("REFUSES an unknown argument rather than starting with it discarded", () => {
    // Arrange, Act, Assert.
    expect(() => parseArgs(["--nonsense"])).toThrow(/unknown argument "--nonsense"/);
  });

  it("names the whole spawn contract when it refuses one", () => {
    // Arrange, Act, Assert.
    expect(() => parseArgs(["--nonsense"])).toThrow(/--listen <uds> --store-socket <uds> --log-fd 3/);
  });

  it.each([
    ["--session-id"],
    ["--cwd"],
    ["--model"],
    ["--permission-mode"],
    ["--resume"],
    ["--claude-bin"],
    ["--daemon-socket"],
    ["--rewound-from"],
  ])("refuses the retired flag %s", (flag) => {
    // Arrange, Act, Assert.
    expect(() => parseArgs([flag, "value"])).toThrow(/unknown argument/);
  });

  it("refuses a flag whose value is missing", () => {
    // Arrange, Act, Assert.
    expect(() => parseArgs(["--listen"])).toThrow(/missing value for --listen/);
  });

  it("accepts fd 3 for the durable sink", () => {
    // Arrange, Act.
    const args = parseArgs(["--log-fd", "3"]);

    // Assert.
    expect(args.logFd).toBe(3);
  });

  it("refuses any other descriptor, which could point the record at a dying pipe", () => {
    // Arrange, Act, Assert.
    expect(() => parseArgs(["--log-fd", "2"])).toThrow(/the durable sink is inherited fd 3/);
  });
});

describe("requireServingArgs", () => {
  it("accepts a complete serving command line", () => {
    // Arrange, Act, Assert.
    expect(() => requireServingArgs(servingArgs())).not.toThrow();
  });

  it("refuses a shim with nowhere to listen", () => {
    // Arrange.
    const args = parseArgs(["--log-fd", "3"]);

    // Act, Assert.
    expect(() => requireServingArgs(args)).toThrow(/--listen <uds> is required/);
  });

  it("refuses a shim with no durable log sink", () => {
    // Arrange.
    const args = parseArgs(["--listen", "/tmp/0100059cb65649bc.sock"]);

    // Act, Assert.
    expect(() => requireServingArgs(args)).toThrow(/--log-fd 3 is required/);
  });
});

describe("resolveEnvironment", () => {
  it("resolves a complete environment", () => {
    // Arrange, Act.
    const environment = resolveEnvironment(spawnEnv(), servingArgs(), "/home/dev");

    // Assert.
    expect(environment).toEqual({
      claudeConfigDir: "/accounts/primary",
      stateDir: `/home/dev/${DEFAULT_STATE_DIR_NAME}`,
      shimBuildSha: "abc1234",
      storeSocket: "/tmp/store.sock",
    });
  });

  it("refuses a missing account root rather than guessing which account to run as", () => {
    // Arrange.
    const env = spawnEnv({ CLAUDE_CONFIG_DIR: undefined });

    // Act, Assert.
    expect(() => resolveEnvironment(env, servingArgs(), "/home/dev")).toThrow(
      /CLAUDE_CONFIG_DIR is required/,
    );
  });

  it("refuses an empty account root, which is a sentinel and not an absence", () => {
    // Arrange.
    const env = spawnEnv({ CLAUDE_CONFIG_DIR: "" });

    // Act, Assert.
    expect(() => resolveEnvironment(env, servingArgs(), "/home/dev")).toThrow(
      /CLAUDE_CONFIG_DIR is required/,
    );
  });

  it("refuses to run unowned", () => {
    // Arrange.
    const env = spawnEnv({ [OWNED_ENV]: undefined });

    // Act, Assert.
    expect(() => resolveEnvironment(env, servingArgs(), "/home/dev")).toThrow(
      /AGENT_REPL_OWNED=1 is required/,
    );
  });

  it("refuses an owned marker that is not exactly 1", () => {
    // Arrange.
    const env = spawnEnv({ [OWNED_ENV]: "true" });

    // Act, Assert.
    expect(() => resolveEnvironment(env, servingArgs(), "/home/dev")).toThrow(
      /AGENT_REPL_OWNED=1 is required/,
    );
  });

  it("refuses a missing build sha, which would make every shim look current", () => {
    // Arrange.
    const env = spawnEnv({ SHIM_BUILD_SHA: undefined });

    // Act, Assert.
    expect(() => resolveEnvironment(env, servingArgs(), "/home/dev")).toThrow(
      /SHIM_BUILD_SHA is required/,
    );
  });

  it("falls back to AGENT_REPL_STORE_SOCKET when the flag is absent", () => {
    // Arrange.
    const env = spawnEnv({ [STORE_SOCKET_ENV]: "/env/store.sock" });
    const args = servingArgs({ storeSocket: undefined });

    // Act.
    const environment = resolveEnvironment(env, args, "/home/dev");

    // Assert.
    expect(environment.storeSocket).toBe("/env/store.sock");
  });

  it("lets the FLAG beat the env, because a caller that stated it meant it", () => {
    // Arrange.
    const env = spawnEnv({ [STORE_SOCKET_ENV]: "/env/store.sock" });
    const args = servingArgs({ storeSocket: "/flag/store.sock" });

    // Act.
    const environment = resolveEnvironment(env, args, "/home/dev");

    // Assert.
    expect(environment.storeSocket).toBe("/flag/store.sock");
  });

  it("refuses when neither the flag nor the env names a store", () => {
    // Arrange.
    const args = servingArgs({ storeSocket: undefined });

    // Act, Assert.
    expect(() => resolveEnvironment(spawnEnv(), args, "/home/dev")).toThrow(
      /the store socket is required/,
    );
  });

  it("defaults the state root under the user's home", () => {
    // Arrange, Act.
    const environment = resolveEnvironment(spawnEnv(), servingArgs(), "/home/dev");

    // Assert.
    expect(environment.stateDir).toBe(`/home/dev/${DEFAULT_STATE_DIR_NAME}`);
  });

  it("honors an explicit state root", () => {
    // Arrange.
    const env = spawnEnv({ AGENT_REPL_STATE_DIR: "/var/state" });

    // Act.
    const environment = resolveEnvironment(env, servingArgs(), "/home/dev");

    // Assert.
    expect(environment.stateDir).toBe("/var/state");
  });

  it("treats an EMPTY state root as unset rather than as the filesystem root", () => {
    // Arrange.
    const env = spawnEnv({ AGENT_REPL_STATE_DIR: "" });

    // Act.
    const environment = resolveEnvironment(env, servingArgs(), "/home/dev");

    // Assert.
    expect(environment.stateDir).toBe(`/home/dev/${DEFAULT_STATE_DIR_NAME}`);
  });
});

describe("lockDir", () => {
  it("defaults to the directory the deployed daemon probes", () => {
    // Arrange.
    const original = process.env[LOCK_DIR_ENV];
    delete process.env[LOCK_DIR_ENV];

    // Act.
    const resolved = lockDir();

    // Assert.
    expect(resolved).toMatch(/\.cache\/agent-repl\/run$/);
    if (original !== undefined) process.env[LOCK_DIR_ENV] = original;
  });

  it("honors an override, so a test can isolate its locks from the machine's", () => {
    // Arrange.
    const original = process.env[LOCK_DIR_ENV];
    process.env[LOCK_DIR_ENV] = "/tmp/isolated-locks";

    // Act.
    const resolved = lockDir();

    // Assert.
    expect(resolved).toBe("/tmp/isolated-locks");
    if (original === undefined) delete process.env[LOCK_DIR_ENV];
    else process.env[LOCK_DIR_ENV] = original;
  });

  it("treats an EMPTY override as unset", () => {
    // Arrange.
    const original = process.env[LOCK_DIR_ENV];
    process.env[LOCK_DIR_ENV] = "";

    // Act.
    const resolved = lockDir();

    // Assert.
    expect(resolved).toMatch(/\.cache\/agent-repl\/run$/);
    if (original === undefined) delete process.env[LOCK_DIR_ENV];
    else process.env[LOCK_DIR_ENV] = original;
  });
});

describe("versionLine", () => {
  it("names the shim and its package version", () => {
    // Arrange, Act.
    const line = versionLine();

    // Assert.
    expect(line).toBe(`claude-shim ${packageVersion()}`);
  });

  it("reports a real version rather than the unknown fallback", () => {
    // Arrange, Act.
    const version = packageVersion();

    // Assert.
    expect(version).not.toBe("unknown");
  });
});

describe("processIdentity", () => {
  // IT JOINS THE DAEMON'S RECORDS, which is the whole point of a correlation
  // id: keyed by the shim's own md5 prefix it joined nothing outside this
  // process, and `bin/logs.sh --workspace` grouped it under a workspace the
  // fleet had never heard of.
  it("is keyed by the daemon's own workspace id", () => {
    // Arrange, Act.
    const identity = processIdentity("0100059cb65649bc");

    // Assert.
    expect(identity).toMatch(/^shim-0100059cb65649bc-\d+$/);
  });

  it("gives two workspaces two identities", () => {
    // Arrange, Act.
    const first = processIdentity("0100059cb65649bc");
    const second = processIdentity("0100059cb65649bd");

    // Assert.
    expect(first).not.toBe(second);
  });
});

describe("workspaceIdFromListenSocket", () => {
  it("reads the id off a first-generation socket", () => {
    // Arrange, Act, Assert.
    expect(
      workspaceIdFromListenSocket("/Users/x/.claude-emacs/sock/0100059cb65649bc.sock"),
    ).toBe("0100059cb65649bc");
  });

  // A REPLACEMENT SHIM SERVES THE SAME WORKSPACE. The rollout appends its
  // generation to the socket name, and a generation is not a new workspace.
  it("reads the same id off a rolled generation's socket", () => {
    // Arrange, Act, Assert.
    expect(
      workspaceIdFromListenSocket("/Users/x/.claude-emacs/sock/0100059cb65649bc.n7.sock"),
    ).toBe("0100059cb65649bc");
  });

  it("refuses a socket whose name is not a workspace id", () => {
    // Arrange, Act, Assert.
    expect(() => workspaceIdFromListenSocket("/tmp/shim-for-a-human.sock")).toThrow(
      /not named after a workspace id/,
    );
  });

  it("refuses an id of the wrong width", () => {
    // Arrange, Act, Assert.
    expect(() => workspaceIdFromListenSocket("/tmp/0100059cb656.sock")).toThrow(
      /not named after a workspace id/,
    );
  });

  it("refuses an id that is not hexadecimal", () => {
    // Arrange, Act, Assert.
    expect(() => workspaceIdFromListenSocket("/tmp/0100059cb65649bZ.sock")).toThrow(
      /not named after a workspace id/,
    );
  });
});

/** An engine that records calls to `resetKeepalives`. */
function resetKeepalivesEngine(): { engine: SessionEngine; calls: number[] } {
  const calls: number[] = [];
  const engine = {
    resetKeepalives: async (): Promise<void> => {
      calls.push(calls.length);
    },
  } as unknown as SessionEngine;
  return { engine, calls };
}

describe("keepaliveResetSignalHandler", () => {
  it("invokes the engine's resetKeepalives on SIGUSR2", () => {
    // Arrange.
    const { engine, calls } = resetKeepalivesEngine();
    const handlers = keepaliveResetSignalHandler(engine);

    // Act.
    handlers.onSigusr2();

    // Assert.
    expect(calls).toEqual([0]);
  });

  it("is a safe no-op when the engine has no active session", () => {
    // Arrange: `resetKeepalives` itself is the no-op session owns; the handler
    // just has to call it without throwing.
    const engine = {
      resetKeepalives: async (): Promise<void> => undefined,
    } as unknown as SessionEngine;
    const handlers = keepaliveResetSignalHandler(engine);

    // Act, Assert.
    expect(() => handlers.onSigusr2()).not.toThrow();
  });
});

/** An engine that records its stand-down, and can be made to fail it. */
function standDownEngine(
  failure?: Error,
  exitCode = 0,
): { engine: Engine; calls: string[] } {
  const calls: string[] = [];
  const engine = {
    standDown: async (reason: string): Promise<number> => {
      calls.push(reason);
      if (failure !== undefined) throw failure;
      return exitCode;
    },
  } as unknown as Engine;
  return { engine, calls };
}

describe("shutdownSignalHandlers", () => {
  it("stands the session down on SIGTERM", async () => {
    // Arrange.
    const { engine, calls } = standDownEngine();
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: () => undefined,
    });

    // Act.
    handlers.onSigterm();
    await handlers.standingDown();

    // Assert.
    expect(calls).toEqual(["SIGTERM"]);
  });

  it("closes the listener so the socket file does not outlive the process", async () => {
    // Arrange.
    const { engine } = standDownEngine();
    let closed = false;
    const handlers = shutdownSignalHandlers({
      engine,
      server: {
        close: async () => {
          closed = true;
        },
      },
      exit: () => undefined,
    });

    // Act.
    handlers.onSigterm();
    await handlers.standingDown();

    // Assert.
    expect(closed).toBe(true);
  });

  it("exits with the code the stand-down earned when writes were lost", async () => {
    // A23: a stand-down that flushed with rows the store never acked has NOT
    // stood down cleanly, and reporting 0 would tell the daemon otherwise.
    // Arrange.
    const { engine } = standDownEngine(undefined, 1);
    const codes: number[] = [];
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: (code) => codes.push(code),
    });

    // Act.
    handlers.onSigterm();
    await handlers.standingDown();

    // Assert.
    expect(codes).toEqual([1]);
  });

  it("exits 0 after a clean stand-down", async () => {
    // Arrange.
    const { engine } = standDownEngine();
    const codes: number[] = [];
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: (code) => codes.push(code),
    });

    // Act.
    handlers.onSigterm();
    await handlers.standingDown();

    // Assert.
    expect(codes).toEqual([0]);
  });

  it("exits NONZERO when the stand-down failed, never claiming good order", async () => {
    // Arrange.
    const { engine } = standDownEngine(new Error("store never acked"));
    const codes: number[] = [];
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: (code) => codes.push(code),
    });

    // Act.
    handlers.onSigterm();
    await handlers.standingDown();

    // Assert.
    expect(codes).toEqual([1]);
  });

  it("ignores a second SIGTERM instead of racing two teardowns", async () => {
    // Arrange.
    const { engine, calls } = standDownEngine();
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: () => undefined,
    });

    // Act.
    handlers.onSigterm();
    handlers.onSigterm();
    await handlers.standingDown();

    // Assert.
    expect(calls).toEqual(["SIGTERM"]);
  });

  it("REFUSES SIGINT, leaving the session running", () => {
    // Arrange.
    const { engine, calls } = standDownEngine();
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: () => undefined,
    });

    // Act.
    handlers.onSigint();

    // Assert.
    expect({ stoodDown: calls, standingDown: handlers.standingDown() }).toEqual({
      stoodDown: [],
      standingDown: null,
    });
  });

  it("logs the named SIGINT shutdown refusal at warn", () => {
    // Arrange.
    const { engine } = standDownEngine();
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: () => undefined,
    });
    const stderr = vi.spyOn(process.stderr, "write").mockImplementation(() => true);

    // Act.
    handlers.onSigint();

    // Assert.
    const written = stderr.mock.calls.map((call) => String(call[0])).join("");
    expect(written).toContain('"level":"warn"');
  });

  it("reports no stand-down in flight before any signal arrives", () => {
    // Arrange.
    const { engine } = standDownEngine();

    // Act.
    const handlers = shutdownSignalHandlers({
      engine,
      server: { close: async () => undefined },
      exit: () => undefined,
    });

    // Assert.
    expect(handlers.standingDown()).toBeNull();
  });
});

describe("the keep-alive interval override", () => {
  it("is unset when the environment names none", () => {
    expect(resolveKeepaliveIntervalMs({}, true)).toBeUndefined();
  });

  it("is honored under --fake", () => {
    expect(resolveKeepaliveIntervalMs({ [FAKE_KEEPALIVE_INTERVAL_ENV]: "200" }, true)).toBe(200);
  });

  it("is REFUSED for a real session", () => {
    // A production shim that took its cadence from the environment could be
    // told to hammer the vendor or never to beat at all.
    expect(resolveKeepaliveIntervalMs({ [FAKE_KEEPALIVE_INTERVAL_ENV]: "200" }, false)).toBeUndefined();
  });

  it("refuses a value that is not a whole number", () => {
    expect(resolveKeepaliveIntervalMs({ [FAKE_KEEPALIVE_INTERVAL_ENV]: "1.5" }, true)).toBeUndefined();
  });

  it("refuses a value that is not positive", () => {
    expect(resolveKeepaliveIntervalMs({ [FAKE_KEEPALIVE_INTERVAL_ENV]: "0" }, true)).toBeUndefined();
  });

  it("refuses a value that is not a number at all", () => {
    expect(resolveKeepaliveIntervalMs({ [FAKE_KEEPALIVE_INTERVAL_ENV]: "soon" }, true)).toBeUndefined();
  });
});

describe("the quiet-drain budget override", () => {
  it("answers the production budget when the environment names none", () => {
    expect(resolveExitQuietBudgetMs({}, true)).toBe(EXIT_QUIET_BUDGET_MS);
  });

  it("answers the production budget for an empty value", () => {
    expect(resolveExitQuietBudgetMs({ [FAKE_EXIT_QUIET_BUDGET_ENV]: "" }, true)).toBe(
      EXIT_QUIET_BUDGET_MS,
    );
  });

  it("is honored under --fake", () => {
    expect(resolveExitQuietBudgetMs({ [FAKE_EXIT_QUIET_BUDGET_ENV]: "150" }, true)).toBe(150);
  });

  it("is REFUSED for a real session", () => {
    // A production shim told to exit instantly would destroy the socket out
    // from under a response still on it.
    expect(resolveExitQuietBudgetMs({ [FAKE_EXIT_QUIET_BUDGET_ENV]: "150" }, false)).toBe(
      EXIT_QUIET_BUDGET_MS,
    );
  });

  it("refuses a value that is not a whole number", () => {
    expect(resolveExitQuietBudgetMs({ [FAKE_EXIT_QUIET_BUDGET_ENV]: "1.5" }, true)).toBe(
      EXIT_QUIET_BUDGET_MS,
    );
  });

  it("refuses a value that is not positive", () => {
    expect(resolveExitQuietBudgetMs({ [FAKE_EXIT_QUIET_BUDGET_ENV]: "0" }, true)).toBe(
      EXIT_QUIET_BUDGET_MS,
    );
  });

  it("refuses a value that is not a number at all", () => {
    expect(resolveExitQuietBudgetMs({ [FAKE_EXIT_QUIET_BUDGET_ENV]: "soon" }, true)).toBe(
      EXIT_QUIET_BUDGET_MS,
    );
  });
});

describe("the watcher-conclusion budget override", () => {
  it("is unset when the environment names none", () => {
    expect(resolveWatcherConclusionBudgetMs({}, true)).toBeUndefined();
  });

  it("is unset for an empty value", () => {
    expect(
      resolveWatcherConclusionBudgetMs({ [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "" }, true),
    ).toBeUndefined();
  });

  it("is honored under --fake", () => {
    expect(
      resolveWatcherConclusionBudgetMs({ [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "250" }, true),
    ).toBe(250);
  });

  it("is REFUSED for a real session", () => {
    // A production shim told to give up on a tail immediately would cut
    // streams exactly where a terminal was owed.
    expect(
      resolveWatcherConclusionBudgetMs({ [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "250" }, false),
    ).toBeUndefined();
  });

  it("refuses a value that is not a whole number", () => {
    expect(
      resolveWatcherConclusionBudgetMs({ [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "1.5" }, true),
    ).toBeUndefined();
  });

  it("refuses a value that is not positive", () => {
    expect(
      resolveWatcherConclusionBudgetMs({ [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "0" }, true),
    ).toBeUndefined();
  });

  it("refuses a value that is not a number at all", () => {
    expect(
      resolveWatcherConclusionBudgetMs({ [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "soon" }, true),
    ).toBeUndefined();
  });
});

describe("the store retry backoff override", () => {
  it("answers the production policy when the environment names none", () => {
    expect(resolveRetryPolicy({}, true)).toEqual(DEFAULT_RETRY_POLICY);
  });

  it("answers the production policy for an empty value", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "" }, true)).toEqual(
      DEFAULT_RETRY_POLICY,
    );
  });

  it("is honored under --fake", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1,2,3,4" }, true).backoffMs).toEqual([
      1, 2, 3, 4,
    ]);
  });

  it("tolerates whitespace around each delay", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: " 1 , 2 " }, true).backoffMs).toEqual([
      1, 2,
    ]);
  });

  it("accepts a zero delay, which is a schedule that never waits", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "0" }, true).backoffMs).toEqual([0]);
  });

  it("leaves the ATTEMPT COUNT at the production value", () => {
    // Only the waiting is overridable: an override that could shorten the
    // attempt count would weaken the exhaustion assertions this exists to keep.
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1" }, true).maxAttempts).toBe(
      DEFAULT_RETRY_POLICY.maxAttempts,
    );
  });

  it("leaves the BUFFER DEPTH at the production value", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1" }, true).bufferCapacity).toBe(
      DEFAULT_RETRY_POLICY.bufferCapacity,
    );
  });

  it("is REFUSED for a real session", () => {
    // A production shim told to retry with no backoff would hammer a store
    // that is merely restarting.
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1,2" }, false)).toEqual(
      DEFAULT_RETRY_POLICY,
    );
  });

  it("refuses a schedule with a negative delay", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1,-2" }, true)).toEqual(
      DEFAULT_RETRY_POLICY,
    );
  });

  it("refuses a schedule with a fractional delay", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1.5" }, true)).toEqual(
      DEFAULT_RETRY_POLICY,
    );
  });

  it("refuses a schedule with a non-numeric delay", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1,soon" }, true)).toEqual(
      DEFAULT_RETRY_POLICY,
    );
  });

  it("refuses a schedule with an empty slot", () => {
    expect(resolveRetryPolicy({ [FAKE_STORE_BACKOFF_ENV]: "1,,2" }, true)).toEqual(
      DEFAULT_RETRY_POLICY,
    );
  });
});

describe("logCorrelation", () => {
  const env = (agentReplSessionId?: string): ShimEnvironment => ({
    claudeConfigDir: "/accounts/primary",
    stateDir: "/state",
    shimBuildSha: "abc1234",
    storeSocket: "/tmp/store.sock",
    ...(agentReplSessionId === undefined ? {} : { agentReplSessionId }),
  });

  it("uses the daemon's exported id when it exported one", () => {
    expect(logCorrelation(env("daemon-id-1"), "0100059cb65649bc")).toEqual({
      agentReplSessionId: "daemon-id-1",
      source: "daemon_env",
    });
  });

  it("self-names from the daemon's workspace id when the daemon exported none", () => {
    const correlation = logCorrelation(env(), "0100059cb65649bc");

    expect(correlation.source).toBe("self_named");
    expect(correlation.agentReplSessionId).toBe(processIdentity("0100059cb65649bc"));
  });

  it("self-names when the daemon exported an empty string", () => {
    expect(logCorrelation(env(""), "0100059cb65649bc").source).toBe("self_named");
  });
});

describe("queryFactory", () => {
  const configDir = mkdtempSync(path.join(os.tmpdir(), "shim-query-factory-config-"));
  const cwd = mkdtempSync(path.join(os.tmpdir(), "shim-query-factory-cwd-"));
  const env: ShimEnvironment = {
    claudeConfigDir: configDir,
    stateDir: "/state",
    shimBuildSha: "abc1234",
    storeSocket: "/tmp/store.sock",
  };

  function spec(overrides: Partial<QuerySpec> = {}): QuerySpec {
    return {
      binding: { kind: "fresh", sessionId: "session-1" },
      permissionMode: "default",
      canUseTool: async (_name, input) => ({ behavior: "allow", updatedInput: input }),
      abortController: new AbortController(),
      prompt: (async function* () {})(),
      ...overrides,
    };
  }

  it("writes the workspace's trust entry BEFORE the vendor is constructed", async () => {
    // AN UNTRUSTED WORKSPACE RUNS WITH ITS PERMISSION ALLOWLISTS DROPPED, and
    // the vendor says so on stderr and nowhere else. Every agent-repl workspace
    // is a worktree no human ever opened interactively, so the shim grants the
    // trust itself — synchronously, before the query exists, which is what this
    // asserts by reading the file before the returned promise is awaited.
    // Arrange.
    const repo = mkdtempSync(path.join(os.tmpdir(), "shim-query-factory-repo-"));
    mkdirSync(path.join(repo, ".git"));
    const create = queryFactory(true, env, repo);

    // Act.
    const pending = create(spec());
    const written = JSON.parse(readFileSync(path.join(configDir, ".claude.json"), "utf8")) as {
      projects?: Record<string, Record<string, unknown>>;
    };

    // Assert.
    expect(written.projects?.[repo]?.["hasTrustDialogAccepted"]).toBe(true);
    await (await pending).interrupt?.();
  });

  it("under --fake, builds a factory that constructs the mocked vendor", async () => {
    const create = queryFactory(true, env, cwd);

    const query = await create(spec());

    expect(query).toBeDefined();
    await query.interrupt?.();
  });

  it("without --fake, builds a factory whose calls the vendor-guard refuses", async () => {
    const create = queryFactory(false, env, cwd);

    // AGENT_REPL_FORBID_VENDOR_CALLS=1 is set for every test run in this
    // worktree; constructing the real-query factory never touches the vendor,
    // only CALLING it does, which is exactly what is pinned here.
    await expect(create(spec())).rejects.toThrow();
  });

  it("without --fake, refuses with the guard's OWN error naming createRealQuery", async () => {
    // Arrange. The daemon now forces --fake on every spawn it makes under the
    // guard, so nothing should ever reach this path in a guarded run. That is
    // exactly why the refusal has to stay typed and named: if the daemon's
    // rule ever regresses, the shim must say WHICH vendor entry was tripped
    // rather than dying with an anonymous throw.
    const create = queryFactory(false, env, cwd);

    // Act.
    let raised: unknown;
    try {
      await create(spec());
    } catch (err) {
      raised = err;
    }

    // Assert.
    expect({
      typed: raised instanceof VendorCallsForbiddenError,
      site: raised instanceof Error && /blocked at: createRealQuery/.test(raised.message),
    }).toEqual({ typed: true, site: true });
  });
});

/**
 * `main()` itself — the wiring, the exit paths and the signal boundary.
 *
 * Everything outside this process is replaced: the listener, the engine, the
 * store client, the record plane and the vendor query factory are all mocked,
 * so no socket is bound, no lock is taken, no process is spawned and the SDK is
 * never reached. What is asserted is the wiring main() performs and the order
 * it performs it in, which is the part no other suite covers: the dist smoke
 * runs the built bundle, and neither it nor the integration suite can provoke
 * a quiet-drain failure or a kill that arrives before the listener exists.
 */
describe("main", () => {
  const priorArgv = process.argv;
  const priorEnv = { ...process.env };

  afterEach(() => {
    vi.doUnmock("../src/service/server.js");
    vi.doUnmock("../src/service/routes.js");
    vi.doUnmock("../src/engine/session.js");
    vi.doUnmock("../src/store/client.js");
    vi.doUnmock("../src/store/persistence.js");
    vi.doUnmock("../src/sdk/real-query.js");
    vi.doUnmock("../src/fatal.js");
    vi.resetModules();
    vi.restoreAllMocks();
    process.argv = priorArgv;
    for (const key of Object.keys(process.env)) {
      if (!(key in priorEnv)) delete process.env[key];
    }
    Object.assign(process.env, priorEnv);
  });

  /** One lifecycle record, including the method-selected level. */
  interface Lifecycle {
    readonly fields: Record<string, unknown>;
    readonly message: string;
  }

  /** Everything the harness lets a test reach inside a running main(). */
  interface Harness {
    readonly lifecycle: Lifecycle[];
    readonly fatals: unknown[];
    /** Resolves with the first fatal main() reported, so no poll is needed. */
    readonly firstFatal: Promise<unknown>;
    readonly exits: number[];
    readonly quiet: ReturnType<typeof vi.fn>;
    readonly close: ReturnType<typeof vi.fn>;
    readonly stdout: string[];
    /** The signals main() registered a handler for, without registering them. */
    readonly signals: Map<string, () => void>;
    /** Resolve to let `serve` return; until then the listener does not exist. */
    letServeReturn: () => void;
    /** Resolves once the given lifecycle outcome has been recorded. */
    reached: (outcome: string) => Promise<Lifecycle>;
    /** The engine deps main() built, available once createEngine has run. */
    engineDeps: () => Record<string, unknown>;
    run: () => void;
    /** Await main() itself; only the --version path ever returns. */
    runToCompletion: () => Promise<void>;
  }

  /**
   * Wire main() over mocks, without starting it.
   *
   * `serve` parks on a promise this test resolves, so the window BEFORE the
   * listener exists — the one `endProcess`'s pre-serving arm lives in — is
   * reachable at all.
   */
  async function harness(argv: string[], env: NodeJS.ProcessEnv = {}): Promise<Harness> {
    const lifecycle: Lifecycle[] = [];
    const fatals: unknown[] = [];
    let announceFatal: (err: unknown) => void = () => {};
    const firstFatal = new Promise<unknown>((resolve) => {
      announceFatal = resolve;
    });
    const exits: number[] = [];
    const stdout: string[] = [];
    const signals = new Map<string, () => void>();
    const waiters = new Map<string, (record: Lifecycle) => void>();
    const quiet = vi.fn(async (): Promise<void> => {});
    const close = vi.fn(async (): Promise<void> => {});
    let deps: Record<string, unknown> = {};
    let letServeReturn = (): void => {};
    const serveGate = new Promise<void>((resolve) => {
      letServeReturn = resolve;
    });

    process.argv = ["node", "/shim/dist/main.js", ...argv];
    // HERMETIC: the shim is spawned with exactly the daemon's environment, so
    // the harness replaces the runner's rather than layering over it. A suite
    // run from inside an agent-repl session inherits that session's
    // AGENT_REPL_* exports, which would otherwise decide the outcome.
    for (const key of Object.keys(process.env)) {
      if (key !== "PATH" && key !== "HOME" && key !== "TMPDIR") delete process.env[key];
    }
    Object.assign(process.env, spawnEnv(env));

    vi.resetModules();
    const recordLifecycle =
      (level: "debug" | "info" | "warn" | "error") =>
      (fields: Record<string, unknown>, message: string): void => {
        const record = { fields: { level, ...fields }, message };
        lifecycle.push(record);
        const outcome = typeof fields["outcome"] === "string" ? fields["outcome"] : "";
        waiters.get(outcome)?.(record);
      };
    vi.doMock("../src/fatal.js", () => ({
      MAIN_LIFECYCLE_OPERATION: "shim.main.lifecycle",
      MAIN_FATAL_OPERATION: "shim.main.fatal",
      MAIN_LIFECYCLE_LOGGER: {
        debug: recordLifecycle("debug"),
        info: recordLifecycle("info"),
        warn: recordLifecycle("warn"),
        error: recordLifecycle("error"),
        logVerbose: recordLifecycle("debug"),
        with: (): never => {
          throw new Error("main must not rebind its lifecycle logger");
        },
      },
      reportFatal: (err: unknown): void => {
        fatals.push(err);
        announceFatal(err);
      },
    }));
    vi.doMock("../src/service/server.js", () => ({
      serve: async () => {
        await serveGate;
        return { socketPath: "/tmp/0100059cb65649bc.sock", quiet, close };
      },
    }));
    vi.doMock("../src/service/routes.js", () => ({ shimRoutes: () => (): void => {} }));
    vi.doMock("../src/store/client.js", () => ({ createStoreClient: () => ({}) }));
    vi.doMock("../src/store/persistence.js", () => ({
      createPersistence: () => ({}),
      DEFAULT_RETRY_POLICY: { attempts: 1, baseDelayMs: 1, maxDelayMs: 1 },
    }));
    vi.doMock("../src/engine/session.js", () => ({
      createEngine: (given: Record<string, unknown>) => {
        deps = given;
        return { standDown: async (): Promise<number> => 0 };
      },
    }));

    const mainModule = await import("../src/main.js");
    vi.spyOn(process, "exit").mockImplementation(((code?: number) => {
      exits.push(code ?? 0);
      return undefined as never;
    }) as typeof process.exit);
    vi.spyOn(process.stdout, "write").mockImplementation(((chunk: unknown) => {
      stdout.push(String(chunk));
      return true;
    }));
    vi.spyOn(process, "on").mockImplementation(((name: string, handler: () => void) => {
      signals.set(name, handler);
      return process;
    }) as typeof process.on);

    return {
      lifecycle,
      fatals,
      firstFatal,
      exits,
      quiet,
      close,
      stdout,
      signals,
      letServeReturn,
      reached: (outcome) =>
        new Promise<Lifecycle>((resolve) => {
          const seen = lifecycle.find((record) => record.fields["outcome"] === outcome);
          if (seen !== undefined) {
            resolve(seen);
            return;
          }
          waiters.set(outcome, resolve);
        }),
      engineDeps: () => deps,
      run: () => {
        void mainModule.main();
      },
      runToCompletion: () => mainModule.main(),
    };
  }

  /** The serving arguments, as the daemon spells them on the command line. */
  const SERVING = ["--listen", "/tmp/0100059cb65649bc.sock", "--store-socket", "/tmp/store.sock", "--log-fd", "3"];

  it("answers --version and returns without binding anything", async () => {
    // Arrange.
    const h = await harness(["--version"]);

    // Act — --version is the one argv that makes main() return.
    await h.runToCompletion();

    // Assert — no listener, no engine, no lifecycle record at all: it is a
    // dependency-free smoke of the bundle.
    expect(h.stdout).toEqual([`${versionLine()}\n`]);
    expect(h.lifecycle).toEqual([]);
  });

  it("records the whole resolved spawn contract before it serves", async () => {
    // Arrange.
    const h = await harness(SERVING, { AGENT_REPL_SESSION_ID: "daemon-session-7" });

    // Act.
    h.run();
    const record = await h.reached("startup_arguments_validated");

    // Assert.
    expect(record.fields).toMatchObject({
      listen_socket: "/tmp/0100059cb65649bc.sock",
      store_socket: "/tmp/store.sock",
      claude_config_dir: "/accounts/primary",
      shim_build_sha: "abc1234",
      fake: false,
      agent_repl_session_id_source: "daemon_env",
      lock_dir: lockDir(),
      lock_binary: lockBinaryPath(),
    });
  });

  it("says the lock directory was relocated when the environment relocated it", async () => {
    // Arrange — the daemon's probe reads the same variable, which is what
    // makes relocation safe; a record that did not say so hides the split.
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-main-lockdir-"));
    const h = await harness(SERVING, { [LOCK_DIR_ENV]: dir });

    // Act.
    h.run();
    const record = await h.reached("startup_arguments_validated");

    // Assert.
    expect(record.fields).toMatchObject({ lock_dir: dir, lock_dir_overridden: true });
  });

  it("announces that it is serving only after the listener exists", async () => {
    // Arrange.
    const h = await harness(SERVING);

    // Act.
    h.run();
    await h.reached("startup_arguments_validated");

    // Assert — while serve() has not returned, nothing has announced readiness.
    expect(h.lifecycle.map((r) => r.fields["outcome"])).toEqual(["startup_arguments_validated"]);
    h.letServeReturn();
    const serving = await h.reached("serving");
    expect(serving.fields).toMatchObject({ listen_socket: "/tmp/0100059cb65649bc.sock" });
  });

  it("installs the signal handlers BEFORE announcing readiness", async () => {
    // Arrange — a supervisor acts on the readiness record, so a shim that
    // announced it under node's default dispositions could be killed by the
    // very SIGINT it exists to refuse.
    const h = await harness(SERVING);

    // Act.
    h.run();
    h.letServeReturn();
    await h.reached("serving");

    // Assert. SIGUSR2 is the manual keep-alive reset backdoor, installed
    // alongside the other two.
    expect([...h.signals.keys()].sort()).toEqual(["SIGINT", "SIGTERM", "SIGUSR2"]);
  });

  it("refuses SIGINT through the handler it registered", async () => {
    // Arrange.
    const h = await harness(SERVING);
    h.run();
    h.letServeReturn();
    await h.reached("serving");

    // Act.
    h.signals.get("SIGINT")?.();

    // Assert.
    const refusal = await h.reached("refused_shutdown");
    expect(refusal.fields).toMatchObject({ level: "warn", query_preserved: true });
  });

  it("exits immediately when a session end is requested before the listener exists", async () => {
    // Arrange: serve() has not returned, so KillSession's exit has no
    // response left to drain.
    const h = await harness(SERVING);
    h.run();
    await h.reached("startup_arguments_validated");

    // Act.
    (h.engineDeps()["endProcess"] as (code: number) => void)(3);

    // Assert.
    const record = await h.reached("exit_before_serving");
    expect(record.fields).toMatchObject({ level: "error", exit_code: 3 });
    expect(h.exits).toEqual([3]);
    expect(h.quiet).not.toHaveBeenCalled();
  });

  it("drains the wire before closing the listener on a clean kill", async () => {
    // Arrange.
    const h = await harness(SERVING);
    h.run();
    h.letServeReturn();
    await h.reached("serving");

    // Act.
    (h.engineDeps()["endProcess"] as (code: number) => void)(0);
    const record = await h.reached("session_killed_exit");

    // Assert — closing first would destroy the socket carrying the
    // KillSession response itself.
    expect(h.quiet.mock.invocationCallOrder[0]).toBeLessThan(h.close.mock.invocationCallOrder[0]);
    expect(record.fields).toMatchObject({ exit_code: 0 });
    expect(record.fields["level"]).toBe("info");
    expect(h.exits).toEqual([0]);
  });

  it("exits nonzero and at error when the kill left writes the store never acked", async () => {
    // Arrange.
    const h = await harness(SERVING);
    h.run();
    h.letServeReturn();
    await h.reached("serving");

    // Act.
    (h.engineDeps()["endProcess"] as (code: number) => void)(1);
    const record = await h.reached("session_killed_exit_lost_writes");

    // Assert.
    expect(record.fields).toMatchObject({ level: "error", exit_code: 1 });
    expect(h.exits).toEqual([1]);
  });

  it("ignores a second end request rather than draining twice", async () => {
    // Arrange.
    const h = await harness(SERVING);
    h.run();
    h.letServeReturn();
    await h.reached("serving");
    const end = h.engineDeps()["endProcess"] as (code: number) => void;

    // Act.
    end(0);
    end(0);
    await h.reached("session_killed_exit");

    // Assert — two concurrent drains race over one socket's responses.
    expect(h.quiet).toHaveBeenCalledTimes(1);
  });

  it("reports a failed drain as a fatal and exits 1 rather than claiming a clean end", async () => {
    // Arrange.
    const h = await harness(SERVING);
    h.run();
    h.letServeReturn();
    await h.reached("serving");
    const failure = new Error("the socket died mid-drain");
    h.quiet.mockRejectedValueOnce(failure);

    // Act.
    (h.engineDeps()["endProcess"] as (code: number) => void)(0);
    await h.firstFatal;

    // Assert — the first exit is the honest one; `process.exit` is mocked to
    // a no-op here, so the real process's termination at that point is what
    // stops anything after it from running.
    expect(h.fatals).toEqual([failure]);
    expect(h.exits[0]).toBe(1);
  });

  it("self-names its log correlation when the daemon exported no session id", async () => {
    // Arrange.
    const h = await harness(SERVING);

    // Act.
    h.run();
    const record = await h.reached("startup_arguments_validated");

    // Assert — a record with no correlation id at all is the one outcome
    // neither side can recover from.
    expect(record.fields["agent_repl_session_id_source"]).toBe("self_named");
  });

  it("hands the engine the mocked vendor query factory under --fake", async () => {
    // Arrange.
    const h = await harness([...SERVING, "--fake"]);

    // Act.
    h.run();
    const record = await h.reached("startup_arguments_validated");

    // Assert.
    expect(record.fields["fake"]).toBe(true);
    expect(h.engineDeps()["createQuery"]).toBeTypeOf("function");
  });

  it("hands the engine the keep-alive cadence the fake environment named", async () => {
    // Arrange.
    const h = await harness([...SERVING, "--fake"], { [FAKE_KEEPALIVE_INTERVAL_ENV]: "200" });

    // Act.
    h.run();
    await h.reached("startup_arguments_validated");

    // Assert.
    expect(h.engineDeps()["keepaliveIntervalMs"]).toBe(200);
  });

  it("hands the engine the watcher-conclusion budget the fake environment named", async () => {
    // Arrange.
    const h = await harness([...SERVING, "--fake"], {
      [FAKE_WATCHER_CONCLUSION_BUDGET_ENV]: "250",
    });

    // Act.
    h.run();
    await h.reached("startup_arguments_validated");

    // Assert.
    expect(h.engineDeps()["watcherConclusionBudgetMs"]).toBe(250);
  });
});

/**
 * The non-fake query factory's forwarding, over a mocked `createRealQuery`.
 *
 * The vendor guard makes the real factory unreachable, so the ARGUMENTS it is
 * handed — the ones a dropped field made unobservable before — are asserted at
 * the seam instead.
 */
describe("queryFactory forwards the whole spec to the real query", () => {
  const calls: Array<Record<string, unknown>> = [];
  let factory: typeof queryFactory;

  beforeAll(async () => {
    vi.resetModules();
    vi.doMock("../src/sdk/real-query.js", () => ({
      createRealQuery: (options: Record<string, unknown>) => {
        calls.push(options);
        return Promise.resolve({ interrupt: async (): Promise<void> => {} });
      },
    }));
    const log = await import("../src/log.js");
    log.configureLog({ fd: 3, cwd: "/ws", workspaceId: "00000000000000ee", agentReplSessionId: "main-query-factory" });
    factory = (await import("../src/main.js")).queryFactory;
  });

  beforeEach(() => {
    calls.length = 0;
  });

  afterAll(() => {
    vi.doUnmock("../src/sdk/real-query.js");
    vi.resetModules();
  });

  // A REAL DIRECTORY, because the factory now grants the workspace folder trust
  // under this account root before it constructs anything, and a root that does
  // not exist is a fault the shim raises rather than a session it runs degraded.
  const env: ShimEnvironment = {
    claudeConfigDir: mkdtempSync(path.join(os.tmpdir(), "shim-forwarding-config-")),
    stateDir: "/state",
    shimBuildSha: "abc1234",
    storeSocket: "/tmp/store.sock",
  };

  function spec(overrides: Partial<QuerySpec> = {}): QuerySpec {
    return {
      binding: { kind: "fresh", sessionId: "session-1" },
      permissionMode: "default",
      canUseTool: async (_name, input) => ({ behavior: "allow", updatedInput: input }),
      abortController: new AbortController(),
      prompt: (async function* () {})(),
      ...overrides,
    };
  }

  it("states the model when the session named one", async () => {
    // Arrange: the describe block's mocked real-query seam.
    // Act.
    await factory(false, env, "/ws")(spec({ model: "claude-opus-5" }));

    // Assert.
    expect(calls[0]).toMatchObject({ model: "claude-opus-5" });
  });

  it("omits the model entirely when the session named none", async () => {
    // Arrange: the describe block's mocked real-query seam.
    // Act.
    await factory(false, env, "/ws")(spec());

    // Assert.
    expect(calls[0]).not.toHaveProperty("model");
  });

  it("forwards the keep-alive rewind target so the rewind is observable at the vendor", async () => {
    // Arrange: the describe block's mocked real-query seam.
    // Act.
    await factory(false, env, "/ws")(
      spec({ binding: { kind: "resume", resumeSessionId: "vendor-old" }, resumeSessionAt: "uuid-9" }),
    );

    // Assert — the shim's own log saying what it intended is not evidence the
    // value arrived.
    expect(calls[0]).toMatchObject({ resumeSessionAt: "uuid-9" });
  });

  it("omits the rewind target when the turn asked for no rewind", async () => {
    // Arrange: the describe block's mocked real-query seam.
    // Act.
    await factory(false, env, "/ws")(spec());

    // Assert.
    expect(calls[0]).not.toHaveProperty("resumeSessionAt");
  });
});

describe("queryFactory under --fake", () => {
  const configDir = mkdtempSync(path.join(os.tmpdir(), "shim-fake-factory-config-"));
  const cwd = mkdtempSync(path.join(os.tmpdir(), "shim-fake-factory-cwd-"));
  const env: ShimEnvironment = {
    claudeConfigDir: configDir,
    stateDir: "/state",
    shimBuildSha: "abc1234",
    storeSocket: "/tmp/store.sock",
  };

  it("constructs the mocked vendor for a RESUME binding carrying a rewind target", async () => {
    // Arrange.
    const create = queryFactory(true, env, cwd);

    // Act.
    const query = await create({
      binding: { kind: "resume", resumeSessionId: "vendor-old" },
      permissionMode: "plan",
      canUseTool: async (_name, input) => ({ behavior: "allow", updatedInput: input }),
      abortController: new AbortController(),
      prompt: (async function* () {})(),
      model: "claude-opus-5",
      resumeSessionAt: "uuid-9",
    });

    // Assert.
    expect(query).toBeDefined();
    await query.interrupt?.();
  });
});
