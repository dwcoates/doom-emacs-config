/**
 * The process shell: argv, environment, --version, the signal boundary, and the
 * lock directory.
 *
 * The suite deliberately does NOT call `main()`. main() binds a socket, takes a
 * kernel lock and then parks forever, so the parts worth asserting are exported
 * as pure functions and asserted directly; the end-to-end behavior of the built
 * bundle is the dist smoke's job.
 */
import { describe, expect, it, vi } from "vitest";
import { LOCK_DIR_ENV, lockDir } from "../src/locks.js";
import {
  DEFAULT_STATE_DIR_NAME,
  OWNED_ENV,
  STORE_SOCKET_ENV,
  packageVersion,
  parseArgs,
  processIdentity,
  requireServingArgs,
  resolveEnvironment,
  shutdownSignalHandlers,
  versionLine,
  type CliArgs,
} from "../src/main.js";
import type { Engine } from "../src/engine/engine.js";

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
    listen: "/tmp/shim.sock",
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
      "/tmp/shim.sock",
      "--store-socket",
      "/tmp/store.sock",
      "--log-fd",
      "3",
      "--fake",
    ]);

    // Assert.
    expect(args).toEqual({
      listen: "/tmp/shim.sock",
      storeSocket: "/tmp/store.sock",
      logFd: 3,
      fake: true,
      version: false,
    });
  });

  it("defaults --fake off", () => {
    // Arrange, Act.
    const args = parseArgs(["--listen", "/tmp/shim.sock", "--log-fd", "3"]);

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
    const args = parseArgs(["--listen", "/tmp/shim.sock"]);

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
  it("correlates with the workspace lock file and every log record", () => {
    // Arrange, Act.
    const identity = processIdentity("/ws/feature");

    // Assert.
    expect(identity).toMatch(/^shim-[0-9a-f]{8}-\d+$/);
  });

  it("gives two workspaces two identities", () => {
    // Arrange, Act.
    const first = processIdentity("/ws/one");
    const second = processIdentity("/ws/two");

    // Assert.
    expect(first).not.toBe(second);
  });
});

/** An engine that records its stand-down, and can be made to fail it. */
function standDownEngine(failure?: Error): { engine: Engine; calls: string[] } {
  const calls: string[] = [];
  const engine = {
    standDown: async (reason: string): Promise<void> => {
      calls.push(reason);
      if (failure !== undefined) throw failure;
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

  it("logs the SIGINT refusal at ERROR, because it means something is misconfigured", () => {
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
    expect(written).toContain('"level":"error"');
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
