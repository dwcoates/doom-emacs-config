/**
 * test/integration-support/harness.ts — spawn the REAL shim bundle and dial it.
 *
 * # What an integration test here actually exercises
 *
 * `dist/main.js`, as a process, spawned the way the daemon spawns it: the real
 * argv, the real environment, a real unix socket, a real inherited fd 3, and a
 * real Connect client on the other end. The vendor is the MOCKED one (`--fake`,
 * `src/fake/`), and the store is the in-process fake (`test/fakes/store-server`).
 * No other real system is ever involved, which is what keeps these integration
 * tests rather than e2e tests.
 *
 * # Every wait is LEVEL-THEN-EDGE, and never a sleep
 *
 * No wait here is a timed guess. "The shim is serving" is a LOG RECORD the shim
 * writes when its listener binds; "the shim exited" is the child's own exit
 * event; "a frame arrived" is the stream yielding. A sleep would turn each of
 * those into a race that passes on a fast machine.
 *
 * The file-backed waits are not PURELY edge-driven, and saying so would be a
 * lie the flakes already caught: the shape is check the level, install the
 * watcher, re-check the level — and, under `fs.watch` on macOS, a bounded 20 ms
 * re-check alongside it (`redrain.ts`), because FSEvents can drop an append
 * notification outright and a lost edge would otherwise hang the wait for the
 * whole test budget. That re-check is a BACKSTOP FOR A MISSING NOTIFICATION,
 * never the mechanism a wait is expected to succeed by, and it sequences
 * nothing: the condition it re-reads is the same level the edge would have
 * announced.
 *
 * # Isolation
 *
 * Every spawn gets its own temp root, and every path the shim can write to
 * points inside it: the vendor account dir, the state dir, the LOCK dir (so a
 * test's workspace lock cannot contend with the developer's own running shim),
 * the mock's spool root, and the workspace cwd. `AGENT_REPL_FORBID_VENDOR_CALLS`
 * is always set — nothing here may reach the real vendor.
 */
import { spawn, type ChildProcess } from "node:child_process";
import { closeSync, mkdirSync, mkdtempSync, openSync, rmSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import { LogTail, StreamRecords, type RecordSink } from "./log.js";
import { createShimClients, type ShimClients } from "./client.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The package root — `test/integration-support/..` twice up. */
export const PACKAGE_ROOT = path.resolve(here, "..", "..");

/** The bundle the daemon spawns, and the one every test here spawns. */
export const SHIM_BUNDLE = path.join(PACKAGE_ROOT, "dist", "main.js");

/**
 * The REAL `shim-lock` binary, built beside the bundle by `pretest:integration`.
 *
 * The kernel claim is a child process (agent-shim/shim-lock) because Node
 * cannot take a flock, so a spawned shim here takes REAL locks with the REAL
 * holder — the override exists so it is the binary this checkout just built
 * rather than whatever is deployed on the machine.
 */
export const SHIM_LOCK_BINARY = path.join(PACKAGE_ROOT, "dist", "shim-lock");

/** The build identity every spawned shim reports on `SessionStarted`. */
export const ITEST_BUILD_SHA = "itest";

/** Distinguishes one spawn's `AGENT_REPL_SESSION_ID` from the next's. */
let spawnCounter = 0;

/** Every path a spawned shim may write to, all inside one temp root. */
export interface ShimDirectories {
  /** The temp root; removed whole at cleanup. */
  readonly root: string;
  /** The shim's cwd — the "workspace". */
  readonly workspace: string;
  /** `CLAUDE_CONFIG_DIR`: the vendor account root the mock writes under. */
  readonly configDir: string;
  /** `AGENT_REPL_STATE_DIR`. */
  readonly stateDir: string;
  /** `AGENT_REPL_LOCK_DIR`: where the two kernel lock files appear. */
  readonly lockDir: string;
  /** `AGENT_REPL_FAKE_SPOOL_ROOT`: where the mock writes task spools. */
  readonly spoolRoot: string;
  /** The file fd 3 is bound to (unless the spawn asked for a pipe). */
  readonly logPath: string;
  /** The shim.v1 socket. */
  readonly listen: string;
  /** The store.v1 socket. */
  readonly storeSocket: string;
}

/** How the process ended. */
export interface ShimExit {
  readonly code: number | null;
  readonly signal: NodeJS.Signals | null;
}

/** Knobs a test turns before the shim starts. */
export interface SpawnShimOptions {
  /**
   * Environment overrides layered onto the standard spawn env. A key mapped to
   * `undefined` is REMOVED, which is how the "required variable is missing"
   * refusals are provoked.
   */
  readonly env?: Readonly<Record<string, string | undefined>>;
  /** Replace the whole argv (the flags after `dist/main.js`). */
  readonly argv?: readonly string[];
  /** Extra argv appended to the standard flags. */
  readonly extraArgv?: readonly string[];
  /** Reuse another spawn's directories — how a second shim contends for a lock. */
  readonly reuse?: ShimDirectories;
  /** Start the fake store. Default true. */
  readonly store?: boolean;
  /**
   * Bind fd 3 to a PIPE rather than a file, so the test can close the read end
   * and poison the sink. The log tail is unavailable in that mode.
   */
  readonly logPipe?: boolean;
  /**
   * Wait for the shim's "serving" record before resolving. Default true; a
   * spawn expected to REFUSE to start sets it false and awaits `exited`.
   */
  readonly awaitServing?: boolean;
}

/** One spawned shim, and everything a test needs to interrogate it. */
export interface ShimHandle {
  readonly dirs: ShimDirectories;
  readonly child: ChildProcess;
  /** The fake store, when one was started. */
  readonly store: FakeStore | null;
  /** The shim's records, whether fd 3 is a file or a pipe. */
  readonly log: RecordSink;
  /** The parent's read end of fd 3, when fd 3 is a pipe (else null). */
  readonly logPipe: NodeJS.ReadableStream | null;
  /** shim.v1 clients over the one socket, in both dialects. */
  readonly clients: ShimClients;
  /** Everything the process wrote to stdout and stderr, as it arrives. */
  stderr(): string;
  /** Resolves when the process exits. */
  readonly exited: Promise<ShimExit>;
  /** Send a signal to the process. */
  signal(signal: NodeJS.Signals): void;
  /** SIGTERM and await the exit — the graceful path a test asserts on. */
  standDown(): Promise<ShimExit>;
}

/** Every handle spawned in the current test, torn down by {@link cleanupShims}. */
const live: ShimHandle[] = [];

/** Create the temp tree one spawn writes into. */
export function makeDirectories(): ShimDirectories {
  const root = mkdtempSync(path.join(os.tmpdir(), "shim-itest-"));
  const dirs: ShimDirectories = {
    root,
    workspace: path.join(root, "workspace"),
    configDir: path.join(root, "account"),
    stateDir: path.join(root, "state"),
    lockDir: path.join(root, "locks"),
    spoolRoot: path.join(root, "spool"),
    logPath: path.join(root, "shim.log"),
    // NAMED AFTER A WORKSPACE ID, exactly as the daemon names it: the shim
    // reads `workspace_id` off this basename and refuses a socket that does not
    // spell one.
    listen: path.join(root, "00000000000000a1.sock"),
    storeSocket: path.join(root, "store.sock"),
  };
  for (const dir of [dirs.workspace, dirs.configDir, dirs.stateDir, dirs.lockDir, dirs.spoolRoot]) {
    mkdirSync(dir, { recursive: true });
  }
  closeSync(openSync(dirs.logPath, "a"));
  return dirs;
}

/**
 * Spawn `dist/main.js` and resolve once it is serving.
 *
 * The handle is registered for teardown before anything can fail, so a spawn
 * that refuses to start still gets its temp tree removed.
 */
export async function spawnShim(options: SpawnShimOptions = {}): Promise<ShimHandle> {
  const dirs = options.reuse ?? makeDirectories();
  const store =
    options.store === false || options.reuse !== undefined
      ? null
      : await startFakeStore(dirs.storeSocket);

  const argv =
    options.argv !== undefined
      ? [...options.argv]
      : [
          "--listen",
          dirs.listen,
          "--store-socket",
          dirs.storeSocket,
          "--log-fd",
          "3",
          "--fake",
          ...(options.extraArgv ?? []),
        ];

  // THE CLIENTS DIAL THE SOCKET THIS SPAWN ACTUALLY BOUND, not the directory
  // set's default. A second shim over reused directories listens on its own
  // path, and clients built from `dirs.listen` would silently reach the FIRST
  // shim — every assertion about the second one would then be about the wrong
  // process.
  const listenAt = argv[argv.indexOf("--listen") + 1] ?? dirs.listen;

  const env: NodeJS.ProcessEnv = {
    ...process.env,
    CLAUDE_CONFIG_DIR: dirs.configDir,
    AGENT_REPL_OWNED: "1",
    AGENT_REPL_STATE_DIR: dirs.stateDir,
    AGENT_REPL_LOCK_DIR: dirs.lockDir,
    AGENT_REPL_SHIM_LOCK_BIN: SHIM_LOCK_BINARY,
    AGENT_REPL_FAKE_SPOOL_ROOT: dirs.spoolRoot,
    SHIM_BUILD_SHA: ITEST_BUILD_SHA,
    AGENT_REPL_SESSION_ID: `host-itest-${++spawnCounter}`,
    AGENT_REPL_FORBID_VENDOR_CALLS: "1",
    // Integration assertions consume the production remediation trace. The
    // runtime default remains info and is pinned independently in log.test.ts.
    AGENT_REPL_LOG_LEVEL: "debug",
    // THE TWO PRODUCTION WINDOWS, SCALED — not weakened. Both are `--fake`-only
    // overrides the shim refuses for a real session (`src/main.ts`), so the
    // production defaults are untouched; what changes is only how long a test
    // that legitimately SPENDS one of them sits idle.
    //
    // The quiet-drain budget is a LAST RESORT bound. The forced-kill scenarios
    // leave a stream deliberately un-concluded, so they spend the whole thing;
    // at the production 5,000ms that made them the two slowest tests in the
    // suite. What they assert is that the exit WAITS for the wire and then ends
    // anyway, which a 500ms budget proves exactly as well — and every other
    // test's exit settles on its responses' own close events in microseconds
    // and never reaches this bound at all.
    AGENT_REPL_FAKE_EXIT_QUIET_BUDGET_MS: "500",
    // The teardown's per-tail conclusion budget, the window the forced-kill
    // scenarios actually spend: they leave a tail whose consumer stopped
    // pulling, so the teardown waits out the whole thing. What those tests
    // assert is the ORDERING — the tail's terminal reaches the consumer before
    // the process goes — which 500ms establishes exactly as well as 5,000ms.
    AGENT_REPL_FAKE_WATCHER_CONCLUSION_BUDGET_MS: "500",
    // The retry schedule keeps its SHAPE — four rising delays, five attempts
    // before the failure is declared persistent, then the last step repeated —
    // and only shrinks the waiting, 10x down from the production
    // [50, 200, 800, 3000]. What the outage scenarios assert is the attempt
    // count, the loud exhaustion, that nothing is dropped and the preserved
    // order, none of which is a function of the idle time between attempts;
    // `maxAttempts` and the batch/backlog bounds are not reachable from the
    // environment at all, precisely so this cannot become a way to weaken
    // them. The 405ms total still leaves the transient-outage test, which
    // restores the store within one event-loop turn of an awaited rpc, orders
    // of magnitude of margin before a batch could exhaust.
    AGENT_REPL_FAKE_STORE_BACKOFF_MS: "5,20,80,300",
  };
  for (const [key, value] of Object.entries(options.env ?? {})) {
    if (value === undefined) delete env[key];
    else env[key] = value;
  }

  const logFd = options.logPipe === true ? undefined : openSync(dirs.logPath, "a");
  const child = spawn(process.execPath, [SHIM_BUNDLE, ...argv], {
    cwd: dirs.workspace,
    env,
    stdio: ["ignore", "pipe", "pipe", logFd ?? "pipe"],
  });
  if (logFd !== undefined) closeSync(logFd);

  let stderrText = "";
  child.stdout?.on("data", (chunk: Buffer) => {
    stderrText += chunk.toString("utf8");
  });
  child.stderr?.on("data", (chunk: Buffer) => {
    stderrText += chunk.toString("utf8");
  });

  const exited = new Promise<ShimExit>((resolve) => {
    child.on("exit", (code, signal) => resolve({ code, signal }));
  });

  const pipeEnd =
    options.logPipe === true ? ((child.stdio[3] as NodeJS.ReadableStream | null) ?? null) : null;
  // A pipe's records arrive as data events; a file's are tailed on fs.watch.
  // Either way the suites read one interface, so only the poisoned-sink test
  // knows which sink it got.
  const log: RecordSink =
    pipeEnd === null ? LogTail.open(dirs.logPath) : new StreamRecords(pipeEnd);
  const handle: ShimHandle = {
    dirs,
    child,
    store,
    log,
    logPipe: pipeEnd,
    clients: createShimClients(listenAt),
    stderr: () => stderrText,
    exited,
    signal: (signal) => {
      child.kill(signal);
    },
    standDown: async () => {
      child.kill("SIGTERM");
      return exited;
    },
  };
  live.push(handle);

  if (options.awaitServing !== false) {
    // The listener's own record, written the moment it is accepting. Racing it
    // against the exit means a shim that DIED during startup reports as the
    // death it was rather than as a hang.
    // MATCHED ON THIS CHILD'S PID. A reused directory set replays the previous
    // shim's log file, whose own "serving" record is still in it; without the
    // pid the wait would settle on the DEAD shim's record and the test would
    // then dial a socket the exit had already removed.
    await Promise.race([
      log
        .record((record) => record.context.outcome === "serving" && record.pid === child.pid)
        .then(() => undefined),
      exited.then((exit) => {
        throw new Error(
          `shim exited before it served (code ${String(exit.code)}, signal ${String(exit.signal)}):\n${stderrText}`,
        );
      }),
    ]);
  }
  return handle;
}

/**
 * Tear every shim this test spawned down, deterministically.
 *
 * SIGKILL and not SIGTERM: cleanup must not depend on the graceful path
 * working, because a test whose subject is a BROKEN graceful path would then
 * hang the whole file instead of failing. Tests that assert the graceful exit
 * call {@link ShimHandle.standDown} themselves and have already awaited it.
 */
export async function cleanupShims(): Promise<void> {
  const handles = live.splice(0);
  await Promise.all(
    handles.map(async (handle) => {
      handle.log.close();
      if (handle.child.exitCode === null && handle.child.signalCode === null) {
        handle.child.kill("SIGKILL");
        await handle.exited;
      }
      await handle.store?.close();
      // AGENT_REPL_ITEST_KEEP leaves the temp tree in place so a failing run's
      // shim log can be read afterwards. Never set in CI: the trees are large
      // and one per spawn.
      if (process.env.AGENT_REPL_ITEST_KEEP === "1") {
        process.stderr.write(`itest: kept ${handle.dirs.root}\n`);
        return;
      }
      rmSync(handle.dirs.root, { recursive: true, force: true });
    }),
  );
}

/** Run the shim bundle to completion with the given argv, capturing its output. */
export async function runShim(
  argv: readonly string[],
  env: Readonly<Record<string, string | undefined>> = {},
): Promise<{ readonly exit: ShimExit; readonly stdout: string; readonly stderr: string }> {
  const childEnv: NodeJS.ProcessEnv = { ...process.env, AGENT_REPL_FORBID_VENDOR_CALLS: "1" };
  for (const [key, value] of Object.entries(env)) {
    if (value === undefined) delete childEnv[key];
    else childEnv[key] = value;
  }
  const child = spawn(process.execPath, [SHIM_BUNDLE, ...argv], { env: childEnv });
  let stdout = "";
  let stderr = "";
  child.stdout.on("data", (chunk: Buffer) => {
    stdout += chunk.toString("utf8");
  });
  child.stderr.on("data", (chunk: Buffer) => {
    stderr += chunk.toString("utf8");
  });
  const exit = await new Promise<ShimExit>((resolve) => {
    child.on("exit", (code, signal) => resolve({ code, signal }));
  });
  return { exit, stdout, stderr };
}
