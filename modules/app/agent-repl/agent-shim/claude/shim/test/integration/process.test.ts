/**
 * test/integration/process.test.ts — the PROCESS SHELL contract.
 *
 * Everything here is about the shim as a process rather than as a service: the
 * argv it accepts, the environment it refuses to run without, the two kernel
 * locks, the signals, and the durable log sink. None of it is reachable through
 * an rpc, which is exactly why it needs a suite that spawns the real bundle.
 *
 * # Where a startup refusal is observable
 *
 * The environment is validated BEFORE the log is configured (`main.ts`'s
 * startup order — `--version` and argv must not touch anything, and the log fd
 * is only usable once the arguments are known good). So a refusal's record has
 * nowhere durable to go and lands on stderr through `log.ts`'s emergency path.
 * That is not a workaround: the refusal must be legible to whoever spawned the
 * process, and the only channel that always exists is stderr.
 */
import { existsSync, readdirSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, test } from "vitest";
import { workspaceLockKey } from "../../src/locks.js";
import { shimv1 } from "../../src/proto.js";
import { create } from "@bufbuild/protobuf";
import { Code } from "@connectrpc/connect";
import {
  cleanupShims,
  ITEST_BUILD_SHA,
  SHIM_LOCK_BINARY,
  makeDirectories,
  runShim,
  spawnShim,
} from "../integration-support/harness.js";
import { parseRecords } from "../integration-support/log.js";
import {
  connectCode,
  freshSession,
  openSessionUpdates,
  readHistoryFirst,
} from "../integration-support/client.js";
import {
  sessionStarted,
  sessionUpdate,
  startSessionCause,
} from "../integration-support/expect.js";
import { workspaceRealPath } from "../integration-support/vendor.js";

afterEach(cleanupShims);

/** The env a serving shim needs, for the spawns that build it by hand. */
function servingEnv(dirs: ReturnType<typeof makeDirectories>): Record<string, string> {
  return {
    CLAUDE_CONFIG_DIR: dirs.configDir,
    AGENT_REPL_OWNED: "1",
    AGENT_REPL_STATE_DIR: dirs.stateDir,
    AGENT_REPL_LOCK_DIR: dirs.lockDir,
    AGENT_REPL_SHIM_LOCK_BIN: SHIM_LOCK_BINARY,
    SHIM_BUILD_SHA: ITEST_BUILD_SHA,
    AGENT_REPL_STORE_SOCKET: dirs.storeSocket,
  };
}

describe("--version", () => {
  test("prints the version and exits 0 before any socket, lock or SDK import", async () => {
    // Arrange: no environment at all — the point of the flag is that it needs
    // none. Every required variable is deliberately absent.
    const bare = {
      CLAUDE_CONFIG_DIR: undefined,
      AGENT_REPL_OWNED: undefined,
      SHIM_BUILD_SHA: undefined,
      AGENT_REPL_STORE_SOCKET: undefined,
    };

    // Act.
    const run = await runShim(["--version"], bare);

    // Assert: a version line, a clean exit, and nothing bound anywhere.
    expect(run.exit.code).toBe(0);
    expect(run.stdout.trim()).toMatch(/^claude-shim /);
  });
});

describe("required environment", () => {
  test("a missing AGENT_REPL_OWNED refuses to start", async () => {
    const dirs = makeDirectories();
    const env = { ...servingEnv(dirs), AGENT_REPL_OWNED: undefined };

    const run = await runShim(
      ["--listen", dirs.listen, "--store-socket", dirs.storeSocket, "--log-fd", "3", "--fake"],
      env,
    );

    expect(run.exit.code).not.toBe(0);
    expect(run.stderr).toContain("AGENT_REPL_OWNED");
  });

  test("a missing CLAUDE_CONFIG_DIR refuses to start", async () => {
    const dirs = makeDirectories();
    const env = { ...servingEnv(dirs), CLAUDE_CONFIG_DIR: undefined };

    const run = await runShim(
      ["--listen", dirs.listen, "--store-socket", dirs.storeSocket, "--log-fd", "3", "--fake"],
      env,
    );

    expect(run.exit.code).not.toBe(0);
    expect(run.stderr).toContain("CLAUDE_CONFIG_DIR");
  });

  test("a missing SHIM_BUILD_SHA refuses to start", async () => {
    const dirs = makeDirectories();
    const env = { ...servingEnv(dirs), SHIM_BUILD_SHA: undefined };

    const run = await runShim(
      ["--listen", dirs.listen, "--store-socket", dirs.storeSocket, "--log-fd", "3", "--fake"],
      env,
    );

    expect(run.exit.code).not.toBe(0);
    expect(run.stderr).toContain("SHIM_BUILD_SHA");
  });

  test("a startup refusal is a RECORD, not just a message", async () => {
    // The daemon reads the shim's record, not its prose: a refusal that logged
    // nothing structured would be invisible to the supervisor that spawned it.
    const dirs = makeDirectories();
    const env = { ...servingEnv(dirs), AGENT_REPL_OWNED: undefined };

    const run = await runShim(
      ["--listen", dirs.listen, "--store-socket", dirs.storeSocket, "--log-fd", "3", "--fake"],
      env,
    );

    const records = parseRecords(run.stderr);
    expect(records.length).toBeGreaterThan(0);
    expect(records.some((record) => record.level === "error")).toBe(true);
  });
});

describe("the spawn contract's argv", () => {
  // ONE TEST PER LEGACY FLAG. A shim that shrugged at an unknown flag would run
  // with the caller's intent silently discarded, and each of these was a real
  // flag once, so each is its own opportunity for a stale daemon to spawn a
  // shim that half-obeys it.
  for (const legacy of [
    ["--session-id", "abc"],
    ["--model", "claude-opus-5"],
    ["--resume", "abc"],
    ["--claude-bin", "/usr/bin/claude"],
    ["--daemon-socket", "/tmp/daemon.sock"],
  ]) {
    test(`the legacy flag ${String(legacy[0])} is refused`, async () => {
      const dirs = makeDirectories();

      const run = await runShim(
        [
          "--listen",
          dirs.listen,
          "--store-socket",
          dirs.storeSocket,
          "--log-fd",
          "3",
          "--fake",
          ...legacy,
        ],
        servingEnv(dirs),
      );

      expect(run.exit.code).not.toBe(0);
      expect(run.stderr).toContain(String(legacy[0]));
    });
  }

  test("--log-fd other than 3 is refused", async () => {
    // The durable sink is INHERITED fd 3; accepting another number would let a
    // caller point the record at whatever happened to be open — including the
    // stderr pipe whose death this design exists to survive.
    const dirs = makeDirectories();

    const run = await runShim(
      ["--listen", dirs.listen, "--store-socket", dirs.storeSocket, "--log-fd", "4", "--fake"],
      servingEnv(dirs),
    );

    expect(run.exit.code).not.toBe(0);
    expect(run.stderr).toContain("--log-fd");
  });
});

describe("an unspawnable lock holder", () => {
  test("StartSession is refused lock_holder_unavailable, never conversation_owned", async () => {
    // Arrange: nobody owns the conversation; the shim's own helper is missing.
    const missing = path.join(os.tmpdir(), "no-such-shim-lock-for-itest");
    const shim = await spawnShim({ env: { AGENT_REPL_SHIM_LOCK_BIN: missing } });

    // Act.
    const refused = await shim.clients.h1.startSession(freshSession());

    // Assert.
    expect(startSessionCause(refused)).toBe("lockHolderUnavailable");
  });

  test("the refusal names the binary the shim could not spawn", async () => {
    // Arrange.
    const missing = path.join(os.tmpdir(), "no-such-shim-lock-for-itest");
    const shim = await spawnShim({ env: { AGENT_REPL_SHIM_LOCK_BIN: missing } });

    // Act.
    const refused = await shim.clients.h1.startSession(freshSession());

    // Assert.
    const cause = refused.result.case === "failure" ? refused.result.value.cause : undefined;
    expect(cause?.case === "lockHolderUnavailable" ? cause.value.binary : undefined).toBe(missing);
  });
});

describe("the workspace lock", () => {
  /** A second shim over the first's directories, on its own socket. */
  async function secondShimOver(
    first: Awaited<ReturnType<typeof spawnShim>>,
  ): Promise<Awaited<ReturnType<typeof spawnShim>>> {
    return spawnShim({
      reuse: first.dirs,
      argv: [
        "--listen",
        path.join(first.dirs.root, "00000000000000a3.sock"),
        "--store-socket",
        first.dirs.storeSocket,
        "--log-fd",
        "3",
        "--fake",
      ],
    });
  }

  test("a second shim over one workspace SERVES, because an inert shim owns nothing", async () => {
    // THE PRELAUNCH RULE. A shim with no session excludes nobody, so the
    // daemon can stand a replacement up beside the shim it is retiring. A
    // startup-time workspace claim would wedge the newcomer behind a lock the
    // live shim only drops when it dies.
    const first = await spawnShim();
    await first.clients.h1.startSession(freshSession());

    const second = await secondShimOver(first);

    // Serving: `spawnShim` resolves on the second process's OWN serving record.
    expect(second.child.exitCode).toBeNull();
  });

  test("the inert second shim's StartSession is refused conversation_owned", async () => {
    // The exclusion did not vanish, it MOVED: the claim is made where the
    // conversation is, and the refusal names the lock that proved it.
    const first = await spawnShim();
    await first.clients.h1.startSession(freshSession());
    const second = await secondShimOver(first);

    const refused = await second.clients.h1.startSession(freshSession());

    expect(startSessionCause(refused)).toBe("conversationOwned");
  });

  test("the refused shim stays inert and keeps serving", async () => {
    // A refusal is not a death: the shim is still the daemon's to use once the
    // conversation is free, and killing it would throw away a warm process.
    const first = await spawnShim();
    await first.clients.h1.startSession(freshSession());
    const second = await secondShimOver(first);
    await second.clients.h1.startSession(freshSession());

    const again = await second.clients.h1.startSession(freshSession());

    expect(startSessionCause(again)).toBe("conversationOwned");
    expect(second.child.exitCode).toBeNull();
  });

  test("the refused shim released NOTHING it did not hold: the first shim still owns the lock", async () => {
    // The session claim is taken before the workspace claim, so the refusal
    // path hands one lock back. It must hand back only its OWN — a release that
    // reached the incumbent's lock file would unlock a live conversation.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const second = await secondShimOver(first);
    await second.clients.h1.startSession(freshSession());

    const key = workspaceLockKey(workspaceRealPath(first.dirs));
    expect(existsSync(path.join(first.dirs.lockDir, `workspace-${key}.lock`))).toBe(true);
    // And the incumbent is unharmed: its session still answers.
    const stillOwned = await first.clients.h1.startSession(freshSession());
    expect(startSessionCause(stillOwned)).toBe("alreadyStarted");
    expect(started.vendorSessionId).not.toBe("");
  });

  test("SIGTERM on an INERT shim exits 0 with no lock activity at all", async () => {
    // An inert shim's stand-down has nothing to release, and a teardown that
    // logged a release would mean it had been holding one.
    const shim = await spawnShim();

    const exit = await shim.standDown();

    expect(exit.code).toBe(0);
    const lockRecords = shim.log
      .records()
      .filter((record) => record.operation === "shim.session-lock.lifecycle");
    expect(lockRecords).toEqual([]);
  });

  test("KillSession on an INERT shim is refused no_session and holds no lock", async () => {
    // There is no session to kill, so the verb refuses; the point here is that
    // the refusal path never touched a lock either.
    const shim = await spawnShim();

    const response = await shim.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: false }),
    );

    expect(response.result.case).toBe("failure");
    expect(readdirSync(shim.dirs.lockDir)).toEqual([]);
  });

  test("no lock file of either kind exists before StartSession", async () => {
    // The whole inert contract in one assertion: the directory is empty.
    const shim = await spawnShim();

    expect(readdirSync(shim.dirs.lockDir)).toEqual([]);
  });

  test("AGENT_REPL_LOCK_DIR is honored: the workspace lock file appears there once a session exists", async () => {
    // The lock directory is a CROSS-SYSTEM rendezvous (the daemon probes the
    // workspace lock by path), so the override is what lets a test run private
    // locks instead of contending with the developer's own running shim.
    const shim = await spawnShim();

    await shim.clients.h1.startSession(freshSession());

    const key = workspaceLockKey(workspaceRealPath(shim.dirs));
    expect(existsSync(path.join(shim.dirs.lockDir, `workspace-${key}.lock`))).toBe(true);
  });

  test("the session lock appears once StartSession has minted a vendor id", async () => {
    // The session lock is keyed by the vendor session id, which does not exist
    // until StartSession pre-mints it.
    const shim = await spawnShim();

    const before = readdirSync(shim.dirs.lockDir);
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const after = readdirSync(shim.dirs.lockDir);

    expect(before.some((name) => name.startsWith("session-"))).toBe(false);
    expect(after).toContain(`session-${started.vendorSessionId}.lock`);
  });
});

describe("--listen over an existing socket file", () => {
  test("a STALE socket file is unlinked and bound", async () => {
    // SIGKILL leaves the file behind and `listen` on it fails EADDRINUSE
    // forever after, so a shim that refused every existing file could never be
    // restarted after a hard kill. "Stale" is a VERDICT, not an assumption: the
    // probe dials it and only unlinks what refuses the connection.
    const first = await spawnShim();
    first.signal("SIGKILL");
    await first.exited;
    expect(existsSync(first.dirs.listen)).toBe(true);

    const second = await spawnShim({ reuse: first.dirs });

    // Serving on the very path the corpse left: `spawnShim` resolved on this
    // process's own serving record.
    const started = sessionStarted(await second.clients.h1.startSession(freshSession()));
    expect(started.vendorSessionId).not.toBe("");
    const unlinked = second.log
      .records()
      .find((record) => record.context.why === "stale predecessor");
    expect(unlinked?.context.socket_path).toBe(first.dirs.listen);
  });

  test("a LIVE socket file is REFUSED, and the incumbent is untouched", async () => {
    // The opposite verdict, and the reason the probe exists at all: unlinking a
    // socket somebody is listening on would leave the incumbent serving a path
    // nothing can reach, and this shim bound over the top of it.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));

    const second = await spawnShim({
      reuse: first.dirs,
      awaitServing: false,
      argv: [
        "--listen",
        first.dirs.listen,
        "--store-socket",
        first.dirs.storeSocket,
        "--log-fd",
        "3",
        "--fake",
      ],
    });
    const exit = await second.exited;

    expect(exit.code).not.toBe(0);
    expect(second.stderr()).toContain("live listener");
    // THE INCUMBENT IS UNHARMED: its socket still answers and still holds its
    // session, which a shim that had unlinked and rebound would have destroyed.
    const still = await first.clients.h1.startSession(freshSession());
    expect(startSessionCause(still)).toBe("alreadyStarted");
    expect(started.vendorSessionId).not.toBe("");
  });
});

describe("signals", () => {
  test("SIGINT is refused, logged as a named decision, and the shim keeps serving", async () => {
    // A shim may be spawned under an attached terminal, and a Ctrl-C there must
    // not end a turn the user is watching.
    const shim = await spawnShim();

    shim.signal("SIGINT");
    const refusal = await shim.log.record(
      (record) => record.context.signal === "SIGINT" && record.context.outcome === "refused_shutdown",
    );

    expect(refusal.level).toBe("warn");
    // Still serving: a legal rpc still answers after the refused signal.
    const still = await shim.clients.h1.startSession(freshSession());
    expect(still.result.case).toBe("success");
    expect(shim.child.exitCode).toBeNull();
  });

  test("SIGTERM with no session exits 0 with the stand-down record", async () => {
    const shim = await spawnShim();

    const exit = await shim.standDown();

    expect(exit.code).toBe(0);
    // AWAITED, not scanned: the child's exit event and the last bytes of its
    // log arriving in the parent's view of the file are two different moments,
    // and a synchronous scan of the second from the first is a race that fails
    // on whichever machine loses it.
    const stoodDown = await shim.log.record(
      (record) => record.context.outcome === "graceful_stand_down_complete",
    );
    expect(stoodDown.context.exit_code).toBe(0);
  });
});

describe("the durable log sink", () => {
  test("AGENT_REPL_SESSION_ID names the records when the daemon set it", async () => {
    // Log correlation ONLY: the daemon's host session id is never a session
    // fact (StartSession stays the only carrier of those).
    const shim = await spawnShim({ env: { AGENT_REPL_SESSION_ID: "host-correlation-probe" } });

    const serving = await shim.log.record((record) => record.context.outcome === "serving");

    expect(serving.agent_repl_session_id).toBe("host-correlation-probe");
  });

  test("without AGENT_REPL_SESSION_ID the shim names itself shim-<workspace-id>-<pid>", async () => {
    // The self-name CORRELATES WITH THE DAEMON: it is keyed by the daemon's own
    // workspace id -- the one this shim's listen socket is named after and the
    // one every record's `workspace_id` carries -- so a self-named record joins
    // the daemon's records for the same workspace with no session id in play.
    // Keyed by the shim's md5 prefix it joined nothing outside this process.
    const shim = await spawnShim({ env: { AGENT_REPL_SESSION_ID: undefined } });

    const serving = await shim.log.record((record) => record.context.outcome === "serving");

    expect(serving.agent_repl_session_id).toBe(
      `shim-${path.basename(shim.dirs.listen, ".sock")}-${String(shim.child.pid)}`,
    );
  });

  test("every record carries the daemon's workspace id, read off the listen socket", async () => {
    // Arrange.
    const shim = await spawnShim();

    // Act.
    const serving = await shim.log.record((record) => record.context.outcome === "serving");

    // Assert.
    expect(serving.workspace_id).toBe(path.basename(shim.dirs.listen, ".sock"));
  });

  test("every record keeps the shim's own md5 workspace key beside it", async () => {
    // Arrange.
    const shim = await spawnShim();

    // Act.
    const serving = await shim.log.record((record) => record.context.outcome === "serving");

    // Assert.
    expect(serving.context.shim_workspace_hash).toBe(
      workspaceLockKey(workspaceRealPath(shim.dirs)),
    );
  });

  test("a listen socket that is not named after a workspace id is refused", async () => {
    // A SPAWN-CONTRACT DISAGREEMENT, exactly like an unrecognized flag: the
    // daemon always names the socket after the workspace, and a record filed
    // under a workspace the fleet never heard of is worse than a refusal.
    // Arrange.
    const dirs = makeDirectories();

    // Act.
    const run = await runShim(
      [
        "--listen",
        path.join(dirs.root, "not-a-workspace-id.sock"),
        "--store-socket",
        dirs.storeSocket,
        "--log-fd",
        "3",
        "--fake",
      ],
      servingEnv(dirs),
    );

    // Assert.
    expect(run.exit.code).not.toBe(0);
    expect(run.stderr).toContain("not named after a workspace id");
  });

  test("a poisoned log sink keeps the shim serving and surfaces log_sink_poisoned", async () => {
    // THE EPIPE INCIDENT (2026-08-10): a shim must survive its daemon's death
    // without dying on its own log line. The sink's death is REPORTED, never
    // swallowed and never fatal.
    const shim = await spawnShim({ logPipe: true });
    await shim.clients.h1.startSession(freshSession());
    const watch = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    // The opening frame is the current health verdict; the fault comes later.
    await watch.next();

    // Act: close the READ end, so the next log write takes EPIPE.
    (shim.logPipe as unknown as { destroy: () => void } | null)?.destroy();
    // Provoke log traffic: every rpc logs its entry.
    await shim.clients.h1.readHistory(readHistoryFirst());

    const faulted = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      const health = update.update.value.health;
      return (
        health.case === "unhealthy" &&
        health.value.faults.some((fault) => fault.kind.case === "logSinkPoisoned")
      );
    });
    expect(sessionUpdate(faulted).update.case).toBe("diagnostics");
    // Still answering: the fault is a report, not a death.
    expect(await connectCode(shim.clients.h1.readHistory(readHistoryFirst()))).not.toBe(
      Code.Unavailable,
    );
    expect(shim.child.exitCode).toBeNull();
    watch.close();
  });
});

describe("the store socket", () => {
  test("neither --store-socket nor AGENT_REPL_STORE_SOCKET refuses to start", async () => {
    const dirs = makeDirectories();
    const env = { ...servingEnv(dirs), AGENT_REPL_STORE_SOCKET: undefined };

    const run = await runShim(["--listen", dirs.listen, "--log-fd", "3", "--fake"], env);

    expect(run.exit.code).not.toBe(0);
    expect(run.stderr).toContain("store socket");
  });

  test("the flag beats the env when both name a socket", async () => {
    // A caller that stated the socket explicitly meant it; the env exists so a
    // harness can redirect every process it starts without rewriting each spawn.
    const shim = await spawnShim({
      env: { AGENT_REPL_STORE_SOCKET: "/nonexistent/decoy.sock" },
    });

    const serving = await shim.log.record((record) => record.context.outcome === "serving");
    const startup = shim.log
      .records()
      .find((record) => record.context.store_socket !== undefined);

    expect(serving.context.outcome).toBe("serving");
    expect(startup?.context.store_socket).toBe(shim.dirs.storeSocket);
  });
});

describe("SessionStarted's build identity", () => {
  test("SessionRuntime reports the SHIM_BUILD_SHA the process was spawned with", async () => {
    // The daemon compares this against its deploy stamp and bounces a stale
    // survivor, which only works if the value comes from the spawn env.
    const shim = await spawnShim();

    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    expect(started.runtime?.shimBuildSha).toBe(ITEST_BUILD_SHA);
    expect(started.runtime?.sdkVersion).not.toBe("");
  });
});
