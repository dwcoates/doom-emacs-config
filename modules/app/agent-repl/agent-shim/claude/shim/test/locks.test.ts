import { afterEach, describe, expect, it, vi } from "vitest";
import { textContaining } from "./expect-shapes.js";
import fs from "node:fs";
import { EventEmitter } from "node:events";
import os from "node:os";
import path from "node:path";
import {
  acquireSessionLock,
  acquireWorkspaceLock,
  lockBinaryPath,
  lockPath,
  describeLockHolderHow,
  LockHeldError,
  LockHolderUnavailableError,
  type LockHolderHow,
  workspaceLockPath,
  LOCK_BIN_ENV,
  LOCK_DIR_ENV,
  type LockRelease,
} from "../src/locks.js";

// The lock replaces the uniqueness bind() used to give away for free: while
// each shim listened on its own path, a second shim for one session could not
// exist. Dialling out removes that, and two shims on one conversation means two
// writers on one transcript.
//
// THE KERNEL LOCK ITSELF LIVES IN A CHILD PROCESS (agent-shim/shim-lock),
// because Node cannot take a flock and `O_EXLOCK` is macOS/BSD only. What this
// unit suite owns is therefore the PROTOCOL: spawn, wait for the ready line,
// distinguish a refusal from a failure, release by closing stdin. The kernel
// half — that the claim is a real flock the daemon's probe contends with, and
// that it dies with the holder — is shim-lock's own Go suite, and the two meet
// in test/integration, which spawns the real binary.

const releases: Array<LockRelease> = [];
const homes: string[] = [];
const originalBin = process.env[LOCK_BIN_ENV];
afterEach(async () => {
  for (const release of releases.splice(0)) await release();
  homes.splice(0).forEach((h) => fs.rmSync(h, { recursive: true, force: true }));
  if (originalBin === undefined) delete process.env[LOCK_BIN_ENV];
  else process.env[LOCK_BIN_ENV] = originalBin;
});

/** Point homedir at a temp dir so tests never touch the real lock directory. */
function isolateHome(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "shim-lock-"));
  homes.push(dir);
  vi.spyOn(os, "homedir").mockReturnValue(dir);
  return dir;
}

/**
 * Install a STAND-IN holder speaking shim-lock's protocol, and point the shim
 * at it.
 *
 * A stand-in rather than the real Go binary because this suite must stay
 * hermetic — `npm test` does not have a Go toolchain in its contract — and
 * because what is under test here is how the shim READS the protocol, not what
 * the kernel does with a flock. Every stand-in is a node script, so it runs
 * wherever the suite does.
 */
function installHolder(body: string): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "shim-lock-bin-"));
  homes.push(dir);
  const file = path.join(dir, "holder.mjs");
  fs.writeFileSync(file, body, { mode: 0o755 });
  const launcher = path.join(dir, "shim-lock");
  fs.writeFileSync(launcher, `#!/bin/sh\nexec "${process.execPath}" "${file}" "$@"\n`, { mode: 0o755 });
  process.env[LOCK_BIN_ENV] = launcher;
  return launcher;
}

/** A holder that takes the claim and stays alive until its stdin closes. */
const HOLDS = `
process.stdout.write("locked\\n");
process.stdin.resume();
process.stdin.on("end", () => process.exit(0));
`;

/** A holder that refuses with shim-lock's distinct "held by another" code. */
const REFUSES = `
process.stderr.write("the lock is already held by another process\\n");
process.exit(3);
`;

/** A holder that fails for a reason that is NOT another shim owning the lock. */
const FAILS = `
process.stderr.write("the lock directory could not be created: EACCES\\n");
process.exit(1);
`;

describe("the lock holder binary", () => {
  it("defaults to the deploy location, beside shim-store", () => {
    // Arrange
    const home = isolateHome();
    delete process.env[LOCK_BIN_ENV];
    // Act / Assert
    expect(lockBinaryPath()).toBe(path.join(home, ".cache", "agent-repl", "bin", "shim-lock"));
  });

  it("honors the override, so a suite spawns the binary it built itself", () => {
    // Arrange
    isolateHome();
    process.env[LOCK_BIN_ENV] = "/somewhere/else/shim-lock";
    // Act / Assert
    expect(lockBinaryPath()).toBe("/somewhere/else/shim-lock");
  });
});

describe("session lock", () => {
  it("takes the lock for a free session", async () => {
    // Arrange
    isolateHome();
    installHolder(HOLDS);
    // Act
    const release = await acquireSessionLock("s_free");
    releases.push(release);
    // Assert: the claim resolved, which happens only on the ready line, and the
    // lock directory was made ready for the holder.
    expect(fs.existsSync(path.dirname(lockPath("s_free")))).toBe(true);
  });

  it("refuses a session another shim already holds", async () => {
    // Arrange: the holder answers with the distinct held code.
    isolateHome();
    installHolder(REFUSES);

    // Act / Assert: a duplicate must refuse to start, not warn and continue.
    await expect(acquireSessionLock("s_taken")).rejects.toThrow(/already held/);
  });

  it("distinguishes a holder that FAILED from one that found the lock held", async () => {
    // Arrange: exit 1, not exit 3. An unwritable lock directory is not a live
    // duplicate, and reporting it as one would refuse the session forever.
    isolateHome();
    installHolder(FAILS);

    // Act
    const refusal = await acquireSessionLock("s_broken").catch((err: unknown) => err);

    // Assert
    expect((refusal as LockHolderUnavailableError).how).toEqual({
      kind: "exited",
      code: 1,
      stderr: "the lock directory could not be created: EACCES",
    });
  });

  it("refuses a holder that found the lock held as LockHeldError, the one genuine owner", async () => {
    // Arrange
    isolateHome();
    installHolder(REFUSES);

    // Act
    const refusal = await acquireSessionLock("s_held_typed").catch((err: unknown) => err);

    // Assert
    expect(refusal).toBeInstanceOf(LockHeldError);
  });

  it("fails loudly when the holder binary is not there at all", async () => {
    // Arrange
    isolateHome();
    process.env[LOCK_BIN_ENV] = path.join(os.tmpdir(), "no-such-shim-lock-binary");

    // Act / Assert: never a silent no-op, which would hand the daemon a false
    // "free" and let it spawn the duplicate this exists to prevent.
    await expect(acquireSessionLock("s_nobin")).rejects.toThrow(/could not be spawned/);
  });

  it("refuses a missing holder binary as LockHolderUnavailableError naming that binary", async () => {
    // Arrange
    isolateHome();
    const missing = path.join(os.tmpdir(), "no-such-shim-lock-binary");
    process.env[LOCK_BIN_ENV] = missing;

    // Act
    const refusal = await acquireSessionLock("s_nobin_typed").catch((err: unknown) => err);

    // Assert: nobody owns the session, so the refusal must not read as one.
    expect(refusal).toBeInstanceOf(LockHolderUnavailableError);
    expect((refusal as LockHolderUnavailableError).binary).toBe(missing);
  });

  it("never refuses a held lock as an unavailable holder", async () => {
    // Arrange: exit 3 is a real ownership conflict.
    isolateHome();
    installHolder(REFUSES);

    // Act
    const refusal = await acquireSessionLock("s_held_untyped").catch((err: unknown) => err);

    // Assert
    expect(refusal).not.toBeInstanceOf(LockHolderUnavailableError);
  });

  it("refuses a holder that announces something other than the ready line", async () => {
    // Arrange: stdout is the readiness protocol, so anything else on it is a
    // holder this shim does not understand.
    isolateHome();
    installHolder(`process.stdout.write("ok\\n");\nprocess.stdin.resume();\n`);

    // Act
    const refusal = await acquireSessionLock("s_wrong").catch((err: unknown) => err);

    // Assert
    expect((refusal as LockHolderUnavailableError).how).toEqual({ kind: "misanswered", line: "ok" });
  });

  it("frees the session when the holder releases", async () => {
    // Arrange: releasing closes the holder's stdin, which is what the kernel
    // does for us when this process dies — so a dead shim's session must be
    // reclaimable.
    isolateHome();
    installHolder(HOLDS);
    const release = await acquireSessionLock("s_recycled");

    // Act
    await release();

    // Assert: the next claim is taken, by a NEW holder process.
    const second = await acquireSessionLock("s_recycled");
    releases.push(second);
    expect(second).toBeTypeOf("function");
  });

  it("puts locks under run/, beside sock/ rather than among the sockets", () => {
    // Arrange
    isolateHome();
    // Act
    const p = lockPath("s_abc");
    // Assert
    expect(path.basename(path.dirname(p))).toBe("run");
    expect(path.basename(p)).toBe("session-s_abc.lock");
  });
});

// The workspace lock is the claim the session lock cannot make: two daemon
// session ids over one workspace take two session locks and exclude nothing.
describe("workspace lock", () => {
  const WORKTREE = "/work/.config/doom-worktrees/model-selection-convergence-hwx";

  it.each([
    ["a worktree path", WORKTREE],
    ["a trailing slash naming the same workspace", `${WORKTREE}/`],
  ])("derives the pinned lock file for %s", (_name, cwd) => {
    // Arrange: the Go side pins these same literals in
    // daemon/internal/sessionlock/sessionlock_test.go, so the two derivations
    // cannot drift into two locks over one workspace.
    isolateHome();
    // Act
    const p = workspaceLockPath(cwd);
    // Assert
    expect(path.basename(path.dirname(p))).toBe("run");
    expect(path.basename(p)).toBe("workspace-5c78c72d.lock");
  });

  it("keeps the root directory itself as a workspace, separator and all", () => {
    // Arrange: cleanForKey strips a TRAILING separator, but "/" is nothing but
    // one — stripping it would key the root off the empty string, the same key
    // an unnamed workspace would have if it were allowed at all.
    isolateHome();
    // Act
    const root = workspaceLockPath("/");
    const doubled = workspaceLockPath("//");
    // Assert
    expect(path.basename(root)).toBe(path.basename(doubled));
    expect(path.basename(root)).not.toBe(path.basename(workspaceLockPath("/ws")));
  });

  it("refuses an unnamed workspace rather than resolving one shared lock", () => {
    // Arrange
    isolateHome();
    // Act / Assert
    expect(() => workspaceLockPath("")).toThrow(/absolute workspace directory/);
  });

  it("refuses a workspace another shim already holds", async () => {
    // Arrange: a live shim owns the workspace under some other session id.
    isolateHome();
    installHolder(REFUSES);

    // Act / Assert
    await expect(acquireWorkspaceLock(WORKTREE)).rejects.toThrow(/already held/);
  });

  it("takes the workspace claim with its own holder, not the session's", async () => {
    // Arrange: both claims are made, in the fixed order the module documents.
    isolateHome();
    installHolder(HOLDS);

    // Act
    releases.push(await acquireSessionLock("s_first"));
    releases.push(await acquireWorkspaceLock(WORKTREE));

    // Assert: two separate holders, so the workspace claim is not a byproduct
    // of the session one.
    expect(releases).toHaveLength(2);
  });
});

/**
 * The claim's PROTOCOL, driven over a fully synthetic holder.
 *
 * The stand-in scripts above prove the happy paths, but a real child cannot be
 * made to fail on cue: a spawn that raises synchronously, a stdin pipe that
 * EPIPEs at release, a holder killed by a signal. Those arms are driven here
 * over a fake `node:child_process`, so every one of them is deterministic.
 */
describe("the claim protocol over a synthetic holder", () => {
  const priorLockDir = process.env[LOCK_DIR_ENV];

  afterEach(() => {
    vi.doUnmock("node:child_process");
    vi.resetModules();
    if (priorLockDir === undefined) delete process.env[LOCK_DIR_ENV];
    else process.env[LOCK_DIR_ENV] = priorLockDir;
  });

  /** A child whose every stream and event this test drives by hand. */
  interface FakeChild {
    stdout: EventEmitter & { setEncoding(enc: string): void };
    stderr: EventEmitter & { setEncoding(enc: string): void };
    stdin: EventEmitter & { end(): void };
    stdinEnded: number;
    killed: string[];
    emitter: EventEmitter;
  }

  function makeChild(): FakeChild {
    const stdout = Object.assign(new EventEmitter(), { setEncoding: (): void => {} });
    const stderr = Object.assign(new EventEmitter(), { setEncoding: (): void => {} });
    const child: FakeChild = {
      stdout,
      stderr,
      stdin: Object.assign(new EventEmitter(), {
        end: (): void => {
          child.stdinEnded += 1;
        },
      }),
      stdinEnded: 0,
      killed: [],
      emitter: new EventEmitter(),
    };
    return child;
  }

  /** The locks module wired to a spawn this test controls. */
  async function withSpawn(
    spawnImpl: (binary: string, args: string[]) => unknown,
  ): Promise<typeof import("../src/locks.js")> {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "shim-lock-fake-"));
    homes.push(dir);
    process.env[LOCK_DIR_ENV] = dir;
    process.env[LOCK_BIN_ENV] = path.join(dir, "shim-lock");
    vi.resetModules();
    vi.doMock("node:child_process", () => ({ spawn: spawnImpl }));
    const log = await import("../src/log.js");
    log.configureLog({ fd: 3, cwd: "/ws", workspaceId: "00000000000000bb", agentReplSessionId: "locks-suite" });
    return import("../src/locks.js");
  }

  /** A spawn that yields a hand-driven child, exposed for the test to poke. */
  async function withFakeChild(): Promise<{
    locks: typeof import("../src/locks.js");
    child: FakeChild;
  }> {
    const child = makeChild();
    const locks = await withSpawn(() =>
      Object.assign(child.emitter, {
        stdout: child.stdout,
        stderr: child.stderr,
        stdin: child.stdin,
        kill: (signal: string): void => {
          child.killed.push(signal);
        },
      }),
    );
    return { locks, child };
  }

  it("rejects when spawning the holder raises synchronously", async () => {
    // Arrange: an argument the platform refuses outright, which is how a
    // misconfigured holder path reaches the caller as a throw rather than an
    // 'error' event.
    const locks = await withSpawn(() => {
      throw new Error("EINVAL: invalid argument");
    });

    // Act, Assert — the claim cannot be attempted, and a shim that cannot
    // prove it is the only one must not start.
    await expect(locks.acquireSessionLock("s_unspawnable")).rejects.toThrow(
      /could not be spawned: EINVAL: invalid argument/,
    );
  });

  it("carries the OS error of an asynchronous spawn failure on the typed refusal", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_enoent").catch((err: unknown) => err);

    // Act: Node reports an unspawnable binary as an 'error' event.
    child.emitter.emit("error", new Error("spawn /x/shim-lock ENOENT"));

    // Assert.
    const refusal = await claim;
    expect(refusal).toBeInstanceOf(locks.LockHolderUnavailableError);
    expect((refusal as InstanceType<typeof locks.LockHolderUnavailableError>).how).toEqual({
      kind: "spawnFailed",
      osError: "spawn /x/shim-lock ENOENT",
    });
  });

  it("carries the OS error of a synchronous spawn failure on the typed refusal", async () => {
    // Arrange.
    const locks = await withSpawn(() => {
      throw new Error("EINVAL: invalid argument");
    });

    // Act.
    const refusal = await locks.acquireWorkspaceLock("/ws").catch((err: unknown) => err);

    // Assert.
    expect((refusal as InstanceType<typeof locks.LockHolderUnavailableError>).how).toEqual({
      kind: "spawnFailed",
      osError: "EINVAL: invalid argument",
    });
  });

  it("records an unspawnable holder at ERROR, because it is a defect", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_enoent_logged").catch(() => undefined);
    // eslint-disable-next-line @typescript-eslint/unbound-method -- read .mock.calls only
    const mirror = vi.mocked(process.stderr.write);
    const before = mirror.mock.calls.length;

    // Act.
    child.emitter.emit("error", new Error("spawn /x/shim-lock EACCES"));
    await claim;

    // Assert.
    const recorded = mirror.mock.calls
      .slice(before)
      .map(([line]) => JSON.parse(String(line)) as Record<string, unknown>);
    expect(recorded).toContainEqual(
      expect.objectContaining({ level: "error", message: textContaining("could not be spawned") }),
    );
  });

  it("records an error raised after the claim is held at WARN, never as a spawn failure", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_late_error");
    child.stdout.emit("data", "locked\n");
    await claim;
    // eslint-disable-next-line @typescript-eslint/unbound-method -- read .mock.calls only
    const mirror = vi.mocked(process.stderr.write);
    const before = mirror.mock.calls.length;

    // Act: a failed kill of a live holder.
    child.emitter.emit("error", new Error("kill ESRCH"));

    // Assert.
    const recorded = mirror.mock.calls
      .slice(before)
      .map(([line]) => JSON.parse(String(line)) as Record<string, unknown>);
    expect(recorded).toEqual([
      expect.objectContaining({ level: "warn", message: textContaining("after its claim settled") }),
    ]);
  });

  it("refuses a holder that never answers as the silent arm naming the bound, and kills it", async () => {
    // Arrange: a holder that spawned but neither answers nor exits.
    vi.useFakeTimers();
    try {
      const { locks, child } = await withFakeChild();
      const claim = locks.acquireSessionLock("s_silent").catch((err: unknown) => err);

      // Act.
      vi.advanceTimersByTime(locks.HOLDER_ANSWER_TIMEOUT_MS);
      const refusal = await claim;

      // Assert.
      expect({
        typed: refusal instanceof locks.LockHolderUnavailableError,
        how: (refusal as InstanceType<typeof locks.LockHolderUnavailableError>).how,
        killed: child.killed,
      }).toEqual({ typed: true, how: { kind: "silent", timeoutMs: locks.HOLDER_ANSWER_TIMEOUT_MS }, killed: ["SIGKILL"] });
    } finally {
      vi.useRealTimers();
    }
  });

  it("records a holder that never answers at ERROR", async () => {
    // Arrange.
    vi.useFakeTimers();
    try {
      const { locks } = await withFakeChild();
      const claim = locks.acquireSessionLock("s_silent_logged").catch(() => undefined);
      // eslint-disable-next-line @typescript-eslint/unbound-method -- read .mock.calls only
      const mirror = vi.mocked(process.stderr.write);
      const before = mirror.mock.calls.length;

      // Act.
      vi.advanceTimersByTime(locks.HOLDER_ANSWER_TIMEOUT_MS);
      await claim;

      // Assert.
      const recorded = mirror.mock.calls
        .slice(before)
        .map(([line]) => JSON.parse(String(line)) as Record<string, unknown>);
      expect(recorded).toContainEqual(
        expect.objectContaining({ level: "error", message: textContaining("gave no \"locked\" answer within") }),
      );
    } finally {
      vi.useRealTimers();
    }
  });

  it("leaves a holder that answers just inside the bound held, past the bound too", async () => {
    // Arrange.
    vi.useFakeTimers();
    try {
      const { locks, child } = await withFakeChild();
      const claim = locks.acquireSessionLock("s_prompt");
      vi.advanceTimersByTime(locks.HOLDER_ANSWER_TIMEOUT_MS - 1);

      // Act: the answer lands inside the bound, then the bound passes.
      child.stdout.emit("data", "locked\n");
      const release = await claim;
      vi.advanceTimersByTime(locks.HOLDER_ANSWER_TIMEOUT_MS);

      // Assert: held, and never killed by the deadline.
      expect({ release: typeof release, killed: child.killed }).toEqual({ release: "function", killed: [] });
    } finally {
      vi.useRealTimers();
    }
  });

  it("reports the signal when the holder was killed before taking the lock", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_signalled");

    // Act.
    child.emitter.emit("close", null, "SIGKILL");

    // Assert — a signal death is not a "held by another shim" refusal.
    const refusal = await claim.catch((err: unknown) => err);
    expect((refusal as InstanceType<typeof locks.LockHolderUnavailableError>).how).toEqual({
      kind: "signaled",
      signal: "SIGKILL",
      stderr: "",
    });
  });

  it("carries the holder's stderr on a signal death", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_signalled_stderr").catch((err: unknown) => err);
    child.stderr.emit("data", "fatal error: unexpected signal\n");

    // Act.
    child.emitter.emit("close", null, "SIGSEGV");

    // Assert.
    expect(((await claim) as InstanceType<typeof locks.LockHolderUnavailableError>).how).toEqual({
      kind: "signaled",
      signal: "SIGSEGV",
      stderr: "fatal error: unexpected signal",
    });
  });

  it("refuses a holder exiting 3 as LockHeldError, never as a failed holder", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_held_fake").catch((err: unknown) => err);

    // Act.
    child.emitter.emit("close", 3, null);

    // Assert.
    expect((await claim) instanceof locks.LockHeldError).toBe(true);
  });

  it("records a holder that exited before taking the lock at ERROR", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_exited_logged").catch(() => undefined);
    // eslint-disable-next-line @typescript-eslint/unbound-method -- read .mock.calls only
    const mirror = vi.mocked(process.stderr.write);
    const before = mirror.mock.calls.length;

    // Act.
    child.emitter.emit("close", 2, null);
    await claim;

    // Assert.
    const recorded = mirror.mock.calls
      .slice(before)
      .map(([line]) => JSON.parse(String(line)) as Record<string, unknown>);
    expect(recorded).toContainEqual(
      expect.objectContaining({ level: "error", message: textContaining("exited with code 2 before taking the lock") }),
    );
  });

  it("raises a close naming neither a code nor a signal as an untyped invariant violation", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_neither").catch((err: unknown) => err);

    // Act.
    child.emitter.emit("close", null, null);
    const refusal = await claim;

    // Assert: neither refusal the contract names, so neither typed error.
    expect({
      held: refusal instanceof locks.LockHeldError,
      unavailable: refusal instanceof locks.LockHolderUnavailableError,
      message: (refusal as Error).message,
    }).toEqual({ held: false, unavailable: false, message: textContaining("neither an exit code nor a signal") });
  });

  it("records a close naming neither a code nor a signal at ERROR", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_neither_logged").catch(() => undefined);
    // eslint-disable-next-line @typescript-eslint/unbound-method -- read .mock.calls only
    const mirror = vi.mocked(process.stderr.write);
    const before = mirror.mock.calls.length;

    // Act.
    child.emitter.emit("close", null, null);
    await claim;

    // Assert.
    const recorded = mirror.mock.calls
      .slice(before)
      .map(([line]) => JSON.parse(String(line)) as Record<string, unknown>);
    expect(recorded).toContainEqual(
      expect.objectContaining({ level: "error", message: textContaining("invariant violated") }),
    );
  });

  it("waits for a whole line before judging what the holder announced", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_partial");

    // Act: the ready line arrives split across two chunks.
    child.stdout.emit("data", "loc");
    child.stdout.emit("data", "ked\n");

    // Assert — a partial read is not a wrong announcement.
    const release = await claim;
    expect(release).toBeTypeOf("function");
  });

  it("survives the holder's stdin failing, because a gone holder IS the release", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_epipe");
    child.stdout.emit("data", "locked\n");
    await claim;
    // Captured to READ `.mock.calls` off, never invoked through this reference, so there is
    // no `this` to lose.
    // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
    const mirror = vi.mocked(process.stderr.write);
    const before = mirror.mock.calls.length;

    // Act: the holder died, so writing to its stdin raises EPIPE.
    child.stdin.emit("error", new Error("write EPIPE"));

    // Assert — recorded, not thrown: an unhandled 'error' would take the shim
    // down over a lock it no longer needs.
    const recorded = mirror.mock.calls
      .slice(before)
      .map(([line]) => JSON.parse(String(line)) as Record<string, unknown>);
    expect(recorded).toHaveLength(1);
    expect(recorded[0]).toMatchObject({ level: "warn", message: textContaining("stdin failed") });
  });

  it("closes the holder's stdin exactly once however often the release is called", async () => {
    // Arrange.
    const { locks, child } = await withFakeChild();
    const claim = locks.acquireSessionLock("s_double_release");
    child.stdout.emit("data", "locked\n");
    const release = await claim;

    // Act: the first release closes stdin and awaits the exit; the second
    // must not re-close a pipe that is already gone.
    const first = release();
    child.emitter.emit("close", 0, null);
    await first;
    await release();

    // Assert.
    expect(child.stdinEnded).toBe(1);
  });
});

describe("describeLockHolderHow", () => {
  it.each<[string, LockHolderHow, string]>([
    ["a spawn failure", { kind: "spawnFailed", osError: "spawn ENOENT" }, "could not be spawned: spawn ENOENT"],
    ["an exit with stderr", { kind: "exited", code: 1, stderr: "EACCES" }, "exited with code 1 before taking the lock (EACCES)"],
    ["an exit with no stderr", { kind: "exited", code: 1, stderr: "" }, "exited with code 1 before taking the lock"],
    ["a signal", { kind: "signaled", signal: "SIGSEGV", stderr: "" }, "was killed by SIGSEGV before taking the lock"],
    ["a wrong line", { kind: "misanswered", line: "ok" }, 'answered "ok" instead of "locked" and was killed'],
    ["no answer", { kind: "silent", timeoutMs: 5000 }, 'gave no "locked" answer within 5000 ms and was killed'],
  ])("words %s", (_name, how, want) => {
    // Arrange, Act, Assert.
    expect(describeLockHolderHow(how)).toBe(want);
  });
});
