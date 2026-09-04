import { afterEach, describe, expect, it, vi } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  acquireSessionLock,
  acquireWorkspaceLock,
  lockBinaryPath,
  lockPath,
  workspaceLockPath,
  LOCK_BIN_ENV,
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

    // Act / Assert
    await expect(acquireSessionLock("s_broken")).rejects.toThrow(/exited \(code 1/);
  });

  it("fails loudly when the holder binary is not there at all", async () => {
    // Arrange
    isolateHome();
    process.env[LOCK_BIN_ENV] = path.join(os.tmpdir(), "no-such-shim-lock-binary");

    // Act / Assert: never a silent no-op, which would hand the daemon a false
    // "free" and let it spawn the duplicate this exists to prevent.
    await expect(acquireSessionLock("s_nobin")).rejects.toThrow(/could not run/);
  });

  it("refuses a holder that announces something other than the ready line", async () => {
    // Arrange: stdout is the readiness protocol, so anything else on it is a
    // holder this shim does not understand.
    isolateHome();
    installHolder(`process.stdout.write("ok\\n");\nprocess.stdin.resume();\n`);

    // Act / Assert
    await expect(acquireSessionLock("s_wrong")).rejects.toThrow(/announced "ok"/);
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
  const WORKTREE = "/Users/dodgecoates/.config/doom-worktrees/model-selection-convergence-hwx";

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
    expect(path.basename(p)).toBe("workspace-0b96ccc5.lock");
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
