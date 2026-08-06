import { afterEach, describe, expect, it, vi } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  acquireSessionLock,
  acquireWorkspaceLock,
  lockPath,
  workspaceLockPath,
} from "../src/uds/session-lock.js";

// The lock replaces the uniqueness bind() used to give away for free: while
// each shim listened on its own path, a second shim for one session could not
// exist. Dialling out removes that, and two shims on one conversation means two
// writers on one transcript.

const releases: Array<() => void> = [];
const homes: string[] = [];
afterEach(() => {
  releases.splice(0).forEach((r) => r());
  homes.splice(0).forEach((h) => fs.rmSync(h, { recursive: true, force: true }));
});

/** Point homedir at a temp dir so tests never touch the real lock directory. */
function isolateHome(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "shim-lock-"));
  homes.push(dir);
  vi.spyOn(os, "homedir").mockReturnValue(dir);
  return dir;
}

describe("session lock", () => {
  it("takes the lock for a free session", () => {
    // Arrange
    isolateHome();
    // Act
    const release = acquireSessionLock("s_free");
    releases.push(release);
    // Assert: the lock file exists and we hold it.
    expect(fs.existsSync(lockPath("s_free"))).toBe(true);
  });

  it("refuses a session another shim already holds", () => {
    // Arrange: a first shim owns the session.
    isolateHome();
    releases.push(acquireSessionLock("s_taken"));

    // Act / Assert: a duplicate must refuse to start, not warn and continue.
    expect(() => acquireSessionLock("s_taken")).toThrow(/already held/);
  });

  it("frees the session when the holder releases", () => {
    // Arrange: the kernel drops the lock on close, which is what it does on
    // process death too — so a dead shim's session must be reclaimable.
    isolateHome();
    const release = acquireSessionLock("s_recycled");
    release();

    // Act / Assert
    const second = acquireSessionLock("s_recycled");
    releases.push(second);
    expect(fs.existsSync(lockPath("s_recycled"))).toBe(true);
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

  it("refuses a workspace another shim already holds", () => {
    // Arrange: a live shim owns the workspace under some other session id.
    isolateHome();
    releases.push(acquireWorkspaceLock(WORKTREE));

    // Act / Assert
    expect(() => acquireWorkspaceLock(WORKTREE)).toThrow(/already held/);
  });

  it("is independent of the session lock, so both must be taken", () => {
    // Arrange: one shim holds session s_first over the workspace.
    isolateHome();
    releases.push(acquireSessionLock("s_first"));
    releases.push(acquireWorkspaceLock(WORKTREE));

    // Act / Assert: a DIFFERENT session id sails through the session lock and
    // is stopped only by the workspace lock — the exact duplicate spawn.
    releases.push(acquireSessionLock("s_second"));
    expect(() => acquireWorkspaceLock(WORKTREE)).toThrow(/already held/);
  });
});
