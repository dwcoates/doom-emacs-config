/**
 * The shim's exclusive claim on its session.
 *
 * # Why
 *
 * While each shim LISTENED on its own `session-<id>.sock`, uniqueness was free:
 * only one process can bind a path, so a second shim for the same session was
 * unreachable — bind(2) returned EADDRINUSE and it died. Now that shims dial
 * OUT to the daemon (design-shim-transport-inversion.md) nothing stops two
 * processes claiming one session, and two shims on one conversation means two
 * writers on one transcript.
 *
 * The daemon cannot close that gap by tracking connections alone. On a fresh
 * boot a surviving shim may not have dialled in yet, so "do I have a connection
 * for this session?" answers NO when the truth is NOT YET, and the daemon would
 * spawn a duplicate of a shim that is alive and mid-turn.
 *
 * So the shim takes a kernel-enforced lock at startup and holds it for its
 * lifetime. Held is what the daemon probes before spawning.
 *
 * # Two keys, both required
 *
 * A session id names one daemon-side conversation attempt; a WORKSPACE names
 * the thing the invariant is actually about. A workspace and each resumed
 * transcript keep exactly one live session at a time, and two daemon session
 * ids can point at one workspace and one vendor transcript — so a claim keyed
 * only by session id lets two shims take two different locks over one
 * transcript and excludes nothing.
 *
 * The workspace key is the CWD rather than the vendor transcript uuid because a
 * FRESH session has no transcript yet: a transcript key cannot cover the window
 * in which the duplicate is spawned.
 *
 * Both locks are taken, in a fixed order — session lock first, then workspace
 * lock — so no two shims can ever take them in opposite orders. Failing to take
 * either is a refusal to start.
 *
 * # Mechanism
 *
 * `open(2)`'s `O_EXLOCK`, which takes a BSD flock as part of opening the file —
 * the same lock Go's `syscall.Flock` takes, so the daemon's probe and this
 * claim interoperate. Two properties matter and neither is available from a
 * plain lock FILE:
 *
 *   - the kernel enforces it, so exclusion is not advisory bookkeeping; and
 *   - it is released automatically when the process dies, however it dies, so
 *     there is no stale lock to reap and no PID-reuse hazard.
 *
 * `O_EXLOCK` is macOS/BSD only. Linux has no equivalent in Node without native
 * code, so there acquiring FAILS LOUDLY rather than pretending the session is
 * claimed — a silent no-op would hand the daemon a false "free" and let it
 * spawn the duplicate this exists to prevent.
 */
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { createHash } from "node:crypto";
import { bindLog } from "./log.js";

const COMPONENT = "shim-session-lock";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.session-lock.lifecycle" });

/**
 * `O_EXLOCK` from `<sys/fcntl.h>`: take an exclusive advisory lock as part of
 * open(2), the same lock `flock(2)` takes — so the daemon's Go-side probe
 * (`syscall.Flock`) and this claim contend correctly with each other. Verified
 * both directions: while Node holds it Go gets EWOULDBLOCK, and once Node
 * closes the fd Go acquires.
 *
 * Spelled numerically because Node does not expose it on `fs.constants`, even
 * on platforms whose open(2) honors it — flags are passed straight through, so
 * the raw value works. BSD/macOS only; undefined elsewhere, which
 * acquireSessionLock turns into a loud refusal rather than a silent no-op.
 */
const O_EXLOCK: number | undefined =
  process.platform === "darwin" || process.platform.endsWith("bsd") ? 0x20 : undefined;

/** The directory session locks live in: a sibling of sock/ and store/. */
export function lockDir(): string {
  return path.join(os.homedir(), ".cache", "agent-repl", "run");
}

/** The lock file for sessionId. */
export function lockPath(sessionId: string): string {
  return path.join(lockDir(), `session-${sessionId}.lock`);
}

/**
 * The workspace's identity in a file name: the first eight hex digits of the
 * MD5 of its normalized absolute directory.
 *
 * This is the SAME derivation as the `workspace_id` on every canonical log
 * record (log.ts, and dlog.WorkspaceFromDirectory on the Go side), so a lock
 * file is greppable against the log lines of the shim holding it. The daemon
 * hands the shim an already symlink-resolved absolute `--cwd`, and both sides
 * normalize identically (cleanForKey is Go's filepath.Clean), so the Node and
 * Go derivations agree on one path for one workspace.
 */
export function workspaceLockKey(cwd: string): string {
  if (cwd === "") {
    throw new Error(`${COMPONENT}: a workspace lock needs an absolute workspace directory, got ""`);
  }
  return createHash("md5").update(cleanForKey(cwd)).digest("hex").slice(0, 8);
}

/**
 * Go's `filepath.Clean`, spelled in Node. `path.normalize` collapses `.`, `..`
 * and repeated separators the same way and differs in exactly one respect: it
 * KEEPS a trailing separator where Clean strips it. Unreconciled, that one
 * difference gives `/ws` and `/ws/` two locks on the Node side and one on the
 * Go side — two shims over one workspace, which is what the lock exists to stop.
 */
function cleanForKey(p: string): string {
  const normalized = path.normalize(p);
  return normalized.length > 1 ? normalized.replace(/\/+$/, "") : normalized;
}

/** The lock file for the workspace rooted at cwd. */
export function workspaceLockPath(cwd: string): string {
  return path.join(lockDir(), `workspace-${workspaceLockKey(cwd)}.lock`);
}

/**
 * Take this session's exclusive lock and hold it until the process exits.
 *
 * Returns a release function for a deliberate teardown. Not calling it is fine
 * and is the normal path — the kernel drops the lock when the process dies,
 * which is precisely the property that makes the lock trustworthy.
 *
 * Throws when the lock is already held (another shim owns this session) or when
 * the platform cannot take it. Both are refusals to run, not warnings: a shim
 * that cannot prove it is the only one for its session must not start.
 */
export function acquireSessionLock(sessionId: string): () => void {
  return acquireExclusiveLock({
    kind: "session",
    subject: `session ${sessionId}`,
    file: lockPath(sessionId),
    context: { agent_repl_session_id: sessionId },
  });
}

/**
 * Take the workspace's exclusive lock and hold it until the process exits.
 *
 * Identical in contract to acquireSessionLock, and taken AFTER it: the fixed
 * order is what keeps two shims racing for the same pair from deadlocking each
 * other. Throws when another shim already owns the workspace or when the
 * platform cannot take the lock; a shim that cannot prove it is the workspace's
 * only one must not start.
 */
export function acquireWorkspaceLock(cwd: string): () => void {
  return acquireExclusiveLock({
    kind: "workspace",
    subject: `workspace ${cwd}`,
    file: workspaceLockPath(cwd),
    context: { workspace_dir: cwd, workspace_id: workspaceLockKey(cwd) },
  });
}

/** One kernel-enforced claim, which is the whole of both locks' mechanism. */
function acquireExclusiveLock(claim: {
  kind: string;
  subject: string;
  file: string;
  context: Record<string, unknown>;
}): () => void {
  LOGGER.log({ ...claim.context, platform: process.platform }, `acquiring exclusive shim ${claim.kind} lock`);
  if (O_EXLOCK === undefined) {
    throw new Error(
      `${COMPONENT}: ${process.platform} has no O_EXLOCK, so the shim cannot claim ${claim.subject} ` +
        `exclusively; refusing to start rather than risk two shims writing one transcript`,
    );
  }
  const file = claim.file;
  fs.mkdirSync(path.dirname(file), { recursive: true });

  let fd: number;
  try {
    // O_NONBLOCK so an already-held lock fails immediately instead of hanging
    // this process behind whichever shim owns the claim.
    fd = fs.openSync(file, fs.constants.O_CREAT | fs.constants.O_RDWR | O_EXLOCK | fs.constants.O_NONBLOCK);
  } catch (err) {
    const code = (err as NodeJS.ErrnoException).code;
    if (code === "EAGAIN" || code === "EWOULDBLOCK") {
      throw new Error(
        `${COMPONENT}: ${claim.subject} is already held by another shim (${file}); refusing to start a duplicate`,
      );
    }
    throw new Error(`${COMPONENT}: cannot take the ${claim.kind} lock ${file}: ${(err as Error).message}`);
  }

  LOGGER.log(claim.context, `holding ${claim.kind} lock ${file}`);
  let released = false;
  return () => {
    if (released) return;
    released = true;
    try {
      fs.closeSync(fd); // closing drops the flock
      LOGGER.log({ ...claim.context, lock_path: file }, `released exclusive shim ${claim.kind} lock`);
    } catch (err) {
      LOGGER.log({ level: "error", ...claim.context, cause: err }, `releasing the ${claim.kind} lock failed: ${(err as Error).message}`);
    }
  };
}
