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
  if (O_EXLOCK === undefined) {
    throw new Error(
      `${COMPONENT}: ${process.platform} has no O_EXLOCK, so the shim cannot claim session ${sessionId} ` +
        `exclusively; refusing to start rather than risk two shims writing one transcript`,
    );
  }
  const exlock = O_EXLOCK;
  const file = lockPath(sessionId);
  fs.mkdirSync(path.dirname(file), { recursive: true });

  let fd: number;
  try {
    // O_NONBLOCK so an already-held lock fails immediately instead of hanging
    // this process behind whichever shim owns the session.
    fd = fs.openSync(file, fs.constants.O_CREAT | fs.constants.O_RDWR | exlock | fs.constants.O_NONBLOCK);
  } catch (err) {
    const code = (err as NodeJS.ErrnoException).code;
    if (code === "EAGAIN" || code === "EWOULDBLOCK") {
      throw new Error(
        `${COMPONENT}: session ${sessionId} is already held by another shim (${file}); refusing to start a duplicate`,
      );
    }
    throw new Error(`${COMPONENT}: cannot take the session lock ${file}: ${(err as Error).message}`);
  }

  LOGGER.log({ agent_repl_session_id: sessionId }, `holding session lock ${file}`);
  let released = false;
  return () => {
    if (released) return;
    released = true;
    try {
      fs.closeSync(fd); // closing drops the flock
    } catch (err) {
      LOGGER.log({ level: "error", agent_repl_session_id: sessionId, cause: err }, `releasing session lock failed: ${(err as Error).message}`);
    }
  };
}
