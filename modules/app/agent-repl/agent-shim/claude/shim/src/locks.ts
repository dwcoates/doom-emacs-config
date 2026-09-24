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
 * So the shim takes a kernel-enforced lock and holds it for its lifetime. Held
 * is what the daemon probes before spawning.
 *
 * BOTH LOCKS ARE TAKEN INSIDE `StartSession`, not at process start. A shim that
 * has bound its socket but has no session is INERT: it owns no conversation, so
 * it must exclude nobody. That is what lets the daemon prelaunch a replacement
 * shim beside the live one it is about to retire — a startup-time workspace
 * claim would wedge the newcomer behind a lock the live shim only drops when it
 * dies. The probe's meaning is unchanged: a held lock still means a LIVE shim
 * owns the conversation, because only a shim with a session holds one.
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
 * either is a refusal to START THE SESSION: `StartSession` answers
 * `conversation_owned` (or `lock_holder_unavailable`, when this shim could not
 * spawn its own holder) and the process stays inert and serving.
 *
 * # Mechanism
 *
 * A `flock(2)` — the same lock Go's `syscall.Flock` takes, so the daemon's
 * probe and this claim interoperate. Two properties matter and neither is
 * available from a plain lock FILE:
 *
 *   - the kernel enforces it, so exclusion is not advisory bookkeeping; and
 *   - it is released automatically when the process dies, however it dies, so
 *     there is no stale lock to reap and no PID-reuse hazard.
 *
 * NODE CANNOT TAKE ONE. It has no `flock` binding, and the predecessor reached
 * one through `open(2)`'s `O_EXLOCK`, which exists only on macOS/BSD — so on
 * Linux the shim refused to start ANY session and every Linux deployment was
 * dead in the water.
 *
 * So the claim is a CHILD PROCESS: `shim-lock <path>` (agent-shim/shim-lock)
 * takes the flock, writes one `locked` line, and holds it until its stdin
 * reaches EOF. This process keeps the write end of that pipe, so both
 * properties survive intact: the lock is a real kernel flock on the path the
 * daemon probes, and when THIS process dies however it dies the kernel closes
 * the pipe, the holder reads EOF and exits, and the lock goes with it. One
 * code path on both platforms; nothing here is conditioned on the platform any
 * more.
 */
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { createHash } from "node:crypto";
import { bindLog } from "./log.js";

const COMPONENT = "shim-session-lock";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.session-lock.lifecycle" });

/**
 * The exit code `shim-lock` reserves for "another process already holds this".
 *
 * It is spelled distinctly from a generic failure because the two answers are
 * not the same refusal: a held lock is a live shim owning the conversation,
 * which `StartSession` reports as `conversation_owned`, while any other
 * failure is the claim not having been ATTEMPTED. Collapsing them would make
 * an unwritable lock directory look like a duplicate shim.
 */
const HELD_EXIT_CODE = 3;

/** The one line `shim-lock` writes to stdout, and only once it holds the lock. */
const READY_LINE = "locked";

/**
 * How long a spawned holder has to answer `locked` (or exit) before the claim
 * is refused.
 *
 * `shim-lock` takes `LOCK_EX|LOCK_NB`, so it never waits on the kernel: a
 * healthy answer is one exec plus one flock. Measured spawn-to-`locked` on the
 * deployed binary (darwin, 2026-09-24): 300 serial claims max 4.3 ms (p50
 * 1.9 ms, first exec of a fresh copy included); 32 concurrent max 16.0 ms; 128
 * concurrent (a spawn storm far past any real fleet) max 59.6 ms. The bound is
 * ~4x that worst observed case. Without it a holder that spawned but neither
 * answered nor exited would hang StartSession forever.
 */
export const HOLDER_ANSWER_TIMEOUT_MS = 250;

/**
 * The environment variable that names the lock-holder binary.
 *
 * It exists for the suites and worlds that build their own `shim-lock` rather
 * than reading the deployed one: the e2e harnesses compile it into a temp
 * directory, and a shim that only ever looked in the deploy location would
 * take its locks with whatever binary the machine happened to have installed.
 */
export const LOCK_BIN_ENV = "AGENT_REPL_SHIM_LOCK_BIN";

/**
 * The `shim-lock` binary this process spawns: the override when set, otherwise
 * the deploy location `bin/build-frontend.sh lock` installs it at, beside
 * shim-store and shim-claude-sidecar.
 */
export function lockBinaryPath(): string {
  const override = process.env[LOCK_BIN_ENV];
  if (override !== undefined && override !== "") return override;
  return path.join(os.homedir(), ".cache", "agent-repl", "bin", "shim-lock");
}

/**
 * The environment variable that relocates the kernel-lock directory.
 *
 * It exists because the lock directory is a CROSS-SYSTEM rendezvous: the daemon
 * probes the workspace lock by path, so a test that wants isolated locks cannot
 * simply point the shim somewhere else — both sides must agree. One variable
 * both processes read is that agreement. Unset (or empty) keeps the default,
 * which is the convention the deployed daemon probes.
 */
export const LOCK_DIR_ENV = "AGENT_REPL_LOCK_DIR";

/** The directory the kernel locks live in: a sibling of sock/ and store/. */
export function lockDir(): string {
  const override = process.env[LOCK_DIR_ENV];
  if (override !== undefined && override !== "") return override;
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
 * THIS shim could not start its own lock holder, so no claim was attempted.
 *
 * It is its own error class because it is its own refusal: a held lock means a
 * live shim owns the conversation (`conversation_owned`), while an unspawnable
 * holder means NOBODY is known to own it and this shim's deployment is broken
 * (a missing or unexecutable `shim-lock`). `StartSession` answers
 * `lock_holder_unavailable` carrying both fields, and nothing else does.
 */
export class LockHolderUnavailableError extends Error {
  /** The holder binary the spawn was asked to run. */
  readonly binary: string;
  /** The operating system's account of why the spawn failed. */
  readonly osError: string;

  constructor(binary: string, osError: string, message: string) {
    super(message);
    this.name = "LockHolderUnavailableError";
    this.binary = binary;
    this.osError = osError;
  }
}

/**
 * The teardown half of a claim: drop the lock and wait for the holder to be
 * gone. Awaiting matters — a caller that re-acquires immediately (the daemon
 * retiring a shim and starting its replacement) must not race the holder's
 * exit.
 */
export type LockRelease = () => void | Promise<void>;

/**
 * Take this session's exclusive lock and hold it until the process exits.
 *
 * Returns a release function for a deliberate teardown. Not calling it is fine
 * and is the normal path — the kernel drops the lock when the process dies,
 * which is precisely the property that makes the lock trustworthy.
 *
 * Rejects when the lock is already held (another shim owns this session) or
 * when the claim could not be attempted at all. Both are refusals to run, not
 * warnings: a shim that cannot prove it is the only one for its session must
 * not start.
 */
export function acquireSessionLock(sessionId: string): Promise<LockRelease> {
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
 * other. Rejects when another shim already owns the workspace or when the
 * claim could not be attempted; a shim that cannot prove it is the workspace's
 * only one must not start A SESSION — the caller turns the rejection into
 * `StartSession{conversation_owned}` and keeps serving, inert.
 */
export function acquireWorkspaceLock(cwd: string): Promise<LockRelease> {
  return acquireExclusiveLock({
    kind: "workspace",
    subject: `workspace ${cwd}`,
    file: workspaceLockPath(cwd),
    context: { workspace_dir: cwd, workspace_id: workspaceLockKey(cwd) },
  });
}

/**
 * One kernel-enforced claim, which is the whole of both locks' mechanism.
 *
 * Resolves once `shim-lock` has announced it HOLDS the flock, never merely
 * once it has been spawned.
 */
function acquireExclusiveLock(claim: {
  kind: string;
  subject: string;
  file: string;
  context: Record<string, unknown>;
}): Promise<LockRelease> {
  LOGGER.debug({ ...claim.context, platform: process.platform }, `acquiring exclusive shim ${claim.kind} lock`);
  const file = claim.file;
  fs.mkdirSync(path.dirname(file), { recursive: true });
  const binary = lockBinaryPath();

  let child: ChildProcessWithoutNullStreams;
  try {
    // stdin is a PIPE and stays open for the claim's whole life: it is the
    // channel whose EOF releases the lock, and the kernel closes it for us when
    // this process dies however it dies.
    child = spawn(binary, [file], { stdio: ["pipe", "pipe", "pipe"] });
  } catch (err) {
    return Promise.reject(
      holderUnavailable(claim, binary, file, err),
    );
  }

  return new Promise<LockRelease>((resolve, reject) => {
    let settled = false;
    let stdout = "";
    let stderr = "";

    /** Every branch below ends here, so no claim can resolve and reject both. */
    const settle = (act: () => void): void => {
      if (settled) return;
      settled = true;
      clearTimeout(answerDeadline);
      act();
    };

    // A HOLDER THAT NEITHER ANSWERS NOR EXITS is refused, and killed so it
    // cannot take the lock after this claim has already been refused.
    const answerDeadline = setTimeout(() => {
      settle(() => {
        child.kill("SIGKILL");
        reject(holderSilent(claim, binary, file));
      });
    }, HOLDER_ANSWER_TIMEOUT_MS);

    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stderr.on("data", (chunk: string) => {
      stderr += chunk;
    });
    // A write to a holder that has already died raises EPIPE on this stream.
    // It is recorded rather than thrown: the holder being gone IS the release
    // this process was asking for, and an unhandled 'error' event would take
    // the shim down over a lock it no longer needs.
    child.stdin.on("error", (err: Error) => {
      // warn: a defect because the lock holder disappeared while the shim kept running.
      LOGGER.warn(
        { ...claim.context, lock_path: file, cause: err.message },
        `the ${claim.kind} lock holder's stdin failed; the holder is gone and so is the lock`,
      );
    });

    // Before the claim settles, an 'error' event is Node reporting that the
    // holder could not be SPAWNED (ENOENT, EACCES): the asynchronous twin of
    // the synchronous throw above, and the same refusal. After it settles it
    // is a failed kill of a live holder, recorded as that and never misread
    // as a spawn failure.
    child.on("error", (err: Error) => {
      if (settled) {
        // warn: a defect because a settled claim's holder raised an error (a failed kill).
        LOGGER.warn(
          { ...claim.context, lock_path: file, lock_binary: binary, cause: err.message },
          `the ${claim.kind} lock holder raised an error after its claim settled`,
        );
        return;
      }
      settle(() => reject(holderUnavailable(claim, binary, file, err)));
    });

    // THE HOLDER'S OWN DIAGNOSTICS, folded into this component's records rather
    // than left on a pipe nobody drains. A held lock's reason is on stderr, and
    // it is what makes a refusal readable.
    const exited = new Promise<void>((exit) => {
      child.on("close", (code: number | null, signal: NodeJS.Signals | null) => {
        settle(() =>
          reject(
            code === HELD_EXIT_CODE
              ? new Error(
                  `${COMPONENT}: ${claim.subject} is already held by another shim (${file}); ` +
                    `refusing to start a duplicate`,
                )
              : new Error(
                  `${COMPONENT}: the lock holder for the ${claim.kind} lock ${file} exited ` +
                    `(code ${code ?? "null"}, signal ${signal ?? "null"}) before taking it: ${stderr.trim()}`,
                ),
          ),
        );
        exit();
      });
    });

    child.stdout.on("data", (chunk: string) => {
      stdout += chunk;
      if (!stdout.includes("\n")) return;
      const line = stdout.slice(0, stdout.indexOf("\n")).trim();
      if (line !== READY_LINE) {
        settle(() => {
          child.kill("SIGKILL");
          reject(
            new Error(
              `${COMPONENT}: the lock holder for the ${claim.kind} lock ${file} announced ` +
                `${JSON.stringify(line)}, not ${JSON.stringify(READY_LINE)}`,
            ),
          );
        });
        return;
      }
      settle(() => {
        LOGGER.debug(claim.context, `holding ${claim.kind} lock ${file}`);
        let released = false;
        resolve(async () => {
          if (released) return;
          released = true;
          // Closing stdin is the release; awaiting the exit is what makes the
          // lock provably gone by the time this resolves.
          child.stdin.end();
          await exited;
          LOGGER.debug(
            { ...claim.context, lock_path: file, holder_stderr: stderr.trim() },
            `released exclusive shim ${claim.kind} lock`,
          );
        });
      });
    });
  });
}

/**
 * A spawned holder that never answered inside {@link HOLDER_ANSWER_TIMEOUT_MS}:
 * recorded at ERROR and refused as `lock_holder_unavailable`, because nobody is
 * known to own the conversation and this shim's holder is broken.
 */
function holderSilent(
  claim: { kind: string; context: Record<string, unknown> },
  binary: string,
  file: string,
): LockHolderUnavailableError {
  const osError = `the lock holder gave no ${JSON.stringify(READY_LINE)} answer within ${HOLDER_ANSWER_TIMEOUT_MS} ms and was killed`;
  // error: a defect, because a healthy holder answers in milliseconds and this
  // one neither answered nor exited, so no session can start on it.
  LOGGER.error(
    { ...claim.context, lock_path: file, lock_binary: binary, os_error: osError, timeout_ms: HOLDER_ANSWER_TIMEOUT_MS },
    `the ${claim.kind} lock holder ${binary} timed out: ${osError}`,
  );
  return new LockHolderUnavailableError(
    binary,
    osError,
    `${COMPONENT}: the lock holder ${binary} for the ${claim.kind} lock ${file} timed out: ${osError}`,
  );
}

/**
 * The one spelling of "this shim's lock holder would not start", recorded at
 * ERROR and returned as the typed error `StartSession` maps onto
 * `lock_holder_unavailable`. Both spawn-failure branches come through here, so
 * the record and the refusal cannot drift apart.
 */
function holderUnavailable(
  claim: { kind: string; context: Record<string, unknown> },
  binary: string,
  file: string,
  err: unknown,
): LockHolderUnavailableError {
  const osError = err instanceof Error ? err.message : String(err);
  // error: a defect, because the shim's own lock helper is missing or cannot
  // be executed, so no session can start in this deployment at all.
  LOGGER.error(
    { ...claim.context, lock_path: file, lock_binary: binary, os_error: osError, cause: osError },
    `the ${claim.kind} lock holder ${binary} could not be spawned: ${osError}`,
  );
  return new LockHolderUnavailableError(
    binary,
    osError,
    `${COMPONENT}: cannot spawn the lock holder ${binary} for the ${claim.kind} lock ${file}: ${osError}`,
  );
}
