/**
 * engine/backup.ts — the one-copy transcript backup.
 *
 * RESPONSIBILITY. Mirror the vendor transcript at every turn end and at every
 * vendor-uuid rotation into ONE backup file per conversation, beside the work,
 * restorable.
 *
 * WHY IT EXISTS. The transcript is THE ONE ARTIFACT NOBODY CAN REGENERATE. Code
 * can be rewritten and state can be rebuilt; a conversation cannot. The backup
 * is the rung below the fresh-start refusal: when the refusal fails to protect
 * the transcript, this is what still has a copy.
 *
 * ONE COPY, APPENDED, NEVER OVERWRITTEN. The vendor only ever appends to a
 * transcript, so the backup is kept a byte-for-byte PREFIX of it: each backup
 * appends the bytes written since the last one. A transcript that shrank or
 * whose existing bytes changed is the damage the backup exists for, so the
 * backup is left exactly as it was (the last good copy) and the refusal is
 * recorded at `warn`. One copy therefore protects what a rolling set of whole
 * copies did (damage noticed turns later still finds the pre-damage bytes),
 * at the cost of one transcript instead of many.
 *
 * A FAILED BACKUP NEVER FAILS A TURN. It is recorded at `warn` and the turn
 * ends normally: refusing to conclude a turn because a copy could not be made
 * would trade the conversation for the copy of it.
 *
 * A TRANSCRIPT THAT WAS NEVER WRITTEN IS NOT A FAILED BACKUP. It is recorded,
 * at `info`, as the nothing-to-copy it is; only a copy that could have been
 * made and was not is a warning.
 */
import { appendFileSync, closeSync, fstatSync, mkdirSync, openSync, readdirSync, readSync, rmSync } from "node:fs";
import path from "node:path";
import { bindLog } from "../log.js";
import { identityDir } from "./identity.js";

const LOGGER = bindLog({ component: "shim-engine-backup", operation: "shim.engine.backup" });

/** How many bytes one compare or append step reads at a time. */
const CHUNK_BYTES = 1 << 20;

/** The directory this workspace's transcript backups live in. */
export function backupDir(stateDir: string, workspaceKey: string): string {
  return path.join(identityDir(stateDir, workspaceKey), "backups");
}

/** One conversation's backup: the only file it ever has. */
export function backupName(vendorSessionId: string): string {
  return `${vendorSessionId}.jsonl`;
}

/** What one mirror step did to the backup. */
export type MirrorOutcome =
  /** The backup is a prefix of the transcript again; `appended` bytes were added. */
  | { readonly kind: "mirrored"; readonly appended: number; readonly bytes: number }
  /**
   * The transcript is no longer an extension of the backup, so the backup was
   * left untouched. `at` is the first offset the two disagree at (the
   * transcript's length when it shrank below the backup).
   */
  | { readonly kind: "diverged"; readonly at: number; readonly transcriptBytes: number; readonly backupBytes: number };

/** Mirror the transcript and record the outcome; never throws. */
export function backupTranscript(options: {
  readonly transcript: string;
  readonly stateDir: string;
  readonly workspaceKey: string;
  readonly vendorSessionId: string;
}): void {
  const directory = backupDir(options.stateDir, options.workspaceKey);
  const target = path.join(directory, backupName(options.vendorSessionId));
  let outcome: MirrorOutcome;
  try {
    mkdirSync(directory, { recursive: true });
    outcome = mirrorTranscript(options.transcript, target);
  } catch (err) {
    // THERE IS NOTHING TO COPY IS NOT A FAILED COPY. A vendor session id whose
    // transcript file was never written — the id a session ROTATES AWAY FROM at
    // its very start, before the vendor has taken a single turn under it — has
    // no bytes to lose, and every rotation announced one of these as a defect.
    // Matched on the SOURCE path specifically: the target's directory is
    // created recursively one line above, so a `ENOENT` naming anything else is
    // still the unexplained failure this record exists for.
    if (missingSource(err, options.transcript)) {
      LOGGER.info(
        { transcript: options.transcript, vendor_session_id: options.vendorSessionId },
        "no transcript exists under this vendor session id, so there is nothing to back up",
      );
      return;
    }
    // warn: a defect because the transcript backup failed while the turn continued.
    LOGGER.warn(
      {
        transcript: options.transcript,
        backup: target,
        cause: err instanceof Error ? err.message : String(err),
      },
      "transcript backup FAILED; the turn proceeds — a copy is worth less than the conversation it copies",
    );
    return;
  }
  if (outcome.kind === "diverged") {
    // warn: a defect because the transcript lost or changed bytes the backup holds.
    LOGGER.warn(
      {
        transcript: options.transcript,
        backup: target,
        vendor_session_id: options.vendorSessionId,
        diverged_at: outcome.at,
        transcript_bytes: outcome.transcriptBytes,
        backup_bytes: outcome.backupBytes,
      },
      "the transcript no longer extends its backup, so the backup keeps the last good copy and takes nothing newer",
    );
    return;
  }
  LOGGER.info(
    {
      transcript: options.transcript,
      backup: target,
      vendor_session_id: options.vendorSessionId,
      appended_bytes: outcome.appended,
      backup_bytes: outcome.bytes,
    },
    "backed up the vendor transcript",
  );
  removeLegacyCopies(directory, options.vendorSessionId);
}

/**
 * Bring `target` up to `source` by appending what `source` grew by, when
 * `source` still begins with every byte `target` holds; leave `target` alone
 * otherwise. Throws on any filesystem failure, naming the path it failed on.
 *
 * An interrupted append leaves `target` a shorter prefix of `source`, which the
 * next call simply extends.
 */
export function mirrorTranscript(source: string, target: string): MirrorOutcome {
  const from = openSync(source, "r");
  try {
    const to = openSync(target, "a+");
    try {
      const transcriptBytes = fstatSync(from).size;
      const backupBytes = fstatSync(to).size;
      if (transcriptBytes < backupBytes) {
        return { kind: "diverged", at: transcriptBytes, transcriptBytes, backupBytes };
      }
      const theirs = Buffer.allocUnsafe(CHUNK_BYTES);
      const ours = Buffer.allocUnsafe(CHUNK_BYTES);
      for (let at = 0; at < backupBytes; at += CHUNK_BYTES) {
        const length = Math.min(CHUNK_BYTES, backupBytes - at);
        readFully(from, theirs, length, at);
        readFully(to, ours, length, at);
        const differs = firstDifference(theirs, ours, length);
        if (differs >= 0) {
          return { kind: "diverged", at: at + differs, transcriptBytes, backupBytes };
        }
      }
      for (let at = backupBytes; at < transcriptBytes; at += CHUNK_BYTES) {
        const length = Math.min(CHUNK_BYTES, transcriptBytes - at);
        readFully(from, theirs, length, at);
        appendFileSync(to, theirs.subarray(0, length));
      }
      return { kind: "mirrored", appended: transcriptBytes - backupBytes, bytes: transcriptBytes };
    } finally {
      closeSync(to);
    }
  } finally {
    closeSync(from);
  }
}

/** Read exactly `length` bytes at `position`; a short file is an error. */
function readFully(fd: number, into: Buffer, length: number, position: number): void {
  let done = 0;
  while (done < length) {
    const read = readSync(fd, into, done, length - done, position + done);
    if (read === 0) throw new Error(`transcript backup: the file ended at ${String(position + done)}, expected ${String(position + length)} bytes`);
    done += read;
  }
}

/** The first offset below `length` where the two buffers differ, or -1. */
function firstDifference(a: Buffer, b: Buffer, length: number): number {
  if (a.subarray(0, length).equals(b.subarray(0, length))) return -1;
  for (let i = 0; i < length; i++) if (a[i] !== b[i]) return i;
  return -1;
}

/** Whether a copy failed only because the transcript it names does not exist. */
function missingSource(err: unknown, transcript: string): boolean {
  if (!(err instanceof Error)) return false;
  const failure = err as NodeJS.ErrnoException;
  return failure.code === "ENOENT" && failure.path === transcript;
}

/**
 * Whether `name` is one of the timestamped whole copies (`<id>-<ms>.jsonl`)
 * the backup took before it kept one appended copy.
 */
export function isLegacyCopy(name: string, vendorSessionId: string): boolean {
  const prefix = `${vendorSessionId}-`;
  return name.startsWith(prefix) && /^\d+\.jsonl$/.test(name.slice(prefix.length));
}

/**
 * Delete this conversation's timestamped whole copies once its one appended
 * copy exists, so a workspace upgraded mid-conversation stops carrying them.
 */
export function removeLegacyCopies(directory: string, vendorSessionId: string): void {
  let names: string[];
  try {
    names = readdirSync(directory);
  } catch (err) {
    legacyCopyStayed({ directory, vendor_session_id: vendorSessionId, step: "list" }, err);
    return;
  }
  for (const name of names.filter((n) => isLegacyCopy(n, vendorSessionId))) {
    try {
      rmSync(path.join(directory, name));
      LOGGER.debug({ directory, removed: name }, "removed an old whole transcript copy");
    } catch (err) {
      legacyCopyStayed({ directory, vendor_session_id: vendorSessionId, step: "remove", removed: name }, err);
    }
  }
}

/** Record that old whole copies stayed on disk, at the step that failed. */
function legacyCopyStayed(fields: { readonly [field: string]: string }, err: unknown): void {
  // warn: a defect because an old whole transcript copy stayed on disk.
  LOGGER.warn(
    { ...fields, cause: err instanceof Error ? err.message : String(err) },
    "could not remove the old whole transcript copies",
  );
}
