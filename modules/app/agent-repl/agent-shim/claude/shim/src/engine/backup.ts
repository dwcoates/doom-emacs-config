/**
 * engine/backup.ts — the bounded transcript backup.
 *
 * RESPONSIBILITY. Copy the vendor transcript at every turn end and at every
 * vendor-uuid rotation into a bounded, pruned backup directory beside the work,
 * restorable.
 *
 * WHY IT EXISTS. The transcript is THE ONE ARTIFACT NOBODY CAN REGENERATE. Code
 * can be rewritten and state can be rebuilt; a conversation cannot. The backup
 * is the rung below the fresh-start refusal: when the refusal fails to protect
 * the transcript, this is what still has a copy. Bounded and pruned because an
 * unbounded backup of a growing file eventually costs more than it protects.
 *
 * A FAILED BACKUP NEVER FAILS A TURN. It is recorded at `warn` and the turn
 * ends normally: refusing to conclude a turn because a copy could not be made
 * would trade the conversation for the copy of it.
 *
 * A TRANSCRIPT THAT WAS NEVER WRITTEN IS NOT A FAILED BACKUP. It is recorded,
 * at `info`, as the nothing-to-copy it is; only a copy that could have been
 * made and was not is a warning.
 */
import { copyFileSync, mkdirSync, readdirSync, rmSync } from "node:fs";
import path from "node:path";
import { bindLog } from "../log.js";
import { identityDir } from "./identity.js";

const LOGGER = bindLog({ component: "shim-engine-backup", operation: "shim.engine.backup" });

/**
 * How many copies are kept.
 *
 * Enough to survive a bad run of turns (the damage a backup protects against is
 * usually noticed a few turns later), small enough that a large transcript
 * cannot fill a disk with its own history.
 */
export const BACKUP_KEEP = 8;

/** The directory this workspace's transcript copies live in. */
export function backupDir(stateDir: string, workspaceKey: string): string {
  return path.join(identityDir(stateDir, workspaceKey), "backups");
}

/** One copy's name: which conversation, and when it was taken. */
export function backupName(vendorSessionId: string, atMs: number): string {
  return `${vendorSessionId}-${atMs}.jsonl`;
}

/** Take a copy and prune the directory back to {@link BACKUP_KEEP}. */
export function backupTranscript(options: {
  readonly transcript: string;
  readonly stateDir: string;
  readonly workspaceKey: string;
  readonly vendorSessionId: string;
  readonly atMs: number;
  readonly keep?: number;
}): void {
  const directory = backupDir(options.stateDir, options.workspaceKey);
  const target = path.join(directory, backupName(options.vendorSessionId, options.atMs));
  try {
    mkdirSync(directory, { recursive: true });
    copyFileSync(options.transcript, target);
    LOGGER.info(
      { transcript: options.transcript, backup: target, vendor_session_id: options.vendorSessionId },
      "backed up the vendor transcript",
    );
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
  pruneBackups(directory, options.keep ?? BACKUP_KEEP);
}

/** Whether a copy failed only because the transcript it names does not exist. */
function missingSource(err: unknown, transcript: string): boolean {
  if (!(err instanceof Error)) return false;
  const failure = err as NodeJS.ErrnoException;
  return failure.code === "ENOENT" && failure.path === transcript;
}

/**
 * Keep the newest `keep` copies and delete the rest.
 *
 * Ordered by NAME, which is ordered by the millisecond stamp the name ends
 * with — a fixed-width integer within any one session, so lexical order is
 * chronological order and no stat call is needed.
 */
export function pruneBackups(directory: string, keep: number): void {
  let names: string[];
  try {
    names = readdirSync(directory).filter((name) => name.endsWith(".jsonl"));
  } catch (err) {
    // warn: a defect because backup retention could not inspect its directory.
    LOGGER.warn(
      { directory, cause: err instanceof Error ? err.message : String(err) },
      "could not list the transcript backup directory to prune it",
    );
    return;
  }
  if (names.length <= keep) return;
  const doomed = names.sort().slice(0, names.length - keep);
  for (const name of doomed) {
    try {
      rmSync(path.join(directory, name));
      LOGGER.debug({ directory, pruned: name, keep }, "pruned an old transcript backup");
    } catch (err) {
      // warn: a defect because backup retention left an expired copy on disk.
      LOGGER.warn(
        { directory, pruned: name, cause: err instanceof Error ? err.message : String(err) },
        "could not prune a transcript backup",
      );
    }
  }
}
