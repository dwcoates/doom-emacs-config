/**
 * engine/compaction-mark.ts — the durable record that this transcript is
 * already compacted.
 *
 * WHAT IT IS FOR. `Hibernate` compacts, and a compaction is a real vendor
 * summary turn. Nothing in the transcript's own shape made a SECOND compaction
 * of the same conversation a no-op, so an idle sweep whose stand-down never
 * completed asked again five minutes later and paid for the whole summary turn
 * again: 13 of them on one workspace between 09:02 and 10:03 on 2026-09-14,
 * each appending another `compact_boundary` and another "continuation notes"
 * message the user watched arrive in the feed.
 *
 * WHAT IT KEYS ON. The transcript's BYTE LENGTH at the instant the compaction
 * finished. A transcript is append-only (`engine/compaction.ts` appends and
 * never truncates), so "the file is exactly as long as it was when I last
 * compacted it" is the same statement as "nothing has been said since" — and
 * it is a statement a fresh process can make, which a promise held in memory
 * cannot. That is the point: the daemon restarts, the shim restarts, and the
 * sweep asks again, and the answer must still be "already done".
 *
 * WHERE IT LIVES. Beside the shim's other per-workspace state, under
 * `$AGENT_REPL_STATE_DIR/shim/<workspace-key>/compaction/<vendor-session-id>.json`
 * — one file per vendor session, because a workspace's session id rotates and
 * a mark for a conversation that is no longer the live one must never be read
 * as this one's.
 *
 * IT IS AN OPTIMIZATION RECORD, NOT A SOURCE OF TRUTH. Every read failure
 * answers "no mark" and is recorded; the cost of a lost mark is one extra
 * compaction, and the cost of TREATING a read failure as a mark would be a
 * hibernation that never compacts at all.
 */
import { mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import path from "node:path";
import { bindLog } from "../log.js";
import { identityDir } from "./identity.js";

const LOGGER = bindLog({
  component: "shim-engine-compaction-mark",
  operation: "shim.engine.compaction-mark",
});

/** What one finished compaction recorded about the transcript it left behind. */
export interface CompactionMark {
  /** The transcript's byte length once the two records had been appended. */
  readonly transcript_bytes: number;
  /** When the compaction finished, in epoch milliseconds. */
  readonly compacted_at_ms: number;
}

/** The directory this workspace's compaction marks live in. */
export function compactionMarkDir(stateDir: string, workspaceKey: string): string {
  return path.join(identityDir(stateDir, workspaceKey), "compaction");
}

/** One vendor session's compaction mark file. */
export function compactionMarkPath(
  stateDir: string,
  workspaceKey: string,
  vendorSessionId: string,
): string {
  return path.join(compactionMarkDir(stateDir, workspaceKey), `${vendorSessionId}.json`);
}

/**
 * The mark this vendor session last wrote, or absence.
 *
 * ABSENCE COVERS EVERY FAILURE, AND EVERY FAILURE IS SAID OUT LOUD. A missing
 * file is the ordinary case and is not worth a record; anything else — an
 * unreadable file, a truncated write, a shape this build does not recognize —
 * is recorded and then answered as absence, so the hibernation compacts rather
 * than skipping on a mark it could not actually read.
 *
 * AT INFO AND NOT AT WARN, on purpose. Every one of those failures resolves to
 * the same benign outcome — the next hibernation buys one more summary turn —
 * and the shim's warn budget is reserved for a defect somebody must act on. A
 * record nobody has to act on still has to be VISIBLE, which is why none of
 * them is silent.
 */
export function readCompactionMark(
  stateDir: string,
  workspaceKey: string,
  vendorSessionId: string,
): CompactionMark | undefined {
  const file = compactionMarkPath(stateDir, workspaceKey, vendorSessionId);
  let contents: string;
  try {
    contents = readFileSync(file, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code !== "ENOENT") {
      LOGGER.info(
        { file, cause: err instanceof Error ? err.message : String(err) },
        "could not read this session's compaction mark; treating the transcript as uncompacted",
      );
    }
    return undefined;
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(contents);
  } catch (err) {
    LOGGER.info(
      { file, cause: err instanceof Error ? err.message : String(err) },
      "this session's compaction mark is not readable json; treating the transcript as uncompacted",
    );
    return undefined;
  }
  const record = parsed as Partial<CompactionMark> | null;
  if (
    record === null ||
    typeof record !== "object" ||
    typeof record.transcript_bytes !== "number" ||
    typeof record.compacted_at_ms !== "number"
  ) {
    LOGGER.info(
      { file },
      "this session's compaction mark states neither a length nor an instant; treating the transcript as uncompacted",
    );
    return undefined;
  }
  return { transcript_bytes: record.transcript_bytes, compacted_at_ms: record.compacted_at_ms };
}

/**
 * Record that this transcript is compacted at its current length.
 *
 * A WRITE THAT FAILS IS RECORDED AND NOTHING MORE. The compaction ITSELF
 * succeeded — the boundary and the summary are on the transcript — and
 * refusing the hibernation over a mark that could not be persisted would undo
 * a real vendor turn's worth of work over a bookkeeping file. The cost of the
 * loss is one extra compaction on a later ask, which is exactly the cost the
 * mark exists to avoid and not a correctness question.
 */
export function writeCompactionMark(
  stateDir: string,
  workspaceKey: string,
  vendorSessionId: string,
  mark: CompactionMark,
): void {
  const file = compactionMarkPath(stateDir, workspaceKey, vendorSessionId);
  try {
    mkdirSync(path.dirname(file), { recursive: true });
    writeFileSync(file, `${JSON.stringify(mark)}\n`, "utf8");
  } catch (err) {
    LOGGER.error(
      { file, cause: err instanceof Error ? err.message : String(err) },
      "could not persist this session's compaction mark; a later hibernation will compact again",
    );
    return;
  }
  LOGGER.debug(
    { file, transcript_bytes: mark.transcript_bytes },
    "recorded the transcript length this session is compacted at",
  );
}

/**
 * The transcript's byte length, or absence when it cannot be measured.
 *
 * The one reading of "how long is this file", shared by the mark's writer and
 * its reader so the two cannot measure differently.
 */
export function transcriptBytes(file: string): number | undefined {
  try {
    return statSync(file).size;
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code !== "ENOENT") {
      LOGGER.info(
        { file, cause: err instanceof Error ? err.message : String(err) },
        "could not measure the transcript's length",
      );
    }
    return undefined;
  }
}
