/**
 * test/integration-support/vendor.ts — read what the MOCKED vendor wrote.
 *
 * The mock writes vendor-shaped files exactly where the real binary would, and
 * several contract obligations are only observable there: that a transcript
 * materialized for the session at all, that a subagent got its `.meta.json`
 * with the four camelCase fields, that a stopped task's spool ends `EXIT=143`,
 * and that the keep-alive rewind actually discarded the trailing keep-alive
 * turns before the real prompt was delivered.
 *
 * The path helpers come from `src/fake/vendor-files.ts` on purpose: a test that
 * spelled the layout a second time would agree with itself and not with the
 * producer.
 */
import { existsSync, readFileSync, realpathSync, watch } from "node:fs";
import path from "node:path";
import {
  cwdSlug,
  spoolPath,
  subagentMetaPath,
  subagentTranscriptPath,
  transcriptPath,
  type TranscriptRecord,
} from "../../src/fake/vendor-files.js";
import type { ShimDirectories } from "./harness.js";

/**
 * The workspace as the CHILD sees it.
 *
 * macOS puts temp dirs under `/var`, a symlink to `/private/var`, and the shim
 * slugs its own `process.cwd()` — which the runtime has already resolved. A
 * test that slugged the unresolved path would look for a directory the mock
 * never created.
 */
export function workspaceRealPath(dirs: ShimDirectories): string {
  return realpathSync(dirs.workspace);
}

/** The session transcript's path for one vendor session id. */
export function sessionTranscriptPath(dirs: ShimDirectories, vendorSessionId: string): string {
  return transcriptPath(dirs.configDir, workspaceRealPath(dirs), vendorSessionId);
}

/** The directory every one of this session's vendor files lives under. */
export function projectDir(dirs: ShimDirectories): string {
  return path.join(dirs.configDir, "projects", cwdSlug(workspaceRealPath(dirs)));
}

/** Parse a JSONL vendor file into records; a missing file is an empty read. */
export function readJsonl(file: string): TranscriptRecord[] {
  if (!existsSync(file)) return [];
  return readFileSync(file, "utf8")
    .split("\n")
    .filter((line) => line.trim() !== "")
    .flatMap((line) => {
      try {
        return [JSON.parse(line) as TranscriptRecord];
      } catch {
        // A half-written trailing line is what a killed CLI leaves; it is not a
        // record yet, and reading it as one would invent a shape.
        return [];
      }
    });
}

/** The session transcript's records. */
export function readTranscript(
  dirs: ShimDirectories,
  vendorSessionId: string,
): TranscriptRecord[] {
  return readJsonl(sessionTranscriptPath(dirs, vendorSessionId));
}

/** One subagent's own chained sidechain transcript. */
export function readSubagentTranscript(
  dirs: ShimDirectories,
  vendorSessionId: string,
  agentId: string,
): TranscriptRecord[] {
  return readJsonl(
    subagentTranscriptPath(dirs.configDir, workspaceRealPath(dirs), vendorSessionId, agentId),
  );
}

/** One subagent's metadata sidecar — exactly four camelCase fields. */
export function readSubagentMeta(
  dirs: ShimDirectories,
  vendorSessionId: string,
  agentId: string,
): Record<string, unknown> {
  const file = subagentMetaPath(
    dirs.configDir,
    workspaceRealPath(dirs),
    vendorSessionId,
    agentId,
  );
  return JSON.parse(readFileSync(file, "utf8")) as Record<string, unknown>;
}

/** One task spool's raw bytes. */
export function readSpool(
  dirs: ShimDirectories,
  vendorSessionId: string,
  taskId: string,
): string {
  const file = spoolPath(dirs.spoolRoot, workspaceRealPath(dirs), vendorSessionId, taskId);
  return existsSync(file) ? readFileSync(file, "utf8") : "";
}

/** Where one task spool lives, for path assertions against announcements. */
export function spoolFilePath(
  dirs: ShimDirectories,
  vendorSessionId: string,
  taskId: string,
): string {
  return spoolPath(dirs.spoolRoot, workspaceRealPath(dirs), vendorSessionId, taskId);
}

/**
 * Resolve once `file` exists.
 *
 * Watching the DIRECTORY rather than the file: a file that does not exist yet
 * cannot be watched, and the creation is exactly the event being waited for.
 * The existence check runs first, so a file already written never waits.
 */
export async function awaitFile(file: string): Promise<void> {
  if (existsSync(file)) return;
  const dir = path.dirname(file);
  await new Promise<void>((resolve) => {
    const watcher = watch(dir, () => {
      if (!existsSync(file)) return;
      watcher.close();
      resolve();
    });
    // The file may have appeared between the check above and the watcher being
    // installed; re-checking here closes that window without a poll.
    if (existsSync(file)) {
      watcher.close();
      resolve();
    }
  });
}

/** Every user-prompt record in a transcript, oldest first. */
export function userPrompts(records: readonly TranscriptRecord[]): TranscriptRecord[] {
  return records.filter((record) => record.type === "user" && record.promptId !== undefined);
}

/** The text of a transcript user record's first content block, when it has one. */
export function promptText(record: TranscriptRecord): string {
  const message = record.message as { content?: unknown } | undefined;
  const content = message?.content;
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  const first = content[0] as { text?: unknown } | undefined;
  return typeof first?.text === "string" ? first.text : "";
}
