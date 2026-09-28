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
import { existsSync, readdirSync, readFileSync, realpathSync, watch } from "node:fs";
import path from "node:path";
import { ReDrain } from "./redrain.js";
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

/**
 * One subagent's metadata sidecar, FOUND BY THE SPAWNING CALL.
 *
 * THE TWO PLANES USE TWO KEYS, and this is the join between them. On the wire a
 * subagent's `AgentId` IS the spawning call's `tool_use_id` (landing 3's minting
 * rule). On disk the vendor names the files by its OWN 17-hex `agentId`, and
 * `meta.toolUseId` is the only link back to the call — so a reader that guessed
 * the file name from the wire id found nothing. Real captures are keyed that
 * way and the mock's layout stays vendor-faithful, so the join belongs here, in
 * the reader.
 */
export function findSubagentMetaByToolUseId(
  dirs: ShimDirectories,
  vendorSessionId: string,
  toolUseId: string,
): Record<string, unknown> {
  return subagentMetaEntry(dirs, vendorSessionId, toolUseId).meta;
}

/**
 * The vendor task LOCATOR of the subagent `toolUseId` spawned: the `<id>` of
 * the `agent-<id>.meta.json` that names that call — read exactly as the
 * sidecar reads it, from the file's name, to pair it with the agent.
 */
export function findSubagentLocatorByToolUseId(
  dirs: ShimDirectories,
  vendorSessionId: string,
  toolUseId: string,
): string {
  return subagentMetaEntry(dirs, vendorSessionId, toolUseId).locator;
}

/** The one scan both lookups share: the meta naming `toolUseId`, and its file's locator. */
function subagentMetaEntry(
  dirs: ShimDirectories,
  vendorSessionId: string,
  toolUseId: string,
): { readonly locator: string; readonly meta: Record<string, unknown> } {
  const dir = path.dirname(
    subagentMetaPath(dirs.configDir, workspaceRealPath(dirs), vendorSessionId, "probe"),
  );
  if (!existsSync(dir)) throw new Error(`no subagents directory at ${dir}`);
  const metas = readdirSync(dir).filter((name) => name.endsWith(".meta.json"));
  for (const name of metas) {
    const meta = JSON.parse(readFileSync(path.join(dir, name), "utf8")) as Record<string, unknown>;
    if (meta.toolUseId === toolUseId) {
      return { locator: name.replace(/^agent-/, "").replace(/\.meta\.json$/, ""), meta };
    }
  }
  throw new Error(
    `no subagent meta names tool_use_id ${toolUseId}; the directory holds ${metas.join(", ")}`,
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
 * LEVEL, THEN EDGE, THEN LEVEL AGAIN. The existence check runs first, so a file
 * already written never waits; the DIRECTORY is watched rather than the file,
 * because a file that does not exist yet cannot be watched and its creation is
 * exactly the event being waited for; and the level is re-checked after the
 * watcher is installed, closing the window between the two.
 *
 * The bounded re-drain beside the watcher is NOT a poll standing in for the
 * event — it is the ruled backstop for an FSEvents notification that is never
 * delivered at all (see `redrain.ts`). Without it a dropped edge does not fail,
 * it hangs.
 */
export async function awaitFile(file: string): Promise<void> {
  if (existsSync(file)) return;
  const dir = path.dirname(file);
  await new Promise<void>((resolve) => {
    let settled = false;
    const finish = (): void => {
      if (settled || !existsSync(file)) return;
      settled = true;
      redrain.stop();
      watcher.close();
      resolve();
    };
    const redrain = new ReDrain(finish);
    const watcher = watch(dir, finish);
    redrain.start();
    // The file may have appeared between the check above and the watcher being
    // installed; re-checking here closes that window without a poll.
    finish();
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

/** Where this session's task spools live. */
export function spoolDir(dirs: ShimDirectories, vendorSessionId: string): string {
  return path.dirname(spoolFilePath(dirs, vendorSessionId, "placeholder"));
}

/**
 * Every spool the mock wrote for one session, by task id.
 *
 * Read by SCANNING rather than by deriving one path from a wire value: the
 * vendor's task id never appears on the wire (the `DetachedWorkId` is the
 * spawning call's tool_use_id), so a test that built the filename from an
 * announcement would be asserting a mapping the contract deliberately hides.
 */
export function readSpools(
  dirs: ShimDirectories,
  vendorSessionId: string,
): Array<{ readonly taskId: string; readonly text: string }> {
  const dir = spoolDir(dirs, vendorSessionId);
  if (!existsSync(dir)) return [];
  return readdirSync(dir)
    .filter((name) => name.endsWith(".output"))
    .map((name) => ({
      taskId: name.replace(/\.output$/, ""),
      text: readFileSync(path.join(dir, name), "utf8"),
    }));
}

/** Resolve once any spool for the session terminates with `EXIT=<code>`. */
export async function awaitSpoolExit(
  dirs: ShimDirectories,
  vendorSessionId: string,
  code: number,
): Promise<string> {
  const marker = `EXIT=${String(code)}`;
  const found = (): string | null => {
    const hit = readSpools(dirs, vendorSessionId).find((spool) => spool.text.includes(marker));
    return hit === undefined ? null : hit.taskId;
  };
  const already = found();
  if (already !== null) return already;
  const dir = spoolDir(dirs, vendorSessionId);
  await awaitFile(dir);
  return new Promise<string>((resolve) => {
    let settled = false;
    const finish = (): void => {
      if (settled) return;
      const hit = found();
      if (hit === null) return;
      settled = true;
      redrain.stop();
      watcher.close();
      resolve(hit);
    };
    const redrain = new ReDrain(finish);
    // The directory event covers a spool being CREATED; the marker, though,
    // arrives as an APPEND to a spool that already exists, which a directory
    // watch on macOS need not report at all — hence the re-drain, which
    // re-reads the same level the missing edge would have announced.
    const watcher = watch(dir, finish);
    redrain.start();
    finish();
  });
}

/** Where one subagent's transcript lives, for existence assertions. */
export function subagentTranscriptPathFor(
  dirs: ShimDirectories,
  vendorSessionId: string,
  agentId: string,
): string {
  return subagentTranscriptPath(
    dirs.configDir,
    workspaceRealPath(dirs),
    vendorSessionId,
    agentId,
  );
}
