/**
 * fake/vendor-files.ts — the mocked vendor's ON-DISK half.
 *
 * # Why the mock writes files at all
 *
 * The shim is only one of two producers. The other is the SIDECAR, which reads
 * the vendor's own files: the session transcript, each subagent's transcript
 * plus its `.meta.json`, and the detached-work spools. Nothing in the SDK
 * stream carries a subagent's model, a workflow agent's worktree, or a single
 * byte of a detached shell's output — those exist only on disk.
 *
 * So a mocked vendor that emitted messages and wrote nothing would leave HALF
 * the system untestable: every sidecar path, every store row the sidecar
 * produces, every page the two planes merge. `--fake` runs the REAL sidecar
 * against these files, which means they must be written exactly where the real
 * binary writes them, in the real line shapes. Every shape here is copied from
 * `testdata/corpus`; where the corpus is silent the field is OMITTED rather
 * than invented (see `docs/overhaul/shim.md`'s mock section for the list).
 *
 * # The three trees
 *
 * ```
 * $CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>.jsonl
 * $CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>/subagents/agent-<agent-id>.jsonl
 * $CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>/subagents/agent-<agent-id>.meta.json
 * <spool-root>/<cwd-slug>/<vendor-session-id>/tasks/<task-id>.output
 * ```
 *
 * # The parentUuid chain is the transcript's only ordering
 *
 * Every record but the first names its predecessor in `parentUuid`. A reader
 * walks that chain, so a writer that forgets it produces a transcript that
 * loads as a single orphan record. `TranscriptWriter` therefore owns the chain
 * head and stamps it — callers never pass `parentUuid`.
 *
 * On RESUME the chain must continue from what a previous run wrote, so the
 * writer reads the file's last record and adopts its uuid as the head. On
 * ROTATION (`/clear`) the identity changes: a new file, a new chain, and the
 * old file left exactly as it was.
 */
import { appendFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";

import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-fake", operation: "shim.fake.vendor-files" });

/**
 * The `version` every record reports.
 *
 * A single pinned string, not the shim's own version: the field states which
 * CLI wrote the record, and the mock is impersonating one. `2.1.215` is the
 * newest version present in `testdata/corpus`, so a consumer written against
 * the corpus sees the version it was calibrated on.
 */
export const FAKE_CLI_VERSION = "2.1.215";

/**
 * A thinking signature shaped like the corpus's (opaque, truncated there too).
 *
 * Lives here rather than beside the scenarios because the mock's own emitters
 * need it: every tool call and every turn conclusion carries a reasoning block,
 * and the signature is what makes a WITHHELD one legible as withheld.
 */
export const FAKE_REASONING_SIGNATURE =
  "EqICCokBCBAYAipA7QezsC7A4qgwYLQJ7i3E1wpsSpekzx1YfakeSignature==";

/**
 * The `entrypoint` every record reports. The shim always drives the SDK, and
 * the SDK's CLI stamps `sdk-cli` (corpus: every `sdk-cli` line was produced by
 * a shim-driven session; `cli` lines came from interactive terminals).
 */
const FAKE_ENTRYPOINT = "sdk-cli";

/** The `userType` every record reports. The corpus has exactly one value. */
const FAKE_USER_TYPE = "external";

/**
 * Slugify an absolute directory the way the vendor names its project folder.
 *
 * EVERY byte that is not `[A-Za-z0-9]` becomes `-`; case is preserved and
 * existing dashes are untouched. This is stronger than "replace `/` and `.`":
 * an underscore is replaced too, which matters because macOS temp directories
 * (`/private/var/folders/_m/...`) are full of them and a test that guessed the
 * weaker rule would look for a directory the vendor never created.
 *
 * Verified against the live `~/.claude/projects` tree:
 * - `/Users/x/.config/y`      → `-Users-x--config-y`
 * - `/private/var/folders/_m/x` → `-private-var-folders--m-x`
 */
export function cwdSlug(absoluteDir: string): string {
  return absoluteDir.replace(/[^A-Za-z0-9]/g, "-");
}

/** Where one vendor session's transcript file lives. */
export function transcriptPath(configDir: string, cwd: string, sessionId: string): string {
  return join(configDir, "projects", cwdSlug(cwd), `${sessionId}.jsonl`);
}

/** Where one subagent's transcript lives, beside its sibling `.meta.json`. */
export function subagentTranscriptPath(
  configDir: string,
  cwd: string,
  sessionId: string,
  agentId: string,
): string {
  return join(configDir, "projects", cwdSlug(cwd), sessionId, "subagents", `agent-${agentId}.jsonl`);
}

/** Where one subagent's metadata sidecar lives. */
export function subagentMetaPath(
  configDir: string,
  cwd: string,
  sessionId: string,
  agentId: string,
): string {
  return join(configDir, "projects", cwdSlug(cwd), sessionId, "subagents", `agent-${agentId}.meta.json`);
}

/** Where one detached task's output spool lives. */
export function spoolPath(
  spoolRoot: string,
  cwd: string,
  sessionId: string,
  taskId: string,
): string {
  return join(spoolRoot, cwdSlug(cwd), sessionId, "tasks", `${taskId}.output`);
}

/** A JSON object destined for a transcript line. */
export type TranscriptRecord = Record<string, unknown>;

/**
 * What every session-transcript record carries beyond its own payload.
 *
 * Assembled once per writer rather than per record because these five values
 * are session facts, and a record that disagreed with its siblings about the
 * cwd or the branch would be a shape no real transcript contains.
 */
interface TranscriptEnvelope {
  readonly cwd: string;
  readonly sessionId: string;
  readonly gitBranch: string;
}

function ensureDir(filePath: string): void {
  mkdirSync(dirname(filePath), { recursive: true });
}

/**
 * Read the uuid of the last record in an existing transcript, or null.
 *
 * Only records that PARTICIPATE in the chain count. `ai-title`, `mode`,
 * `permission-mode`, `last-prompt`, `queue-operation`, `pr-link`, `frame-link`
 * and the file-history pair carry no `uuid` at all (corpus), so they are
 * skipped: adopting a chainless record as the head would make the next record
 * name a parent no reader can resolve.
 */
export function lastChainedUuid(path: string): string | null {
  if (!existsSync(path)) return null;
  const lines = readFileSync(path, "utf8").split("\n");
  for (let i = lines.length - 1; i >= 0; i--) {
    const line = lines[i]?.trim();
    if (line === undefined || line === "") continue;
    let parsed: unknown;
    try {
      parsed = JSON.parse(line);
    } catch {
      // A half-written trailing line is what a killed CLI leaves behind; the
      // vendor's own reader skips it, so skipping is faithful, not lenient.
      continue;
    }
    const uuid = (parsed as { uuid?: unknown }).uuid;
    if (typeof uuid === "string" && uuid !== "") return uuid;
  }
  return null;
}

/**
 * The session transcript writer: one per vendor session identity.
 *
 * It owns the parentUuid chain head and the shared envelope. A rotation makes a
 * NEW writer (see {@link VendorFiles.rotate}) rather than mutating this one,
 * because the old identity's file must stay exactly as it was — a resume of the
 * pre-rotation id has to still load.
 */
export class TranscriptWriter {
  private head: string | null;

  constructor(
    readonly path: string,
    private readonly envelope: TranscriptEnvelope,
  ) {
    ensureDir(path);
    this.head = lastChainedUuid(path);
    LOGGER.debug(
      { transcript_path: path, resumed_chain_head: this.head ?? "", claude_session_id: envelope.sessionId },
      this.head === null ? "opened a FRESH vendor transcript" : "REOPENED a vendor transcript and adopted its chain head",
    );
  }

  /** The uuid the next record will name as its parent. */
  get chainHead(): string | null {
    return this.head;
  }

  /**
   * Append one chained record.
   *
   * `uuid` and `timestamp` come from the caller (they are the SDK message's own
   * values — the same record exists on both planes and must agree), everything
   * else is stamped here.
   */
  append(record: TranscriptRecord & { uuid: string; timestamp: string }): void {
    const line: TranscriptRecord = {
      parentUuid: this.head,
      isSidechain: false,
      ...record,
      userType: FAKE_USER_TYPE,
      entrypoint: FAKE_ENTRYPOINT,
      cwd: this.envelope.cwd,
      sessionId: this.envelope.sessionId,
      version: FAKE_CLI_VERSION,
      gitBranch: this.envelope.gitBranch,
    };
    appendFileSync(this.path, `${JSON.stringify(line)}\n`);
    this.head = record.uuid;
  }

  /**
   * Append one UNCHAINED record — the sidebar-metadata line family.
   *
   * `ai-title`, `mode`, `permission-mode`, `last-prompt`, `queue-operation`,
   * `pr-link` and `frame-link` carry no uuid and no parentUuid in the corpus,
   * and they do not advance the chain. Writing them through `append` would
   * both invent fields and break the next record's parent.
   */
  appendUnchained(record: TranscriptRecord): void {
    appendFileSync(this.path, `${JSON.stringify({ ...record, sessionId: this.envelope.sessionId })}\n`);
  }
}

/**
 * One subagent's transcript plus its metadata sidecar.
 *
 * The metadata is a SEPARATE file because that is where the vendor puts the
 * only copy of the agent's type, description, spawning tool call and spawn
 * depth — a consumer that reads transcripts alone cannot learn them (shim.md,
 * "Gotchas"). Its field names are the corpus's: camelCase, four keys, no
 * `model` (the corpus sidecar carries none, so the mock writes none).
 */
export class SubagentWriter {
  private head: string | null = null;

  constructor(
    readonly path: string,
    readonly metaPath: string,
    private readonly envelope: TranscriptEnvelope,
    private readonly agentId: string,
  ) {
    ensureDir(path);
  }

  /** Write the metadata sidecar. Called once, at announcement. */
  writeMeta(meta: {
    agentType: string;
    description: string;
    toolUseId: string;
    spawnDepth: number;
  }): void {
    ensureDir(this.metaPath);
    writeFileSync(this.metaPath, `${JSON.stringify(meta, null, 2)}\n`);
    LOGGER.debug(
      { subagent_meta_path: this.metaPath, agent_id: this.agentId, agent_type: meta.agentType },
      "wrote a subagent metadata sidecar",
    );
  }

  /** Append one chained sidechain record. */
  append(record: TranscriptRecord & { uuid: string; timestamp: string }): void {
    const line: TranscriptRecord = {
      parentUuid: this.head,
      isSidechain: true,
      agentId: this.agentId,
      ...record,
      userType: FAKE_USER_TYPE,
      entrypoint: FAKE_ENTRYPOINT,
      cwd: this.envelope.cwd,
      sessionId: this.envelope.sessionId,
      version: FAKE_CLI_VERSION,
      gitBranch: this.envelope.gitBranch,
    };
    appendFileSync(this.path, `${JSON.stringify(line)}\n`);
    this.head = record.uuid;
  }

  /** The whole transcript as one string — what an agent spool duplicates. */
  read(): string {
    return existsSync(this.path) ? readFileSync(this.path, "utf8") : "";
  }
}

/**
 * One detached task's output spool.
 *
 * INCREMENTAL BY CONSTRUCTION. The sidecar TAILS this file while the work runs,
 * so a spool written whole at the end would leave the tailer's entire delta
 * path — the only route detached shell output ever takes — unexercised. Every
 * `append` is a separate `appendFileSync`, which is what a tail observes as a
 * growth event.
 *
 * `finish` writes the `EXIT=<code>` terminator that tells the tailer the run
 * ended and with what status (corpus: `spools/bash-clean.output`). A spool with
 * no terminator is a run still going, or a killed one — `spools/
 * bash-midoutput.output` is the corpus sample of that, and the mock reproduces
 * it by simply never calling `finish`.
 */
export class SpoolWriter {
  private finished = false;

  constructor(readonly path: string) {
    ensureDir(path);
    writeFileSync(path, "");
  }

  /** Append a chunk exactly as the command produced it. */
  append(chunk: string): void {
    if (this.finished) {
      throw new Error(`fake spool ${this.path} was appended to after its EXIT line`);
    }
    appendFileSync(this.path, chunk);
  }

  /** Append one whole line. */
  appendLine(line: string): void {
    this.append(`${line}\n`);
  }

  /** Terminate the spool with the vendor's `EXIT=<code>` line. */
  finish(exitCode: number): void {
    if (this.finished) {
      throw new Error(`fake spool ${this.path} was finished twice`);
    }
    this.finished = true;
    appendFileSync(this.path, `EXIT=${exitCode}\n`);
    LOGGER.debug({ spool_path: this.path, exit_code: exitCode }, "terminated a fake task spool");
  }

  /** Whether the `EXIT=` terminator has been written. */
  get isFinished(): boolean {
    return this.finished;
  }
}

/** How a {@link VendorFiles} tree is rooted. */
interface VendorFilesConfig {
  /** `CLAUDE_CONFIG_DIR` — the account root the transcripts hang under. */
  readonly configDir: string;
  /** The workspace directory; the slug's source and every record's `cwd`. */
  readonly cwd: string;
  /** `$AGENT_REPL_FAKE_SPOOL_ROOT` or `/tmp/claude-<uid>`. */
  readonly spoolRoot: string;
  /** The vendor session id in force; the transcript's name and `sessionId`. */
  readonly sessionId: string;
  /** The branch every record reports. */
  readonly gitBranch: string;
}

/**
 * THE on-disk writer a scenario is handed.
 *
 * One instance spans a whole fake session, including rotations: `rotate`
 * swaps the transcript for a new identity's while leaving the subagent and
 * spool trees addressed by their own session ids, exactly as the vendor does.
 */
export class VendorFiles {
  private transcriptWriter: TranscriptWriter;
  private readonly subagents = new Map<string, SubagentWriter>();
  private readonly spools = new Map<string, SpoolWriter>();
  private sessionId: string;

  constructor(private readonly config: VendorFilesConfig) {
    this.sessionId = config.sessionId;
    this.transcriptWriter = this.openTranscript(config.sessionId);
  }

  private openTranscript(sessionId: string): TranscriptWriter {
    return new TranscriptWriter(transcriptPath(this.config.configDir, this.config.cwd, sessionId), {
      cwd: this.config.cwd,
      sessionId,
      gitBranch: this.config.gitBranch,
    });
  }

  /** The session transcript for the identity currently in force. */
  get transcript(): TranscriptWriter {
    return this.transcriptWriter;
  }

  /** The vendor session id the files are currently addressed by. */
  get vendorSessionId(): string {
    return this.sessionId;
  }

  /**
   * Retire the current identity and start a new transcript under a new id.
   *
   * The old file is CLOSED, not truncated or moved. A `/clear` does not destroy
   * the conversation it retired — resuming the old id still works — and a mock
   * that deleted it would make that untestable.
   */
  rotate(newSessionId: string): void {
    LOGGER.debug(
      { previous_claude_session_id: this.sessionId, claude_session_id: newSessionId },
      "ROTATED the fake vendor session identity; a new transcript file begins",
    );
    this.sessionId = newSessionId;
    this.transcriptWriter = this.openTranscript(newSessionId);
    this.subagents.clear();
  }

  /** The writer for one subagent, created on first use. */
  subagent(agentId: string): SubagentWriter {
    const existing = this.subagents.get(agentId);
    if (existing !== undefined) return existing;
    const writer = new SubagentWriter(
      subagentTranscriptPath(this.config.configDir, this.config.cwd, this.sessionId, agentId),
      subagentMetaPath(this.config.configDir, this.config.cwd, this.sessionId, agentId),
      { cwd: this.config.cwd, sessionId: this.sessionId, gitBranch: this.config.gitBranch },
      agentId,
    );
    this.subagents.set(agentId, writer);
    return writer;
  }

  /** The spool for one detached task, created on first use. */
  spool(taskId: string): SpoolWriter {
    const existing = this.spools.get(taskId);
    if (existing !== undefined) return existing;
    const writer = new SpoolWriter(
      spoolPath(this.config.spoolRoot, this.config.cwd, this.sessionId, taskId),
    );
    this.spools.set(taskId, writer);
    return writer;
  }

  /** The absolute path the vendor reports for a task's spool. */
  spoolPathFor(taskId: string): string {
    return spoolPath(this.config.spoolRoot, this.config.cwd, this.sessionId, taskId);
  }

  /** Every spool this session opened and has not terminated. */
  unfinishedSpools(): string[] {
    return [...this.spools.entries()].filter(([, s]) => !s.isFinished).map(([id]) => id);
  }
}

