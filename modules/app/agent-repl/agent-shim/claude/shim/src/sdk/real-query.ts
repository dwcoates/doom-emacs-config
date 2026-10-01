/**
 * sdk/real-query.ts — THE real `query()` factory.
 *
 * One place assembles the options for a live vendor session, so a session, a
 * compaction pass, and any future probe cannot drift into asking the vendor for
 * different postures. Everything here is load-bearing (docs/overhaul/shim.md,
 * "SYSTEM PROMPT AND SETTINGS"):
 *
 *   - the `claude_code` SYSTEM-PROMPT PRESET carries the environment block
 *     (cwd, platform, home). Without it the model cannot resolve `~` and
 *     invents paths like /Users/user/... for tilde-phrased instructions. The
 *     harness metaprompt rides it as an `append`, which is how the guidelines
 *     survive /clear, /compact and resume: the SDK re-sends the system prompt
 *     on every request, so nothing has to be re-armed.
 *   - `settingSources: user + project + local` loads the user's permission
 *     allowlists, hooks and CLAUDE.md. It is ALSO what makes the vendor emit
 *     `denied.by_policy`, which the permission gate relies on — without the
 *     setting sources those emissions simply do not exist.
 *   - `includePartialMessages` is the stream-event source the fold's
 *     thinking/response units are built from.
 *   - `forwardSubagentText` is what makes a spawned agent's prose arrive at
 *     all; without it a subagent is a black box between spawn and result.
 *   - `perTaskStopAffordance` is what keeps an interrupt to the turn alone;
 *     without it the CLI kills every background task on an interrupt.
 *
 * THE BINARY IS THE SDK'S OWN. `pathToClaudeCodeExecutable` is deliberately
 * NOT set (ruling R12): the shim drives the SDK's bundled, pinned agent binary
 * — the version the upgrade canary guards — and no system-binary override
 * exists. A user's independently-upgraded `claude` is not this session's
 * engine.
 */
import { spawn } from "node:child_process";
import type { Options, SpawnedProcess, SpawnOptions } from "@anthropic-ai/claude-agent-sdk";
import { bindLog } from "../log.js";
import { systemPromptOption } from "../metaprompt.js";
import { importRealSDK } from "../vendor-guard.js";
import type {
  CanUseToolLike,
  PermissionModeLike,
  QueryLike,
  SdkUserMessage,
} from "./types.js";

const LOGGER = bindLog({ component: "shim-real-query", operation: "shim.sdk.real-query" });

/**
 * THE SHIM RENDERS A PER-TASK STOP, SO AN INTERRUPT ENDS THE TURN AND NOTHING ELSE.
 *
 * `Options.perTaskStopAffordance` tells the CLI this consumer drives
 * `stop_task` for each background task (the footer's per-task stop). Declared,
 * an interrupt on an OPEN-INPUT session — the streaming `AsyncIterable` prompt
 * every query here is built with — spares running background agents and
 * workflows, and Stop aborts only the turn. ABSENT, the CLI fails closed and
 * the interrupt kills every background task: on 2026-09-23 at 13:23:48,
 * 14:37:37 and 14:49:23 an ordinary interjection stopped every live subagent
 * in the session because the shim never declared it.
 *
 * Exported so the mocked vendor is handed the SAME declaration: `--fake` models
 * the CLI's interrupt both ways, and the posture it runs under is this one.
 */
export const PER_TASK_STOP_AFFORDANCE = true;

/**
 * How the session binds to a vendor conversation. A oneof, because the two are
 * mutually exclusive at the SDK and confusing them is how a fresh session
 * silently adopts someone else's transcript:
 *
 *   - `fresh` PRE-MINTS the vendor session id (`Options.sessionId`) so the
 *     shim knows the conversation's identity BEFORE the first message arrives.
 *     That pre-mint is what lets the main agent's `AgentId` be the original
 *     vendor session id (R9) and be persisted before anything can rotate it.
 *   - `resume` continues an existing transcript by its vendor id.
 */
export type VendorBinding =
  | { readonly kind: "fresh"; readonly sessionId: string }
  | { readonly kind: "resume"; readonly resumeSessionId: string };

/** Everything a real query needs that is not fixed policy. */
export interface RealQuerySpec {
  /** The workspace directory. The session's cwd; never derived from process.cwd() here. */
  readonly cwd: string;
  /** The account root the vendor binary must read, passed through in `env`. */
  readonly claudeConfigDir: string;
  /** Fresh (pre-minted id) or resume (existing vendor id). */
  readonly binding: VendorBinding;
  /**
   * Resume only THROUGH this record, discarding everything after it.
   *
   * The keep-alive yield obligation's mechanism: a real prompt must not build
   * on keep-alive context, and `resumeSessionAt` is the ONE declared surface
   * that truncates a conversation without rewriting the vendor's own file.
   * Meaningless without a `resume` binding, which is why it lives beside it.
   */
  readonly resumeSessionAt?: string;
  /**
   * With `resumeSessionAt`: the prompt uuid of the ONE turn the truncating
   * resume discards, which arms the vendor's guard — it refuses the resume when
   * anything past the cut is not that turn's (sdk.d.ts, `resumeDropsTurn`).
   * RollBackSession sets it only for a single dropped turn.
   */
  readonly resumeDropsTurn?: string;
  /** The model to answer with, or absence for the account default. */
  readonly model?: string;
  /** The permission mode every gate starts under. */
  readonly permissionMode: PermissionModeLike;
  /** The permission gate. Every tool call the vendor gates arrives here. */
  readonly canUseTool: CanUseToolLike;
  /** The query's ONLY lifecycle capability; aborting it ends the CLI child. */
  readonly abortController: AbortController;
  /** Resolve the metaprompt against this home instead of the real one (tests). */
  readonly home?: string;
  /**
   * Every chunk the vendor child writes to stderr.
   *
   * THE ONLY CHANNEL THE CLI HAS FOR ITS OWN REFUSALS. A resume the binary will
   * not honour is printed there and then followed by silence on the message
   * stream, so a shim that does not read it can report the silence and never
   * the reason. Absent means the caller does not want it, not that the child
   * has none.
   */
  readonly onStderr?: (data: string) => void;
  /**
   * How the vendor child ENDED: its exit code, or the signal that killed it.
   *
   * THE FACT THAT IS OTHERWISE UNRECORDED. When the query dies the shim has
   * the SDK's own wording for it and nothing else — and the SDK's wording is
   * about the stream, not about the process. A vendor that died on 2026-09-14
   * left `getContextUsage failed`, then `ProcessTransport is not ready for
   * writing`, then "the vendor query is gone", and the immediate cause was
   * simply not on the record: no exit code, no signal, no stderr beside it.
   *
   * Absence means the caller does not want it, and then nothing about the
   * spawn changes.
   */
  readonly onChildExit?: (exit: VendorChildExit) => void;
}

/** How the vendor child ended. */
export interface VendorChildExit {
  /** The exit code, or absence when a signal ended it. */
  readonly code: number | null;
  /** The signal that ended it, or absence when it exited on its own. */
  readonly signal: string | null;
}

/** How a child is actually spawned. Injected so a suite spawns nothing. */
export type SpawnChild = (options: SpawnOptions) => SpawnedProcess;

/**
 * Spawn the vendor child exactly as the SDK would, and WATCH IT END.
 *
 * WHY A CUSTOM SPAWNER AT ALL. `Options.spawnClaudeCodeProcess` is the only
 * declared surface that puts the child itself in the shim's hands, and the
 * child's exit code and signal live nowhere else: the SDK reports the stream's
 * death, not the process's.
 *
 * IT WIRES STDERR TOO, AND THAT IS NOT OPTIONAL. `SpawnedProcess` declares
 * stdin and stdout and says nothing about stderr, so a custom spawner is where
 * the `Options.stderr` callback stops being fed. Feeding it here is what keeps
 * the vendor's own words — "No conversation found with session ID ..." and
 * every other refusal it prints — on the record; a spawner that forgot this
 * would trade a death's exit code for every start's reason.
 *
 * WHAT THE SDK'S OWN SPAWN DOES THAT THIS DOES NOT, and why each is covered:
 *   - it checks the executable exists before spawning. Without the check the
 *     failure arrives as an `error` event carrying ENOENT, which the SDK
 *     already listens for and turns into the same refusal.
 *   - it delays `exit` until the child's stderr has also closed, so its own
 *     exit errors quote a complete stderr tail. The shim keeps its own bounded
 *     stderr ring for the whole life of the child, so the record this exists
 *     to write quotes that ring rather than the SDK's message.
 */
export function vendorSpawner(
  onChildExit: (exit: VendorChildExit) => void,
  onStderr?: (data: string) => void,
  spawnChild: SpawnChild = defaultSpawn,
): SpawnChild {
  return (options: SpawnOptions): SpawnedProcess => {
    const child = spawnChild(options);
    child.once("exit", (code: number | null, signal: NodeJS.Signals | null) => {
      onChildExit({ code, signal });
    });
    const errors = (child as { stderr?: NodeJS.ReadableStream | null }).stderr;
    if (onStderr !== undefined && errors != null) {
      errors.setEncoding("utf8");
      errors.on("data", (chunk: string | Buffer) => onStderr(chunk.toString()));
    }
    return child;
  };
}

/** The plain spawn the SDK would have performed. */
function defaultSpawn(options: SpawnOptions): SpawnedProcess {
  return spawn(options.command, options.args, {
    ...(options.cwd === undefined ? {} : { cwd: options.cwd }),
    env: options.env,
    signal: options.signal,
    stdio: ["pipe", "pipe", "pipe"],
  });
}

/**
 * The SDK options for one real session.
 *
 * Exported separately from {@link createRealQuery} because the factory itself
 * needs the live SDK and therefore cannot run in a unit test, while the options
 * are pure and carry every load-bearing decision above — so they are what the
 * suite asserts.
 */
export function realQueryOptions(spec: RealQuerySpec): Options {
  const options: Options = {
    systemPrompt: systemPromptOption(spec.home),
    settingSources: ["user", "project", "local"],
    includePartialMessages: true,
    forwardSubagentText: true,
    // REGRESSION (2026-09-23): an interrupt killed every background subagent,
    // because this was never declared. See PER_TASK_STOP_AFFORDANCE.
    perTaskStopAffordance: PER_TASK_STOP_AFFORDANCE,
    // EXPOSE READABLE SUMMARIZED THINKING. Without a `thinking` option current
    // models default `display` to "omitted": reasoning still happens but the
    // text is withheld (empty thinking deltas), so no thinking bubble ever
    // renders. `display: "summarized"` makes the vendor stream readable
    // `thinking_delta` text, which convert/stream-events.ts already folds into
    // thinking units. `type: "adaptive"` is the form current models
    // (Claude 4.7+/5 family) accept — `"enabled"` is rejected by them.
    thinking: { type: "adaptive", display: "summarized" },
    // EVERY SESSION CHECKPOINTS ITS FILES. RollBackSession's `restore_files`
    // rewinds the files to a prompt (`Query.rewindFiles`), which only works for
    // a prompt sent while checkpointing was on, so it is on from the first one.
    enableFileCheckpointing: true,
    cwd: spec.cwd,
    permissionMode: spec.permissionMode,
    canUseTool: spec.canUseTool,
    abortController: spec.abortController,
    // The vendor binary reads its account root from the environment, and the
    // shim is spawned with an explicit one (a workspace may be bound to a
    // non-default account). Passing the whole environment through and then
    // stating CLAUDE_CONFIG_DIR keeps PATH/HOME/proxy settings intact while
    // making the account root authoritative rather than inherited by luck.
    env: { ...process.env, CLAUDE_CONFIG_DIR: spec.claudeConfigDir },
    ...(spec.model === undefined ? {} : { model: spec.model }),
    // THE STDERR CALLBACK OR THE SPAWNER, NEVER BOTH. A custom spawner is
    // where `Options.stderr` stops being fed (`SpawnedProcess` declares no
    // stderr), so the spawner takes the callback over when there is one and
    // the two can never double-report the same chunk.
    ...(spec.onChildExit === undefined
      ? { ...(spec.onStderr === undefined ? {} : { stderr: spec.onStderr }) }
      : { spawnClaudeCodeProcess: vendorSpawner(spec.onChildExit, spec.onStderr) }),
    ...(spec.binding.kind === "fresh"
      ? { sessionId: spec.binding.sessionId }
      : {
          resume: spec.binding.resumeSessionId,
          ...(spec.resumeSessionAt === undefined ? {} : { resumeSessionAt: spec.resumeSessionAt }),
          ...(spec.resumeDropsTurn === undefined ? {} : { resumeDropsTurn: spec.resumeDropsTurn }),
        }),
  };
  return options;
}

/**
 * Construct the one live query a session owns.
 *
 * Routes through {@link importRealSDK}, the single vendor chokepoint: under
 * AGENT_REPL_FORBID_VENDOR_CALLS this throws rather than reaching the vendor,
 * and the throw is the caller's to surface.
 */
export async function createRealQuery(
  spec: RealQuerySpec,
  prompt: AsyncIterable<SdkUserMessage>,
): Promise<QueryLike> {
  LOGGER.debug(
    {
      workspace_dir: spec.cwd,
      binding: spec.binding.kind,
      vendor_session_id:
        spec.binding.kind === "fresh" ? spec.binding.sessionId : spec.binding.resumeSessionId,
      model: spec.model ?? "",
      permission_mode: spec.permissionMode,
    },
    "constructing the real vendor query",
  );
  const sdk = await importRealSDK("createRealQuery");
  return sdk.query({ prompt, options: realQueryOptions(spec) });
}
