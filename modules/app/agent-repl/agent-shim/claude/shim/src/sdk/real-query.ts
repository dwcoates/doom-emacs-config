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
 *   - `mcpServers` carries agent-repl's OWN MCP server (sdk/agent-repl-mcp.ts),
 *     whose tools agent-repl itself draws, and `allowedTools` pre-allows them:
 *     they change nothing on the machine, so a prompt would only stall the turn.
 *   - `hooks` makes the vendor's `ListAgents` and `SendMessage` say that
 *     agent-repl workspaces are reached through agent-repld, never through
 *     them (sdk/agent-repl-hooks.ts).
 *
 * THE BINARY IS THE SDK'S OWN. `pathToClaudeCodeExecutable` is deliberately
 * NOT set (ruling R12): the shim drives the SDK's bundled, pinned agent binary
 * — the version the upgrade canary guards — and no system-binary override
 * exists. A user's independently-upgraded `claude` is not this session's
 * engine.
 */
import { spawn } from "node:child_process";
import type { SessionProfile } from "../engine/profile.js";
import type { McpServerConfig, Options, SpawnedProcess, SpawnOptions } from "@anthropic-ai/claude-agent-sdk";
import { bindLog } from "../log.js";
import { systemPromptOption } from "../metaprompt.js";
import { importRealSDK } from "../vendor-guard.js";
import { agentReplHooks } from "./agent-repl-hooks.js";
import { AGENT_REPL_AUTO_ALLOWED_TOOLS, agentReplMcpServers } from "./agent-repl-mcp.js";
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
  | { readonly kind: "resume"; readonly resumeSessionId: string }
  /**
   * `ephemeral_fork` reads an existing conversation and writes NOTHING: the
   * vendor forks it to a new session id (`Options.forkSession`) that it never
   * persists (`Options.persistSession: false`). The two options travel as ONE
   * arm because each alone writes somewhere: a fork alone files a new
   * transcript, and an unpersisted resume alone is not a fork of anything. A
   * side query about the conversation (a response revision) binds this way, so
   * the session's own transcript is never touched.
   */
  | { readonly kind: "ephemeral_fork"; readonly parentSessionId: string };

/** The vendor session id a binding names: the one minted, resumed, or forked from. */
export function boundSessionId(binding: VendorBinding): string {
  switch (binding.kind) {
    case "fresh":
      return binding.sessionId;
    case "resume":
      return binding.resumeSessionId;
    case "ephemeral_fork":
      return binding.parentSessionId;
  }
}

/** Everything a real query needs that is not fixed policy. */
export interface RealQuerySpec {
  /** The kind of session, which decides settings, tools and the system prompt. */
  readonly profile: SessionProfile;
  /** The workspace directory. The session's cwd; never derived from process.cwd() here. */
  readonly cwd: string;
  /** The workspace's name, which the system prompt states (metaprompt.ts, workspaceSection). */
  readonly workspaceName: string;
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
  /**
   * The errno the SPAWN failed with (`ENOENT`, `EACCES`, ...), when the child
   * never ran at all. Read off the error's structured `code`, never its text.
   * Absent for a child that spawned.
   */
  readonly spawnErrno?: string;
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
    let spawnFailed = false;
    child.once("exit", (code: number | null, signal: NodeJS.Signals | null) => {
      // ONE END PER CHILD: a spawn that failed has already been reported.
      if (spawnFailed) return;
      onChildExit({ code, signal });
    });
    // A SPAWN THAT FAILED IS AN END TOO, and its errno says whether trying
    // again can help (a missing or unexecutable binary will not appear by
    // itself). Only a child with no pid failed to SPAWN: an `error` from a
    // child that ran (a failed kill, say) is not this, and is the SDK's.
    child.once("error", (err: Error) => {
      if ((child as { pid?: number }).pid !== undefined) return;
      spawnFailed = true;
      const code = (err as NodeJS.ErrnoException).code;
      onChildExit({ code: null, signal: null, spawnErrno: typeof code === "string" ? code : "" });
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
 * suite asserts. `mcpServers` is agent-repl's own servers, built from the live
 * SDK by the factory and passed in for the same reason.
 */
export function realQueryOptions(spec: RealQuerySpec, mcpServers: Record<string, McpServerConfig>): Options {
  const options: Options = {
    ...profileOptions(spec, mcpServers),
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
    //
    // THE VENDOR MAKES NO API RETRY OF ITS OWN (owner ruling, 2026-10-20):
    // agent-repl owns the cadence (engine/vendor-error-retry.ts), so a failed
    // call ends the turn at once and the shim retries it where the footer can
    // count down to it and the user can ask for it now.
    //
    // THE VENDOR STATES ITS SESSION STATE ON THE STREAM. Without
    // CLAUDE_CODE_EMIT_SESSION_STATE_EVENTS the SDK asks for the state for its
    // own use only (`sdk_host_only`) and swallows it; the shim needs it,
    // because the vendor's idle is the one fact that covers work the vendor
    // owes before any turn or task says so (engine/session.ts
    // `servedVendorBusy`, the stand-down guard).
    env: {
      ...process.env,
      CLAUDE_CONFIG_DIR: spec.claudeConfigDir,
      CLAUDE_CODE_MAX_RETRIES: "0",
      CLAUDE_CODE_EMIT_SESSION_STATE_EVENTS: "1",
    },
    ...(spec.model === undefined ? {} : { model: spec.model }),
    // THE STDERR CALLBACK OR THE SPAWNER, NEVER BOTH. A custom spawner is
    // where `Options.stderr` stops being fed (`SpawnedProcess` declares no
    // stderr), so the spawner takes the callback over when there is one and
    // the two can never double-report the same chunk.
    ...(spec.onChildExit === undefined
      ? { ...(spec.onStderr === undefined ? {} : { stderr: spec.onStderr }) }
      : { spawnClaudeCodeProcess: vendorSpawner(spec.onChildExit, spec.onStderr) }),
    ...bindingOptions(spec),
  };
  return options;
}

/** The SDK options a spec's binding (and its resume cut) becomes. */
export function bindingOptions(
  spec: Pick<RealQuerySpec, "binding" | "resumeSessionAt" | "resumeDropsTurn">,
): Pick<Options, "sessionId" | "resume" | "resumeSessionAt" | "resumeDropsTurn" | "forkSession" | "persistSession"> {
  const cut = {
    ...(spec.resumeSessionAt === undefined ? {} : { resumeSessionAt: spec.resumeSessionAt }),
    ...(spec.resumeDropsTurn === undefined ? {} : { resumeDropsTurn: spec.resumeDropsTurn }),
  };
  switch (spec.binding.kind) {
    case "fresh":
      return { sessionId: spec.binding.sessionId };
    case "resume":
      return { resume: spec.binding.resumeSessionId, ...cut };
    case "ephemeral_fork":
      return { resume: spec.binding.parentSessionId, forkSession: true, persistSession: false, ...cut };
  }
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
      vendor_session_id: boundSessionId(spec.binding),
      model: spec.model ?? "",
      permission_mode: spec.permissionMode,
    },
    "constructing the real vendor query",
  );
  const sdk = await importRealSDK("createRealQuery");
  return asQueryLike(sdk.query({ prompt, options: realQueryOptions(spec, agentReplMcpServers(sdk)) }));
}

/**
 * The members the vendor's query provides WITHOUT DECLARING them, each with
 * what the shim cannot do without it (see types.ts QueryLike).
 */
const UNDECLARED_MEMBERS = [
  { member: "getSettings", consequence: "the shim cannot read the applied effort" },
  { member: "cancelAsyncMessage", consequence: "the shim cannot take back a prompt waiting to join the running turn" },
] as const;

/**
 * The real query as the shim drives it, held to the members the vendor
 * provides without declaring (UNDECLARED_MEMBERS). A query lacking one is a
 * vendor change, refused loudly at construction rather than discovered as a
 * TypeError on first use.
 */
export function asQueryLike(query: object): QueryLike {
  for (const { member, consequence } of UNDECLARED_MEMBERS) {
    if (typeof (query as Record<string, unknown>)[member] !== "function") {
      LOGGER.error(
        { cause: `the vendor query's ${member} is not a function`, member },
        `the vendor query provides no ${member}(); ${consequence}`,
      );
      throw new Error(`the vendor SDK's query no longer provides ${member}()`);
    }
  }
  return query as QueryLike;
}

/**
 * The options a session's profile decides.
 *
 * AN AGENT SESSION is the workspace agent: the vendor's coding preset with the
 * metaprompt appended, agent-repl's own tools pre-allowed, agent-repl's hooks on
 * the vendor's agent-to-agent tools, and every settings source (which
 * `denied.by_policy` gate emissions need).
 *
 * A CHAT SESSION answers questions in a zettel thread and must inherit nothing
 * of the user's agent setup: no settings sources (so no skills, CLAUDE.md or
 * memory), no tools of any kind, no MCP servers, no hooks, and the daemon's system prompt
 * in place of the coding preset.
 */
export function profileOptions(
  spec: RealQuerySpec,
  mcpServers: Record<string, McpServerConfig>,
): Pick<Options, "systemPrompt" | "mcpServers" | "allowedTools" | "settingSources" | "tools" | "hooks"> {
  switch (spec.profile.kind) {
    case "agent":
      return {
        systemPrompt: systemPromptOption(spec.workspaceName, spec.home),
        mcpServers,
        allowedTools: [...AGENT_REPL_AUTO_ALLOWED_TOOLS],
        settingSources: ["user", "project", "local"],
        hooks: agentReplHooks(),
      };
    case "chat":
      return {
        systemPrompt: { type: "custom", prompt: spec.profile.systemPrompt },
        mcpServers: {},
        allowedTools: [],
        settingSources: [],
        tools: [],
        hooks: {},
      };
  }
}
