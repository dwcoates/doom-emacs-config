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
 *
 * THE BINARY IS THE SDK'S OWN. `pathToClaudeCodeExecutable` is deliberately
 * NOT set (ruling R12): the shim drives the SDK's bundled, pinned agent binary
 * — the version the upgrade canary guards — and no system-binary override
 * exists. A user's independently-upgraded `claude` is not this session's
 * engine.
 */
import type { Options } from "@anthropic-ai/claude-agent-sdk";
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
    ...(spec.binding.kind === "fresh"
      ? { sessionId: spec.binding.sessionId }
      : { resume: spec.binding.resumeSessionId }),
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
  LOGGER.log(
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
