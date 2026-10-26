/**
 * sdk/agent-repl-hooks.ts — the vendor's agent-to-agent tools, told about
 * agent-repl's WORKSPACES.
 *
 * The vendor's `ListAgents` lists the Claude sessions it can reach right now:
 * this session's subagents, teammates, and the other Claude processes running
 * on the machine. An agent-repl workspace is none of those as such. A workspace
 * that is open but idle or cold has no running vendor process, so it is simply
 * absent, and a workspace whose agent IS running shows only as an anonymous
 * peer session. Its own `SendMessage` then answers a workspace's name with "No
 * peer session named …". An agent asking "which agents can I talk to?" was
 * told, wrongly and silently, that an open workspace does not exist
 * (torch-human-support-ceac, 2026-10-26).
 *
 * A workspace is reached only through agent-repld (`workspace prompt`), which
 * holds the prompt behind the recipient's running work, waits out its cold
 * gate, and draws the send in both feeds. So these hooks make the vendor's own
 * tools say so, wherever an agent reaches for them:
 *
 *   - every `ListAgents` answer carries, as context beside its list, that
 *     agent-repl workspaces are not in it and how to list and message them
 *     ({@link LIST_AGENTS_WORKSPACES_NOTE}). The omission can no longer be
 *     silent, and the list of workspaces stays agent-repld's alone, never a
 *     second copy here that could go stale;
 *   - a `SendMessage` to an `agent-repl:<name>` address (the address a
 *     workspace's prompt arrives from) is refused before it runs, naming the
 *     `workspace prompt` call that reaches it. No vendor session answers to
 *     that address, so the send could only fail or go around agent-repld;
 *   - a `SendMessage` that fails says that its recipient may be a workspace,
 *     reachable only through `workspace prompt`.
 *
 * Every session of the agent profile carries these hooks (sdk/real-query.ts
 * profileOptions); a chat session has no tools to hook.
 */
import type { HookCallback, HookCallbackMatcher, HookEvent, HookInput, HookJSONOutput } from "@anthropic-ai/claude-agent-sdk";
import { bindLog } from "../log.js";
import { AGENT_REPL_PEER_PREFIX } from "../convert/peer.js";

const LOGGER = bindLog({ component: "shim-agent-repl-hooks", operation: "shim.sdk.agent_repl_hooks" });

/** The vendor's agent-listing tool. */
export const LIST_AGENTS_TOOL = "ListAgents";

/** The vendor's agent-messaging tool. */
export const SEND_MESSAGE_TOOL = "SendMessage";

/** The agent API's front door, as the /agent-repl-workspaces skill names it. */
const AGENT_REPLD_COMMAND = "~/.claude/skills/agent-repl-workspaces/agent-repld";

/** What every `ListAgents` answer is told about agent-repl's workspaces. */
export const LIST_AGENTS_WORKSPACES_NOTE =
  "agent-repl: this list never includes agent-repl workspaces. A workspace that is open but idle " +
  "or cold, in any repository, is missing from it, and a peer session listed here may itself be " +
  "a workspace's agent. Never message a workspace with SendMessage. List the open workspaces " +
  `with \`${AGENT_REPLD_COMMAND} workspace list\` (one with \`workspace status NAME\`, closed ones ` +
  `with \`workspace find\`), and message one with \`${AGENT_REPLD_COMMAND} workspace prompt NAME TEXT\` ` +
  "(the /agent-repl-workspaces skill).";

/** Why a `SendMessage` to the workspace NAME's `agent-repl:` address is refused. */
export function workspaceAddressRefusal(name: string): string {
  return (
    `agent-repl: '${AGENT_REPL_PEER_PREFIX}${name}' is the agent-repl workspace ${name}, which ` +
    "SendMessage cannot reach. Message it with " +
    `\`${AGENT_REPLD_COMMAND} workspace prompt ${name} TEXT\`, which holds the prompt behind its ` +
    "running work, wakes it if it is cold, and shows the send in both workspaces' feeds."
  );
}

/** What a failed `SendMessage` to RECIPIENT is told about workspaces. */
export function sendMessageFailureNote(recipient: string): string {
  return (
    `agent-repl: if '${recipient}' is an agent-repl workspace, SendMessage cannot reach it, and an ` +
    "idle or cold workspace is never among the peer sessions. Look it up with " +
    `\`${AGENT_REPLD_COMMAND} workspace status ${recipient}\` (or \`workspace list\`) and message ` +
    `it with \`${AGENT_REPLD_COMMAND} workspace prompt ${recipient} TEXT\`.`
  );
}

/**
 * The `to` of a `SendMessage` call's INPUT.
 *
 * The hook is matched to `SendMessage` alone, and the vendor validates the
 * call's input against its own schema before any hook sees it, so an input
 * with no string `to` is a vendor contract change, thrown rather than guessed
 * around.
 */
export function sendMessageRecipient(input: unknown): string {
  const to = typeof input === "object" && input !== null ? (input as Record<string, unknown>).to : undefined;
  if (typeof to !== "string") {
    LOGGER.error(
      { cause: "the SendMessage input carries no string `to`", input_type: typeof input },
      "a SendMessage call reached its hook without a recipient",
    );
    throw new Error("shim agent-repl hooks: a SendMessage input carries no string `to`");
  }
  return to;
}

/** INPUT, held to the one event its hook is registered for. */
function expectEvent<E extends HookInput["hook_event_name"]>(
  input: HookInput,
  event: E,
): Extract<HookInput, { hook_event_name: E }> {
  if (input.hook_event_name !== event) {
    LOGGER.error(
      { cause: `registered for ${event}, called for ${input.hook_event_name}`, expected: event, got: input.hook_event_name },
      "an agent-repl hook ran for an event it is not registered for",
    );
    throw new Error(`shim agent-repl hooks: a ${event} hook ran for ${input.hook_event_name}`);
  }
  return input as Extract<HookInput, { hook_event_name: E }>;
}

/** `ListAgents` answered: tell the agent about the workspaces the list leaves out. */
export const listAgentsWorkspacesHook: HookCallback = (input): Promise<HookJSONOutput> => {
  const post = expectEvent(input, "PostToolUse");
  LOGGER.debug({ tool_use_id: post.tool_use_id }, "a ListAgents answer was told about agent-repl workspaces");
  return Promise.resolve({
    hookSpecificOutput: { hookEventName: "PostToolUse", additionalContext: LIST_AGENTS_WORKSPACES_NOTE },
  });
};

/** `SendMessage` about to run: refuse it when it names a workspace's `agent-repl:` address. */
export const sendMessageWorkspaceAddressHook: HookCallback = (input): Promise<HookJSONOutput> => {
  const pre = expectEvent(input, "PreToolUse");
  const to = sendMessageRecipient(pre.tool_input);
  if (!to.startsWith(AGENT_REPL_PEER_PREFIX)) return Promise.resolve({});
  const name = to.slice(AGENT_REPL_PEER_PREFIX.length);
  LOGGER.info(
    { tool_use_id: pre.tool_use_id, recipient: to, workspace_name: name },
    "refused a SendMessage to an agent-repl workspace address, pointing the agent at workspace prompt",
  );
  return Promise.resolve({
    hookSpecificOutput: {
      hookEventName: "PreToolUse",
      permissionDecision: "deny",
      permissionDecisionReason: workspaceAddressRefusal(name),
    },
  });
};

/** `SendMessage` failed: say its recipient may be a workspace, and how a workspace is reached. */
export const sendMessageFailureHook: HookCallback = (input): Promise<HookJSONOutput> => {
  const failure = expectEvent(input, "PostToolUseFailure");
  const to = sendMessageRecipient(failure.tool_input);
  LOGGER.debug(
    { tool_use_id: failure.tool_use_id, recipient: to, error: failure.error },
    "a failed SendMessage was told how an agent-repl workspace is reached",
  );
  return Promise.resolve({
    hookSpecificOutput: { hookEventName: "PostToolUseFailure", additionalContext: sendMessageFailureNote(to) },
  });
};

/** The hooks every agent session carries, by event. */
export function agentReplHooks(): Partial<Record<HookEvent, HookCallbackMatcher[]>> {
  return {
    PreToolUse: [{ matcher: SEND_MESSAGE_TOOL, hooks: [sendMessageWorkspaceAddressHook] }],
    PostToolUse: [{ matcher: LIST_AGENTS_TOOL, hooks: [listAgentsWorkspacesHook] }],
    PostToolUseFailure: [{ matcher: SEND_MESSAGE_TOOL, hooks: [sendMessageFailureHook] }],
  };
}
