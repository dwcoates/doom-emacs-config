/**
 * The refusal arms ONLY AN AGENT'S REQUEST can provoke, by rpc.
 *
 * The webapp asks every rpc as the user: a create is never an async merge (an
 * agent's request), and a prompt is never an agent's slash command. So these
 * arms can never answer a request the webapp sent, the call sites word none
 * of them, and one reaching a call site is a breach (`unreachableArm`).
 *
 * The suites that enumerate a refusal oneof off the schema (`oneofArms`) take
 * these out of the arms each call site must word, and assert the breach for
 * them instead, so an arm added to the proto still fails until it is placed on
 * one side or the other.
 */
export const AGENT_ONLY_REFUSAL_ARMS = {
  createWorkspace: new Set<string>([
    "asyncMergeUnknownBranch",
    "asyncMergeBranchOwned",
    "asyncMergeUnknownSubagent",
    "asyncMergeUnknownMaker",
    "asyncMergeContextNotMaker",
    "asyncMergeNameTaken",
    "asyncMergeUnknownCloseAfter",
    "opIdWithAsyncMerge",
    "versionIncrementRequired",
    "versionIncrementUnexpected",
    "asyncMergeRequesterQueued",
    "asyncMergeRequesterMerging",
    "asyncMergeRequesterSessionDeleted",
    "originFetchFailed",
  ]),
  submitPrompt: new Set<string>(["slashCommandFromAgent", "unknownSender"]),
} as const satisfies Record<string, ReadonlySet<string>>;

/** The rpcs that have agent-only refusal arms. */
export type AgentOnlyRpc = keyof typeof AGENT_ONLY_REFUSAL_ARMS;

/** ARMS without RPC's agent-only ones: what the webapp's call site must word. */
export function userRefusalArms(rpc: string, arms: readonly string[]): string[] {
  const agentOnly: ReadonlySet<string> | undefined =
    (AGENT_ONLY_REFUSAL_ARMS as Record<string, ReadonlySet<string>>)[rpc];
  return agentOnly === undefined ? [...arms] : arms.filter((arm) => !agentOnly.has(arm));
}
