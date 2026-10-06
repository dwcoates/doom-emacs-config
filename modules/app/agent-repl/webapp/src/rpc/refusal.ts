/**
 * refusal — the ONE wording of the four refusal causes every per-workspace
 * agentrepl rpc can answer with.
 *
 * LANDING 4 TYPED EVERY `<Rpc>Error`. Each endpoint owns its own cause oneof
 * and its own arm messages, but four of those arms say the same thing on every
 * endpoint: the daemon does not know the workspace, the ref's directory
 * disagrees with the registry's, the workspace has been handed to a successor
 * daemon, or a joining daemon has not adopted it yet. Written per call site
 * those four would be a dozen tables drifting apart, so they are worded here
 * once and every site delegates to this function first.
 *
 * WHY IT ANSWERS `undefined` RATHER THAN A FALLBACK SENTENCE. The cross-cutting
 * four are the only arms this file can honestly speak for; a per-rpc arm knows
 * facts only its own call site can phrase (a blocked close, a merge already
 * queued, a path that escapes the workspace). `undefined` is therefore "not
 * mine — say your own thing", not "I do not know what this is": a site that
 * gets `undefined` is REQUIRED to have its own exhaustive switch, and an arm
 * neither knows is a malformed view at that site.
 *
 * TWO OF THE FOUR CARRY THE FACT THE READER ACTS ON — the registry's directory
 * and the successor daemon's address — so those are read off the arm's message
 * and named in the sentence. Reading them is strict: an arm that should carry a
 * string and does not is a MALFORMED VIEW, never a sentence with a hole in it.
 */
import { MalformedView } from "./malformed.js";

/** The arms this file speaks for, in the generated (lowerCamel) spelling. */
export const CROSS_CUTTING_REFUSAL_ARMS: readonly string[] = [
  "unknownWorkspace",
  "workspaceRefMismatch",
  "transferringAway",
  "notYetAdopted",
];

/**
 * SelectWorkspace's EXPECTED ANSWER, which is not a refusal: a daemon that is
 * standing down stamps the selection and starts no session for it, and the
 * daemon that serves next takes the selection over. Every SelectWorkspace call
 * site logs it at INFO with this sentence and draws nothing.
 */
export const SELECT_WORKSPACE_EXPECTED_ARMS: Readonly<Record<string, string>> = {
  standingDown: "the daemon is standing down; the daemon that serves next takes the selection",
};

/**
 * A set arm of some `<Rpc>Error`'s cause oneof, as narrowly as this file can
 * type it: every endpoint's cause is a different union, so the shared helper
 * takes the shape they all have and validates the payload it reads.
 */
export interface RefusalCause {
  case: string;
  value?: unknown;
}

/**
 * The sentence for one of the cross-cutting four, or `undefined` when the arm
 * is the endpoint's own and the call site must word it.
 *
 * `rpcName` never appears in the sentence — the control the refusal is drawn
 * beside already says which verb was clicked — but it names the message tree in
 * a `MalformedView` so a bad payload points at the endpoint that sent it.
 */
export function refusalSentence(rpcName: string, cause: RefusalCause): string | undefined {
  switch (cause.case) {
    case "unknownWorkspace":
      return "the daemon does not know this workspace";
    case "workspaceRefMismatch": {
      const dir = requireStringField(rpcName, cause, "registryDir");
      return `this workspace's directory disagrees with the registry's: ${dir}`;
    }
    case "transferringAway": {
      const address = requireStringField(rpcName, cause, "address");
      return `this workspace moved to another daemon at ${address}`;
    }
    case "notYetAdopted":
      return "the daemon has not finished adopting this workspace yet";
    default:
      return undefined;
  }
}

/**
 * Read one string off an arm's message, or refuse the view.
 *
 * A cross-cutting arm whose fact is missing is exactly the case the no-fallback
 * rule exists for: the sentence's whole value is the directory or the address,
 * and printing "moved to another daemon at undefined" would be worse than
 * saying nothing.
 */
function requireStringField(rpcName: string, cause: RefusalCause, field: string): string {
  const path = `${rpcName}Error.cause.${cause.case}.${field}`;
  const value = (cause.value as Record<string, unknown> | undefined)?.[field];
  if (typeof value !== "string") {
    throw new MalformedView(path, `expected a string, got ${typeof value}`);
  }
  return value;
}
