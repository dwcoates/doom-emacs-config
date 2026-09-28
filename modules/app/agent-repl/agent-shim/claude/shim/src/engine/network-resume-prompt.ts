/**
 * engine/network-resume-prompt.ts — the prompt that asks the main agent to
 * continue the agents a network outage cut off (engine/network-resume.ts).
 *
 * DEPENDENCY-FREE ON PURPOSE: the mocked vendor reads this prompt back
 * (`fake/scenarios/subagents.ts`, `network-resume`) to answer it as the vendor
 * would, and it must not pull the engine or the protos in to do so.
 */

/**
 * The marker the resume prompt opens with.
 *
 * An HTML comment, as the keep-alive's is, so a markdown surface draws nothing
 * for it while a reader of the transcript can still tell whose prompt it was.
 */
export const NETWORK_RESUME_MARKER = "<!--agent-repl:network-resume-->";

/** The message each resumed agent is sent. */
export const NETWORK_RESUME_MESSAGE =
  "You were interrupted by a network outage: the API server could not be reached. " +
  "The connection is back. Continue exactly where you left off, with the same brief and the same finish.";

/** One agent a resume prompt continues. */
export interface ResumeTarget {
  /** The vendor's agent id — what `SendMessage.to` addresses. */
  readonly taskId: string;
  /** The agent's own description, so the main agent can tell which it is. */
  readonly description: string;
}

const TARGET_LINE = /^- agent `([^`]+)`/;

/**
 * The prompt the main agent is given to continue the agents.
 *
 * ONE PROMPT FOR EVERY AGENT DUE, so N agents cost one main-agent turn. Each
 * target is one line naming its id in backticks; {@link resumePromptTargets}
 * reads those lines back.
 */
export function networkResumePrompt(targets: readonly ResumeTarget[]): string {
  const lines = targets.map((target) => `- agent \`${target.taskId}\` — ${JSON.stringify(target.description)}`);
  return [
    NETWORK_RESUME_MARKER,
    "agent-repl: a network outage cut off the background agent(s) below; the API is reachable again.",
    `For each one, call SendMessage with \`to\` set to its agent id and this message: ${JSON.stringify(NETWORK_RESUME_MESSAGE)}`,
    "Do nothing else. Reply with one short line once the messages are sent.",
    ...lines,
  ].join("\n");
}

/** Whether a prompt is the shim's own network-resume prompt. */
export function isNetworkResumePrompt(text: string): boolean {
  return text.trimStart().startsWith(NETWORK_RESUME_MARKER);
}

/** The agent ids a resume prompt names, in order. */
export function resumePromptTargets(text: string): string[] {
  const ids: string[] = [];
  for (const line of text.split("\n")) {
    const id = TARGET_LINE.exec(line)?.[1];
    if (id !== undefined) ids.push(id);
  }
  return ids;
}
