/**
 * The metaprompt, as the session's SYSTEM PROMPT.
 *
 * `metaprompt.md` holds the guidelines every agent-repl session must answer
 * under. It used to reach the agent as a read-DIRECTIVE — a sentence in the
 * user turn telling the agent to go read the file — fired periodically by
 * Emacs and re-fired by the daemon after a `/clear` or a resume. That shape
 * cost a tool call per injection, put harness text inside the user's turn, and
 * had to be re-established by hand every time the conversation was cut,
 * because anything living in the conversation dies with it.
 *
 * The system prompt has none of those properties: the SDK re-sends it on every
 * request, so it survives `/clear`, `/compact`, and resume without anyone
 * arming anything, and it never appears in the transcript as something the
 * user said.
 *
 * The file is read from ONE CANONICAL LOCATION — the doom config checkout at
 * `~/.config/doom` — and the session's cwd plays no part in resolving it. It
 * used to be read out of the session's own worktree, so each branch's copy
 * governed the sessions running on it. That is the wrong shape: the guidelines
 * are HARNESS POLICY, not per-project content, so every session in every
 * workspace and every repository answers under the same version-controlled
 * copy, and a feature branch that edits `metaprompt.md` cannot fork the policy
 * for its own sessions before the edit is reviewed and merged.
 */
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";
import type { WorkspaceStanding } from "./engine/profile.js";

/** The doom config checkout's location, relative to the user's home. */
export const DOOM_CHECKOUT_REL_PATH = ".config/doom";

/**
 * In-repo location of the metaprompt, relative to the doom checkout root.
 *
 * Mirrors the Emacs side (`agent-repl-metaprompt-file` in input.el), which
 * resolves the same file inside the checkout Emacs itself was loaded from.
 */
export const METAPROMPT_REL_PATH = "modules/app/agent-repl/metaprompt.md";

/**
 * Absolute path of the canonical metaprompt.
 *
 * HOME defaults to the running user's home directory and exists so tests can
 * resolve against a throwaway home instead of the developer's real one.
 */
export function metapromptPath(home: string = homedir()): string {
  return path.join(home, DOOM_CHECKOUT_REL_PATH, METAPROMPT_REL_PATH);
}

/**
 * The canonical metaprompt's text, or `undefined` when this machine carries
 * no doom checkout.
 *
 * ABSENCE IS NORMAL AND SILENT: a machine may run the shim without the doom
 * config checkout present, and a session with no guidelines file simply has no
 * guidelines to impose. Absence is the ONLY tolerated failure — a file that
 * exists but cannot be read is a broken checkout or a permission fault, and
 * the error is rethrown rather than degraded into "no guidelines", which would
 * silently run the session unguided and look identical to the normal case.
 *
 * A blank file is treated as no metaprompt: appending an empty section to the
 * system prompt says nothing and only muddies what the model is handed.
 */
export function readMetaprompt(home: string = homedir()): string | undefined {
  let raw: string;
  try {
    raw = readFileSync(metapromptPath(home), "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw new Error(
      `shim: reading the metaprompt at ${metapromptPath(home)} failed: ${err instanceof Error ? err.message : String(err)}`,
      { cause: err },
    );
  }
  const text = raw.trim();
  return text === "" ? undefined : text;
}

/** The SDK `systemPrompt` option shape, preset plus optional append. */
interface SystemPromptOption {
  type: "preset";
  preset: "claude_code";
  append?: string;
}

/**
 * The section naming the workspace the session runs in, appended after the
 * metaprompt. It is composed HERE, from the name the daemon states at
 * StartSession (`shim.v1.StartSessionRequest.workspace_name`), and is never
 * part of `metaprompt.md`: the file is one policy every session shares, and
 * the name is this session's own.
 */
export function workspaceSection(workspaceName: string): string {
  return (
    `You are running in an agent-repl IDE workspace, and your workspace's name is \`${workspaceName}\`. ` +
    "The user, other workspaces, and the agent API (`agent-repld workspace ...`) refer to this workspace by that name."
  );
}

/**
 * The section stating how the workspace stands within its repository: its
 * repository's main workspace, or a linked worktree. Composed from the
 * standing the daemon states at StartSession
 * (`shim.v1.SessionProfileAgent.standing`); the main workspace's rules are
 * what an agent there must not get wrong (it lands nothing, is never a
 * one-shot, and splits work off into created workspaces).
 */
export function standingSection(standing: WorkspaceStanding): string {
  switch (standing) {
    case "main_workspace":
      return (
        "This workspace is its repository's MAIN workspace: it sits on the repository's main checkout, on the main branch. " +
        "It lands nothing itself and is never a one-shot (`agent-repld workspace set here -one-shot` is refused), " +
        "so separate work is split off into created workspaces (`agent-repld workspace create`), which land their own work."
      );
    case "linked_workspace":
      return (
        "This workspace is not its repository's main workspace: it is a linked worktree on its own branch, " +
        "and it lands its own work."
      );
  }
}

/**
 * The session's `systemPrompt` option: the `claude_code` preset with the
 * metaprompt appended, then the section naming WORKSPACENAME, the workspace
 * the session runs in (workspaceSection), then the section stating STANDING
 * (standingSection), which is ALWAYS LAST.
 *
 * The PRESET STAYS. It carries the environment block (cwd, platform, home)
 * that interactive-CLI parity depends on — replacing it with the metaprompt
 * outright would leave the model unable to resolve `~` and inventing paths.
 * The metaprompt is additional guidelines, not a replacement for knowing where
 * it is running.
 */
export function systemPromptOption(
  workspaceName: string,
  standing: WorkspaceStanding,
  home: string = homedir(),
): SystemPromptOption {
  if (workspaceName === "") {
    throw new Error("shim: the system prompt names its workspace, and the workspace name is empty");
  }
  const metaprompt = readMetaprompt(home);
  // THE STANDING NOTICE MUST STAY THE LAST SECTION APPENDED. Whenever the
  // system prompt is changed -- a section added, moved or reordered, here or
  // anywhere else it is assembled -- standingSection stays at the very end, so
  // the last thing every agent reads before acting is whether its workspace is
  // its repository's main workspace. metaprompt.test.ts pins it last for both
  // standings.
  const sections = [
    ...(metaprompt !== undefined ? [metaprompt] : []),
    workspaceSection(workspaceName),
    standingSection(standing),
  ];
  return {
    type: "preset",
    preset: "claude_code",
    append: sections.join("\n\n"),
  };
}
