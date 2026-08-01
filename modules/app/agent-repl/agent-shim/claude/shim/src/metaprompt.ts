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
 * The file is read from the SESSION'S OWN cwd rather than from anywhere near
 * this shim's install path. A workspace runs in its own git worktree, and the
 * metaprompt it must obey is the one committed in THAT tree — the same reason
 * the Emacs read-directive resolved a worktree copy before it named a path.
 */
import { readFileSync } from "node:fs";
import path from "node:path";

/**
 * In-repo location of the metaprompt, relative to a session's cwd.
 *
 * Mirrors the Emacs side (`agent-repl--metaprompt-file-for` in input.el),
 * which resolves the same path under a workspace's `:project-dir`.
 */
export const METAPROMPT_REL_PATH = "modules/app/agent-repl/metaprompt.md";

/** Absolute path the metaprompt would live at for a session rooted at CWD. */
export function metapromptPath(cwd: string): string {
  return path.join(cwd, METAPROMPT_REL_PATH);
}

/**
 * The metaprompt text for a session rooted at CWD, or `undefined` when this
 * checkout carries none.
 *
 * ABSENCE IS NORMAL AND SILENT: most checkouts a session may run in are not
 * this repository, and a project that vendors no metaprompt simply has no
 * guidelines to impose. Absence is the ONLY tolerated failure — a file that
 * exists but cannot be read is a broken checkout or a permission fault, and
 * the error is rethrown rather than degraded into "no guidelines", which would
 * silently run the session unguided and look identical to the normal case.
 *
 * A blank file is treated as no metaprompt: appending an empty section to the
 * system prompt says nothing and only muddies what the model is handed.
 */
export function readMetaprompt(cwd: string | undefined): string | undefined {
  if (cwd === undefined || cwd === "") return undefined;
  let raw: string;
  try {
    raw = readFileSync(metapromptPath(cwd), "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw new Error(
      `shim: reading the metaprompt at ${metapromptPath(cwd)} failed: ${err instanceof Error ? err.message : String(err)}`,
      { cause: err },
    );
  }
  const text = raw.trim();
  return text === "" ? undefined : text;
}

/** The SDK `systemPrompt` option shape, preset plus optional append. */
export interface SystemPromptOption {
  type: "preset";
  preset: "claude_code";
  append?: string;
}

/**
 * The session's `systemPrompt` option: the `claude_code` preset with the
 * metaprompt appended.
 *
 * The PRESET STAYS. It carries the environment block (cwd, platform, home)
 * that interactive-CLI parity depends on — replacing it with the metaprompt
 * outright would leave the model unable to resolve `~` and inventing paths.
 * The metaprompt is additional guidelines, not a replacement for knowing where
 * it is running.
 */
export function systemPromptOption(cwd: string | undefined): SystemPromptOption {
  const metaprompt = readMetaprompt(cwd);
  return {
    type: "preset",
    preset: "claude_code",
    ...(metaprompt !== undefined ? { append: metaprompt } : {}),
  };
}
