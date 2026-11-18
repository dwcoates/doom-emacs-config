import { afterEach, describe, expect, it, vi } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  DOOM_CHECKOUT_REL_PATH,
  METAPROMPT_REL_PATH,
  LANDING_REL_PATHS,
  metapromptPath,
  readLandingSection,
  readMetaprompt,
  standingSection,
  systemPromptOption,
  workspaceSection,
} from "../src/metaprompt.js";

const homes: string[] = [];
afterEach(() => {
  homes.splice(0).forEach((h) => fs.rmSync(h, { recursive: true, force: true }));
});

/**
 * A throwaway HOME, optionally carrying a doom checkout whose metaprompt holds
 * BODY. Every test resolves against one of these rather than the developer's
 * real home, so no test reads the machine's actual canonical metaprompt.
 */
function makeHome(body?: string): string {
  const home = fs.mkdtempSync(path.join(os.tmpdir(), "shim-metaprompt-home-"));
  homes.push(home);
  if (body !== undefined) {
    const file = metapromptPath(home);
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, body, "utf8");
    // Every landing's section beside it, as a doom checkout carries.
    for (const rel of Object.values(LANDING_REL_PATHS)) {
      fs.writeFileSync(path.join(home, DOOM_CHECKOUT_REL_PATH, rel), `landing section ${rel}`, "utf8");
    }
  }
  return home;
}

describe("metapromptPath", () => {
  it("resolves the canonical doom checkout under the home directory", () => {
    // Arrange + Act
    const resolved = metapromptPath("/home/u");
    // Assert — the session's cwd plays no part: the guidelines are harness
    // policy, so every session reads the one version-controlled copy.
    expect(resolved).toBe("/home/u/.config/doom/modules/app/agent-repl/metaprompt.md");
  });

  it("defaults to the running user's home directory", () => {
    // Arrange + Act
    const resolved = metapromptPath();
    // Assert — the default is the only resolution production uses; the
    // parameter exists so tests can point at a throwaway home.
    expect(resolved).toBe(path.join(os.homedir(), DOOM_CHECKOUT_REL_PATH, METAPROMPT_REL_PATH));
  });
});

describe("readMetaprompt", () => {
  it("returns the canonical file's text when the doom checkout carries one", () => {
    // Arrange
    const home = makeHome("# Metaprompt\n\nBe terse.\n");
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toBe("# Metaprompt\n\nBe terse.");
  });

  it("returns undefined on a machine with no doom checkout", () => {
    // Arrange — a home that never cloned the config repository.
    const home = makeHome();
    // Act + Assert: absence is normal, and runs the session unguided silently.
    expect(readMetaprompt(home)).toBeUndefined();
  });

  it("ignores a metaprompt sitting in a session's own checkout", () => {
    // Arrange — the pre-canonical layout: a copy at the in-repo path under a
    // worktree root, with no doom checkout anywhere above it.
    const worktree = makeHome();
    const stray = path.join(worktree, METAPROMPT_REL_PATH);
    fs.mkdirSync(path.dirname(stray), { recursive: true });
    fs.writeFileSync(stray, "Branch-local guidelines.\n", "utf8");
    // Act + Assert: a feature branch editing metaprompt.md must not fork the
    // policy for its own sessions.
    expect(readMetaprompt(worktree)).toBeUndefined();
  });

  it("treats a blank metaprompt as no metaprompt", () => {
    // Arrange
    const home = makeHome("   \n\n\t\n");
    // Act + Assert: an empty append says nothing to the model.
    expect(readMetaprompt(home)).toBeUndefined();
  });

  it("throws when the metaprompt exists but cannot be read", () => {
    // Arrange: a DIRECTORY where the file belongs, so the read fails with
    // EISDIR rather than ENOENT.
    const home = makeHome();
    fs.mkdirSync(metapromptPath(home), { recursive: true });
    // Act + Assert: degrading this to "no guidelines" would run the session
    // unguided and look identical to the normal absent case.
    expect(() => readMetaprompt(home)).toThrow(/reading the metaprompt at/);
  });
});

/** This repository's own checkout root, seven levels above `shim/test`. */
const REPO_ROOT = fileURLToPath(new URL("../../../../../../..", import.meta.url));

describe("this repository's committed metaprompt", () => {
  // A throwaway home whose canonical checkout path points at this repository,
  // so the real committed file is read exactly as a session reads it without
  // touching the developer's actual home.
  function homeOnThisRepository(): string {
    const home = makeHome();
    fs.mkdirSync(path.join(home, path.dirname(DOOM_CHECKOUT_REL_PATH)), { recursive: true });
    fs.symlinkSync(REPO_ROOT, path.join(home, DOOM_CHECKOUT_REL_PATH));
    return home;
  }

  it("defaults subagent dispatch to opus at medium effort and allows the sonnet tier", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert — the directive only governs a session if it survives into the
    // system-prompt append, so the guard is on the delivered text.
    expect(text).toContain("Subagents default to opus at medium reasoning effort");
    expect(text).toContain("subagent_type: opus-medium");
    expect(text).toContain("dispatched through the `sonnet-medium` subagent type");
    expect(text).not.toContain("weaker tier than opus is a defect");
  });

  it("names bypass queue mode as the mode a queue-bypass session runs in", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("bypassing_queue", home);
    // Assert — the user names the mode "bypass queue mode", so the section
    // must tie that name to this session.
    expect(text).toContain("This session runs in bypass queue mode, which agent-repl also calls queue bypass mode.");
  });

  it("never lets a queue-landing workspace run the full suite, and scopes a gate failure's remediation", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("through_queue", home);
    // Assert — owner ruling 2026-11-15: only the merge queue's gate runs the
    // full suite; a workspace runs only what it added, changed, or was handed.
    expect(text).toContain("A workspace NEVER runs the full suite, for any reason");
    expect(text).toContain("The merge queue alone runs it, programmatically, as the gate of every landing.");
    expect(text).toContain("That remediation runs only the failing tests it was handed, once fixed, and the tests the work added or changed.");
    expect(text).not.toContain("The rare exception is a change with systemic knock-on effects");
  });

  it("keeps a queue-bypass session off the merge queue, doing its own work, and alone managing the queue", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("bypassing_queue", home);
    // Assert — owner ruling 2026-11-15: bypass mode exists to skip the queue,
    // and creates no child or subagent the user did not explicitly ask for.
    expect(text).toContain("nothing this workspace lands or dispatches ever goes through it");
    expect(text).toContain("it creates no child workspace and runs no subagent unless the user explicitly asks");
    expect(text).toContain("Several such branches are merged into one branch and land at once");
    expect(text).toContain("runs only the tests its changes add or change");
    expect(text).toContain("No child or subagent of this workspace ever runs it");
    expect(text).toContain("Only this workspace pauses or resumes the merge queue");
    expect(text).not.toContain("creates child workspaces, one-shots and async merges as any workspace does");
  });

  it("has a queue-bypass session pause the queue only to land, and resume it at once", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("bypassing_queue", home);
    // Assert — owner ruling 2026-11-14: the queue is paused for no longer
    // than a landing needs, and a moved main branch is rebased onto first.
    expect(text).toContain("Pause the queue only once a change is complete and about to land: `agent-repld merge-queue pause -repository here -to-land`.");
    // 2026-11-14: a pause the owner asked for outlives the turn's end.
    expect(text).toContain("A pause the user asks for (to save power, to look at the queue) is made without `-to-land`, so it outlives this turn: `agent-repld merge-queue pause -repository here`.");
    expect(text).toContain("Resume the queue the moment the landing is done: `agent-repld merge-queue resume -repository here`.");
    expect(text).toContain("If the main branch moved since the work began, rebase onto it and resolve every conflict.");
    expect(text).not.toContain("Before the first change of every prompt");
    expect(text).not.toContain("hard-stopped");
  });

  it("has a queue-bypass session defer to the deploy mediator and watch it as it runs", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("bypassing_queue", home);
    // Assert — owner ruling 2026-11-17: a full suite run during a check
    // contended with the mediator; the bypass session defers to it and
    // remediates it when it fails.
    expect(text).toContain("this workspace defers to it in procedure");
    expect(text).toContain("It never lands, deploys, bounces or restarts anything, pauses the queue, or runs a full suite.");
    expect(text).toContain("Watch the mediator as it runs");
    expect(text).toContain("needs remediation itself");
  });

  it("governs a queue-bypass session's test runs unless the user asks for an ungoverned one", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("bypassing_queue", home);
    // Assert — owner ruling 2026-11-13: an ungoverned full run starved the
    // owner's machine.
    expect(text).toContain("Every test run is governed by default");
    expect(text).toContain("Run one ungoverned only when the user asks for it, by adding `-ungoverned -as-user`.");
  });

  it("gives the deploy mediator its own section: check every deploy, and fix what it dispatches itself", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("deploy_mediator", home);
    // Assert — owner ruling 2026-11-17: one standing mediator checks every
    // deploy, fixes what it dispatches, and settles escalations with the user.
    expect(text).toContain("This session is the deploy mediator: the one workspace agent-repl keeps to check every deploy");
    expect(text).toContain("`agent-repld merge-queue mediation approve -summary");
    expect(text).toContain("agent-repld merge-queue mediation dispatch -summary");
    expect(text).toContain("Problems this workspace can fix, of any standing but TAKEN ON: dispatch them, then fix them in the same turn.");
    expect(text).toContain("The dispatch comes first, before any edit or investigation of a fix: it is what moves the queue's label from ASSESSING to FIXING");
    expect(text).toContain("agent-repld workspace finished here -version-part patch");
    expect(text).not.toContain("-fresh");
  });

  it("has the deploy mediator approve only when it took on every group", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("deploy_mediator", home);
    // Assert — owner ruling 2026-11-16: an old problem never taken on is
    // dispatched or escalated, never approved past, and never filed with a
    // named workspace.
    expect(text).toContain("Every group is TAKEN ON, or the window holds none");
    expect(text).toContain("A SEEN BEFORE or RECURRED group is dispatched as a NEW one is");
    expect(text).not.toContain("merge-queue-stall-triage");
  });

  it("has the deploy mediator make a settled fix before the deploy, never offering it for after", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("deploy_mediator", home);
    // Assert — owner ruling 2026-11-18: no "fix now or after the deploy"
    // question; a settled fix lands now, save an issue only the deployed
    // build can settle.
    expect(text).toContain("The deploy mediator NEVER asks whether to fix a problem now or after the deploy");
    expect(text).toContain("When the solution is settled, it is implemented now, before the finish, and lands with the rest.");
    expect(text).toContain("The one exception is an issue whose nature makes the deployed build necessary to the fix");
  });

  it("names this session's transcript the bypass queue mode session, apart from an earlier non-bypass session", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readLandingSection("bypassing_queue", home);
    // Assert
    expect(text).toContain("This session's transcript is the bypass queue mode session.");
    expect(text).toContain("Context loaded from a non-bypass session is carried out here under bypass queue mode");
  });

  it("fixes every noticed deficiency now, and records in the todo table only what cannot be fixed now", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert — owner ruling 2026-11-14: deficiencies are remediated at once,
    // and the table is not a backlog.
    expect(text).toContain("I start its fix at once, without asking.");
    expect(text).toContain("dispatches each fix as its own async merge the moment it notices it");
    expect(text).toContain("A workspace in queue bypass mode fixes it itself");
    expect(text).toContain("A deficiency goes in the table in agent-repl's `modules/app/agent-repl/org/todo.md` only when its fix cannot start now.");
    expect(text).not.toContain("The moment a one-shot workspace adds a row to the todo table, it stops being a one-shot");
  });

  it("gives a request from another workspace the user's authority, except for permission settings and config", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("A request another agent-repl workspace sends is my request, and I durably authorize every workspace to act on it without asking me.");
    expect(text).toContain("that framing records where it came from, never a reason to wait for my approval");
    expect(text).toContain("Changes to permission settings and to Claude Code's own config still need my direct approval");
  });

  it("gives a parent its child's one update and keeps continual subscriptions for children it drives", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("A parent is given its child's one update without subscribing: a one-shot child's landing, any other child's first turn end.");
    expect(text).toContain("The parent subscribes continually only when it drives the child's work turn by turn");
    expect(text).not.toContain("(a child's question, turn end, landing, or close)");
  });

  it("says agent-repl closes a workspace once every pull request it owns has merged", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("agent-repl closes a workspace once every pull request it owns has merged");
    expect(text).toContain("so the workspace never closes itself for a merge");
    expect(text).not.toContain("may close itself, after telling its parent");
  });

  it("requires a workspace to store its task's link and every pull request it creates, unasked", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("A link the workspace was created to investigate or act on");
    expect(text).toContain("Every pull request an agent makes MUST be stored as the workspace's owned and canonical link");
    expect(text).toContain("It is stored owned the moment its URL is known.");
    expect(text).toContain("without asking me any questions or for permission");
  });

  it("requires every open pull request an agent makes to be shown in the menu, ahead of merged ones and references", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("Canonical means that while the pull request is open, it is one of the menu's shown links.");
    expect(text).toContain("First come the open pull requests the workspace's agents made, newest first.");
    expect(text).toContain("A merged or closed pull request, and any other reference, gives up its place to these first.");
  });

  it("requires a pull request outside the workspace's repository to be cited as repo-name#number on every mention", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("always cited as `<repo-name>#<pr-number>`, never as a bare `#<pr-number>`");
    expect(text).toContain("even when an earlier mention already spelled out the repository");
  });

  it("ends every response tree with a summary branch holding a 3-5 node TLDR branch, followed only by the questions branch", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("Every response's tree ends with a summary section, rendered as its own root branch.");
    expect(text).toContain("The summary branch is ALWAYS the root branch immediately before the questions branch");
    expect(text).toContain("When the response has no questions branch, the summary branch is the LAST root branch of the tree.");
    expect(text).toContain("The questions branch is ALWAYS the LAST root branch of the tree, after the summary branch.");
    expect(text).toContain("The TLDR branch holds no more than 3-5 nodes in total");
    expect(text).toContain("it ALWAYS sits immediately before the summary branch.");
    expect(text).not.toContain("it is ALWAYS the LAST root branch of the tree.");
  });

  it("keeps a reply as plain prose only when it fits in a few short sentences", () => {
    // Arrange
    const text = readMetaprompt(homeOnThisRepository()) ?? "";
    // Assert — owner ruling 2026-11-18: twenty lines of prose is a tree.
    expect(text).toContain("A response that fits in three to five SHORT sentences, or two to three lines of text, is a brief, normal reply.");
  });

  it("requires a shell command to be one runnable block fenced as bash", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home);
    // Assert
    expect(text).toContain("A shell command for the user to run is always its own fenced block, opened with ```bash");
    expect(text).toContain("The block holds exactly ONE runnable command");
  });

  it("dispatches a handed failure's fix with a command valid from any repository", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home) ?? "";
    // Assert — a fork never crosses repositories, so the other-repository
    // command is fresh and names its repository.
    expect(text).toContain("`agent-repld workspace create -relationship child -parent here -lands-into main -context fork -fork here -one-shot -prompt <the fix>`");
    expect(text).toContain("`agent-repld workspace create -repository <repository> -relationship child -parent here -lands-into main -context fresh -one-shot -prompt <the fix>`");
    expect(text).not.toContain("-repository <repository> -relationship child -parent here -lands-into main -context fork");
  });

  it("forbids version controlling any conversation, in any repository", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home) ?? "";
    // Assert
    expect(text).toContain("NEVER commit a chat thread, an agent transcript, or any other record of a conversation, in any repository, ever.");
    expect(text).toContain("zettel chats, agent-repl workspace transcripts, Claude Code session files");
  });

  it("points a main workspace at its standing notice rather than restating how it dispatches work", () => {
    // Arrange
    const home = homeOnThisRepository();
    // Act
    const text = readMetaprompt(home) ?? "";
    // Assert
    expect(text).toContain("How a main workspace dispatches work is the notice that ends its system prompt.");
    expect(text).not.toContain("only when it truly needs that conversation");
  });
});

describe("systemPromptOption", () => {
  it("appends the canonical metaprompt, then the workspace's section, to the claude_code preset", () => {
    // Arrange
    const home = makeHome("Be terse.\n");
    // Act
    const option = systemPromptOption("parser-fix", "linked_workspace", "through_queue", home);
    // Assert
    expect(option).toEqual({
      type: "preset",
      preset: "claude_code",
      append: `Be terse.\n\n${readLandingSection("through_queue", home)}\n\n${workspaceSection("parser-fix")}\n\n${standingSection("linked_workspace")}`,
    });
  });

  it("appends the workspace's section alone when the machine carries no doom checkout", () => {
    // Arrange
    const home = makeHome();
    // Act
    const option = systemPromptOption("parser-fix", "linked_workspace", "through_queue", home);
    // Assert — the preset itself is never dropped: it carries the
    // environment block the model needs to resolve `~`.
    expect(option).toEqual({
      type: "preset",
      preset: "claude_code",
      append: `${workspaceSection("parser-fix")}\n\n${standingSection("linked_workspace")}`,
    });
  });

  it("refuses a workspace with no name", () => {
    // Arrange
    const home = makeHome("Be terse.\n");
    // Act, Assert
    expect(() => systemPromptOption("", "linked_workspace", "through_queue", home)).toThrow("the workspace name is empty");
  });
});

describe("the standing notice", () => {
  it.each([
    { standing: "main_workspace" as const, home: "Be terse.\n" },
    { standing: "linked_workspace" as const, home: "Be terse.\n" },
    { standing: "main_workspace" as const, home: undefined },
    { standing: "linked_workspace" as const, home: undefined },
  ])("is the last section of the system prompt for a $standing (metaprompt: $home)", ({ standing, home }) => {
    // Arrange
    const dir = makeHome(home);
    // Act
    const append = systemPromptOption("parser-fix", standing, "through_queue", dir).append ?? "";
    // Assert — nothing may ever be appended after it.
    expect(append.endsWith(standingSection(standing))).toBe(true);
    expect(append.lastIndexOf(workspaceSection("parser-fix"))).toBeLessThan(append.lastIndexOf(standingSection(standing)));
  });

  it("tells a main workspace it lands nothing, is never a one-shot, and splits work off", () => {
    // Act
    const section = standingSection("main_workspace");
    // Assert
    expect(section).toContain("its repository's MAIN workspace");
    expect(section).toContain("It lands nothing itself and is never a one-shot");
    expect(section).toContain("`agent-repld workspace create`");
  });

  it("tells a main workspace to dispatch fresh one-shot children cut from the main branch", () => {
    // Act
    const section = standingSection("main_workspace");
    // Assert
    expect(section).toContain("`-relationship child -parent here -lands-into main`");
    expect(section).toContain("one-shots (`-one-shot`) by preference");
    expect(section).toContain("(`-context fresh`) by default");
    expect(section).toContain("only when it truly needs that conversation");
    expect(section).toContain("Every child lands its own work, one-shot or not, whatever its context.");
    expect(section).not.toContain("-owner-notice");
  });

  it("tells a linked worktree it is not the main workspace and lands its own work", () => {
    // Act
    const section = standingSection("linked_workspace");
    // Assert
    expect(section).toContain("not its repository's main workspace");
    expect(section).toContain("it lands its own work");
  });
});

describe("workspaceSection", () => {
  it("says the session runs in an agent-repl workspace, and names it", () => {
    // Act
    const section = workspaceSection("parser-fix");
    // Assert
    expect(section).toContain("You are running in an agent-repl IDE workspace");
    expect(section).toContain("your workspace's name is `parser-fix`");
  });
});

describe("readMetaprompt surfaces a non-Error read failure", () => {
  afterEach(() => {
    vi.doUnmock("node:fs");
    vi.resetModules();
  });

  it("stringifies a thrown value that is not an Error", async () => {
    // Arrange — a read failure that is neither ENOENT nor an Error instance.
    const home = makeHome("guidelines");
    vi.resetModules();
    vi.doMock("node:fs", async (importOriginal) => {
      const actual = await importOriginal<typeof import("node:fs")>();
      return {
        ...actual,
        readFileSync: () => {
          throw "the volume went away";
        },
      };
    });
    const metaprompt = await import("../src/metaprompt.js");

    // Act, Assert — absence is the ONLY tolerated failure, so this rethrows
    // with the raw value stringified into the message.
    expect(() => metaprompt.readMetaprompt(home)).toThrow(
      `shim: reading the metaprompt at ${metapromptPath(home)} failed: the volume went away`,
    );
  });
});
