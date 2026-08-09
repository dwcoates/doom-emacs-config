import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  DOOM_CHECKOUT_REL_PATH,
  METAPROMPT_REL_PATH,
  metapromptPath,
  readMetaprompt,
  systemPromptOption,
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
  it("pins subagent dispatch to opus at medium effort", () => {
    // Arrange — a throwaway home whose canonical checkout path points at this
    // repository, so the real committed file is read exactly as a session
    // reads it without touching the developer's actual home.
    const home = makeHome();
    fs.mkdirSync(path.join(home, path.dirname(DOOM_CHECKOUT_REL_PATH)), { recursive: true });
    fs.symlinkSync(REPO_ROOT, path.join(home, DOOM_CHECKOUT_REL_PATH));
    // Act
    const text = readMetaprompt(home);
    // Assert — the directive only governs a session if it survives into the
    // system-prompt append, so the guard is on the delivered text.
    expect(text).toContain("Every subagent runs as opus at medium reasoning effort");
    expect(text).toContain("subagent_type: opus-medium");
  });
});

describe("systemPromptOption", () => {
  it("appends the canonical metaprompt to the claude_code preset", () => {
    // Arrange
    const home = makeHome("Be terse.\n");
    // Act
    const option = systemPromptOption(home);
    // Assert
    expect(option).toEqual({ type: "preset", preset: "claude_code", append: "Be terse." });
  });

  it("keeps the bare preset when the machine carries no doom checkout", () => {
    // Arrange
    const home = makeHome();
    // Act
    const option = systemPromptOption(home);
    // Assert — the preset itself is never dropped: it carries the
    // environment block the model needs to resolve `~`.
    expect(option).toEqual({ type: "preset", preset: "claude_code" });
  });
});
