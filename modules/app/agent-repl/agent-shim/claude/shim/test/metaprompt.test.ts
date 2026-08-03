import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  METAPROMPT_REL_PATH,
  metapromptPath,
  readMetaprompt,
  systemPromptOption,
} from "../src/metaprompt.js";

const roots: string[] = [];
afterEach(() => {
  roots.splice(0).forEach((r) => fs.rmSync(r, { recursive: true, force: true }));
});

/** A throwaway checkout root, optionally carrying a metaprompt with BODY. */
function makeCheckout(body?: string): string {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "shim-metaprompt-"));
  roots.push(root);
  if (body !== undefined) {
    const file = path.join(root, METAPROMPT_REL_PATH);
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, body, "utf8");
  }
  return root;
}

describe("metapromptPath", () => {
  it("resolves the in-repo path under the session's cwd", () => {
    // Arrange + Act
    const resolved = metapromptPath("/checkout");
    // Assert — mirrors the Emacs side's worktree-local resolution, so a
    // workspace obeys the metaprompt committed in the tree it runs in.
    expect(resolved).toBe("/checkout/modules/app/agent-repl/metaprompt.md");
  });
});

describe("readMetaprompt", () => {
  it("returns the file's text for a checkout that carries one", () => {
    // Arrange
    const root = makeCheckout("# Metaprompt\n\nBe terse.\n");
    // Act
    const text = readMetaprompt(root);
    // Assert
    expect(text).toBe("# Metaprompt\n\nBe terse.");
  });

  it("returns undefined for a checkout with no metaprompt", () => {
    // Arrange — a foreign project that vendors no agent-repl module.
    const root = makeCheckout();
    // Act + Assert: absence is normal, not an error.
    expect(readMetaprompt(root)).toBeUndefined();
  });

  it("returns undefined when no cwd was given", () => {
    // Arrange + Act + Assert
    expect(readMetaprompt(undefined)).toBeUndefined();
  });

  it("returns undefined for an empty cwd string", () => {
    // Arrange + Act + Assert
    expect(readMetaprompt("")).toBeUndefined();
  });

  it("treats a blank metaprompt as no metaprompt", () => {
    // Arrange
    const root = makeCheckout("   \n\n\t\n");
    // Act + Assert: an empty append says nothing to the model.
    expect(readMetaprompt(root)).toBeUndefined();
  });

  it("throws when the metaprompt exists but cannot be read", () => {
    // Arrange: a DIRECTORY where the file belongs, so the read fails with
    // EISDIR rather than ENOENT.
    const root = makeCheckout();
    fs.mkdirSync(path.join(root, METAPROMPT_REL_PATH), { recursive: true });
    // Act + Assert: degrading this to "no guidelines" would run the session
    // unguided and look identical to the normal absent case.
    expect(() => readMetaprompt(root)).toThrow(/reading the metaprompt at/);
  });
});

/** This repository's own checkout root, seven levels above `shim/test`. */
const REPO_ROOT = fileURLToPath(new URL("../../../../../../..", import.meta.url));

describe("this repository's committed metaprompt", () => {
  it("pins subagent dispatch to opus at medium effort", () => {
    // Arrange + Act — the real file, read exactly as a session reads it.
    const text = readMetaprompt(REPO_ROOT);
    // Assert — the directive only governs a session if it survives into the
    // system-prompt append, so the guard is on the delivered text.
    expect(text).toContain("Every subagent runs as opus at medium reasoning effort");
    expect(text).toContain("subagent_type: opus-medium");
  });
});

describe("systemPromptOption", () => {
  it("appends the metaprompt to the claude_code preset", () => {
    // Arrange
    const root = makeCheckout("Be terse.\n");
    // Act
    const option = systemPromptOption(root);
    // Assert
    expect(option).toEqual({ type: "preset", preset: "claude_code", append: "Be terse." });
  });

  it("keeps the bare preset when the checkout carries no metaprompt", () => {
    // Arrange
    const root = makeCheckout();
    // Act
    const option = systemPromptOption(root);
    // Assert — the preset itself is never dropped: it carries the
    // environment block the model needs to resolve `~`.
    expect(option).toEqual({ type: "preset", preset: "claude_code" });
  });
});
