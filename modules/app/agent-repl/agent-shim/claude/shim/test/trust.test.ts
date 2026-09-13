/**
 * The folder-trust entry the shim writes before the vendor is spawned.
 *
 * Every assertion here is against a THROWAWAY account root and a THROWAWAY
 * checkout: nothing in this file may read or write the developer's own
 * `.claude.json`, and no git process is run — a linked worktree is nothing but
 * a `.git` FILE naming its git dir, which is exactly what the resolver reads.
 */
import { mkdirSync, mkdtempSync, readFileSync, statSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { TRUST_KEY, VENDOR_CONFIG_FILE, ensureWorkspaceTrusted, trustRoot } from "../src/trust.js";

/** A throwaway directory, one per call. */
function scratch(prefix: string): string {
  return mkdtempSync(path.join(os.tmpdir(), `shim-trust-${prefix}-`));
}

/** A main checkout with a real `.git` DIRECTORY, as `git init` leaves one. */
function mainCheckout(): string {
  const repo = scratch("repo");
  mkdirSync(path.join(repo, ".git"));
  return repo;
}

/** A linked worktree of `repo`, as `git worktree add` leaves one: a `.git` FILE. */
function linkedWorktree(repo: string, name: string): string {
  const tree = scratch(`worktree-${name}`);
  writeFileSync(
    path.join(tree, ".git"),
    `gitdir: ${path.join(repo, ".git", "worktrees", name)}\n`,
    "utf8",
  );
  return tree;
}

/** The shape this suite reads back out of a written config file. */
interface VendorConfig {
  readonly numStartups?: number;
  readonly projects?: Record<string, Record<string, unknown>>;
}

/** The account root's config file, parsed. */
function config(configDir: string): VendorConfig {
  return JSON.parse(readFileSync(path.join(configDir, VENDOR_CONFIG_FILE), "utf8")) as VendorConfig;
}

describe("trustRoot", () => {
  it("answers the cwd for a directory that is no git checkout at all", () => {
    // Arrange.
    const plain = scratch("plain");

    // Act.
    const root = trustRoot(plain);

    // Assert.
    expect(root).toBe(plain);
  });

  it("answers the checkout itself when `.git` is a directory", () => {
    // Arrange.
    const repo = mainCheckout();

    // Act.
    const root = trustRoot(repo);

    // Assert.
    expect(root).toBe(repo);
  });

  it("answers the MAIN repository for a linked worktree", () => {
    // THE VENDOR KEYS TRUST BY THE REPOSITORY. Grounded against claude 2.1.220:
    // trusting the worktree's own path leaves the warning standing and the
    // allowlists dropped; trusting the main repository is what silences it.
    // Arrange.
    const repo = mainCheckout();
    const tree = linkedWorktree(repo, "feature-x");

    // Act.
    const root = trustRoot(tree);

    // Assert.
    expect(root).toBe(repo);
  });

  it("answers the cwd for a `.git` file that names no worktree git dir", () => {
    // A SUBMODULE'S `.git` FILE IS NOT A WORKTREE'S. It points straight at the
    // superproject's modules directory, and nothing about it says the
    // superproject is this session's project.
    // Arrange.
    const submodule = scratch("submodule");
    writeFileSync(path.join(submodule, ".git"), "gitdir: /elsewhere/.git/modules/sub\n", "utf8");

    // Act.
    const root = trustRoot(submodule);

    // Assert.
    expect(root).toBe(submodule);
  });
});

describe("ensureWorkspaceTrusted", () => {
  it("writes the entry for a fresh worktree the account root has never seen", () => {
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    const tree = linkedWorktree(repo, "fresh");

    // Act.
    const outcome = ensureWorkspaceTrusted(configDir, tree);

    // Assert.
    expect({ outcome, trusted: config(configDir).projects?.[repo]?.[TRUST_KEY] }).toEqual({
      outcome: "granted",
      trusted: true,
    });
  });

  it("leaves an entry that already grants trust exactly as it found it", () => {
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    const file = path.join(configDir, VENDOR_CONFIG_FILE);
    const before = `{\n  "projects": {\n    ${JSON.stringify(repo)}: {\n      "${TRUST_KEY}": true,\n      "history": []\n    }\n  }\n}\n`;
    writeFileSync(file, before, "utf8");

    // Act.
    const outcome = ensureWorkspaceTrusted(configDir, repo);

    // Assert.
    expect({ outcome, text: readFileSync(file, "utf8") }).toEqual({
      outcome: "already_trusted",
      text: before,
    });
  });

  it("keeps every other key in the account's config file", () => {
    // THE FILE IS THE WHOLE ACCOUNT'S STATE. A trust write that clobbered it
    // would take onboarding, history and every other project with it.
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    writeFileSync(
      path.join(configDir, VENDOR_CONFIG_FILE),
      JSON.stringify({ numStartups: 7, projects: { "/somewhere/else": { [TRUST_KEY]: false } } }),
      "utf8",
    );

    // Act.
    ensureWorkspaceTrusted(configDir, repo);

    // Assert.
    expect({
      startups: config(configDir).numStartups,
      other: config(configDir).projects?.["/somewhere/else"]?.[TRUST_KEY],
    }).toEqual({ startups: 7, other: false });
  });

  it("upgrades an entry that exists but refuses trust", () => {
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    writeFileSync(
      path.join(configDir, VENDOR_CONFIG_FILE),
      JSON.stringify({ projects: { [repo]: { [TRUST_KEY]: false, exampleFiles: ["a.ts"] } } }),
      "utf8",
    );

    // Act.
    ensureWorkspaceTrusted(configDir, repo);

    // Assert.
    expect(config(configDir).projects?.[repo]).toEqual({ [TRUST_KEY]: true, exampleFiles: ["a.ts"] });
  });

  it("creates the config file when the account root has none yet", () => {
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();

    // Act.
    ensureWorkspaceTrusted(configDir, repo);

    // Assert.
    expect(config(configDir).projects?.[repo]?.[TRUST_KEY]).toBe(true);
  });

  it("keeps the file's own mode on the replacement", () => {
    // THE VENDOR WRITES THIS FILE 0600. A trust write that widened it would
    // hand every local process the account's state.
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    const file = path.join(configDir, VENDOR_CONFIG_FILE);
    writeFileSync(file, "{}", { encoding: "utf8", mode: 0o600 });

    // Act.
    ensureWorkspaceTrusted(configDir, repo);

    // Assert.
    expect(statSync(file).mode & 0o777).toBe(0o600);
  });

  it("refuses to rewrite a config file it cannot parse, and leaves it standing", () => {
    // NEVER DESTROY WHAT WE CANNOT READ. A half-written or hand-edited config
    // is a condition to surface, not one to overwrite with two keys.
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    const file = path.join(configDir, VENDOR_CONFIG_FILE);
    writeFileSync(file, "{ this is not json", "utf8");

    // Act.
    let raised: unknown;
    try {
      ensureWorkspaceTrusted(configDir, repo);
    } catch (err) {
      raised = err;
    }

    // Assert.
    expect({
      raised: raised instanceof Error && raised.message.includes("is not valid JSON"),
      text: readFileSync(file, "utf8"),
    }).toEqual({ raised: true, text: "{ this is not json" });
  });

  it("refuses a config file whose top level is not an object", () => {
    // Arrange.
    const configDir = scratch("config");
    const repo = mainCheckout();
    writeFileSync(path.join(configDir, VENDOR_CONFIG_FILE), "[1, 2, 3]", "utf8");

    // Act.
    let raised: unknown;
    try {
      ensureWorkspaceTrusted(configDir, repo);
    } catch (err) {
      raised = err;
    }

    // Assert.
    expect(raised instanceof Error && raised.message.includes("does not hold a JSON object")).toBe(true);
  });
});
