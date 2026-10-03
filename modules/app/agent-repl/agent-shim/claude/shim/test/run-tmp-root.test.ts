import { mkdirSync, mkdtempSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { RUN_TMP_PARENT, RUN_TMP_ROOT_ENV, makeRunTmpRoot } from "./run-tmp-root.js";

describe("the run's temp root, as this run sees it", () => {
  it("answers os.tmpdir() with the root the run's globalSetup made", () => {
    // Arrange / Act
    const root = process.env[RUN_TMP_ROOT_ENV];

    // Assert
    expect({ tmpdir: tmpdir(), parent: root === undefined ? undefined : dirname(root) }).toEqual({
      tmpdir: root,
      parent: RUN_TMP_PARENT,
    });
  });
});

describe("makeRunTmpRoot", () => {
  let parent: string;
  let saved: { tmpdir: string | undefined; root: string | undefined };

  beforeEach(() => {
    parent = mkdtempSync(join(tmpdir(), "root-parent-"));
    saved = { tmpdir: process.env.TMPDIR, root: process.env[RUN_TMP_ROOT_ENV] };
  });

  afterEach(() => {
    for (const [name, value] of [["TMPDIR", saved.tmpdir], [RUN_TMP_ROOT_ENV, saved.root]] as const) {
      if (value === undefined) delete process.env[name];
      else process.env[name] = value;
    }
    rmSync(parent, { recursive: true, force: true });
  });

  it("points TMPDIR at a fresh directory under the parent", () => {
    // Act
    const teardown = makeRunTmpRoot(parent);
    const root = process.env.TMPDIR;
    teardown();

    // Assert
    expect(root === undefined ? undefined : dirname(root)).toBe(parent);
  });

  it("names the same directory in the marker variable", () => {
    // Act
    const teardown = makeRunTmpRoot(parent);
    const both = [process.env.TMPDIR, process.env[RUN_TMP_ROOT_ENV]];
    teardown();

    // Assert
    expect(both[0]).toBe(both[1]);
  });

  it("removes the root and everything a test left in it at teardown", () => {
    // Arrange
    const teardown = makeRunTmpRoot(parent);
    const leaked = join(process.env.TMPDIR ?? "", "fake-drive-x", "cfg");
    mkdirSync(leaked, { recursive: true });
    writeFileSync(join(leaked, "transcript.jsonl"), "{}\n");

    // Act
    teardown();

    // Assert
    expect(readdirSync(parent)).toEqual([]);
  });

  it("puts TMPDIR back as it was at teardown", () => {
    // Arrange
    const before = process.env.TMPDIR;
    const teardown = makeRunTmpRoot(parent);

    // Act
    teardown();

    // Assert
    expect(process.env.TMPDIR).toBe(before);
  });

  it("removes the marker variable at teardown when it was unset before", () => {
    // Arrange
    delete process.env[RUN_TMP_ROOT_ENV];
    const teardown = makeRunTmpRoot(parent);

    // Act
    teardown();

    // Assert
    expect(RUN_TMP_ROOT_ENV in process.env).toBe(false);
  });

  it("throws when the root cannot be removed, rather than leaving it quietly", () => {
    // Arrange: the root is already gone, the way a racing remover would leave it.
    const teardown = makeRunTmpRoot(parent);
    rmSync(process.env.TMPDIR ?? "", { recursive: true });

    // Act / Assert
    expect(teardown).toThrow(/ENOENT/);
  });

  it("refuses a parent that does not exist", () => {
    // Arrange
    const missing = join(parent, "missing");

    // Act / Assert
    expect(() => makeRunTmpRoot(missing)).toThrow(/ENOENT/);
  });
});
