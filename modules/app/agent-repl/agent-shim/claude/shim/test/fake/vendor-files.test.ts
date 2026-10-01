/**
 * The mocked vendor's on-disk half. Every assertion here is against a shape
 * copied from `testdata/corpus`, because the REAL sidecar reads these files in
 * `--fake` runs: a wrong path or a wrong field name is a silent ingestion gap,
 * not a test failure, unless it is pinned here.
 */
import { existsSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

import {
  cwdSlug,
  FAKE_CLI_VERSION,
  lastChainedUuid,
  SpoolWriter,
  spoolPath,
  SubagentWriter,
  subagentMetaPath,
  subagentTranscriptPath,
  TranscriptWriter,
  transcriptPath,
  VendorFiles,
} from "../../src/fake/vendor-files.js";

const temp = (): string => mkdtempSync(join(tmpdir(), "fake-vendor-files-"));

const lines = (path: string): Record<string, unknown>[] =>
  readFileSync(path, "utf8")
    .split("\n")
    .filter((l) => l.trim() !== "")
    .map((l) => JSON.parse(l) as Record<string, unknown>);

describe("cwdSlug", () => {
  it("replaces every slash and dot, as the observed dotfile-config path does", () => {
    // Arrange + Act + Assert
    expect(cwdSlug("/Users/x/.config/y")).toBe("-Users-x--config-y");
  });

  it("replaces an UNDERSCORE too, which the macOS temp-folder path depends on", () => {
    // Arrange + Act + Assert. `_m` becomes `-m`, so the vendor directory for a
    // /private/var/folders session carries a dash where the underscore was; a
    // reader that only replaced / and . would look in a folder that never exists.
    expect(cwdSlug("/private/var/folders/_m/x")).toBe("-private-var-folders--m-x");
  });

  it("preserves case and leaves existing dashes alone", () => {
    // Arrange + Act + Assert
    expect(cwdSlug("/Users/DWC/doom-worktrees/Feature")).toBe("-Users-DWC-doom-worktrees-Feature");
  });
});

describe("paths", () => {
  it("names the session transcript under projects/<slug>/", () => {
    // Arrange + Act + Assert
    expect(transcriptPath("/cfg", "/w/s", "sess-1")).toBe("/cfg/projects/-w-s/sess-1.jsonl");
  });

  it("names a subagent transcript under the session's own directory", () => {
    // Arrange + Act + Assert
    expect(subagentTranscriptPath("/cfg", "/w/s", "sess-1", "a1b2")).toBe(
      "/cfg/projects/-w-s/sess-1/subagents/agent-a1b2.jsonl",
    );
  });

  it("names a subagent meta sidecar beside its transcript", () => {
    // Arrange + Act + Assert
    expect(subagentMetaPath("/cfg", "/w/s", "sess-1", "a1b2")).toBe(
      "/cfg/projects/-w-s/sess-1/subagents/agent-a1b2.meta.json",
    );
  });

  it("names a task spool under <spool-root>/<slug>/<session>/tasks/", () => {
    // Arrange + Act + Assert
    expect(spoolPath("/tmp/claude-501", "/w/s", "sess-1", "bdeadbeef")).toBe(
      "/tmp/claude-501/-w-s/sess-1/tasks/bdeadbeef.output",
    );
  });
});

describe("TranscriptWriter", () => {
  const envelope = { cwd: "/w/s", sessionId: "sess-1", gitBranch: "main" };

  it("stamps the shared envelope on every record", () => {
    // Arrange
    const path = join(temp(), "t.jsonl");
    const writer = new TranscriptWriter(path, envelope);

    // Act
    writer.append({ type: "user", uuid: "u1", timestamp: "2026-08-29T00:00:00.000Z" });

    // Assert
    expect(lines(path)[0]).toEqual({
      parentUuid: null,
      isSidechain: false,
      type: "user",
      uuid: "u1",
      timestamp: "2026-08-29T00:00:00.000Z",
      userType: "external",
      entrypoint: "sdk-cli",
      cwd: "/w/s",
      sessionId: "sess-1",
      version: FAKE_CLI_VERSION,
      gitBranch: "main",
    });
  });

  it("chains each record's parentUuid to its predecessor's uuid", () => {
    // Arrange
    const path = join(temp(), "t.jsonl");
    const writer = new TranscriptWriter(path, envelope);

    // Act
    writer.append({ type: "user", uuid: "u1", timestamp: "t1" });
    writer.append({ type: "assistant", uuid: "u2", timestamp: "t2" });

    // Assert
    expect(lines(path).map((l) => [l.uuid, l.parentUuid])).toEqual([
      ["u1", null],
      ["u2", "u1"],
    ]);
  });

  it("writes an unchained metadata line with no parentUuid at all", () => {
    // Arrange. `queue-operation` carries no uuid in the corpus, so inventing a
    // parentUuid for it would be a shape no real transcript contains.
    const path = join(temp(), "t.jsonl");
    const writer = new TranscriptWriter(path, envelope);

    // Act
    writer.appendUnchained({ type: "queue-operation", operation: "enqueue", timestamp: "t0" });

    // Assert
    expect(lines(path)[0]).toEqual({
      type: "queue-operation",
      operation: "enqueue",
      timestamp: "t0",
      sessionId: "sess-1",
    });
  });

  it("does not let an unchained line become the next record's parent", () => {
    // Arrange
    const path = join(temp(), "t.jsonl");
    const writer = new TranscriptWriter(path, envelope);
    writer.append({ type: "user", uuid: "u1", timestamp: "t1" });

    // Act
    writer.appendUnchained({ type: "mode", mode: "normal" });
    writer.append({ type: "assistant", uuid: "u2", timestamp: "t2" });

    // Assert
    expect(lines(path)[2]?.parentUuid).toBe("u1");
  });

  it("adopts the existing chain head when reopening a transcript (resume)", () => {
    // Arrange
    const path = join(temp(), "t.jsonl");
    const first = new TranscriptWriter(path, envelope);
    first.append({ type: "user", uuid: "u1", timestamp: "t1" });

    // Act
    const resumed = new TranscriptWriter(path, envelope);
    resumed.append({ type: "user", uuid: "u2", timestamp: "t2" });

    // Assert
    expect(lines(path)[1]?.parentUuid).toBe("u1");
  });
});

describe("lastChainedUuid", () => {
  it("answers null for a file that does not exist", () => {
    // Arrange + Act + Assert
    expect(lastChainedUuid(join(temp(), "absent.jsonl"))).toBeNull();
  });

  it("skips a trailing uuid-less metadata line to find the real head", () => {
    // Arrange
    const path = join(temp(), "t.jsonl");
    writeFileSync(
      path,
      `${JSON.stringify({ type: "user", uuid: "u1" })}\n${JSON.stringify({ type: "ai-title", aiTitle: "x" })}\n`,
    );

    // Act + Assert
    expect(lastChainedUuid(path)).toBe("u1");
  });

  it("skips a half-written trailing line rather than raising", () => {
    // Arrange. A killed CLI leaves exactly this; the vendor's reader skips it.
    const path = join(temp(), "t.jsonl");
    writeFileSync(path, `${JSON.stringify({ type: "user", uuid: "u1" })}\n{"type":"assi`);

    // Act + Assert
    expect(lastChainedUuid(path)).toBe("u1");
  });
});

describe("SubagentWriter", () => {
  const envelope = { cwd: "/w/s", sessionId: "sess-1", gitBranch: "main" };

  it("writes the four-field camelCase meta sidecar the corpus carries", () => {
    // Arrange
    const dir = temp();
    const writer = new SubagentWriter(join(dir, "a.jsonl"), join(dir, "a.meta.json"), envelope, "a1");

    // Act
    writer.writeMeta({ agentType: "general-purpose", description: "d", toolUseId: "toolu_1", spawnDepth: 1 });

    // Assert
    expect(JSON.parse(readFileSync(join(dir, "a.meta.json"), "utf8"))).toEqual({
      agentType: "general-purpose",
      description: "d",
      toolUseId: "toolu_1",
      spawnDepth: 1,
    });
  });

  it("marks every record isSidechain with the agent's id", () => {
    // Arrange
    const dir = temp();
    const writer = new SubagentWriter(join(dir, "a.jsonl"), join(dir, "a.meta.json"), envelope, "a1");

    // Act
    writer.append({ type: "user", uuid: "u1", timestamp: "t1" });

    // Assert
    expect(lines(join(dir, "a.jsonl"))[0]).toMatchObject({
      isSidechain: true,
      agentId: "a1",
      parentUuid: null,
    });
  });

  it("keeps its own chain, independent of the session transcript's", () => {
    // Arrange
    const dir = temp();
    const writer = new SubagentWriter(join(dir, "a.jsonl"), join(dir, "a.meta.json"), envelope, "a1");

    // Act
    writer.append({ type: "user", uuid: "s1", timestamp: "t1" });
    writer.append({ type: "assistant", uuid: "s2", timestamp: "t2" });

    // Assert
    expect(lines(join(dir, "a.jsonl")).map((l) => l.parentUuid)).toEqual([null, "s1"]);
  });
});

describe("SpoolWriter", () => {
  it("creates the file empty so a tailer can attach before any output", () => {
    // Arrange + Act
    const path = join(temp(), "b1.output");
    new SpoolWriter(path);

    // Assert
    expect({ exists: existsSync(path), body: readFileSync(path, "utf8") }).toEqual({ exists: true, body: "" });
  });

  it("appends incrementally so each chunk is a separate growth event", () => {
    // Arrange
    const path = join(temp(), "b1.output");
    const spool = new SpoolWriter(path);

    // Act
    spool.appendLine("one");
    spool.appendLine("two");

    // Assert
    expect(readFileSync(path, "utf8")).toBe("one\ntwo\n");
  });

  it("terminates with the vendor's EXIT= line", () => {
    // Arrange
    const path = join(temp(), "b1.output");
    const spool = new SpoolWriter(path);
    spool.appendLine("out");

    // Act
    spool.finish(3);

    // Assert
    expect(readFileSync(path, "utf8")).toBe("out\nEXIT=3\n");
  });

  it("leaves no terminator when the run never ends, as a killed run does", () => {
    // Arrange
    const path = join(temp(), "b1.output");
    const spool = new SpoolWriter(path);

    // Act
    spool.appendLine("partial");

    // Assert
    expect({ body: readFileSync(path, "utf8"), finished: spool.isFinished }).toEqual({
      body: "partial\n",
      finished: false,
    });
  });

  it("refuses an append after its EXIT line rather than corrupting the spool", () => {
    // Arrange
    const spool = new SpoolWriter(join(temp(), "b1.output"));
    spool.finish(0);

    // Act + Assert
    expect(() => spool.appendLine("late")).toThrow(/appended to after its EXIT line/);
  });

  it("refuses a second finish rather than writing two EXIT lines", () => {
    // Arrange
    const spool = new SpoolWriter(join(temp(), "b1.output"));
    spool.finish(0);

    // Act + Assert
    expect(() => spool.finish(0)).toThrow(/finished twice/);
  });
});

describe("VendorFiles", () => {
  const config = (dir: string) => ({
    configDir: join(dir, "cfg"),
    cwd: "/w/s",
    spoolRoot: join(dir, "spools"),
    sessionId: "sess-1",
    gitBranch: "main",
  });

  it("opens the transcript at the vendor's path for the session in force", () => {
    // Arrange
    const dir = temp();

    // Act
    const files = new VendorFiles(config(dir));

    // Assert
    expect(files.transcript.path).toBe(join(dir, "cfg", "projects", "-w-s", "sess-1.jsonl"));
  });

  it("starts a NEW transcript on rotation and leaves the old file intact", () => {
    // Arrange
    const dir = temp();
    const files = new VendorFiles(config(dir));
    const oldPath = files.transcript.path;
    files.transcript.append({ type: "user", uuid: "u1", timestamp: "t1" });

    // Act
    files.rotate("sess-2");
    files.transcript.append({ type: "user", uuid: "u2", timestamp: "t2" });

    // Assert
    expect({
      old: lines(oldPath).map((l) => l.uuid),
      fresh: lines(files.transcript.path).map((l) => [l.uuid, l.parentUuid, l.sessionId]),
    }).toEqual({ old: ["u1"], fresh: [["u2", null, "sess-2"]] });
  });

  it("addresses a subagent's files by the session id in force", () => {
    // Arrange
    const dir = temp();
    const files = new VendorFiles(config(dir));
    files.rotate("sess-2");

    // Act
    const writer = files.subagent("a1");

    // Assert
    expect(writer.path).toBe(
      join(dir, "cfg", "projects", "-w-s", "sess-2", "subagents", "agent-a1.jsonl"),
    );
  });

  it("hands back the same subagent writer for one agent id", () => {
    // Arrange
    const files = new VendorFiles(config(temp()));

    // Act + Assert
    expect(files.subagent("a1")).toBe(files.subagent("a1"));
  });

  it("hands back the same spool writer for one task id", () => {
    // Arrange
    const files = new VendorFiles(config(temp()));

    // Act + Assert
    expect(files.spool("b1")).toBe(files.spool("b1"));
  });

  it("reports every spool it opened and never terminated", () => {
    // Arrange
    const files = new VendorFiles(config(temp()));
    files.spool("b1");
    files.spool("b2").finish(0);

    // Act + Assert
    expect(files.unfinishedSpools()).toEqual(["b1"]);
  });

  it("reports the vendor session id it is currently addressed by", () => {
    // Arrange
    const files = new VendorFiles(config(temp()));

    // Act + Assert
    expect(files.vendorSessionId).toBe("sess-1");
  });

  it("reports the ROTATED session id after rotate()", () => {
    // Arrange
    const files = new VendorFiles(config(temp()));

    // Act
    files.rotate("sess-2");

    // Assert
    expect(files.vendorSessionId).toBe("sess-2");
  });

});
