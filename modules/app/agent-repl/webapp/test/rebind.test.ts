import { afterEach, describe, expect, it, vi } from "vitest";
import { rebindSession, recallResumeKeys, rememberResumeKeys } from "../src/rebind.js";
import { ForwardingLogger, resetLoggingForTests, setLogger } from "../src/wslog.js";
import { ClientLogCmd } from "../src/protocol.js";

/** Minimal in-memory Storage for the localStorage seam. */
class FakeStorage implements Storage {
  private items = new Map<string, string>();

  get length(): number {
    return this.items.size;
  }

  clear(): void {
    this.items.clear();
  }

  getItem(key: string): string | null {
    return this.items.get(key) ?? null;
  }

  key(index: number): string | null {
    return [...this.items.keys()][index] ?? null;
  }

  removeItem(key: string): void {
    this.items.delete(key);
  }

  setItem(key: string, value: string): void {
    this.items.set(key, value);
  }
}

/** A logger wired to spies: captured forwards, captured console lines. */
function spyLogger(): { logger: ForwardingLogger; forwarded: ClientLogCmd[]; consoleLines: string[] } {
  const forwarded: ClientLogCmd[] = [];
  const consoleLines: string[] = [];
  const logger = new ForwardingLogger(
    (cmd) => {
      forwarded.push(cmd);
      return true;
    },
    (level, line) => consoleLines.push(`${level}: ${line}`),
    () => 0,
  );
  return { logger, forwarded, consoleLines };
}

describe("resume key persistence", () => {
  afterEach(() => {
    resetLoggingForTests();
    vi.restoreAllMocks();
  });


  it("round-trips resume keys per session id", () => {
    // Arrange
    const storage = new FakeStorage();
    // Act
    rememberResumeKeys(storage, "s_1", { claudeSessionId: "uuid-1", cwd: "/w" });
    // Assert
    expect(recallResumeKeys(storage, "s_1")).toEqual({ claudeSessionId: "uuid-1", cwd: "/w" });
  });

  it("skips a pre-init hello so it cannot clobber a stored record", () => {
    // Arrange — a filled record, then a hello with no durable id yet.
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_1", { claudeSessionId: "uuid-1", cwd: "/w" });
    // Act
    rememberResumeKeys(storage, "s_1", { claudeSessionId: "", cwd: "/w" });
    // Assert
    expect(recallResumeKeys(storage, "s_1")).toEqual({ claudeSessionId: "uuid-1", cwd: "/w" });
  });

  it("recalls null for a session id that was never stored, logging an info breadcrumb", () => {
    // Arrange
    const storage = new FakeStorage();
    const spy = spyLogger();
    setLogger(spy.logger);
    // Act
    const result = recallResumeKeys(storage, "s_unknown");
    // Assert
    expect(result).toBeNull();
    expect(spy.consoleLines).toHaveLength(1);
    expect(spy.consoleLines[0]).toContain("info:");
    expect(spy.consoleLines[0]).toContain("s_unknown");
  });

  it("recalls null for a corrupt record, logging an error naming the session id", () => {
    // Arrange
    const storage = new FakeStorage();
    storage.setItem("agent-repl.resume.s_1", "{not json");
    const spy = spyLogger();
    setLogger(spy.logger);
    // Act
    const result = recallResumeKeys(storage, "s_1");
    // Assert
    expect(result).toBeNull();
    expect(spy.consoleLines).toHaveLength(1);
    expect(spy.consoleLines[0]).toContain("error:");
    expect(spy.consoleLines[0]).toContain("s_1");
    expect(spy.consoleLines[0]).toContain("corrupt");
    expect(spy.consoleLines[0]).toContain("remediation");
  });

  it("recalls null for a record missing claudeSessionId, logging an error naming the session id", () => {
    // Arrange
    const storage = new FakeStorage();
    storage.setItem("agent-repl.resume.s_1", JSON.stringify({ cwd: "/w" }));
    const spy = spyLogger();
    setLogger(spy.logger);
    // Act
    const result = recallResumeKeys(storage, "s_1");
    // Assert
    expect(result).toBeNull();
    expect(spy.consoleLines).toHaveLength(1);
    expect(spy.consoleLines[0]).toContain("error:");
    expect(spy.consoleLines[0]).toContain("s_1");
    expect(spy.consoleLines[0]).toContain("claudeSessionId");
    expect(spy.consoleLines[0]).toContain("remediation");
  });
});

describe("rebindSession", () => {
  /** A session creator recording the args it was asked to create with. */
  function fakeCreator(result: string | Error): {
    create: (args: { cwd: string; resumeClaudeSessionId: string }) => Promise<string>;
    calls: Array<{ cwd: string; resumeClaudeSessionId: string }>;
  } {
    const calls: Array<{ cwd: string; resumeClaudeSessionId: string }> = [];
    const create = async (args: { cwd: string; resumeClaudeSessionId: string }) => {
      calls.push(args);
      if (result instanceof Error) throw result;
      return result;
    };
    return { create, calls };
  }

  it("trades the stored keys for a successor session id", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { create } = fakeCreator("s_new");
    // Act
    const next = await rebindSession("s_old", storage, create);
    // Assert
    expect(next).toBe("s_new");
  });

  it("creates the successor with the stored resume keys", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { create, calls } = fakeCreator("s_new");
    // Act
    await rebindSession("s_old", storage, create);
    // Assert — the CreateSessionCmd resumes the SAME claude session.
    expect(calls).toEqual([{ cwd: "/w", resumeClaudeSessionId: "uuid-1" }]);
  });

  it("migrates the resume keys onto the successor id", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { create } = fakeCreator("s_new");
    // Act
    await rebindSession("s_old", storage, create);
    // Assert — a SECOND loss can rebind too; the old key is gone.
    expect(recallResumeKeys(storage, "s_new")).toEqual({ claudeSessionId: "uuid-1", cwd: "/w" });
    expect(recallResumeKeys(storage, "s_old")).toBeNull();
  });

  it("returns null without creating when nothing was ever stored", async () => {
    // Arrange
    const storage = new FakeStorage();
    const { create, calls } = fakeCreator("s_new");
    // Act
    const next = await rebindSession("s_old", storage, create);
    // Assert — the caller escalates to remediation instead.
    expect(next).toBeNull();
    expect(calls).toHaveLength(0);
  });

  it("rejects on a failed create rather than reading it as rebound", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { create } = fakeCreator(new Error("createSession rejected: no such cwd"));
    // Act / Assert
    await expect(rebindSession("s_old", storage, create)).rejects.toThrow("no such cwd");
  });

  it("leaves the old keys in place when the create failed", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { create } = fakeCreator(new Error("boom"));
    // Act
    await expect(rebindSession("s_old", storage, create)).rejects.toThrow();
    // Assert — a retry can still rebind; the keys were not spent.
    expect(recallResumeKeys(storage, "s_old")).toEqual({ claudeSessionId: "uuid-1", cwd: "/w" });
  });
});
