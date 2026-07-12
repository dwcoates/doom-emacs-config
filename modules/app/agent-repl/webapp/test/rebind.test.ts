import { describe, expect, it } from "vitest";
import { rebindSession, recallResumeKeys, rememberResumeKeys } from "../src/rebind.js";

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

/** fetch fake capturing the request and returning a canned response. */
function fakeFetch(
  status: number,
  body: unknown,
): { fetchFn: typeof fetch; calls: Array<{ url: string; init?: RequestInit }> } {
  const calls: Array<{ url: string; init?: RequestInit }> = [];
  const fetchFn = ((url: string, init?: RequestInit) => {
    calls.push({ url, init });
    return Promise.resolve({
      ok: status >= 200 && status < 300,
      status,
      json: () => Promise.resolve(body),
      text: () => Promise.resolve(JSON.stringify(body)),
    } as Response);
  }) as typeof fetch;
  return { fetchFn, calls };
}

describe("resume key persistence", () => {
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

  it("recalls null for a session id that was never stored", () => {
    // Arrange
    const storage = new FakeStorage();
    // Act / Assert
    expect(recallResumeKeys(storage, "s_unknown")).toBeNull();
  });

  it("recalls null for a corrupt record", () => {
    // Arrange
    const storage = new FakeStorage();
    storage.setItem("agent-repl.resume.s_1", "{not json");
    // Act / Assert
    expect(recallResumeKeys(storage, "s_1")).toBeNull();
  });

  it("recalls null for a record missing claudeSessionId", () => {
    // Arrange
    const storage = new FakeStorage();
    storage.setItem("agent-repl.resume.s_1", JSON.stringify({ cwd: "/w" }));
    // Act / Assert
    expect(recallResumeKeys(storage, "s_1")).toBeNull();
  });
});

describe("rebindSession", () => {
  it("trades the stored keys for a successor session id", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { fetchFn, calls } = fakeFetch(201, { session_id: "s_new" });
    // Act
    const next = await rebindSession("http://d", "s_old", storage, fetchFn);
    // Assert
    expect(next).toBe("s_new");
    expect(calls).toHaveLength(1);
    expect(calls[0].url).toBe("http://d/sessions");
    expect(JSON.parse(String(calls[0].init?.body))).toEqual({ cwd: "/w", resume: "uuid-1" });
  });

  it("migrates the resume keys onto the successor id", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { fetchFn } = fakeFetch(201, { session_id: "s_new" });
    // Act
    await rebindSession("http://d", "s_old", storage, fetchFn);
    // Assert — a SECOND loss can rebind too; the old key is gone.
    expect(recallResumeKeys(storage, "s_new")).toEqual({ claudeSessionId: "uuid-1", cwd: "/w" });
    expect(recallResumeKeys(storage, "s_old")).toBeNull();
  });

  it("returns null without fetching when nothing was ever stored", async () => {
    // Arrange
    const storage = new FakeStorage();
    const { fetchFn, calls } = fakeFetch(201, { session_id: "s_new" });
    // Act
    const next = await rebindSession("http://d", "s_old", storage, fetchFn);
    // Assert — the caller escalates to remediation instead.
    expect(next).toBeNull();
    expect(calls).toHaveLength(0);
  });

  it("throws on a failed POST rather than reading it as rebound", async () => {
    // Arrange
    const storage = new FakeStorage();
    rememberResumeKeys(storage, "s_old", { claudeSessionId: "uuid-1", cwd: "/w" });
    const { fetchFn } = fakeFetch(500, { error: "boom" });
    // Act / Assert
    await expect(rebindSession("http://d", "s_old", storage, fetchFn)).rejects.toThrow("500");
  });
});
