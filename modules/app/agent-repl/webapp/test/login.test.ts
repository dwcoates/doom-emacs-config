/**
 * Login request contract: the topbar button asks the daemon to open the
 * interactive Claude login in Emacs, and a request that never landed must
 * be surfaced rather than read as success.
 */
import { describe, expect, it } from "vitest";

import { LOGIN_FAILED, LOGIN_OPENING, loginNotice, requestLogin } from "../src/login.js";

/** A fetch that answers POST /sessions/{id}/login with the given status. */
function fakeFetch(resp: { ok: boolean; status?: number }): {
  fetchFn: typeof fetch;
  calls: Array<{ url: string; method: string | undefined }>;
} {
  const calls: Array<{ url: string; method: string | undefined }> = [];
  const fetchFn = (async (url: string, init: RequestInit) => {
    calls.push({ url, method: init.method });
    return {
      ok: resp.ok,
      status: resp.status ?? 202,
      json: async () => ({ requested: true }),
      text: async () => "boom",
    };
  }) as unknown as typeof fetch;
  return { fetchFn, calls };
}

describe("requestLogin", () => {
  it("posts to the session's own login route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true });
    // Act
    await requestLogin("http://d", "s_1", fetchFn);
    // Assert
    expect(calls[0].url).toBe("http://d/sessions/s_1/login");
  });

  it("uses POST", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true });
    // Act
    await requestLogin("http://d", "s_1", fetchFn);
    // Assert
    expect(calls[0].method).toBe("POST");
  });

  it("resolves to the opening phase when the daemon accepted the request", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: true, status: 202 });
    // Act
    const phase = await requestLogin("http://d", "s_1", fetchFn);
    // Assert
    expect(phase).toBe("opening");
  });

  it("rejects when the daemon has no channel to Emacs", async () => {
    // Arrange — 503: the sentinel sink is unconfigured, so no login can open.
    const { fetchFn } = fakeFetch({ ok: false, status: 503 });
    // Act / Assert
    await expect(requestLogin("http://d", "s_1", fetchFn)).rejects.toThrow(/503/);
  });

  it("rejects when the daemon does not know the session", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: false, status: 404 });
    // Act / Assert
    await expect(requestLogin("http://d", "s_gone", fetchFn)).rejects.toThrow(/404/);
  });
});

describe("loginNotice", () => {
  it("names the opening phase", () => {
    // Arrange / Act / Assert
    expect(loginNotice("opening")).toBe(LOGIN_OPENING);
  });

  it("names the failed phase", () => {
    // Arrange / Act / Assert
    expect(loginNotice("failed")).toBe(LOGIN_FAILED);
  });
});
