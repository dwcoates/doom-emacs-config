/**
 * The topbar's account label.
 *
 * With two accounts in play, "which account am I about to spend tokens as"
 * must be readable at a glance rather than inferred from a workspace path.
 */
import { describe, expect, it } from "vitest";

import {
  ACCOUNT_LOGGED_OUT,
  accountIsLoggedOut,
  accountLabel,
  fetchAccount,
} from "../src/account.js";

/** A fetch that answers GET /sessions/{id}/account. */
function fakeFetch(resp: { ok: boolean; status?: number; body?: unknown }): {
  fetchFn: typeof fetch;
  calls: string[];
} {
  const calls: string[] = [];
  const fetchFn = (async (url: string) => {
    calls.push(url);
    return {
      ok: resp.ok,
      status: resp.status ?? 200,
      json: async () => resp.body ?? { config_dir: "", email: "" },
      text: async () => "boom",
    };
  }) as unknown as typeof fetch;
  return { fetchFn, calls };
}

describe("fetchAccount", () => {
  it("gets the session's own account route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true });
    // Act
    await fetchAccount("http://d", "s_1", fetchFn);
    // Assert
    expect(calls[0]).toBe("http://d/sessions/s_1/account");
  });

  it("reports the logged-in email", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({
      ok: true,
      body: { config_dir: "/root/.claude-chesscom", email: "dodge@chess.com" },
    });
    // Act
    const account = await fetchAccount("http://d", "s_1", fetchFn);
    // Assert
    expect(account.email).toBe("dodge@chess.com");
  });

  it("reports a logged-out root without failing", async () => {
    // Arrange — logged out is a state to render, not an error to throw.
    const { fetchFn } = fakeFetch({
      ok: true,
      body: { config_dir: "/root/.claude", email: "" },
    });
    // Act
    const account = await fetchAccount("http://d", "s_1", fetchFn);
    // Assert
    expect(account.email).toBe("");
  });

  it("rejects when the daemon does not know the session", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: false, status: 404 });
    // Act / Assert
    await expect(fetchAccount("http://d", "s_gone", fetchFn)).rejects.toThrow(/404/);
  });
});

describe("accountLabel", () => {
  it("names the logged-in email", () => {
    // Arrange / Act / Assert
    expect(accountLabel({ config_dir: "/x", email: "dodge@chess.com" })).toBe(
      "dodge@chess.com",
    );
  });

  it("says so when the root has no login", () => {
    // Arrange — a blank slot reads as "still loading", which is the one thing
    // a logged-out account is not.
    // Act / Assert
    expect(accountLabel({ config_dir: "/x", email: "" })).toBe(ACCOUNT_LOGGED_OUT);
  });

  it("renders nothing before the account is known", () => {
    // Arrange / Act / Assert
    expect(accountLabel(null)).toBe("");
  });
});

describe("accountIsLoggedOut", () => {
  it("flags a root with no login", () => {
    // Arrange / Act / Assert
    expect(accountIsLoggedOut({ config_dir: "/x", email: "" })).toBe(true);
  });

  it("does not flag a logged-in root", () => {
    // Arrange / Act / Assert
    expect(accountIsLoggedOut({ config_dir: "/x", email: "a@b.c" })).toBe(false);
  });

  it("does not flag an account that is merely unknown yet", () => {
    // Arrange — before the fetch lands there is nothing to warn about.
    // Act / Assert
    expect(accountIsLoggedOut(null)).toBe(false);
  });
});
