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
  accountMenuEntries,
  accountModeLabel,
  fetchAccount,
  fetchAccountMenuEntries,
  fetchAccounts,
  switchAccount,
  type RosterEntry,
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

/** A fetch that also records each call's RequestInit (method, body). */
function fakeFetchWithInit(resp: { ok: boolean; status?: number; body?: unknown }): {
  fetchFn: typeof fetch;
  calls: { url: string; init?: RequestInit }[];
} {
  const calls: { url: string; init?: RequestInit }[] = [];
  const fetchFn = (async (url: string, init?: RequestInit) => {
    calls.push({ url, init });
    return {
      ok: resp.ok,
      status: resp.status ?? 200,
      json: async () => resp.body ?? {},
      text: async () => "boom",
    };
  }) as unknown as typeof fetch;
  return { fetchFn, calls };
}

describe("fetchAccounts", () => {
  it("gets the roster route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true, body: { accounts: [] } });
    // Act
    await fetchAccounts("http://d", fetchFn);
    // Assert
    expect(calls[0]).toBe("http://d/accounts");
  });

  it("hands back the roster entries", async () => {
    // Arrange
    const roster = [
      { label: "personal", config_dir: "", email: "a@b.c" },
      { label: "work", config_dir: "/w", email: "d@e.f" },
    ];
    const { fetchFn } = fakeFetch({ ok: true, body: { accounts: roster } });
    // Act
    const got = await fetchAccounts("http://d", fetchFn);
    // Assert
    expect(got).toEqual(roster);
  });

  it("rejects when the daemon has no roster configured", async () => {
    // Arrange — the 503 of a daemon started without -accounts.
    const { fetchFn } = fakeFetch({ ok: false, status: 503 });
    // Act / Assert
    await expect(fetchAccounts("http://d", fetchFn)).rejects.toThrow(/503/);
  });
});

describe("switchAccount", () => {
  it("posts the target root to the session's account route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetchWithInit({
      ok: true,
      body: { switched: true, account: { label: "work", config_dir: "/w", email: "d@e.f" } },
    });
    // Act
    await switchAccount("http://d", "s_1", "/w", fetchFn);
    // Assert
    expect(calls[0].url).toBe("http://d/sessions/s_1/account");
    expect(calls[0].init?.method).toBe("POST");
    expect(JSON.parse(calls[0].init?.body as string)).toEqual({ config_dir: "/w" });
  });

  it("hands back the switch outcome", async () => {
    // Arrange
    const outcome = {
      switched: true,
      account: { label: "work", config_dir: "/w", email: "d@e.f" },
    };
    const { fetchFn } = fakeFetchWithInit({ ok: true, body: outcome });
    // Act
    const got = await switchAccount("http://d", "s_1", "/w", fetchFn);
    // Assert
    expect(got).toEqual(outcome);
  });

  it("rejects a mid-turn refusal rather than reporting it as switched", async () => {
    // Arrange — the daemon 409s while a turn is in flight.
    const { fetchFn } = fakeFetchWithInit({ ok: false, status: 409 });
    // Act / Assert
    await expect(switchAccount("http://d", "s_1", "/w", fetchFn)).rejects.toThrow(/409/);
  });
});

describe("accountModeLabel", () => {
  const personal: RosterEntry = { label: "personal", config_dir: "", email: "a@b.c" };
  const work: RosterEntry = { label: "work", config_dir: "/w", email: "d@e.f" };

  it("names the roster label for the root the current account runs as", () => {
    // Arrange / Act / Assert
    expect(accountModeLabel({ config_dir: "/w", email: "d@e.f" }, [personal, work])).toBe("work");
  });

  it("names personal for the CLI default empty root", () => {
    // Arrange / Act / Assert
    expect(accountModeLabel({ config_dir: "", email: "a@b.c" }, [personal, work])).toBe("personal");
  });

  it("returns empty for a config root the roster does not name", () => {
    // Arrange — an off-roster account has no label to show.
    // Act / Assert
    expect(accountModeLabel({ config_dir: "/gone", email: "a@b.c" }, [personal, work])).toBe("");
  });

  it("returns empty before the account is known", () => {
    // Arrange / Act / Assert
    expect(accountModeLabel(null, [personal, work])).toBe("");
  });
});

describe("accountMenuEntries", () => {
  const personal: RosterEntry = { label: "personal", config_dir: "", email: "a@b.c" };
  const work: RosterEntry = { label: "work", config_dir: "/w", email: "d@e.f" };

  it("leads with a re-auth entry naming the current email and root", () => {
    // Arrange / Act
    const entries = accountMenuEntries({ config_dir: "", email: "a@b.c" }, [personal, work]);
    // Assert
    expect(entries[0]).toEqual({ kind: "reauth", text: "re-auth a@b.c (personal)" });
  });

  it("omits the root suffix when the current root is off the roster", () => {
    // Arrange — an account whose config_dir no roster entry names has no label
    // to show, so re-auth falls back to the bare email.
    const entries = accountMenuEntries({ config_dir: "/gone", email: "a@b.c" }, [personal, work]);
    // Assert
    expect(entries[0]).toEqual({ kind: "reauth", text: "re-auth a@b.c" });
  });

  it("says log in when the current root is logged out", () => {
    // Arrange — "re-auth" of nothing is not a verb the user can follow.
    // Act
    const entries = accountMenuEntries({ config_dir: "", email: "" }, []);
    // Assert
    expect(entries[0]).toEqual({ kind: "reauth", text: "log in" });
  });

  it("says log in before the account is known", () => {
    // Arrange / Act
    const entries = accountMenuEntries(null, []);
    // Assert
    expect(entries[0]).toEqual({ kind: "reauth", text: "log in" });
  });

  it("offers a switch entry naming each other root", () => {
    // Arrange / Act
    const entries = accountMenuEntries({ config_dir: "", email: "a@b.c" }, [personal, work]);
    // Assert
    expect(entries[1]).toEqual({
      kind: "switch",
      text: "switch to work (d@e.f)",
      configDir: "/w",
    });
  });

  it("excludes the root the session already runs as", () => {
    // Arrange / Act
    const entries = accountMenuEntries({ config_dir: "/w", email: "d@e.f" }, [personal, work]);
    // Assert
    expect(entries.filter((e) => e.kind === "switch")).toHaveLength(1);
    expect(entries[1]).toMatchObject({ configDir: "" });
  });

  it("annotates a logged-out switch target", () => {
    // Arrange — switching to a dead root flows into re-auth, so the menu
    // names the state up front rather than springing it after the bounce.
    const out: RosterEntry = { label: "work", config_dir: "/w", email: "" };
    // Act
    const entries = accountMenuEntries({ config_dir: "", email: "a@b.c" }, [out]);
    // Assert
    expect(entries[1]).toMatchObject({ text: `switch to work (${ACCOUNT_LOGGED_OUT})` });
  });

  it("annotates an unreadable root instead of hiding it", () => {
    // Arrange — a corrupt .claude.json is surfaced, never blanked into a
    // fake logged-out.
    const broken: RosterEntry = { label: "work", config_dir: "/w", email: "", error: "corrupt" };
    // Act
    const entries = accountMenuEntries({ config_dir: "", email: "a@b.c" }, [broken]);
    // Assert
    expect(entries[1]).toMatchObject({ text: "switch to work (unreadable)" });
  });
});

describe("fetchAccountMenuEntries", () => {
  const personal: RosterEntry = { label: "personal", config_dir: "", email: "a@b.c" };
  const work: RosterEntry = { label: "work", config_dir: "/w", email: "d@e.f" };

  /** A fetch routing /account and /accounts to distinct bodies. */
  function router(
    current: unknown,
    roster: unknown,
  ): { fetchFn: typeof fetch; calls: string[] } {
    const calls: string[] = [];
    const fetchFn = (async (url: string) => {
      calls.push(url);
      const body = url.endsWith("/accounts") ? { accounts: roster } : current;
      return { ok: true, status: 200, json: async () => body, text: async () => "" };
    }) as unknown as typeof fetch;
    return { fetchFn, calls };
  }

  it("offers switch-to-personal once the live account reads as work", async () => {
    // Arrange — the regression: a session that just moved to work must be
    // offered personal, never re-offered the work it already runs as.
    const { fetchFn } = router({ config_dir: "/w", email: "d@e.f" }, [personal, work]);
    // Act
    const { entries } = await fetchAccountMenuEntries("http://d", "s_1", fetchFn);
    // Assert
    const switches = entries.filter((e) => e.kind === "switch");
    expect(switches).toHaveLength(1);
    expect(switches[0]).toMatchObject({ configDir: "" });
  });

  it("offers switch-to-work when the live account reads as personal", async () => {
    // Arrange
    const { fetchFn } = router({ config_dir: "", email: "a@b.c" }, [personal, work]);
    // Act
    const { entries } = await fetchAccountMenuEntries("http://d", "s_1", fetchFn);
    // Assert
    const switches = entries.filter((e) => e.kind === "switch");
    expect(switches).toHaveLength(1);
    expect(switches[0]).toMatchObject({ configDir: "/w" });
  });

  it("reads the current account live rather than trusting a cached one", async () => {
    // Arrange — it must hit the per-session account route, not the roster alone.
    const { fetchFn, calls } = router({ config_dir: "/w", email: "d@e.f" }, [personal, work]);
    // Act
    await fetchAccountMenuEntries("http://d", "s_1", fetchFn);
    // Assert
    expect(calls).toContain("http://d/sessions/s_1/account");
  });

  it("also reads the roster live", async () => {
    // Arrange
    const { fetchFn, calls } = router({ config_dir: "/w", email: "d@e.f" }, [personal, work]);
    // Act
    await fetchAccountMenuEntries("http://d", "s_1", fetchFn);
    // Assert
    expect(calls).toContain("http://d/accounts");
  });

  it("hands back the freshly-read current account", async () => {
    // Arrange
    const { fetchFn } = router({ config_dir: "/w", email: "d@e.f" }, [personal, work]);
    // Act
    const { current } = await fetchAccountMenuEntries("http://d", "s_1", fetchFn);
    // Assert
    expect(current).toEqual({ config_dir: "/w", email: "d@e.f" });
  });

  it("hands back the freshly-read roster for the chip's mode label", async () => {
    // Arrange
    const { fetchFn } = router({ config_dir: "/w", email: "d@e.f" }, [personal, work]);
    // Act
    const { roster } = await fetchAccountMenuEntries("http://d", "s_1", fetchFn);
    // Assert
    expect(roster).toEqual([personal, work]);
  });

  it("rejects when the account read fails", async () => {
    // Arrange — a menu on a half-stale read is worse than a caller fallback.
    const fetchFn = (async (url: string) => {
      const isAccount = url.endsWith("/sessions/s_1/account");
      return {
        ok: !isAccount,
        status: isAccount ? 404 : 200,
        json: async () => ({ accounts: [] }),
        text: async () => "boom",
      };
    }) as unknown as typeof fetch;
    // Act / Assert
    await expect(fetchAccountMenuEntries("http://d", "s_1", fetchFn)).rejects.toThrow(/404/);
  });

  it("rejects when the roster read fails", async () => {
    // Arrange
    const fetchFn = (async (url: string) => {
      const isRoster = url.endsWith("/accounts");
      return {
        ok: !isRoster,
        status: isRoster ? 503 : 200,
        json: async () => ({ config_dir: "", email: "a@b.c" }),
        text: async () => "boom",
      };
    }) as unknown as typeof fetch;
    // Act / Assert
    await expect(fetchAccountMenuEntries("http://d", "s_1", fetchFn)).rejects.toThrow(/503/);
  });
});
