/**
 * The login terminal's wire contract.
 *
 * The topbar asks the daemon to open the Claude login on a pty it owns, then
 * renders that terminal. A login that never opened must be surfaced rather
 * than read as success, and the frames going over the socket must be exactly
 * what the daemon expects: raw bytes for keystrokes, one JSON frame for
 * geometry.
 *
 * The xterm.js binding itself (login-terminal.ts) is not exercised here: it
 * is a browser bundle that throws on import under the test runner, which is
 * precisely why everything worth asserting on lives in login.ts.
 */
import { describe, expect, it } from "vitest";

import {
  LOGIN_CLOSED,
  LOGIN_FAILED,
  LOGIN_OPEN,
  closeLogin,
  decodeTerminalChunk,
  describeChunkType,
  encodeKeystrokes,
  loginNotice,
  loginTerminalUrl,
  requestLogin,
  resizeFrame,
} from "../src/login.js";

/** A fetch that answers the login routes with the given status. */
function fakeFetch(resp: { ok: boolean; status?: number; body?: unknown }): {
  fetchFn: typeof fetch;
  calls: Array<{ url: string; method: string | undefined }>;
} {
  const calls: Array<{ url: string; method: string | undefined }> = [];
  const fetchFn = (async (url: string, init?: RequestInit) => {
    calls.push({ url, method: init?.method });
    return {
      ok: resp.ok,
      status: resp.status ?? 202,
      json: async () =>
        resp.body ?? { account: "/root/.claude", terminal_url: "/t" },
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

  it("reports the account the terminal was opened against", async () => {
    // Arrange — the account is the config dir, and the user needs to know
    // WHICH one they are about to log into.
    const { fetchFn } = fakeFetch({
      ok: true,
      body: { account: "/root/.claude-chesscom", terminal_url: "/t" },
    });
    // Act
    const opened = await requestLogin("http://d", "s_1", fetchFn);
    // Assert
    expect(opened.account).toBe("/root/.claude-chesscom");
  });

  it("reports the default account as the empty config dir", async () => {
    // Arrange — an empty config dir is a REAL account (the CLI's own root),
    // not a missing one.
    const { fetchFn } = fakeFetch({
      ok: true,
      body: { account: "", terminal_url: "/t" },
    });
    // Act
    const opened = await requestLogin("http://d", "s_1", fetchFn);
    // Assert
    expect(opened.account).toBe("");
  });

  it("rejects when the daemon cannot open a login", async () => {
    // Arrange — 503: no login manager, so no terminal can open.
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

describe("closeLogin", () => {
  it("deletes the session's login route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true, status: 204 });
    // Act
    await closeLogin("http://d", "s_1", fetchFn);
    // Assert
    expect(calls[0].url).toBe("http://d/sessions/s_1/login");
    expect(calls[0].method).toBe("DELETE");
  });

  it("rejects when the close failed", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: false, status: 500 });
    // Act / Assert
    await expect(closeLogin("http://d", "s_1", fetchFn)).rejects.toThrow(/500/);
  });
});

describe("loginTerminalUrl", () => {
  it("names the session's terminal socket", () => {
    // Arrange / Act / Assert
    expect(loginTerminalUrl("ws://d", "s_1")).toBe(
      "ws://d/sessions/s_1/login/terminal",
    );
  });
});

describe("resizeFrame", () => {
  it("carries the geometry the daemon expects", () => {
    // Arrange / Act
    const frame = resizeFrame(40, 180);
    // Assert
    expect(JSON.parse(frame)).toEqual({ resize: { rows: 40, cols: 180 } });
  });

  it("is what keeps the OAuth URL on one line", () => {
    // Arrange — the TUI hard-wraps at the column count and the URL runs ~350
    // characters, so the width is load-bearing rather than cosmetic.
    const frame = resizeFrame(60, 400);
    // Act
    const parsed = JSON.parse(frame) as { resize: { cols: number } };
    // Assert
    expect(parsed.resize.cols).toBe(400);
  });
});

describe("encodeKeystrokes", () => {
  it("sends the pasted code as bytes, not text", () => {
    // Arrange / Act
    const bytes = encodeKeystrokes("code-123\r");
    // Assert
    expect(bytes).toBeInstanceOf(Uint8Array);
    expect(new TextDecoder().decode(bytes)).toBe("code-123\r");
  });
});

describe("decodeTerminalChunk", () => {
  it("renders a binary frame as bytes", () => {
    // Arrange
    const buf = new TextEncoder().encode("Paste code here >").buffer;
    // Act
    const chunk = decodeTerminalChunk(buf);
    // Assert
    expect(chunk).toBeInstanceOf(Uint8Array);
  });

  it("still renders a text frame rather than dropping it", () => {
    // Arrange — dropping screen bytes would leave the user staring at a
    // half-drawn prompt with no hint anything went wrong.
    // Act
    const chunk = decodeTerminalChunk("hello");
    // Assert
    expect(chunk).toBe("hello");
  });

  it("drops a frame it cannot render", () => {
    // Arrange / Act
    const chunk = decodeTerminalChunk({ not: "a frame" });
    // Assert
    expect(chunk).toBeNull();
  });
});

describe("describeChunkType", () => {
  // The caller logs this string when decodeTerminalChunk drops a frame, so
  // it has to actually name what arrived rather than a generic "unknown".
  it("names an object's constructor", () => {
    // Arrange / Act / Assert
    expect(describeChunkType({ not: "a frame" })).toBe("Object");
  });

  it("names a number's constructor", () => {
    // Arrange / Act / Assert
    expect(describeChunkType(7)).toBe("Number");
  });

  it("names a boolean's constructor", () => {
    // Arrange / Act / Assert
    expect(describeChunkType(true)).toBe("Boolean");
  });

  it("names null explicitly rather than throwing", () => {
    // Arrange / Act / Assert — null has no .constructor to read.
    expect(describeChunkType(null)).toBe("null");
  });

  it("names undefined explicitly rather than throwing", () => {
    // Arrange / Act / Assert — undefined has no .constructor to read.
    expect(describeChunkType(undefined)).toBe("undefined");
  });
});

describe("loginNotice", () => {
  it("names the open phase", () => {
    // Arrange / Act / Assert
    expect(loginNotice("open")).toBe(LOGIN_OPEN);
  });

  it("names the closed phase", () => {
    // Arrange / Act / Assert
    expect(loginNotice("closed")).toBe(LOGIN_CLOSED);
  });

  it("names the failed phase", () => {
    // Arrange / Act / Assert
    expect(loginNotice("failed")).toBe(LOGIN_FAILED);
  });
});
