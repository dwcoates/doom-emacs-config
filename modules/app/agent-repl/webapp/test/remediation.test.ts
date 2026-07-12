import { describe, expect, it } from "vitest";

import {
  REMEDIATION_ALREADY_UNDERWAY,
  REMEDIATION_DEVISING,
  REMEDIATION_FAILED,
  remediationNotice,
  requestRemediation,
} from "../src/remediation.js";

/** A fetch that answers POST /remediation with the given payload. */
function fakeFetch(resp: { ok: boolean; status?: number; started?: boolean }): {
  fetchFn: typeof fetch;
  calls: Array<{ url: string; body: unknown }>;
} {
  const calls: Array<{ url: string; body: unknown }> = [];
  const fetchFn = (async (url: string, init: RequestInit) => {
    calls.push({ url, body: JSON.parse(String(init.body)) });
    return {
      ok: resp.ok,
      status: resp.status ?? 202,
      json: async () => ({ started: resp.started ?? true }),
      text: async () => "boom",
    };
  }) as unknown as typeof fetch;
  return { fetchFn, calls };
}

describe("requestRemediation", () => {
  it("hands the lost session id to the daemon's remediation endpoint", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true, started: true });
    // Act
    await requestRemediation("http://d", "s_ghost", fetchFn);
    // Assert
    expect(calls[0].url).toBe("http://d/remediation");
    expect(calls[0].body).toEqual({ session_id: "s_ghost" });
  });

  it("reports the devising phase when this call dispatched the analyst", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: true, started: true });
    // Act / Assert
    await expect(requestRemediation("http://d", "s_ghost", fetchFn)).resolves.toBe("devising");
  });

  it("reports the underway phase when an analyst was already running", async () => {
    // Arrange — a second tab (or an earlier reconnect) got there first.
    const { fetchFn } = fakeFetch({ ok: true, started: false });
    // Act / Assert
    await expect(requestRemediation("http://d", "s_ghost", fetchFn)).resolves.toBe("underway");
  });

  it("rejects on a non-2xx rather than claiming a remediation that never ran", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: false, status: 503 });
    // Act / Assert
    await expect(requestRemediation("http://d", "s_ghost", fetchFn)).rejects.toThrow("503");
  });
});

describe("remediationNotice", () => {
  it("announces the plan being devised while the analyst works", () => {
    // Arrange / Act / Assert
    expect(remediationNotice("devising")).toBe(REMEDIATION_DEVISING);
    expect(REMEDIATION_DEVISING).toBe("devising remediation plan");
  });

  it("announces a dispatch failure instead of a phantom plan", () => {
    // Arrange / Act / Assert
    expect(remediationNotice("failed")).toBe(REMEDIATION_FAILED);
  });

  it("announces an analyst already at work on the session", () => {
    // Arrange / Act / Assert
    expect(remediationNotice("underway")).toBe(REMEDIATION_ALREADY_UNDERWAY);
  });
});
