import { afterEach, describe, expect, it } from "vitest";
import { ForwardingLogger, setLogger } from "../../src/log.js";
import {
  clearClientFailures,
  clientFailureSubstatus,
  onClientVerdict,
  reportClientFailure,
  standingClientFailure,
  type ClientFailureKind,
  type ClientVerdict,
} from "../../src/rpc/link.js";

/** Every listener a test subscribed, dropped so the next test starts clean. */
const unsubscribers: Array<() => void> = [];

afterEach(() => {
  while (unsubscribers.length > 0) unsubscribers.pop()?.();
  clearClientFailures();
});

/** Collect the verdicts published to a subscriber. */
function collect(): ClientVerdict[] {
  const seen: ClientVerdict[] = [];
  unsubscribers.push(
    onClientVerdict((verdict) => {
      if (verdict !== null) seen.push(verdict);
    }),
  );
  return seen;
}

/** Capture the records the canonical logger emits. */
function captureLog(): Array<[string, string]> {
  const lines: Array<[string, string]> = [];
  setLogger(
    new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line]), {}, "debug"),
  );
  return lines;
}

describe("reportClientFailure", () => {
  it("stands the reported verdict up", () => {
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(standingClientFailure()?.activity).toBe("AnswerColdGate: unavailable");
  });

  it("draws the ruling's substatus under a link failure", () => {
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(standingClientFailure()?.substatus).toBe("daemon unreachable");
  });

  it("logs the report at info with its kind and context", () => {
    const lines = captureLog();
    reportClientFailure("stream_ended", "WatchFooter stream ended (source_ended)");
    const record = lines.find(([level]) => level === "info")?.[1] ?? "";
    expect(JSON.parse(record)).toMatchObject({
      operation: "rpc.client-failure",
      context: { kind: "stream_ended", detail: "WatchFooter stream ended (source_ended)" },
    });
  });

  it("publishes the verdict to a subscriber", () => {
    const seen = collect();
    reportClientFailure("stream_ended", "WatchFeed stream ended (producer_ended)");
    expect(seen.map((verdict) => verdict.kind)).toEqual(["stream_ended"]);
  });

  it("replaces a standing verdict with a later, different one", () => {
    reportClientFailure("stream_ended", "WatchFeed stream ended (producer_ended)");
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    expect(standingClientFailure()?.kind).toBe("unary_transport");
  });

  it("DROPS an identical repeat, which is what terminates the ClientLog loop", () => {
    const seen = collect();
    reportClientFailure("client_log_failed", "ClientLog forwarding failed");
    reportClientFailure("client_log_failed", "ClientLog forwarding failed");
    expect(seen).toHaveLength(1);
  });

  it("republishes the same kind under a different line", () => {
    const seen = collect();
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    reportClientFailure("unary_transport", "Interrupt: unavailable");
    expect(seen).toHaveLength(2);
  });
});

describe("clientFailureSubstatus", () => {
  const table: ReadonlyArray<readonly [ClientFailureKind, string]> = [
    ["unary_transport", "daemon unreachable"],
    ["stream_ended", "daemon unreachable"],
    ["subscription_source_ended", "daemon unreachable"],
    ["unsubscribe_failed", "daemon unreachable"],
    ["client_log_failed", "daemon unreachable"],
    ["daemon_unreachable_card", "daemon unreachable"],
    ["frame_undecodable_card", "frame unreadable"],
    ["feed_not_tailing", "feed not tailing"],
  ];
  for (const [kind, substatus] of table) {
    it(`words ${kind} as "${substatus}"`, () => {
      expect(clientFailureSubstatus(kind)).toBe(substatus);
    });
  }
});

describe("clearClientFailures", () => {
  it("drops the standing verdict", () => {
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    clearClientFailures();
    expect(standingClientFailure()).toBeNull();
  });

  it("publishes the null verdict to a subscriber", () => {
    const seen: Array<ClientVerdict | null> = [];
    unsubscribers.push(onClientVerdict((verdict) => seen.push(verdict)));
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    clearClientFailures();
    expect(seen[seen.length - 1]).toBeNull();
  });

  it("says nothing when no verdict stands", () => {
    const lines = captureLog();
    clearClientFailures();
    expect(lines).toHaveLength(0);
  });
});

describe("onClientVerdict", () => {
  it("tells a late subscriber the verdict already standing", () => {
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    const seen = collect();
    expect(seen).toHaveLength(1);
  });

  it("stops publishing to an unsubscribed listener", () => {
    const seen: Array<ClientVerdict | null> = [];
    const stop = onClientVerdict((verdict) => seen.push(verdict));
    stop();
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    expect(seen).toEqual([null]);
  });
});
