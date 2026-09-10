// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import type { FailureSink } from "../../src/failure/sink.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { registerWorkspaceMoved } from "../../src/rpc/moved.js";
import {
  clearRefusals,
  drawMalformedRefusal,
  drawTransportRefusal,
  drawTypedRefusal,
  refusal,
  refusalOf,
  type SentenceTable,
} from "../../src/rpc/refuse.js";

/** A sink that keeps what was filed. */
function recordingSink(): FailureSink & { reports: FailureKind[] } {
  const reports: FailureKind[] = [];
  return { reports, report: (kind) => reports.push(kind), retract: () => undefined };
}

/** The arms an endpoint words for itself. */
const OWN: SentenceTable = {
  askNotStanding: () => "that ask is no longer standing",
};

const installed: Array<() => void> = [];
afterEach(() => {
  for (const uninstall of installed.splice(0)) uninstall();
});

/** Register a move handler that records the addresses it was raised with. */
function watchMoves(): string[] {
  const seen: string[] = [];
  installed.push(registerWorkspaceMoved((address) => seen.push(address)));
  return seen;
}

describe("refusalOf", () => {
  const cases = [
    {
      arm: "unknownWorkspace",
      cause: { case: "unknownWorkspace", value: {} },
      text: "the daemon does not know this workspace",
    },
    {
      arm: "workspaceRefMismatch",
      cause: { case: "workspaceRefMismatch", value: { registryDir: "/elsewhere" } },
      text: "this workspace's directory disagrees with the registry's: /elsewhere",
    },
    {
      arm: "transferringAway",
      cause: { case: "transferringAway", value: { address: "127.0.0.1:9931" } },
      text: "this workspace moved to another daemon at 127.0.0.1:9931",
    },
    {
      arm: "notYetAdopted",
      cause: { case: "notYetAdopted", value: {} },
      text: "the daemon has not finished adopting this workspace yet",
    },
  ] as const;

  for (const c of cases) {
    it(`words the cross-cutting ${c.arm} arm`, () => {
      expect(refusalOf(c.cause, OWN, "AnswerPermissionError.cause").text).toBe(c.text);
    });
  }

  it("carries the wire's own arm name", () => {
    const said = refusalOf(cases[0].cause, OWN, "AnswerPermissionError.cause");
    expect(said.arm).toBe("unknownWorkspace");
  });

  it("prefers the caller's own sentence for the caller's own arm", () => {
    const said = refusalOf({ case: "askNotStanding", value: {} }, OWN, "AnswerPermissionError.cause");
    expect(said.text).toBe("that ask is no longer standing");
  });

  it("refuses an unset cause", () => {
    expect(() => refusalOf({ case: undefined }, OWN, "AnswerPermissionError.cause")).toThrow(
      MalformedView,
    );
  });

  it("refuses an arm neither this build nor the caller knows", () => {
    expect(() =>
      refusalOf({ case: "quarantined", value: {} }, OWN, "AnswerPermissionError.cause"),
    ).toThrow(MalformedView);
  });

  it("refuses a workspaceRefMismatch arm carrying no registry dir", () => {
    expect(() =>
      refusalOf({ case: "workspaceRefMismatch", value: {} }, OWN, "AnswerPermissionError.cause"),
    ).toThrow(MalformedView);
  });

  it("raises the page-wide move notice for transferringAway", () => {
    const seen = watchMoves();
    refusalOf(cases[2].cause, OWN, "AnswerPermissionError.cause");
    expect(seen).toEqual(["127.0.0.1:9931"]);
  });

  it("raises no move notice for any other arm", () => {
    const seen = watchMoves();
    refusalOf(cases[0].cause, OWN, "AnswerPermissionError.cause");
    expect(seen).toEqual([]);
  });
});

describe("drawTypedRefusal", () => {
  it("draws the sentence at the control", () => {
    const host = document.createElement("div");
    drawTypedRefusal(
      host,
      "SetModelError.cause",
      "SetModel",
      { case: "unknownWorkspace", value: {} },
      {},
    );
    expect(host.querySelector(".refusal")?.textContent).toBe(
      "the daemon does not know this workspace",
    );
  });

  it("hooks the arm name onto the refusal", () => {
    const host = document.createElement("div");
    drawTypedRefusal(
      host,
      "SetModelError.cause",
      "SetModel",
      { case: "notYetAdopted", value: {} },
      {},
    );
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("notYetAdopted");
  });

  it("replaces a previous click's refusal rather than stacking one", () => {
    const host = document.createElement("div");
    host.append(refusal("stale", "an older answer"));
    drawTypedRefusal(
      host,
      "SetModelError.cause",
      "SetModel",
      { case: "notYetAdopted", value: {} },
      {},
    );
    expect(host.querySelectorAll(".refusal")).toHaveLength(1);
  });

  it("raises the move notice from a topbar-side refusal too", () => {
    const seen = watchMoves();
    drawTypedRefusal(
      document.createElement("div"),
      "SetModelError.cause",
      "SetModel",
      { case: "transferringAway", value: { address: "127.0.0.1:9940" } },
      {},
    );
    expect(seen).toEqual(["127.0.0.1:9940"]);
  });
});

describe("clearRefusals", () => {
  it("removes what a previous click left", () => {
    const host = document.createElement("div");
    host.append(refusal("stale", "an older answer"));
    clearRefusals(host);
    expect(host.querySelector(".refusal")).toBeNull();
  });
});

describe("drawTransportRefusal", () => {
  it("marks the control with the transport arm", () => {
    const host = document.createElement("div");
    drawTransportRefusal(host);
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("drawMalformedRefusal", () => {
  it("answers false for an error that is not a malformed view", () => {
    const host = document.createElement("div");
    const drawn = drawMalformedRefusal(
      { failures: recordingSink() },
      host,
      "op",
      new Error("something else"),
    );
    expect(drawn).toBe(false);
  });

  it("files one frame_undecodable card for a malformed refusal", () => {
    const sink = recordingSink();
    drawMalformedRefusal(
      { failures: sink },
      document.createElement("div"),
      "op",
      new MalformedView("A.b", "the oneof sets no arm"),
    );
    expect(sink.reports.map((k) => k.kind.case)).toEqual(["frameUndecodable"]);
  });

  it("draws no refusal at the control for a frame it could not read", () => {
    const host = document.createElement("div");
    drawMalformedRefusal(
      { failures: recordingSink() },
      host,
      "op",
      new MalformedView("A.b", "the oneof sets no arm"),
    );
    // Since landing 4 every error carries a typed cause, so an unset one is an
    // unreadable frame rather than a wordless refusal: inventing a sentence
    // here would state a refusal the daemon never made.
    expect(host.querySelector(".refusal")).toBeNull();
  });
});
