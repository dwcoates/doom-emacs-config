import { describe, expect, it } from "vitest";
import { FailureKindSchema } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  CLIENT_FAILURE_ARMS,
  RETRACTABLE_ON_RECONNECT,
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  isClientFailureArm,
  staleBundle,
  workspaceGone,
} from "../../src/failure/sink.js";

describe("CLIENT_FAILURE_ARMS", () => {
  it("names exactly the six arms a frontend may mint", () => {
    expect([...CLIENT_FAILURE_ARMS]).toEqual([
      "daemonUnreachable",
      "workspaceGone",
      "bootFailed",
      "controlPlaneFailed",
      "frameUndecodable",
      "staleBundle",
    ]);
  });

  it("names only arms the generated FailureKind actually declares", () => {
    // ARRANGE: the split is by producer, so a renamed arm must fail here.
    const group = FailureKindSchema.oneofs.find((o) => o.name === "kind");
    const declared = new Set((group?.fields ?? []).map((f) => f.localName));
    // ACT
    const missing = CLIENT_FAILURE_ARMS.filter((arm) => !declared.has(arm));
    // ASSERT
    expect(missing).toEqual([]);
  });
});

describe("isClientFailureArm", () => {
  for (const arm of CLIENT_FAILURE_ARMS) {
    it(`accepts ${arm}`, () => {
      expect(isClientFailureArm(arm)).toBe(true);
    });
  }

  it("rejects a daemon-minted arm, which a frontend may never set", () => {
    expect(isClientFailureArm("shimDegraded")).toBe(false);
  });

  it("rejects an arm that exists nowhere", () => {
    expect(isClientFailureArm("nonesuch")).toBe(false);
  });
});

describe("the minting helpers", () => {
  it("mints daemonUnreachable on its own arm", () => {
    expect(daemonUnreachable(1006, "abnormal").kind.case).toBe("daemonUnreachable");
  });

  it("carries the close code as typed evidence, not prose", () => {
    const kind = daemonUnreachable(1006, "abnormal");
    expect(kind.kind.case === "daemonUnreachable" && kind.kind.value.closeCode).toBe(1006);
  });

  it("carries the close reason verbatim", () => {
    const kind = daemonUnreachable(1000, "shutting down");
    expect(kind.kind.case === "daemonUnreachable" && kind.kind.value.closeReason).toBe("shutting down");
  });

  it("mints frameUndecodable with its cause", () => {
    const kind = frameUndecodable("a oneof sets no arm", "FooterView");
    expect(kind.kind.case === "frameUndecodable" && kind.kind.value.cause).toBe("a oneof sets no arm");
  });

  it("mints frameUndecodable with the frame head, for whoever debugs it", () => {
    const kind = frameUndecodable("cause", "WatchFooterResponse at .footer");
    expect(kind.kind.case === "frameUndecodable" && kind.kind.value.frameHead).toBe(
      "WatchFooterResponse at .footer",
    );
  });

  it("mints bootFailed with whatever was caught, verbatim", () => {
    const kind = bootFailed("Error: no ?workspace");
    expect(kind.kind.case === "bootFailed" && kind.kind.value.cause).toBe("Error: no ?workspace");
  });

  it("mints controlPlaneFailed naming the request, so two do not reconcile", () => {
    const kind = controlPlaneFailed("OpenLogin", "unavailable");
    expect(kind.kind.case === "controlPlaneFailed" && kind.kind.value.what).toBe("OpenLogin");
  });

  it("mints workspaceGone, which carries no evidence", () => {
    expect(workspaceGone().kind.case).toBe("workspaceGone");
  });

  it("mints staleBundle with its detail", () => {
    const kind = staleBundle("the bundle predates the schema");
    expect(kind.kind.case === "staleBundle" && kind.kind.value.detail).toBe(
      "the bundle predates the schema",
    );
  });
});

describe("RETRACTABLE_ON_RECONNECT", () => {
  it("holds daemonUnreachable, the one purely window-shaped arm", () => {
    expect([...RETRACTABLE_ON_RECONNECT]).toEqual(["daemonUnreachable"]);
  });

  it("does NOT hold workspaceGone, which never resolves", () => {
    expect(RETRACTABLE_ON_RECONNECT).not.toContain("workspaceGone");
  });

  it("does NOT hold staleBundle, which is deliberately unresolvable", () => {
    expect(RETRACTABLE_ON_RECONNECT).not.toContain("staleBundle");
  });
});
