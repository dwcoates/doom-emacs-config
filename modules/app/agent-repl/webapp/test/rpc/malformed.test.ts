import { describe, expect, it } from "vitest";
import {
  MalformedView,
  UnknownPushArm,
  isMalformedView,
  isUnknownPushArm,
} from "../../src/rpc/malformed.js";

describe("MalformedView", () => {
  it("keeps the path as its own field", () => {
    expect(new MalformedView("FooterView.status", "unset").path).toBe("FooterView.status");
  });

  it("keeps the detail as its own field", () => {
    expect(new MalformedView("FooterView.status", "unset").detail).toBe("unset");
  });

  it("joins both into the message a stack trace shows", () => {
    expect(new MalformedView("A.b", "c").message).toBe("malformed view at A.b: c");
  });

  it("names itself so a caught value is identifiable by name", () => {
    expect(new MalformedView("A.b", "c").name).toBe("MalformedView");
  });

  it("is an Error, so an unguarded catch still reports it", () => {
    expect(new MalformedView("A.b", "c")).toBeInstanceOf(Error);
  });
});

describe("isMalformedView", () => {
  it("recognizes the refusal", () => {
    expect(isMalformedView(new MalformedView("A.b", "c"))).toBe(true);
  });

  it("rejects an ordinary Error, which is not a view refusal", () => {
    expect(isMalformedView(new Error("boom"))).toBe(false);
  });

  it("rejects a non-Error throw", () => {
    expect(isMalformedView("boom")).toBe(false);
  });

  it("recognizes an UnknownPushArm, which is a MalformedView too", () => {
    expect(isMalformedView(new UnknownPushArm("R.push", "somethingNew"))).toBe(true);
  });
});

describe("UnknownPushArm", () => {
  it("keeps the arm it could not draw as its own field", () => {
    expect(new UnknownPushArm("R.push", "mutationProgress").arm).toBe("mutationProgress");
  });

  it("names the arm in the detail, matching the loud unreachable-arm sentence", () => {
    expect(new UnknownPushArm("R.push", "mutationProgress").detail).toBe(
      "arm 'mutationProgress' is not one this build can draw",
    );
  });

  it("names itself so a caught value is identifiable by name", () => {
    expect(new UnknownPushArm("R.push", "x").name).toBe("UnknownPushArm");
  });

  it("is a MalformedView, so code that does not care still treats it as the refusal", () => {
    expect(new UnknownPushArm("R.push", "x")).toBeInstanceOf(MalformedView);
  });
});

describe("isUnknownPushArm", () => {
  it("recognizes the forward-compat skew refusal", () => {
    expect(isUnknownPushArm(new UnknownPushArm("R.push", "x"))).toBe(true);
  });

  it("rejects a plain MalformedView, which is a real malformation", () => {
    expect(isUnknownPushArm(new MalformedView("A.b", "c"))).toBe(false);
  });
});
