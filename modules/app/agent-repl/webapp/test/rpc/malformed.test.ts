import { describe, expect, it } from "vitest";
import { MalformedView, isMalformedView } from "../../src/rpc/malformed.js";

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
});
