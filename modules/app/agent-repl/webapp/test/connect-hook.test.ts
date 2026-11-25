// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { createConnectHookElement } from "../src/connect-hook.js";

describe("createConnectHookElement", () => {
  it("builds an element of the tag it is given", () => {
    expect(createConnectHookElement("ar-test-hook-tag").localName).toBe("ar-test-hook-tag");
  });

  it("defines the tag once, however many elements are built", () => {
    createConnectHookElement("ar-test-hook-once");
    expect(() => createConnectHookElement("ar-test-hook-once")).not.toThrow();
  });

  it("runs no hook before the element joins the document", () => {
    const el = createConnectHookElement("ar-test-hook-detached");
    let runs = 0;
    el.onConnect(() => (runs += 1));
    document.createElement("div").append(el);
    expect(runs).toBe(0);
  });

  it("runs every hook as the element joins the document", () => {
    const el = createConnectHookElement("ar-test-hook-joins");
    const runs: string[] = [];
    el.onConnect(() => runs.push("a"));
    el.onConnect(() => runs.push("b"));
    document.body.replaceChildren(el);
    expect(runs).toEqual(["a", "b"]);
  });

  it("runs its hooks again on every re-attachment", () => {
    const el = createConnectHookElement("ar-test-hook-again");
    let runs = 0;
    el.onConnect(() => (runs += 1));
    document.body.replaceChildren(el);
    el.remove();
    document.body.replaceChildren(el);
    expect(runs).toBe(2);
  });

  it("runs the hook when an ancestor is attached", () => {
    const el = createConnectHookElement("ar-test-hook-ancestor");
    let runs = 0;
    el.onConnect(() => (runs += 1));
    const host = document.createElement("article");
    host.append(el);
    document.body.replaceChildren(host);
    expect(runs).toBe(1);
  });
});
