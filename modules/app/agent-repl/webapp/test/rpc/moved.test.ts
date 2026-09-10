import { afterEach, describe, expect, it } from "vitest";
import { registerWorkspaceMoved, workspaceMoved } from "../../src/rpc/moved.js";

describe("workspaceMoved", () => {
  // The handler is one module-level slot, so a test that leaves one
  // installed would decide the answer for whichever test runs next.
  // Registering a handler and immediately uninstalling it empties the slot
  // through the module's own lifecycle.
  afterEach(() => {
    registerWorkspaceMoved(() => undefined)();
  });

  it("answers false when no lifecycle is mounted to say so", () => {
    expect(workspaceMoved("127.0.0.1:9931")).toBe(false);
  });

  it("hands the address to the registered handler", () => {
    const seen: string[] = [];
    const uninstall = registerWorkspaceMoved((address) => seen.push(address));
    workspaceMoved("127.0.0.1:9931");
    uninstall();
    expect(seen).toEqual(["127.0.0.1:9931"]);
  });

  it("answers true once a handler is registered", () => {
    const uninstall = registerWorkspaceMoved(() => undefined);
    const answered = workspaceMoved("127.0.0.1:9931");
    uninstall();
    expect(answered).toBe(true);
  });

  it("stops calling a handler that has been uninstalled", () => {
    const seen: string[] = [];
    registerWorkspaceMoved((address) => seen.push(address))();
    workspaceMoved("127.0.0.1:9931");
    expect(seen).toEqual([]);
  });

  it("leaves a successor's handler installed when a late dispose uninstalls", () => {
    const seen: string[] = [];
    const stale = registerWorkspaceMoved(() => seen.push("stale"));
    const current = registerWorkspaceMoved(() => seen.push("current"));
    stale();
    workspaceMoved("127.0.0.1:9931");
    current();
    expect(seen).toEqual(["current"]);
  });
});
