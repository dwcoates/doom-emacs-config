/**
 * The session's Remote Control bridge, held.
 *
 * WHAT THIS GUARDS: that every bridge is asked for so the same claude.ai
 * session survives its vendor process (keep-on-exit, and a re-bind of the
 * latest bridge session id), that only what the vendor accepted is held, and
 * that calls reach the vendor in the order they were made.
 */
import { describe, expect, it } from "vitest";
import { bridgeSessionIdOf, RemoteControl } from "../../src/engine/remote-control.js";
import { ScriptedQuery } from "./fakes.js";

const ON = { enabled: true, name: "parser-fix" } as const;
const OFF = { enabled: false, name: "" } as const;

describe("bridgeSessionIdOf", () => {
  it("reads the bridge session id the answer states", () => {
    // Arrange, Act, Assert
    expect(bridgeSessionIdOf({ bridge_session_id: "session_1" })).toBe("session_1");
  });

  it.each([
    { name: "no answer", answer: undefined },
    { name: "an answer stating none", answer: {} },
    { name: "an empty id", answer: { bridge_session_id: "" } },
    { name: "an id that is not a string", answer: { bridge_session_id: 7 } },
  ])("reads none from $name", ({ answer }) => {
    // Arrange, Act, Assert
    expect(bridgeSessionIdOf(answer)).toBeUndefined();
  });
});

describe("RemoteControl.set", () => {
  it("turns the bridge on under the session's name, kept on the vendor's exit", async () => {
    // Arrange
    const query = new ScriptedQuery();

    // Act
    await new RemoteControl().set(query, ON);

    // Assert
    expect(query.remoteControlCalls).toEqual([{ enabled: true, name: "parser-fix", options: { keepSessionOnExit: true } }]);
  });

  it("answers the bridge session id the vendor answered", async () => {
    // Arrange, Act
    const outcome = await new RemoteControl().set(new ScriptedQuery(), ON);

    // Assert
    expect(outcome).toEqual({ kind: "on", bridgeSessionId: "bridge-1" });
  });

  it("holds the bridge once the vendor accepted it", async () => {
    // Arrange
    const control = new RemoteControl();

    // Act
    await control.set(new ScriptedQuery(), ON);

    // Assert
    expect([control.enabled, control.bridgeSessionId]).toEqual([true, "bridge-1"]);
  });

  it("re-binds the bridge session the request names", async () => {
    // Arrange
    const query = new ScriptedQuery();

    // Act
    await new RemoteControl().set(query, { ...ON, reattachBridgeSessionId: "session_prev" });

    // Assert
    expect(query.remoteControlCalls[0]?.options?.reattachSessionId).toBe("session_prev");
  });

  it("re-binds the bridge session it already holds when the request names none", async () => {
    // Arrange
    const query = new ScriptedQuery();
    const control = new RemoteControl();
    await control.set(query, ON);

    // Act
    await control.set(query, ON);

    // Assert
    expect(query.remoteControlCalls[1]?.options?.reattachSessionId).toBe("bridge-1");
  });

  it("answers vendor refused, with the vendor's words, when the vendor refuses", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.enableRemoteControlRejects = new Error("Remote Control is disabled by policy");

    // Act
    const outcome = await new RemoteControl().set(query, ON);

    // Assert
    expect(outcome).toEqual({ kind: "refused", detail: "Remote Control is disabled by policy" });
  });

  it("holds nothing the vendor refused", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.enableRemoteControlRejects = new Error("not eligible");
    const control = new RemoteControl();

    // Act
    await control.set(query, ON);

    // Assert
    expect(control.enabled).toBe(false);
  });

  it("refuses an ON the vendor answered with no bridge session id, holding nothing", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.remoteControlAnswer = () => ({});
    const control = new RemoteControl();

    // Act
    const outcome = await control.set(query, ON);

    // Assert
    expect([outcome.kind, control.enabled]).toEqual(["refused", false]);
  });

  it("turns the bridge off through the vendor", async () => {
    // Arrange
    const query = new ScriptedQuery();
    const control = new RemoteControl();
    await control.set(query, ON);

    // Act
    const outcome = await control.set(query, OFF);

    // Assert
    expect([outcome, query.remoteControlCalls.at(-1), control.enabled]).toEqual([{ kind: "off" }, { enabled: false }, false]);
  });

  it("asks the vendor to turn the bridge off even when it never held one", async () => {
    // Arrange
    const query = new ScriptedQuery();

    // Act
    await new RemoteControl().set(query, OFF);

    // Assert
    expect(query.remoteControlCalls).toEqual([{ enabled: false }]);
  });

  it("keeps the bridge stated as on when the vendor refuses to turn it off", async () => {
    // Arrange
    const query = new ScriptedQuery();
    const control = new RemoteControl();
    await control.set(query, ON);
    query.enableRemoteControlRejects = new Error("the bridge would not close");

    // Act
    const outcome = await control.set(query, OFF);

    // Assert
    expect([outcome.kind, control.enabled]).toEqual(["refused", true]);
  });

  it("reaches the vendor in the order its calls were made", async () => {
    // Arrange
    const query = new ScriptedQuery();
    const control = new RemoteControl();

    // Act
    await Promise.all([control.set(query, ON), control.set(query, OFF), control.set(query, ON)]);

    // Assert
    expect(query.remoteControlCalls.map((call) => call.enabled)).toEqual([true, false, true]);
  });

  it("lets a call after a refused one through", async () => {
    // Arrange
    const query = new ScriptedQuery();
    const control = new RemoteControl();
    query.enableRemoteControlRejects = new Error("once");
    const refused = control.set(query, ON);
    await refused;
    query.enableRemoteControlRejects = undefined;

    // Act
    const outcome = await control.set(query, ON);

    // Assert
    expect(outcome.kind).toBe("on");
  });
});

describe("RemoteControl.reapply", () => {
  it("asks nothing of a new vendor process while the bridge is off", async () => {
    // Arrange
    const query = new ScriptedQuery();

    // Act
    const outcome = await new RemoteControl().reapply(query, "test", () => true);

    // Assert
    expect([outcome, query.remoteControlCalls]).toEqual([undefined, []]);
  });

  it("puts the held bridge back on a new vendor process, re-binding its session, kept on exit", async () => {
    // Arrange
    const control = new RemoteControl();
    await control.set(new ScriptedQuery(), { ...ON, reattachBridgeSessionId: "session_prev" });
    const next = new ScriptedQuery();

    // Act
    await control.reapply(next, "test", () => true);

    // Assert
    expect(next.remoteControlCalls).toEqual([
      { enabled: true, name: "parser-fix", options: { keepSessionOnExit: true, reattachSessionId: "session_prev" } },
    ]);
  });

  it("re-binds the LATEST bridge session the vendor answered", async () => {
    // Arrange
    const control = new RemoteControl();
    const first = new ScriptedQuery();
    first.remoteControlAnswer = () => ({ bridge_session_id: "session_new" });
    await control.set(first, { ...ON, reattachBridgeSessionId: "session_prev" });
    const next = new ScriptedQuery();

    // Act
    await control.reapply(next, "test", () => true);

    // Assert
    expect(next.remoteControlCalls[0]?.options?.reattachSessionId).toBe("session_new");
  });

  it("asks nothing of a query let go before its turn came", async () => {
    // Arrange
    const control = new RemoteControl();
    await control.set(new ScriptedQuery(), ON);
    const released = new ScriptedQuery();

    // Act
    const outcome = await control.reapply(released, "test", () => false);

    // Assert
    expect([outcome, released.remoteControlCalls]).toEqual([undefined, []]);
  });

  it("answers the vendor's refusal on the new process, still holding the bridge for the next one", async () => {
    // Arrange
    const control = new RemoteControl();
    await control.set(new ScriptedQuery(), ON);
    const next = new ScriptedQuery();
    next.enableRemoteControlRejects = new Error("the bridge failed to connect");

    // Act
    const outcome = await control.reapply(next, "test", () => true);

    // Assert
    expect([outcome?.kind, control.enabled]).toEqual(["refused", true]);
  });

  it("reads the state the call ahead of it left", async () => {
    // Arrange
    const control = new RemoteControl();
    const query = new ScriptedQuery();
    await control.set(query, ON);
    const next = new ScriptedQuery();

    // Act: an OFF made before the reapply runs ahead of it.
    const off = control.set(query, OFF);
    const reapplied = control.reapply(next, "test", () => true);
    await off;

    // Assert
    expect([await reapplied, next.remoteControlCalls]).toEqual([undefined, []]);
  });
});
