/**
 * store/persistence.ts's own coverage: {@link unavailablePersistence} is the
 * placeholder wired in for a build with no store socket. Nothing on the
 * ordinary --fake/--real paths constructs one (they always hand `main.ts` a
 * real store target), so it never runs in any scenario test — but the module
 * comment is explicit that it is KEPT, not leftover, as the honest answer for
 * that build shape: every verb refuses loudly with `store_unavailable` rather
 * than silently degrading. This pins that every verb does exactly that.
 */
import { describe, expect, it } from "vitest";
import { containing } from "../expect-shapes.js";

import { PersistenceError, unavailablePersistence } from "../../src/store/persistence.js";

describe("unavailablePersistence", () => {
  const persistence = unavailablePersistence();

  it("write throws store_unavailable synchronously", () => {
    expect(() => persistence.write({} as never)).toThrowError(
      containing({ kind: "store_unavailable" }) as Error,
    );
  });

  it("writeDurable rejects with store_unavailable", async () => {
    await expect(persistence.writeDurable({} as never)).rejects.toBeInstanceOf(PersistenceError);
    await expect(persistence.writeDurable({} as never)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("openAgentPage rejects with store_unavailable", async () => {
    await expect(persistence.openAgentPage({} as never, 1)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("readFirstPage rejects with store_unavailable", async () => {
    await expect(persistence.readFirstPage({} as never, 1)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("readAgentPage rejects with store_unavailable", async () => {
    await expect(persistence.readAgentPage({} as never, 1, {} as never)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("liveWork rejects with store_unavailable", async () => {
    await expect(persistence.liveWork({} as never)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("openBashRun rejects with store_unavailable", async () => {
    await expect(persistence.openBashRun({} as never)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("whenWritable resolves at once: nothing is ever buffered to wait out", async () => {
    await expect(persistence.whenWritable()).resolves.toBeUndefined();
  });

  it("flush resolves with zero lost rows rather than refusing", async () => {
    await expect(persistence.flush()).resolves.toEqual({ lostRows: 0 });
  });

  it("setProducer, clearProducer, onFault and onDegradedWindow are harmless no-ops", () => {
    expect(persistence.setProducer("producer")).toBeUndefined();
    expect(persistence.clearProducer()).toBeUndefined();
    const unsubscribeFault = persistence.onFault(() => undefined);
    const unsubscribeWindow = persistence.onDegradedWindow(() => undefined);
    expect(unsubscribeFault()).toBeUndefined();
    expect(unsubscribeWindow()).toBeUndefined();
  });
});
