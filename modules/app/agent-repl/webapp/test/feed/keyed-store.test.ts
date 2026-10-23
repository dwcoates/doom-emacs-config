import { describe, expect, it, vi } from "vitest";
import { createKeyedStore, type Disposable } from "../../src/feed/keyed-store.js";
import toolGroupSource from "../../src/feed/tool-group.ts?raw";
import responseRunSource from "../../src/feed/response-run.ts?raw";

/** An entry that counts its disposals. */
function entry(): Disposable & { disposed: number } {
  const e = {
    disposed: 0,
    dispose() {
      e.disposed += 1;
    },
  };
  return e;
}

describe("createKeyedStore", () => {
  it("creates an entry the first time its key is used", () => {
    // Arrange
    const store = createKeyedStore<ReturnType<typeof entry>>();
    const create = vi.fn(entry);
    // Act
    store.use("a", create);
    // Assert
    expect(create).toHaveBeenCalledOnce();
  });

  it("returns the SAME entry for a key used again, without creating another", () => {
    // Arrange
    const store = createKeyedStore<ReturnType<typeof entry>>();
    const first = store.use("a", entry);
    const create = vi.fn(entry);
    // Act
    const again = store.use("a", create);
    // Assert
    expect(again).toBe(first);
    expect(create).not.toHaveBeenCalled();
  });

  it("keeps an entry used since the previous prune", () => {
    // Arrange
    const store = createKeyedStore<ReturnType<typeof entry>>();
    const kept = store.use("a", entry);
    // Act
    store.prune();
    // Assert
    expect(kept.disposed).toBe(0);
    expect(store.use("a", entry)).toBe(kept);
  });

  it("disposes and forgets an entry not used since the previous prune", () => {
    // Arrange
    const store = createKeyedStore<ReturnType<typeof entry>>();
    const dropped = store.use("a", entry);
    store.prune();
    // Act: a pass that does not use "a".
    store.prune();
    // Assert
    expect(dropped.disposed).toBe(1);
    expect(store.use("a", entry)).not.toBe(dropped);
  });

  it("disposes a dropped entry exactly once across later prunes", () => {
    // Arrange
    const store = createKeyedStore<ReturnType<typeof entry>>();
    const dropped = store.use("a", entry);
    store.prune();
    // Act
    store.prune();
    store.prune();
    // Assert
    expect(dropped.disposed).toBe(1);
  });
});

describe("the arrangement's run stores share the keyed store", () => {
  it.each([
    ["tool-group.ts", toolGroupSource],
    ["response-run.ts", responseRunSource],
  ])("%s keeps its runs in createKeyedStore", (_name, source) => {
    expect(source).toContain("createKeyedStore<");
    expect(source).not.toMatch(/new Map<string, [A-Z]\w*>\(\);\s*let seen/);
  });
});
