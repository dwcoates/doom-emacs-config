/**
 * store/answered-lookup.ts — the shape every store lookup shares: a transport
 * error is the kind the engine switches on, a failure arm is a refused request
 * or the read failure it names, and what is left after the retry schedule is
 * the `failed` answer.
 */
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { answeredLookup, lookupRefused, sendLookup } from "../../src/store/answered-lookup.js";
import { PersistenceError } from "../../src/store/persistence.js";

/** The schedule, shortened so the assertions are about behavior, not time. */
const RETRY = { backoffMs: [1], maxAttempts: 2 } as const;
const noSleep = (): Promise<void> => Promise.resolve();

/** The kind of PersistenceError a promise rejected with, or "resolved". */
async function rejectedKind(promise: Promise<unknown>): Promise<string> {
  try {
    await promise;
    return "resolved";
  } catch (error) {
    return error instanceof PersistenceError ? error.kind : `not a PersistenceError: ${String(error)}`;
  }
}

describe("sendLookup", () => {
  it("answers the response the request returned", async () => {
    // Arrange, Act, Assert
    await expect(sendLookup(() => Promise.resolve("answer"))).resolves.toBe("answer");
  });

  it("turns a thrown transport error into store_unavailable", async () => {
    // Arrange, Act, Assert
    expect(await rejectedKind(sendLookup(() => Promise.reject(new Error("connect ECONNREFUSED"))))).toBe("store_unavailable");
  });

  it("passes a PersistenceError through unchanged", async () => {
    // Arrange, Act, Assert
    expect(await rejectedKind(sendLookup(() => Promise.reject(new PersistenceError("stale_pointer", "gone"))))).toBe(
      "stale_pointer",
    );
  });
});

describe("lookupRefused", () => {
  it("throws a refused request as invalid_request, naming the field", () => {
    // Arrange
    const failure = { detail: "no", kind: { case: "invalidRequest" as const, value: { field: "delivery_id" } } };
    // Act, Assert
    expect(() => lookupRefused(failure)).toThrow(/field delivery_id/);
  });

  it("throws a storage failure as store_unavailable", () => {
    // Arrange
    const failure = { detail: "database is locked", kind: { case: "storageFailure" as const } };
    // Act
    let kind = "";
    try {
      lookupRefused(failure);
    } catch (error) {
      kind = error instanceof PersistenceError ? error.kind : "";
    }
    // Assert
    expect(kind).toBe("store_unavailable");
  });
});

describe("answeredLookup", () => {
  it("answers what the attempt answered", async () => {
    // Arrange, Act
    const answer = await answeredLookup("Get", () => Promise.resolve({ kind: "found" }), { retry: RETRY, sleep: noSleep });
    // Assert
    expect(answer).toEqual({ kind: "found" });
  });

  it("answers failed, never a throw, after the retry schedule", async () => {
    // Arrange
    let attempts = 0;
    const attempt = (): Promise<never> => {
      attempts += 1;
      return Promise.reject(new PersistenceError("store_unavailable", "database is locked"));
    };
    // Act
    const answer = await answeredLookup("Get", attempt, { retry: RETRY, sleep: noSleep });
    // Assert
    expect([answer, attempts]).toEqual([{ kind: "failed", detail: "database is locked" }, RETRY.maxAttempts]);
  });

  it("answers a refused request failed at once, never retried", async () => {
    // Arrange
    let attempts = 0;
    const attempt = (): Promise<never> => {
      attempts += 1;
      return Promise.reject(new PersistenceError("invalid_request", "malformed"));
    };
    // Act
    const answer = await answeredLookup("Get", attempt, { retry: RETRY, sleep: noSleep });
    // Assert
    expect([answer.kind, attempts]).toEqual(["failed", 1]);
  });
});

describe("every store lookup shares the one shape", () => {
  const storeDir = join(import.meta.dirname, "../../src/store");
  const lookups = ["locator.ts", "detached-work.ts", "peer-delivery.ts"];

  it.each(lookups)("%s sends, refuses and answers through answered-lookup", (file) => {
    // Arrange, Act
    const source = readFileSync(join(storeDir, file), "utf8");
    // Assert
    expect([source.includes("sendLookup("), source.includes("lookupRefused("), source.includes("answeredLookup(")]).toEqual([
      true,
      true,
      true,
    ]);
  });

  it("leaves no store file hand-rolling a lookup's refusal or its retry wrapper", () => {
    // Arrange
    const files = readdirSync(storeDir).filter((name) => name.endsWith(".ts") && name !== "answered-lookup.ts");
    // Act
    const handRolled = files.filter((name) => {
      const source = readFileSync(join(storeDir, name), "utf8");
      return source.includes("the store refused the lookup as malformed");
    });
    // Assert
    expect(handRolled).toEqual([]);
  });
});
