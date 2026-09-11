/**
 * The READ half's retry schedule.
 *
 * The condition under test is the one that cost the owner a whole day of
 * bring-ups: one `SQLITE_BUSY` on `begin read transaction`, which another
 * attempt milliseconds later would have answered.
 */
import { describe, expect, it } from "vitest";
import { PersistenceError } from "../../src/store/persistence.js";
import { isRetryableRead, readWithRetry } from "../../src/store/retry.js";

/** The schedule, shortened so the assertions are about behavior, not time. */
const POLICY = { bufferCapacity: 4, backoffMs: [1, 2], maxAttempts: 3 } as const;

/** Every backoff this suite took, so the schedule itself is observable. */
function recorder(): { slept: number[]; sleep: (ms: number) => Promise<void> } {
  const slept: number[] = [];
  return {
    slept,
    sleep: (ms: number) => {
      slept.push(ms);
      return Promise.resolve();
    },
  };
}

const busy = (): PersistenceError =>
  new PersistenceError(
    "store_unavailable",
    "storage failure: begin read transaction: database is locked (5) (SQLITE_BUSY)",
  );

describe("which read failures are worth another attempt", () => {
  it("retries a store that could not be reached or failed the read", () => {
    // Arrange, Act, Assert.
    expect(isRetryableRead(busy())).toBe(true);
  });

  it("does not retry a book the store says it holds no rows for", () => {
    // Arrange, Act, Assert.
    expect(isRetryableRead(new PersistenceError("unknown_agent", "no such book"))).toBe(false);
  });

  it("does not retry a pointer the store says names no line", () => {
    // Arrange, Act, Assert.
    expect(isRetryableRead(new PersistenceError("stale_pointer", "no such line"))).toBe(false);
  });

  it("does not retry a handle the store says it never announced", () => {
    // Arrange, Act, Assert.
    expect(isRetryableRead(new PersistenceError("unknown_work", "no such run"))).toBe(false);
  });

  it("does not retry a defect that is not a store refusal at all", () => {
    // Arrange, Act, Assert.
    expect(isRetryableRead(new TypeError("the reader itself is broken"))).toBe(false);
  });
});

describe("a read on the retry schedule", () => {
  it("answers on the first attempt without taking any backoff", async () => {
    // Arrange.
    const { slept, sleep } = recorder();

    // Act.
    const answer = await readWithRetry("liveWork", () => Promise.resolve("page"), {
      retry: POLICY,
      sleep,
    });

    // Assert.
    expect(answer).toBe("page");
    expect(slept).toEqual([]);
  });

  it("answers after a busy database let go", async () => {
    // Arrange.
    const { slept, sleep } = recorder();
    let attempts = 0;
    const read = (): Promise<string> => {
      attempts += 1;
      return attempts === 1 ? Promise.reject(busy()) : Promise.resolve("page");
    };

    // Act.
    const answer = await readWithRetry("liveWork", read, { retry: POLICY, sleep });

    // Assert.
    expect(answer).toBe("page");
    expect(slept).toEqual([1]);
  });

  it("walks the backoff schedule it was given", async () => {
    // Arrange.
    const { slept, sleep } = recorder();

    // Act.
    await readWithRetry("liveWork", () => Promise.reject(busy()), {
      retry: POLICY,
      sleep,
    }).catch(() => undefined);

    // Assert.
    expect(slept).toEqual([1, 2]);
  });

  // THE LAST FAILURE IS THE ONE THE CALLER RAISES ITS FAULT FROM, so the arm
  // and the driver's own text both have to survive the replays.
  it("throws the last failure exactly as it stood once the schedule is spent", async () => {
    // Arrange.
    const { sleep } = recorder();

    // Act.
    const rejection = await readWithRetry("liveWork", () => Promise.reject(busy()), {
      retry: POLICY,
      sleep,
    }).then(() => null, (error: unknown) => error);

    // Assert.
    expect(rejection).toBeInstanceOf(PersistenceError);
    expect((rejection as PersistenceError).kind).toBe("store_unavailable");
    expect((rejection as PersistenceError).message).toContain("SQLITE_BUSY");
  });

  it("gives up at once on a refusal another attempt cannot change", async () => {
    // Arrange.
    const { slept, sleep } = recorder();
    let attempts = 0;
    const read = (): Promise<string> => {
      attempts += 1;
      return Promise.reject(new PersistenceError("unknown_agent", "no such book"));
    };

    // Act.
    await readWithRetry("readFirstPage", read, { retry: POLICY, sleep }).catch(() => undefined);

    // Assert.
    expect(attempts).toBe(1);
    expect(slept).toEqual([]);
  });

  it("takes the default schedule when none is supplied", async () => {
    // Arrange: the default's first backoff, taken through an injected sleep.
    const { slept, sleep } = recorder();
    let attempts = 0;
    const read = (): Promise<string> => {
      attempts += 1;
      return attempts === 1 ? Promise.reject(busy()) : Promise.resolve("page");
    };

    // Act.
    await readWithRetry("liveWork", read, { sleep });

    // Assert.
    expect(slept).toEqual([50]);
  });
});
