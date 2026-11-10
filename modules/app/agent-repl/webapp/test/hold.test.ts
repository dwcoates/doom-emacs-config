// The shared drawing hold. See src/hold.ts.
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WorkspaceRosterSchema, type WorkspaceRoster } from "../../proto/gen/ts/frontend/v1/sidebar_pb";
import { createDrawingHold, type DrawingHold } from "../src/hold.js";
import { log } from "../src/log.js";

/** A roster carrying nothing but REVISION. */
function at(revision: number): WorkspaceRoster {
  return create(WorkspaceRosterSchema, { revision: BigInt(revision) });
}

/** A hold whose draws are recorded by revision. */
function recordedHold(): { hold: DrawingHold<WorkspaceRoster>; drawn: number[] } {
  const drawn: number[] = [];
  return {
    hold: createDrawingHold({ draw: (roster) => drawn.push(Number(roster.revision)), revisionOf: (roster) => roster.revision, operation: "test.hold" }),
    drawn,
  };
}

describe("the drawing hold", () => {
  it("draws a push at once while nothing is held", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    expect(drawn).toEqual([1]);
  });

  it("keeps every push while a drag is in progress", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.drag();
    hold.offer(at(2));
    expect([drawn, hold.holding()]).toEqual([[1], true]);
  });

  it("draws the newest push held back when a drag is cancelled", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.drag();
    hold.offer(at(2));
    hold.offer(at(3));
    hold.cancel();
    expect([drawn, hold.holding()]).toEqual([[1, 3], false]);
  });

  it("draws the last roster again when a cancelled drag held nothing back", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.drag();
    hold.cancel();
    expect(drawn).toEqual([1, 1]);
  });

  it("lets go and redraws when the ask did not take", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.await();
    hold.settle(null);
    expect([drawn, hold.holding()]).toEqual([[1, 1], false]);
  });

  it("holds an older push until one at the answered revision arrives", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.await();
    hold.settle(3n);
    hold.offer(at(2));
    expect([drawn, hold.holding()]).toEqual([[1], true]);
    hold.offer(at(3));
    expect([drawn, hold.holding()]).toEqual([[1, 3], false]);
  });

  it("lets go at once when the answered revision is already held back", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.await();
    hold.offer(at(4));
    hold.settle(4n);
    expect([drawn, hold.holding()]).toEqual([[1, 4], false]);
  });

  it("lets go at once when the answered revision is already drawn", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(5));
    hold.await();
    hold.settle(5n);
    expect([drawn, hold.holding()]).toEqual([[5, 5], false]);
  });

  it("lets an awaited revision go when the stream ends", () => {
    const { hold, drawn } = recordedHold();
    hold.offer(at(1));
    hold.await();
    hold.settle(9n);
    hold.reset();
    expect([drawn, hold.holding()]).toEqual([[1, 1], false]);
  });

  it("keeps a drag's hold when the stream ends", () => {
    const { hold } = recordedHold();
    hold.offer(at(1));
    hold.drag();
    hold.reset();
    expect(hold.holding()).toBe(true);
  });

  it("records why it let go", () => {
    const debug = vi.spyOn(log, "debug");
    const { hold } = recordedHold();
    hold.offer(at(1));
    hold.drag();
    hold.cancel();
    const record = debug.mock.calls.find(([message]) => message === "a held drawing let go")?.[1];
    expect([record?.operation, (record?.context as { why?: string } | undefined)?.why]).toEqual([
      "test.hold",
      "the drag was cancelled",
    ]);
    debug.mockRestore();
  });
});

describe("a drawing whose pushes carry no revision", () => {
  /** A hold over strings, which carry no revision. */
  function unrevisioned(): { hold: DrawingHold<string>; drawn: string[] } {
    const drawn: string[] = [];
    return { hold: createDrawingHold<string>({ draw: (push) => drawn.push(push), operation: "test.hold" }), drawn };
  }

  it("draws the newest push held back when its ask settles", () => {
    const { hold, drawn } = unrevisioned();
    hold.offer("a");
    hold.drag();
    hold.offer("b");
    hold.await();
    hold.offer("c");
    hold.settle(null);
    expect([drawn, hold.holding()]).toEqual([["a", "c"], false]);
  });

  it("records no revision when it lets go", () => {
    const debug = vi.spyOn(log, "debug");
    const { hold } = unrevisioned();
    hold.offer("a");
    hold.drag();
    hold.cancel();
    const record = debug.mock.calls.find(([message]) => message === "a held drawing let go")?.[1];
    expect((record?.context as { revision?: unknown } | undefined)?.revision).toBeNull();
    debug.mockRestore();
  });

  it("refuses to settle at a revision it cannot read", () => {
    const { hold } = unrevisioned();
    hold.offer("a");
    hold.await();
    expect(() => hold.settle(1n)).toThrow(/invariant/);
  });
});
