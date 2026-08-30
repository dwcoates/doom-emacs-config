/**
 * The WatchSession fan-out.
 *
 * WHAT THIS GUARDS: that a consumer's very first frame is the diagnostics one,
 * on EVERY open. The failure mode being excluded is a WatchSession that pushes
 * nothing until something changes — connect-go surfaces a stream refusal only
 * at the first Receive, so a silent stream is indistinguishable from a refused
 * one and the daemon's whole bring-up blocks on it.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import { SessionPushes, SUBSCRIBER_QUEUE_LIMIT } from "../../src/engine/pushes.js";

function modelChanged(name: string): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "modelChanged",
      value: create(conversationv1.SessionModelChangedSchema, {
        effectiveModel: create(conversationv1.AgentModelSchema, { name }),
      }),
    },
  });
}

function compacting(): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, {
    update: { case: "compacting", value: create(conversationv1.SessionCompactingSchema, {}) },
  });
}

function fault(detail: string): conversationv1.SessionFault {
  return create(conversationv1.SessionFaultSchema, {
    component: "test",
    detail,
    kind: {
      case: "storeUnreachable",
      value: create(conversationv1.SessionFaultStoreUnreachableSchema, {}),
    },
  });
}

/**
 * Read from ONE iterator over the stream.
 *
 * Deliberately not a `for await ... break`: breaking out of a for-await calls
 * the iterator's `return()`, which is how a consumer says it has gone away —
 * so a test that broke and then read again would be testing a subscription it
 * had already cancelled.
 */
function reader(stream: AsyncIterable<conversationv1.SessionUpdate>): {
  next(): Promise<conversationv1.SessionUpdate | undefined>;
  rest(): Promise<conversationv1.SessionUpdate[]>;
} {
  const iterator = stream[Symbol.asyncIterator]();
  return {
    next: async () => (await iterator.next()).value as conversationv1.SessionUpdate | undefined,
    rest: async () => {
      const taken: conversationv1.SessionUpdate[] = [];
      for (;;) {
        const step = await iterator.next();
        if (step.done === true) return taken;
        taken.push(step.value);
      }
    },
  };
}

async function take(
  stream: AsyncIterable<conversationv1.SessionUpdate>,
  count: number,
): Promise<conversationv1.SessionUpdate[]> {
  const read = reader(stream);
  const taken: conversationv1.SessionUpdate[] = [];
  for (let index = 0; index < count; index++) {
    const next = await read.next();
    if (next === undefined) break;
    taken.push(next);
  }
  return taken;
}

describe("opening a stream", () => {
  it("delivers diagnostics FIRST", async () => {
    const pushes = new SessionPushes(() => 1);

    const [first] = await take(pushes.subscribe(), 1);

    expect(first?.update.case).toBe("diagnostics");
  });

  it("reports healthy when nothing has faulted", async () => {
    const pushes = new SessionPushes(() => 1);

    const [first] = await take(pushes.subscribe(), 1);

    expect(first?.update.case === "diagnostics" ? first.update.value.health.case : undefined).toBe(
      "healthy",
    );
  });

  it("gives a SECOND concurrent subscriber diagnostics first too", async () => {
    const pushes = new SessionPushes(() => 1);
    const first = pushes.subscribe();
    await take(first, 1);

    const [second] = await take(pushes.subscribe(), 1);

    expect(second?.update.case).toBe("diagnostics");
  });

  it("reports unhealthy with the faults kept since start", async () => {
    const pushes = new SessionPushes(() => 1);
    pushes.fault(fault("the store went away"));

    const [first] = await take(pushes.subscribe(), 1);

    expect(
      first?.update.case === "diagnostics" && first.update.value.health.case === "unhealthy"
        ? first.update.value.health.value.faults.map((f) => f.detail)
        : undefined,
    ).toEqual(["the store went away"]);
  });

  it("catches a late joiner up on the current model", async () => {
    const pushes = new SessionPushes(() => 1);
    pushes.push(modelChanged("claude-opus-5"));

    const taken = await take(pushes.subscribe(), 2);

    expect(taken[1]?.update.case).toBe("modelChanged");
  });

  it("does not replay an event arm to a late joiner", async () => {
    // Two identical compactions are two compactions; replaying one to a late
    // joiner would announce a compaction that is not happening.
    const pushes = new SessionPushes(() => 1);
    pushes.push(compacting());

    const taken = await take(pushes.subscribe(), 1);

    expect(taken.map((update) => update.update.case)).toEqual(["diagnostics"]);
  });

  it("counts its subscribers", () => {
    const pushes = new SessionPushes(() => 1);
    pushes.subscribe();
    pushes.subscribe();

    expect(pushes.subscriberCount).toBe(2);
  });
});

describe("pushing", () => {
  it("reaches an attached consumer", async () => {
    const pushes = new SessionPushes(() => 1);
    const stream = pushes.subscribe();
    pushes.push(modelChanged("claude-opus-5"));

    const taken = await take(stream, 2);

    expect(taken[1]?.update.case).toBe("modelChanged");
  });

  it("DROPS an unchanged replayed arm", async () => {
    const pushes = new SessionPushes(() => 1);
    pushes.push(modelChanged("claude-opus-5"));

    expect(pushes.push(modelChanged("claude-opus-5"))).toBe(false);
  });

  it("delivers a CHANGED replayed arm", () => {
    const pushes = new SessionPushes(() => 1);
    pushes.push(modelChanged("claude-opus-5"));

    expect(pushes.push(modelChanged("claude-sonnet-5"))).toBe(true);
  });

  it("always delivers an event arm, even an identical one", () => {
    const pushes = new SessionPushes(() => 1);
    pushes.push(compacting());

    expect(pushes.push(compacting())).toBe(true);
  });

  it("reaches every attached consumer", async () => {
    const pushes = new SessionPushes(() => 1);
    const one = pushes.subscribe();
    const two = pushes.subscribe();
    pushes.push(modelChanged("claude-opus-5"));

    expect([(await take(one, 2))[1]?.update.case, (await take(two, 2))[1]?.update.case]).toEqual([
      "modelChanged",
      "modelChanged",
    ]);
  });

  it("opens a degraded window when a consumer's queue overflows", () => {
    const pushes = new SessionPushes(() => 1);
    pushes.subscribe();
    for (let index = 0; index < SUBSCRIBER_QUEUE_LIMIT + 2; index++) pushes.push(compacting());

    expect(
      (pushes.diagnostics().update.value as conversationv1.SessionDiagnostics).degradedWindows.length,
    ).toBeGreaterThan(0);
  });
});

describe("faults", () => {
  it("restate the diagnostics to every consumer", async () => {
    const pushes = new SessionPushes(() => 1);
    const read = reader(pushes.subscribe());
    await read.next();

    pushes.fault(fault("the store went away"));

    expect((await read.next())?.update.case).toBe("diagnostics");
  });

  it("accumulate since start", () => {
    const pushes = new SessionPushes(() => 1);
    pushes.fault(fault("one"));
    pushes.fault(fault("two"));

    expect(pushes.faultCount).toBe(2);
  });
});

describe("standing down", () => {
  it("ENDS every consumer's stream", async () => {
    const pushes = new SessionPushes(() => 1);
    const read = reader(pushes.subscribe());
    await read.next();

    pushes.standDown();

    expect(await read.rest()).toEqual([]);
  });

  it("closes a stream opened after the stand-down rather than hanging it", async () => {
    const pushes = new SessionPushes(() => 1);
    pushes.standDown();

    const taken = await reader(pushes.subscribe()).rest();
    expect(taken.map((update) => update.update.case)).toEqual(["diagnostics"]);
  });
});

describe("a component recovering", () => {
  it("clears that component's standing faults", () => {
    const pushes = new SessionPushes(() => 7);
    pushes.fault(
      create(conversationv1.SessionFaultSchema, {
        component: "converter",
        detail: "refused",
        kind: {
          case: "converterDefect",
          value: create(conversationv1.SessionFaultConverterDefectSchema, {}),
        },
      }),
    );

    pushes.resolveComponent("converter", 1);

    expect(pushes.faultCount).toBe(0);
  });

  it("leaves another component's fault standing", () => {
    const pushes = new SessionPushes(() => 7);
    pushes.fault(fault("the store is gone"));

    pushes.resolveComponent("converter", 0);

    expect(pushes.faultCount).toBe(1);
  });

  it("closes that component's open window with the dropped count", async () => {
    const pushes = new SessionPushes(() => 7);
    pushes.openDegradedWindow("converter", "the fold refused a message");

    pushes.resolveComponent("converter", 3);

    const diagnostics = pushes.diagnostics().update;
    const window = diagnostics.case === "diagnostics" ? diagnostics.value.degradedWindows[0] : undefined;
    expect(window?.extent.case === "closed" ? window.extent.value.droppedCount : -1n).toBe(3n);
  });

  it("stamps the close with the clock's instant", () => {
    const pushes = new SessionPushes(() => 7);
    pushes.openDegradedWindow("converter", "the fold refused a message");

    pushes.resolveComponent("converter", 0);

    const diagnostics = pushes.diagnostics().update;
    const window = diagnostics.case === "diagnostics" ? diagnostics.value.degradedWindows[0] : undefined;
    expect(window?.extent.case === "closed" ? window.extent.value.endedAtMs : -1n).toBe(7n);
  });

  it("answers false when nothing was standing for that component", () => {
    const pushes = new SessionPushes(() => 7);

    expect(pushes.resolveComponent("converter", 0)).toBe(false);
  });

  it("pushes the healthy diagnostics to a subscriber", async () => {
    const pushes = new SessionPushes(() => 7);
    pushes.fault(
      create(conversationv1.SessionFaultSchema, {
        component: "converter",
        detail: "refused",
        kind: {
          case: "converterDefect",
          value: create(conversationv1.SessionFaultConverterDefectSchema, {}),
        },
      }),
    );
    const read = reader(pushes.subscribe());
    await read.next();

    pushes.resolveComponent("converter", 1);

    const next = await read.next();
    expect(next?.update.case === "diagnostics" ? next.update.value.health.case : "").toBe("healthy");
  });
});
