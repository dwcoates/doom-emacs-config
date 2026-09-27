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
import { nextPush } from "../next-push.js";
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
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
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    const [first] = await take(pushes.subscribe(), 1);

    expect(first?.update.case).toBe("diagnostics");
  });

  it("reports healthy when nothing has faulted", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    const [first] = await take(pushes.subscribe(), 1);

    expect(first?.update.case === "diagnostics" ? first.update.value.health.case : undefined).toBe(
      "healthy",
    );
  });

  it("carries the shim's build identity on the opening diagnostics frame", async () => {
    // An inert shim -- no session started, no faults recorded -- still opens
    // every WatchSession with a diagnostics frame stamped with its own build,
    // since that is the only readiness signal the daemon's deploy can compare
    // against a freshly built bundle before any session exists.
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    const [first] = await take(pushes.subscribe(), 1);

    expect(first?.update.case === "diagnostics" ? first.update.value.shimBuild : undefined).toBe(
      "test-build-sha",
    );
  });

  it("carries the shim's build identity on every subsequent diagnostics restatement", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    pushes.fault(fault("the store went away"));
    const restated = pushes.diagnostics();

    expect(restated.update.case === "diagnostics" ? restated.update.value.shimBuild : undefined).toBe(
      "test-build-sha",
    );
  });

  it("gives a SECOND concurrent subscriber diagnostics first too", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const first = pushes.subscribe();
    await take(first, 1);

    const [second] = await take(pushes.subscribe(), 1);

    expect(second?.update.case).toBe("diagnostics");
  });

  it("reports unhealthy with the faults kept since start", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.fault(fault("the store went away"));

    const [first] = await take(pushes.subscribe(), 1);

    expect(
      first?.update.case === "diagnostics" && first.update.value.health.case === "unhealthy"
        ? first.update.value.health.value.faults.map((f) => f.detail)
        : undefined,
    ).toEqual(["the store went away"]);
  });

  it("catches a late joiner up on the current model", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(modelChanged("claude-opus-5"));

    const taken = await take(pushes.subscribe(), 2);

    expect(taken[1]?.update.case).toBe("modelChanged");
  });

  it("does not replay an event arm to a late joiner", async () => {
    // Two identical compactions are two compactions; replaying one to a late
    // joiner would announce a compaction that is not happening.
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(compacting());

    const taken = await take(pushes.subscribe(), 1);

    expect(taken.map((update) => update.update.case)).toEqual(["diagnostics"]);
  });

  it("counts its subscribers", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.subscribe();
    pushes.subscribe();

    expect(pushes.subscriberCount).toBe(2);
  });
});

describe("construction", () => {
  it("refuses to build with an empty shim build identity", () => {
    // SessionDiagnostics.shim_build is REQUIRED on every frame; an empty
    // string here would otherwise ride the wire as a malformed frame, so the
    // construction site fails loudly instead of silently proceeding.
    expect(() => new SessionPushes(() => 1, "")).toThrow(/shimBuildSha is required/);
  });
});

describe("pushing", () => {
  it("reaches an attached consumer", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const stream = pushes.subscribe();
    pushes.push(modelChanged("claude-opus-5"));

    const taken = await take(stream, 2);

    expect(taken[1]?.update.case).toBe("modelChanged");
  });

  it("DROPS an unchanged replayed arm", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(modelChanged("claude-opus-5"));

    expect(pushes.push(modelChanged("claude-opus-5"))).toBe(false);
  });

  it("delivers a CHANGED replayed arm", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(modelChanged("claude-opus-5"));

    expect(pushes.push(modelChanged("claude-sonnet-5"))).toBe(true);
  });

  it("always delivers an event arm, even an identical one", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(compacting());

    expect(pushes.push(compacting())).toBe(true);
  });

  it("reaches every attached consumer", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const one = pushes.subscribe();
    const two = pushes.subscribe();
    pushes.push(modelChanged("claude-opus-5"));

    expect([(await take(one, 2))[1]?.update.case, (await take(two, 2))[1]?.update.case]).toEqual([
      "modelChanged",
      "modelChanged",
    ]);
  });

  it("opens a degraded window when a consumer's queue overflows", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.subscribe();
    for (let index = 0; index < SUBSCRIBER_QUEUE_LIMIT + 2; index++) pushes.push(compacting());

    expect(
      (pushes.diagnostics().update.value as conversationv1.SessionDiagnostics).degradedWindows.length,
    ).toBeGreaterThan(0);
  });
});

describe("faults", () => {
  it("restate the diagnostics to every consumer", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const read = reader(pushes.subscribe());
    await read.next();

    pushes.fault(fault("the store went away"));

    expect((await read.next())?.update.case).toBe("diagnostics");
  });

  it("accumulate distinct components since start", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
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
    pushes.fault(fault("the store went away"));

    expect(pushes.faultCount).toBe(2);
  });

  it("a repeat of the same component and kind REPLACES the standing fault rather than stacking", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.fault(fault("one"));
    pushes.fault(fault("two"));

    expect(pushes.faultCount).toBe(1);
    const diagnostics = pushes.diagnostics().update;
    const faults =
      diagnostics.case === "diagnostics" && diagnostics.value.health.case === "unhealthy"
        ? diagnostics.value.health.value.faults
        : [];
    expect(faults.map((entry) => entry.detail)).toEqual(["two"]);
  });

  it("a different kind on the same component is a SECOND fault", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.fault(fault("the store went away"));
    pushes.fault(
      create(conversationv1.SessionFaultSchema, {
        component: "test",
        detail: "the converter refused",
        kind: {
          case: "converterDefect",
          value: create(conversationv1.SessionFaultConverterDefectSchema, {}),
        },
      }),
    );

    expect(pushes.faultCount).toBe(2);
  });

  it("logs the first occurrence of a fault at error", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const before = logSinkMark();

    pushes.fault(fault("the store went away"));

    expect(
      logRecordsSince(before)
        .filter((record) => record.message === "recorded a session fault")
        .map((record) => record.level),
    ).toEqual(["error"]);
  });

  it("logs a repeat at debug, carrying the repeat count", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.fault(fault("one"));
    const before = logSinkMark();

    pushes.fault(fault("two"));

    expect(
      logRecordsSince(before)
        .filter((record) => record.message === "recorded a session fault")
        .map((record) => ({ level: record.level, repeats: record.context.repeats })),
    ).toEqual([{ level: "debug", repeats: 2 }]);
  });
});

describe("standing down", () => {
  it("ENDS every consumer's stream", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const read = reader(pushes.subscribe());
    await read.next();

    pushes.standDown();

    expect(await read.rest()).toEqual([]);
  });

  it("closes a stream opened after the stand-down rather than hanging it", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.standDown();

    const taken = await reader(pushes.subscribe()).rest();
    expect(taken.map((update) => update.update.case)).toEqual(["diagnostics"]);
  });
});

describe("a component recovering", () => {
  it("clears that component's standing faults", () => {
    const pushes = new SessionPushes(() => 7, "test-build-sha");
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
    const pushes = new SessionPushes(() => 7, "test-build-sha");
    pushes.fault(fault("the store is gone"));

    pushes.resolveComponent("converter", 0);

    expect(pushes.faultCount).toBe(1);
  });

  it("closes that component's open window with the dropped count", async () => {
    const pushes = new SessionPushes(() => 7, "test-build-sha");
    pushes.openDegradedWindow("converter", "the fold refused a message");

    pushes.resolveComponent("converter", 3);

    const diagnostics = pushes.diagnostics().update;
    const window = diagnostics.case === "diagnostics" ? diagnostics.value.degradedWindows[0] : undefined;
    expect(window?.extent.case === "closed" ? window.extent.value.droppedCount : -1n).toBe(3n);
  });

  it("stamps the close with the clock's instant", () => {
    const pushes = new SessionPushes(() => 7, "test-build-sha");
    pushes.openDegradedWindow("converter", "the fold refused a message");

    pushes.resolveComponent("converter", 0);

    const diagnostics = pushes.diagnostics().update;
    const window = diagnostics.case === "diagnostics" ? diagnostics.value.degradedWindows[0] : undefined;
    expect(window?.extent.case === "closed" ? window.extent.value.endedAtMs : -1n).toBe(7n);
  });

  it("answers false when nothing was standing for that component", () => {
    const pushes = new SessionPushes(() => 7, "test-build-sha");

    expect(pushes.resolveComponent("converter", 0)).toBe(false);
  });

  it("pushes the healthy diagnostics to a subscriber", async () => {
    const pushes = new SessionPushes(() => 7, "test-build-sha");
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

describe("fast mode", () => {
  function fastMode(on: boolean): conversationv1.SessionUpdate {
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "fastMode",
        value: create(conversationv1.SessionFastModeSchema, {
          state: on
            ? { case: "on", value: create(conversationv1.SessionFastModeOnSchema, {}) }
            : { case: "off", value: create(conversationv1.SessionFastModeOffSchema, { reason: "preference" }) },
        }),
      },
    });
  }

  it("is replayed to a consumer that joins after the vendor stated it", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(fastMode(true));

    const opening = await take(pushes.subscribe(), 2);

    expect(opening[1]?.update.case).toBe("fastMode");
  });

  it("is dropped when the state did not change", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(fastMode(true));

    expect(pushes.push(fastMode(true))).toBe(false);
  });

  it("goes out when the state changed", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(fastMode(true));

    expect(pushes.push(fastMode(false))).toBe(true);
  });
});

describe("the vendor's title for the conversation", () => {
  function title(text: string): conversationv1.SessionUpdate {
    return create(conversationv1.SessionUpdateSchema, {
      update: { case: "title", value: create(conversationv1.SessionTitleSchema, { text }) },
    });
  }

  it("is replayed to a consumer that joins after the shim read it", async () => {
    // THE DAEMON IS ALWAYS THAT CONSUMER: the title is read during
    // StartSession and the daemon's standing WatchSession opens after
    // StartSession has answered, so without the replay it would never see it.
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(title("Add SPC j keybinding support"));

    const opening = await take(pushes.subscribe(), 2);

    expect(opening[1]?.update.case).toBe("title");
  });

  it("is dropped when the vendor restated the same title", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(title("the one title"));

    expect(pushes.push(title("the one title"))).toBe(false);
  });

  it("goes out when the vendor changed its mind", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(title("first guess"));

    expect(pushes.push(title("what it turned out to be"))).toBe(true);
  });
});

describe("account usage", () => {
  function accountUsage(observedAtMs: bigint, utilizationPercent: number): conversationv1.SessionUpdate {
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "accountUsage",
        value: create(conversationv1.SessionAccountUsageSchema, {
          observedAtMs,
          subscriptionType: "max",
          outcome: {
            case: "available",
            value: create(conversationv1.SessionAccountUsageAvailableSchema, {
              fiveHour: create(conversationv1.SessionUsageWindowSchema, {
                utilizationPercent,
                resetsAtMs: 1_700_000_000_000n,
              }),
            }),
          },
        }),
      },
    });
  }

  // THE DEFECT THIS PINS: the session probes the account's usage ONCE at
  // StartSession, and the daemon opens its standing WatchSession only after
  // StartSession has answered. With this arm unreplayed that first sample
  // reached nobody, so the footer drew no allowance figure until a turn
  // closed and reprobed — the whole of a fresh session's usage line, missing.
  it("is replayed to a consumer that joins after the session probed it", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(accountUsage(1n, 41));

    const opening = await take(pushes.subscribe(), 2);

    expect(opening[1]?.update.case).toBe("accountUsage");
  });

  it("goes out again for a later sample, which never repeats an instant", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.push(accountUsage(1n, 41));

    expect(pushes.push(accountUsage(2n, 41))).toBe(true);
  });
});

describe("a consumer that goes away", () => {
  it("ends the stream when the iterator's own return() is called directly", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const iterator = pushes.subscribe()[Symbol.asyncIterator]();
    await iterator.next();

    const result = await iterator.return?.();

    expect(result).toEqual({ value: undefined, done: true });
    await expect(iterator.next()).resolves.toEqual({ value: undefined, done: true });
  });

  // A CANCELLED CONSUMER IS GONE FROM THE FAN-OUT, not merely unblocked. A
  // daemon that gives up an adoption cancels its standing WatchSession while
  // nothing is pending, and a subscriber left in the set would go on taking
  // every later fact into a queue nobody drains.
  it("drops the subscriber from the fan-out when it leaves with nothing pending", async () => {
    // Arrange.
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const iterator = pushes.subscribe()[Symbol.asyncIterator]();
    await iterator.next();
    const pending = iterator.next();

    // Act.
    await iterator.return?.();

    // Assert.
    await expect(pending).resolves.toEqual({ value: undefined, done: true });
    expect(pushes.subscriberCount).toBe(0);
  });

  it("also ends via a for-await break, which the runtime maps to return()", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    for await (const _ of pushes.subscribe()) {
      break;
    }

    // The fault push after the break must not throw or hang the fan-out that
    // lost this subscriber.
    expect(() => pushes.fault(fault("after the consumer left"))).not.toThrow();
  });
});

describe("the default clock", () => {
  it("stamps a degraded window with a real wall-clock time when none is injected", () => {
    const pushes = new SessionPushes(undefined, "test-build-sha");
    const before = Date.now();

    const window = pushes.openDegradedWindow("test", "no clock injected");

    expect(Number(window.beganAtMs)).toBeGreaterThanOrEqual(before);
  });
});

describe("pushing after the stand-down", () => {
  it("REPORTS the undeliverable fact as a degraded window rather than dropping it silently", () => {
    // A stream opened after the stand-down is closed on arrival but still
    // attached, so the next fact cannot reach it — and a consumer's view having
    // a hole is exactly what a degraded window states.
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.standDown();
    pushes.subscribe();

    pushes.push(compacting());

    const diagnostics = pushes.diagnostics().update;
    expect(
      diagnostics.case === "diagnostics" ? diagnostics.value.degradedWindows.length : undefined,
    ).toBe(1);
  });
});

describe("a session fact carrying no arm", () => {
  it("still reaches an attached consumer rather than being swallowed as unchanged", async () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    const stream = pushes.subscribe()[Symbol.asyncIterator]();
    await stream.next();

    pushes.push(create(conversationv1.SessionUpdateSchema, {}));

    expect((await nextPush(stream)).update.case).toBeUndefined();
  });
});

describe("a fault that names no kind", () => {
  it("is still recorded, and still makes the session unhealthy", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");

    pushes.fault(create(conversationv1.SessionFaultSchema, { component: "test", detail: "why" }));

    const diagnostics = pushes.diagnostics().update;
    expect(diagnostics.case === "diagnostics" ? diagnostics.value.health.case : undefined).toBe(
      "unhealthy",
    );
  });
});

describe("recovering a component whose window is already closed", () => {
  it("answers false, so the caller does not restate an unchanged verdict", () => {
    const pushes = new SessionPushes(() => 1, "test-build-sha");
    pushes.recordDegradedWindow(
      create(conversationv1.SessionDegradedWindowSchema, {
        component: "store-writer",
        reason: "already over",
        beganAtMs: 1n,
        extent: {
          case: "closed",
          value: create(conversationv1.SessionDegradedClosedSchema, {
            endedAtMs: 2n,
            droppedCount: 0n,
          }),
        },
      }),
    );

    expect(pushes.resolveComponent("store-writer", 0)).toBe(false);
  });
});
