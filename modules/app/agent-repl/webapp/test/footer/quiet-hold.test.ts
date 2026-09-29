// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FooterStatusQuietStretchEndingSchema,
  FooterStatusSchema,
  FooterStripSchema,
  type FooterStatusQuietStretchEnding,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import {
  QUIET_HOLD_DWELL_MS,
  createQuietHold,
  quietStretchEndingOf,
  withHeldLine,
  type QuietHoldDeps,
} from "../../src/footer/quiet-hold.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** An ending held until ROW is painted. */
function ending(row: string, text = "✅ Bash finished — handling result..."): FooterStatusQuietStretchEnding {
  return create(FooterStatusQuietStretchEndingSchema, {
    text,
    untilPainted: { value: row },
    at: { atMs: 1_000n },
  });
}

/** A paint watch the test drives: rows painted at known instants. */
function fakePaints() {
  const painted = new Map<string, number>();
  const listeners = new Set<(id: string, at: number) => void>();
  return {
    watch: {
      paintedAt: (id: string) => painted.get(id) ?? null,
      onPainted: (fn: (id: string, at: number) => void) => {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    },
    paint(id: string, at: number) {
      painted.set(id, at);
      for (const fn of [...listeners]) fn(id, at);
    },
    listeners,
  };
}

function setup(opts: { following?: boolean } = {}) {
  const paints = fakePaints();
  const redraw = vi.fn();
  const deps: QuietHoldDeps = {
    paints: paints.watch,
    followingTail: () => opts.following ?? true,
    now: () => Date.now(),
    setTimer: (fn, ms) => window.setTimeout(fn, ms),
    clearTimer: (handle) => {
      window.clearTimeout(handle);
    },
    redraw,
  };
  return { hold: createQuietHold(deps), paints, redraw };
}

describe("createQuietHold", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(10_000);
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("holds the ended line until its row is painted, then the dwell", () => {
    const { hold, paints, redraw } = setup();
    hold.observe(ending("row-2"));
    vi.advanceTimersByTime(5_000);
    expect(hold.held()?.text).toBe("✅ Bash finished — handling result...");

    paints.paint("row-2", Date.now());
    vi.advanceTimersByTime(QUIET_HOLD_DWELL_MS - 1);
    expect(hold.held()).not.toBeNull();
    vi.advanceTimersByTime(1);

    expect(hold.held()).toBeNull();
    expect(redraw).toHaveBeenCalledTimes(1);
  });

  it("counts the dwell from a paint that landed before the ending", () => {
    const { hold, paints } = setup();
    paints.paint("row-2", Date.now() - 200);

    hold.observe(ending("row-2"));
    vi.advanceTimersByTime(QUIET_HOLD_DWELL_MS - 201);
    expect(hold.held()).not.toBeNull();
    vi.advanceTimersByTime(1);

    expect(hold.held()).toBeNull();
  });

  it("releases at once for a row painted longer ago than the dwell", () => {
    const { hold, paints } = setup();
    paints.paint("row-2", Date.now() - 2 * QUIET_HOLD_DWELL_MS);

    hold.observe(ending("row-2"));
    vi.advanceTimersByTime(0);

    expect(hold.held()).toBeNull();
  });

  it("holds nothing for a reader not following the live tail", () => {
    const { hold, redraw } = setup({ following: false });

    hold.observe(ending("row-2"));

    expect(hold.held()).toBeNull();
    expect(redraw).not.toHaveBeenCalled();
  });

  it("is released, without a redraw of its own, by a push stating no ending", () => {
    const { hold, paints, redraw } = setup();
    hold.observe(ending("row-2"));

    hold.observe(undefined);

    expect(hold.held()).toBeNull();
    expect(redraw).not.toHaveBeenCalled();
    expect(paints.listeners.size).toBe(0);
  });

  it("takes another row's ending in place of the one it held", () => {
    const { hold } = setup();
    hold.observe(ending("row-2"));

    hold.observe(ending("row-3", "✅ Read finished — handling result..."));

    expect(hold.held()?.untilPainted?.value).toBe("row-3");
  });

  it("never holds a row again once its hold was released", () => {
    const { hold, paints } = setup();
    hold.observe(ending("row-2"));
    paints.paint("row-2", Date.now());
    vi.advanceTimersByTime(QUIET_HOLD_DWELL_MS);

    hold.observe(ending("row-2"));

    expect(hold.held()).toBeNull();
  });

  it("keeps one hold across repeated pushes of the same ending", () => {
    const { hold, paints } = setup();
    hold.observe(ending("row-2"));
    hold.observe(ending("row-2"));
    paints.paint("row-2", Date.now());

    vi.advanceTimersByTime(QUIET_HOLD_DWELL_MS);

    expect(hold.held()).toBeNull();
    expect(paints.listeners.size).toBe(0);
  });

  it("ignores the paint of a row it does not hold", () => {
    const { hold, paints } = setup();
    hold.observe(ending("row-2"));

    paints.paint("row-9", Date.now());
    vi.advanceTimersByTime(QUIET_HOLD_DWELL_MS * 4);

    expect(hold.held()?.untilPainted?.value).toBe("row-2");
  });

  it("is released by dispose, and its timer never fires", () => {
    const { hold, paints, redraw } = setup();
    hold.observe(ending("row-2"));
    paints.paint("row-2", Date.now());

    hold.dispose();
    vi.advanceTimersByTime(QUIET_HOLD_DWELL_MS);

    expect(hold.held()).toBeNull();
    expect(redraw).not.toHaveBeenCalled();
  });

  it("records the release at debug with its reason", async () => {
    const capture = captureLogRecords("debug");
    const { hold } = setup();
    hold.observe(ending("row-2"));

    hold.observe(undefined);

    const record = await forwardedRecord(capture, "footer.quiet-hold-released");
    expect(record.level.case).toBe("debug");
    expect(JSON.stringify(record)).toContain("the push states no ending");
  });
});

describe("quietStretchEndingOf", () => {
  it("reads the working arm's ending", () => {
    const status = create(FooterStatusSchema, {
      status: { case: "working", value: { quietStretchEnding: ending("row-2") } },
    });
    expect(quietStretchEndingOf(status)?.untilPainted?.value).toBe("row-2");
  });

  it("reads the background arm's ending", () => {
    const status = create(FooterStatusSchema, {
      status: { case: "background", value: { quietStretchEnding: ending("row-2") } },
    });
    expect(quietStretchEndingOf(status)?.untilPainted?.value).toBe("row-2");
  });

  it("answers none for an arm that carries no ending", () => {
    const status = create(FooterStatusSchema, { status: { case: "idle", value: {} } });
    expect(quietStretchEndingOf(status)).toBeUndefined();
  });
});

describe("withHeldLine", () => {
  it("draws the held line in the working arm's activity, with its own instant", () => {
    const strip = create(FooterStripSchema, {
      status: { status: { case: "working", value: { activity: undefined } } },
    });

    const out = withHeldLine(strip, ending("row-2"));

    const status = out.status?.status;
    expect(status?.case).toBe("working");
    const activity = status?.case === "working" ? status.value.activity : undefined;
    expect(activity?.kind.case).toBe("quietStretch");
    expect(activity?.kind.value).toMatchObject({ text: "✅ Bash finished — handling result..." });
    expect(activity?.at?.atMs).toBe(1_000n);
  });

  it("draws the held line in the background arm's activity", () => {
    const strip = create(FooterStripSchema, {
      status: { status: { case: "background", value: {} } },
    });

    const out = withHeldLine(strip, ending("row-2", "✅ Subagent finished"));

    const status = out.status?.status;
    const activity = status?.case === "background" ? status.value.activity : undefined;
    expect(activity?.kind.case).toBe("quietStretch");
  });

  it("leaves the pushed strip itself untouched", () => {
    const strip = create(FooterStripSchema, {
      status: { status: { case: "working", value: {} } },
    });

    withHeldLine(strip, ending("row-2"));

    const status = strip.status?.status;
    expect(status?.case === "working" ? status.value.activity : "unset").toBeUndefined();
  });

  it("leaves an arm that carries no ending as pushed", () => {
    const strip = create(FooterStripSchema, { status: { status: { case: "idle", value: {} } } });
    expect(withHeldLine(strip, ending("row-2"))).toBe(strip);
  });
});
