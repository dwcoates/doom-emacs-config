// @vitest-environment jsdom
/**
 * The interrupt (I1), END TO END: protojson `ProgressView` / `WorkspaceState`
 * frames → `decodeFrontendFrame` → `StateAdapter` → `ConversationStore` →
 * `ProgressFooter` → the DOM.
 *
 * The unit coverage already pins each stage in isolation —
 * test/frontend-proto.test.ts decodes the window, test/progress-footer.test.ts
 * builds the chip and the dock from a hand-made `ProgressInput`,
 * test/command-dispatch.test.ts correlates the challenge ack. None of that
 * crosses a stage boundary, and every one of those tests starts from a value
 * some other stage is supposed to have produced.
 *
 * What these tests exist for is the stage-crossing contract:
 *
 * - the wire's three outcomes reach the READER as three different cells —
 *   `.pfooter-interrupt.ok` for the two successes, `.pfooter-interrupt.error`
 *   for the stop that could not be delivered;
 * - the webapp keeps NO interrupt state of its own: the daemon opens the
 *   window and the daemon closes it, and a frame with it closed takes the chip
 *   out of the DOM with nothing left behind;
 * - a chip qualifies the phase cell, it never rewrites it;
 * - live delivery and a connect-time `StateSnapshot` paint the same dock;
 * - `RENDER_STATE_INTERRUPTED` is a CONCLUDED turn everywhere it is drawn —
 *   the footer's green `interrupted` word, the rail's green `st-interrupted`
 *   disc;
 * - an OPEN window with no outcome is REFUSED loudly, and nothing downstream
 *   sees a frame the decoder rejected.
 *
 * Everything here is fed as a protojson envelope, exactly as the socket
 * delivers it: a hand-built adapter input would skip the very decode step half
 * of this contract lives in.
 */
import { afterEach, describe, expect, it } from "vitest";

import { sessionSubagents } from "../src/agents.js";
import { CommandDispatcher, InterruptConfirmRequiredError } from "../src/command-dispatch.js";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { ProgressFooter } from "../src/progress-footer.js";
import { WorkspaceSidebar } from "../src/sidebar.js";
import { StateAdapter } from "../src/state-adapter.js";
import { ConversationStore } from "../src/store.js";
import { ForwardingLogger, resetLoggingForTests, setLogger } from "../src/wslog.js";

afterEach(() => resetLoggingForTests());
import type { SystemFailure } from "../src/frontend-proto.js";

import mainSource from "../src/main.ts?raw";

/** The one workspace and session every frame here belongs to. */
const WS = "/ws/flow";
const SESSION = "sess-interrupt-flow";

/** When the turn under test began. */
const TURN_START_MS = Date.parse("2026-06-01T12:00:00.000Z");

/** The reader's clock, pinned so no elapsed reading can drift mid-test. */
const NOW_MS = TURN_START_MS + 30_000;

/** The clock cell's baked reading, held constant so it never masks a diff. */
const TIMER_LABEL = "0:30";

/** One protojson `ProgressView`, as it rides inside a `FrontendFrame`. */
type WireProgress = Record<string, unknown>;

/**
 * A `ProgressView` for this workspace mid-turn. Every zero-valued field is
 * OMITTED, exactly as protojson omits proto3 defaults — sending them would
 * test a wire shape the daemon never emits.
 */
function progressView(over: WireProgress = {}): WireProgress {
  return {
    workspace: WS,
    sessionId: SESSION,
    turnStartedAtMs: String(TURN_START_MS),
    ...over,
  };
}

/** That view as a whole frame. */
function progressFrame(over: WireProgress = {}): string {
  return JSON.stringify({ progress: progressView(over) });
}

/** An OPEN interrupt window carrying OUTCOME. */
function openWindow(outcome: string): WireProgress {
  return { interrupt: { active: true, sinceMs: String(NOW_MS), outcome } };
}

/** The window CLOSED, which is how the daemon says the interrupt is spent. */
const CLOSED_WINDOW: WireProgress = { interrupt: { active: false } };

/** One protojson `WorkspaceState` for this workspace in STATE. */
function workspaceStateView(state: string, turnActive = false): Record<string, unknown> {
  return {
    workspace: WS,
    sessionId: SESSION,
    state,
    connectivity: "SESSION_CONNECTIVITY_OPERATIONAL",
    status: turnActive ? "SESSION_STATUS_THINKING" : "SESSION_STATUS_DONE",
    controllerGenerationId: "g1",
    activeFaults: [],
    ...(turnActive ? { turnActive: true } : {}),
    atMs: String(NOW_MS),
  };
}

/** That state as a whole frame. */
function workspaceStateFrame(state: string, turnActive = false): string {
  return JSON.stringify({ workspaceState: workspaceStateView(state, turnActive) });
}

/** One end-to-end pipeline: frames in, painted footer out. */
interface Flow {
  el: HTMLElement;
  store: ConversationStore;
  /** Decode + adapt + ingest ONE protojson frame. */
  ingest(raw: string): void;
  /** Repaint the dock from the store, the way main.ts's renderChrome does. */
  paint(): void;
}

/**
 * The real pipeline, wired as `main.ts` wires it: the store's own
 * `progress`/`renderState`/rosters are the footer's whole input, and nothing
 * here derives a progress fact of its own.
 */
function flow(): Flow {
  const el = document.createElement("div");
  const footer = new ProgressFooter(el, () => NOW_MS);
  const store = new ConversationStore(
    () => {},
    () => NOW_MS,
  );
  const adapter = new StateAdapter();
  return {
    el,
    store,
    ingest(raw) {
      store.ingest(adapter.apply(decodeFrontendFrame(raw)));
    },
    paint() {
      const s = store.state;
      footer.render({
        progress: store.progress,
        renderState: s.renderState,
        connectivity: s.sessionConnectivity,
        sessionStatus: s.sessionStatus,
        agents: sessionSubagents(s.items),
        tasks: store.taskRoster,
        items: s.items,
        timerLabel: TIMER_LABEL,
      });
    },
  };
}

/** The interrupt chip currently mounted, or null. */
function chip(el: HTMLElement): HTMLElement | null {
  return el.querySelector<HTMLElement>(".pfooter-interrupt");
}

/** The phase cell currently mounted, or null. */
function phase(el: HTMLElement): HTMLElement | null {
  return el.querySelector<HTMLElement>(".pfooter-phase");
}

describe("an interrupt arriving over a live, thinking footer", () => {
  it("raises the interrupted chip on the open window and drops it when the daemon closes it", () => {
    // Arrange — a turn in flight, drawn: the state the user is looking at when
    // they reach for the stop key.
    const f = flow();
    f.ingest(workspaceStateFrame("RENDER_STATE_THINKING", true));
    f.ingest(progressFrame());
    f.paint();
    // Act — the shim acks the stop, so the daemon opens the window; then the
    // next turn starts and the daemon closes it again. The webapp banks
    // neither event: it renders the window the latest frame reports.
    f.ingest(progressFrame(openWindow("INTERRUPT_OUTCOME_INTERRUPTED")));
    f.paint();
    const raised = chip(f.el);
    f.ingest(progressFrame(CLOSED_WINDOW));
    f.paint();
    // Assert
    expect(raised?.textContent).toBe("interrupted");
    expect(chip(f.el)).toBeNull();
  });
});

describe("a stop that found the turn already over", () => {
  it("draws the calm already-finished chip without touching the phase cell", () => {
    // Arrange — the workspace settled into `done` on its own; the user's stop
    // arrived after. `already finished` is a SUCCESS, and it qualifies that
    // phase rather than replacing it — the phase word stays the workspace's.
    const f = flow();
    f.ingest(workspaceStateFrame("RENDER_STATE_DONE"));
    // Act
    f.ingest(progressFrame(openWindow("INTERRUPT_OUTCOME_ALREADY_COMPLETE")));
    f.paint();
    // Assert
    expect(chip(f.el)?.textContent).toBe("already finished");
    expect(chip(f.el)?.classList.contains("ok")).toBe(true);
    expect(phase(f.el)?.textContent).toBe("done");
  });
});

describe("a stop that could not be delivered", () => {
  it("draws the chip in the error tone, the one outcome that reads as a fault", () => {
    // Arrange
    const f = flow();
    f.ingest(workspaceStateFrame("RENDER_STATE_THINKING", true));
    // Act
    f.ingest(progressFrame(openWindow("INTERRUPT_OUTCOME_FAILED")));
    f.paint();
    // Assert — the word AND the tone: `.pfooter-interrupt.error` is what
    // styles.css paints red (`.pfooter-interrupt.error { color: var(--err) }`).
    expect(chip(f.el)?.textContent).toBe("stop failed");
    expect(chip(f.el)?.classList.contains("error")).toBe(true);
  });
});

describe("the same interrupt delivered live and at connect time", () => {
  it("paints the identical dock either way", () => {
    // Arrange — one sequence, two delivery shapes. A running session receives
    // the state and the view as separate frames; a fresh join receives the
    // daemon's whole resolved world in one `StateSnapshot`. Latest-wins per
    // workspace means these are the same claim, and a reader who reconnects
    // mid-interrupt must not see a different footer for it.
    const window = openWindow("INTERRUPT_OUTCOME_INTERRUPTED");
    const incremental = flow();
    incremental.ingest(workspaceStateFrame("RENDER_STATE_INTERRUPTED"));
    incremental.ingest(progressFrame(window));
    const batched = flow();
    batched.ingest(
      JSON.stringify({
        snapshot: {
          workspaces: [workspaceStateView("RENDER_STATE_INTERRUPTED")],
          progress: [progressView(window)],
        },
      }),
    );
    // Act
    incremental.paint();
    batched.paint();
    // Assert
    expect(incremental.el.innerHTML).toBe(batched.el.innerHTML);
  });
});

describe("a workspace resolved to INTERRUPTED", () => {
  it("names the phase `interrupted` in the green a concluded turn earns", () => {
    // Arrange — the SSM's verdict, the same `WorkspaceState` the Emacs tab bar
    // reads. An interrupted turn is a CONCLUDED turn: the user asked for the
    // stop, got it, and can prompt again — so the word carries the distinction
    // and the color makes the same claim `done` makes.
    const f = flow();
    f.ingest(progressFrame());
    // Act
    f.ingest(workspaceStateFrame("RENDER_STATE_INTERRUPTED"));
    f.paint();
    // Assert
    expect(phase(f.el)?.textContent).toBe("interrupted");
    expect(phase(f.el)?.classList.contains("ok")).toBe(true);
  });
});

describe("an interrupted workspace on the rail", () => {
  it("wears the st-interrupted disc the stylesheet paints green", () => {
    // Arrange — the sidebar is fed by Emacs's pushed roster, NOT by the
    // frontend frame plane, so this is the other half of the same claim:
    // `#ws-sidebar .st-interrupted { background: var(--ok); }` only ever
    // reaches the DOM if the roster path admits the status.
    const mount = document.createElement("div");
    mount.hidden = true;
    const sidebar = new WorkspaceSidebar(mount, { httpBase: "http://localhost:0", now: () => NOW_MS });
    // Act
    sidebar.update({
      view: "repository",
      repos: [
        {
          key: "repo",
          label: "repo",
          folded: false,
          done: false,
          rows: [
            {
              name: "flow",
              dir: WS,
              status: "interrupted",
              closed: false,
              current: true,
              lastViewedAt: null,
              mergedAt: null,
              branch: null,
              parentBranch: null,
              summary: null,
              children: [],
            },
          ],
        },
      ],
      tasks: [],
      recentlyMerged: null,
      navDir: null,
    });
    // Assert
    expect(mount.querySelector(".st.st-interrupted")).not.toBeNull();
  });
});

describe("an OPEN interrupt window carrying no outcome", () => {
  it("is refused at the socket's decode, leaving the store with nothing to paint", () => {
    // Arrange — absent === INTERRUPT_OUTCOME_UNSPECIFIED on the wire, and the
    // outcome is decided ATOMICALLY on the ack that opens the window, so an
    // open window with no outcome is a frame the daemon never sends. Guessing
    // one would invent the very claim it declined to make.
    const f = flow();
    f.ingest(workspaceStateFrame("RENDER_STATE_THINKING", true));
    f.ingest(progressFrame());
    f.paint();
    // Act
    const refuse = (): void =>
      f.ingest(progressFrame({ interrupt: { active: true, sinceMs: String(NOW_MS) } }));
    // Assert — loud, naming the field, and nothing downstream advanced: the
    // last good view is still what the dock shows.
    expect(refuse).toThrow(/ProgressView\.interrupt is open with no outcome/);
    expect(chip(f.el)).toBeNull();
    expect(f.store.progress?.interrupt ?? null).toBeNull();
  });
});

/**
 * `main.ts` is the SPA's top-level wiring with no importable seam, so the
 * disposition of a decode failure is asserted against its source — the idiom
 * test/main.test.ts established for exactly this reason. Local copy, local
 * name: nothing is shared with that file.
 */
function inboundHandlerBodies(source: string): string[] {
  const marker = "onMessage: (data) => {";
  const found: string[] = [];
  for (let at = source.indexOf(marker); at !== -1; at = source.indexOf(marker, at + 1)) {
    const open = source.indexOf("{", at + marker.length - 1);
    let depth = 0;
    for (let i = open; i < source.length; i++) {
      if (source[i] === "{") depth++;
      else if (source[i] === "}" && --depth === 0) {
        found.push(source.slice(open + 1, i));
        break;
      }
    }
  }
  if (found.length === 0) throw new Error("main.ts has no inbound frame handler");
  return found;
}

describe("the session socket's disposition of a refused frame", () => {
  it("logs the refusal as an error and re-throws rather than swallowing it", () => {
    // Arrange — the handler that feeds the store is the one an interrupt frame
    // rides. A decode refusal there is evidence FIRST (the frame head goes to
    // the log) and then propagates: silently dropping it would leave the dock
    // painting a stale window with no record of why.
    const ingesting = inboundHandlerBodies(mainSource).filter((b) => b.includes("store.ingest("));
    expect(ingesting).toHaveLength(1);
    // Act
    const body = ingesting[0];
    // Assert
    expect(body).toMatch(/catch \(err\) \{[\s\S]*?clog\(\s*"error",[\s\S]*?throw err;/);
  });
});

describe("the interrupt confirmation challenge as one arriving frame", () => {
  it("rejects the command with its live-task count and files no failure card", async () => {
    // Arrange — the challenge is the daemon ASKING, not refusing: ok=false with
    // no `failure`. It must reach the caller as a typed rejection carrying the
    // count (so the affordance can ask "interrupt 3 running subagents?") and
    // must reach the conversation as NOTHING — a refusal card here would show
    // the user an error for a question.
    const sent: string[] = [];
    const failures: SystemFailure[] = [];
    setLogger(new ForwardingLogger(() => true, () => {}));
    const dispatcher = new CommandDispatcher({
      send: (raw) => {
        sent.push(raw);
        return true;
      },
      newRequestId: () => "req-1",
      logLocal: () => {},
      onFailure: (f) => failures.push(f),
    });
    const rejected = dispatcher.interrupt(WS).catch((err: unknown) => err);
    const frame = decodeFrontendFrame(
      JSON.stringify({
        commandAck: { requestId: "req-1", ok: false, interruptConfirmRequired: { liveTasks: "3" } },
      }),
    );
    const f = flow();
    // Act — the ONE frame goes to both observers the socket feeds.
    dispatcher.observe(frame);
    f.store.ingest(new StateAdapter().apply(frame));
    f.paint();
    // Assert
    expect(sent).toHaveLength(1);
    expect(failures).toEqual([]);
    expect(f.store.state.items.filter((i) => i.kind === "failure")).toEqual([]);
    const err = await rejected;
    expect(err).toBeInstanceOf(InterruptConfirmRequiredError);
    expect((err as InterruptConfirmRequiredError).liveTasks).toBe(3);
  });
});
