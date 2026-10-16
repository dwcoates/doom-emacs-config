/**
 * The shim's RUNTIME CPU and heap profiling.
 *
 * bin/profile-agent-repl.sh opens a session and sends SIGWINCH; the shim then
 * starts a V8 CPU profile and a sampling heap profile through its own
 * inspector session, and writes `shim-<pid>.cpuprofile` and
 * `shim-<pid>.heapprofile` into the session directory when the session closes
 * (or after `MAX_SECONDS`). The contract is
 * `agent-shim/logging/ts/profile-session.ts`.
 *
 * OFF COSTS NOTHING: no inspector session exists and nothing is polled until a
 * signal arrives. A shim that exits mid-profile writes no files; close the
 * session before standing a shim down to keep its profile.
 */
import { mkdirSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { Session } from "node:inspector/promises";
import { join } from "node:path";

import { ACTIVE_FILE, ARMED_DIR, LOG_LEVEL_FILE, MAX_SECONDS, profilesRoot, validSession } from "../../../logging/ts/profile-session.js";
import { parseWindowLevel, WINDOW_SECONDS, type LevelExpiry, type WindowLevel } from "../../../logging/ts/level-window.js";

/** What a reconcile did. */
export type ProfilingEventKind = "started" | "stopped" | "failed" | "level_opened" | "level_closed";

/** The runtime's live log threshold a session's LOG_LEVEL_FILE raises (LevelWindow). */
export interface LevelWindowPort {
  open(level: WindowLevel, lastingMs: number): number;
  close(): LevelExpiry | null;
}

/** One thing a reconcile did, for the shim to record: started and stopped at info, failed at error. */
export interface ProfilingEvent {
  kind: ProfilingEventKind;
  session: string;
  /** Why a profile stopped: session_closed, session_changed, max_duration or exit. */
  reason?: string;
  files: string[];
  /** The log level a level_opened or level_closed event is about. */
  level?: WindowLevel;
  /** When that window ends (level_opened) or ended (level_closed), epoch ms. */
  untilMs?: number;
  error?: Error;
}

/** The part of an inspector session the profiler drives. */
export interface InspectorPort {
  connect(): void;
  post(method: string, params?: object): Promise<unknown>;
  disconnect(): void;
}

/** A scheduled max-duration stop. */
export interface StopTimer {
  clear(): void;
}

export interface RuntimeProfilerOptions {
  runtime: string;
  root: string;
  pid: number;
  events: (event: ProfilingEvent) => void;
  /** The runtime's live log threshold, which a session's LOG_LEVEL_FILE raises. */
  level: LevelWindowPort;
  /** A fresh inspector session; the default is node's own. */
  inspector?: () => InspectorPort;
  /** Schedules the max-duration stop; the default is an unref'd setTimeout. */
  after?: (ms: number, fn: () => void) => StopTimer;
  maxSeconds?: number;
}

const defaultAfter = (ms: number, fn: () => void): StopTimer => {
  const handle = setTimeout(fn, ms);
  handle.unref();
  return { clear: () => clearTimeout(handle) };
};

/** One runtime's profiling state. Reconciles are serialized. */
export class RuntimeProfiler {
  private readonly opts: Required<RuntimeProfilerOptions>;
  private session = "";
  private port: InspectorPort | undefined;
  private timer: StopTimer | undefined;
  private generation = 0;
  /** Whether this profile's session opened a log level window, which its stop closes. */
  private levelOpen = false;
  private chain: Promise<void> = Promise.resolve();

  constructor(opts: RuntimeProfilerOptions) {
    if (!opts.runtime) throw new Error("profiling: a profiler needs its runtime's name");
    if (!opts.root) throw new Error("profiling: a profiler needs the profiles root");
    this.opts = {
      ...opts,
      inspector: opts.inspector ?? (() => new Session()),
      after: opts.after ?? defaultAfter,
      maxSeconds: opts.maxSeconds ?? MAX_SECONDS,
    };
  }

  /** The session being profiled into, empty when none is. */
  current(): string {
    return this.session;
  }

  /** Makes the profiler match the root's active file. */
  reconcile(): Promise<void> {
    return this.serialize(() => this.reconcileNow());
  }

  /** Ends a running profile and writes its files; nothing when none runs. */
  stop(reason: string): Promise<void> {
    return this.serialize(async () => {
      if (this.session) await this.stopNow(reason);
    });
  }

  /** Writes the armed marker and answers its path. */
  arm(): string {
    const dir = join(this.opts.root, ARMED_DIR);
    mkdirSync(dir, { recursive: true });
    const marker = join(dir, `${this.opts.runtime}-${this.opts.pid}`);
    writeFileSync(marker, `${this.opts.runtime}\n`);
    return marker;
  }

  private serialize(step: () => Promise<void>): Promise<void> {
    const next = this.chain.then(step);
    this.chain = next.catch(() => undefined);
    return next;
  }

  private activeSession(): string {
    let raw: string;
    try {
      raw = readFileSync(join(this.opts.root, ACTIVE_FILE), "utf8");
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === "ENOENT") return "";
      throw new Error(`profiling: read the active session: ${(err as Error).message}`, { cause: err });
    }
    const id = raw.trim();
    if (!validSession(id)) throw new Error(`profiling: the active file names no valid session: ${JSON.stringify(id)}`);
    return id;
  }

  private async reconcileNow(): Promise<void> {
    let want: string;
    try {
      want = this.activeSession();
    } catch (err) {
      this.opts.events({ kind: "failed", session: this.session, files: [], error: err as Error });
      return;
    }
    if (want === this.session) return;
    if (this.session) await this.stopNow(want ? "session_changed" : "session_closed");
    if (want) await this.startNow(want);
  }

  private async startNow(session: string): Promise<void> {
    const port = this.opts.inspector();
    try {
      mkdirSync(join(this.opts.root, session), { recursive: true });
      port.connect();
      await port.post("Profiler.enable");
      await port.post("Profiler.start");
      await port.post("HeapProfiler.enable");
      await port.post("HeapProfiler.startSampling");
    } catch (err) {
      port.disconnect();
      this.opts.events({ kind: "failed", session, files: [], error: err as Error });
      return;
    }
    this.session = session;
    this.port = port;
    this.generation += 1;
    const armed = this.generation;
    this.timer = this.opts.after(this.opts.maxSeconds * 1000, () => {
      void this.serialize(async () => {
        if (this.session && this.generation === armed) await this.stopNow("max_duration");
      });
    });
    this.opts.events({ kind: "started", session, files: [] });
    this.openLevel(session);
  }

  /** Opens the log level window SESSION's LOG_LEVEL_FILE names, when it names one. */
  private openLevel(session: string): void {
    let raw: string;
    try {
      raw = readFileSync(join(this.opts.root, session, LOG_LEVEL_FILE), "utf8");
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === "ENOENT") return;
      this.opts.events({ kind: "failed", session, files: [], error: new Error(`profiling: read the session's log level: ${(err as Error).message}`, { cause: err }) });
      return;
    }
    try {
      const level = parseWindowLevel(raw.trim(), "the session's log level");
      const untilMs = this.opts.level.open(level, WINDOW_SECONDS * 1000);
      this.levelOpen = true;
      this.opts.events({ kind: "level_opened", session, files: [], level, untilMs });
    } catch (err) {
      this.opts.events({ kind: "failed", session, files: [], error: err as Error });
    }
  }

  private async stopNow(reason: string): Promise<void> {
    const session = this.session;
    if (this.levelOpen) {
      this.levelOpen = false;
      const closed = this.opts.level.close();
      if (closed !== null) this.opts.events({ kind: "level_closed", session, reason, files: [], level: closed.from, untilMs: closed.untilMs });
    }
    const port = this.port as InspectorPort;
    this.timer?.clear();
    this.session = "";
    this.port = undefined;
    this.timer = undefined;
    const dir = join(this.opts.root, session);
    const base = `${this.opts.runtime}-${this.opts.pid}`;
    const files: string[] = [];
    try {
      const cpu = (await port.post("Profiler.stop")) as { profile: unknown };
      const cpuPath = join(dir, `${base}.cpuprofile`);
      writeFileSync(cpuPath, JSON.stringify(cpu.profile));
      files.push(cpuPath);
      const heap = (await port.post("HeapProfiler.stopSampling")) as { profile: unknown };
      const heapPath = join(dir, `${base}.heapprofile`);
      writeFileSync(heapPath, JSON.stringify(heap.profile));
      files.push(heapPath);
    } catch (err) {
      port.disconnect();
      this.opts.events({ kind: "failed", session, reason, files, error: err as Error });
      return;
    }
    port.disconnect();
    this.opts.events({ kind: "stopped", session, reason, files });
  }
}

/** The logger shape the shim records profiling events with. */
export interface ProfilingLogger {
  info(fields: Record<string, unknown>, message: string): void;
  error(fields: Record<string, unknown>, message: string): void;
}

/** Records one profiling event: a start, a stop, or a session's log level window opening or closing at info; a failure at error. */
export function recordProfilingEvent(log: ProfilingLogger, event: ProfilingEvent): void {
  const fields: Record<string, unknown> = { session: event.session, files: event.files };
  if (event.reason) fields.reason = event.reason;
  switch (event.kind) {
    case "started":
      log.info({ ...fields, operation: "shim.profiling.started" }, "runtime profiling started into the open session");
      return;
    case "stopped":
      log.info({ ...fields, operation: "shim.profiling.stopped" }, "runtime profiling stopped and wrote its profiles");
      return;
    case "level_opened":
      log.info(
        { ...fields, operation: "shim.profiling.level_opened", level: event.level, until: Math.floor((event.untilMs ?? 0) / 1000) },
        "the profiling session raised the shim's log level for its window",
      );
      return;
    case "level_closed":
      log.info(
        { ...fields, operation: "shim.profiling.level_closed", level: event.level },
        "the profiling session's log level window closed; the shim is back at info",
      );
      return;
    default:
      log.error(
        { ...fields, operation: "shim.profiling.failed", cause: String(event.error?.message ?? event.error) },
        "runtime profiling could not do what the session asked",
      );
  }
}

/** Removes the armed marker; a failure other than its absence is recorded. */
export function disarm(marker: string, log: ProfilingLogger): void {
  try {
    rmSync(marker);
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return;
    log.error({ operation: "shim.profiling.failed", cause: (err as Error).message }, "could not remove the profiling armed marker");
  }
}

/** What arming needs from the process; the defaults are the real process. */
export interface ArmDeps {
  env: Readonly<Record<string, string | undefined>>;
  home: string;
  pid: number;
  onSignal: (signal: "SIGWINCH", handler: () => void) => void;
  onExit: (handler: () => void) => void;
  /** The shim's live log threshold (log.ts, logLevelWindow). */
  level: LevelWindowPort;
}

/**
 * Arms the shim's runtime profiling: writes its armed marker, reconciles on
 * SIGWINCH, and removes the marker as the process exits. A shim that cannot
 * arm records why at error and runs on unprofiled; profiling is never a
 * reason to refuse a session.
 */
export function armRuntimeProfiling(log: ProfilingLogger, deps: ArmDeps): RuntimeProfiler | undefined {
  let profiler: RuntimeProfiler;
  let marker: string;
  try {
    profiler = new RuntimeProfiler({
      runtime: "shim",
      root: profilesRoot(deps.env, deps.home),
      pid: deps.pid,
      events: (event) => recordProfilingEvent(log, event),
      level: deps.level,
    });
    marker = profiler.arm();
  } catch (err) {
    log.error({ operation: "shim.profiling.failed", cause: (err as Error).message }, "runtime profiling could not be armed");
    return undefined;
  }
  deps.onSignal("SIGWINCH", () => {
    void profiler.reconcile();
  });
  deps.onExit(() => disarm(marker, log));
  return profiler;
}
