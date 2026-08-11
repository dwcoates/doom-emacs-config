/**
 * The page's own answer to "have I recovered?", readable by the host.
 *
 * THE DEFECT THIS ENDS: after a daemon bounce, every existing signal the
 * host could reach said "the socket is open" and nothing said "the daemon's
 * content is landing in this page". Those are different facts, and today's
 * evidence is that they diverge by tens of seconds — a page can hold an open
 * socket, an adopted snapshot, and still be drawing the conversation it held
 * before the bounce. So an open socket is deliberately NOT sufficient here:
 * `satisfied` requires a snapshot ADOPTION *and* at least one CONTENT frame
 * ingested after the recovery epoch opened. Socket-open alone can never
 * satisfy the probe, which is the whole point of the probe existing.
 *
 * WHAT COUNTS AS CONTENT is the adapter's own vocabulary rather than a
 * second opinion invented here: `conversation-items` / `conversation-page`
 * are the conversation deltas, `workspace-state` / `session-view` are the
 * state deltas. Everything else the adapter emits (rosters, chrome,
 * `ignored`) is page furniture that arrives whether or not the daemon's view
 * of THIS workspace has landed, so it does not clear the bar.
 *
 * The host reads this through one global, `agentReplRecoveryProbe`, which
 * returns a JSON string — `xwidget-webkit-execute-script`'s callback carries
 * a scalar back, so a string is the whole contract and lisp parses it.
 */

/** Adapter effect kinds that count as REAL daemon data for this workspace. */
export const REAL_DATA_KINDS: readonly string[] = [
  "conversation-items",
  "conversation-page",
  "workspace-state",
  "session-view",
];

/** The shape the host reads back, one JSON object per probe. */
export interface RecoveryProbeReport {
  /** The workspace this page is addressed at, "" when it has none yet. */
  workspace: string;
  /** Epoch (ms) the current recovery attempt opened at, 0 when never opened. */
  epochAtMs: number;
  /** Whether the page currently holds an open, current socket. */
  socketOpen: boolean;
  /** Whether a snapshot has been ADOPTED at or after `epochAtMs`. */
  adopted: boolean;
  /** When that adoption landed, 0 when none has since the epoch. */
  adoptedAtMs: number;
  /** Content frames ingested at or after `epochAtMs`. */
  realDataFrames: number;
  /** When the first such frame landed, 0 when none has. */
  firstRealDataAtMs: number;
  /** The conjunction: adopted AND content landed. Never socket-open alone. */
  satisfied: boolean;
}

/** What the probe needs from the page to answer at all. */
export interface RecoveryProbeHost {
  now(): number;
  /** The page's workspace address, "" when it is not workspace-addressed. */
  workspace(): string;
  /** Whether the transport reports a live, current socket. */
  socketOpen(): boolean;
}

/**
 * Accumulates the page-side recovery evidence.
 *
 * An EPOCH is what makes the evidence answer the right question. Without one,
 * a page that ingested content an hour ago would report "recovered" the
 * instant the host asked, which is exactly the optimistic flag this module
 * exists to replace. `openEpoch` is called when the page starts recovering —
 * the socket dropping, or the host firing its recovery hook — and every
 * counter below is relative to it.
 */
export class RecoveryProbe {
  private epochAtMs: number;
  private adoptedAtMs = 0;
  private frames = 0;
  private firstAtMs = 0;

  /**
   * CONSTRUCTION IS ITSELF AN EPOCH, and that is a defect fix rather than a
   * convenience. This probe is built once per page boot, and a page boot in
   * this system IS a recovery: the host's webview sweep repairs a workspace
   * by RE-NAVIGATING its webview, so the page that must answer "have I
   * recovered?" is usually a brand-new document. Such a page has no socket
   * drop to open an epoch from, and the host's recover hook already fired at
   * the link-up edge that ordered the re-navigation — so under the previous
   * "epoch starts at 0, evidence before it is discarded" rule it ingested a
   * whole snapshot and every content frame, counted none of them, and
   * reported `satisfied: false` for the rest of its life. Live records show
   * exactly that: `webapp_ms=-1` on every workspace of every bounce.
   *
   * Opening here does not weaken the epoch's purpose. The purpose is that
   * evidence from a DIFFERENT connection cannot prove this one is carrying
   * anything, and a document that did not exist before this boot has no such
   * evidence to inherit. `openEpoch` remains for the case that does: a live
   * page whose socket dropped and is being repaired in place.
   */
  constructor(private readonly host: RecoveryProbeHost) {
    this.epochAtMs = host.now();
  }

  /**
   * Open a new recovery epoch, discarding evidence from the previous one.
   *
   * Evidence is discarded rather than kept because it is evidence about a
   * DIFFERENT connection: content that landed before the link dropped says
   * nothing about whether the link that replaced it is carrying anything.
   */
  openEpoch(): void {
    this.epochAtMs = this.host.now();
    this.adoptedAtMs = 0;
    this.frames = 0;
    this.firstAtMs = 0;
  }

  /** Record that a snapshot was adopted. Ignored before any epoch opened. */
  noteAdopted(): void {
    if (this.epochAtMs === 0) return;
    if (this.adoptedAtMs === 0) this.adoptedAtMs = this.host.now();
  }

  /**
   * Record the content effects in one ingested batch, returning how many
   * counted. Effects outside `REAL_DATA_KINDS` are not content and are not
   * counted — a batch of pure chrome advances nothing here.
   */
  noteBatch(kinds: readonly string[]): number {
    if (this.epochAtMs === 0) return 0;
    let counted = 0;
    for (const kind of kinds) {
      if (REAL_DATA_KINDS.includes(kind)) counted += 1;
    }
    if (counted === 0) return 0;
    this.frames += counted;
    if (this.firstAtMs === 0) this.firstAtMs = this.host.now();
    return counted;
  }

  /** The current evidence, as the host reads it. */
  report(): RecoveryProbeReport {
    const adopted = this.adoptedAtMs > 0;
    const hasData = this.frames > 0;
    return {
      workspace: this.host.workspace(),
      epochAtMs: this.epochAtMs,
      socketOpen: this.host.socketOpen(),
      adopted,
      adoptedAtMs: this.adoptedAtMs,
      realDataFrames: this.frames,
      firstRealDataAtMs: this.firstAtMs,
      satisfied: adopted && hasData,
    };
  }
}

/**
 * Name of the global the host reads the report from.
 * `agent-repl-frontend-recovery-probe-hook' (lisp/recovery-slo.el) MUST
 * match this string — the two names are one contract.
 */
export const RECOVERY_PROBE_HOOK = "agentReplRecoveryProbe";

/** What a hook is planted on: `window`, or a plain object under test. */
export type ProbeGlobal = Record<string, unknown>;

/**
 * Plant the probe hook on TARGET, answering with PROBE's report as JSON.
 *
 * JSON rather than an object because the only channel the host has is
 * `evaluateJavaScript`, whose callback marshals a scalar; a string crosses
 * that boundary intact where a live object does not.
 */
export function installHostRecoveryProbeHook(target: ProbeGlobal, probe: RecoveryProbe): void {
  target[RECOVERY_PROBE_HOOK] = (): string => JSON.stringify(probe.report());
}
