/**
 * The paint attestation: this frontend's answer for one render pass (F5).
 *
 * WHAT IT ANSWERS FOR. The daemon tracks state; a frontend decides when that
 * state is RENDERABLE. Nothing on the daemon's side of the wire can tell a
 * webview that received every frame and drew them from one that received every
 * frame and drew nothing, so it asks — and it withholds the state from the
 * Emacs tab bar until this end answers, so the two surfaces cannot show
 * different states at the same moment.
 *
 * WHY IT IS A MODULE. Two watermarks, two outcomes and a visibility rule is a
 * policy, and a policy scattered across a render callback and a socket handler
 * is a policy nobody can check. It lives here so it is one readable thing with
 * one test file.
 *
 * THE SUSPENDED ANSWER. A mounted-but-hidden webview has no
 * requestAnimationFrame (a 13.6-second render stall was measured directly from
 * the client logs, visibility hidden), so it CANNOT paint — but it can still
 * receive frames and still answer on its socket. Going silent would wedge the
 * tab bar behind a webview nobody is looking at, and the only way out of that
 * would be a delivery timeout, which is a guess dressed as a mechanism. So it
 * answers honestly instead: "I hold this generation and I cannot draw it."
 * That settles the state's delivery and attests no paint, which is exactly the
 * pair of facts that are true.
 *
 * INTERIM DISPLAY, chosen deliberately: while this end is hidden, Emacs shows
 * the current state, the same as when no webview is mounted at all. The
 * divergence the sequencer exists to prevent is a VISIBLE one — two surfaces on
 * screen disagreeing — and a hidden webview presents no surface to disagree
 * with. Freezing the tab bar to protect against a comparison nobody can make
 * would be strictly worse: it would show a state that is actually wrong.
 *
 * ON REGAINING VISIBILITY the page repaints the current snapshot and then acks
 * PAINTED, in that order, so the daemon never learns this end is painting again
 * until it has actually drawn what it holds.
 */
import type { PaintOutcome } from "./frontend-command.js";

/** What one render pass has in hand. */
export interface PaintSnapshot {
  /** The conversation through-seq the store holds. */
  throughSeq: number;
  /**
   * The delivery generation of the workspace state the store holds; 0 before
   * the first state has landed, which is not an emission and is never acked.
   */
  generation: number;
}

export interface PaintAttestationOptions {
  /** Sends one acknowledgment. Rejects when the daemon refused it. */
  send: (throughSeq: number, generation: number, outcome: PaintOutcome) => Promise<void>;
  /**
   * Whether this frontend can draw RIGHT NOW. False for a hidden webview,
   * whose animation frames are suspended.
   */
  canPaint: () => boolean;
  /** Diagnostic sink; a failed attestation is a real failure, never quiet. */
  log: (message: string) => void;
}

export class PaintAttestation {
  /** The highest conversation seq this end has attested PAINTING. */
  private paintedSeq = -1;
  /** The highest state generation this end has attested PAINTING. */
  private paintedGeneration = -1;
  /**
   * The highest state generation this end has ANSWERED for at all, painted or
   * suspended. It is what keeps a hidden webview from re-answering the same
   * generation on every frame, and it is deliberately separate from
   * `paintedGeneration`: a suspended answer settles delivery without attesting
   * anything, so a later repaint of that very generation must still be able to
   * send the PAINTED attestation the workspace's green depends on.
   */
  private settledGeneration = -1;

  constructor(private readonly opts: PaintAttestationOptions) {}

  /**
   * Report that a render actually DREW this snapshot.
   *
   * Called from the completion of a render, never from the arrival of the
   * frames describing one: receiving the history and drawing it are different
   * facts, and only the second earns the ready state.
   */
  painted(snap: PaintSnapshot): void {
    if (snap.throughSeq <= this.paintedSeq && snap.generation <= this.paintedGeneration) return;
    const seq = snap.throughSeq;
    const generation = snap.generation;
    this.paintedSeq = Math.max(this.paintedSeq, seq);
    this.paintedGeneration = Math.max(this.paintedGeneration, generation);
    this.settledGeneration = Math.max(this.settledGeneration, generation);
    this.dispatch(seq, generation, "painted");
  }

  /**
   * Observe a snapshot this end has RECEIVED, whether or not it can draw it.
   *
   * While this end can paint, it does nothing: the render that follows will
   * report the truth. While it cannot, it answers at once with the suspended
   * outcome, so the state reaches Emacs instead of waiting on a frame that is
   * not coming.
   */
  observe(snap: PaintSnapshot): void {
    if (this.opts.canPaint()) return;
    if (snap.generation <= this.settledGeneration) return;
    const generation = snap.generation;
    this.settledGeneration = generation;
    this.dispatch(snap.throughSeq, generation, "suspended");
  }

  /**
   * Send one acknowledgment, rolling the watermarks back on failure.
   *
   * The failure this whole mechanism guards against is a frontend that LOOKS
   * attested without having drawn, so it must never bookkeep an ack it did not
   * land. A rollback to the initial watermarks makes the next pass re-attest
   * from scratch rather than assume the daemon heard.
   */
  private dispatch(throughSeq: number, generation: number, outcome: PaintOutcome): void {
    void this.opts.send(throughSeq, generation, outcome).catch((err: unknown) => {
      this.paintedSeq = -1;
      this.paintedGeneration = -1;
      this.settledGeneration = -1;
      this.opts.log(`paint ack (${outcome}) failed: ${String(err)}`);
    });
  }
}
