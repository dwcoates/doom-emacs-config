/**
 * pointer-rest — WHAT THE POINTER RAISED STAYS DOWN UNTIL THE USER IS BACK.
 *
 * When the user leaves (the focus goes to Emacs, the page is hidden), a
 * notice the pointer raised is let go of and SUSPENDED: WebKit still believes
 * a resting pointer is where it was, and reports it there again after a
 * redraw (a synthesized move, or a `mouseover` of what was drawn under it), so
 * answering those would raise the notice again with nobody there. The
 * suspension lifts only when the pointer MOVES from where it rested, or when
 * the owner ends it for a reason of its own (a key pressed in it).
 *
 * One latch, so every surface that suspends on leaving (the rail's
 * transients, the footer's hovered response) judges "the user is back" the
 * same way.
 */

/** A suspension that lifts when the pointer moves from where it rested. */
export interface PointerRest {
  /** Whether a suspension is in force. */
  suspended(): boolean;
  /** The pointer was seen at (X, Y); true when the move ends a suspension. */
  moved(x: number, y: number): boolean;
  /**
   * Suspend until the pointer moves from where it was last seen; true when
   * this begins a suspension, false when one was already in force.
   */
  suspend(): boolean;
  /** End a suspension for the owner's own reason; true when one was in force. */
  resume(): boolean;
}

/** Where the pointer was seen, in the window's coordinates. */
interface Point {
  readonly x: number;
  readonly y: number;
}

/** A pointer-rest latch, not suspended, that has seen no pointer yet. */
export function createPointerRest(): PointerRest {
  /** The pointer's last position seen, or null before any. */
  let lastPoint: Point | null = null;
  let suspended = false;
  /** Where the pointer was when the suspension began, or null when unseen. */
  let suspendedAt: Point | null = null;
  return {
    suspended: () => suspended,
    moved(x, y) {
      lastPoint = { x, y };
      if (!suspended) return false;
      if (suspendedAt !== null && suspendedAt.x === x && suspendedAt.y === y) return false;
      suspended = false;
      return true;
    },
    suspend() {
      if (suspended) return false;
      suspended = true;
      suspendedAt = lastPoint;
      return true;
    },
    resume() {
      if (!suspended) return false;
      suspended = false;
      return true;
    },
  };
}
