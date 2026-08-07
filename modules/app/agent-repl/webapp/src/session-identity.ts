/**
 * When the page's session-addressed side-calls are allowed to happen.
 *
 * A workspace-addressed page mounts knowing WHICH WORKSPACE it renders and
 * nothing else: the daemon rules on which session that workspace owns, and the
 * answer arrives on the pushed plane some time after the socket opens. Any
 * side-call wired straight to mount therefore ran with an empty id — a literal
 * `GET /sessions//account` — and every one of them was answered with a 404 the
 * page reported as a lookup failure. The warning was guaranteed and meant
 * nothing, which is the worst thing a warning can be.
 *
 * The gate makes the ordering explicit rather than hoping the identity lands
 * first: a caller registers what it wants done WITH an identity, and the gate
 * runs it when there is one — immediately if the page already knows, and again
 * on every rebind, because a session rotating under a live view can change the
 * answer the call returns.
 */

/** A side-call that needs the page's session id. */
export type SessionIdentityListener = (sessionId: string) => void;

export class SessionIdentityGate {
  private readonly listeners: SessionIdentityListener[] = [];

  /**
   * @param read the page's live session id, "" until the daemon has ruled.
   *   It is a reader rather than a stored copy so the gate can never disagree
   *   with the id the side-calls themselves target.
   */
  constructor(private readonly read: () => string) {}

  /** Whether the page currently knows which session it targets. */
  get bound(): boolean {
    return this.read() !== "";
  }

  /**
   * Register a side-call, running it now if the identity is already known.
   *
   * Registration order is irrelevant: a listener added after the identity
   * landed still runs, and one added before waits for `announce`.
   */
  whenBound(listener: SessionIdentityListener): void {
    this.listeners.push(listener);
    if (this.bound) listener(this.read());
  }

  /**
   * Run every registered side-call against the identity just bound.
   *
   * A no-op while the id is still empty: announcing an absence would put back
   * exactly the empty-id request this gate exists to prevent.
   */
  announce(): void {
    if (!this.bound) return;
    const sessionId = this.read();
    for (const listener of this.listeners) listener(sessionId);
  }
}
