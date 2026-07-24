/**
 * The permission-mode pick that has not taken effect yet.
 *
 * `agentshim.frontend.v1` has NO standalone set-permission-mode command: the
 * mode is a field of `SubmitPromptCmd`, so a pick can only take effect when
 * the next prompt carries it. That gap is what this holder exists to make
 * honest. Until a pushed `SessionView` reports the picked mode actually in
 * force, the picker keeps showing the user's choice rather than snapping back
 * to the mode still running on the next rendered frame — which would make
 * every pick look like it had been silently discarded.
 *
 * The pick is deliberately NOT spent at send time: a submit that fails must
 * not quietly drop the user's choice, so only the daemon reporting the mode
 * in force retires it.
 */
export class PendingPermissionMode {
  private pending = "";

  /** Hold a pick the user made in the picker, replacing any earlier one. */
  pick(mode: string): void {
    this.pending = mode;
  }

  /** The mode an outbound prompt should carry; "" when nothing is pending. */
  get outbound(): string {
    return this.pending;
  }

  /**
   * Reconcile against the mode the daemon reports in force, returning the mode
   * the picker should display. A pick the daemon has adopted is spent.
   */
  settle(live: string): string {
    if (this.pending !== "" && this.pending === live) this.pending = "";
    return this.pending !== "" ? this.pending : live;
  }
}
