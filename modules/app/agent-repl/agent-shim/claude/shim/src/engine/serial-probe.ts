/**
 * engine/serial-probe.ts — a pulled reading whose probes run ONE AT A TIME,
 * in the order they were asked for.
 *
 * A session re-reads some facts often (its context usage, the account's
 * usage), from more than one trigger. Two rules hold for every such reading,
 * stated once here:
 *
 * - THE PROBES ARE SERIAL. Each starts only after the one before it has
 *   settled, so the readings reach the daemon in the order they were asked
 *   for: a slow probe can never be overtaken by a later one and leave a stale
 *   figure standing as the newest.
 * - REFRESHES COALESCE. While a refresh is queued and not yet started, a
 *   further refresh asks for nothing more: the queued probe will read the
 *   later state anyway. `run` never coalesces, for a caller that must know a
 *   probe started after its own moment and settled.
 *
 * The caller holds the returned promise and so hears the probe's own failure;
 * the chain keeps only its SETTLEMENT, so one failed probe cannot stop every
 * later one from running.
 */

export class SerialProbe {
  private chain: Promise<void> = Promise.resolve();
  private queuedRefresh: Promise<void> | undefined;

  constructor(private readonly probe: () => Promise<void>) {}

  /** Whether a refresh is queued and not yet started. */
  get refreshPending(): boolean {
    return this.queuedRefresh !== undefined;
  }

  /** Queue one probe behind every probe already asked for. */
  run(): Promise<void> {
    return this.enqueue(() => undefined);
  }

  /**
   * Queue one probe unless a refresh is already queued and not yet started,
   * in which case that refresh's promise is the answer.
   */
  refresh(): Promise<void> {
    if (this.queuedRefresh !== undefined) return this.queuedRefresh;
    const queued = this.enqueue(() => {
      this.queuedRefresh = undefined;
    });
    this.queuedRefresh = queued;
    return queued;
  }

  private enqueue(onStart: () => void): Promise<void> {
    const next = this.chain.then(async () => {
      onStart();
      await this.probe();
    });
    this.chain = next.then(
      () => undefined,
      () => undefined,
    );
    return next;
  }
}
