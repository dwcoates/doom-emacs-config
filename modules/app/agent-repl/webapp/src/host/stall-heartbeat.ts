/**
 * stall-heartbeat — a beat on a clock the main thread cannot delay, for the
 * stall probe (load-census.ts).
 *
 * A worker's timer is not throttled when macOS occludes the web view, and a
 * worker keeps beating while the main thread is blocked; its messages queue
 * behind the block and arrive when it ends. So a beat's delivery lateness is
 * the main thread's block, and never timer throttling.
 */

/** Beat every MS, each beat stating when it was SENT on the page's `performance.now()` clock; answers the stop. */
export type Heartbeat = (ms: number, onBeat: (sentAt: number) => void) => () => void;

/** The worker's source: it states each beat on the wall clock (`Date.now()`), the one clock both threads share. */
const WORKER_SOURCE = "let id=setInterval(()=>postMessage(Date.now()),Number(self.name));onmessage=()=>{clearInterval(id);close()}";

/** The page's heartbeat: an inline worker. */
export const workerHeartbeat: Heartbeat = (ms, onBeat) => {
  const url = URL.createObjectURL(new Blob([WORKER_SOURCE], { type: "text/javascript" }));
  const worker = new Worker(url, { name: String(ms) });
  URL.revokeObjectURL(url);
  worker.onmessage = (event: MessageEvent<number>): void => {
    // Wall clock to this page's monotonic clock.
    onBeat(event.data - performance.timeOrigin);
  };
  return () => {
    worker.postMessage("stop");
    worker.terminate();
  };
};
