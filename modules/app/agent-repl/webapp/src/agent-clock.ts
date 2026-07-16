/**
 * The bubble topbars' live elapsed tick: the agent-scoped twin of the
 * header's `TaskTimer` (timer.ts), for however many subagents run at
 * once. A running agent's bubble must keep counting even when the agent
 * is quiet — no frames, so no re-render — which is exactly the gap the
 * header's timer fills for the session clock.
 *
 * Same discipline as the `TaskTimer`: the tick writes ONLY the timer
 * slots (via PAINT), never re-renders, because the feed already re-renders
 * on every streamed frame and a whole-feed rewrite once a second on top of
 * that would be churn in service of a few changing digits.
 *
 * Unlike the `TaskTimer`, sync does not repaint immediately: every render
 * bakes a fresh label into each bubble's HTML (`agentTopbarDatapoints`
 * reads the caller's clock), so the moment the live set changes the DOM is
 * already current, and the tick only has to keep it moving between renders.
 *
 * Nothing here touches the DOM. The clock and the paint are both injected,
 * which is what lets the tick lifecycle be tested at all: the webapp's test
 * dependencies carry no DOM.
 */
import { formatElapsed } from "./duration.js";
import { TICK_MS, TimerHost } from "./timer.js";
import { AgentClockEntry } from "./topbar.js";

/**
 * A once-a-second repaint of every live agent bubble's timer slot, for
 * exactly as long as at least one subagent runs. PAINT receives the agent
 * id and the label; a paint against a slot the DOM does not currently hold
 * (a bubble inside a closed activity panel) is the caller's to skip.
 */
export class AgentClock {
  private tick: number | null = null;
  private live: readonly AgentClockEntry[] = [];

  constructor(
    private readonly host: TimerHost,
    private readonly paint: (agentId: string, label: string) => void,
  ) {}

  /**
   * Reconcile the tick against the store's live agents
   * (`runningAgentClocks`). Cheap to call on every frame: the interval is
   * only touched on the empty/non-empty edges.
   */
  sync(live: readonly AgentClockEntry[]): void {
    this.live = live;
    if (live.length === 0) {
      this.clear();
    } else if (this.tick === null) {
      this.tick = this.host.setInterval(() => this.repaint(), TICK_MS);
    }
  }

  /**
   * Stop ticking. Used when the view is torn down or rebound; settled
   * labels need no farewell paint, since renders bake them in frozen.
   */
  stop(): void {
    this.live = [];
    this.clear();
  }

  private clear(): void {
    if (this.tick === null) return;
    this.host.clearInterval(this.tick);
    this.tick = null;
  }

  private repaint(): void {
    const now = this.host.now();
    for (const agent of this.live) {
      this.paint(agent.id, formatElapsed(now - Date.parse(agent.startedAt)));
    }
  }
}
