# Fresh-input token figures: footer, response bubbles, subagent cards

The token figures the GUI shows beside a turn do not say what the owner means
by them. The owner wants every per-turn figure to count FRESH input — the input
tokens that were not cache hits (`input_tokens` plus
`cache_creation_input_tokens`), because new cache writes are what cost money —
for the ONE agent the figure belongs to, never its subagents. The footer is the
turn's running tally of the main agent's fresh input, colored on a continuous
green→red gradient by size. Each intermediate (amber) response bubble freezes,
when it lands, the fresh input added since the previous bubble, so the bubbles
of a turn partition the tally; the final-answer (green) bubble carries the
turn's whole tally, equal to the footer's final value. A subagent's figure is
that subagent's own fresh input over its whole lifetime. The topbar's
context-window figure is unchanged.

## Landed changes

### 1. Fresh input is the one quantity every per-turn figure counts

- WHAT: `frontend.v1.FooterTokensCell`, `frontend.v1.FeedResponseUsageStamp`
  and `frontend.v1.FeedSubagentTokens` are all redefined to count FRESH INPUT:
  `input_tokens` plus `cache_creation_input_tokens` (the
  `conversation.v1.TokenCacheMisses` of each API response, written plus
  unwritten), for one agent only.
- WHY: the owner reads these figures as a spend signal. Cache writes are the
  expensive part of a turn, so a cold cache MUST show up as a large figure.
  Output is excluded because it is re-sent as input on the next request and
  would otherwise be counted twice.
- CONSEQUENCES (accepted): the footer can exceed the topbar's context chip
  after a cold cache, because re-written context is fresh without growing the
  window; the owner accepted this explicitly. The footer's cell no longer
  follows the context window, so its cut-rebaseline no longer applies to the
  cell (the panel's context-growth line keeps it).
- REJECTED: `input_tokens` alone (reads near zero under Claude Code's heavy
  caching); context-window growth (hides cold-cache rewrites, which is the
  signal the owner wants).

### 2. The footer cell is the main agent's turn tally, with a heat element

- WHAT: the cell's figure is the main agent's fresh input summed over the turn.
  `frontend.v1.FooterTokensCellInput` gains `optional
  frontend.v1.FooterTokensCellInputHeat heat`, a position in [0, 1] on a
  continuous green → yellow → orange → red gradient.
- WHY: the owner asked for the figure colored green under 30k, yellow under
  50k, orange under 100k, red from 100k, with a continuous gradient.
- DECISION TAKEN BY THE ORCHESTRATOR (owner delegated it): the four colors sit
  at 0, 30k, 50k and 100k and the position is piecewise-linear between them
  (30k → 1/3, 50k → 2/3, 100k and above → 1). The daemon owns the thresholds;
  the webapp owns only the four theme colors and interpolates between the two
  bracketing the position.
- CONSEQUENCE: unset heat with no turn in flight, so `--` draws uncolored.

### 3. Response bubbles are frozen deltas; the final answer carries the total

- WHAT: a response bubble's stamp is the fresh input its agent added since
  that agent's previous bubble landed, growing while the bubble arrives and
  frozen when it settles. The turn's final-answer bubble is re-stamped once,
  at turn end, with the main agent's whole turn tally. Nothing else re-stamps a
  landed bubble.
- WHY: the owner wants the bubbles of a turn to partition the footer's tally
  and the green bubble to equal the footer's final value.
- CONSEQUENCES: the per-API-response grouping that attributed usage to the
  bubble of the same API response is gone; usage is tallied per agent account
  (the main agent's turn, or a subagent's lifetime) and a bubble's stamp is
  read off its account. A bubble that settles before its own API response's
  usage arrives leaves that usage to the next bubble; the partition and the
  final total are unaffected.
- IMPLEMENTATION NOTE: the green border is applied at turn end
  (`daemon/internal/resolve/feed/selection.go` `restampFinalAnswer`), after the
  bubble already landed with its delta, so the turn-total re-stamp rides that
  same site.

### 4. A subagent's figure is its own lifetime fresh input

- WHAT: `frontend.v1.FeedSubagentTokens` is the subagent's fresh input over
  its whole lifetime, tallied from its own usage frames; the response bubbles
  in its sub-feed partition it the same way the main agent's bubbles partition
  a turn.
- WHY: the owner asked for the subagent bubble to show the footer's quantity
  for that agent, over its lifetime rather than a turn.
- CONSEQUENCE: the vendor's running grand total (`AgentSubagentProgress.total_tokens`)
  and the settled totals are no longer drawn as the figure; a subagent whose
  usage frames never reach the feed resolver draws no figure.
