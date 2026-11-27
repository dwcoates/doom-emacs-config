<!-- used by: daemon/internal/merge (orchestrator.compactForQueue: the instructions of the /compact the daemon runs on a workspace whose own merge is about to enter the merge queue, before its session switches to Sonnet); placeholders: none -->
This workspace's work is finished and its merge is about to enter the merge queue. From here on, this conversation is read only when the queue hands the workspace a problem to fix: a test the merge gate found failing, or a conflict when the branch is rebased onto the main branch. A smaller model will fix it from this summary alone, so the summary must carry what that fix needs.

Keep, in this order:

1. The purpose of the work: what it changes, why it was asked for, and what the user wanted it to achieve, in the user's own terms where they gave them.
2. The design: the approach taken, the alternatives rejected and why, and every decision or ruling the user made along the way.
3. The tricky parts: invariants the change establishes or relies on, ordering and concurrency constraints, the edge cases that took care to get right, and anything that looks wrong but is deliberate.
4. Where the work lives: the branch, the worktree, every file the change touches and what each part of the change does there, and the tests that cover it with what each one pins.
5. What a fix must not do: behavior, contracts or tests that must not be changed to make a failure go away, and anything the user ruled out.

Drop the exploration that led nowhere, tool output, and the back-and-forth that reached each decision, keeping only the decisions. A failing test or a conflict is judged against the purpose and the invariants above, so those are what the summary must never lose.
