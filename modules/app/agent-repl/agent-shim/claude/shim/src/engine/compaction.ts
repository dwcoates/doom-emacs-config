/**
 * engine/compaction.ts — the throwaway session that rewrites the transcript.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. Compaction summarizes a conversation and REWRITES its
 * transcript, and it is driven by a SEPARATE, throwaway query rather than the
 * live one: the live query owns the session's identity, its locks and its
 * in-flight turn, and handing it a summarization prompt would put harness work
 * inside the user's conversation.
 *
 * WHO ASKS FOR IT. `Hibernate` (the daemon compacts before standing a shim
 * down, and waits for the ack), and `SessionColdCompact` as a remediation for a
 * cold context. A failed compaction is `HibernateCompactionFailed`, which
 * carries the vendor's own wording — the one failure arm that holds its own
 * human string, because `HibernateError` has no `detail`.
 */
export {};
