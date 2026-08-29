/**
 * engine/backup.ts — the bounded transcript backup.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. Copy the vendor transcript at every turn end and at every
 * vendor-uuid rotation into a bounded, pruned backup directory beside the work,
 * restorable.
 *
 * WHY IT EXISTS. The transcript is THE ONE ARTIFACT NOBODY CAN REGENERATE. Code
 * can be rewritten and state can be rebuilt; a conversation cannot. The backup
 * is the rung below the fresh-start refusal: when the refusal fails to protect
 * the transcript, this is what still has a copy. Bounded and pruned because an
 * unbounded backup of a growing file eventually costs more than it protects.
 */
export {};
