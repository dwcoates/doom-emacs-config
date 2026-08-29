/**
 * engine/session.ts — THE session engine.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. The one implementation of {@link Engine}: it owns the single
 * vendor query, drives `StartSession` fresh and resume, applies
 * `SetSessionModel` at a turn boundary and `SetSessionPermissionMode`
 * immediately, performs `Hibernate` and `KillSession`, and carries the SIGTERM
 * stand-down.
 *
 * THE SESSION LOCK IS TAKEN HERE, NOT IN main.ts. `main.ts` takes the WORKSPACE
 * lock at startup because a workspace is knowable from argv; the session lock
 * is keyed by the VENDOR SESSION ID, which does not exist until StartSession
 * pre-mints it (fresh) or is handed it (resume). It is taken BEFORE the SDK is
 * touched — a shim that started a query and then discovered another shim owned
 * the conversation would already have two writers on one transcript — and held
 * for the process lifetime.
 *
 * STATELESSNESS. The engine accumulates NOTHING of variable size. History is
 * served from the store, never from memory; the joins it keeps are constant
 * (the pending permission callbacks, the live task table, spawn provenance).
 *
 * TEARDOWN ORDER, WHICH IS NOT NEGOTIABLE: resolve every pending permission
 * callback as denied (an unresolved one wedges the vendor process), then end
 * the query, then wait for every store write to be acked, then exit. Ending the
 * query first would abandon writes the record needs.
 */
export {};
