/**
 * engine/cold.ts — the cold gate, read off the transcript before the SDK is
 * touched.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. A resumed conversation whose prompt cache has lapsed costs
 * FULL PRICE to continue, and the user must be told BEFORE it is spent, not
 * after. This module reads the vendor transcript to answer three questions
 * without starting a query: how many context tokens the conversation holds,
 * when its last request was (so cache lapse can be judged), and which model and
 * permission mode it was last running under (so a resume restores the
 * conversation's own posture rather than a default).
 *
 * THE REFUSAL IS THE POINT. `StartSession` and `SetSessionModel` answer
 * `SessionCold` — with the cost and the reason — until the caller names a
 * remediation. It is a refusal and not a warning because a warning arrives
 * after the money is spent.
 */
export {};
