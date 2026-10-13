/**
 * Every arm of one oneof, read off the GENERATED SCHEMA rather than a list
 * written out by hand: the source's own reader (`src/rpc/oneof.ts`), so the
 * suites enumerate exactly what the call sites do.
 *
 * A refusal suite's job is to prove the call site says something for every arm
 * the contract can send. Enumerating from the schema is what makes an arm added
 * to the proto fail the existing tests instead of quietly going undrawn.
 */
export { oneofArms } from "../src/rpc/oneof.js";
