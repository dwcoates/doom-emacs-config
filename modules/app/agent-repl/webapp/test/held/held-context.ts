/**
 * held-context — the one HeldContext the held section's tests draw into: the app
 * context they built, a teardown sink, and a fresh classifier form registry.
 */
import type { AppContext } from "../../src/rpc/context.js";
import { ClassifierUpdateForms } from "../../src/held/classifier-update.js";
import type { HeldContext } from "../../src/held/context.js";

/** A HeldContext over CTX whose teardowns go to ONDISPOSE (dropped by default). */
export function testHeldContext(ctx: AppContext, onDispose: (fn: () => void) => void = () => undefined): HeldContext {
  return { ctx, onDispose, classifierForms: new ClassifierUpdateForms(ctx) };
}
