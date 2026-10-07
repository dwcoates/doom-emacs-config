/**
 * tray-context — the one TrayContext the tray's tests draw into: the app
 * context they built, a teardown sink, and a fresh classifier form registry.
 */
import type { AppContext } from "../../src/rpc/context.js";
import { ClassifierUpdateForms } from "../../src/tray/classifier-update.js";
import type { TrayContext } from "../../src/tray/context.js";

/** A TrayContext over CTX whose teardowns go to ONDISPOSE (dropped by default). */
export function testTrayContext(ctx: AppContext, onDispose: (fn: () => void) => void = () => undefined): TrayContext {
  return { ctx, onDispose, classifierForms: new ClassifierUpdateForms(ctx) };
}
