/**
 * THE single import site for the generated protobuf stubs the shim speaks.
 *
 * The shim sits on three surfaces and this file names all three, one namespace
 * each:
 *
 *   - `shimv1`         — the Connect service this process SERVES to the daemon.
 *   - `storev1`        — the record layer this process WRITES and READS.
 *   - `conversationv1` — the vendor-neutral model it PRODUCES into the store
 *                        and serves back through history.
 *
 * NAMESPACES, NOT A FLAT BARREL. shim.v1 and store.v1 each declare a
 * `GetWorkflowRequest`, a `GetWorkflowResponse`, and matching success/failure
 * arms. A flat `export *` of both makes every one of those names AMBIGUOUS,
 * and TypeScript resolves an ambiguous star export by silently exporting
 * nothing — the two surfaces would quietly lose their workflow vocabulary and
 * the failure would surface as a mysterious missing symbol far from here. The
 * namespaces also keep the four identifier spaces legible at every use site:
 * `shimv1.GetWorkflowRequest` and `storev1.GetWorkflowRequest` are different
 * messages on different wires, and the spelling now says so.
 *
 * `workspace.v1` and `frontend.v1`/`agentrepl.v1` stay absent: the shim echoes
 * no workspace identity on any of its three surfaces, serves no frontend, and
 * holds no daemon state, so naming them here would be reaching across a
 * boundary rather than importing a type.
 */
export * as conversationv1 from "./proto-conversation.js";
export * as shimv1 from "./proto-shim.js";
export * as storev1 from "./proto-store.js";
