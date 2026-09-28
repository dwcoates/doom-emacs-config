/**
 * store.v1 — the record layer the shim writes and reads. Re-exported as one
 * namespace by `src/proto.ts`; never import a generated file directly.
 *
 * store.v1 and shim.v1 BOTH declare `GetWorkflowRequest`/`GetWorkflowResponse`
 * and their arms. That collision is why `src/proto.ts` re-exports each package
 * as its own namespace instead of flattening all three: a flat barrel would
 * make `GetWorkflowRequest` ambiguous and TypeScript would silently drop it.
 */
export * from "../../../../proto/gen/ts/store/v1/service_pb.js";
export * from "../../../../proto/gen/ts/store/v1/store_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_get_agent_by_vendor_task_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_get_live_work_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_get_sidecar_cursors_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_get_workflow_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_open_agent_session_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_read_agent_page_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_watch_agent_session_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_watch_bash_run_pb.js";
export * from "../../../../proto/gen/ts/store/v1/endpoint_write_batch_pb.js";
