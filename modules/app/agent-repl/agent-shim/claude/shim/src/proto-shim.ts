/**
 * shim.v1 — the service THIS process serves to the daemon. Re-exported as one
 * namespace by `src/proto.ts`; never import a generated file directly.
 *
 * `endpoint_get_session_context_usage_pb.ts` and
 * `endpoint_get_session_diagnostics_pb.ts` are deliberately absent: the two
 * pull verbs were retired (diagnostics and context usage ride WatchSession as
 * SessionUpdate arms) and `service.proto` no longer names them, so the
 * generated files are leftovers of a previous generation, not contract.
 */
export * from "../../../../proto/gen/ts/shim/v1/service_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_detach_foreground_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_get_workflow_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_hibernate_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_kill_session_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_kill_turn_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_gather_title_digest_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_read_history_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_read_transcripts_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_roll_back_session_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_set_session_effort_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_set_session_model_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_set_session_permission_mode_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_start_session_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_start_turn_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_stop_bash_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_stop_workflow_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_update_agent_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_watch_agent_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_watch_bash_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_watch_session_pb.js";
export * from "../../../../proto/gen/ts/shim/v1/endpoint_watch_workflow_pb.js";
