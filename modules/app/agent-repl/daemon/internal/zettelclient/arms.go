package zettelclient

import (
	zettelv1 "agentrepl/proto/zettel/v1"
)

// armName answers a failure kind's arm by its proto name.
func armName(kind any) string {
	switch kind.(type) {
	case *zettelv1.RecordExchangeFailure_StorageFailure, *zettelv1.GetThreadFailure_StorageFailure, *zettelv1.RecordSightingsFailure_StorageFailure:
		return "storage_failure"
	case *zettelv1.RecordExchangeFailure_InvalidRequest, *zettelv1.GetThreadFailure_InvalidRequest, *zettelv1.RecordSightingsFailure_InvalidRequest:
		return "invalid_request"
	case *zettelv1.RecordExchangeFailure_UnknownParent:
		return "unknown_parent"
	case *zettelv1.RecordExchangeFailure_ThreadAlreadyStarted:
		return "thread_already_started"
	case *zettelv1.RecordSightingsFailure_UnknownNode:
		return "unknown_node"
	case nil:
		return "unset"
	default:
		return "unknown"
	}
}
