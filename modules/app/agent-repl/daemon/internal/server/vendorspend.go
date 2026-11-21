package server

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/vendorspend"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
)

// VendorSpendRecorder is where ReportVendorSpend records what it accepts: the
// daemon's metrics surface (runtimemetrics.Surface), whose off value records
// nothing and says so.
type VendorSpendRecorder interface {
	// Enabled reports whether anything is recorded.
	Enabled() bool
	// ObserveVendorSpend folds one run's calls for ws, billed to configDir,
	// refusing an invalid report whole with an error wrapping
	// vendorspend.ErrInvalid.
	ObserveVendorSpend(ws ids.WorkspaceID, configDir string, entries []*conversationv1.VendorSpend) error
}

// ReportVendorSpend folds the vendor calls Emacs made itself into the spend
// series (endpoint_report_vendor_spend.proto). A report that breaks the
// contract is the client's defect: it is refused whole on the invalid arm and
// recorded at ERROR, so nothing of it is half-counted.
func (s *server) ReportVendorSpend(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportVendorSpendRequest],
) (*connect.Response[agentreplv1.ReportVendorSpendResponse], error) {
	const rpc = "ReportVendorSpend"
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	ws := ids.WorkspaceID(req.Msg.GetWorkspace().GetId())
	if !s.deps.VendorSpend.Enabled() {
		return connect.NewResponse(&agentreplv1.ReportVendorSpendResponse{Result: &agentreplv1.ReportVendorSpendResponse_Success{Success: &agentreplv1.ReportVendorSpendSuccess{
			Outcome: &agentreplv1.ReportVendorSpendSuccess_MetricsOff{MetricsOff: &agentreplv1.ReportVendorSpendMetricsOff{}},
		}}}), nil
	}
	err := validateConfigDir(req.Msg.GetConfigDir())
	if err == nil {
		err = s.deps.VendorSpend.ObserveVendorSpend(ws, req.Msg.GetConfigDir(), req.Msg.GetSpend())
	}
	if err != nil {
		if !errors.Is(err, vendorspend.ErrInvalid) {
			return nil, fail(s.log, rpc, err)
		}
		s.log.Error(rpc, "refused a vendor spend report that breaks the contract; nothing of it was recorded", dlog.Context{
			"workspace": string(ws), "config_dir": req.Msg.GetConfigDir(), "entries": len(req.Msg.GetSpend()), "error": err.Error(),
		})
		return connect.NewResponse(&agentreplv1.ReportVendorSpendResponse{Result: &agentreplv1.ReportVendorSpendResponse_Error{Error: &agentreplv1.ReportVendorSpendError{
			Kind: &agentreplv1.ReportVendorSpendError_Invalid{Invalid: &agentreplv1.ReportVendorSpendInvalid{Detail: err.Error()}},
		}}}), nil
	}
	s.log.Debug(rpc, "recorded a vendor spend report", dlog.Context{"workspace": string(ws), "entries": len(req.Msg.GetSpend())})
	return connect.NewResponse(&agentreplv1.ReportVendorSpendResponse{Result: &agentreplv1.ReportVendorSpendResponse_Success{Success: &agentreplv1.ReportVendorSpendSuccess{
		Outcome: &agentreplv1.ReportVendorSpendSuccess_Recorded{Recorded: &agentreplv1.ReportVendorSpendRecorded{}},
	}}}), nil
}

// validateConfigDir refuses a report that names no account root.
func validateConfigDir(configDir string) error {
	if configDir == "" {
		return errors.Join(vendorspend.ErrInvalid, errors.New("the report names no account root"))
	}
	return nil
}
