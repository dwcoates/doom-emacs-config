package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opColdReadPayment is the operation every cold read payment record carries.
const opColdReadPayment = "daemon.workspace.cold_read_payment"

// A PAID COLD READ STAYS PAID UNTIL IT IS SPENT (wsm.ColdReadPayment).
//
// The gate's pay answer brings the session up without sending a request, so
// the cache is still cold, and the next bring-up of the same conversation --
// a rollout's bounce, a daemon restart -- is refused cold by its new shim for
// the very read the user already paid for (ship-gns, 2026-10-19). The answer
// is therefore recorded BEFORE the paid resume is asked for, and every
// bring-up the shim refuses cold reads it: a refusal naming the read that was
// paid for is answered with the payment, not a second gate. The answer is
// structural, not timed: whatever restarts the session, and whenever, the
// record outlives the shim it was given to.

// recordColdAnswer records what an answered gate's remediation means for the
// conversation's cold read, before the remediated resume is asked for. Pay
// records the read it pays for, named by the facts the gate was raised with.
// Clear and compact drop any standing payment: they rewrite the context, so
// no later read is the one that was paid for.
//
// A record that cannot be written refuses the re-open, which the answer's
// settle stands the gate again for: a pay that is not durable is the bounce
// defect waiting to recur. The store recorded its own failure at ERROR.
func (f *Fleet) recordColdAnswer(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, resume ColdResume) error {
	if resume.Remediation.GetPay() == nil {
		if err := f.deps.DB.DropColdReadPayment(ctx, ws); err != nil {
			log.Info(opColdReadPayment, "the standing cold read payment could not be dropped; the re-open is refused",
				dlog.Context{"vendor_session_id": resume.VendorSessionID, "cause": err.Error()})
			return fmt.Errorf("re-open session for %q: drop the cold read payment: %w", ws, err)
		}
		return nil
	}
	f.mu.RLock()
	cold := f.lastCold[ws]
	f.mu.RUnlock()
	if cold == nil {
		log.Error(opColdReadPayment, "the paid gate's cold facts are gone, so the read it paid for cannot be named; the re-open is refused",
			dlog.Context{"vendor_session_id": resume.VendorSessionID})
		return fmt.Errorf("re-open session for %q: the paid gate's cold facts are gone", ws)
	}
	payment := wsm.ColdReadPayment{
		VendorSessionID: resume.VendorSessionID,
		LastRequestAtMs: cold.GetLastRequestAtMs(),
		Model:           cold.GetRequestedModel().GetName(),
		PaidAt:          f.now(),
	}
	fields := coldReadFields(payment)
	if err := f.deps.DB.PutColdReadPayment(ctx, ws, payment); err != nil {
		fields["cause"] = err.Error()
		log.Info(opColdReadPayment, "the cold read payment could not be recorded; the re-open is refused", fields)
		return fmt.Errorf("re-open session for %q: record the cold read payment: %w", ws, err)
	}
	log.Info(opColdReadPayment, "recorded the cold read the user paid for", fields)
	return nil
}

// paidColdRead answers whether the user already paid for the cold read the
// shim just refused VENDORSESSIONID's resume for. A standing payment that
// names a different read was spent or superseded -- a request has been made
// since, or the conversation or its model changed -- and is dropped, so it can
// never pay for a read nobody agreed to.
func (f *Fleet) paidColdRead(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, vendorSessionID string, cold *conversationv1.SessionCold) (bool, error) {
	payment, found, err := f.deps.DB.ColdReadPaymentOf(ctx, ws)
	if err != nil {
		log.Info(opColdReadPayment, "the cold read payment could not be read; the bring-up is refused",
			dlog.Context{"vendor_session_id": vendorSessionID, "cause": err.Error()})
		return false, fmt.Errorf("read the cold read payment of %q: %w", ws, err)
	}
	if !found {
		return false, nil
	}
	if coversColdRead(payment, vendorSessionID, cold) {
		return true, nil
	}
	fields := coldReadFields(payment)
	fields["refused_vendor_session_id"] = vendorSessionID
	fields["refused_last_request_at_ms"] = cold.GetLastRequestAtMs()
	fields["refused_model"] = cold.GetRequestedModel().GetName()
	if err := f.deps.DB.DropColdReadPayment(ctx, ws); err != nil {
		fields["cause"] = err.Error()
		log.Info(opColdReadPayment, "the spent cold read payment could not be dropped; the bring-up is refused", fields)
		return false, fmt.Errorf("drop the spent cold read payment of %q: %w", ws, err)
	}
	log.Info(opColdReadPayment, "the standing cold read payment names a different read; it is spent and dropped", fields)
	return false, nil
}

// coversColdRead reports whether PAYMENT is the payment for the cold read the
// shim stated as COLD on VENDORSESSIONID's resume: the same conversation, the
// same last request, the same model.
func coversColdRead(payment wsm.ColdReadPayment, vendorSessionID string, cold *conversationv1.SessionCold) bool {
	return payment.VendorSessionID == vendorSessionID &&
		payment.LastRequestAtMs == cold.GetLastRequestAtMs() &&
		payment.Model == cold.GetRequestedModel().GetName()
}

// coldReadFields is the structured context every record about PAYMENT carries.
func coldReadFields(payment wsm.ColdReadPayment) dlog.Context {
	return dlog.Context{
		"vendor_session_id":  payment.VendorSessionID,
		"last_request_at_ms": payment.LastRequestAtMs,
		"model":              payment.Model,
	}
}

// payRemediation is the remediation a paid cold read is resumed with.
func payRemediation() *conversationv1.SessionColdRemediation {
	return &conversationv1.SessionColdRemediation{
		Remediation: &conversationv1.SessionColdRemediation_Pay{Pay: &conversationv1.SessionColdPay{}},
	}
}
