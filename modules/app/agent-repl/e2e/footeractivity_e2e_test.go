package e2e

import (
	"google.golang.org/protobuf/reflect/protoreflect"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE FOOTER'S ACTIVITY CELL, READ WHICHEVER STATUS ARM STANDS. Every status
// arm carries its cell under the same shape (footer.proto, the status family's
// rules): a salient line, or the unpinned tiers — a transient over an optional
// quiet-stretch line over the one enduring line. The status-independent
// salient kinds (update, notification, context_budget) ride every
// arm's salient oneof under the same field names, so one reader serves every
// arm and a test never misses a line because the status moved underneath it.

// footerActivity answers the standing status arm's activity cell, or nil.
func footerActivity(v *frontendv1.FooterView) protoreflect.Message {
	status := v.GetStrip().GetStatus().ProtoReflect()
	armField := status.WhichOneof(status.Descriptor().Oneofs().ByName("status"))
	if armField == nil {
		return nil
	}
	arm := status.Get(armField).Message()
	activity := arm.Descriptor().Fields().ByName("activity")
	if activity == nil || !arm.Has(activity) {
		return nil
	}
	return arm.Get(activity).Message()
}

// footerTier answers the cell's drawn tier message by name ("salient" or
// "unpinned"), or nil when the cell carries the other one. The waiting cell
// holds its salient line directly.
func footerTier(v *frontendv1.FooterView, tier protoreflect.Name) protoreflect.Message {
	cell := footerActivity(v)
	if cell == nil {
		return nil
	}
	if oneof := cell.Descriptor().Oneofs().ByName("tier"); oneof != nil {
		field := cell.WhichOneof(oneof)
		if field == nil || field.Name() != tier {
			return nil
		}
		return cell.Get(field).Message()
	}
	if tier != "salient" {
		return nil
	}
	return cell.Get(cell.Descriptor().Fields().ByName("salient")).Message()
}

// footerSalientKind answers the salient line's kind message when it is the
// named kind, or nil.
func footerSalientKind(v *frontendv1.FooterView, kind protoreflect.Name) protoreflect.Message {
	salient := footerTier(v, "salient")
	if salient == nil {
		return nil
	}
	field := salient.WhichOneof(salient.Descriptor().Oneofs().ByName("kind"))
	if field == nil || field.Name() != kind {
		return nil
	}
	return salient.Get(field).Message()
}

// footerNotification answers the standing push notification's text, or "".
func footerNotification(v *frontendv1.FooterView) string {
	if m := footerSalientKind(v, "notification"); m != nil {
		return m.Interface().(*frontendv1.FooterStatusActivityNotification).GetText()
	}
	return ""
}

// footerContextBudget answers the standing context-budget line's text, or "".
func footerContextBudget(v *frontendv1.FooterView) string {
	if m := footerSalientKind(v, "context_budget"); m != nil {
		return m.Interface().(*frontendv1.FooterStatusActivityContextBudget).GetText()
	}
	return ""
}

// footerEnduringUsage answers the enduring line's usage when the cell is
// unpinned and the daemon chose the usage line, or nil.
func footerEnduringUsage(v *frontendv1.FooterView) *frontendv1.FooterActivityEnduringUsage {
	unpinned := footerTier(v, "unpinned")
	if unpinned == nil {
		return nil
	}
	enduring := unpinned.Get(unpinned.Descriptor().Fields().ByName("enduring")).Message()
	return enduring.Interface().(*frontendv1.FooterActivityEnduring).GetUsage()
}
