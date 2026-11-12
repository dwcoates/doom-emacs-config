package e2e

import (
	"google.golang.org/protobuf/reflect/protoreflect"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/oneofarm"
)

// THE FOOTER'S ACTIVITY CELL, READ WHICHEVER STATUS ARM STANDS. Every status
// arm carries its cell under the same shape (footer.proto, the status family's
// rules): a salient line, or the unpinned tiers — a transient over the one
// enduring line. The status-independent
// salient kinds (update, notification) ride every
// arm's salient oneof under the same field names, so one reader serves every
// arm and a test never misses a line because the status moved underneath it.

// footerActivity answers the standing status arm's activity cell, or nil.
func footerActivity(v *frontendv1.FooterView) protoreflect.Message {
	status := v.GetStrip().GetStatus().ProtoReflect()
	armField := oneofarm.Field(status, "status")
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
	if cell.Descriptor().Oneofs().ByName("tier") != nil {
		field := oneofarm.Field(cell, "tier")
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
