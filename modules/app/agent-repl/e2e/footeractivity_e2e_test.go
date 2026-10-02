package e2e

import (
	"context"
	"regexp"
	"testing"

	"google.golang.org/protobuf/encoding/prototext"
	"google.golang.org/protobuf/reflect/protoreflect"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE FOOTER'S ACTIVITY CELL, READ WHICHEVER STATUS ARM STANDS. Every status
// arm carries its cell under the same shape (footer.proto, the status family's
// rules): a salient line, or the unpinned tiers — a transient over the one
// enduring line. The status-independent
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

// feedItemDerivedLine matches the wording of a line composed from a feed item
// that landed ("✅ Read finished — handling result...", "❌ Bash failed",
// "✅ Prompt delivered — awaiting response...", "✅ Moved to background —
// continuing..."): the retired quiet tier's lines (owner ruling, 2026-10-01).
var feedItemDerivedLine = regexp.MustCompile(`[✅❌] [^"]* (finished|failed|cancelled|started|delivered)|Moved to background`)

// THE QUIET TIER IS RETIRED: between two feed items the footer composes no line
// from the item that landed. The `!read` scenario draws two feed items, the
// Read call and the closing response, and every footer push across the turn
// is swept for a line worded from a landed item. The pushes are read on their
// own goroutine as they arrive, so the stream's buffer never decides what the
// sweep sees.
func TestNoFooterPushBetweenFeedItemsCarriesAFeedItemDerivedLine(t *testing.T) {
	t.Parallel()
	// Arrange
	w, ws := newFileToolsWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()
	type swept struct {
		working    int
		violations []string
	}
	done := make(chan swept, 1)
	go func() {
		var out swept
		for v := range footer.Stream.C {
			status := v.GetStrip().GetStatus()
			if status.GetWorking() != nil {
				out.working++
			}
			if cell := footerActivity(v); cell != nil {
				if text := prototext.Format(cell.Interface()); feedItemDerivedLine.MatchString(text) {
					out.violations = append(out.violations, text)
				}
			}
			if out.working > 0 && status.GetIdle() != nil {
				break
			}
		}
		done <- out
	}()

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "read")
	awaitFeedRow(t, w, ws, "the read call's settled row", toolCallSettled(turn, "Read"))
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	var got swept
	select {
	case got = <-done:
	case <-ctx.Done():
		t.Fatalf("waiting for the footer to return to idle after the turn: %v", ctx.Err())
	}

	// Assert
	if got.working == 0 {
		t.Fatal("no working push observed: the sweep saw none of the turn")
	}
	if len(got.violations) != 0 {
		t.Fatalf("footer pushes carried %d feed-item-derived lines, want none:\n%v", len(got.violations), got.violations)
	}
}
