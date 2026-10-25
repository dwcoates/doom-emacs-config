package wsm

import (
	"bytes"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedmarker"
	"agent-repld/internal/prose"
)

// withRetiredBytes appends a length-delimited retired field NUM carrying V to
// M's unknown fields, as a build that still wrote the field stored it.
func withRetiredBytes[M proto.Message](m M, num protowire.Number, v []byte) M {
	r := m.ProtoReflect()
	r.SetUnknown(protowire.AppendBytes(protowire.AppendTag(r.GetUnknown(), num, protowire.BytesType), v))
	return m
}

// withRetiredVarint appends a varint retired field NUM carrying V to M.
func withRetiredVarint[M proto.Message](m M, num protowire.Number, v uint64) M {
	r := m.ProtoReflect()
	r.SetUnknown(protowire.AppendVarint(protowire.AppendTag(r.GetUnknown(), num, protowire.VarintType), v))
	return m
}

// marshalled encodes M, failing the test on an error.
func marshalled(t *testing.T, m proto.Message) []byte {
	t.Helper()
	raw, err := proto.Marshal(m)
	if err != nil {
		t.Fatal(err)
	}
	return raw
}

func TestCarryRetiredShape(t *testing.T) {
	log := dlog.NewTestLogger()
	sw := &frontendv1.FeedMergeWorkspaceSwitch{Label: &frontendv1.FeedMergeWorkspaceSwitchLabel{}}
	identity := &frontendv1.FeedMergeRebaseCommitIdentity{ShortSha: "abc1234", Subject: "fix the thing"}
	tests := []struct {
		name string
		in   proto.Message
		want proto.Message
	}{
		{
			name: "a response's retired markdown is parsed into settled prose",
			in:   withRetiredBytes(&frontendv1.FeedResponseProse{}, retiredResponseMarkdownTag, []byte("**hi** there")),
			want: &frontendv1.FeedResponseProse{Prose: prose.Parse(log, "**hi** there", prose.Settled)},
		},
		{
			name: "a sent address line becomes its name and an unclassified recipient",
			in:   withRetiredBytes(&frontendv1.FeedAgentPromptAddress{}, retiredAddressTextTag, []byte("→ ws-a")),
			want: &frontendv1.FeedAgentPromptAddress{
				Name: &frontendv1.FeedAgentPromptAddressName{Text: "ws-a"},
				Direction: &frontendv1.FeedAgentPromptAddress_Sent{Sent: &frontendv1.FeedAgentPromptAddressSent{
					Recipient: &frontendv1.FeedAgentPromptAddressSent_Unclassified{Unclassified: &frontendv1.FeedAgentPromptAddressUnclassified{}},
				}},
			},
		},
		{
			name: "a notice's retired heading becomes the neutral vendor-notice marker",
			in:   withRetiredBytes(&frontendv1.FeedResponseNotice{}, retiredNoticeHeadingTag, []byte("Notice")),
			want: &frontendv1.FeedResponseNotice{Marker: feedmarker.Neutral(feedmarker.VendorNoticeLabel, "")},
		},
		{
			name: "a merge head's retired workspace switch becomes its async mode",
			in:   withRetiredBytes(&frontendv1.FeedMergeHead{}, retiredHeadWorkspaceSwitch, marshalledOrPanic(sw)),
			want: &frontendv1.FeedMergeHead{Mode: &frontendv1.FeedMergeMode{Mode: &frontendv1.FeedMergeMode_Async{Async: &frontendv1.FeedMergeModeAsync{WorkspaceSwitch: sw}}}},
		},
		{
			name: "a rebase commit tab's retired commit becomes its commit label",
			in: &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_RebaseCommit{
				RebaseCommit: withRetiredBytes(&frontendv1.FeedMergeTabRebaseCommit{}, retiredRebaseCommitTag, marshalledOrPanic(identity)),
			}},
			want: &frontendv1.FeedMergeTab{
				Kind:  &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: &frontendv1.FeedMergeTabRebaseCommit{}},
				Label: &frontendv1.FeedMergeTabLabel{Words: &frontendv1.FeedMergeTabLabel_Commit{Commit: identity}},
			},
		},
		{
			name: "a tests tab's retired suites and log are dropped",
			in: withRetiredBytes(withRetiredBytes(&frontendv1.FeedMergeTabTests{}, retiredTestsSuitesTag, []byte("s")),
				retiredTestsLogTag, []byte("l")),
			want: &frontendv1.FeedMergeTabTests{},
		},
		{
			name: "an answered permission's retired instant is dropped",
			in:   withRetiredVarint(&frontendv1.FeedPermissionAnswered{}, retiredPermissionAtMsTag, 1792894995819),
			want: &frontendv1.FeedPermissionAnswered{},
		},
		{
			name: "a returned tool call's retired runtime is dropped",
			in:   withRetiredBytes(&frontendv1.FeedToolCallReturned{}, retiredToolReturnRuntimeTag, []byte("rt")),
			want: &frontendv1.FeedToolCallReturned{},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			changed, err := carryRetiredShape(log, tt.in.ProtoReflect())

			// Assert
			if err != nil || !changed {
				t.Fatalf("carryRetiredShape = changed %v, %v; want a carry", changed, err)
			}
			if !proto.Equal(tt.in, tt.want) || len(tt.in.ProtoReflect().GetUnknown()) != 0 {
				t.Fatalf("carried = %v (unknown %x), want %v", tt.in, tt.in.ProtoReflect().GetUnknown(), tt.want)
			}
		})
	}
}

// marshalledOrPanic encodes M for a table built outside any test.
func marshalledOrPanic(m proto.Message) []byte {
	raw, err := proto.Marshal(m)
	if err != nil {
		panic(err)
	}
	return raw
}

func TestCarryRetiredShapeRefuses(t *testing.T) {
	log := dlog.NewTestLogger()
	tests := []struct {
		name string
		in   proto.Message
	}{
		{
			name: "markdown beside its prose",
			in: withRetiredBytes(&frontendv1.FeedResponseProse{Prose: prose.Parse(log, "x", prose.Settled)},
				retiredResponseMarkdownTag, []byte("x")),
		},
		{
			name: "an address line beside its name",
			in: withRetiredBytes(&frontendv1.FeedAgentPromptAddress{Name: &frontendv1.FeedAgentPromptAddressName{Text: "ws-a"}},
				retiredAddressTextTag, []byte("→ ws-a")),
		},
		{
			name: "a received address line, which never said who sent it",
			in:   withRetiredBytes(&frontendv1.FeedAgentPromptAddress{}, retiredAddressTextTag, []byte("← ws-a")),
		},
		{
			name: "a sent address line naming no one",
			in:   withRetiredBytes(&frontendv1.FeedAgentPromptAddress{}, retiredAddressTextTag, []byte("→ ")),
		},
		{
			name: "a heading beside its marker",
			in: withRetiredBytes(&frontendv1.FeedResponseNotice{Marker: feedmarker.Neutral(feedmarker.VendorNoticeLabel, "")},
				retiredNoticeHeadingTag, []byte("Notice")),
		},
		{
			name: "a workspace switch beside a mode",
			in: withRetiredBytes(&frontendv1.FeedMergeHead{Mode: &frontendv1.FeedMergeMode{}},
				retiredHeadWorkspaceSwitch, nil),
		},
		{
			name: "a workspace switch that does not decode",
			in:   withRetiredBytes(&frontendv1.FeedMergeHead{}, retiredHeadWorkspaceSwitch, []byte{0xff}),
		},
		{
			name: "a rebase commit beside a commit label",
			in: &frontendv1.FeedMergeTab{
				Kind: &frontendv1.FeedMergeTab_RebaseCommit{
					RebaseCommit: withRetiredBytes(&frontendv1.FeedMergeTabRebaseCommit{}, retiredRebaseCommitTag, nil),
				},
				Label: &frontendv1.FeedMergeTabLabel{Words: &frontendv1.FeedMergeTabLabel_Commit{Commit: &frontendv1.FeedMergeRebaseCommitIdentity{}}},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			before := marshalled(t, tt.in)

			// Act
			_, err := carryRetiredShape(log, tt.in.ProtoReflect())

			// Assert
			if err == nil {
				t.Fatalf("carryRetiredShape accepted %v, want a refusal", tt.in)
			}
			if tt.name == "a received address line, which never said who sent it" && !bytes.Equal(marshalled(t, tt.in), before) {
				t.Fatalf("the refused address was rewritten to %v", tt.in)
			}
		})
	}
}

func TestCarryRetiredShapeLeavesAMessageWithNoRetiredFieldAlone(t *testing.T) {
	// Arrange
	notice := &frontendv1.FeedResponseNotice{Marker: feedmarker.Neutral(feedmarker.VendorNoticeLabel, "")}

	// Act
	changed, err := carryRetiredShape(dlog.NewTestLogger(), notice.ProtoReflect())

	// Assert
	if err != nil || changed {
		t.Fatalf("carryRetiredShape = changed %v, %v; want nothing carried", changed, err)
	}
}

func TestRewriteRetiredShapesCarriesANestedRetiredField(t *testing.T) {
	// Arrange
	row := &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
		Address: withRetiredBytes(&frontendv1.FeedAgentPromptAddress{}, retiredAddressTextTag, []byte("→ ws-a")),
	}}}
	raw := marshalled(t, row)

	// Act
	out, changed, err := rewriteRetiredShapes(dlog.NewTestLogger(), raw)

	// Assert
	if err != nil || !changed {
		t.Fatalf("rewriteRetiredShapes = changed %v, %v; want a rewrite", changed, err)
	}
	got := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(out, got); err != nil {
		t.Fatal(err)
	}
	address := got.GetAgentPrompt().GetAddress()
	if address.GetName().GetText() != "ws-a" || len(address.ProtoReflect().GetUnknown()) != 0 {
		t.Fatalf("address = %v (unknown %x), want ws-a and no unknown field", address, address.ProtoReflect().GetUnknown())
	}
}

func TestRewriteRetiredShapesLeavesACurrentRowUntouched(t *testing.T) {
	// Arrange
	raw := marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
		Address: &frontendv1.FeedAgentPromptAddress{Name: &frontendv1.FeedAgentPromptAddressName{Text: "ws-a"}},
	}}})

	// Act
	out, changed, err := rewriteRetiredShapes(dlog.NewTestLogger(), raw)

	// Assert
	if err != nil || changed || !bytes.Equal(out, raw) {
		t.Fatalf("rewriteRetiredShapes = changed %v, %v; want the row untouched", changed, err)
	}
}
