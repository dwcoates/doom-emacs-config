package sessiondrv

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

func TestConsumerRetainsLatestSystemInit(t *testing.T) {
	// Arrange — a vendor event carrying a SystemInit snapshot.
	c := newTestConsumer(&fakePusher{}, &fakeApplier{})
	csm := &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_SystemInit{SystemInit: &datav1.SystemInit{
			Model:         "haiku",
			SlashCommands: []string{"/foo", "/bar"},
			Skills:        []string{"skillA"},
		}},
	}
	any, err := anypb.New(csm)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}

	// Act
	c.Consume(&corev1.Event{SessionId: "s1", Payload: &corev1.Event_Vendor{Vendor: any}})

	// Assert — the snapshot is retained for the /status and /commands routes.
	si := c.latestSystemInit()
	if si == nil || si.GetModel() != "haiku" || len(si.GetSlashCommands()) != 2 {
		t.Fatalf("latestSystemInit = %v", si)
	}
}

func TestSystemInitFromVendorIgnoresNonInitVendor(t *testing.T) {
	// Arrange — a vendor event that is a ClaudeStreamMessage but NOT a system:init.
	csm := &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{Uuid: "u1"}},
	}
	any, err := anypb.New(csm)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	// Act / Assert
	if got := systemInitFromVendor(any); got != nil {
		t.Fatalf("systemInitFromVendor of a non-init vendor = %v, want nil", got)
	}
}

// fakeRegistrar records the claude_session_id write-throughs.
type fakeRegistrar struct{ writes []string }

func (f *fakeRegistrar) ClaudeSessionIDChanged(sessionID, csid string) {
	f.writes = append(f.writes, sessionID+"="+csid)
}

func newRegistrarManager(t *testing.T, reg SessionRegistrar) *Manager {
	t.Helper()
	m, err := New(Config{
		Push:            &fakePusher{},
		SSM:             &fakeApplier{},
		Spawner:         &fakeSpawner{},
		Locator:         fakeLocator{m: map[string]string{}},
		SeqStore:        &fakeSeqStore{seq: map[string]uint64{}},
		ProtocolVersion: "1",
		Registrar:       reg,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	return m
}

func TestPersistVendorSessionIDWritesThroughOncePerValue(t *testing.T) {
	// Arrange
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act — same value twice, then a new value.
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.persistVendorSessionID("s1", "cli-uuid-2")

	// Assert — deduped per session: two distinct write-throughs, not three.
	if len(reg.writes) != 2 || reg.writes[0] != "s1=cli-uuid-1" || reg.writes[1] != "s1=cli-uuid-2" {
		t.Fatalf("writes = %v", reg.writes)
	}
}

func TestPersistVendorSessionIDIgnoresEmpty(t *testing.T) {
	// Arrange
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)
	// Act — an empty uuid (no claude session yet) is not written.
	m.persistVendorSessionID("s1", "")
	// Assert
	if len(reg.writes) != 0 {
		t.Fatalf("writes = %v, want none", reg.writes)
	}
}
