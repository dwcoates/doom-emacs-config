package sessiondrv

import (
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"

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

	// Assert — the snapshot is retained for the pushed SessionInitView frame.
	si := c.latestSystemInit()
	if si == nil || si.GetModel() != "haiku" || len(si.GetSlashCommands()) != 2 {
		t.Fatalf("latestSystemInit = %v", si)
	}
}

func TestConsumeVendorSystemInitPushesSessionInitView(t *testing.T) {
	// Arrange — a vendor event carrying a SystemInit snapshot.
	push := &fakePusher{}
	c := newTestConsumer(push, &fakeApplier{})
	csm := &datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_SystemInit{SystemInit: &datav1.SystemInit{
			Model: "haiku", SlashCommands: []string{"/foo"},
		}},
	}
	any, err := anypb.New(csm)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}

	// Act.
	c.Consume(&corev1.Event{SessionId: "s1", Payload: &corev1.Event_Vendor{Vendor: any}})

	// Assert — a SessionInitView is pushed when the init lands (S9), scoped to
	// the consumer's workspace/session and carrying the retained init.
	if len(push.inits) != 1 {
		t.Fatalf("expected 1 SessionInitView push, got %d", len(push.inits))
	}
	got := push.inits[0]
	if got.GetWorkspace() != "ws" || got.GetSessionId() != "s1" || got.GetInit().GetModel() != "haiku" {
		t.Fatalf("SessionInitView = ws=%q session=%q model=%q", got.GetWorkspace(), got.GetSessionId(), got.GetInit().GetModel())
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
type fakeRegistrar struct {
	mu        sync.Mutex
	writes    []string
	queued    map[string][]registry.QueuedPrompt
	backfills []string
	deaths    []string
}

func (f *fakeRegistrar) ClaudeSessionIDChanged(sessionID, csid string) {
	f.writes = append(f.writes, sessionID+"="+csid)
}

// BackfillStateChanged records the never-blue backfill transitions (F2).
func (f *fakeRegistrar) BackfillStateChanged(sessionID, state string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.backfills = append(f.backfills, sessionID+"="+state)
}

// backfillWrites returns the recorded transitions, taken under the lock.
func (f *fakeRegistrar) backfillWrites() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.backfills...)
}

// SessionDied records the terminal write a shim death produces (F4).
func (f *fakeRegistrar) SessionDied(sessionID, reason string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.deaths = append(f.deaths, sessionID+"="+reason)
}

// deathWrites returns the recorded deaths, taken under the lock.
func (f *fakeRegistrar) deathWrites() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.deaths...)
}

func (f *fakeRegistrar) QueuedPromptsChanged(sessionID string, queued []registry.QueuedPrompt) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.queued == nil {
		f.queued = map[string][]registry.QueuedPrompt{}
	}
	f.queued[sessionID] = queued
}

func newRegistrarManager(t *testing.T, reg SessionRegistrar) *Manager {
	t.Helper()
	m, err := New(Config{
		Push:            &fakePusher{},
		SSM:             &fakeApplier{},
		Spawner:         &fakeSpawner{},
		Locator:         fakeLocator{m: map[string]string{}},
		SeqStore:        &fakeSeqStore{seq: map[string]uint64{}},
		Source:          stubSource{},
		ProtocolVersion: "1",
		Registrar:       reg,
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	return m
}

func TestPersistSessionDeathWritesTheShimDiedReason(t *testing.T) {
	// Arrange: the reason the registry documented while no code path wrote it.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act.
	m.persistSessionDeath("s1", errclass.DeathReasonShimDied)

	// Assert.
	want := "s1=" + errclass.DeathReasonShimDied
	if got := reg.deathWrites(); len(got) != 1 || got[0] != want {
		t.Fatalf("death writes = %v, want [%s]", got, want)
	}
}

func TestPersistSessionDeathIsANoOpWithoutARegistrar(t *testing.T) {
	// Arrange: a driver built without a registrar (a unit harness).
	m := newRegistrarManager(t, nil)

	// Act + Assert: the absence of a registrar must not panic the read loop
	// the death is reported from.
	m.persistSessionDeath("s1", errclass.DeathReasonShimDied)
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
