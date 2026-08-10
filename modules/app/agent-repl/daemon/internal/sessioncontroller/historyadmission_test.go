package sessioncontroller

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

// ONE LADDER, BOTH SURFACES.
//
// A frontend asks this daemon for its conversation two ways — a resync from a
// mark and a page from an anchor — and both must climb the SAME eligibility
// ladder. The risk the extraction exists to remove is asymmetric drift: a
// ladder that admitted a page where it refused a resync would be a silent
// cross-generation read on one surface only, and nothing about the paging code
// would look wrong.
//
// So the cases below assert the ladder's rungs AND assert that the two call
// sites really share them, rather than each having grown its own copy.

// historyRequest is one of the two ways a frontend asks for its conversation,
// reduced to the only thing this file cares about: the identity it echoes and
// the refusal it gets back.
type historyRequest struct {
	name string
	call func(h *repullHarness, sessionID, generationID string) error
}

// bothHistorySurfaces is the pair every case below runs. A rung that stops
// being shared fails here rather than in whichever surface was left behind.
func bothHistorySurfaces() []historyRequest {
	return []historyRequest{
		{
			name: "resync",
			call: func(h *repullHarness, sessionID, generationID string) error {
				return h.m.ResyncForFence("ws", ssm.Fence(sessionID, generationID), 0)
			},
		},
		{
			name: "conversation page",
			call: func(h *repullHarness, sessionID, generationID string) error {
				_, err := h.m.ConversationPage(context.Background(), "ws", ssm.Fence(sessionID, generationID), PageAnchor{Tail: true, Limit: 3})
				return err
			},
		},
	}
}

func TestEveryHistorySurfaceRefusesARetiredGeneration(t *testing.T) {
	// Arrange — a tab that outlived a daemon bounce echoes the generation it
	// was reading when it decided to ask.
	for _, surface := range bothHistorySurfaces() {
		t.Run(surface.name, func(t *testing.T) {
			client := &replayClient{}
			h := newRepullHarness(t, client)
			d := h.controller(t)

			// Act.
			err := surface.call(h, d.sessionID, d.generationID+"-retired")

			// Assert — refused BEFORE any read, so a stale request can never
			// half-serve someone else's conversation.
			if !errors.Is(err, errclass.ErrSessionSuperseded) {
				t.Fatalf("%s error = %v, want session superseded", surface.name, err)
			}
			if got := client.callCount(); got != 0 {
				t.Fatalf("%s started %d shim read(s) on a retired generation, want none", surface.name, got)
			}
		})
	}
}

func TestEveryHistorySurfaceRefusesAWorkspaceMidHibernation(t *testing.T) {
	// Arrange — a controller being torn down on purpose. Serving history
	// against it would replay a generation that is deliberately ending.
	for _, surface := range bothHistorySurfaces() {
		t.Run(surface.name, func(t *testing.T) {
			client := &replayClient{}
			h := newRepullHarness(t, client)
			d := h.controller(t)
			h.m.mu.Lock()
			if h.m.hibernating == nil {
				h.m.hibernating = map[string]bool{}
			}
			h.m.hibernating["ws"] = true
			h.m.mu.Unlock()

			// Act.
			err := surface.call(h, d.sessionID, d.generationID)

			// Assert.
			if !errors.Is(err, errclass.ErrSessionSuperseded) {
				t.Fatalf("%s error = %v, want session superseded", surface.name, err)
			}
			if got := client.callCount(); got != 0 {
				t.Fatalf("%s started %d shim read(s) mid-hibernation, want none", surface.name, got)
			}
		})
	}
}

func TestEveryHistorySurfaceAdmitsACurrentGenerationWhoseSessionRotated(t *testing.T) {
	// Arrange — THE RUNG THAT MUST NOT BE DROPPED. A non-empty controller
	// generation uniquely identifies this live controller, so a client carrying
	// it is current on the pushed plane and only its session field is stale.
	// Refusing it deadlocks the view: a replay is a view's only recovery
	// mechanism, so a refused one is a permanent stale banner.
	for _, surface := range bothHistorySurfaces() {
		t.Run(surface.name, func(t *testing.T) {
			client := &replayClient{}
			h := newRepullHarness(t, client)
			d := h.controller(t)

			// Act — the generation is live; the session id has rotated.
			err := surface.call(h, d.sessionID+"-rotated", d.generationID)

			// Assert.
			if errors.Is(err, errclass.ErrSessionSuperseded) {
				t.Fatalf("%s refused a client naming the LIVE generation under a rotated session: %v", surface.name, err)
			}
		})
	}
}

func TestEveryHistorySurfaceRefusesARotatedSessionWithNoGeneration(t *testing.T) {
	// Arrange — an EMPTY generation identifies nothing, so a rotated session
	// beside it is just a stale client with nothing current to stand on.
	for _, surface := range bothHistorySurfaces() {
		t.Run(surface.name, func(t *testing.T) {
			client := &replayClient{}
			h := newRepullHarness(t, client)
			d := h.controller(t)
			h.m.mu.Lock()
			d.generationID = ""
			h.m.mu.Unlock()

			// Act.
			err := surface.call(h, d.sessionID+"-rotated", "")

			// Assert.
			if !errors.Is(err, errclass.ErrSessionSuperseded) {
				t.Fatalf("%s error = %v, want session superseded", surface.name, err)
			}
		})
	}
}

func TestARefusalNamesTheRequestThatWasRefused(t *testing.T) {
	// Arrange — the ladder deliberately knows nothing about what was asked
	// for, so the request-shaped detail is carried in by the caller. A refusal
	// that could not name the request would be diagnosable only as far as the
	// workspace.
	tests := []struct {
		name string
		call func(h *repullHarness, sessionID, generationID string) error
		want string
	}{
		{
			name: "a resync names its mark",
			call: func(h *repullHarness, sessionID, generationID string) error {
				return h.m.ResyncForFence("ws", ssm.Fence(sessionID, generationID), 9)
			},
			want: "from_seq=9",
		},
		{
			name: "a page names its anchor and limit",
			call: func(h *repullHarness, sessionID, generationID string) error {
				_, err := h.m.ConversationPage(context.Background(), "ws", ssm.Fence(sessionID, generationID), PageAnchor{Tail: true, Limit: 7})
				return err
			},
			want: "anchor=tail limit=7",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			h := newRepullHarness(t, &replayClient{})
			d := h.controller(t)

			// Act.
			err := tt.call(h, d.sessionID, d.generationID+"-retired")

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("refusal = %v, want it to carry %q", err, tt.want)
			}
		})
	}
}

func TestAnAdmittedLiveRequestReleasesTheManagerLock(t *testing.T) {
	// Arrange — the live route must not hold the manager lock across a shim
	// round-trip, or one workspace's read serializes every other workspace's
	// bring-up behind it. The lock being free after an admitted read is what
	// proves the release ran.
	h := newRepullHarness(t, &replayClient{})
	d := h.controller(t)

	// Act.
	if _, err := h.m.ConversationPage(context.Background(), "ws", ssm.Fence(d.sessionID, d.generationID), PageAnchor{Tail: true, Limit: 3}); err != nil {
		t.Fatalf("ConversationPage: %v", err)
	}

	// Assert — a lock still held here would DEADLOCK rather than fail, so
	// acquiring it is the assertion. The read under it is incidental; taking
	// the lock at all is the whole test.
	h.m.mu.Lock()
	_, stillRegistered := h.m.byWS["ws"]
	h.m.mu.Unlock()
	if !stillRegistered {
		t.Fatalf("the workspace lost its controller during an admitted read")
	}
}
