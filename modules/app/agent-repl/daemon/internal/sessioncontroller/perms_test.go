package sessioncontroller

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestPermRegistryAnswerAllow(t *testing.T) {
	// Arrange.
	reg := newPermRegistry(nil)
	ch, release := reg.await("r1", "ws")
	defer release()

	// Act.
	if err := reg.answer("r1", true, "", nil); err != nil {
		t.Fatalf("answer: %v", err)
	}

	// Assert.
	select {
	case resp := <-ch:
		if resp.GetRequestId() != "r1" {
			t.Errorf("request_id: got %q, want r1", resp.GetRequestId())
		}
		if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_ALLOW {
			t.Errorf("decision: got %v, want ALLOW", resp.GetDecision())
		}
	case <-time.After(time.Second):
		t.Fatal("timed out waiting for the parked answer")
	}
}

func TestPermRegistryAnswerDenyCarriesMessage(t *testing.T) {
	// Arrange.
	reg := newPermRegistry(nil)
	ch, release := reg.await("r2", "ws")
	defer release()

	// Act.
	if err := reg.answer("r2", false, "nope", nil); err != nil {
		t.Fatalf("answer: %v", err)
	}

	// Assert.
	resp := <-ch
	if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_DENY {
		t.Errorf("decision: got %v, want DENY", resp.GetDecision())
	}
	if resp.GetDenyMessage() != "nope" {
		t.Errorf("deny_message: got %q, want nope", resp.GetDenyMessage())
	}
}

func TestPermRegistryAnswerUnknownIsError(t *testing.T) {
	reg := newPermRegistry(nil)
	if err := reg.answer("ghost", true, "", nil); err == nil {
		t.Fatal("answering an unknown request_id must be a loud error, not swallowed")
	}
}

func TestPermRegistryFailAbandonsWithNil(t *testing.T) {
	// Arrange.
	reg := newPermRegistry(nil)
	ch, release := reg.await("r3", "ws")
	defer release()

	// Act.
	reg.fail("teardown")

	// Assert: the waiter receives nil (no fabricated decision).
	select {
	case resp := <-ch:
		if resp != nil {
			t.Errorf("fail must deliver nil (shim re-asks on reattach); got %v", resp)
		}
	case <-time.After(time.Second):
		t.Fatal("fail did not release the waiter")
	}
}

func TestPermRegistryAnswerIsOneShot(t *testing.T) {
	// Arrange.
	reg := newPermRegistry(nil)
	_, release := reg.await("r4", "ws")
	defer release()

	// Act + Assert: first answer wins; a second is a loud error.
	if err := reg.answer("r4", true, "", nil); err != nil {
		t.Fatalf("first answer: %v", err)
	}
	if err := reg.answer("r4", true, "", nil); err == nil {
		t.Fatal("second answer for the same request_id must error")
	}
}
