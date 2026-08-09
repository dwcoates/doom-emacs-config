package sessioncontroller

import (
	"fmt"
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

func TestPermRegistryResentRequestJoinsTheSameRendezvous(t *testing.T) {
	// Arrange: the shim re-sends an unanswered ask, so a second handler parks
	// on the request_id already held.
	reg := newPermRegistry(nil)
	first, releaseFirst := reg.await("r5", "ws")
	defer releaseFirst()
	second, releaseSecond := reg.await("r5", "ws")
	defer releaseSecond()

	// Act.
	if err := reg.answer("r5", true, "", nil); err != nil {
		t.Fatalf("answer: %v", err)
	}

	// Assert: the one answer releases BOTH parked handlers; replacing the
	// waiter instead would wedge the displaced one forever.
	for name, ch := range map[string]<-chan *corev1.PermissionResponse{"first": first, "second": second} {
		select {
		case resp := <-ch:
			if resp.GetDecision() != corev1.PermissionDecision_PERMISSION_DECISION_ALLOW {
				t.Errorf("%s decision: got %v, want ALLOW", name, resp.GetDecision())
			}
		case <-time.After(time.Second):
			t.Fatalf("%s parked handler was never released", name)
		}
	}
}

func TestPermRegistryResentRequestReportsOnePendingID(t *testing.T) {
	// Arrange.
	reg := newPermRegistry(nil)
	_, releaseFirst := reg.await("r6", "ws")
	defer releaseFirst()
	_, releaseSecond := reg.await("r6", "ws")
	defer releaseSecond()

	// Act.
	ids := reg.idsForWorkspace("ws")

	// Assert: a re-sent ask is ONE open question, so the pending count the
	// permission render-state reads must not double.
	if len(ids) != 1 || ids[0] != "r6" {
		t.Fatalf("pending ids = %v, want [r6]", ids)
	}
}

func TestPermRegistryReleasingOneJoinedCallerKeepsTheRequestPending(t *testing.T) {
	// Arrange: two parked callers, one of which is on a connection that dies.
	reg := newPermRegistry(nil)
	_, releaseFirst := reg.await("r7", "ws")
	_, releaseSecond := reg.await("r7", "ws")
	defer releaseSecond()

	// Act.
	releaseFirst()

	// Assert: the surviving caller still holds the question open.
	if ids := reg.idsForWorkspace("ws"); len(ids) != 1 {
		t.Fatalf("pending ids = %v, want the request still pending for the surviving caller", ids)
	}
}

func TestPermRegistryFailReleasesEveryJoinedCaller(t *testing.T) {
	// Arrange.
	reg := newPermRegistry(nil)
	first, releaseFirst := reg.await("r8", "ws")
	defer releaseFirst()
	second, releaseSecond := reg.await("r8", "ws")
	defer releaseSecond()

	// Act.
	reg.fail("teardown")

	// Assert: neither handler is left blocked, and neither is fed a decision.
	for name, ch := range map[string]<-chan *corev1.PermissionResponse{"first": first, "second": second} {
		select {
		case resp := <-ch:
			if resp != nil {
				t.Errorf("%s: fail must deliver nil, got %v", name, resp)
			}
		case <-time.After(time.Second):
			t.Fatalf("%s parked handler was never released", name)
		}
	}
}

func TestPermRegistryRecall(t *testing.T) {
	tests := []struct {
		name      string
		answer    bool
		denyMsg   string
		wantFound bool
		want      corev1.PermissionDecision
	}{
		{name: "allow is recalled", answer: true, wantFound: true, want: corev1.PermissionDecision_PERMISSION_DECISION_ALLOW},
		{name: "deny is recalled", answer: false, denyMsg: "nope", wantFound: true, want: corev1.PermissionDecision_PERMISSION_DECISION_DENY},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			reg := newPermRegistry(nil)
			_, release := reg.await("r9", "ws")
			defer release()

			// Act.
			if err := reg.answer("r9", tc.answer, tc.denyMsg, nil); err != nil {
				t.Fatalf("answer: %v", err)
			}
			resp, ok := reg.recall("r9")

			// Assert.
			if ok != tc.wantFound {
				t.Fatalf("recall found = %v, want %v", ok, tc.wantFound)
			}
			if resp.GetDecision() != tc.want {
				t.Errorf("decision: got %v, want %v", resp.GetDecision(), tc.want)
			}
		})
	}
}

func TestPermRegistryRecallMissesAnUnansweredRequest(t *testing.T) {
	// Arrange: parked but never answered.
	reg := newPermRegistry(nil)
	_, release := reg.await("r10", "ws")
	defer release()

	// Act.
	_, ok := reg.recall("r10")

	// Assert: an open question must be RE-ASKED on a re-send, never served a
	// decision nobody made.
	if ok {
		t.Fatal("recall must not report an answer for a request nobody answered")
	}
}

func TestPermRegistryFailKeepsRecordedAnswers(t *testing.T) {
	// Arrange: answered, then the connection tears down.
	reg := newPermRegistry(nil)
	_, release := reg.await("r11", "ws")
	if err := reg.answer("r11", true, "", nil); err != nil {
		t.Fatalf("answer: %v", err)
	}
	release()

	// Act.
	reg.fail("teardown")

	// Assert: a re-send after the teardown is exactly what the memory exists
	// for, so the decision must survive it.
	if _, ok := reg.recall("r11"); !ok {
		t.Fatal("a teardown must not forget a decision the human already made")
	}
}

func TestPermRegistryAnsweredMemoryIsBounded(t *testing.T) {
	// Arrange: answer one more request than the ring holds.
	reg := newPermRegistry(nil)
	ids := make([]string, 0, answeredMemoryLimit+1)
	for i := 0; i <= answeredMemoryLimit; i++ {
		id := fmt.Sprintf("bounded-%d", i)
		ids = append(ids, id)
		_, release := reg.await(id, "ws")
		if err := reg.answer(id, true, "", nil); err != nil {
			t.Fatalf("answer %s: %v", id, err)
		}
		release()
	}

	// Act.
	_, oldest := reg.recall(ids[0])
	_, newest := reg.recall(ids[len(ids)-1])

	// Assert: the oldest is evicted (a re-send that old re-asks, which is
	// honest) and the newest is retained.
	if oldest {
		t.Error("the answered ring must evict its oldest entry rather than grow without bound")
	}
	if !newest {
		t.Error("the most recent answer must stay recallable")
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
