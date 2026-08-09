package e2e

import (
	"sync"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// testTurnAccountingStore gives every E2E controller the same required,
// durable-within-the-harness accounting semantics as production.
type testTurnAccountingStore struct {
	mu      sync.Mutex
	records map[string][]*frontendv1.TurnAccounting
}

func newTestTurnAccountingStore() *testTurnAccountingStore {
	return &testTurnAccountingStore{records: map[string][]*frontendv1.TurnAccounting{}}
}

func (s *testTurnAccountingStore) Record(sessionID string, accounting *frontendv1.TurnAccounting) (*frontendv1.TurnAccounting, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	canonical := proto.Clone(accounting).(*frontendv1.TurnAccounting)
	s.records[sessionID] = append(s.records[sessionID], canonical)
	return canonical, nil
}

func (s *testTurnAccountingStore) List(sessionID string) ([]*frontendv1.TurnAccounting, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	result := make([]*frontendv1.TurnAccounting, 0, len(s.records[sessionID]))
	for _, accounting := range s.records[sessionID] {
		result = append(result, proto.Clone(accounting).(*frontendv1.TurnAccounting))
	}
	return result, nil
}
