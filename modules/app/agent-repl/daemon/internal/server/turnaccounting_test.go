package server

import (
	"sync"

	statev1 "agentrepl/proto/state/v1"

	"google.golang.org/protobuf/proto"
)

type testTurnAccountingStore struct {
	mu      sync.Mutex
	records map[string][]*statev1.TurnAccounting
}

func newTestTurnAccountingStore() *testTurnAccountingStore {
	return &testTurnAccountingStore{records: map[string][]*statev1.TurnAccounting{}}
}

func (s *testTurnAccountingStore) Record(sessionID string, accounting *statev1.TurnAccounting) (*statev1.TurnAccounting, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	canonical := proto.Clone(accounting).(*statev1.TurnAccounting)
	s.records[sessionID] = append(s.records[sessionID], canonical)
	return canonical, nil
}

func (s *testTurnAccountingStore) List(sessionID string) ([]*statev1.TurnAccounting, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	result := make([]*statev1.TurnAccounting, 0, len(s.records[sessionID]))
	for _, accounting := range s.records[sessionID] {
		result = append(result, proto.Clone(accounting).(*statev1.TurnAccounting))
	}
	return result, nil
}
