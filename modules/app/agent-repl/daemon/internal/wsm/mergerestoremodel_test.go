package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestTakeMergeRestoreModel(t *testing.T) {
	tests := []struct {
		name      string
		holds     []string
		wantModel string
		wantHeld  bool
	}{
		{name: "nothing held answers none", wantHeld: false},
		{name: "a held model is answered", holds: []string{"opus"}, wantModel: "opus", wantHeld: true},
		{name: "a second hold keeps the first model", holds: []string{"opus", "sonnet"}, wantModel: "opus", wantHeld: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			for _, model := range tt.holds {
				if err := s.HoldMergeRestoreModel(context.Background(), ws.ID, model); err != nil {
					t.Fatalf("HoldMergeRestoreModel(%q): %v", model, err)
				}
			}

			// Act
			model, held, err := s.TakeMergeRestoreModel(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("TakeMergeRestoreModel: %v", err)
			}
			if model != tt.wantModel || held != tt.wantHeld {
				t.Fatalf("TakeMergeRestoreModel = (%q, %v), want (%q, %v)", model, held, tt.wantModel, tt.wantHeld)
			}
		})
	}
}

func TestTakeMergeRestoreModelClearsTheHeldModel(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.HoldMergeRestoreModel(context.Background(), ws.ID, "opus"); err != nil {
		t.Fatalf("HoldMergeRestoreModel: %v", err)
	}
	if _, _, err := s.TakeMergeRestoreModel(context.Background(), ws.ID); err != nil {
		t.Fatalf("first TakeMergeRestoreModel: %v", err)
	}

	// Act
	model, held, err := s.TakeMergeRestoreModel(context.Background(), ws.ID)

	// Assert
	if err != nil || held || model != "" {
		t.Fatalf("second TakeMergeRestoreModel = (%q, %v, %v), want nothing held", model, held, err)
	}
}

func TestHoldMergeRestoreModelRefusesAnEmptyModel(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.HoldMergeRestoreModel(context.Background(), ws.ID, "")

	// Assert
	if err == nil {
		t.Fatal("HoldMergeRestoreModel(\"\") succeeded, want a refusal")
	}
	if _, held, _ := s.TakeMergeRestoreModel(context.Background(), ws.ID); held {
		t.Fatal("a refused hold left a model held")
	}
}

func TestMergeRestoreModelRefusesAnUnknownWorkspace(t *testing.T) {
	tests := []struct {
		name string
		op   string
		act  func(s *store) error
	}{
		{name: "hold", op: "daemon.wsm.hold_merge_restore_model", act: func(s *store) error {
			return s.HoldMergeRestoreModel(context.Background(), "ws-unknown", "opus")
		}},
		{name: "take", op: "daemon.wsm.take_merge_restore_model", act: func(s *store) error {
			_, _, err := s.TakeMergeRestoreModel(context.Background(), "ws-unknown")
			return err
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := tt.act(s)

			// Assert
			if !errors.Is(err, ErrNotFound) {
				t.Fatalf("%s = %v, want ErrNotFound", tt.name, err)
			}
			if !loggedOperation(log, tt.op, "debug") {
				t.Fatalf("the refused write was not recorded: %v", log.Records())
			}
		})
	}
}
