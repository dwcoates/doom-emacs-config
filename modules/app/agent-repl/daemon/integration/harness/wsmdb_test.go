package harness

import "testing"

func TestRefuseInMemoryRead(t *testing.T) {
	tests := []struct {
		name     string
		inMemory bool
		wantErr  bool
	}{
		{name: "a daemon on in-memory state has no file to read", inMemory: true, wantErr: true},
		{name: "a daemon on its file is read", inMemory: false, wantErr: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := refuseInMemoryRead(tc.inMemory)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("refuseInMemoryRead(%v) = %v, wantErr %v", tc.inMemory, err, tc.wantErr)
			}
		})
	}
}
