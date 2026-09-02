package db

import (
	"errors"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestEncodePointerRoundTripsEveryPosition(t *testing.T) {
	// Arrange
	tests := []struct {
		name     string
		position int64
	}{
		{name: "first", position: 1},
		{name: "two digit", position: 36},
		{name: "large", position: 1 << 40},
		{name: "max", position: 1<<63 - 1},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got, err := decodePointer(encodePointer(test.position), "after")

			// Assert
			if err != nil {
				t.Fatalf("decodePointer: %v", err)
			}
			if got != test.position {
				t.Fatalf("position = %d, want %d", got, test.position)
			}
		})
	}
}

func TestDecodePointerRefusesAnUnsetPointer(t *testing.T) {
	// Arrange, Act
	_, err := decodePointer(nil, "after")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestDecodePointerRefusesAnEmptyValue(t *testing.T) {
	// Arrange, Act
	_, err := decodePointer(&storev1.StoreItemPointer{}, "after")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestDecodePointerRefusesAValueThisStoreNeverMinted(t *testing.T) {
	// Arrange: a plausible-looking opaque value with no store prefix. Refusing
	// it as INVALID rather than STALE is the point: a caller sending garbage
	// must not be told to re-open and try again.
	pointer := &storev1.StoreItemPointer{Value: "deadbeef"}

	// Act
	_, err := decodePointer(pointer, "known_through")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if errors.Is(err, ErrStalePointer) {
		t.Fatal("a value this store never minted was reported as merely stale")
	}
}

func TestDecodePointerRefusesANonNumericBody(t *testing.T) {
	// Arrange
	pointer := &storev1.StoreItemPointer{Value: pointerPrefix + "not base36!"}

	// Act
	_, err := decodePointer(pointer, "after")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

func TestDecodePointerRefusesAPositionThisStoreNeverAssigns(t *testing.T) {
	// Arrange: positions are AUTOINCREMENT rowids, so zero is never one.
	pointer := &storev1.StoreItemPointer{Value: pointerPrefix + "0"}

	// Act
	_, err := decodePointer(pointer, "after")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}
