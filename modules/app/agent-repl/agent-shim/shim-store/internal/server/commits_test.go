package server

import "testing"

func TestCommitSignal(t *testing.T) {
	tests := []struct {
		name      string
		fires     int
		wantWoken bool
	}{
		{name: "a channel taken before a commit is closed by it", fires: 1, wantWoken: true},
		{name: "a channel taken with no commit since stays open", fires: 0, wantWoken: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c := newCommitSignal()
			taken := c.next()

			// Act
			for range tt.fires {
				c.fire()
			}

			// Assert
			select {
			case <-taken:
				if !tt.wantWoken {
					t.Fatal("the channel closed with no commit")
				}
			default:
				if tt.wantWoken {
					t.Fatal("the commit left the channel taken before it open")
				}
			}
		})
	}
}

func TestCommitSignalArmsAFreshChannelAfterEachCommit(t *testing.T) {
	// Arrange
	c := newCommitSignal()
	c.fire()

	// Act
	after := c.next()

	// Assert
	select {
	case <-after:
		t.Fatal("the channel taken after the commit is already closed")
	default:
	}
}
