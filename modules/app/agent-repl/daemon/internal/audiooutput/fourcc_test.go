package audiooutput

import "testing"

func TestFourCC(t *testing.T) {
	cases := []struct {
		name string
		code uint32
		want string
	}{
		{name: "bluetooth", code: 'b'<<24 | 'l'<<16 | 'u'<<8 | 'e', want: TransportBluetooth},
		{name: "headphones", code: 'h'<<24 | 'd'<<16 | 'p'<<8 | 'n', want: DataSourceHeadphones},
		{name: "a trailing space", code: 'u'<<24 | 's'<<16 | 'b'<<8 | ' ', want: TransportUSB},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := fourCC(tc.code)

			// Assert
			if got != tc.want {
				t.Fatalf("fourCC(%#x) = %q, want %q", tc.code, got, tc.want)
			}
		})
	}
}
