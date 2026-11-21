package audiooutput

// fourCC spells a CoreAudio four-character code as its four characters, the
// way CoreAudio's headers write it ('blue', 'hdpn').
func fourCC(code uint32) string {
	return string([]byte{byte(code >> 24), byte(code >> 16), byte(code >> 8), byte(code)})
}
