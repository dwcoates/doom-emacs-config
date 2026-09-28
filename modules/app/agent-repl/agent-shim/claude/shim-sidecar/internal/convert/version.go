package convert

// version.go — THE CONVERSION VERSION, and the rows a record can stop
// converting to.
//
// Every row the file plane writes records the conversion that produced it
// (store.v1 StoreEntry.conversion_version), and every cursor records the
// conversion its file was read under (CursorConversion). When this number
// advances, the reader re-reads each transcript whose rows an older conversion
// produced from its start: every row a record still converts to is re-derived
// (the store keeps an unchanged one as it is, without telling any reader), and
// every row a record NO LONGER converts to is retired. Without it a conversion
// fix only ever reaches bytes read after it: re-reading writes the new keys,
// and the old rows under the old keys stand forever.

import (
	"strconv"

	storev1 "agentrepl/proto/store/v1"
)

// ConversionVersion is the version of this package's conversion of a vendor
// record into store rows.
//
// BUMP IT WHENEVER A RECORD CONVERTS DIFFERENTLY: a record that becomes a
// different row, stops becoming one, or changes the content of one. A bump
// re-reads every transcript whose rows predate it, which on the owner's
// machine is the whole corpus once, in bounded batches beside the live reads.
// It is never lowered.
//
//   - 1: the file plane never mints a prompt for a record no person typed —
//     slash-command envelopes, local-command output, interrupt markers, bare
//     /compact lines, task notifications and non-human origins become residue
//     or the spawn's settle. Rows stored before versioning read as version 0.
//   - 2: every entry a transcript record converts to states its conversation
//     place (place.go): the timestamp of the record that opened its unit and
//     its index among the record's entries. The re-read is what gives every
//     row stored without a place its recorded one.
const ConversionVersion uint32 = 2

// RetiredKeys answers the upsert keys a record could own that the entries it
// converted to now do not carry: the rows a re-read of an older conversion's
// bytes must retire, because nothing will ever write those keys again.
//
// ONLY KEYS NO OTHER RECORD CAN PRODUCE. A key is named here only when it is
// minted from THIS record's own uuid and from nothing else, so its row can only
// ever have come from this record: a prompt this record opened (PromptKey — the
// turn of an adopted prompt IS its record's uuid), a peer message it carried
// (PeerKey), and a mid-turn API error it recorded (SessionKey "api_error").
// A key another record can also produce — a context cut, which a later summary
// supersedes under the boundary's uuid — is never named, because its row's
// absence from THIS record's conversion says nothing about whether it is stale.
//
// A record with no uuid owns no such key and names nothing.
func RetiredKeys(record map[string]any, converted []*storev1.StoreEntry) []string {
	uuid := str(record["uuid"])
	if uuid == "" {
		return nil
	}
	produced := make(map[string]bool, len(converted))
	for _, entry := range converted {
		produced[entry.GetUpsertKey()] = true
	}
	var retired []string
	for _, key := range []string{PromptKey(uuid), PeerKey(uuid), SessionKey("api_error", uuid)} {
		if !produced[key] {
			retired = append(retired, key)
		}
	}
	return retired
}

// versionTag is the conversion version as it is digested into every write
// identity (writeID): the same bytes re-read under a new conversion are a NEW
// write, which the store's ledger must not absorb as a replay of the old one —
// or a record whose content changed would keep the content the old conversion
// gave it.
var versionTag = "v" + strconv.FormatUint(uint64(ConversionVersion), 10)
