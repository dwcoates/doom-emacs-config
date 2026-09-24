package holds

// HeldBadges exposes the badge composer to the external test package, so every
// arm — including ones no durable record can reach — is tested directly.
var HeldBadges = heldBadges

// TruncateLabel exposes the command truncation for the same reason.
var TruncateLabel = truncateLabel
