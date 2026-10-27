---
name: gns-auth-renew
description: 'Renew an expired or missing GNS token yourself, without asking, signing in ONLY through the work account''s own Chrome profile. Use whenever a gns command or the LLM gateway fails on authentication (for example `HTTP 401: Invalid or expired JWT`, "not logged in", "token expired"), whenever a GNS token is missing, and before any `gns auth login`, which must never be run with its default browser flow.'
argument-hint: "[environment, default production]"
allowed-tools: Bash(~/.claude/skills/gns-auth-renew/run.sh:*), Bash(.claude/skills/gns-auth-renew/run.sh:*)
lineage_root: user.dodge.skills.gns-auth-renew
---

## What This Skill Does

Renews the GNS token for one environment through the device-code sign-in, opened in the work account's Chrome profile, and confirms the environment reads as the work account. An expired token is renewed and the interrupted work continues; the user is never asked first.

## Arguments

| Argument | Behaviour |
|---|---|
| *(none)* | Renew the `production` environment's token. |
| `<env>` | Renew that environment's token (for example `local`). |

## Steps

1. Renew the token.
  - Call `run.sh --renew [<env>]` from this skill's base directory.
    - `EXIT CODE 0:` The token is renewed. Resume the work the failure interrupted.
    - `EXIT CODE 1:` IMMEDIATELY terminate and surface the raw usage error.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 3:` The work account or its Chrome profile is not set up. STOP and surface the printed message in the response's setup branch.
    - `EXIT CODE 4:` The sign-in did not complete. Relay the sign-in code the call printed, ask the user to finish signing in, then restart step 1.

## Notes

- CRITICAL: NEVER run `gns auth login` without `--device`: its default flow opens the system default browser and profile.
- CRITICAL: The sign-in page opens ONLY in the work account's Chrome profile, never another profile or browser.
- IMPORTANT NOTE: Do not ask before renewing; an expired token is renewed and the work continues.
- CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals. React only to the documented exit codes.
