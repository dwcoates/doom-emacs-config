# Vetting register: the account's monthly spend

## 1. The SDK's runtime usage answer carries `extra_usage` for a spend-billed account

- **Assumption**: `usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET({skipBehaviors: true})`
  answers the work account (subscription type `enterprise`) with a non-null
  `rate_limits` whose `extra_usage` carries `is_enabled`, `monthly_limit`,
  `used_credits` and `currency`, as the vendor's own usage cache does, and in
  the same minor units.
- **Affected if false**: `conversation.v1.SessionPlanUsage.monthly_spend` is
  never set for that account, so `frontend.v1.FooterActivityEnduringUsage.monthly_spend`
  never draws and the line stays `unobserved`.
- **How to verify**: after the change deploys, start a turn in a workspace on
  the work account and read the shim's `shim.convert.plan_usage` records for
  it: the record "the vendor stated the account's monthly spend" names the
  amounts; compare them with `cachedUsageUtilization.utilization.extra_usage`
  in the work account root's `.claude.json`.
- **Status**: OPEN.
