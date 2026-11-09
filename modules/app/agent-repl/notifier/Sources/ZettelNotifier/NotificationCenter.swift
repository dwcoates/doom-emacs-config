import Foundation
import UserNotifications
import ZettelProto

/// The key a posted notification stores its click token under.
public let clickTokenKey = "agent-repl.click"

/// Maps the notification center's answer to `add` onto a post's result: no
/// error is posted, the center's not-allowed error is the user's setting, and
/// anything else is a failure carrying the platform's own description.
public func postResult(for error: Error?) -> PostResult {
    guard let error else { return .posted }
    let ns = error as NSError
    if ns.domain == UNErrorDomain && ns.code == UNError.Code.notificationsNotAllowed.rawValue {
        return .notAllowed
    }
    return .failed("\(ns.domain) \(ns.code): \(ns.localizedDescription)")
}

/// The notification content one banner posts as: its title and body, no sound
/// (the daemon rings the banner's one chime), and its click token stored for
/// the click to hand back.
public func notificationContent(for banner: Agentrepl_V1_DesktopBanner) throws -> UNMutableNotificationContent {
    let content = UNMutableNotificationContent()
    content.title = banner.title.text
    content.body = banner.body.text
    content.userInfo = [clickTokenKey: try banner.click.serializedData()]
    return content
}

/// Reads a clicked notification's click token, nil when it carries none.
public func clickToken(from userInfo: [AnyHashable: Any]) -> Data? {
    userInfo[clickTokenKey] as? Data
}

/// Posts banners through macOS's notification center. A banner's group is the
/// notification's identifier, so a banner replaces the earlier one of its
/// group.
public struct NotificationCenterPoster: BannerCenter {
    public init() {}

    public func post(_ banner: Agentrepl_V1_DesktopBanner) async -> PostResult {
        let content: UNMutableNotificationContent
        do {
            content = try notificationContent(for: banner)
        } catch {
            return .failed("encode the click token: \(error)")
        }
        let request = UNNotificationRequest(identifier: banner.group.id, content: content, trigger: nil)
        do {
            try await UNUserNotificationCenter.current().add(request)
            return .posted
        } catch {
            return postResult(for: error)
        }
    }
}

/// One line naming a notification authorization, and whether it lets the
/// app post: the answer of `--check-notifications`.
public func authorizationReport(_ status: UNAuthorizationStatus) -> (line: String, allowed: Bool) {
    switch status {
    case .authorized: return ("Zettel may notify", true)
    case .provisional: return ("Zettel may notify quietly (provisional)", true)
    case .ephemeral: return ("Zettel may notify (ephemeral)", true)
    case .denied: return ("Zettel may not notify: denied (System Settings → Notifications → Zettel)", false)
    case .notDetermined: return ("Zettel may not notify: not asked yet (run Zettel --request-notifications)", false)
    @unknown default: return ("Zettel may not notify: an authorization this app does not know (\(status.rawValue))", false)
    }
}
