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
        return .notAllowed("the notification center refused: notifications are not allowed")
    }
    return .failed("\(ns.domain) \(ns.code): \(ns.localizedDescription)")
}

/// The three of Zettel's notification settings that decide whether a banner
/// is drawn on screen.
public struct DisplaySettings: Equatable, Sendable {
    public var authorization: UNAuthorizationStatus
    public var alert: UNNotificationSetting
    public var style: UNAlertStyle

    public init(authorization: UNAuthorizationStatus, alert: UNNotificationSetting, style: UNAlertStyle) {
        self.authorization = authorization
        self.alert = alert
        self.style = style
    }

    /// The settings macOS reports for Zettel.
    public init(_ settings: UNNotificationSettings) {
        self.init(authorization: settings.authorizationStatus, alert: settings.alertSetting, style: settings.alertStyle)
    }
}

/// The setting that keeps a banner off the screen, or nil when Zettel's
/// settings draw it. THE ONE ANSWER to whether a banner can appear: a post
/// asks it before handing macOS the banner, and `--check-notifications` asks
/// it for the installer.
///
/// macOS accepts a banner (`add` answers no error) even when its settings
/// will never draw it, so the center's answer alone cannot say a banner
/// appeared. The daemon rings the chime on `posted`, so `posted` is answered
/// only when the settings draw the banner.
public func offScreenSetting(_ settings: DisplaySettings) -> String? {
    switch settings.authorization {
    case .authorized: break
    case .denied: return "notifications are denied"
    case .notDetermined: return "notifications were never allowed (run Zettel --request-notifications)"
    case .provisional: return "notifications are delivered quietly (provisional), never drawn"
    case .ephemeral: return "notifications are ephemeral"
    @unknown default: return "the authorization is one this app does not know (\(settings.authorization.rawValue))"
    }
    switch settings.alert {
    case .enabled: break
    case .disabled: return "alerts are off"
    case .notSupported: return "alerts are not supported"
    @unknown default: return "the alert setting is one this app does not know (\(settings.alert.rawValue))"
    }
    switch settings.style {
    case .banner, .alert: return nil
    case .none: return "the alert style is None"
    @unknown default: return "the alert style is one this app does not know (\(settings.style.rawValue))"
    }
}

/// Posts one banner only when SETTINGS draw it on screen: otherwise it is not
/// handed to macOS at all and answers the setting that keeps it off. ADD hands
/// the banner to the notification center.
public func gatedPost(_ settings: DisplaySettings, add: () async throws -> Void) async -> PostResult {
    if let setting = offScreenSetting(settings) {
        return .notAllowed(setting)
    }
    do {
        try await add()
        return .posted
    } catch {
        return postResult(for: error)
    }
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
        let center = UNUserNotificationCenter.current()
        let settings = DisplaySettings(await center.notificationSettings())
        return await gatedPost(settings) { try await center.add(request) }
    }
}

/// One line saying whether Zettel's settings draw banners, and whether they
/// do: the answer of `--check-notifications`.
public func displayReport(_ settings: DisplaySettings) -> (line: String, allowed: Bool) {
    guard let setting = offScreenSetting(settings) else { return ("Zettel may notify", true) }
    return ("Zettel's banners stay off screen: \(setting) (System Settings → Notifications → Zettel)", false)
}
