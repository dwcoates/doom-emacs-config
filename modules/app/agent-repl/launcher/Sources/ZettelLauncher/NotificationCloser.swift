import Foundation

// SLACK'S AND ZETTEL'S NOTIFICATIONS CLOSE AFTER A SET TIME. macOS shows a
// temporary notification for about five seconds and gives no setting for
// longer, so the user sets each closed app's alert style to Persistent and the
// launcher closes each of its notifications itself once it has been on screen
// for the configured number of seconds (NotificationSeconds, default 15). Each
// app still decides which events notify, so every per-channel setting holds.
//
// The launcher reaches the notifications only through the Accessibility API
// (AccessibilityScreen.swift), whose view of Notification Center's windows
// Apple does not document. EVERY WAY THAT VIEW CAN STOP MATCHING is recorded
// at WARN or ERROR, so a macOS update that changes it is seen in the
// launcher's log the first time it happens, never as notifications that
// silently stay up.

/// Slack's name, as the first part of its notifications' Accessibility
/// description names it.
public let slackAppName = "Slack"

/// Zettel's own name, its bundle's CFBundleName (Bundle/Info.plist), which
/// macOS shows above each of its banners.
public let zettelAppName = "Zettel"

/// The apps whose notifications close after the configured time.
public let closedApps: Set<String> = [slackAppName, zettelAppName]

/// The defaults key (domain `app.zettel`) holding how many seconds a closed
/// app's notification stays on screen.
public let notificationSecondsKey = "NotificationSeconds"

/// How many seconds a notification stays on screen when the user set no
/// NotificationSeconds.
public let defaultNotificationSeconds = 15

/// The action macOS's notification offers to dismiss it.
public let closeActionName = "Close"

enum CloserOp {
    static let close = "launcher.notification_closer"
}

/// One notification on screen, as Notification Center's Accessibility tree
/// shows it.
public struct ShownNotification: Equatable, Sendable {
    /// The notification's Accessibility identifier, which names it for as
    /// long as it is on screen.
    public let identifier: String
    /// Its Accessibility description, `<app>, <title>, <body>`, or nil when
    /// it carries none.
    public let description: String?
    /// The names of the actions it offers (`Close`, `Show Details`, ...).
    public let actions: [String]

    public init(identifier: String, description: String?, actions: [String]) {
        self.identifier = identifier
        self.description = description
        self.actions = actions
    }

    /// The app that posted it, read from its description, or nil when the
    /// description names none.
    public var app: String? {
        guard let description, let comma = description.range(of: ", ") else { return nil }
        return String(description[..<comma.lowerBound])
    }
}

/// What one look at Notification Center's on-screen windows found.
public struct ScreenScan: Equatable, Sendable {
    /// Every notification on screen. Notification Center's own panel, the
    /// history the user opens from the menu bar, is never part of it.
    public var notifications: [ShownNotification]
    /// Each window holding a notification group this launcher cannot act on
    /// (a layout it does not recognize), by a key naming the window for as
    /// long as it is open, with its layout (`layoutOutline`).
    public var unrecognizedWindows: [String: String]
    /// Each window holding neither a notification group nor the panel's
    /// list, by key, with its layout. A banner's window is left so for a
    /// moment after its notification leaves (measured on macOS 26.2,
    /// 2026-11-26: 6 ms after a Close, `AXScrollArea(AXGroup)`); a window
    /// that never held a notification and is empty is a layout this launcher
    /// does not recognize.
    public var emptyWindows: [String: String]
    /// The key of each window holding at least one notification.
    public var notificationWindows: Set<String>

    public init(notifications: [ShownNotification], unrecognizedWindows: [String: String] = [:],
                emptyWindows: [String: String] = [:], notificationWindows: Set<String> = []) {
        self.notifications = notifications
        self.unrecognizedWindows = unrecognizedWindows
        self.emptyWindows = emptyWindows
        self.notificationWindows = notificationWindows
    }
}

/// Notification Center's windows, as the closer reads and acts on them.
public protocol NotificationScreen: AnyObject {
    /// Looks at every window Notification Center shows now.
    func scan() throws -> ScreenScan
    /// Performs the action named `action` on the notification `identifier`
    /// found by the last scan.
    func perform(_ action: String, on identifier: String) throws
}

/// A pending call the scheduler can cancel.
public protocol ScheduledCall: AnyObject {
    func cancel()
}

/// When the closer's deadlines fall due.
public protocol DeadlineScheduler {
    func now() -> Date
    /// Calls `fire` once at `date`, on the thread the closer runs on.
    func schedule(at date: Date, _ fire: @escaping () -> Void) -> ScheduledCall
}

/// How long a notification stays on screen, as the user set it.
public enum NotificationDuration: Equatable, Sendable {
    case seconds(Int)
    /// The value set is not a whole number of seconds above zero.
    case invalid(String)
}

/// Reads NotificationSeconds from `defaults`: unset is the default, a
/// whole number above zero is that many seconds, anything else is invalid.
public func notificationDuration(_ defaults: UserDefaults) -> NotificationDuration {
    guard let value = defaults.object(forKey: notificationSecondsKey) else {
        return .seconds(defaultNotificationSeconds)
    }
    if let number = value as? NSNumber, CFGetTypeID(number) != CFBooleanGetTypeID(),
       number.doubleValue == number.doubleValue.rounded(), number.intValue > 0 {
        return .seconds(number.intValue)
    }
    if let text = value as? String, let seconds = Int(text), seconds > 0 {
        return .seconds(seconds)
    }
    return .invalid("\(value)")
}

/// Closes every notification of a closed app once it has been on screen for
/// its configured time. Runs on one thread (the main run loop): the screen
/// calls `screenChanged` whenever Notification Center's windows change, and
/// the scheduler calls back when a deadline falls due.
public final class NotificationCloser {
    /// A notification waiting to close: the app that posted it, and when.
    private struct Pending {
        let app: String
        let deadline: Date
    }

    private let screen: NotificationScreen
    private let scheduler: DeadlineScheduler
    private let duration: () -> NotificationDuration
    private let log: Logger
    /// Each closed app's notification on screen, by identifier.
    private var waiting: [String: Pending] = [:]
    /// Identifiers already recorded as unreadable or refused, so each is
    /// recorded once rather than at every change of the screen.
    private var reported: Set<String> = []
    /// Unrecognized or empty windows already recorded, by key.
    private var reportedWindows: Set<String> = []
    /// Each window on screen seen holding a notification, by key: once it is
    /// empty, its notification left rather than its layout changed.
    private var heldNotification: Set<String> = []
    private var timer: ScheduledCall?

    public init(screen: NotificationScreen, scheduler: DeadlineScheduler,
                duration: @escaping () -> NotificationDuration, log: Logger) {
        self.screen = screen
        self.scheduler = scheduler
        self.duration = duration
        self.log = log
    }

    /// The notifications the closer is waiting to close, by identifier.
    public var pending: [String: Date] { waiting.mapValues(\.deadline) }

    /// Notification Center's windows changed: a closed app's notification
    /// seen for the first time gets its deadline, and one gone from the
    /// screen is forgotten.
    public func screenChanged() {
        guard let scan = look() else { return }
        for (window, layout) in scan.emptyWindows.sorted(by: { $0.key < $1.key }) where !reportedWindows.contains(window) {
            if heldNotification.contains(window) {
                log.debug(CloserOp.close, "a Notification Center window emptied as its notification left", ["window": window, "layout": layout])
            } else {
                warnUnrecognized(window, layout)
            }
        }
        for (window, layout) in scan.unrecognizedWindows.sorted(by: { $0.key < $1.key }) where !reportedWindows.contains(window) {
            warnUnrecognized(window, layout)
        }
        reportedWindows = Set(scan.unrecognizedWindows.keys).union(scan.emptyWindows.keys)
        heldNotification = heldNotification.union(scan.notificationWindows)
            .intersection(scan.notificationWindows.union(scan.unrecognizedWindows.keys).union(scan.emptyWindows.keys))
        let now = scheduler.now()
        var shown: Set<String> = []
        for notification in scan.notifications {
            shown.insert(notification.identifier)
            guard let app = notification.app else {
                if reported.insert(notification.identifier).inserted {
                    log.warn(CloserOp.close, "a notification on screen names no app in its description; macOS may have changed its layout, and a notification like it stays on screen",
                             ["identifier": notification.identifier, "description": notification.description ?? "<none>"])
                }
                continue
            }
            guard closedApps.contains(app), waiting[notification.identifier] == nil,
                  !reported.contains(notification.identifier) else { continue }
            switch duration() {
            case .seconds(let seconds):
                waiting[notification.identifier] = Pending(app: app, deadline: now.addingTimeInterval(TimeInterval(seconds)))
                log.info(CloserOp.close, "a notification appeared; it closes after its time on screen",
                         ["identifier": notification.identifier, "app": app, "seconds": seconds])
            case .invalid(let value):
                reported.insert(notification.identifier)
                log.error(CloserOp.close, "NotificationSeconds is not a whole number of seconds above zero, so this notification stays on screen; set it with defaults write app.zettel NotificationSeconds -int <seconds>",
                          ["identifier": notification.identifier, "app": app, "value": value])
            }
        }
        for (identifier, pending) in waiting where !shown.contains(identifier) {
            waiting[identifier] = nil
            log.debug(CloserOp.close, "a notification left the screen before its time", ["identifier": identifier, "app": pending.app])
        }
        reported.formIntersection(shown)
        reschedule()
    }

    /// The earliest deadline fell due: each notification whose time is up is
    /// closed.
    func deadlineReached() {
        timer = nil
        guard let scan = look() else { return }
        let now = scheduler.now()
        let byIdentifier = Dictionary(scan.notifications.map { ($0.identifier, $0) }, uniquingKeysWith: { first, _ in first })
        for (identifier, pending) in waiting where pending.deadline <= now {
            waiting[identifier] = nil
            let context: LogContext = ["identifier": identifier, "app": pending.app]
            guard let notification = byIdentifier[identifier] else {
                log.debug(CloserOp.close, "a notification left the screen before its time", context)
                continue
            }
            guard notification.actions.contains(closeActionName) else {
                log.error(CloserOp.close, "a notification offers no Close action, so it stays on screen; macOS may have changed its notifications' actions",
                          context.merging(["actions": notification.actions.joined(separator: ", ")]) { first, _ in first })
                continue
            }
            do {
                try screen.perform(closeActionName, on: identifier)
                log.info(CloserOp.close, "closed a notification after its time on screen", context)
            } catch {
                log.error(CloserOp.close, "closing a notification failed, so it stays on screen", context.merging(["cause": "\(error)"]) { first, _ in first })
            }
        }
        reschedule()
    }

    private func warnUnrecognized(_ window: String, _ layout: String) {
        log.warn(CloserOp.close, "a Notification Center window holds no notification this launcher recognizes; macOS may have changed its layout, and notifications in it stay on screen",
                 ["window": window, "layout": layout])
    }

    /// One scan of the screen, or nil after recording why it failed.
    private func look() -> ScreenScan? {
        do {
            return try screen.scan()
        } catch {
            log.error(CloserOp.close, "reading Notification Center's windows failed, so notifications stay on screen", ["cause": "\(error)"])
            return nil
        }
    }

    /// Arms the one timer for the earliest deadline, if any.
    private func reschedule() {
        timer?.cancel()
        timer = nil
        guard let earliest = waiting.values.map(\.deadline).min() else { return }
        timer = scheduler.schedule(at: earliest) { [weak self] in self?.deadlineReached() }
    }
}
