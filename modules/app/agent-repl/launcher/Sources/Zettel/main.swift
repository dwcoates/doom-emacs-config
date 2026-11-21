// Zettel.app, the launcher. A background app (no window, no Dock icon) that
// opens Emacs, and with it the backend, when it is opened by hand, and posts
// every desktop banner a daemon hands it under its own name and icon.
// Everything it does lives in ZettelLauncher; this file is the AppKit shell.
import AppKit
import UserNotifications
import ZettelLauncher

/// The application whose window a click or an open by hand brings forward.
let emacsBundleID = "org.gnu.Emacs"

/// The launcher's launchd agent, which an open by hand starts when it is
/// not running.
let launcherLabel = "app.zettel.launcher"

/// The name the launcher's socket holder holds its socket under
/// (launcher/requirements.json, socket-holder-launcher).
let heldSocketName = "launcher"

enum Op {
    static let app = "launcher.app"
}

final class AppDelegate: NSObject, NSApplicationDelegate, UNUserNotificationCenterDelegate {
    let options: Options
    let log: FileLogger
    var clicks: ClickReporter!
    var watcher: AdvertisementWatcher!
    var server: BannerServer!
    /// The one-instance lock, held for the life of the process.
    var lock: Int32 = -1

    init(options: Options, log: FileLogger) {
        self.options = options
        self.log = log
    }

    // The delegate is set BEFORE launching finishes, so a click that launched
    // the app is delivered to it.
    func applicationWillFinishLaunching(_ notification: Notification) {
        UNUserNotificationCenter.current().delegate = self
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        log.info(Op.app, "Zettel started", ["resident": options.resident, "state_dir": options.stateDir.path])
        // OPENED BY HAND, AND NO LAUNCHER RUNS: opening the bundle while the
        // resident launcher runs reopens that one instead
        // (applicationShouldHandleReopen), so this copy opens Emacs, starts
        // the resident service, and exits. Only launchd's copy ever serves.
        guard options.resident else {
            bringEmacsForward(because: "Zettel was opened by hand")
            startResidentLauncher()
            NSApp.terminate(nil)
            return
        }
        do {
            lock = try takeInstanceLock(at: options.runDir.appendingPathComponent("launcher.lock"))
        } catch {
            log.error(Op.app, "another launcher already serves; this one exits", ["cause": "\(error)"])
            NSApp.terminate(nil)
            return
        }
        UNUserNotificationCenter.current().requestAuthorization(options: [.alert]) { [log] granted, error in
            if let error {
                log.error(Op.app, "asking for permission to notify failed", ["cause": "\(error)"])
            } else if !granted {
                log.error(Op.app, "Zettel is not allowed to notify; banners stay unposted until System Settings → Notifications → Zettel allows it", [:])
            } else {
                log.info(Op.app, "Zettel is allowed to notify", [:])
            }
        }
        let listener: Int32
        do {
            listener = try takeHeldSocket(control: options.holder, name: heldSocketName)
        } catch {
            log.error(Op.app, "could not take the launcher's socket from its holder; Zettel exits", ["holder": options.holder, "cause": "\(error)"])
            NSApp.terminate(nil)
            return
        }
        server = BannerServer(listener: listener, exchange: BannerExchange(center: NotificationCenterPoster(), log: log), log: log)
        server.start()
        do {
            let build = try executableBuild(at: Bundle.main.executableURL!)
            try writeBuildReport(dir: options.runDir, pid: getpid(), build: build)
        } catch {
            log.error(Op.app, "could not report this launcher's build; a deploy restarts it as stale", ["run_dir": options.runDir.path, "cause": "\(error)"])
        }
        clicks = ClickReporter(dial: { ConnectDaemon(address: $0) }, log: log)
        watcher = AdvertisementWatcher(stateDir: options.stateDir) { [clicks] advertisement in
            Task { await clicks!.advertised(advertisement) }
        }
        do {
            try watcher.start()
        } catch {
            log.error(Op.app, "could not watch the state root for the daemon's advertisement; Zettel exits", ["cause": "\(error)"])
            NSApp.terminate(nil)
            return
        }
        log.info(Op.app, "the launcher serves banners on its held socket", ["holder": options.holder])
    }

    /// Starts the resident launcher's launchd agent (`launchctl kickstart`).
    func startResidentLauncher() {
        let kickstart = Process()
        kickstart.executableURL = URL(fileURLWithPath: "/bin/launchctl")
        kickstart.arguments = ["kickstart", "gui/\(getuid())/\(launcherLabel)"]
        do {
            try kickstart.run()
            kickstart.waitUntilExit()
        } catch {
            log.error(Op.app, "could not start the resident launcher", ["label": launcherLabel, "cause": "\(error)"])
            return
        }
        if kickstart.terminationStatus != 0 {
            log.error(Op.app, "launchctl refused to start the resident launcher", ["label": launcherLabel, "status": kickstart.terminationStatus])
        } else {
            log.info(Op.app, "started the resident launcher", ["label": launcherLabel])
        }
    }

    // Opened by hand while already running.
    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        bringEmacsForward(because: "Zettel was opened by hand")
        return false
    }

    func userNotificationCenter(
        _ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse,
        withCompletionHandler completionHandler: @escaping () -> Void
    ) {
        defer { completionHandler() }
        guard response.actionIdentifier == UNNotificationDefaultActionIdentifier else { return }
        bringEmacsForward(because: "a banner was clicked")
        guard let token = clickToken(from: response.notification.request.content.userInfo) else {
            log.error(Op.app, "a clicked banner carried no click token; it selects nothing", ["identifier": response.notification.request.identifier])
            return
        }
        guard let clicks else {
            log.error(Op.app, "a banner was clicked while this launcher serves nothing; it selects nothing", ["resident": options.resident])
            return
        }
        Task { await clicks.clicked(token: token) }
    }

    func userNotificationCenter(
        _ center: UNUserNotificationCenter, willPresent notification: UNNotification,
        withCompletionHandler completionHandler: @escaping (UNNotificationPresentationOptions) -> Void
    ) {
        completionHandler([.banner, .list])
    }

    func bringEmacsForward(because reason: String) {
        guard let url = NSWorkspace.shared.urlForApplication(withBundleIdentifier: emacsBundleID) else {
            log.error(Op.app, "Emacs is not installed where macOS can find it; nothing was brought forward", ["reason": reason, "bundle": emacsBundleID])
            return
        }
        let configuration = NSWorkspace.OpenConfiguration()
        configuration.activates = true
        NSWorkspace.shared.openApplication(at: url, configuration: configuration) { [log] _, error in
            if let error {
                log.error(Op.app, "bringing Emacs forward failed", ["reason": reason, "cause": "\(error)"])
            } else {
                log.debug(Op.app, "brought Emacs forward", ["reason": reason])
            }
        }
    }
}

let options: Options
do {
    options = try parseOptions(Array(CommandLine.arguments.dropFirst()), environment: ProcessInfo.processInfo.environment,
                               home: FileManager.default.homeDirectoryForCurrentUser)
} catch {
    FileHandle.standardError.write(Data("Zettel: \(error)\n".utf8))
    exit(2)
}
// THE PERMISSION MODES answer and exit before any app runs: they are the
// installer's check and request (bin/requirements.sh), run as the bundle's
// own executable so macOS answers for Zettel.app.
func exitWithAuthorization() -> Never {
    let settled = DispatchSemaphore(value: 0)
    var status = UNAuthorizationStatus.notDetermined
    UNUserNotificationCenter.current().getNotificationSettings { settings in
        status = settings.authorizationStatus
        settled.signal()
    }
    settled.wait()
    let report = authorizationReport(status)
    print(report.line)
    exit(report.allowed ? 0 : 1)
}
switch options.mode {
case .run:
    break
case .checkNotifications:
    exitWithAuthorization()
case .requestNotifications:
    let asked = DispatchSemaphore(value: 0)
    UNUserNotificationCenter.current().requestAuthorization(options: [.alert]) { _, error in
        if let error { FileHandle.standardError.write(Data("Zettel: asking to notify failed: \(error)\n".utf8)) }
        asked.signal()
    }
    asked.wait()
    exitWithAuthorization()
}
let delegate = AppDelegate(options: options, log: FileLogger(path: options.logPath))
let app = NSApplication.shared
app.setActivationPolicy(.accessory)
app.delegate = delegate
app.run()
