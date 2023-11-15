user_pref("browser.uidensity", 1);
// disable Firefox sync
user_pref("identity.fxaccounts.enabled", false);
// disable fullscreen autohide
user_pref("browser.fullscreen.autohide", false);
// do not show bookmarks in toolbar
user_pref("browser.toolbars.bookmarks.visibility", "never");
// disable firefox-view
user_pref("browser.tabs.firefox-view", false);
user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
user_pref("extensions.pocket.enabled", false);
user_pref("keyword.enabled", true);
// disable the Twitter/R*ddit/Faceberg ads in the URL bar
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.suggest.topsites", false); // [FF78+]
// Do not prefil forms:
user_pref("signon.prefillForms", false);
// Do not autocomplete in the URL bar:
user_pref("browser.urlbar.autoFill", false);
// allow access to http sites
user_pref("dom.security.https_only_mode", false);
// disable push notifications
user_pref("dom.push.enabled", false);
// save history
user_pref("privacy.sanitize.sanitizeOnShutdown", false);
// maximum window size
user_pref("privacy.window.maxInnerWidth", 1000);
user_pref("privacy.window.maxInnerHeight", 700);
// disable website is now full screen warning
user_pref("full-screen-api.warning.timeout", 0);
// no smooth scrolling
user_pref("general.smoothScroll", false);
// This could otherwise cause some issues on bank logins and other annoying sites:
user_pref("network.http.referer.XOriginPolicy", 0);
// Fix the issue where right mouse button instantly clicks
user_pref("ui.context_menus.after_mouseup", true);
