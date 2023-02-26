// Disable Firefox sync and its menu entries
user_pref("identity.fxaccounts.enabled", false);
// Disable automatic hiding for fullscreen
user_pref("browser.fullscreen.autohide", false);
// do not show bookmarks in toolbar
user_pref("browser.toolbars.bookmarks.visibility", "never")
// disable firefox-view
user_pref("browser.tabs.firefox-view", false)
user_pref("browser.ctrlTab.sortByRecentlyUsed", true);
user_pref("extensions.pocket.enabled", false);
user_pref("keyword.enabled", true);
// Disable the Twitter/R*ddit/Faceberg ads in the URL bar:
user_pref("browser.urlbar.quicksuggest.enabled", false);
user_pref("browser.urlbar.suggest.topsites", false); // [FF78+]
// Do not prefil forms:
user_pref("signon.prefillForms", false);
// Do not autocomplete in the URL bar:
user_pref("browser.urlbar.autoFill", false);
// Allow access to http (i.e. not https) sites:
user_pref("dom.security.https_only_mode", false);
// Disable push notifications:
user_pref("dom.push.enabled", false);
