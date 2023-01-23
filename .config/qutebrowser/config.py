config.source('scripts/redirector.py')
config.load_autoconfig()
c.new_instance_open_target = 'tab'
c.statusbar.position = 'bottom'
# c.downloads.position = 'top'
c.tabs.position = 'bottom'

config.set("url.default_page", "about:blank")
config.set("url.start_pages", "about:blank")
c.spellcheck.languages = ["en-US"]

c.confirm_quit = ["downloads"]  # always
c.downloads.location.prompt = True

c.scrolling.smooth = False
c.content.autoplay = False
c.tabs.background = True

c.auto_save.session = False

config.set('url.searchengines', {"DEFAULT": "https://searx.be/?q={}", "s": "https://www.startpage.com/do/search?q={}", "w": "https://wiby.me/?q={}", "S": "https://www.searx.be/search?q={}", "aw": "https://wiki.archlinux.org/?search={}",
           "wi": "https://en.wikipedia.org/wiki/Special:Search/{}", "dic": "https://www.urbandictionary.com/define.php?term=%7B{}%7D", "y": "invidious.namazso.eu/search?q={}", "yt": "https://www.youtube.com/results?search_query={}", "yy": "https://yandex.com/images/search?text={}"})


# config.unbind('gf')
config.unbind("<ctrl+q>")
# config.unbind("ad")
# config.unbind("u")
# config.unbind("gC")
# config.unbind("U")
# config.unbind("d")

config.bind('t', 'open -t')
config.bind('gc', 'tab-clone')
# config.bind('uu', 'undo')
# config.bind('uw', 'undo -w')

# true fullscreen
config.bind(';;', 'config-cycle statusbar.show in-mode always ;; config-cycle tabs.show switching always;; config-cycle content.notifications.enabled true false')

# print
config.bind('<Ctrl+Alt+p>',
            "print --pdf ~/dox/readlater/{title}.pdf ;; spawn notify-send -t 1500 'printing'")
config.bind('<Ctrl+Alt+r>',
            "spawn bach -c 'echo {url}{title} >> ~/dox/notes/todo.txt && notify-send -t 1500 'Read Later''")
# clipboard
config.bind('pP', 'open -t -- {clipboard}')
# yank
config.bind('yP', 'yank pretty-url')

# yank phone
config.bind(
    'yp', 'spawn kdeconnect-cli --refresh && kdeconnect-cli --share-text {url} --device 415a811582e4a899')
config.bind(';p', 'spawn kdeconnect-cli --refresh')
config.bind(';p', 'hint links spawn kdeconnect-cli --share-text {url} --device 415a811582e4a899')
config.set("colors.webpage.darkmode.enabled", True)

# Command mode
config.bind('<Ctrl+p>', 'completion-item-focus prev', mode='command')
config.bind('<Ctrl+n>', 'completion-item-focus next', mode='command')


config.bind('gm', 'tab-give')
config.bind('<Alt+p>', 'tab-pin')
# userscripts
config.bind('gs', 'spawn -u ~/.config/qutebrowser/scripts/selection.sh')
config.bind('ap', 'spawn --userscript ~/.config/qutebrowser/scripts/qutepocket')
config.bind(',R', 'spawn --userscript ~/.config/qutebrowser/scripts/readability')

config.bind('gS', 'view-source')

config.bind('gh', 'home')
config.bind('gH', 'history')

config.bind(',o', 'download-open')
config.bind('<Ctrl+Shift+r>', 'restart')
config.bind('<Ctrl+b>', 'bookmark-list')
config.bind('<Ctrl+Shift+b>', 'bookmark-del')
config.bind(',p', 'config-cycle -p content.plugins ;; reload')
config.bind('<Ctrl+Shift+i>', "config-cycle colors.webpage.darkmode.enabled ;; restart")
config.bind('J', 'tab-prev')
config.bind('K', 'tab-next')
config.bind('gj', 'tab-move -')
config.bind('gk', 'tab-move +')

config.bind(',b', 'spawn -d firefox {url}')
config.bind(',m', 'hint links spawn mpv $1 {hint-url}')
config.bind(',M', 'spawn -d mpv {url}')
config.bind(',a', 'hint links spawn -d mpv --profile=pseudo-gui --loop=inf {hint-url}')
config.bind(',A', 'spawn -d mpv  --profile=pseudo-gui --loop=inf {url}')

#c.url.searchengines['dictcc'] = 'https://www.dict.cc/?s={}'
#c.url.searchengines['DEFAULT'] = 'https://duckduckgo.com/?q={}'
#c.url.searchengines['ss'] = 'https://www.startpage.com/do/search?q={}'
#c.url.searchengines['y'] = 'https://invidious.namazso.eu/search?q={}'
#c.url.searchengines['yt'] = 'https://www.youtube.com/results?search_query={}'
#c.url.searchengines['aw'] = 'https://wiki.archlinux.org/?search={}'
#c.url.searchengines['w'] = 'https://en.wikipedia.org/wiki/Special:Search/{}'
#c.url.searchengines['dic'] = 'https://www.urbandictionary.com/define.php?term=%7B{}%7D'
#c.url.searchengines['maps'] = 'https://www.google.com/maps?q=%s'

# Security enhancements
# c.content.headers.accept_language en-US,en;q = 0.5
# c.content.headers.custom = '{"accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"}'

# config.set ('content.headers.user_agent', 'Mozilla/5.0 (Windows NT 10.0; rv:68.0) Gecko/20100101 Firefox/68.0')
# config.set ('content.headers.user_agent', 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.131 Safari/537.36')
# Per site settings
# Javascript
c.content.javascript.enabled = True
config.set('content.javascript.enabled', False, '*://www.google.com/')
c.completion.show = "always"  # never
c.content.javascript.can_access_clipboard = False
# c.content.javascript.can_open_tabs_automatically = False

c.content.canvas_reading = False
c.content.webgl = False
c.content.blocking.method = "both"
