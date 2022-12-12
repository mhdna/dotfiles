-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
-- package.loaded["naughty.dbus"] = {}
-- pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")
-- local batteryarc_widget = require("widgets.batteryarc.batteryarc")
local net_speed_widget = require("widgets.net-speed.net-speed")
-- local naughty_sidebar = require("widgets.sidebar")
-- local net_widgets = require("widgets.net_widgets")
-- net_wireless = net_widgets.wireless({interface="wlp2s0", indent = 4,  timeout = 5 })
-- local mpdarc= require("widgets.mpdarc.mpdarc")
-- for non empty function
local focused = awful.screen.focused()

-- source modules
-- local config_path = awful.util.getdir("config")
-- package.path = config_path .. "/modules/?.lua;" .. package.path
-- package.path = config_path .. "/modules/?/init.lua;" .. package.path

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   -- awful.spawn("notify-send -u critical 'an error happened!'")
   naughty.notify({ preset = naughty.config.presets.critical,
   	title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- Make sure we don't go into an endless error loop
                             if in_error then return end
                             in_error = true
                             -- awful.spawn("notify-send -u critical 'an error happened!'")

                             naughty.notify({ preset = naughty.config.presets.critical, title = "Oops, an error happened!",
                                              text = tostring(err) })
                             in_error = false
   end)
end

-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.init(gears.filesystem.get_configuration_dir() .. "theme.lua")
beautiful.init(theme_path)

-- This is used later as the default terminal and editor to run.
terminal = "xterm"
browser = "firefox" --xterm" .. " -e " .. " lynx -cfg=~/.config/lynx/lynx.cfg -lss ~/.config/lynx/lynx.lss "
home = os.getenv("HOME")
wiki = home .. "/stuff/wiki/"  os.getenv("WIKI")
editor =    "nvim" or os.getenv("EDITOR")
editor_cmd = terminal .. " -e " .. editor or "emacs"

naughty.config.defaults['icon_size'] = 100
naughty.config.defaults.timeout = 20
naughty.config.defaults.position = 'top_right'
-- naughty.config.defaults.font = 'Serif 10'
-- naughty.config.defaults.width = 200

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"


-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   -- { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   -- { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                             { "shutdown",  function() awful.util.spawn_with_shell"prompt 'Shutdown computer?' '$shutdown now'" end },
                             { "open terminal", terminal }
}
                       })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock('%a %b %d, %I:%M:%S %p ', 1) --('%a %b %d, %H:%M:%S ', refresh rate)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end)
   -- awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   -- awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            c:emit_signal(
               "request::activate",
               "tasklist",
               {raise = true}
            )
         end
   end),
   awful.button({ }, 3, function()
         awful.menu.client_list({ theme = { width = 250 } })
   end),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))
-- Wallpaper auto changer
-- require('wallpaper-changer').start({
-- path = home .. '/.config/awesome/wallpaper/',
-- show_notify = false,
-- timeout = 3000,
-- change_on_click = true
-- })

local function set_wallpaper(s)
   -- Wallpaper
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
      -- gears.wallpaper.maximized(wallpaper, s, false)
      -- gears.wallpaper.fit(wallpaper, s, black)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
      -- Wallpaper
      set_wallpaper(s)

      -- Each screen has its own tag table.
      -- Table of layouts to cover with awful.layout.inc, order matters.
      awful.layout.layouts = {
         awful.layout.suit.tile,
         awful.layout.suit.floating,
         awful.layout.suit.tile.bottom,
         awful.layout.suit.max,
         --awful.layout.suit.spiral,
         --awful.layout.suit.fair,
         --awful.layout.suit.fair.horizontal,
         -- awful.layout.suit.magnifier,
      }
      layouts = {
         awful.layout.suit.tile,
         awful.layout.suit.floating,
         awful.layout.suit.tile.bottom,
         awful.layout.suit.max,
         --awful.layout.suit.spiral,
         --awful.layout.suit.fair,
         --awful.layout.suit.fair.horizontal,
         -- awful.layout.suit.magnifier,
      }


      tags = {
         -- names  = {  "   1 ", " ﰍ  2 ", "    3 ", "   4 ", " ﬐  5 ", "   6 ", " 龎  7  ", " 索 8 "},
         -- names  = {"   1 ", "   2 ", "   3 ", "   4 ", "   5 ", "   6 ", "   7  ", "   8 "},--"1term", "2web", "3code", "4office", "5social", "6", "7edit", "8" },--
         names = {"Main", "Web", "Garage", "Chat", "Office"}, --, "7", "8", "9" },
         -- names = {"1", "2", "3", "4", "5"}, --, "7", "8", "9" },
         layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1]}, -- , layouts[3], layouts[1], layouts[1]  },
      }
      for s = 1, screen.count() do
         tags[s] = awful.tag(tags.names, s, tags.layout )
      end

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(gears.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = awful.widget.taglist {
         screen  = s,
         filter  = awful.widget.taglist.filter.all, -- was all -- noempty
         buttons = taglist_buttons
      }

      -- Create a tasklist widget
      s.mytasklist = awful.widget.tasklist {
         screen  = s,
         filter  = awful.widget.tasklist.filter.currenttags,
         buttons = tasklist_buttons
      }

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "top", screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            -- mylauncher,
            s.mytaglist,
            s.mypromptbox,
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            mykeyboardlayout,
            net_speed_widget(),
            -- batteryarc_widget({
            -- show_current_level = true,
            -- arc_thickness = 1,
            -- show_notification_mode = 'on_click'}),
            -- net_wireless,
            -- mpdarc,
            mytextclock,
            s.mylayoutbox,
         },
      }
end)
-- }}}

-- {{{ Mouse bindings
-- root.buttons(gears.table.join(
-- awful.button({ }, 3, function () mymainmenu:toggle() end)
-- awful.button({ }, 4, awful.tag.viewnextnoempty),
-- awful.button({ }, 5, awful.tag.viewprevnoempty)
-- ))
-- }}}
customization = {}
customization.func = {}

-- closures for client_status
-- client_status[client] = {sidelined = <boolean>, geometry= <client geometry>}
do
   local client_status = {}

   customization.func.client_sideline_left = function (c)
      local scr = screen[awful.screen.focused()]
      local workarea = scr.workarea
      if client_status[c] == nil then
         client_status[c] = {sidelined=false, geometry=nil}
      end
      if client_status[c].sidelined then
         if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
         end
      else
         client_status[c].geometry = c:geometry()
         workarea.width = math.floor(workarea.width/2)
         c:geometry(workarea)
      end
      client_status[c].sidelined = not client_status[c].sidelined
   end

   customization.func.client_sideline_right = function (c)
      local scr = screen[awful.screen.focused()]
      local workarea = scr.workarea
      if client_status[c] == nil then
         client_status[c] = {sidelined=false, geometry=nil}
      end
      if client_status[c].sidelined then
         if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
         end
      else
         client_status[c].geometry = c:geometry()
         workarea.x = workarea.x + math.floor(workarea.width/2)
         workarea.width = math.floor(workarea.width/2)
         c:geometry(workarea)
      end
      client_status[c].sidelined = not client_status[c].sidelined
   end

   customization.func.client_sideline_top = function (c)
      local scr = screen[awful.screen.focused()]
      local workarea = scr.workarea
      if client_status[c] == nil then
         client_status[c] = {sidelined=false, geometry=nil}
      end
      if client_status[c].sidelined then
         if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
         end
      else
         client_status[c].geometry = c:geometry()
         workarea.height = math.floor(workarea.height/2)
         c:geometry(workarea)
      end
      client_status[c].sidelined = not client_status[c].sidelined
   end

   customization.func.client_sideline_bottom = function (c)
      local scr = screen[awful.screen.focused()]
      local workarea = scr.workarea
      if client_status[c] == nil then
         client_status[c] = {sidelined=false, geometry=nil}
      end
      if client_status[c].sidelined then
         if client_status[c].geometry then
            c:geometry(client_status[c].geometry)
         end
      else
         client_status[c].geometry = c:geometry()
         workarea.y = workarea.y + math.floor(workarea.height/2)
         workarea.height = math.floor(workarea.height/2)
         c:geometry(workarea)
      end
      client_status[c].sidelined = not client_status[c].sidelined
   end

end

-- do
--     local instance = nil
--     customization.func.clients_on_tag = function ()
--         local clear_instance = function ()
--             if instance then
--                 instance:hide()
--                 instance = nil
--             end
--         end
--         if instance and instance.wibox.visible then
--             clear_instance()
--             return
--         end
--         local clients = {
--             items = {},
--             theme = { width = 400 },
--         }
--         local next = next
--         local t = awful.tag.selected()
--         if t then
--             for _, c in pairs(t:clients()) do
--                 if c.focusable and c.pid ~= 0 then
--                     table.insert(clients.items, {
--                         c.name .. " ~" .. tostring(c.pid) or "",
--                         function ()
--                             clear_instance()
--                             client.focus = c
--                             c:raise()
--                         end,
--                         c.icon
--                     })
--                 end
--             end
--             if next(clients.items) ~= nil then
--                 instance = awful.menu(clients)
--                 instance:toggle({keygrabber=true})
--             end
--         end
--     end
--
--     customization.func.all_clients = function ()
--         local clear_instance = function ()
--             if instance then
--                 instance:hide()
--                 instance = nil
--             end
--         end
--         if instance and instance.wibox.visible then
--             clear_instance()
--             return
--         end
--         local clients = {
--             items = {},
--             theme = { width = 400},
--         }
--         local next = next
--         for _, c in pairs(client.get()) do
--             if c.focusable and c.pid ~= 0 then
--                 table.insert(clients.items, {
--                     c.name .. " ~" .. tostring(c.pid) or "",
--                     function ()
--                         local t = c:tags()
--                         if t then
--                             awful.tag.viewonly(t[1])
--                         end
--                         clear_instance()
--                         client.focus = c
--                         c:raise()
--                     end,
--                     c.icon
--                 })
--             end
--         end
--         if next(clients.items) ~= nil then
--             instance = awful.menu(clients)
--             instance:toggle({keygrabber=true})
--         end
--     end
-- end
--

-- -- quit function
-- customization.orig = {}
-- customization.widgets = {}
-- customization.widgets.promptbox = {}

-- customization.orig.quit = function ()
--    local scr = awful.screen.focused()
--    awful.prompt.run({prompt = "What sort of action you're taking? (restart, quit, sleep, hibernate, reboot, poweroff)? "},
--       customization.widgets.promptbox[scr].widget,
--       function (t)
--          if string.lower(t) == 'yes' then
--             awesome.restart()
--          end
--       end,
--       function (t, p, n)
--          return awful.completion.generic(t, p, n, {'no', 'NO', 'yes', 'YES'})
--    end)
-- end

-- {{{ Key bindings
globalkeys = gears.table.join(
   -- awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
   --           {description="show help", group="awesome"}),
   -- awful.key({ modkey,           }, "p",   awful.tag.viewprev,
             -- {description = "view previous", group = "tag"}),
   -- awful.key({ modkey,           }, "n",  awful.tag.viewnext,
             -- {description = "view next", group = "tag"}),
   awful.key({ modkey,           }, "Tab", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

 --   awful.key({ modkey, }, "p", customization.func.clients_on_tag),
	--
	-- awful.key({ modkey, "Shift" }, "p", customization.func.all_clients),


   -- old move by index
   awful.key({ modkey,           }, "j",
      function ()
         awful.client.focus.byidx( 1)
      end,
      {description = "focus next by index", group = "client"}
   ),
   awful.key({ modkey,           }, "k",
      function ()
         awful.client.focus.byidx(-1)
      end,
      {description = "focus previous by index", group = "client"}
   ),
   -- awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
   --           {description = "show main menu", group = "awesome"}),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
      {description = "swap with next client by index", group = "client"}),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
      {description = "swap with previous client by index", group = "client"}),
   -- Moving bidirection
   -- awful.key({ modkey }, "h", function()
   --     awful.client.focus.bydirection("left")
   --     if client.focus then client.focus:raise() end
   -- end),
   -- awful.key({ modkey }, "j", function()
   --     awful.client.focus.bydirection("down")
   --     if client.focus then client.focus:raise() end
   -- end),
   -- awful.key({ modkey }, "k", function()
   --     awful.client.focus.bydirection("up")
   --     if client.focus then client.focus:raise() end
   -- end),
   -- awful.key({ modkey }, "l", function()
   --     awful.client.focus.bydirection("right")
   --     if client.focus then client.focus:raise() end
   -- end),
   --     -- Moving windows between positions works between desktops
   --     awful.key({ modkey, "Shift"   }, "h", function (c)
   --       awful.client.swap.global_bydirection("left")
   --       c:raise()
   --     end,
   --     {description = "swap with left client", group = "client"}),
   --     awful.key({ modkey, "Shift"   }, "l", function (c)
   --       awful.client.swap.global_bydirection("right")
   --       c:raise()
   --     end,
   --     {description = "swap with right client", group = "client"}),
   --     awful.key({ modkey, "Shift"   }, "j", function (c)
   --       awful.client.swap.global_bydirection("down")
   --       c:raise()
   --     end,
   --     {description = "swap with down client", group = "client"}),
   --     awful.key({ modkey, "Shift"   }, "k", function (c)
   --       awful.client.swap.global_bydirection("up")
   --       c:raise()
   --     end,
   --     {description = "swap with up client", group = "client"}),

   awful.key({ modkey, "Mod1" }, "left", function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Mod1"}, "Right", function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}),

   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),
   awful.key({"Mod1",           }, "Tab",
      function ()
         awful.client.focus.history.previous()
         if client.focus then
            client.focus:raise()
         end
      end,
      {description = "go back", group = "client"}),

   -- awful.key({ modkey,           }, "BackSpace", function () for i = 1, #focused.tags
   --  do awful.tag.viewidx(-1, focused)
   --      if #focused.clients > 0 then return end end end),
   -- awful.key({ modkey, }, "Escape", function ()
   -- for i = 1, #focused.tags do
   -- awful.tag.viewidx(1, focused)
   -- if #focused.clients > 0 then
   -- return end end end),

   awful.key({ modkey,           }, "BackSpace", function() naughty.destroy_all_notifications() end),
   -- awful.key({ modkey,           "Mod1"}, "BackSpace", function() get_next_notification_id() end),
   -- awful.key({ modkey,           }, "BackSpace", function() awful.util.spawn_with_shell ("dunstctl close-all") end),
   -- awful.key({ modkey,           "Shift"}, "BackSpace", function() awful.util.spawn_with_shell ("dunstctl history-pop") end),
   -- awful.key({ modkey,           "Control"}, "BackSpace", function()awful.util.spawn_with_shell ("dunstctl set-paused toggle && notify-send toggled") end),

   -- brightness
   awful.key({ }, "XF86MonBrightnessUp", function () awful.util.spawn_with_shell("light -A 10") end,
      {description = "Screen brightness +5%", group = "hotkeys"}),
   awful.key({"Shift" }, "XF86MonBrightnessUp", function () awful.util.spawn_with_shell("light -A 2") end,
      {description = "Screen brightness +5%", group = "hotkeys"}),
   awful.key({ }, "XF86MonBrightnessDown", function () awful.util.spawn_with_shell("light -U 10") end,
      {description = "Screen brightness -5%", group = "hotkeys"}),
   awful.key({"Shift" }, "XF86MonBrightnessDown", function () awful.util.spawn_with_shell("light -U 2") end,
      {description = "Screen brightness -5%", group = "hotkeys"}),
   awful.key({modkey }, "XF86MonBrightnessUp", function () awful.util.spawn_with_shell("light -S 100") end,
      {description = "Screen brightness 100%", group = "hotkeys"}),
   awful.key({modkey }, "XF86MonBrightnessDown", function () awful.util.spawn_with_shell("light -S 1") end,
      {description = "Screen brightness 1%", group = "hotkeys"}),
   -- awful.key({}, "XF86Display", function ()
   awful.key({modkey, "Mod1" }, "p", function () awful.util.spawn_with_shell("displayselect") end),
   awful.key({modkey }, ".", function () awful.util.spawn_with_shell("light -A 4") end),
   awful.key({modkey, "Shift" }, ".", function () awful.util.spawn_with_shell("light -S 100") end),
   awful.key({modkey }, ",", function () awful.util.spawn_with_shell("light -U 4") end),
   awful.key({modkey, "Shift" }, ",", function () awful.util.spawn_with_shell("light -S 1") end),

   -- Volume Control
   awful.key({ }, "XF86AudioRaiseVolume", function () awful.util.spawn_with_shell("pactl set-sink-volume @DEFAULT_SINK@ +5%") end), --amixer -D pulse sset Master 10%+") end,
   awful.key({modkey }, "XF86AudioRaiseVolume", function () awful.util.spawn_with_shell("pactl set-sink-volume @DEFAULT_SINK@ 100%") end), --amixer -D pulse sset Master 100%") end,
   awful.key({}, "XF86AudioLowerVolume", function () awful.util.spawn_with_shell("pactl set-sink-volume @DEFAULT_SINK@ -5%") end),-- amixer -D pulse sset Master 10%-") end,
   awful.key({ }, "XF86AudioMute", function () awful.util.spawn_with_shell("pactl set-sink-mute @DEFAULT_SINK@ toggle") end), --amixer -D pulse sset Master toggle") end,
   awful.key({modkey }, "=", function () awful.util.spawn_with_shell("pactl set-sink-volume @DEFAULT_SINK@ +5%") end), --amixer -D pulse sset Master 10%+") end,
   awful.key({modkey, "Shift"}, "=", function () awful.util.spawn_with_shell("pactl set-sink-volume @DEFAULT_SINK@ 100%") end), --amixer -D pulse sset Master 100%") end,
   awful.key({modkey }, "-", function () awful.util.spawn_with_shell("pactl set-sink-volume @DEFAULT_SINK@ -5%") end),-- amixer -D pulse sset Master 10%-") end,
   awful.key({modkey, "Shift"}, "-", function () awful.util.spawn_with_shell("pactl set-sink-mute @DEFAULT_SINK@ toggle") end), --amixer -D pulse sset Master toggle") end,
   --awful.key({modkey, "Shift"}, "m", function () awful.util.spawn_with_shell("pamixer -t") end), --amixer -D pulse sset Master toggle") end,

   awful.key({ }, "Print", function () awful.util.spawn_with_shell("maimFul") end),
   awful.key({modkey }, "Print", function () awful.util.spawn_with_shell("maimpick") end),
   awful.key({"Shift" }, "Print", function () awful.util.spawn_with_shell("maimWin") end),
   awful.key({ "Control"}, "Print", function () awful.util.spawn_with_shell("maimSel") end),
   awful.key({"Control", "Shift" }, "Print", function () awful.util.spawn_with_shell("maimXclip") end),
   awful.key({ modkey}, "Print", function () awful.util.spawn_with_shell("dmenurecord -i") end),
   awful.key({ modkey, "Shift"}, "Print", function () awful.util.spawn_with_shell("dmenurecord kill") end),


   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.util.spawn_with_shell(terminal) end),
   awful.key({ "Control", "Shift",}, "Escape", function () awful.util.spawn_with_shell(terminal .. " -e htop") end),
   -- awful.key({ modkey, "Shift"           }, "r", function () awful.spawn(terminal .. " -e htop") end),
   awful.key({ "Control", "Mod1",}, "Escape", function () awful.util.spawn_with_shell("xkill") end),
   awful.key({ modkey,           }, "e", function () awful.spawn(terminal .. " -e lfub") end),
   awful.key({ modkey,       }, "i", function () awful.util.spawn_with_shell("emacs") end),
   -- awful.key({ modkey,       }, "i", function () awful.util.spawn_with_shell(terminal .. " -e tmux a ") end),
   -- awful.key({ modkey, "Shift"   }, "i", function () awful.util.spawn_with_shell("anki --no-sandbox") end),
   awful.key({ modkey, "Shift"   }, "i", function () awful.util.spawn_with_shell("eclipse") end),
   awful.key({ modkey,           }, "b", function () awful.util.spawn_with_shell(browser) end),
   -- awful.key({ modkey, "Mod1"    }, "e", function () awful.spawn("thunderbird") end),
   awful.key({ modkey,       }, "v", function () awful.util.spawn_with_shell("clipmenu -i -l 15") end),
   -- awful.key({ modkey,      }, "s", function () awful.spawn("qutebrowser --target window :open 'https://www.coursera.org/learn/html-css-javascript-for-web-developers/home/welcome'") end),
   awful.key({ modkey,           }, "n", function () awful.util.spawn_with_shell(editor_cmd .. " " .. wiki) end),
 awful.key({ modkey, "Shift"   }, "n", function () awful.util.spawn_with_shell(editor_cmd .. " " .. wiki .. "/todo.txt") end),
 awful.key({ modkey, "Shift"   }, "d", function () awful.util.spawn_with_shell(editor_cmd .. " " .. wiki .. "/diary/" ) end),

   awful.key({ modkey,          "Shift" }, "a", function () awful.util.spawn_with_shell("xfce4-appfinder") end),
   -- awful.key({ modkey,           }, "c", function () awful.util.spawn_with_shell(terminal .. " -e calcurse -D " .. wiki .. "/calcurse") end),
   -- awful.key({ modkey,      "Shift"}, "t", function () awful.util.spawn_with_shell("telegram-desktop") end),
   awful.key({ modkey,           }, "c", function () awful.spawn(terminal .. " -e " .. " python " .. "-q") end),
   awful.key({ modkey,        "Mod1"   }, "m", function () awful.spawn(terminal .. " -e bicon  -e ncmpcpp") end),
   awful.key({ modkey, "Shift"   }, "e", function () awful.spawn(terminal .. " -e  bicon -e  neomutt") end),
   awful.key({ modkey, "Shift"   }, "r", function () awful.spawn(terminal .. " -e bicon -e newsboat") end),

   awful.key({ modkey, }, "F1", function () awful.util.spawn_with_shell("kdeconnect-indicator & kdeconnect-cli --refresh" ) end),
   awful.key({ modkey, "Shift" }, "F1", function () awful.util.spawn_with_shell("killall kdeconnect-indicator kdeconnectd" ) end),
   awful.key({ modkey,     }, "F2", function () awful.util.spawn_with_shell("redshift -O 5000") end),
   awful.key({ modkey, "Shift"   }, "F2", function () awful.util.spawn_with_shell("redshift -x") end),
   awful.key({ modkey, }, "F3", function () awful.util.spawn_with_shell("torwrap" ) end),
   awful.key({ modkey, "Shift"   }, "F3", function () awful.util.spawn_with_shell("td-toggle" ) end),
   awful.key({ modkey, }, "F4", function () awful.spawn(terminal .. " -e pulsemixer" ) end),
   awful.key({ modkey, }, "F5", function () awful.util.spawn_with_shell("mw -Y && notify-send '📫 All Mailboxes have been updated'" ) end),
   awful.key({ modkey,           }, "F10", function () awful.util.spawn_with_shell("mpc seek -10") end),
   awful.key({ modkey, "Shift"   }, "F10", function () awful.util.spawn_with_shell("mpc next") end),
   awful.key({ modkey,           }, "F11", function () awful.util.spawn_with_shell("mpc toggle") end),
   awful.key({ modkey, "Shift"   }, "F11", function () awful.util.spawn_with_shell("pauseallmpv") end),
   awful.key({ modkey,           }, "F12", function () awful.util.spawn_with_shell("mpc seek +10") end),
   awful.key({ modkey, "Shift"   }, "F12", function () awful.util.spawn_with_shell("mpc prev") end),

   -- awful.key({ modkey,           }, "/", function () awful.util.spawn_with_shell("dp2") end),
    -- awful.key({ modkey, "Shift"   }, "/", function () awful.util.spawn_with_shell("dp1") end),


    awful.key({ modkey, "Control"}, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey,   "Mod1"  }, "c", function () awful.spawn(editor_cmd .. " " .. awesome.conffile) end),


    awful.key({ modkey, "Shift"   }, "q", function() awful.util.spawn_with_shell("sysacta")end),

    awful.key({ modkey, "Shift"   }, "Delete", function() awful.util.spawn_with_shell("slock")end),
    awful.key({ modkey, }, "Delete", function() awful.util.spawn_with_shell("prompt \"sleep?\" && loginctl suspend")end),
    -- awful.key({ "Control", }, "Delete", function() awful.util.spawn_with_shell("sysacta")end),

    awful.key({ modkey,    }, "grave", function() awful.util.spawn_with_shell("dmenuunicode")end),
    -- awful.key({ modkey,    }, "p", function() awful.util.spawn_with_shell("passmenu -i ")end),
    awful.key({ modkey,     }, "/", function() awful.util.spawn_with_shell("transXclip")end),
     awful.key({ modkey, "Mod1"   }, "/", function() awful.spawn(terminal .. " -e transs")end),
    awful.key({ modkey, "Shift"  }, "/", function() awful.util.spawn_with_shell("transXcclip")end),

  -- old resizing
    awful.key({ modkey,         }, "l",     function () awful.tag.incmwfact( 0.1)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,          }, "h",     function () awful.tag.incmwfact(-0.1)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),

    -- awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol( 1, nil, true)    end,
    --           {description = "increase the number of columns", group = "layout"}),
    -- awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol(-1, nil, true)    end,
    --           {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "\\", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "\\", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    -- Prompt
    -- awful.key({ modkey}, "p", function () awful.util.spawn_with_shell("dmenu_run") end),

    awful.key({ modkey, "Mod1"}, "d", function () awful.util.spawn_with_shell("dmenuhandlerXclip") end),
    -- awful.key({ modkey , },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              -- {description = "run prompt", group = "launcher"}),
    -- Menubar
    awful.key({ modkey, }, "r", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"}),

    awful.key({ modkey, "Shift" }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"})

)

clientkeys = gears.table.join(

awful.key({ modkey,           }, "Left", customization.func.client_sideline_left),
awful.key({ modkey,           }, "Right", customization.func.client_sideline_right),
awful.key({ modkey,           }, "Up", customization.func.client_sideline_top),
awful.key({ modkey,           }, "Down", customization.func.client_sideline_bottom),

    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "Return",  awful.client.floating.toggle,
              {description = "toggle floating", group = "client"}),

    awful.key({ modkey,"Shift","Control" }, "k", function (c)
      -- if c.floating then
        c:relative_move( 0, 0, 0, -20)
      -- else
        -- awful.client.incwfact(0.035)
      -- end
    end,
    {description = "Floating Resize Vertical -", group = "client"}),
    awful.key({ modkey, "Shift" , "Control"}, "j", function (c)
      -- if c.floating then
        c:relative_move( 0, 0, 0,  20)
      -- else
        -- awful.client.incwfact(-0.035)
      -- end
    end,
    {description = "Floating Resize Vertical +", group = "client"}),
    awful.key({ modkey, "Shift", "Control" }, "h", function (c)
      -- if c.floating then
        c:relative_move( 0, 0, -20, 0)
      -- else
        -- awful.tag.incmwfact(-0.035)
      -- end
    end,
    {description = "Floating Resize Horizontal -", group = "client"}),
    awful.key({ modkey, "Shift" , "Control"}, "l", function (c)
      -- if c.floating then
        c:relative_move( 0, 0,  20, 0)
      -- else
        -- awful.tag.incmwfact(0.035)
      -- end
    end,
    {description = "Floating Resize Horizontal +", group = "client"}),
    -- Resize floating windows
    awful.key({ modkey, "Control","Shift" }, "k", function (c)
      -- if c.floating then
        c:relative_move( 0, 0, 0, -20)
      -- else
        -- awful.client.incwfact(0.035)
      -- end
    end,
    {description = "Floating Resize Vertical -", group = "client"}),
    awful.key({ modkey, "Control","Shift" }, "j", function (c)
      -- if c.floating then
        c:relative_move( 0, 0, 0,  20)
      -- else
        -- awful.client.incwfact(-0.035)
      -- end
    end,
    {description = "Floating Resize Vertical +", group = "client"}),
    awful.key({ modkey, "Control","Shift" }, "h", function (c)
      -- if c.floating then
        c:relative_move( 0, 0, -20, 0)
      -- else
        -- awful.tag.incmwfact(-0.035)
      -- end
    end,
    {description = "Floating Resize Horizontal -", group = "client"}),
    awful.key({ modkey, "Control","Shift" }, "l", function (c)
      -- if c.floating then
        c:relative_move( 0, 0,  20, 0)
      -- else
        -- awful.tag.incmwfact(0.035)
      -- end
    end,
    {description = "Floating Resize Horizontal +", group = "client"}),


    -- Moving floating windows
    awful.key({ modkey, "Control"}, "k", function (c)
      c:relative_move(  0, -45,   0,   0) end,
    {description = "Floating Move Up", group = "client"}),
    awful.key({ modkey, "Control"}, "j", function (c)
      c:relative_move(  0,  45,   0,   0) end,
    {description = "Floating Move Down", group = "client"}),
    awful.key({ modkey, "Control"}, "h", function (c)
      c:relative_move(-45,   0,   0,   0) end,
    {description = "Floating Move Left", group = "client"}),
    awful.key({ modkey,"Control"}, "l", function (c)
      c:relative_move( 45,   0,   0,   0) end,
    {description = "Floating Move Right", group = "client"}),

    -- awful.key({ modkey,        }, "u",  function() awful.client.getmaster() end,
    --           {description = "toggle floating", group = "client"}),
    -- awful.key({ modkey, "Shift" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              -- {description = "move to master", group = "client"}),
    -- My be master else slave
    awful.key({ modkey, "Shift"  }, "Return", function (c)
        client.focus = c if c == awful.client.getmaster() then awful.client.setslave(c) else c:swap(awful.client.getmaster()) end end),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
       {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
       {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "s",      function (c) c.sticky = not c.sticky  end),
    awful.key({ modkey, "Shift"   }, "m",
       function (c)
          -- The client currently has the input focus, so it cannot be
          -- minimized, since minimized clients can't have the focus.
          c.minimized = true
       end ,
       {description = "minimize", group = "client"}),


    awful.key({ modkey, "Control" }, "m",
       function ()
          local c = awful.client.restore()
          -- Focus restored client
          if c then
             c:emit_signal(
                "request::activate", "key.unminimize", {raise = true}
             )
           end
       end,
       {description = "restore minimized", group = "client"}),


    awful.key({ modkey,  }, "m",
       function (c)
          c.maximized = not c.maximized
          c:raise()
       end ,
       {description = "(un)maximize", group = "client"})
    -- awful.key({ modkey, "Control" }, "w",
    --     function (c)
    --         c.maximized_vertical = not c.maximized_vertical
    --         c:raise()
    --     end ,
    --     {description = "(un)maximize vertically", group = "client"}),
    -- awful.key({ modkey, "Shift"   }, "w",
    --     function (c)
    --         c.maximized_horizontal = not c.maximized_horizontal
    --         c:raise()
    --     end ,
    --     {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(globalkeys,
                                 -- View tag only.
                                 awful.key({ modkey }, "#" .. i + 9,
                                    function ()
                                       local screen = awful.screen.focused()
                                       local tag = screen.tags[i]
                                       if tag then
                                          tag:view_only()
                                       end
                                    end,
                                    {description = "view tag #"..i, group = "tag"}),
                                 -- Toggle tag display.
                                 awful.key({ modkey, "Control" }, "#" .. i + 9,
                                    function ()
                                       local screen = awful.screen.focused()
                                       local tag = screen.tags[i]
                                       if tag then
                                          awful.tag.viewtoggle(tag)
                                       end
                                    end,
                                    {description = "toggle tag #" .. i, group = "tag"}),
                                 -- Move client to tag.
                                 awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                    function ()
                                       if client.focus then
                                          local tag = client.focus.screen.tags[i]
                                          if tag then
                                             client.focus:move_to_tag(tag)
                                          end
                                       end
                                    end,
                                    {description = "move focused client to tag #"..i, group = "tag"}),
                                 -- Toggle tag on focused client.
                                 awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                    function ()
                                       if client.focus then
                                          local tag = client.focus.screen.tags[i]
                                          if tag then
                                             client.focus:toggle_tag(tag)
                                          end
                                       end
                                    end,
                                    {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

clientbuttons = gears.table.join(
   -- awful.button({}, 5, function () awful.util.spawn_with_shell("greenhighlight") end),
   -- awful.button({"Shift"}, 1, function () awful.util.spawn_with_shell("deletehighlight") end),
   -- awful.button({}, 4, function () awful.util.spawn_with_shell("orangehighlight") end),
   -- awful.button({"Shift"}, 5, function () awful.util.spawn_with_shell("redhighlight") end),

   awful.button({ }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 3, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.resize(c)
   end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        raise = true,
        -- floating= true,
        keys = clientkeys,
        buttons = clientbuttons,
     screen = awful.screen.preferred,
     placement = awful.placement.no_overlap+awful.placement.no_offscreen+awful.placement.centered,
         -- size_hints_honor = false
     }
    },

    { rule = { },
       properties = { size_hints_honor = false },
   except = {class= "mpv"}},

     --Floating clients.
     { rule_any = {
         instance = {
           "DTA",  -- Firefox addon DownThemAll.
           "pinentry",
         },
         class = {
           "Arandr", "Blueman-manager", "Gpick", "Kruler",
           "MessageWin",  -- kalarm.
           "Sxiv",
           "Wpa_gui", "veromix", "xtightvncviewer"},

         -- Note that the name property shown in xprop might be set slightly after creation of the client
         -- and the name shown there might not match defined rules here.
         name = {
           "Event Tester",  -- xev.
     "Media viewer", -- telegram media viewer
         },
         role = {
           "AlarmWindow",  -- Thunderbird's calendar.
           "ConfigManager",  -- Thunderbird's about:config.
           "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
         }
       }, properties = { floating = true , ontop = true}},

    -- -- Add titlebars to normal clients and dialogs
     -- { rule_any = {type = { "normal", "dialog" }
       -- }, properties = { titlebars_enabled = true }},
     -- { rule_any = {class = {"XTerm",  "Emacs", "St","Zathura", "flow"}},
     --    properties = { titlebars_enabled = true }},

     -- { rule_any = { class = {"Surf"} },
     --   properties = { placement = awful.placement.centered, floating = true}},

     { rule_any = { class = {"Tor Browser", "firefox"} },
        properties = { placement = awful.placement.centered, tag = tags[1][2]}},

     { rule = { name = "mpvfloat" },
       properties = { floating = true, sticky = true, placement = awful.placement.bottom_right, ontop = true}},

    { rule_any = { class = {"code-oss", "VSCodium", "Atom", "jetbrains-idea-ce", "java-util-concurrent-ForkJoinWorkerThread", "Eclipse", "Java" , "Subl" }},
       properties = { tag = tags[1][3] }}, --switchtotag = true}},

    --[[ { rule_any = { class = {"Brave-browser", "librewolf", "Chromium", "firefox", "qutebrowser", "Tor Browser"}}, ]]
    --[[    properties = { tag = tags[1][2] }}, --switchtotag = true}}, ]]
     -- { rule = { class = "Anki" },
     --       properties = { fullscreen = true}} ,
    -- { rule = { class = "Emacs" },
    --       properties = {tag= tags[1][3]}} ,
    { rule = { class = "Dragon-drop" },
       properties = {sticky = true, ontop = true, placement = awful.placement.top_left}},

    { rule_any = { class = {"pm", "tm", "libreoffice", "Libreoffice", "LibreOffice"}, name = {"LibreOffice"}},
       properties = { tag = tags[1][5] }},

    { rule_any = { class = {"Gimp", "Inkscape", "kdenlive"}},
       properties = { tag = tags[1][5] }},

    { rule_any = { class = {"TelegramDesktop", "telegram-desktop", "zoom", "discord", "SchildiChat" }},
       properties = { tag = tags[1][4] }},


    -- { rule_any = { class = { "Xournal", "FocusWriter" }},
    --       properties = { fullscreen = true } },
--    { rule_any = { class = {"xpad", "Gnome-pomodoro" }},
--          properties = { floating = true, ontop = true,  placement= awful.placement.centered} },

    { rule_any = { name = {"Picture-in-Picture", "Picture in picture"}},
          properties = { floating = true, placement = awful.placement.bottom_right , ontop = true, sticky = false}},

--    { rule = { class = "Mplayer"},
--          properties = { floating = true, placement = awful.placement.bottom_right , ontop = true, sticky = true}},

    -- { rule = { name = "flow" },
    --    properties = { sticky= true, floating=true}},
--    { rule_any = { class = "kdeconnect.daemon" },
--       properties = { ontop= true}},

    { rule= { name = "Authentication" },
       properties = { ontop= true}},

    -- Floating clients.
--    { rule_any = {type = { "floating"} }, properties = { ontop = true}}, --placement = awful.placement.centered } },
    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- Comment these out to spawn new windows as slaves
 -- {{{ Signals
 -- Signal function to execute when a new client appears.
 client.connect_signal("manage", function (c)
     -- Set the windows at the slave,
     -- i.e. put it at the end of others instead of setting it master.
     if not awesome.startup then awful.client.setslave(c) end

     -- if awesome.startup
     --   and not c.size_hints.user_position
     --   and not c.size_hints.program_position then
     --     -- Prevent clients from being unreachable after screen count changes.
     --     awful.placement.no_offscreen(c)
     -- end
 end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     c:emit_signal("request::activate", "mouse_enter", {raise = false})
-- end)

-- borders

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

--gaps

 -- beautiful.useless_gap = 3
 -- beautiful.gap_single_client = true

--disable edges snapping
awful.mouse.snap.edge_enabled =false

-- }}}
-- client.connect_signal("property::floating", function(c)
--     if c.floating then
--         awful.titlebar.show(c)
--     else
--         awful.titlebar.hide(c)
--    end
-- end)
--
--show titlebars for floating windows
-- client.connect_signal("property::floating", function(c)
--     if c.floating and not c.fullscreen and not c.maximized then
--         awful.titlebar.show(c)
--     else
--         awful.titlebar.hide(c)
--    end
-- end)

-- preserve current tag upon restart
awesome.connect_signal('exit', function(reason_restart) if not reason_restart then return end

local file = io.open('/tmp/awesomewm-last-selected-tags', 'w+')

for s in screen do file:write(s.selected_tag.index, '\n') end

file:close() end)

awesome.connect_signal('startup', function() local file = io.open('/tmp/awesomewm-last-selected-tags', 'r') if not file then return end

local selected_tags = {}

for line in file:lines() do table.insert(selected_tags, tonumber(line)) end

for s in screen do local i = selected_tags[s.index] local t = s.tags[i] t:view_only() end

file:close() end)

awful.spawn.with_shell( "~/.config/awesome/autostart.sh")
