--[[

   Awesome WM Battery Widget
   Distopico Vegan <distopico [at] riseup [dot] net>
   Licensed under GPL3

   Original from: https://github.com/mrzapp/awesomerc

--]]

local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local helpers = require("widgets/helpers")

local config = awful.util.getdir("config")
local widget = {}
local popup = nil
local batterytext = "--"
-- local iconpath = ""

-- {{{ Define adapter
local adapter = "BAT1"
local acAdapter = "ACAD"
local charge = "charge"


-- {{{ Define subwidgets
widget.text = wibox.widget{
    font = 'Sans bold 8',
    widget = wibox.widget.textbox
}

-- Change the draw method so icons can be drawn smaller
-- helpers:set_draw_method(widget._icon)
-- }}}

-- {{{ Check adapter method
function widget:check()
   local adapters = string.gmatch(helpers:run("ls /sys/class/power_supply/"), "%S+")
   for value in adapters do
      if value:match("^A") then
         acAdapter = value
      elseif value:match("^B") then
         adapter = value
      end
   end

   -- Test identifier
   charge = "charge"
   widget.hasbattery = helpers:test("cat /sys/class/power_supply/" .. adapter .. "/" .. charge .. "_now")

   -- Try another identifier
   if not widget.hasbattery then
      charge = "energy"
      widget.hasbattery = helpers:test("cat /sys/class/power_supply/" .. adapter .. "/" .. charge .. "_now")
   end
end
-- }}}

-- {{{ Update method
function widget:update()
   local sendNotify = false
   local cur = helpers:run("cat /sys/class/power_supply/" ..adapter .. "/" .. charge .. "_now")
   local cap = helpers:run("cat /sys/class/power_supply/" ..adapter .. "/" .. charge .. "_full")
   local sta = helpers:run("cat /sys/class/power_supply/" ..adapter .. "/status")
   local ac = helpers:run("cat /sys/class/power_supply/" ..acAdapter .. "/online")

   if cur and cap then
      local acStatus = ac ~= "" and math.floor(ac) or 0;
      local battery = math.floor(cur * 100 / cap)
      local colorfg = beautiful.fg_urgent
      if acStatus == 1 then
         batterytext = battery .. "% Battery | AC"
      else
         batterytext = battery .. "% Battery"
      end

      if(battery < 10) then
         colorfg = "#FF2B2B"
      end

      widget.text:set_markup(batterytext)

      if (sendNotify) then
         naughty.notify({
               text = batterytext,
               timeout = 4, hover_timeout = 0.5,
               screen = mouse.screen,
               fg = colorfg,
               ignore_suspend = true
         })
      end

   else
      widget.text:set_markup("N/A")

   end
end

function widget:show()
   popup = naughty.notify({
         -- icon = iconpath,
         icon_size = 16,
         text = batterytext,
         timeout = 0, hover_timeout = 0.5,
         screen = mouse.screen,
         ignore_suspend = true
   })
end

function widget:hide()
   if popup ~= nil then
      naughty.destroy(popup)
      popup = nil
   end
end
-- }}}

-- {{{ Listen if battery was found
widget:check()

if widget.hasbattery then
    -- update every 5 seconds
   helpers:listen(widget, 5)

   -- widget._icon:connect_signal("mouse::enter", function() widget:show() end)
   -- widget._icon:connect_signal("mouse::leave", function() widget:hide() end)
end
-- }}}

return widget
