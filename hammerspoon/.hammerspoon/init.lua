local hyper = {"ctrl", "alt", "cmd"}

hs.loadSpoon("MiroWindowsManager")

hs.window.animationDuration = 0
spoon.MiroWindowsManager:bindHotkeys({
    up = {hyper, "up"},
    right = {hyper, "right"},
    down = {hyper, "down"},
    left = {hyper, "left"},
    fullscreen = {hyper, "f"}
})

-- current space, includes minimized and hidden windows
switcher_space = hs.window.switcher.new(hs.window.filter.new():setCurrentSpace(true):setDefaultFilter{})
hs.hotkey.bind('alt', 'tab', 'Next window', function()switcher_space:next()end)
hs.hotkey.bind('alt-shift', 'tab', 'Prev window', function()switcher_space:previous()end)
