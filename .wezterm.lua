local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- カラースキームの設定
-- config.color_scheme = 'AdventureTime'
config.color_scheme = 'MaterialDesignColors'

-- フォントの設定
config.font = wezterm.font("HackGen Console NF", {weight="Medium", stretch="Normal", style="Normal"})

-- フォントサイズの設定
config.font_size = 11

config.hide_tab_bar_if_only_one_tab = true

-- ショートカットキー設定
local act = wezterm.action
config.keys = {
  {
    key = 'Space',
    mods = 'SHIFT',
    action = wezterm.action.ScrollByPage(-1),
  },
}

return config
