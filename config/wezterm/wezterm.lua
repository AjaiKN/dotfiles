-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

local home = wezterm.home_dir --os.getenv("HOME")
local is_mac = string.find(wezterm.target_triple, 'darwin')

function file_exists(name)
  -- https://stackoverflow.com/a/4991602
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

--disabled because I have no way of knowing if TERM=wezterm is installed on an ssh'ed machine
--TODO: try doing something like this: https://ghostty.org/docs/help/terminfo#configure-ssh-to-fall-back-to-a-known-terminfo-entry
-- -- https://wezterm.org/config/lua/config/term.html
-- if file_exists(home .. "/.terminfo/w/wezterm") then
--   config.term = "wezterm"
-- end

local my_opacity = 1.0 - 0.18640382661647403

wezterm.on('toggle-opacity', function(window, pane)
  local overrides = window:get_config_overrides() or {}
  if (overrides.window_background_opacity and (overrides.window_background_opacity ~= 1.0)) or ((not overrides.window_background_opacity) and (config.window_background_opacity ~= 1.0)) then
    overrides.window_background_opacity = 1.0
  else
    overrides.window_background_opacity = my_opacity
  end
  window:set_config_overrides(overrides)
end)

for _,dir in ipairs({ home.."/.local/bin", home.."/bin", home.."/.config/guix/current/bin", home.."/.nix-profile/bin", "/nix/profile/bin", home.."/.local/state/nix/profile/bin", "/nix/var/nix/profiles/default/bin", "/usr/local/bin", "/usr/bin", "/bin", not is_mac and "/home/linuxbrew/.linuxbrew/bin" }) do
  if dir and file_exists(dir.."/zsh") then
    wezterm.log_info("shell: " .. dir.."/zsh")
    config.default_prog = { dir.."/zsh" }
    break
  end
end

-- This is where you actually apply your config choices

-- config.color_scheme = 'AdventureTime'
config.enable_kitty_keyboard = true
config.window_background_opacity = my_opacity
config.macos_window_background_blur = 3
config.quit_when_all_windows_are_closed = false
if file_exists(home .. "/prog/") then
  config.default_cwd = home .. "/prog"
end
config.prefer_to_spawn_tabs = true
config.scrollback_lines = 200000

local function bind(mods, key, action)
  return { mods = mods, key = key, action = action }
end
local function add_bind(mods, key, action)
  table.insert(config.keys, { mods = mods, key = key, action = action })
end

local act = wezterm.action

-- How to see the Kitty Keyboard Protocol (KKP) translation for a key sequence:
-- 1. open the Kitty terminal
-- 2. Run `kitty kitten show-key -m kitty`
-- 3. Type the key sequence
local csi = '\x1b\x5b'
local function kkp_super(key)
  local translated = csi .. tostring(string.byte(key)) .. ';9u'
  return act.SendString(translated)
end
local kkp_super_left      = act.SendString(csi .. '1;9D')
local kkp_super_right     = act.SendString(csi .. '1;9C')
local kkp_super_right     = act.SendString(csi .. '1;9C')
local kkp_super_shift_up  = act.SendString(csi .. '1;10A')
local kkp_super_shift_down= act.SendString(csi .. '1;2B')
local kkp_super_backspace = act.SendString(csi .. '127;9u')
local kkp_super_enter     = kkp_super('\r')

local function if_kkp(yes, no)
  return wezterm.action_callback(function (win, pane)
    local process = pane:get_foreground_process_name()
    if process:find("emacs") or process:find("vim") then
      win:perform_action(yes, pane)
    else
      win:perform_action(no, pane)
    end
  end)
end

-- can't find a built-in action to do this?
local close_window = act.Multiple{
  act.ActivateTab(-1),
  wezterm.action_callback(
    function (win, pane)
      local window = win:mux_window()
      local tabs = window:tabs()
      for _, tab in ipairs(tabs) do
        tab:activate()
        win:perform_action(act.CloseCurrentTab { confirm = false }, pane)
      end
    end
  ),
}

local clear_pattern = act.Multiple{act.CopyMode 'ClearPattern',
                                   act.CopyMode 'ClearPattern',
                                   act.CopyMode 'ClearPattern',
                                   act.CopyMode 'ClearPattern',
                                   act.CopyMode 'ClearPattern'}

config.window_close_confirmation = 'NeverPrompt'

config.keys = {
  bind('SUPER',       'w', act.CloseCurrentPane{ confirm = false }),
  bind('SHIFT|CTRL',  'W', close_window),
  bind('SHIFT|SUPER', 'W', close_window),
  bind('SUPER',       'd', act.SplitHorizontal),
  bind('SHIFT|SUPER', 'D', act.SplitVertical),
  bind('SHIFT|SUPER', 'L', act.ShowDebugOverlay),
  bind('SUPER', 'p', act.ActivateCommandPalette),
  bind('CTRL|SUPER', 'f', act.ToggleFullScreen),
  bind('SUPER', 'Enter', if_kkp(kkp_super_enter, act.ToggleFullScreen)),
  bind('SUPER', 'LeftArrow', if_kkp(kkp_super_left, act.SendKey{ mods = 'CTRL', key = 'a' })),
  bind('SUPER', 'RightArrow', if_kkp(kkp_super_right, act.SendKey{ mods = 'CTRL', key = 'e' })),
  bind('SUPER', 'Backspace', if_kkp(kkp_super_backspace, act.SendKey{ mods = 'CTRL', key = 'u' })),
  bind('ALT', 'LeftArrow', if_kkp(act.SendKey{ mods = 'ALT', key = 'LeftArrow' }, act.SendKey{ mods = 'ALT', key = 'b' })),
  bind('ALT', 'RightArrow', if_kkp(act.SendKey{ mods = 'ALT', key = 'RightArrow' }, act.SendKey{ mods = 'ALT', key = 'f' })),
  bind('SUPER', 'k', act.DisableDefaultAssignment),
  bind('SUPER', 'h', act.DisableDefaultAssignment),
  -- bind('CTRL', 'f', act.Search{ CaseInSensitiveString = '' }),
  -- bind('SHIFT|CTRL', 'F', act.Search{ CaseInSensitiveString = '' }),
  bind('SUPER', 'f',
       if_kkp(kkp_super('f'),
              act.Multiple{clear_pattern, act.Search{ CaseInSensitiveString = '' }})),
  bind('CTRL', 's', if_kkp(act.SendKey{ mods = 'CTRL', key = 's' }, act.Search{ CaseInSensitiveString = '' })),
  bind('SHIFT|SUPER', 'O', wezterm.action.EmitEvent 'toggle-opacity'),
  bind('SHIFT|SUPER', 'UpArrow', if_kkp(kkp_super_shift_up, act.ScrollToPrompt(-1))),
  bind('SHIFT|SUPER', 'DownArrow', if_kkp(kkp_super_shift_up, act.ScrollToPrompt(1))),
}

if not is_mac then
  -- add_bind('CTRL', 'v', if_kkp(act.SendKey{mods='CTRL', key='v'}, act.PasteFrom 'Clipboard'))
  add_bind('CTRL', 'w', if_kkp(act.SendKey{mods='CTRL', key='w'}, act.CloseCurrentTab{confirm=true}))
  add_bind('CTRL', 't', if_kkp(act.SendKey{mods='CTRL', key='t'}, act.SpawnTab 'CurrentPaneDomain'))
end

-- https://github.com/wez/wezterm/issues/1988
-- https://github.com/wez/wezterm/issues/5952
-- https://github.com/wez/wezterm/issues/5952#issuecomment-2323932832
local function complete_search()
  return wezterm.action_callback(function(window, pane, _)
      window:perform_action(act.CopyMode('ClearPattern'), pane)
      window:perform_action(act.CopyMode('AcceptPattern'), pane)

      window:perform_action(act.CopyMode('ClearSelectionMode'), pane)
      -- -- For some reason this just does not work unless we retry a few times.
      -- -- Probably something to do with state management between Search/Copy mode.
      -- for _ = 1, 3, 1 do
      --   wezterm.sleep_ms(100)
      --   window:perform_action(act.CopyMode('ClearSelectionMode'), pane)
      -- end

      window:perform_action(act.CopyMode('Close'), pane)
  end)
end

config.key_tables = {
  search_mode = {
    bind('NONE', 'Enter', act.CopyMode 'PriorMatch'),
    bind('SHIFT', 'Enter', act.CopyMode 'NextMatch'),
    -- bind('NONE', 'Escape', act.CopyMode 'Close'),
    bind('NONE', 'Escape', complete_search()),
    bind('CTRL', 'p', act.CopyMode 'PriorMatch'),
    bind('CTRL', 'n', act.CopyMode 'NextMatch'),
    bind('CTRL', 'k', act.CopyMode 'PriorMatch'),
    bind('CTRL', 'j', act.CopyMode 'NextMatch'),
    bind('CTRL', 'r', act.CopyMode 'PriorMatch'),
    bind('CTRL', 's', act.CopyMode 'NextMatch'),
    bind('SUPER', 'f', act.CopyMode 'CycleMatchType'),
    bind('CTRL', 'e', act.CopyMode 'EditPattern'),
    bind('CTRL', 'u', act.CopyMode 'ClearPattern'),
    bind('SUPER', 'Backspace', act.CopyMode 'ClearPattern'),
    bind('NONE', 'PageUp', act.CopyMode 'PriorMatchPage'),
    bind('NONE', 'PageDown', act.CopyMode 'NextMatchPage'),
    bind('NONE', 'UpArrow', act.CopyMode 'PriorMatch'),
    bind('NONE', 'DownArrow', act.CopyMode 'NextMatch'),

    bind('SHIFT', 'LeftArrow', act.CopyMode 'MoveLeft'),
    bind('SHIFT', 'RightArrow', act.CopyMode 'MoveRight'),
    bind('SHIFT|ALT', 'RightArrow', act.CopyMode 'MoveForwardWordEnd'),
    bind('SHIFT|ALT', 'LeftArrow', act.CopyMode 'MoveBackwardWord'),
    bind('SHIFT|ALT', 'f', act.CopyMode 'MoveForwardWordEnd'),
    bind('SHIFT|ALT', 'b', act.CopyMode 'MoveBackwardWord'),
    bind('SHIFT', 'DownArrow', act.CopyMode 'MoveDown'),
    bind('SHIFT', 'UpArrow', act.CopyMode 'MoveUp'),
    bind('SHIFT|SUPER', 'DownArrow', act.CopyMode 'MoveForwardSemanticZone'),
    bind('SHIFT|SUPER', 'UpArrow', act.CopyMode 'MoveBackwardSemanticZone'),
    bind('CTRL', 'x', act.CopyMode 'MoveToSelectionOtherEnd'),
  },
}

config.mouse_bindings = {
  -- for consistency with emacs, allow CTRL+ALT instead of just ALT for rectangular block
  { mods = 'CTRL|ALT', event = { Down = { streak = 1, button = 'Left' } }, action = act.SelectTextAtMouseCursor 'Block' },
  { mods = 'CTRL|ALT', event = { Up   = { streak = 1, button = 'Left' } }, action = act.CompleteSelection 'ClipboardAndPrimarySelection' },
  { mods = 'CTRL|ALT', event = { Drag = { streak = 1, button = 'Left' } }, action = act.ExtendSelectionToMouseCursor 'Block' },

  {
    mods = 'NONE',
    event = { Down = { streak = 4, button = 'Left' } },
    action = act.SelectTextAtMouseCursor 'SemanticZone',
  },
}

-- config.font = wezterm.font('Iosevka Term') -- crazy slow for some reason
if wezterm.hostname() == 'Ajais-M5-MacBook-Pro.local' then
  config.font = wezterm.font('M PLUS Code Latin')
else
  config.font = wezterm.font('JetBrains Mono')
end

-- disable ligatures
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

-- and finally, return the configuration to wezterm
return config
