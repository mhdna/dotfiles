-- vim.cmd [[
-- try
  -- colorscheme gruvbox
-- catch /^Vim\%((\a\+)\)\=:E185/
--   colorscheme default
-- endtry
-- ]]
-- vim.cmd('autocmd ColorScheme * highlight Normal ctermbg=none ') --to Show whitespace, MUST be inserted BEFORE the colorscheme command
-- vim.cmd('autocmd ColorScheme * highlight Normal guibg=White guifg=Black') --to Show whitespace, MUST be inserted BEFORE the colorscheme command
-- vim.cmd('colorscheme parchment')

-- [[ require("zenburn").setup() ]]
-- vim.cmd([[
-- augroup TransparentColours
-- autocmd!
-- autocmd ColorScheme * highlight normal ctermbg=NONE guiBG=NONE
-- augroup end
-- ]])
-- vim.cmd [[  colorscheme parchment ]]
-- vim.cmd [[ let g:gruvbox_contrast_dark="med"]]
-- require('solarized').set()
-- vim.g.gruvbox_contrast_dark = 'soft'
-- require('monokai').setup {}

--[[ require("github-theme").setup({ ]]
--[[     theme_style = "light", ]]
--[[ }) ]]
-- vim.g.gruvbox_invert_selection = '0'
-- vim.opt.background = "light"
-- vim.opt.background = "dark"
vim.cmd(('hi Visual  guifg=#000000 guibg=#ffff00 gui=none'))
vim.cmd(('hi StatusLine guifg=#ffffff guibg=#000000'))
vim.cmd(('hi Pmenu guifg=#000000 guibg=#fff8dc'))
vim.cmd(('hi PmenuSbar guifg=#8b0000 guibg=#8b0000 '))
vim.cmd(('hi PmenuSel guifg=black guibg=#add8e6'))
vim.cmd(('hi CursorLine guifg=black guibg=#b4eeb4 '))
