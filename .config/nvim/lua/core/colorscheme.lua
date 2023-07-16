vim.opt.bg = 'dark'
-- " hi TabLine guibg=none
-- " hi CursorLine guibg=#222222 term=underline cterm=underline gui=underline
-- " hi Visual  guifg=black gui=none
-- " hi Pmenu guifg=black guibg=grey
-- " hi PmenuSel gui=underline  guifg=red
-- " "hi CursorColumn guibg=#444444
-- " " highlight FIXME ctermfg=red guibg=red'
-- " " highlight BUG ctermfg=red guibg=red'
-- hi Normal ctermbg=NONE guibg=NONE
-- hi NormalFloat  guibg=blue guifg=white
-- hi SignColumn guibg=none
require("gruvbox").setup({
    contrast = "hard", -- can be "hard", "soft" or empty string
})
vim.cmd [[
colorscheme gruvbox
]]
