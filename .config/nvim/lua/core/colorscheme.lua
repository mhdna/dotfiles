vim.opt.bg = 'dark'
vim.cmd [[
hi TabLine guibg=none
hi CursorLine guibg=#222222 term=underline cterm=underline gui=underline
hi Visual  guifg=black gui=none
hi Pmenu guifg=black guibg=grey
hi PmenuSel gui=underline  guifg=red
hi Normal ctermbg=NONE guibg=NONE
hi NormalFloat  guibg=none
" hi SignColumn guibg=none
"hi CursorColumn guibg=#444444
" highlight FIXME ctermfg=red guibg=red'
" highlight BUG ctermfg=red guibg=red'
" colorscheme gruvbox
]]
-- require("gruvbox").setup({ contrast = "hard" }) -- can be "hard", "soft" or empty string
