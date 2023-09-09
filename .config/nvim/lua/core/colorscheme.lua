vim.opt.bg = 'dark'
vim.cmd [[
hi TabLine guibg=none
hi Visual  guifg=black gui=none
hi Pmenu guifg=white guibg=#444444
hi PmenuSel gui=underline  guibg=red guifg=black
hi Normal ctermbg=NONE guibg=NONE
hi NormalFloat  guibg=#444444 guifg=white
" hi SignColumn guibg=none
hi ColorColumn guibg=#333333
hi CursorLine guibg=#333333 "term=underline cterm=underline gui=underline
highlight FIXME ctermfg=red guibg=red'
highlight BUG ctermfg=red guibg=red'
hi Comment gui=italic
" highlight TelescopeMatching gui=bold guifg=#f78d00
" highlight TelescopeNormal guifg=green "term=underline cterm=underline gui=underline
" highlight TelescopeMatchingChar guibg=green
]]
