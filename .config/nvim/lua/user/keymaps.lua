require('Comment').setup()

local function map(mode, lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.cmd([[
func! ToggleArabic()
if &rl
set norl
set keymap=
else
set rl
set keymap=arabic "Modified keymap. File in .vim/keymap
set encoding=utf-8
set arabicshape
" set nospell
end
endfunc

func! TogglePersian()
if &rl
set norl
set keymap=
else
set rl
set keymap=persian "Modified keymap. File in .vim/keymap
set nospell

end
endfunc

]])
--[[ -- Arabic switch
function ToggleArabic()
if vim.g.rl == true then
   vim.g.rl = true
   vim.g.keymap=''
else
   vim.g.keymap='arabic'
   vim.g.nospell= true
end
--]]

vim.g.mapleader = ' '

vim.keymap.set('t', '<Esc>', '<C-\\><C-n>') -- exit

vim.keymap.set('n', 'Q', 'gq')

vim.keymap.set('n', '<leader>o', ':setlocal spell! spelllang=en_us<CR>')

vim.keymap.set('n', '<c-s>', ':w<CR>')
vim.keymap.set('i', '<c-s>', '<Esc>:w<CR>a')

-- vim.keymap.set('n', '<leader>H', ':call ToggleHiddenAll()<CR>')
-- xnorevim.keymap.set <silent> gP "+P
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')
vim.keymap.set('n', 'J', 'mzJ`z')

--vim.keymap.set ('n', '<Leader>ga', ':G<Space>add . %:p:h<CR>')
--vim.keymap.set ('n', '<Leader>gc', ':G<Space>commit -m 'on "date +'%Y-%m-%d %H:%M:%S'`-- . %:p:h<CR>')
--
--cnorevim.keymap.set w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!
--

-- vim.keymap.set ('n', '<leader>d', '"_d')
-- vim.keymap.set ('v', '<leader>d', '"_d')

vim.keymap.set('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>')

vim.keymap.set('v', 'vvim.keymap.set', '<silent><buffer> <CR> :MiniMDTaskToggle<CR>')

vim.keymap.set('n', '<silent><buffer>', '= :MiniMDPromote<CR>')


-- Perform dot commands over visual blocks
vim.keymap.set('v', '.', ':normal .<CR>')

-- vim.keymap.set ('n', '<leader>pp', ':!cd $PWD  && git add .&& git commit -m "generated files on `date +\'%Y-%m-%d %H:%M:%S\'`"&& git push -u origin main<CR>')
-- vim.keymap.set ('n', '<leader>PP', ':!cd $PWD  && git pull && git merge<CR>')
vim.keymap.set('n', '<F1>', ':call ToggleArabic()<CR>')
vim.keymap.set('i', '<F1>', '<Esc>l:call ToggleArabic()<CR>i')
vim.keymap.set('v', '<F1>', '<Esc>l:call ToggleArabic()<CR>i')
vim.keymap.set('n', '<S-F1>', ':call TogglePersian()<CR>')
vim.keymap.set('v', '<F2>', '<Esc>l:call TogglePersian()<CR>i')
vim.keymap.set('i', '<F2>', '<Esc>l:call TogglePersian()<CR>i')
-- vim.keymap.set('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>')
vim.keymap.set('n', '<Up>', '<c-w>+')
vim.keymap.set('n', '<Down>', '<c-w>-')
vim.keymap.set('n', '<Left>', '<c-w><')
vim.keymap.set('n', '<Right>', '<c-w>>')

vim.keymap.set('n', '!', ':!')

vim.keymap.set('n', '<leader><S-C>', ':w! | !compiler "<c-r>%"<CR>')
--autocmd filetype cpp nnorevim.keymap.set <Leader>c :w! \| :split \| term g++ % -o %< && ./%< <CR>i
--autocmd filetype java nnorevim.keymap.set <Leader>c :w! \| :split \| term java % <CR>


vim.keymap.set('n', '<leader>o', ':!opout <c-r>%<CR><CR>')

-- command line mode
--vim.keymap.set('c', '<C-A>', '<Home>')
--vim.keymap.set('c', '<C-f>', '<Right>')
--vim.keymap.set('c', '<A-f>', '<S-Right>')
--vim.keymap.set('c', '<C-b>', '<Left>')
--vim.keymap.set('c', '<A-b>', '<S-Left>')

vim.keymap.set('n', '<leader>e', ':E<CR>')

-- vim.keymap.set('n', '<C-s>', '<Plug>(easymotion-overwin-f)')
-- vim.keymap.set ('n', '<C-j>', '<Plug>(easymotion-j)')
-- vim.keymap.set ('n', '<C-k>', '<Plug>(easymotion-k)')
-- vim.keymap.set ('n', '<C-s>', '<Plug>(easymotion-overwin-f2)')

-- vim.keymap.set('n', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")
-- vim.keymap.set('t', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")
-- vim.keymap.set('i', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")

-- copy file paths
vim.keymap.set('n', 'yp', '<cmd>let @+ = expand("%")<cr>')
vim.keymap.set('n', 'yP', '<cmd>let @+ = expand("%:p")<cr>')
-- kill buffer
-- vim.keymap.set ('n', '<leader>k', '<cmd>bp<bar>sp<bar>bn<bar>bd<CR>')
vim.keymap.set('', '<C-h>', '<C-w>h')
vim.keymap.set('', '<C-j>', '<C-w>j')
vim.keymap.set('', '<C-k>', '<C-w>k')
vim.keymap.set('', '<C-l>', '<C-w>l')
-- Emacs-like bindings for managing splits
-- vim.keymap.set('', '<A-1>', '<C-w>o')
vim.keymap.set('', '<A-O>', '<C-w>R')
vim.keymap.set('', '<A-=>', '<C-w>=')
-- vim.keymap.set('', '<A-2>', '<cmd>split<CR>')
-- vim.keymap.set('', '<A-3>', '<cmd>vsplit<CR>')
-- managing tabs
-- vim.keymap.set('', '<A-4>', '<C-w>T')
vim.keymap.set('', '<A-t>', '<cmd>tabnew<CR>')
vim.keymap.set('', '<A-C-t>', '<cmd>tabclose<CR>')
vim.keymap.set('', '<A-l>', '<cmd>tabnext<CR>')
vim.keymap.set('', '<A-h>', '<cmd>tabprevious<CR>')
-- Substitute
vim.keymap.set('n', 'S', ':%s//g<Left><Left>')
vim.keymap.set('v', 'S', ':s//g<Left><Left>')
-- diagnostics
vim.keymap.set('n', '<A-p>', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', '<A-n>', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<M-k>', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '<leader>ll', vim.diagnostic.setloclist, opts)

vim.keymap.set('n', '<leader>X', "<cmd>!chmod +x %<CR>", opts)

vim.keymap.set('n', '<leader>c', ':nohl<CR>')
