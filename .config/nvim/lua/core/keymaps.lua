vim.g.mapleader = ' '
local bufopts = { noremap = true, silent = true, buffer = bufnr }

vim.cmd([[
func! ToggleArabic()
if &rl
    set norl
    set keymap=
else
    set rl
    set keymap=arabic
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
    set keymap=persian
    set nospell
end
endfunc ]])

vim.keymap.set('n', 'Q', 'gq', {noremap = true, buffer = bufnr})
vim.keymap.set('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>', {noremap = true, buffer = bufnr})
vim.keymap.set('n', '<leader>F', 'gg=G<C-o>', {noremap = true, buffer = bufnr})
-- vim.keymap.set('n', '<leader>H', ':call ToggleHiddenAll()<CR>', bufopts)
vim.keymap.set('n', 'n', 'nzzzv', {noremap = true, buffer = bufnr})
vim.keymap.set('n', 'N', 'Nzzzv', {noremap = true, buffer = bufnr} )
vim.keymap.set('n', 'J', 'mzJ`z', {noremap = true, buffer = bufnr})
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", {noremap = true, buffer = bufnr})
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", {noremap = true, buffer = bufnr})
vim.keymap.set('c', 'w!!', "execute 'silent! write !sudo tee % >/dev/null' <bar> edit!",  {noremap = true, buffer = bufnr} )
-- vim.keymap.set ('n', '<leader>d', '"_d', bufopts)
-- vim.keymap.set ('v', '<leader>d', '"_d', bufopts)
vim.keymap.set('n', '!', ':!', {noremap = true, buffer = bufnr})
-- Perform dot commands over visual blocks
vim.keymap.set('v', '.', ':normal .<CR>', bufopts)

vim.keymap.set('n', '<F1>', ':call ToggleArabic()<CR>', bufopts)
vim.keymap.set('i', '<F1>', '<Esc>l:call ToggleArabic()<CR>i', bufopts)
vim.keymap.set('v', '<F1>', '<Esc>l:call ToggleArabic()<CR>i', bufopts)
vim.keymap.set('n', '<S-F1>', ':call TogglePersian()<CR>', bufopts)
vim.keymap.set('v', '<F2>', '<Esc>l:call TogglePersian()<CR>i', bufopts)
vim.keymap.set('i', '<F2>', '<Esc>l:call TogglePersian()<CR>i', bufopts)
vim.keymap.set('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>', bufopts)
vim.keymap.set('n', '<Up>', '<c-w>+', bufopts)
vim.keymap.set('n', '<Down>', '<c-w>-', bufopts)
vim.keymap.set('n', '<Left>', '<c-w><', bufopts)
vim.keymap.set('n', '<Right>', '<c-w>>', bufopts)

vim.keymap.set('n', '<leader>c', ':w! | !compiler "<c-r>%"<CR>', bufopts)
vim.keymap.set('n', '<leader>o', ':!opout <c-r>%<CR><CR>', bufopts)

vim.keymap.set('n', '<leader>e', ':Explore<CR>', bufopts)
-- copy file paths
vim.keymap.set('n', 'yp', '<cmd>let @+ = expand("%")<cr>', bufopts)
vim.keymap.set('n', 'yP', '<cmd>let @+ = expand("%:p")<cr>', bufopts)
-- kill buffer
vim.keymap.set ('n', '<leader>k', '<cmd>bp<bar>sp<bar>bn<bar>bd<CR>', bufopts)
-- move around splits
vim.keymap.set('', '<C-h>', '<C-w>h', bufopts)
vim.keymap.set('', '<C-j>', '<C-w>j', bufopts)
vim.keymap.set('', '<C-k>', '<C-w>k', bufopts)
vim.keymap.set('', '<C-l>', '<C-w>l', bufopts)
-- Emacs-like bindings for managing splits
vim.keymap.set('', '<A-0>', '<cmd>close<Cr>', bufopts)
vim.keymap.set('', '<A-1>', '<C-w>o', bufopts)
vim.keymap.set('', '<A-O>', '<C-w>R', bufopts)
vim.keymap.set('', '<A-=>', '<C-w>=', bufopts)
vim.keymap.set('', '<A-2>', '<cmd>split<CR>', bufopts)
vim.keymap.set('', '<A-3>', '<cmd>vsplit<CR>', bufopts)
-- managing tabs
vim.keymap.set('', '<A-4>', '<C-w>T', bufopts)
vim.keymap.set('', '<A-t>', '<cmd>tabnew<CR>', bufopts)
vim.keymap.set('t', '<A-t>', '<cmd>tabnew<CR>', bufopts)
vim.keymap.set('', '<A-C-t>', '<cmd>tabclose<CR>', bufopts)
vim.keymap.set('t', '<A-C-t>', '<cmd>tabclose<CR>', bufopts)
vim.keymap.set('', '<A-l>', '<cmd>tabnext<CR>', bufopts)
vim.keymap.set('t', '<A-l>', '<cmd>tabnext<CR>', bufopts)
vim.keymap.set('', '<A-h>', '<cmd>tabprevious<CR>', bufopts)
vim.keymap.set('t', '<A-h>', '<cmd>tabprevious<CR>', bufopts)
-- Substitute
vim.keymap.set('n', '<C-s>', ':%s//g<Left><Left>', {noremap = true, buffer = bufnr})
vim.keymap.set('v', '<C-s>', ':s//g<Left><Left>', {noremap = true, buffer = bufnr})
-- delete empty lines
vim.keymap.set('v', '<leader>D', ':g/^$/d<CR>:nohl<CR>', bufopts)
-- diagnostics
vim.keymap.set('n', '<A-p>', vim.diagnostic.goto_prev, bufopts)
vim.keymap.set('n', '<A-n>', vim.diagnostic.goto_next, bufopts)
vim.keymap.set('n', '<M-k>', vim.diagnostic.open_float, bufopts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, bufopts)

vim.keymap.set('n', '<leader>x', "<cmd>w<CR><cmd>so %<CR>", {noremap = true, buffer = bufnr})
vim.keymap.set('n', '<leader>X', "<cmd>!chmod +x %<CR>", {noremap = true, buffer = bufnr})
