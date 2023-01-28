vim.g.mapleader = ' '
local opts = { noremap = true, silent = true, buffer = bufnr }

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

vim.keymap.set('n', 'Q', 'gq', opts)
vim.keymap.set('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>', opts)
vim.keymap.set('n', '<leader>F', 'gg=G<C-o>', opts)
-- vim.keymap.set('n', '<leader>H', ':call ToggleHiddenAll()<CR>', opts)
vim.keymap.set('n', 'n', 'nzzzv', opts)
vim.keymap.set('n', 'N', 'Nzzzv', opts)
vim.keymap.set('n', 'J', 'mzJ`z', opts)
vim.keymap.set('c', 'w!!', "execute 'silent! write !sudo tee % >/dev/null' <bar> edit!",  {noremap = true, buffer = bufnr} )
-- vim.keymap.set ('n', '<leader>d', '"_d', opts)
-- vim.keymap.set ('v', '<leader>d', '"_d', opts)
vim.keymap.set('n', '!', ':!', {noremap = true, buffer = bufnr})
-- Perform dot commands over visual blocks
vim.keymap.set('v', '.', ':normal .<CR>', opts)

vim.keymap.set('n', '<F1>', ':call ToggleArabic()<CR>', opts)
vim.keymap.set('i', '<F1>', '<Esc>l:call ToggleArabic()<CR>i', opts)
vim.keymap.set('v', '<F1>', '<Esc>l:call ToggleArabic()<CR>i', opts)
vim.keymap.set('n', '<S-F1>', ':call TogglePersian()<CR>', opts)
vim.keymap.set('v', '<F2>', '<Esc>l:call TogglePersian()<CR>i', opts)
vim.keymap.set('i', '<F2>', '<Esc>l:call TogglePersian()<CR>i', opts)
vim.keymap.set('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>', opts)
vim.keymap.set('n', '<Up>', '<c-w>+', opts)
vim.keymap.set('n', '<Down>', '<c-w>-', opts)
vim.keymap.set('n', '<Left>', '<c-w><', opts)
vim.keymap.set('n', '<Right>', '<c-w>>', opts)

vim.keymap.set('n', '<leader>c', ':w! | !compiler "<c-r>%"<CR>', opts)
vim.keymap.set('n', '<leader>o', ':!opout <c-r>%<CR><CR>', opts)

vim.keymap.set('n', '<leader>e', ':Explore<CR>', opts)
-- copy file paths
vim.keymap.set('n', 'yp', '<cmd>let @+ = expand("%")<cr>', opts)
vim.keymap.set('n', 'yP', '<cmd>let @+ = expand("%:p")<cr>', opts)
-- kill buffer
vim.keymap.set ('n', '<leader>k', '<cmd>bp<bar>sp<bar>bn<bar>bd<CR>', opts)
-- move around splits
vim.keymap.set('', '<C-h>', '<C-w>h', opts)
vim.keymap.set('', '<C-j>', '<C-w>j', opts)
vim.keymap.set('', '<C-k>', '<C-w>k', opts)
vim.keymap.set('', '<C-l>', '<C-w>l', opts)
-- Emacs-like bindings for managing splits
vim.keymap.set('', '<A-0>', '<cmd>close<Cr>', opts)
vim.keymap.set('', '<A-1>', '<C-w>o', opts)
vim.keymap.set('', '<A-O>', '<C-w>R', opts)
vim.keymap.set('', '<A-=>', '<C-w>=', opts)
vim.keymap.set('', '<A-2>', '<cmd>split<CR>', opts)
vim.keymap.set('', '<A-3>', '<cmd>vsplit<CR>', opts)
-- managing tabs
vim.keymap.set('', '<A-4>', '<C-w>T', opts)
vim.keymap.set('', '<A-t>', '<cmd>tabnew<CR>', opts)
vim.keymap.set('t', '<A-t>', '<cmd>tabnew<CR>', opts)
vim.keymap.set('', '<A-C-t>', '<cmd>tabclose<CR>', opts)
vim.keymap.set('t', '<A-C-t>', '<cmd>tabclose<CR>', opts)
vim.keymap.set('', '<A-l>', '<cmd>tabnext<CR>', opts)
vim.keymap.set('t', '<A-l>', '<cmd>tabnext<CR>', opts)
vim.keymap.set('', '<A-h>', '<cmd>tabprevious<CR>', opts)
vim.keymap.set('t', '<A-h>', '<cmd>tabprevious<CR>', opts)
-- Substitute
vim.keymap.set('n', '<C-s>', ':%s//g<Left><Left>', {noremap = true, buffer = bufnr})
vim.keymap.set('v', '<C-s>', ':s//g<Left><Left>', {noremap = true, buffer = bufnr})
-- delete empty lines
vim.keymap.set('v', '<leader>D', ':g/^$/d<CR>:nohl<CR>', opts)
-- diagnostics
vim.keymap.set('n', '<A-p>', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', '<A-n>', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<M-k>', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '<leader>ll', vim.diagnostic.setloclist, opts)

vim.keymap.set('n', '<leader>x', "<cmd>w<CR><cmd>so %<CR>", opts)
vim.keymap.set('n', '<leader>X', "<cmd>!chmod +x %<CR>", {noremap = true, buffer = bufnr})
