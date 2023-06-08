vim.g.mapleader = ' '
local opts = { noremap = true, buffer = bufnr  }

vim.cmd([[
func! ToggleArabic()
if &rl
    set norl
    set keymap=
    set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20
else
    set rl
    set keymap=arabic
    set arabicshape
    set guicursor=
    set nospell
end
endfunc

func! TogglePersian()
if &rl
    set norl
    set keymap=
    set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20
else
    set rl
    set keymap=persian
    set guicursor=
    set nospell
end
endfunc ]])

vim.keymap.set('n', 'Q', 'gq', { noremap = true, buffer = bufnr })
vim.keymap.set('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>', {noremap = true, buffer = bufnr})
vim.keymap.set('n', '<leader>F', 'gg=G<C-o>', {noremap = true, buffer = bufnr})
vim.keymap.set('n', 'n', 'nzzzv', {noremap = true, buffer = bufnr})
vim.keymap.set('n', 'N', 'Nzzzv', {noremap = true, buffer = bufnr} )
vim.keymap.set('n', 'J', 'mzJ`z', {noremap = true, buffer = bufnr})
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", {noremap = true, buffer = bufnr})
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", {noremap = true, buffer = bufnr})
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
-- exit terminal mode with escape
vim.keymap.set('t', '<Esc>', '<C-\\><C-N>', opts)
-- Substitute
vim.keymap.set('n', '<C-s>', ':%s//g<Left><Left>', {noremap = true, buffer = bufnr})
vim.keymap.set('v', '<C-s>', ':s//g<Left><Left>', {noremap = true, buffer = bufnr})
-- delete empty lines
-- vim.keymap.set('v', '<leader>D', ':g/^$/d<CR>:nohl<CR>', opts)
-- diagnostics
vim.keymap.set('n', '<A-p>', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', '<A-n>', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<M-Return>', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)

vim.keymap.set('n', '<leader>x', "<cmd>w<CR><cmd>so %<CR>", {noremap = true, buffer = bufnr})
vim.keymap.set('n', '<leader>X', "<cmd>!chmod +x %<CR>", {noremap = true, buffer = bufnr})

-- a little fix for luasnip exiting insert mode when pressing backspace
-- https://github.com/L3MON4D3/LuaSnip/issues/622
vim.keymap.set('s','<BS>', '<C-O>s')
-- Clear search highlight
vim.keymap.set('n','<leader>c', ':nohlsearch<CR>')
