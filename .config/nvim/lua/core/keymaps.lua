vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
local opts = { noremap = true, buffer = bufnr }

local map = function(mode, lhs, rhs)
    vim.keymap.set(mode, lhs, rhs, opts)
end

function ToggleArabic()
    if vim.o.rl then
        vim.o.rl = false
        vim.o.keymap = ''
        -- vim.o.guicursor = 'n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20'
    else
        vim.o.rl = true
        vim.o.keymap = 'arabic'
        vim.o.arabicshape = true
        -- vim.o.guicursor = ''
        vim.g.nospell = true
    end
end

function TogglePersian()
    if vim.o.rl then
        vim.o.rl = false
        vim.o.keymap = ''
        -- vim.o.guicursor = 'n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20'
    else
        vim.o.rl = true
        vim.o.keymap = 'persian'
        -- vim.o.guicursor = ''
        vim.g.nospell = true
    end
end

vim.keymap.set('n', 'Q', 'gq', { noremap = true, buffer = bufnr })
vim.keymap.set('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>', { noremap = true, buffer = bufnr })
-- map('n', '<leader>=', 'gg=G`<')
vim.keymap.set('n', 'n', 'nzzzv', { noremap = true, buffer = bufnr })
vim.keymap.set('n', 'N', 'Nzzzv', { noremap = true, buffer = bufnr })
vim.keymap.set('n', 'J', 'mzJ`z', { noremap = true, buffer = bufnr })
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv", { noremap = true, buffer = bufnr })
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv", { noremap = true, buffer = bufnr })
vim.keymap.set({ 'n', 'v' }, '!', ':!', { noremap = true, buffer = bufnr })
-- Perform dot commands over visual blocks
map('v', '.', ':normal .<CR>')

map('n', '<F1>', ':lua ToggleArabic()<CR>')
map({ 'i', 'v' }, '<F1>', '<Esc>:lua ToggleArabic()<CR>i')
map('n', '<S-F1>', ':lua TogglePersian()<CR>')
map({ 'i', 'v' }, '<S-F1>', '<Esc>l:lua TogglePersian()<CR>i')
map('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>')
map('n', '<Up>', '<c-w>+')
map('n', '<Down>', '<c-w>-')
map('n', '<Left>', '<c-w><')
map('n', '<Right>', '<c-w>>')
-- Go to the beginning and end of current line in insert mode quickly
map("i", "<C-A>", "<HOME>")
map("i", "<C-E>", "<END>")

-- map({ 'n', 'v' }, '<leader>c', ':w! | !compiler "<c-r>%"<CR>')
map('n', '<leader>o', ':!opout <c-r>%<CR><CR>')

map('n', '<leader>e', ':Explore<CR>')
-- copy file paths
map('n', 'yp', '<cmd>let @+ = expand("%")<cr>')
map('n', 'yP', '<cmd>let @+ = expand("%:p")<cr>')
-- exit terminal mode with escape
map('t', '<Esc>', '<C-\\><C-N>')
-- Substitute
map('n', '<C-s>', ':%s//g<Left><Left>')
map('v', '<C-s>', ':s//g<Left><Left>')
-- delete empty lines
map('v', '<leader>D', ':g/^$/d<CR>:nohl<CR>')

vim.keymap.set('n', '<leader>x', "<cmd>w<CR><cmd>so %<CR>", { noremap = true, buffer = bufnr })
-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', '<M-k>', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostics list' })
