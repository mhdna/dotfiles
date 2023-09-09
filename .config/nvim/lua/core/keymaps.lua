vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
local opts = { noremap = true }

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

vim.keymap.set('n', 'Q', 'gq')
-- vim.keymap.set('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>')
-- map('n', '<leader>=', 'gg=G`<')
vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')
vim.keymap.set('n', 'J', 'mzJ`z')
vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv")
-- Perform dot commands over visual blocks
vim.keymap.set('v', '.', ':normal .<CR>', opts)

vim.keymap.set('n', '<backspace>', ':lua ToggleArabic()<CR>', opts)
vim.keymap.set({ 'i', 'v' }, '<M-backspace>', '<Esc>:lua ToggleArabic()<CR>li', opts)
-- map('n', '<C-space>', ':lua TogglePersian()<CR>')
-- map({ 'i', 'v' }, '<C-space>', '<Esc>l:lua TogglePersian()<CR>i')
vim.keymap.set('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>', opts)
vim.keymap.set('n', '<Up>', '<c-w>+', opts)
vim.keymap.set('n', '<Down>', '<c-w>-', opts)
vim.keymap.set('n', '<Left>', '<c-w><', opts)
vim.keymap.set('n', '<Right>', '<c-w>>', opts)

vim.keymap.set({ 'n', 'v' }, '<leader>c', ':w! | !compiler "<c-r>%"<CR>', opts)
vim.keymap.set('n', '<leader>o', ':!opout <c-r>%<CR><CR>', opts)

-- map('n', '<leader>e', ':Explore<CR>', { silent = true, noremap = true})
-- copy file paths
vim.keymap.set('n', 'yp', '<cmd>let @+ = expand("%")<cr>', opts)
vim.keymap.set('n', 'yP', '<cmd>let @+ = expand("%:p")<cr>', opts)
-- exit terminal mode with escape
vim.keymap.set('t', '<Esc>', '<C-\\><C-N>', opts)
-- Substitute
vim.keymap.set('n', '<C-s>', ':%s//g<Left><Left>', opts)
vim.keymap.set('v', '<C-s>', ':s//g<Left><Left>', opts)
-- delete empty lines
vim.keymap.set('v', '<leader>D', ':g/^$/d<CR>:nohl<CR>', opts)

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<M-k>', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)
