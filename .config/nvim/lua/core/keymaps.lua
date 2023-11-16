vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
local opts = { noremap = true }

vim.keymap.set('n', 'Q', 'gq')

vim.keymap.set('c', '<M-e>', '<C-f>', opts)

vim.keymap.set('n', '<leader>w', ':w<CR>', opts)
vim.keymap.set('n', '<leader>W', ':w ', opts)

vim.keymap.set('n', 'n', 'nzzzv')
vim.keymap.set('n', 'N', 'Nzzzv')
vim.keymap.set('n', 'J', 'mzJ`z')

-- Perform dot commands over visual blocks
vim.keymap.set('v', '.', ':normal .<CR>', opts)

-- repeat substituion in mode
vim.keymap.set('v', '&', ':&&<CR>', opts)

vim.keymap.set('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>', opts)
vim.keymap.set('n', '<Up>', '<c-w>+', opts)
vim.keymap.set('n', '<Down>', '<c-w>-', opts)
vim.keymap.set('n', '<Left>', '<c-w><', opts)
vim.keymap.set('n', '<Right>', '<c-w>>', opts)

vim.keymap.set('n', '<leader>o', ':!opout <c-r>%<CR><CR>', opts)

-- copy file paths
vim.keymap.set('n', 'yp', '<cmd>let @+ = expand("%")<cr>', opts)
vim.keymap.set('n', 'yP', '<cmd>let @+ = expand("%:p")<cr>', opts)
-- exit terminal mode with escape
vim.keymap.set('t', '<Esc>', '<C-\\><C-N>', opts)
-- Substitute
vim.keymap.set('n', '<C-s>', ':%s//g<Left><Left>', opts)
vim.keymap.set('v', '<C-s>', ':s//g<Left><Left>', opts)

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<M-k>', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>l', vim.diagnostic.setloclist)

vim.keymap.set("n", "<leader>x", ":w<CR>:!chmod +x %<CR>", opts)
vim.keymap.set("n", "<leader>F", ":filetype detect<CR>", opts)

-- save as super user
vim.cmd [[cabbrev w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!]]

-- Formatting
vim.keymap.set('n', '<leader>=', ":w<CR>:Format<CR>", { noremap = true, buffer = bufnr, silent = true })

vim.keymap.set({ 'n', 'v' }, '<leader>c', ':w!<CR>:Compile<CR>', { noremap = true, buffer = bufnr, silent = true })

vim.api.nvim_set_keymap('n', '<Leader>h', '<Cmd>noh<CR>', opts)
