vim.keymap.set('n', '<leader>f', "<cmd>lua require('fzf-lua').files()<CR>")
vim.keymap.set('n', '<leader>g', "<cmd>lua require('fzf-lua').live_grep()<CR>")
-- vim.keymap.set('n', '<leader>q', "<cmd>lua require('fzf-lua').quickfix()<CR>")
vim.keymap.set('n', '<leader>j', "<cmd>lua require('fzf-lua').buffers()<CR>")
vim.keymap.set('n', '<leader>r', "<cmd>lua require('fzf-lua').oldfiles()<CR>") -- recent files
vim.keymap.set('n', '<leader>M', "<cmd>lua require('fzf-lua').marks()<CR>")
