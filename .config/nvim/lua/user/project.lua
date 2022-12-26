require('telescope').load_extension('projects')

vim.keymap.set('n', '<leader>p', function() require'telescope'.extensions.projects.projects()end, {})
