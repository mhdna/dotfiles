vim.api.nvim_create_user_command('Compile', function()
    vim.cmd('source %')
end, { nargs = 0 })
