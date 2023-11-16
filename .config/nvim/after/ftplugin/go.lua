local utils = require('utils')

vim.api.nvim_create_user_command('Compile', function()
    local src = vim.fn.expand("%:p:~")
    utils.Compile("go", "run " .. src)
end, { nargs = 0 })

vim.api.nvim_create_user_command('Format', function()
    vim.lsp.buf.format()
    -- vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
    vim.cmd('OrganizeImports')
end, { nargs = 0 })
