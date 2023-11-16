local utils         = require('utils')

vim.bo.tabstop      = 2 -- number of visual spaces per TAB
vim.opt.softtabstop = 2 -- number of spaces in tab when editing
vim.opt.shiftwidth  = 2 -- number of spaces to use for autoindent

vim.api.nvim_create_user_command('Compile', function()
        local src = vim.fn.expand("%:p:~")
        utils.Compile("node", src)
end, { nargs = 0 })
