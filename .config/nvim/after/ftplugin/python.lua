local utils = require('utils')

vim.cmd [[
set nowrap
set sidescroll=5
set sidescrolloff=2
set colorcolumn=80

set tabstop=4       " number of visual spaces per TAB
set softtabstop=4   " number of spaces in tab when editing
set shiftwidth=4    " number of spaces to use for autoindent
set expandtab       " expand tab to spaces so that tabs are spaces
]]

vim.api.nvim_create_user_command('Compile', function()
        local src = vim.fn.expand("%:p:~")
        utils.Compile("python", src)
end, { nargs = 0 })
