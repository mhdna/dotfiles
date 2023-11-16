vim.opt_local.makeprg = "shellcheck -f gcc '%'"
vim.api.nvim_create_user_command('Compile', function()
        -- save current buffer index
        local current_win = vim.fn.winnr()

        -- Check if the quickfix window is already open
        local is_quickfix_open = false

        -- Iterate over all windows
        for _, winid in ipairs(vim.fn.getwininfo()) do
                if winid['quickfix'] == 1 then
                        is_quickfix_open = true
                        break
                end
        end

        if is_quickfix_open then
                -- Close the quickfix window if it's already open
                vim.cmd('cclose')
        else
                -- Open the quickfix window using cwindow
                vim.cmd('w | silent make | botright cwindow') -- INFO belowright to split right below this split

                -- Return to the previous split
                vim.cmd(current_win .. 'wincmd w')
        end
end, { nargs = 0 })

vim.api.nvim_create_user_command('Format', function()
        local src = vim.fn.expand("%:p:~")
        vim.cmd('silent !shfmt -w -i 4 -ln bash ' .. src)
end, { nargs = 0 })

local opts = { noremap = true, silent = true, buffer = 0 }

-- vim.keymap.set('n', '<F5>', function() ToggleQuickfix() end, opts)
vim.keymap.set('n', '<leader>s', ':source ~/.config/nvim/ftplugin/sh.lua<CR>', opts)
vim.keymap.set('n', ']d', ':cnext<CR>', opts)
vim.keymap.set('n', '[d', ':cprevious<CR>', opts)
