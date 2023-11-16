vim.api.nvim_create_user_command('Format', function()
    local bufnr = vim.fn.bufnr('%')

    -- Check if LSP is attached to the current buffer
    if vim.lsp.buf_get_clients(bufnr)[1] ~= nil then
        vim.lsp.buf.format()
    else
        vim.cmd('normal! gg=G``')
    end
end, { nargs = 0 })

-- delete empty lines
vim.api.nvim_create_user_command('DeleteEmptyLines', function()
    if vim.bo.binary == false and vim.opt.filetype:get() ~= "diff" then
        local old_query = vim.fn.getreg("/")      -- save search register
        M.preserve("sil! 1,.s/^\\n\\{2,}/\\r/gn") -- set current search count number
        local result = vim.fn.searchcount({ maxcount = 1000, timeout = 500 }).current
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        M.preserve("sil! keepp keepj %s/^\\n\\{2,}/\\r/ge")
        M.preserve("sil! keepp keepj %s/\\v($\\n\\s*)+%$/\\r/e")
        if result > 0 then
            vim.api.nvim_win_set_cursor({ 0 }, { (line - result), col })
        end
        vim.fn.setreg("/", old_query) -- restore search register
    end
end, { nargs = 0 })

vim.api.nvim_create_user_command('Compile', function()
    vim.cmd('!compiler "%"')
end, { nargs = 0 })


vim.api.nvim_create_user_command('Arabic', function()
    if vim.o.rl then
        vim.o.rl = false
        vim.o.keymap = ''
    else
        vim.o.rl = true
        vim.o.keymap = 'arabic'
        vim.o.arabicshape = true
        vim.g.nospell = true
    end
end, { nargs = 0 })

vim.api.nvim_create_user_command('OrganizeImports', function()
    local params = vim.lsp.util.make_range_params(nil, vim.lsp.util._get_offset_encoding())
    params.context = { only = { "source.organizeImports" } }

    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
    for _, res in pairs(result or {}) do
        for _, r in pairs(res.result or {}) do
            if r.edit then
                vim.lsp.util.apply_workspace_edit(r.edit, vim.lsp.util._get_offset_encoding())
            else
                vim.lsp.buf.execute_command(r.command)
            end
        end
    end
end, { nargs = 0 })
