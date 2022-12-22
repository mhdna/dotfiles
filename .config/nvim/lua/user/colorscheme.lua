function ColorScheme(color)
    color = color or "muted"
    vim.cmd.colorscheme(color)

    -- vim.api.nvim_set_hl(0, "Normal", {bg = "#262626"})
    -- vim.api.nvim_set_hl(0, "NormalFloat", {bg = "#262626"})
end

ColorScheme()

vim.opt.bg = 'dark'
-- vim.cmd(('hi Visual  guifg=#000000 guibg=#ffff00 gui=none'))
vim.cmd(('hi Normal  guibg=#3b3b3d '))
vim.cmd(('hi NormalFloat  guibg=#3b3b3d '))
-- vim.cmd(('hi CursorLine guibg=#b4eeb4 '))
-- vim.cmd(('hi StatusLine guifg=#ffffff guibg=#000000'))
-- vim.cmd(('hi Pmenu guifg=#000000 guibg=#fff8dc'))
-- vim.cmd(('hi PmenuSbar guifg=#8b0000 guibg=#8b0000 '))
-- vim.cmd(('hi PmenuSel guifg=black guibg=#add8e6'))
