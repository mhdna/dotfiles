function ColorScheme(color)
    -- color = color or "gruvbox"
    -- vim.g.gruvbox_contrast_dark = 'hard'
    vim.cmd.colorscheme(color)

    -- vim.api.nvim_set_hl(0, "Normal", {bg = "#262626"})
    -- vim.api.nvim_set_hl(0, "NormalFloat", {bg = "#262626"})
end

ColorScheme()

vim.opt.bg = 'dark'
vim.cmd(('hi SignColumn guibg=#000000 '))
vim.cmd(('hi Visual  guifg=black gui=none'))
-- vim.cmd(('hi Normal  guibg=#262626 '))
-- vim.cmd(('hi NormalFloat  guibg=#262626 '))
vim.cmd(('hi CursorLine guibg=#b4eeb4 '))
vim.cmd(('hi Pmenu guifg=yellow guibg=#000000'))
vim.cmd(('hi PmenuSel guifg=black'))
