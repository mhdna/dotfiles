vim.opt.signcolumn = "yes" --for linting, diagnostics, left column
vim.opt.mouse = ''
vim.o.colorcolumn = 80 -- don't exceed 80 columns, maybe you're in the 3rd 4th indent so rethink
vim.opt.completeopt = 'menuone,noinsert,noselect' -- Autocomplete options
-- vim.opt.laststatus = 2
vim.opt.pumheight= 15
-- vim.opt.foldmethod = 'marker' -- Enable folding (default 'foldmarker')
-- always use a block shaped cursor
vim.o.guicursor = true
vim.opt.expandtab = true -- Use spaces instead of tabs
vim.opt.smartindent = true -- Autoindent new lines
vim.opt.hidden = true -- Enable background buffers
vim.opt.history = 100 -- Remember N lines in history
vim.opt.lazyredraw = true -- Faster scrolling
-- g.syntax = on
-- opt.synmaxcol = 240         -- Max column for syntax highlight
vim.opt.updatetime = 700 -- ms to wait for trigger an event

-- vim.o.exrc       = true -- so if a vimrc file is in a dir, it's automatically sourced (for custom projects)
vim.o.compatible = false
vim.o.incsearch  = true -- highlight incrementaly rather than the whole word word
vim.o.ignorecase  = true -- only ignore case if write uppercase letter
vim.o.smartcase  = true -- only ignore case if write uppercase letter
vim.o.backup     = false
vim.o.undofile   = true
-- vim.o.showmode = false
vim.o.hlsearch   = false -- Do not keep the previous search occurance highlighted
-- vim.o.swapfile   = false

-- vim.wo.relativenumber = true
-- vim.o.number = true
-- vim.o.scrolloff = 8
vim.opt.termguicolors = true -- Enable 24-bit RGB colors
vim.opt.linebreak = false
vim.bo.tabstop = 4
vim.opt.shiftwidth = 4

vim.o.title = true
vim.o.go = a
vim.o.clipboard = "unnamedplus"
vim.o.undofile = true
vim.o.undodir = os.getenv("HOME") .. "/.cache/nvim-undodir"

vim.o.encoding = "utf-8"
-- vim.o.guifont = "hack:h10" -- the font used in vim gui

-- vim.o.autochdir = true
-- Tab command mode completion
-- vim.o.wildmode = longest,list,full

-- Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
vim.o.splitbelow = true
vim.o.splitright = true

-- Highlight current line
-- vim.o.cursorline = true
-- set noshowcmd
-- set formatoptions-=o
-- set delcombine                               " Delete part of combining character with x command. Useful for editing Arabic diacritics.
-- set updatetime=50
-- set inccommand=nosplit
-- vim.opt.statusline="%F"

-- Treesitter folding module
vim.opt.foldmethod='expr'
vim.cmd([[
set foldexpr=nvim_treesitter#foldexpr()
set nofoldenable                     " Disable folding at startup.
]])

-- better netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

-- Disable nvim intro
vim.opt.shortmess:append "sI"
-- Disable builtins plugins
local disabled_built_ins = {
    -- "netrw",
    -- "netrwPlugin",
    -- "netrwSettings",
    -- "netrwFileHandlers",
    "gzip",
    "zip",
    "zipPlugin",
    "tar",
    "tarPlugin",
    "getscript",
    "getscriptPlugin",
    "vimball",
    "vimballPlugin",
    "2html_plugin",
    "logipat",
    "rrhelper",
    "spellfile_plugin",
    "matchit"
}
for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end
-- disable all languages but lua and vimscript for editing
-- vim.g.loaded_perl_provider = 0
-- vim.g.loaded_ruby_provider = 0
-- vim.g.loaded_node_provider = 0
-- vim.g.loaded_python_provider = 0
-- vim.g.loaded_python3_provider = 0
