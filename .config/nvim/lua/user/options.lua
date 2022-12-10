-----------------------------------------------------------
-- General Neovim settings and configuration
-----------------------------------------------------------

-- Default options are not included
-- See: https://neovim.io/doc/user/vim_diff.html
-- [2] Defaults - *nvim-defaults*

local g = vim.g -- Global variables
local opt = vim.opt -- Set options (global/buffer/windows-scoped)

-----------------------------------------------------------
-- General
-----------------------------------------------------------
opt.mouse = 'a' -- Enable mouse support
-- opt.cursorcolumn = true -- highlight cursor column
vim.o.colorcolumn = 80 -- don't exceed 80 columns, maybe you're in the 3rd 4th indent so rethink
opt.completeopt = 'menuone,noinsert,noselect' -- Autocomplete options
opt.bg = 'light'
vim.opt.laststatus = 2
opt.pumheight= 15

-- Always show the signcolumn, otherwise it would shift the text each time
opt.signcolumn = "yes"

-----------------------------------------------------------
-- Neovim UI
-----------------------------------------------------------
opt.showmatch = true -- Highlight matching parenthesis
opt.foldmethod = 'marker' -- Enable folding (default 'foldmarker')
-- mah
-- for always block cursor
vim.o.guicursor = true

-----------------------------------------------------------
-- Tabs, indent
-----------------------------------------------------------
opt.expandtab = true -- Use spaces instead of tabs
opt.smartindent = true -- Autoindent new lines

-----------------------------------------------------------
-- Memory, CPU
-----------------------------------------------------------
opt.hidden = true -- Enable background buffers
opt.history = 100 -- Remember N lines in history
opt.lazyredraw = true -- Faster scrolling
-- g.syntax = on
-- opt.synmaxcol = 240         -- Max column for syntax highlight
opt.updatetime = 700 -- ms to wait for trigger an event

-----------------------------------------------------------
-- Startup
-----------------------------------------------------------
-- Disable nvim intro
opt.shortmess:append "sI"

-- Disable builtins plugins
local disabled_built_ins = {
    "netrw",
    "netrwPlugin",
    "netrwSettings",
    "netrwFileHandlers",
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
    g["loaded_" .. plugin] = 1
end


-- set termbidi
vim.o.exrc       = true -- so if a vimrc file is in a dir, it's automatically sourced (for custom projects)
vim.o.compatible = false
vim.o.incsearch  = true -- highlight incrementaly rather than the whole word word
vim.o.ignorecase = true
vim.o.smartcase  = true -- only ignore case if write uppercase letter
vim.o.backup     = false
vim.o.undofile   = true
-- vim.o.showmode = false
-- vim.o.signcolumn = true --for linting, lsp error, left column
vim.o.hlsearch   = false -- don't keep the previous search occurance highlighted
vim.o.swapfile   = false

-- vim.wo.relativenumber = true
vim.o.number = true
-- vim.o.scrolloff = 2
opt.termguicolors = true -- Enable 24-bit RGB colors
opt.linebreak = false
vim.bo.tabstop = 4
vim.bo.shiftwidth = 4
opt.shiftwidth = 4
vim.o.hlsearch = false

vim.o.title = true
vim.o.go = a
vim.o.clipboard = "unnamedplus"
vim.o.undofile = true
vim.o.undodir = os.getenv("HOME") .. "/.config/nvim/undodir"

vim.o.encoding = "utf-8"
vim.o.guifont = "hack:h10" -- the font used in graphical neovim applications

-- Automatically change working directory
vim.o.autochdir = true
-- Tab command mode completion
-- vim.o.wildmode = longest,list,full

-- Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
vim.o.splitbelow = true
vim.o.splitright = true

-- Highlight current line
-- vim.o.cursorline = true
--hi cursorline cterm=none term=none
--autocmd WinEnter * setlocal cursorline
--autocmd WinLeave * setlocal nocursorline
-- vim.cmd('highlight guibg=#ffffff guifg=#000000')


-- Disables automatic commenting on newline:
-- autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o


-- set noshowcmd
-- set formatoptions-=o
-- set delcombine                               " Delete part of combining character with x command. Useful for editing Arabic diacritics.
-- set updatetime=50
-- set inccommand=nosplit
-- opt.statusline="%F" --"t (tail -filename-), f relative path, F full path

-- autocomplete maximum items
