local utils              = require('utils')

vim.opt.signcolumn       = "yes" --for linting, diagnostics, left column
vim.o.mouse              = "a"
vim.o.colorcolumn        = 80    -- don't exceed 80 columns, maybe you're in the 3rd 4th indent so rethink
-- vim.opt.laststatus = 0
-- vim.opt.showtabline      = 2
vim.opt.pumheight        = 15
vim.opt.pumwidth         = 15
-- vim.opt.foldmethod = 'marker' -- Enable folding (default 'foldmarker')
-- always use a block shaped cursor
vim.opt.expandtab        = true -- Use spaces instead of tabs
vim.opt.smartindent      = true -- Autoindent new lines
vim.opt.hidden           = true -- Enable background buffers
vim.opt.history          = 100  -- Remember N lines in history
vim.opt.lazyredraw       = true -- Faster scrolling
-- g.syntax = on
-- opt.synmaxcol = 240         -- Max column for syntax highlight
vim.opt.updatetime       = 700 -- ms to wait for trigger an event

-- vim.o.exrc       = true -- so if a vimrc file is in a dir, it's automatically sourced (for custom projects)
-- vim.o.colorcolumn = '95'
vim.o.compatible         = false
vim.o.incsearch          = true  -- highlight incrementaly rather than the whole word word
vim.o.ignorecase         = true  -- only ignore case if write uppercase letter
vim.o.smartcase          = true  -- only ignore case if write uppercase letter
-- vim.o.showmode = false
vim.o.hlsearch           = false -- Do not keep the previous search occurance highlighted
vim.o.swapfile           = false

vim.o.number             = true
vim.wo.relativenumber    = true
vim.o.scrolloff          = 8
vim.o.guicursor          = true
vim.opt.linebreak        = false
vim.bo.tabstop           = 4
vim.opt.shiftwidth       = 4

vim.o.title              = true
vim.o.go                 = a
vim.o.clipboard          = "unnamedplus"
vim.o.undofile           = true
vim.o.undodir            = os.getenv("XDG_CACHE_HOME") .. "/nvim/undodir/"
vim.o.backupdir          = os.getenv("XDG_CACHE_HOME") .. "/nvim/backup/"
vim.o.backup             = true
vim.o.encoding           = "utf-8"
-- vim.o.statusline         = "%<%F %h%m%r%=%-14.(%l,%c%V%) %P"

-- Spellchecking
-- vim.opt.spelllang        = 'en_us'
-- vim.opt.spell            = true
-- vim.o.autochdir = true
-- Tab command mode completion
-- vim.o.wildmode           = longest, list, full

-- Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
vim.o.splitbelow         = true
vim.o.splitright         = true

-- vim.o.cursorline         = true -- Highlight current line
-- vim.o.cursorcolumn = true -- Highlight current column
-- set formatoptions-=o
-- set updatetime=50
-- set inccommand=nosplit

-- better netrw
vim.g.netrw_browse_split = 0
vim.g.netrw_banner       = 0
vim.opt.termguicolors    = true -- Enable 24-bit RGB colors
vim.g.netrw_winsize      = 25

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

-- Providers
vim.g.loaded_perl_provider = 0      -- Disable perl provider
vim.g.loaded_ruby_provider = 0      -- Disable ruby provider
vim.g.loaded_node_provider = 0      -- Disable node provider
vim.g.did_install_default_menus = 1 -- do not load menu

if utils.executable('python3') then
    if vim.g.is_win then
        vim.g.python3_host_prog = vim.fn.substitute(fn.exepath("python3"), ".exe$", '', 'g')
    else
        vim.g.python3_host_prog = vim.fn.exepath("python3")
    end
else
    api.nvim_err_writeln("Python3 executable not found! You must install Python3 and set its PATH correctly!")
    return
end
