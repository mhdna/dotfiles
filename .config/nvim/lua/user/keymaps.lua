-----------------------------------------------------------
-- Define maps of Neovim and installed plugins.
-----------------------------------------------------------
-- Comment
require('Comment').setup()

-- local wiki = os.getenv("HOME") .. "/dox/wiki/QuickNote.md"
-- local college = os.getenv("HOME") .. "/dox/college"

local function map(mode, lhs, rhs, opts)
    local options = { noremap = true, silent = true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

vim.cmd([[
"Arabic switch
func! ToggleArabic()
if &rl
	set norl
	set keymap=
else
	set rl
  set keymap=arabic "Modified keymap. File in .vim/keymap
  set encoding=utf-8
  set arabicshape
	" set nospell
end
endfunc

func! TogglePersian()
if &rl
	set norl
	set keymap=
else
	set rl
  set keymap=persian "Modified keymap. File in .vim/keymap
	set nospell

end
endfunc

]])
--[[ -- Arabic switch
function ToggleArabic()
if vim.g.rl == true then
   vim.g.rl = true
   vim.g.keymap=''
else
   vim.g.keymap='arabic'
   vim.g.nospell= true
end
--]]

-- Change leader to a comma
vim.g.mapleader = ' '

-----------------------------------------------------------
-- Neovim shortcuts
-----------------------------------------------------------

-- Disable arrow keys
map('', '<up>', '<nop>')
map('', '<down>', '<nop>')
map('', '<left>', '<nop>')
map('', '<right>', '<nop>')

-- Map Esc to kk
--map('i', 'kk', '<Esc>')

-- Clear search highlighting with <leader> and c
map('n', '<leader>c', ':nohl<CR>')

-- Change split orientation
-- map('n', '<leader>tk', '<C-w>t<C-w>K') -- change vertical to horizontal
-- map('n', '<leader>th', '<C-w>t<C-w>H') -- change horizontal to vertical

-- Move around splits using Ctrl + {h,j,k,l}
map('', '<C-h>', '<C-w>h')
map('', '<C-j>', '<C-w>j')
map('', '<C-k>', '<C-w>k')
map('', '<C-l>', '<C-w>l')

-- Reload configuration without restart nvim
map('n', '<leader>r', ':so %<CR>')

-- Fast saving with <leader> and s
map('n', '<leader>w', ':w<CR>')
--map('i', '<leader>s', '<C-c>:w<CR>')

-- Close all windows and exit from Neovim with <leader> and q
-- map('n', '<leader>q', ':qa!<CR>')

-----------------------------------------------------------
-- Applications and Plugins shortcuts
-----------------------------------------------------------

-- Terminal mappings
map('t', '<Esc>', '<C-\\><C-n>') -- exit

-- NvimTree
map('n', '<leader>f', ':NvimTreeToggle<CR>') -- open/close
-- map('n', '<leader>d', ':Dired<CR>')            -- open/close
-- map('n', '<leader>f', ':NvimTreeRefresh<CR>')       -- refresh
map('n', '<leader>n', ':NvimTreeFindFile<CR>')      -- search file

-- Tagbar
-- map('n', '<leader>z', ':TagbarToggle<CR>') -- open/close

--map ('n', '<leader>g', ':Goyo \| set linebreak \| :hi Normal ctermbg=NONE guibg=NONE<CR><CR>', {silent=true})

map('n', 'Q', 'gq')

map('n', '<leader>o', ':setlocal spell! spelllang=en_us<CR>')

map('n', '<c-s>', ':w<CR>')
map('i', '<c-s>', '<Esc>:w<CR>a')
-- map ('n', '<C-p>', ':GFiles<CR>')
-- map ('n', '<Leader>w', ':e $WIKI<CR>')
-- map ('n', '<Leader>h', ':History<CR>')
-- map ('n', '<Leader>b', ':Buffers<CR>')

--map ('n', '<Leader>ga', ':G<Space>add . %:p:h<CR>')
--map ('n', '<Leader>gc', ':G<Space>commit -m 'on "date +'%Y-%m-%d %H:%M:%S'`-- . %:p:h<CR>')
--cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

--autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<CR>
--autocmd BufRead,BufNewFile /tmp/neomutt* map ZQ :Goyo\|q!<CR>

map('n', '<leader>H', ':call ToggleHiddenAll()<CR>')

-- xnoremap <silent> gP "+P

map('n', 'n', 'nzzzv')
map('n', 'N', 'Nzzzv')
-- map ('n', '<leader>d', '"_d')
-- map ('v', '<leader>d', '"_d')

map('n', 'J', 'mzJ`z')
map('n', '<Leader>u', ':UndotreeToggle<CR>:UndotreeFocus<CR>')

map('n', '<leader>s', ':setlocal spell! spelllang=en_us<CR>')

map('v', 'vmap', '<silent><buffer> <CR> :MiniMDTaskToggle<CR>')

map('n', '<silent><buffer>', '= :MiniMDPromote<CR>')


-- Perform dot commands over visual blocks
map('v', '.', ':normal .<CR>')

-- map ('n', '<leader>pp', ':!cd $PWD  && git add .&& git commit -m "generated files on `date +\'%Y-%m-%d %H:%M:%S\'`"&& git push -u origin main<CR>')
-- map ('n', '<leader>PP', ':!cd $PWD  && git pull && git merge<CR>')
map('n', '<F1>', ':call ToggleArabic()<CR>')
map('i', '<F1>', '<Esc>l:call ToggleArabic()<CR>i')
map('v', '<F1>', '<Esc>l:call ToggleArabic()<CR>i')
map('n', '<S-F1>', ':call TogglePersian()<CR>')
map('v', '<F2>', '<Esc>l:call TogglePersian()<CR>i')
map('i', '<F2>', '<Esc>l:call TogglePersian()<CR>i')
map('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>')
map('n', '<Up>', '<c-w>+')
map('n', '<Down>', '<c-w>-')
map('n', '<Left>', '<c-w><')
map('n', '<Right>', '<c-w>>')

map('n', '<C-h>', '<C-w>h')
map('n', '<C-j>', '<C-w>j')
map('n', '<C-k>', '<C-w>k')
map('n', '<C-l>', '<C-w>l')


map('n', '!', ':!')


map('n', 'S', ':%s//g<Left><Left>')

map('n', '<leader>C', ':w! | !compiler "<c-r>%"<CR>')
--autocmd filetype cpp nnoremap <Leader>c :w! \| :split \| term g++ % -o %< && ./%< <CR>i
--autocmd filetype java nnoremap <Leader>c :w! \| :split \| term java % <CR>


map('n', '<leader>o', ':!opout <c-r>%<CR><CR>')

-- snippets
--map ('i', 'ج"', "<Left><Del>چ")
--map ('i', 'ب"', "<Left><Del>پ")
--map ('i', 'ك"', "<Left><Del>گ")
--map ('i', 'ز"', "<Left><Del>ژ")


-- Telescope
-- map ( 'n',  '<leader>pf', '<cmd>Telescope find_files<cr>')
-- map ( 'n', '<leader>pg', '<cmd>Telescope live_grep<cr>')
-- map ('n', '<leader>pb', '<cmd>Telescope buffers<cr>')
-- map ('n', '<leader>ph', '<cmd>Telescope help_tags<cr>')

-- snippets
-- let g:UltiSnipsExpandTrigger="<tab>"
-- let g:UltiSnipsJumpForwardTrigger="<c-f>"
-- let g:UltiSnipsJumpBackwardTrigger="<c-b>"
--
-- " If you want :UltiSnipsEdit to split your window.
-- let g:UltiSnipsEditSplit="vertical"
-- let g:UltiSnipsSnippetDirectories=[$HOME.'/.config/nvim/plug/ultisnips/UltiSnips']
--

-- " press <Tab> to expand or jump in a snippet. These can also be mapped separately
-- " via <Plug>luasnip-expand-snippet and <Plug>luasnip-jump-next.
-- map('n', '<Tab>', 'luasnip#expand_or_jumpable() ? "<Plug>luasnip-expand-or-jump" : "<Tab>"')

-- " -1 for jumping backwards.
map('n', '<A-n>', ':bn<CR>')
map('n', '<A-p>', ':bp<CR>')

-- snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
-- snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>

-- " For changing choices in choiceNodes (not strictly necessary for a basic setup).
-- map('i', '<C-E>', 'luasnip#choice_active() ? "<Plug>luasnip-next-choice" : "<C-E>"')
-- smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'


-- command line mode
--map('c', '<C-A>', '<Home>')
--map('c', '<C-f>', '<Right>')
--map('c', '<A-f>', '<S-Right>')
--map('c', '<C-b>', '<Left>')
--map('c', '<A-b>', '<S-Left>')

-- Plugins
-- Fzf
-- map('n', '<leader>pr', ":Ido std.recent_files<CR>")
-- map('n', '<leader>pb', ":Ido std.buffer<CR>")
map('n', '<C-p>', "<cmd>lua require('fzf-lua').files()<CR>")
map('n', '<leader>pg', "<cmd>lua require('fzf-lua').live_grep()<CR>")
map('n', '<leader>pq', "<cmd>lua require('fzf-lua').quickfix()<CR>")
map('n', '<leader>pb', "<cmd>lua require('fzf-lua').buffers()<CR>")
map('n', '<leader>ph', "<cmd>lua require('fzf-lua').oldfiles()<CR>")
map('n', '<leader>pm', "<cmd>lua require('fzf-lua').marks()<CR>")
-- map('n', '<leader>pf', ":Ido std.browse<CR>")

-- His stuff
-- local opts = { noremap = true, silent = true }
--
-- local term_opts = { silent = true }
--
-- -- Shorten function name
-- local keymap = vim.api.nvim_set_keymap
--
-- --Remap space as leader key
-- keymap("", "<Space>", "<Nop>", opts)
-- vim.g.mapleader = " "
-- vim.g.maplocalleader = " "
--
-- -- Modes
-- --   normal_mode = "n",
-- --   insert_mode = "i",
-- --   visual_mode = "v",
-- --   visual_block_mode = "x",
-- --   term_mode = "t",
-- --   command_mode = "c",
--
-- -- Normal --
-- -- Better window navigation
-- keymap("n", "<C-h>", "<C-w>h", opts)
-- keymap("n", "<C-j>", "<C-w>j", opts)
-- keymap("n", "<C-k>", "<C-w>k", opts)
-- keymap("n", "<C-l>", "<C-w>l", opts)
--
-- -- Resize with arrows
-- keymap("n", "<C-Up>", ":resize -2<CR>", opts)
-- keymap("n", "<C-Down>", ":resize +2<CR>", opts)
-- keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
-- keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)
--
-- -- Navigate buffers
-- keymap("n", "<S-l>", ":bnext<CR>", opts)
-- keymap("n", "<S-h>", ":bprevious<CR>", opts)
--
-- -- Move text up and down
-- keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
-- keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
--
-- -- Insert --
-- -- Press jk fast to exit insert mode
-- keymap("i", "jk", "<ESC>", opts)
--
-- -- Visual --
-- -- Stay in indent mode
-- keymap("v", "<", "<gv", opts)
-- keymap("v", ">", ">gv", opts)
--
-- -- Move text up and down
-- keymap("v", "<A-j>", ":m .+1<CR>==", opts)
-- keymap("v", "<A-k>", ":m .-2<CR>==", opts)
-- keymap("v", "p", '"_dP', opts)
--
-- -- Visual Block --
-- -- Move text up and down
-- keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
-- keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
-- keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
-- keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)
--
-- -- Terminal --
-- -- Better terminal navigation
-- -- keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
-- -- keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
-- -- keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
-- -- keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)
--
-- -- local opts2 = {
-- --   mode = "n", -- NORMAL mode
-- --   prefix = "<leader>",
-- --   buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
-- --   silent = true, -- use `silent` when creating keymaps
-- --   noremap = true, -- use `noremap` when creating keymaps
-- --   nowait = true, -- use `nowait` when creating keymaps
-- -- }
-- --
-- -- local mappings = {
-- --   ["a"] = { "<cmd>Alpha<cr>", "Alpha" },
-- --   ["b"] = {
-- --     "<cmd>lua require('telescope.builtin').buffers(require('telescope.themes').get_dropdown{previewer = false})<cr>",
-- --     "Buffers",
-- --   },
-- --   ["e"] = { "<cmd>NvimTreeToggle<cr>", "Explorer" },
-- --   ["w"] = { "<cmd>w!<CR>", "Save" },
-- --   ["q"] = { "<cmd>q!<CR>", "Quit" },
-- --   ["c"] = { "<cmd>Bdelete!<CR>", "Close Buffer" },
-- --   ["h"] = { "<cmd>nohlsearch<CR>", "No Highlight" },
-- --   ["f"] = {
-- --     "<cmd>lua require('telescope.builtin').find_files(require('telescope.themes').get_dropdown{previewer = false})<cr>",
-- --     "Find files",
-- --   },
-- --   ["F"] = { "<cmd>Telescope live_grep theme=ivy<cr>", "Find Text" },
-- --   ["P"] = { "<cmd>lua require('telescope').extensions.projects.projects()<cr>", "Projects" },
-- --
-- --   p = {
-- --     name = "Packer",
-- --     c = { "<cmd>PackerCompile<cr>", "Compile" },
-- --     i = { "<cmd>PackerInstall<cr>", "Install" },
-- --     s = { "<cmd>PackerSync<cr>", "Sync" },
-- --     S = { "<cmd>PackerStatus<cr>", "Status" },
-- --     u = { "<cmd>PackerUpdate<cr>", "Update" },
-- --   },
-- --
-- --   g = {
-- --     name = "Git",
-- --     g = { "<cmd>lua _LAZYGIT_TOGGLE()<CR>", "Lazygit" },
-- --     j = { "<cmd>lua require 'gitsigns'.next_hunk()<cr>", "Next Hunk" },
-- --     k = { "<cmd>lua require 'gitsigns'.prev_hunk()<cr>", "Prev Hunk" },
-- --     l = { "<cmd>lua require 'gitsigns'.blame_line()<cr>", "Blame" },
-- --     p = { "<cmd>lua require 'gitsigns'.preview_hunk()<cr>", "Preview Hunk" },
-- --     r = { "<cmd>lua require 'gitsigns'.reset_hunk()<cr>", "Reset Hunk" },
-- --     R = { "<cmd>lua require 'gitsigns'.reset_buffer()<cr>", "Reset Buffer" },
-- --     s = { "<cmd>lua require 'gitsigns'.stage_hunk()<cr>", "Stage Hunk" },
-- --     u = {
-- --       "<cmd>lua require 'gitsigns'.undo_stage_hunk()<cr>",
-- --       "Undo Stage Hunk",
-- --     },
-- --     o = { "<cmd>Telescope git_status<cr>", "Open changed file" },
-- --     b = { "<cmd>Telescope git_branches<cr>", "Checkout branch" },
-- --     c = { "<cmd>Telescope git_commits<cr>", "Checkout commit" },
-- --     d = {
-- --       "<cmd>Gitsigns diffthis HEAD<cr>",
-- --       "Diff",
-- --     },
-- --   },
-- --
-- --   l = {
-- --     name = "LSP",
-- --     a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Code Action" },
-- --     d = {
-- --       "<cmd>Telescope lsp_document_diagnostics<cr>",
-- --       "Document Diagnostics",
-- --     },
-- --     w = {
-- --       "<cmd>Telescope lsp_workspace_diagnostics<cr>",
-- --       "Workspace Diagnostics",
-- --     },
-- --     f = { "<cmd>lua vim.lsp.buf.format{async=true}<cr>", "Format" },
-- --     i = { "<cmd>LspInfo<cr>", "Info" },
-- --     I = { "<cmd>LspInstallInfo<cr>", "Installer Info" },
-- --     j = {
-- --       "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>",
-- --       "Next Diagnostic",
-- --     },
-- --     k = {
-- --       "<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>",
-- --       "Prev Diagnostic",
-- --     },
-- --     l = { "<cmd>lua vim.lsp.codelens.run()<cr>", "CodeLens Action" },
-- --     q = { "<cmd>lua vim.lsp.diagnostic.set_loclist()<cr>", "Quickfix" },
-- --     r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
-- --     s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
-- --     S = {
-- --       "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
-- --       "Workspace Symbols",
-- --     },
-- --   },
-- --   s = {
-- --     name = "Search",
-- --     b = { "<cmd>Telescope git_branches<cr>", "Checkout branch" },
-- --     c = { "<cmd>Telescope colorscheme<cr>", "Colorscheme" },
-- --     h = { "<cmd>Telescope help_tags<cr>", "Find Help" },
-- --     M = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
-- --     r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
-- --     R = { "<cmd>Telescope registers<cr>", "Registers" },
-- --     k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
-- --     C = { "<cmd>Telescope commands<cr>", "Commands" },
-- --   },
-- --
-- --   t = {
-- --     name = "Terminal",
-- --     n = { "<cmd>lua _NODE_TOGGLE()<cr>", "Node" },
-- --     u = { "<cmd>lua _NCDU_TOGGLE()<cr>", "NCDU" },
-- --     t = { "<cmd>lua _HTOP_TOGGLE()<cr>", "Htop" },
-- --     p = { "<cmd>lua _PYTHON_TOGGLE()<cr>", "Python" },
-- --     f = { "<cmd>ToggleTerm direction=float<cr>", "Float" },
-- --     h = { "<cmd>ToggleTerm size=10 direction=horizontal<cr>", "Horizontal" },
-- --     v = { "<cmd>ToggleTerm size=80 direction=vertical<cr>", "Vertical" },
-- --   },
-- -- }
-- --
-- -- setup(setup)
-- -- register(mappings, opts2)

-- java stuff
--[[ map ("n", "<A-o>", "<Cmd>lua require'jdtls'.organize_imports()<CR>")  ]]
--[[ map ("n", "crv", "<Cmd>lua require('jdtls').extract_variable()<CR>")  ]]
--[[ vnoremap crv <Esc><Cmd>lua require('jdtls').extract_variable(true)<CR> ]]
--[[ map ("n", "crc", "<Cmd>lua require('jdtls').extract_constant()<CR>")  ]]
--[[ vnoremap crc <Esc><Cmd>lua require('jdtls').extract_constant(true)<CR> ]]
--[[ vnoremap crm <Esc><Cmd>lua require('jdtls').extract_method(true)<CR> ]]
--[[]]
--[[]]
--[[ -- If using nvim-dap ]]
--[[ -- This requires java-debug and vscode-java-test bundles, see install steps in this README further below. ]]
--[[ nnoremap <leader>df <Cmd>lua require'jdtls'.test_class()<CR> ]]
--[[ nnoremap <leader>dn <Cmd>lua require'jdtls'.test_nearest_method()<CR> ]]


-- Plugins
-- todo
-- map('n', '<leader>t', ":TodoQuickFix<Cr>", { noremap = true, silent = true })

-- easymotion
-- map('n', '<C-s>', '<Plug>(easymotion-overwin-f)')
-- map ('n', '<C-j>', '<Plug>(easymotion-j)')
-- map ('n', '<C-k>', '<Plug>(easymotion-k)')
-- map ('n', '<C-s>', '<Plug>(easymotion-overwin-f2)')

-- keybindings
-- map('n', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")
-- map('t', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")
-- map('i', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")


-- map('n', '<leader>ta', ":ToDoTxtCapture<CR>")
-- map('n', '<leader>tt', ":ToDoTxtTasksToggle<CR>")
