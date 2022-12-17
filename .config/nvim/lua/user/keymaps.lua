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
-- map('t', '<C-h>', '<Esc>:wincmd h<CR>')
-- map('t', '<C-j>', '<Esc>:wincmd j<CR>')
-- map('t', '<C-k>', '<Esc>:wincmd k<CR>')
-- map('t', '<C-l>', '<Esc>:wincmd l<CR>')

-- Reload configuration without restart nvim
map('n', '<leader>R', ':so %<CR>')

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
map('n', '<leader>t', ':NvimTreeToggle<CR>') -- open/close
-- map('n', '<leader>d', ':Dired<CR>')            -- open/close
-- map('n', '<leader>f', ':NvimTreeRefresh<CR>')       -- refresh
-- map('n', '<leader>n', ':NvimTreeFindFile<CR>')      -- search file

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

-- map('n', '<leader>H', ':call ToggleHiddenAll()<CR>')

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
-- map('n', '<Leader>m', ':call mkdir(expand("%:p:h"), "p")<CR>')
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
map('n', '<leader>f', "<cmd>lua require('fzf-lua').files()<CR>")
map('n', '<leader>g', "<cmd>lua require('fzf-lua').live_grep()<CR>")
map('n', '<leader>q', "<cmd>lua require('fzf-lua').quickfix()<CR>")
map('n', '<leader>b', "<cmd>lua require('fzf-lua').buffers()<CR>")
map('n', '<leader>H', "<cmd>lua require('fzf-lua').oldfiles()<CR>")
map('n', '<leader>M', "<cmd>lua require('fzf-lua').marks()<CR>")

map('n', '<leader>G', ":Neogit<CR>")
-- map('n', '<leader>pf', ":Ido std.browse<CR>")

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

-- -- keybindings
map('n', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")
map('t', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")
map('i', '<C-t>', "<cmd>ToggleTerm size=10 direction=horizontal<cr>")

-- map('n', '<leader>ta', ":ToDoTxtCapture<CR>")
-- map('n', '<leader>tt', ":ToDoTxtTasksToggle<CR>")
