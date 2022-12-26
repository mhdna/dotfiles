require("toggleterm").setup {
  size = 10,
  open_mapping = [[<c-\>]],
  autochdir = false, -- when neovim changes it current directory the terminal will change it's own when next it's opened
  start_in_insert = true,
  shade_terminals = false, -- NOTE: this option takes priority over highlights specified so if you specify Normal highlights you should set this to false
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
  persist_size = true,
  persist_mode = true, -- if set to true (default) the previous terminal mode will be remembered
  direction = 'tab',
  close_on_exit = true, -- close the terminal window when the process exits
  shell = vim.o.shell, -- change the default shell
  auto_scroll = false, -- automatically scroll to the bottom on terminal output
}
