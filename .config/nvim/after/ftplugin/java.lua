local jdtls_ok, jdtls = pcall(require, "jdtls")
if not jdtls_ok then
    return
end

-- Installation location of jdtls by nvim-lsp-installer
local JDTLS_LOCATION = vim.fn.stdpath "data" .. "/mason/packages/jdtls"

local home = os.getenv "HOME"

-- Only for Linux and Mac
local SYSTEM = "linux"
if vim.fn.has "mac" == 1 then
    SYSTEM = "mac"
end

local root_markers = { ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" }
local root_dir = require("jdtls.setup").find_root(root_markers)
if root_dir == "" then
    return
end
local workspace_dir = home .. "/.local/share/eclipse/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")


local extendedClientCapabilities = jdtls.extendedClientCapabilities
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true

local bufopts = { noremap = true, silent = true, buffer = bufnr }
vim.keymap.set("n", "<leader>c", ":w! | :split | :term java % <CR>", bufopts)
vim.keymap.set("n", "<leader>S", ":so ~/.config/nvim/ftplugin/java.lua<CR>", bufopts)
-- autocmd filetype java nnoremap <leader>c :w! | :split | :term cd $pwd ; !javac *.java ; java Tester <CR>

local on_attach = function(client, bufnr)
    jdtls.setup.add_commands()
    require("jdtls").setup_dap()
    require('jdtls.dap').setup_dap_main_class_configs()

    -- Use an on_attach function to only map the following keys
    -- after the language server attaches to the current buffer
    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', '<leader>li', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<M-K>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<leader>lwl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<leader>ld', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format, bufopts)

    -- Java extensions
    vim.keymap.set("n", "<leader>lo", jdtls.organize_imports, bufopts, "Organize imports")
    vim.keymap.set("n", "<leader>lt", jdtls.test_class, bufopts, "Test class (DAP)")
    vim.keymap.set("n", "<leader>lT", jdtls.test_nearest_method, bufopts, "Test method (DAP)")
    vim.keymap.set("n", "<leader>lev", jdtls.extract_variable, bufopts, "Extract variable")
    vim.keymap.set("n", "<leader>lec", jdtls.extract_constant, bufopts, "Extract constant")
    vim.keymap.set("v", "<leader>lem", [[<ESC><CMD>lua require('jdtls').extract_method(true)<CR>]], bufopts,
        "Extract method")
end


local bundles = {
    vim.fn.glob(
        home ..
        "/.local/share/nvim/mason/packages/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar",
        1),
}

local extra_bundles = vim.split(
    vim.fn.glob(home .. "/.local/share/nvim/mason/packages/java-test/extension/server/*.jar", 1), "\n")
vim.list_extend(bundles, extra_bundles)


local config = {
    cmd = {
        "java",
        "-Declipse.application=org.eclipse.jdt.ls.core.id1",
        "-Dosgi.bundles.defaultStartLevel=4",
        "-Declipse.product=org.eclipse.jdt.ls.core.product",
        "-Dlog.protocol=true",
        "-Dlog.level=ALL",
        "-Xms1g",
        "-javaagent:" .. home .. "/.local/share/nvim/mason/packages/jdtls/lombok.jar",
        "--add-modules=ALL-SYSTEM",
        "--add-opens",
        "java.base/java.util=ALL-UNNAMED",
        "--add-opens",
        "java.base/java.lang=ALL-UNNAMED",
        "-jar",
        vim.fn.glob(JDTLS_LOCATION .. "/plugins/org.eclipse.equinox.launcher_*.jar"),
        "-configuration",
        JDTLS_LOCATION .. "/config_" .. SYSTEM,
        "-data",
        workspace_dir,
    },
    on_attach = on_attach,

    root_dir = root_dir,
    -- capabilities = require("lvim.lsp").common_capabilities(),
    settings = {
        java = {
            eclipse = {
                downloadSources = true,
            },
            configuration = {
                updateBuildConfiguration = "interactive",
            },
            maven = {
                downloadSources = true,
            },
            implementationsCodeLens = {
                enabled = true,
            },
            referencesCodeLens = {
                enabled = true,
            },
            references = {
                includeDecompiledSources = true,
            },
            inlayHints = {
                parameterNames = {
                    enabled = "all", -- literals, all, none
                },
            },
            format = {
                enabled = true,
                -- settings = {
                --     -- Use Google Java style guidelines for formatting
                --     -- To use, make sure to download the file from https://github.com/google/styleguide/blob/gh-pages/eclipse-java-google-style.xml
                --     -- and place it in the ~/.local/share/eclipse directory
                --     url = "~/.local/share/eclipse/eclipse-java-google-style.xml",
                --     profile = "GoogleStyle",
                -- },
            },
        },
        signatureHelp = { enabled = true },
        completion = {
            favoriteStaticMembers = {
                "org.hamcrest.MatcherAssert.assertThat",
                "org.hamcrest.Matchers.*",
                "org.hamcrest.CoreMatchers.*",
                "org.junit.jupiter.api.Assertions.*",
                "java.util.Objects.requireNonNull",
                "java.util.Objects.requireNonNullElse",
                "org.mockito.Mockito.*",
            },
        },
        contentProvider = { preferred = "fernflower" },
        extendedClientCapabilities = extendedClientCapabilities,
        sources = {
            organizeImports = {
                starThreshold = 9999,
                staticStarThreshold = 9999,
            },
        },
        codeGeneration = {
            toString = {
                template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
            },
            useBlocks = true,
        },
    },

    flags = {
        allow_incremental_sync = true,
    },
    on_init = function(client)
        client.notify('workspace/didChangeConfiguration', { settings = client.config.settings })
    end,
    init_options = {
        bundles = bundles
    },
}



vim.cmd(
    "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_compile JdtCompile lua require('jdtls').compile(<f-args>)"
)
vim.cmd(
    "command! -buffer -nargs=? -complete=custom,v:lua.require'jdtls'._complete_set_runtime JdtSetRuntime lua require('jdtls').set_runtime(<f-args>)"
)
vim.cmd("command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()")
-- vim.cmd "command! -buffer JdtJol lua require('jdtls').jol()"
vim.cmd("command! -buffer JdtBytecode lua require('jdtls').javap()")

require("jdtls").start_or_attach(config)
