test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish
set -x PATH /usr/local/opt/openssl/include /Applications/Postgres.app/Contents/Versions/latest/bin ~/bin /Users/Justin/.cargo/bin /Users/Justin/Documents/Workspace /usr/local/bin /Users/Justin/Library/$HOMEnt/bin /Users/Justin/.rbenv/shims $PATH

export RUST_SRC_PATH="~/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
export GTAGSLIBPATH=$HOME/.gtags/

export AWS_REGION="ca-central-1"
export RUSTC="/Users/Justin/.cargo/bin/rustc"
export EDITOR="/usr/local/bin/emacs"

source ~/.config/fish/secrets/work.fish
source ~/.config/fish/secrets/personal.fish

# Source chtf
if test -f /usr/local/share/chtf/chtf.fish
    source /usr/local/share/chtf/chtf.fish
end

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.fish
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish ]; and . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.fish
