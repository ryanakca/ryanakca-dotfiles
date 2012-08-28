#
# ~/.zshrc
# Z shell configuration file.
#                                                                        CREDITS
# File was first taken from the following location:
#
#   http://www.tomaw.net/configs/zshrc
#
# Some additions were made by me, Jonathan Patrick Davies <jpds@ubuntu.com>.
# Among these are the Launchpad, Debian, Wikipedia/Wikitravel, BBC search,
# Demonoid, Lonely Planet, Urbandictionary and other various functions. I also
# implemented a few "if" statements to stop zsh complaining about any missing
# objects and placed the dircolors support.
#
# The forkex() function is thanks to Siegfried-Angel Gevatter Pujals
#   <rainct@ubuntu.com>.
#
# Some of this file is thanks to <slarti@gentoo.org>, some is take from
# spider's <spider@gentoo.org> zshrc, and some from the zshwiki.org.
# Some bash functions are nicked from ciaranm's <ciaranm@gentoo.org>
# bashrc.
#
# READ ME
#
# • Remember to change the stuff specific to me! It's all at the top of
#   this file.
#
# • You can obviously only get the most out of this file if you take the
#   time to read through the comments. Of course, you can still experience
#   zsh's superiority by simply plugging this file in and using it.
#

# BEGIN LOCAL

export LC_ALL="LOCALE"
export LANG="LOCALE"
export TZ="America/Toronto"             # Force our time zone this location.
export EDITOR="vim"                     # Long live vim (as our editor).
export NAME="Ryan Kavanagh"             # Our name.
export EMAIL="rak@debian.org"     # Our email address.
export GPGKEY1="E95EDDC9"               # Our old GnuPG key ID.
export GPGKEY="4A11C97A"                # Our GnuPG key ID. 
export DEBFULLNAME=$NAME                # These are used by Debian packaging...
export DEBEMAIL=$EMAIL                  # ...programs.
export DEBSIGN_KEYID=$GPGKEY            # Key ID for signing Debian packages.
export GIT_AUTHOR_NAME=$NAME            # Use our real name for Git.
export GIT_AUTHOR_EMAIL=$EMAIL
export GIT_COMMITTER_NAME=$GIT_AUTHOR_NAME
export GIT_COMMITTER_EMAIL=$GIT_AUTHOR_EMAIL
export BZR_EMAIL="$NAME <$EMAIL>"       # Override email for Bazaar.
export GIT_AUTHOR_NAME=$NAME
export DARCS_EMAIL="$NAME <$EMAIL>"
export GREP_OPTIONS="--color=auto"
export QUILT_PATCHES=debian/patches
export PATH=/usr/lib/ccache:/usr/lib/distcc/bin:$HOME/bin:/sbin:$PATH
export CCACHE_DIR=/ccache
#export HTTP_PROXY="http://localhost:3128/"
export MANWIDTH=80
export MANOPT="-L en"
export BTS_SENDMAIL_COMMAND="/usr/bin/msmtp"
export PYTHONSTARTUP=~/.pythonrc.py
export PAGER=less
export PDFVIEWER=evince
export TEXMFHOME=/home/ryan/.texmf
export LESSCHARSET=utf-8                # Needed for cyrillic &c in less

alias apt-upgrade='sudo apt-get update && sudo aptitude dist-upgrade'
alias emacs='/usr/bin/emacs -nw'
alias dbuild='GPG_TTY=$(tty) debuild -S -sa -k$GPGKEY'
alias itp='reportbug -M -B debian --email rak@debian.org --paranoid -K $GPGKEY wnpp'
alias sneezymud='nc play.sneezymud.com 7900'
alias news='tin -g  pqnews.cogeco.ca'
alias bbcr1='mplayer mms://wmlive.bbc.net.uk/wms/bbc_ami/radio1/radio1_bb_live_int_eq1_sl1'
alias bbcr2='mplayer mms://wmlive.bbc.net.uk/wms/bbc_ami/radio2/radio2_bb_live_int_eq1_sl1'
alias bbcr3='mplayer mms://wmlive.bbc.net.uk/wms/bbc_ami/radio3/radio3_bb_live_int_eq1_sl1'
alias rtvec='mplayer -playlist http://radioclasica.rtve.stream.flumotion.com/rtve/radioclasica.mp3.m3u'
alias rtve3='mplayer -playlist http://radio3.rtve.stream.flumotion.com/rtve/radio3.mp3.m3u'
alias rtve5='mplayer -playlist http://radio5.rtve.stream.flumotion.com/rtve/radio5.mp3.m3u'
alias am740='mplayer -playlist http://provisioning.streamtheworld.com/asx/cfzmam.asx'
alias 1920s='mplayer -playlist http://64.5.130.43/listen.pls'
alias 1940s='mplayer http://s2.fastcast4u.com:10102/'
alias dismuke='`curl -s http://www.loudcity.com/stations/radio-dismuke | grep --color=never launch | grep --color=never ram | tail -n 1 | sed -e "s@.*href=.@mplayer -playlist http://www.loudcity.com@g;s@. class.*@@g"`'
alias up-theme='rsync -avz --no-p --no-o --no-times -e ssh ~/work/kubuntu-theme-v2/* ryanak.ca:/home/ryan/kubuntu-theme-v2/'
alias startxkde4='startx -nolisten tcp -- :0 &'
alias irssi-notify='ssh -f ryanak.ca -L 2227:127.0.0.1:2227 -N && irssi-notifier &'
alias gpg='gpg-wrapper'
alias i2e='/usr/bin/i2e-cli'
alias mplayer-fb='mplayer -vo fbdev'
alias links2-fb='links2 -driver fb'
alias bzbuild='bzr builddeb -S -- -sa -k$GPGKEY'
alias svbuildi='svn-buildpackage --svn-ignore-new --svn-builder="debuild -S -sa -k$GPGKEY"'
alias svbuild='svn-buildpackage --svn-builder="debuild -S -sa -k$GPGKEY"'
alias gibuild='git-buildpackage --git-builder="sbuild -sAd u"'
alias gibuildi='git-buildpackage --git-ignore-new --git-builder="sbuild -sAd u"'
#alias wtau='wakeonlan 00:0d:56:1b:7a:f0'
alias wtau='sudo etherwake tau'
alias pology='python $HOME/work/pology/scripts/posieve.py'
alias daylog='dch --changelog /home/ryan/work/mcgill/drafts/daylog/daylog'
alias dquilt="quilt --quiltrc=${HOME}/.quiltrc-dpkg"
alias kbd="xkbcomp -I$HOME/.xkb $HOME/.xkb/keymap/icd $DISPLAY"
alias vi='vim'
alias sm='screen -r mail || screen -c ${HOME}/.screenrc-mail'
alias remote_3051='ssh -f -N -q -L 6301:192.168.1.207:631 ryan@ryanak.ca'

# END LOCAL

fpath=($fpath
        /home/ryan/.zen/zsh/scripts
        /home/ryan/.zen/zsh/zle)
autoload -U zen

# Ensure that we possess a ~/.zsh/ directory. This is required for the
# ~/.zsh/history file.
if [ ! -d $HOME/.zsh/ ]; then
    mkdir -p $HOME/.zsh/
fi

# Alias/custom commands
#
# Many of these options don's exist on BSD rm/cp/mkdir/ln/etc.
#
# Some are just in case - for 'rm', 'cp' and 'mv' - ask about overwriting or
# deleting files.
# Furthermore, be verbose about what each command is performing to be present of
# what is occuring every time.
if [[ `uname` = "Linux" ]]; then
    alias cp="cp -iv"
    alias mkdir="mkdir -v"
    alias mv="mv -iv"
    alias ln="ln -v"
    # Only delete files on the current file system to avoid removing recursively
    # from bind mounts.
    alias rm="SUBSTS_RM -iv --one-file-system"

    alias chown="chown -v"
    alias chmod="chmod -v"
fi

# Cause encfs unmount a mounted encrypted partition after twenty minutes of
# inactivity by default.
alias encfs="encfs --idle=20"

fpath=($fpath $HOME/.zsh/func)

# Prompt theme.
autoload -U promptinit; promptinit
# prompt walters

# Colours.
autoload -U colors; colors
setopt promptsubst

# Give us a prompt along the lines of:
#
#  jpds@topr>
#
#export PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}%\> "

# Change word boundary characters. Nabbed from
# http://zshwiki.org/KeyBindings.

# By default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# We remove the slash, period, angle brackets, dash here.
export WORDCHARS='*?_-[]~=&;!#$%^(){}'

# Follow GNU LS_COLORS for completion menus
zmodload -i zsh/complist

# Should dircolors exist. Fetch LS_COLORS from it.
if [ "`which dircolors`" != 'dircolors not found' ]; then
    eval "$(dircolors -b)"
fi

zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:kill:*' list-colors '=%*=01;31'

alias ls="SUBSTS_LS --classify --color=always" # Add all colours and 
                                        # have fancy symbols for files, etc.
alias grep="grep --colour=always" # Colour grep too.
# Load the completion system
autoload -U compinit; compinit

# Very powerful version of mv implemented in zsh. The main feature I
# know of it that seperates it from the standard mv is that it saves you
# time by being able to use patterns which are expanded into positional
# parameters. So:
#
# slarti@pohl % zmv (*)foo ${1}bar
#
# On a series of files like onefoo, twofoo, threefoo, fivefoo would be
# renamed to onebar twobar threebar fourbar.
#
# Although that's nifty enough, I suspect there are other features I
# don't know about yet...
#
# Read $fpath/zmv for some more basic examples of usage, and also use
# run-help on it :)
autoload -U zmv

# Command line calculator written in zsh, with a complete history
# mechanism and other shell features.
autoload -U zcalc

# Like xargs, but instead of reading lines of arguments from standard input,
# it takes them from the command line. This is possible/useful because,
# especially with recursive glob operators, zsh often can construct a command
# line for a shell function that is longer than can be accepted by an external
# command. This is what's often referred to as the "shitty Linux exec limit" ;)
# The limitation is on the number of characters or arguments.
# 
# slarti@pohl % echo {1..30000}
# zsh: argument list too long: /bin/echo
# zsh: exit 127   /bin/echo {1..30000}
autoload -U zargs

# Yes, we are as bloated as emacs
autoload -U tetris
zle -N tetris
bindkey "^Xt" tetris

# Makes it easy to type URLs as command line arguments. As you type, the
# input character is analyzed and, if it mayn eed quoting, the current
# word is checked for a URI scheme. If one is found and the current word
# is not already quoted, a blackslash is inserted before the input
# caracter.
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# zed is a tiny command-line editor in pure ZSH; no other shell could do
# this.  zed itself is simple as anything, but it's killer feature for
# me is that it can edit functions on the go with zed -f <funcname> (or
# fned <funcname>. This is useful for me when I'm using and defining
# functions interactively, for example, when I'm working through the
# Portage tree in CVS. It allows me to edit a function on the fly,
# without having to call the last definition back up from the history
# and re-edit that in ZLE. It also indents the function, even if it was
# defined on all one line in the line editor, making it easy as anything
# to edit.
#
# ^X^W to save, ^C to abort.
autoload -U zed

# Incremental completion of a word. After starting this, a list of
# completion choices can be shown after every character you type, which
# can deleted with ^H or delete. Return will accept the current
# completion. Hit tab for normal completion, ^G to get back where you
# came from and ^D to list matches.
autoload -U incremental-complete-word
zle -N incremental-complete-word
bindkey "^Xi" incremental-complete-word

# This function allows you type a file pattern, and see the results of
# the expansion at each step.  When you hit return, they will be
# inserted into the command line.
autoload -U insert-files
zle -N insert-files
bindkey "^Xf" insert-files

# This set of functions implements a sort of magic history searching.
# After predict-on, typing characters causes the editor to look backward
# in the history for the first line beginning with what you have typed so
# far.  After predict-off, editing returns to normal for the line found.
# In fact, you often don't even need to use predict-off, because if the
# line doesn't match something in the history, adding a key performs
# standard completion - though editing in the middle is liable to delete
# the rest of the line.
autoload -U predict-on
zle -N predict-on
zle -N predict-off
bindkey "^X^Z" predict-on
bindkey "^Z" predict-off

# run-help is a help finder, bound in ZLE to M-h.  It doesn't need to be
# autoloaded to work - the non-autoloaded version just looks up a man
# page for the command under the cursor, then when that process is
# finished it pulls your old command line back up from the buffer stack.
# However, with the autoloaded function and:
#
# mkdir ~/zsh-help; cd ~/zsh-help MANPAGER="less" man zshbuiltins | \
# colcrt | perl /usr/share/zsh/4.2.1/Util/helpfiles
#
# It'll work for zsh builtins too. By the way, I've assumed some things
# in that command. ~/zsh-help can be wherever you like, MANPAGER needs
# to be any standard pager (less, pg, more, just not the MANPAGER I have
# defined in this file), colcrt can be col -bx, and the path to
# helpfiles may be different for you (Util may not even be installed
# with your distribution; fair enough, make install doesn't install it.
# Dig up a source tarball and everything is in there).

# Load the new one
autoload -U run-help

# Press Alt-H to show help for command we are currently on.
bindkey '[[A' run-help

# History file name and maximum size.
HISTFILE="$HOME/.zsh/history"
SAVEHIST=15000
HISTSIZE=15000

# Push History from previous sessions. IF $HISTFILE exists.
if [ -f $HISTFILE ]; then
    fc -R $HISTFILE
fi

## Key bindings
# You may use:
# % autoload -U zkbd
# % zkbd
# to discover your keys.

bindkey -v             # Vi keybindings.

typeset -U fpath

prompt wunjo
# Actually, stick with emacs for the moment. The vi keymap just doesn't
# seem to be as complete (even if it's nicer for editing, there's no
# execute-named-cmd bound, for example).
#bindkey -e             # Emacs keybindings.

# Up, down left, right.
# echotc forms part of the zsh/termcap module. It outputs the termcap value
# corresponding to the capability it was given as an argument. man zshmodules.
zmodload -i zsh/termcap
bindkey "$(echotc kl)" backward-char
bindkey "$(echotc kr)" forward-char
bindkey "$(echotc ku)" up-line-or-history
bindkey "$(echotc kd)" down-line-or-history

bindkey '\e[3~' delete-char # Delete

if [[ "$TERM" == "rxvt-unicode" || "$TERM" == "screen" ]]; then
    bindkey '\e[7~' beginning-of-line # Home
    bindkey '\e[8~' end-of-line # End
elif [[ "$TERM" == "linux" ]]; then
    bindkey '\e[1~' beginning-of-line # Home
    bindkey '\e[4~' end-of-line # End    
else # At least xterm; probably other terms too
    bindkey '\e[H~' beginning-of-line # Home
    bindkey '\e[F~' end-of-line # End
fi

bindkey '\e[5~' up-history # PageUp
bindkey '\e[6~' down-history # PageDown

# This function sets the window tile to user@host:/workingdir before each
# prompt. If you're using screen, it sets the window title (works
# wonderfully for 'hardstatus' lines.
# Beware: The two functions below have raw control characters.
precmd() {
#    [[ -t 1 ]] || return
    case $TERM in
    *xterm*|rxvt*) print -Pn "]2;%n@%m:%~\a"
    ;;
	screen*) print -Pn "\"%n@%m:%~\134"
	;;
    esac
}

# This sets the window title to the last run command.
preexec() {
#    [[ -t 1 ]] || return
    case $TERM in
    *xterm*|rxvt*)
    print -Pn "]2;$1\a"
    ;;
	screen*)
	print -Pn "\"$1\134"
	;;
    esac
}

# Custom commands.

loop() {
    while [ 1 -eq 1 ]; do
        $@
    done
}

# CTAN downloader

ctand() {
    wget http://mirror.ctan.org/macros/latex/contrib/$1.zip && unzip $1.zip
}

# For formating text files for a printer
fmtpr() {
    fmt --width=62 $@ | LC_ALL=C LANG=C pr -o 10 -W 62 -J -F -l 62 
}

#
# Debian.
#
debian.bugs.number() {
    # Debian Bug Tracker - by number.
    w3m "http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=$1"
}

debian.bugs.package() {
    # Debian Bug Tracker - by package.
    w3m "http://bugs.debian.org/cgi-bin/pkgreport.cgi?src=$1"
}

debian.packages() {
    # Debian Packages.
    # Add 'src:' in front of the package name to search for source packages.
    w3m "http://packages.debian.org/$1"
}

debian.qa.maintainer() {
    # Debian QA Maintainer search by email address.
    w3m "http://qa.debian.org/developer.php?login=$1"
}

debian.qa.package() {
    # Debian QA Maintainer search by package name.
    w3m "http://qa.debian.org/developer.php?package=$1"
}

debian.qa.pts() {
    # Debian Package Tracking system - developers.
    w3m "http://packages.qa.debian.org/$1"
}

debian.qa.task() {
    # Debian QA - search for task.
    w3m "http://qa.debian.org/developer.php?task=$1"
}

debian.wiki() {
    # Search the Debian Wiki.
    w3m "http://wiki.debian.org/?action=fullsearch&value=$@"
}

#
# Ubuntu.
#

ubuntu.help() {
    # Ubuntu Help site.
    w3m "https://help.ubuntu.com/community/?action=fullsearch&value=$@"
}

ubuntu.packages() {
    # Ubuntu packages.
    # Again; add: "src:" in order to search for source packages.
    w3m "http://packages.ubuntu.com/$1"
}

ubuntu.wiki() {
    # Search the Ubuntu wiki.
    w3m "https://wiki.ubuntu.com/?action=fullsearch&value=$@"
}

#
# Launchpad.
#

launchpad.answers.number() {
    # Display a Launchpad answer by number.
    w3m "https://answers.launchpad.net/questions/$1"
}

launchpad.answers.package() {
    # Display list of questions for package in Launchpad.
    w3m "https://answers.launchpad.net/ubuntu/+source/$1"
}

launchpad.answers.search() {
    # Search Launchpad answers.
    w3m "https://answers.launchpad.net/questions/+questions?field.search_text=$@"
}

launchpad.blueprints() {
    # Search Launchpad blueprints.
    w3m "https://blueprints.launchpad.net/?searchtext=$@"
}

launchpad.bugs.number() {
    # Display a Launchpad bug by number.
    w3m "https://bugs.launchpad.net/bugs/$1"
}

launchpad.bugs.package() {
    # Display list of bugs for a package.
    w3m "https://bugs.launchpad.net/ubuntu/+source/$1"
}

launchpad.packages() {
    # Launchpad packages for Ubuntu.
    w3m "http://launchpad.net/ubuntu/+source/$1"
}

launchpad.project() {
    # Display a Launchpad project.
    w3m "https://launchpad.net/$1"
}

launchpad.project.search() {
    # Search Launchpad projects.
    w3m "https://launchpad.net/projects/+index?text=$@"
}

#
# Others.
#

google() {
    # Google search.
    w3m "http://www.google.com/search?q=$@"
}

bbc.search() {
    # Search the BBC website.
    w3m "http://search.bbc.co.uk/cgi-bin/search/results.pl?q=$@"
}

demonoid() {
    # Search Demonoid.com torrents.
    w3m "http://www.demonoid.com/files/?query=$@"
}

freedictonary.acronyms() {
    # Look up an acronym on the FreeDictonary.
    w3m "http://acronyms.thefreedictionary.com/$1"
}

imdb() {
    # Search IMDb.
    w3m "http://www.imdb.com/find?q=$@"
}

lonelyplanet() {
    # Search Lonely Planet.
    w3m "http://search.lonelyplanet.com/search.do?Ntt=$@"
}

urbandictionary() {
    # Search Urbandictionary.com.
    w3m "http://www.urbandictionary.com/define.php?term=$@"
}

wikipedia() {
    # Wikipedia search. English section.
    w3m "http://en.wikipedia.org/wiki/Special:Search?search=$@"
}

wiktionary() {
    # Search Wiktionary for a term.
    w3m "http://en.wiktionary.org/wiki/Special:Search?search=$@"
}

wikitravel() {
    # Wikitravel search. English section.
    w3m "http://wikitravel.org/en/Special:Search?search=$@"
}


forkex() {
    # Fork program $@ from console.
    nohup "$@" >/dev/null 2>&1 <&1 & disown %%
}

#conjugate() {
#    # Conjugates the verb $@
#    w3m "http://www.mijnwoordenboek.nl/EN/verb/$@"
#}

conjugate() {
    # Conjugates the verb $@
    w3m "http://www.verbix.com/cache/webverbix/1/$@.shtml"
}

proxy() {
    export http_proxy=http://localhost:3128/
}

noproxy() {
    unset http_proxy
}

gbp-snap() {
    if [[ -e $@ ]]; then
        git-dch -aSN $1
    else
        git-dch -aS
    fi
    gpg-mounter
    gibuild --git-ignore-new
    DSC=`head -n1 debian/changelog | sed -e 's/\(.*\) (\(.*\)).*/\1_\2.dsc/g'`
    DIST=`head -n1 debian/changelog | sed -e 's/.* \(.*\);.*/\1/g'`
    cd ..
    sbuild -d $DIST $DSC
}

enru() {
    dict -d mueller7accent $@ | less
}

# Pretty menu!
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScroll active at %p%s.

# Completion options.
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete

# Completion caching.
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

# Expand partial paths.
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'

# Include non-hidden directories in globbed file completions
# for certain commands.
zstyle ':completion::complete:*' '\'

# Use menuselection for PID completion.
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

#  tag-order 'globbed-files directories' all-files.
zstyle ':completion::complete:*:tar:directories' file-patterns '*~.*(-/)'

# Do not complete backup files as executables.
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Separate matches into groups.
zstyle ':completion:*:matches' group 'yes'

# With commands like rm, it's annoying if you keep getting offered the same
# file multiple times. This fixes it. Also good for cp, et cetera..
zstyle ':completion:*:rm:*' ignore-line yes
zstyle ':completion:*:cp:*' ignore-line yes

# Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

# Messages/warnings format.
zstyle ':completion:*:messages' format '%B%U---- %d%u%b' 
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'

# Describe options in full.
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

# Simulate spider's old abbrev-expand 3.0.5 patch 
#zstyle ':completion:*:history-words' stop verbose
#zstyle ':completion:*:history-words' remove-all-dups yes
#zstyle ':completion:*:history-words' list false

# From the zshwiki. Hide CVS files/directories from being completed.
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS'

# Also from the wiki. Hide uninteresting users from completion.
zstyle ':completion:*:*:*:users' ignored-patterns \
adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs backup  bind  \
dictd  gnats  identd  irc  man  messagebus  postfix  proxy  sys \
www-data alias amavis at clamav cmd5checkpw cron cyrus dhcp dnscache \
dnslog foldingathome guest haldaemon jabber ldap mailman mpd mysql \
nut p2p portage postmaster qmaild qmaill qmailp qmailq qmailr qmails \
smmsp tinydns vpopmail wasabi zope

# Pull hosts from $HOME/.ssh/known_hosts, also from the wiki
# local _myhosts. If it exists that is.
if [ -f $HOME/.ssh/known_hosts ]; then
    _myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
    zstyle ':completion:*' hosts $_myhosts
fi

# Approximate completion. From the wiki.
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

zstyle ':completion:*:sudo:*' command-path /usr/sbin /usr/bin /sbin /bin \
                                           /usr/X11R6/bin

# NOTE: Comment this out for now. Breaks preexec and precmd above.
#if [ -f /etc/zsh_command_not_found ]; then
#    . /etc/zsh_command_not_found
#fi

# Options
setopt			\
NO_all_export		\
   always_last_prompt	\
   always_to_end	\
   append_history	\
   auto_cd		\
   auto_list		\
   auto_menu		\
   auto_name_dirs	\
   auto_param_keys	\
   auto_param_slash	\
   auto_pushd		\
   auto_remove_slash	\
NO_auto_resume		\
   bad_pattern		\
   bang_hist		\
NO_beep			\
   brace_ccl		\
   correct_all		\
NO_bsd_echo		\
NO_cdable_vars		\
NO_chase_links		\
   clobber		\
   complete_aliases	\
   complete_in_word	\
   correct		\
NO_correct_all		\
   csh_junkie_history	\
NO_csh_junkie_loops	\
NO_csh_junkie_quotes	\
NO_csh_null_glob	\
   equals		\
   extended_glob	\
   extended_history	\
   function_argzero	\
   glob			\
NO_glob_assign		\
   glob_complete	\
NO_glob_dots		\
NO_glob_subst		\
NO_hash_cmds		\
NO_hash_dirs		\
   hash_list_all	\
   hist_allow_clobber	\
   hist_beep		\
   hist_ignore_dups	\
   hist_ignore_space	\
NO_hist_no_store	\
   hist_verify		\
NO_hup			\
NO_ignore_braces	\
NO_ignore_eof		\
   interactive_comments	\
   inc_append_history	\
NO_list_ambiguous	\
NO_list_beep		\
   list_types		\
   long_list_jobs	\
   magic_equal_subst	\
NO_mail_warning		\
NO_mark_dirs		\
   menu_complete	\
   multios		\
   nomatch		\
   notify		\
NO_null_glob		\
   numeric_glob_sort	\
NO_overstrike		\
   path_dirs		\
   posix_builtins	\
NO_print_exit_value 	\
NO_prompt_cr		\
   prompt_subst		\
   pushd_ignore_dups	\
NO_pushd_minus		\
   pushd_silent		\
   pushd_to_home	\
   rc_expand_param	\
NO_rc_quotes		\
NO_rm_star_silent	\
NO_sh_file_expansion	\
   sh_option_letters	\
   share_history        \
   short_loops		\
NO_sh_word_split	\
NO_single_line_zle	\
NO_sun_keyboard_hack	\
NO_verbose		\
   zle

# GPG / SSH AGENT

if [[ "KEYCHAIN" = "True" ]]; then
    keychain id_rsa id_rsa.lambda id_ecdsa
    keychain -Q ${GPGKEY} ${GPGKEY1}
    [ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`
    [ -f $HOME/.keychain/$HOSTNAME-sh ] &&
           . $HOME/.keychain/$HOSTNAME-sh
    [ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] &&
           . $HOME/.keychain/$HOSTNAME-sh-gpg
fi
