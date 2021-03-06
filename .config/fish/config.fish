abbr cls         'clear'
abbr edit-abbr   'emacs  ~/.config/fish/config.fish'
abbr update-abbr 'source ~/.config/fish/config.fish'
abbr cp          'cp -vi'
abbr mv          'mv -vi'
abbr gs          'git status'
abbr gps         'git push'
abbr gpl         'git pull'
abbr gc          'git commit -m'
abbr gca         'git commit -a -m'
abbr ga          'git add'
abbr gd          'git diff'
abbr gco         'git checkout'
# abbr grep        'grep -C 10'
abbr rm          'rm -rf'
abbr cat         'bat'
abbr pull        'cd ~/ ; git pull
                ; cd note/ ; git pull
                ; cd ~/code/kara/ ; git pull'
abbr sync        'cd ~; git commit -a -m \'sync\' ; git push ;
                  cd ~/note ; git commit -a -m \'sync\' ; git push'
abbr tar        'tar -xzvf'
abbr tranf      'trans fi:en'
abbr tranj      'trans ja:en'
abbr apt        'sudo apt'
abbr docker     'sudo docker'
abbr dockerd    'sudo dockerd & disown'
abbr terraform  'sudo terraform'

# clean up stuff that were created less than 1 minute ago
abbr clean 'find . -type f -cmin -1 -delete'
abbr conky-conf 'sudo emacs /etc/conky/conky.conf'

# this does everything I want to do about updating
# this command is stored in the root user's script path
abbr khoa-update 'sudo sh /usr/local/sbin/khoa-update'

#This command deals with updaing the hash of tex:
abbr texhash 'texhash & texhash ~/texmf'

#ls right after cd
function cdl
    builtin cd $argv; and ls -F
end

# fix emacs term
if test "$TERM" = "eterm-color"
  function fish_title; end
end

# PATH
set PATH /home/khoa/.local/bin $PATH
set PATH /snap/bin $PATH