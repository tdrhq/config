export PATH=~/.local/bin:$PATH
gnome-settings-daemon &
nm-applet --sm-disable &

export PATH=~/android-studio/bin:$PATH
eval $(/usr/bin/gnome-keyring-daemon --daemonize --login --start --components=gpg,pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID
/usr/lib/notification-daemon/notification-daemon &
gnome-screensaver &

~arnold/builds/stumpwm/stumpwm
