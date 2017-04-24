gnome-settings-daemon &
nm-applet --sm-disable &

eval $(/usr/bin/gnome-keyring-daemon --daemonize --login --start --components=gpg,pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID
/usr/lib/notification-daemon/notification-daemon &

~arnold/builds/stumpwm/stumpwm
