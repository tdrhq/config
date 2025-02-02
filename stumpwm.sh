export PATH=~/.local/bin:$PATH
export GDK_CORE_DEVICE_EVENTS=1
gnome-settings-daemon &
nm-applet --sm-disable &

# export SBCL_HOME=/usr/local/lib/sbcl
export PATH=~/android-studio/bin:$PATH
eval $(/usr/bin/gnome-keyring-daemon --daemonize --login --start --components=gpg,pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID
/usr/lib/notification-daemon/notification-daemon &
gnome-screensaver &

~arnold/builds/stumpwm/stumpwm
