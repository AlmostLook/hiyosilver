dbus-send --session --dest=org.freedesktop.Notifications \
                         --type=method_call --reply-timeout=10000 \
                         /org/freedesktop/Notifications org.freedesktop.Notifications \
                         string:'Test Application' uint32:0 \
                         string: string:'Notification Title' \
                         string:'This is the body' \
                         array:string: dict:string: int32:10000
