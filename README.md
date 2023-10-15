# README

## How to use launchctl on macOS for change-color.sh

### Starting

```sh
launchctl bootstrap gui/<uid>/ ~/Library/LaunchAgents/com.sflomenb.changecolor.plist
launchctl enable gui/<uid>/change-color
launchctl kickstart -p gui/<uid>/change-color
```


