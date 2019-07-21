# rpi-radio-alarm-frontend

An Elm frontend for rpi-radio-alarm

## Deploy

```
elm make src/Main.elm --output=main.js && scp index.html main.js styles.css pi@<PI>:/home/pi/src/rpi-radio-alarm/static/
```
